#include <exception>
#include <iostream>
#include <limits>
#include <string_view>

#include <llvm/ADT/APInt.h>
#include <llvm/ADT/StringRef.h>

#include "core/atom.h"
#include "core/data.h"
#include "core/prog.h"
#include "core/type.h"
#include "datastore.h"
#include "mappedatom.h"
#include "storagepool.h"
#include "symvalue.h"
#include "symvaluecomp.h"
#include "utilities.h"

//-----------------------------------------------------------------------------
// BaseStore
//-----------------------------------------------------------------------------

BaseStore::BaseStore(
    Prog &prog,
    SymExPool &storagePool
): 
    DataStore(storagePool)
{
    std::unordered_map<StaticPtrSymValue*, std::string_view> storedAddresses;

    unsigned atomIdx = 0;
    atoms.push_back(std::make_unique<MappedAtom>(storagePool));
    for(auto &data: prog.data()) {
        std::cout<<"Starting data segment "<<data.GetName()<<" ("<<(data.GetName()=="const"?"readOnly":"read/write")<<")"<<std::endl;
        unsigned segmentStartIdx = atomIdx;

        unsigned offset = 0;
        for(auto &atom: data) {
            std::cout<<"Found label: "<<atom.GetName()<<" at offset "<<offset<<" with atomIdx "<<atomIdx<<std::endl;
            labels.emplace(atom.GetName(), Label{atoms[atomIdx].get(),offset});
            for(auto item: atom) {
                switch(item->GetKind()) {
                case Item::Kind::ALIGN: {
                    unsigned align = item->GetAlign();
                    unsigned misalignment = offset % align;
                    if(misalignment != 0 ) {
                        std::cout<<"Align "<<align<<": Moving from offset "<<offset;
                        unsigned shift = align - misalignment;
                        atoms[atomIdx]->addSpace(shift);
                        offset += shift;
                        std::cout<<" to offset "<<offset<<std::endl;
                    } else {
                        std::cout<<"Align "<<align<<": Offset "<<offset<<" is already aligned"<<std::endl;
                    }
                } break;
                case Item::Kind::END:
                    if(atoms[atomIdx]->getSize() > 0) {
                        std::cout<<"Ending atom number "<<atomIdx<<std::endl;
                        offset = 0;
                        ++atomIdx;
                        atoms.push_back(std::make_unique<MappedAtom>(storagePool));
                    } else {
                        std::cout<<"Ignoring duplicated end"<<std::endl;
                        assert(offset == 0);
                    }
                    break;
                case Item::Kind::FLOAT64: {
                    auto v = storagePool.persist(
                        new FloatSymValue(
                            llvm::APFloat(
                                llvm::APFloatBase::IEEEdouble(),
                                static_cast<uint64_t>(item->GetFloat64())
                            ),
                            Type::F64
                        )
                    );
                    offset += 8;
                    std::cout<<"Float "<<v->toString()<<std::endl;
                    atoms[atomIdx]->addSymValue(*v);
                } break;
                case Item::Kind::INT8: {
                    auto v = storagePool.persist(
                        new IntSymValue(
                            item->GetInt8(),
                            Type::U8
                        )
                    );
                    offset += 1;
                    std::cout<<"Int8 "<<v->toString()<<std::endl;
                    atoms[atomIdx]->addSymValue(*v);
                } break;
                case Item::Kind::INT16: {
                    auto v = storagePool.persist(
                        new IntSymValue(
                            item->GetInt16(),
                            Type::U16
                        )
                    );
                    offset += 2;
                    std::cout<<"Int16 "<<v->toString()<<std::endl;
                    atoms[atomIdx]->addSymValue(*v);
                } break;
                case Item::Kind::INT32: {
                    auto v = storagePool.persist(
                        new IntSymValue(
                            item->GetInt32(),
                            Type::U32
                        )
                    );
                    offset += 4;
                    std::cout<<"Int32 "<<v->toString()<<std::endl;
                    atoms[atomIdx]->addSymValue(*v);
                } break;
                case Item::Kind::INT64: {
                    auto v = storagePool.persist(
                        new IntSymValue(
                            item->GetInt64(),
                            Type::U64
                        )
                    );
                    offset += 8;
                    std::cout<<"Int64 "<<v->toString()<<std::endl;
                    atoms[atomIdx]->addSymValue(*v);
                } break;
                case Item::Kind::SPACE:
                    std::cout<<"Space "<<item->GetSpace()<<std::endl;
                    atoms[atomIdx]->addSpace(item->GetSpace());
                    offset += item->GetSpace();
                    break;
                case Item::Kind::STRING:
                    std::cout<<"String ";
                    for(auto &c: item->GetString()) {
                        std::cout<<"'"<<c<<"' ";
                        auto v = storagePool.persist(new IntSymValue(c, Type::U8));
                        atoms[atomIdx]->addSymValue(*v);
                    }
                    offset += item->GetString().size();
                    break;
                case Item::Kind::SYMBOL: {
                    Global *g = item->GetSymbol();
                    switch(g->GetKind()) {
                    case Global::Kind::ATOM: {
                        std::cout<<"Atom "<<g->GetName()<<std::endl;
                        //We cannot guarantee that the referrenced atom has been
                        //reached yet, so give an invalid one now and update it later
                        auto v = storagePool.persist(
                            new StaticPtrSymValue(std::string_view("INVALID"), llvm::APInt(), 0, Type::U64)
                        );
                        atoms[atomIdx]->addSymValue(*v);
                        storedAddresses[v] = g->GetName();
                    } break;
                    case Global::Kind::BLOCK:
                        std::cout<<"Block "<<g->GetName()<<std::endl;
                        atoms[atomIdx]->addSymValue(*storagePool.persist(
                            new BlockRefSymValue(g->GetName(), Type::U64)
                        ));
                        break;
                    case Global::Kind::EXTERN: {
                        std::cout<<"Extern "<<g->GetName()<<std::endl;
                        auto v = storagePool.persist(
                            new ExternSymValue(g->GetName(), Type::U64)
                        );
                        atoms[atomIdx]->addSymValue(*v);
                    } break;
                    case Global::Kind::FUNC:
                        std::cout<<"Func "<<g->GetName()<<std::endl;
                        atoms[atomIdx]->addSymValue(*storagePool.persist(
                            new FuncRefSymValue(g->GetName(), Type::U64)
                        ));
                        break;
                    case Global::Kind::SYMBOL:
                        std::cout<<"Symbol "<<g->GetName()<<std::endl;
                        atoms[atomIdx]->addSymValue(*storagePool.persist(
                            new UnknownSymValue(Type::U64)
                        ));
                        break;
                    }
                    offset += 8;
                } break;
                }
            }
        }

        dataSegments.push_back(
            DataSegmentInfo{
                data.GetName(),
                data.GetName() == "const",
                segmentStartIdx,
                atomIdx+1
            }
        );
    }

    for(auto &[v, name]: storedAddresses) {
        Label &label = labels.at(name);
        std::cout<<"Updating "<<v->get_name()<<std::endl;
        new (v) StaticPtrSymValue(
            name,
            label.offset,
            label.atom->getSize(),
            Type::U64
        );
        std::cout<<"Updated "<<v->get_name()<<" with offset "<<v->get_offset().toString(10, false)<<" and size "<<v->get_max()<<std::endl;
    }
    std::cout<<"Finished updating"<<std::endl;
}

SymValue *BaseStore::read(SymValue *loc, size_t loadSize, Type type, Inst *inst, bool, unsigned debugCount)
{
    std::cout<<debugCount<<" delegations"<<std::endl;

    if(loc->get_kind() == SymValue::Kind::STATICPTR) {
        StaticPtrSymValue *addr = static_cast<StaticPtrSymValue*>(loc);

        auto label = labels.find(addr->get_name());
        if(label==labels.end()) {
            std::cout<<"WARNING: Label not found"<<std::endl;
            return storagePool.persist(
                new UnknownSymValue(type)
            );
        } else {
            MappedAtom *atom = label->second.atom;
            std::cout<<"Reading from label "
                <<label->first
                <<" at atom offset of "<<addr->get_offset().getLimitedValue()
                <<std::endl;
            
            return storagePool.persist(
                atom->get(addr->get_offset(), loadSize, type)
            );
        }
    } else {
        std::cout<<"Attempting to read at non-address"<<std::endl;
        return storagePool.persist(
            new UnknownSymValue(type)
        );
    }
}

std::vector<SymValue*> BaseStore::readSequence(SymValue *addr, unsigned size)
{
    std::vector<SymValue*> result;
    if(addr->get_kind() == SymValue::Kind::STATICPTR) {
        auto address = static_cast<StaticPtrSymValue*>(addr);
        auto label = labels.find(address->get_name());
        if(label == labels.end()) {
            std::cout<<"WARNING: Label not found"<<std::endl;
            return result;
        } else {
            MappedAtom *atom = label->second.atom;
            std::cout<<"Reading from label "
                <<label->first
                <<" at atom offset of "<<address->get_offset().getLimitedValue()
                <<std::endl;
            

        }
    }
    return result; //TODO finish this
}

void BaseStore::write(
    SymValue *addr,
    SymValue *value,
    Inst *inst
) {
    std::cout<<"base write not implemented"<<std::endl;
}

void BaseStore::invalidate(
    MappedAtom &startPoint,
    Inst *Inst
) {
    std::cout<<"base invalidate not implemented"<<std::endl;
}

const Label *BaseStore::getLabel(std::string_view name) const
{
    if(labels.find(name) == labels.end()) {
        return nullptr;
    } else {
        return &labels.at(name);
    }
}

//-----------------------------------------------------------------------------
// LogStore
//-----------------------------------------------------------------------------

LogStore::LogStore(
    DataStore &store,
    SymExPool &pool
):
    DataStore(pool),
    baseStore(store),
    nextHeapName(baseStore.getNextHeapName())
{
    std::cout<<"Correct constructor"<<std::endl;
}

SymValue *LogStore::read(SymValue *loc, size_t loadSize, Type type, Inst *inst, bool record, unsigned debugCount)
{
    SymValue *value = nullptr;

    for(int i = actions.size()-1; i >= 0; --i) {
        if(actions[i]->get_kind() == Action::Kind::INVALIDATE) {
            std::cout<<"Reached invalidate"<<std::endl;
            value = new UnknownSymValue(type);
            break;
        } else if (actions[i]->get_kind() == Action::Kind::READ) {
            auto read = static_cast<Read*>(actions[i].get());
            if(SymComp::EQ(loc, read->get_addr()) == SymComp::Result::TRUE) {
                value = read->get_value();
                if(value->get_type() != type) std::cout<<"Load type mismatch with read"<<std::endl;
                break;
            }
        } else { // action is a write
            auto write = static_cast<Write*>(actions[i].get());
            if(SymComp::EQ(loc,write->get_addr()) == SymComp::Result::UNKNOWN) {
                std::cout<<"Found potential match: "<<toString(loc)<<" ?= "<<toString(write->get_addr())<<std::endl;
                value = new UnknownSymValue(type);
                break;
            } else if(SymComp::EQ(loc,write->get_addr()) == SymComp::Result::TRUE) {
                std::cout<<"Found match"<<std::endl;
                value = write->get_value();
                if(value->get_type() != type) std::cout<<"Load type mismatch with write"<<std::endl;
                break;
            }
        }
    }
    if(value == nullptr) {
        value = baseStore.read(loc, loadSize, type, inst, false, debugCount+1);
    }
    if(record) {
        std::cout<<"Recording read"<<std::endl;
        actions.push_back(std::make_unique<Read>(loc, value, inst));
        std::cout<<"Found value is "<<(value == nullptr ? "nullptr":toString(*value))<<std::endl;
    }
    return value;
}

std::vector<SymValue*> LogStore::readSequence(SymValue *addr, unsigned size)
{
    std::cout<<"WARNING: Logstore readSequence is not implemented yet"<<std::endl;
    return std::vector<SymValue*>();
}

void LogStore::write(
    SymValue *addr,
    SymValue *value,
    Inst *inst
) {
    std::cout<<"log write logged: "<<toString(addr)<<" := "<<toString(value)<<std::endl;
    actions.push_back(std::make_unique<Write>(addr,value, inst)); //TODO
}

void LogStore::invalidate(
    MappedAtom &startPoint,
    Inst *inst
) {
    std::cout<<"logging invalidate"<<std::endl;
    actions.push_back(std::make_unique<Invalidate>(startPoint, inst)); //TODO
}

const Label *LogStore::getLabel(std::string_view name) const
{
    return baseStore.getLabel(name);
}

std::string LogStore::getNextHeapName() const
{
    return nextHeapName;
}

void LogStore::incrementNextHeapName()
{
    bool carry;
    int i = nextHeapName.length()-1;
    do {
        if(nextHeapName[i] <= '8') {
            nextHeapName[i] += 1;
            carry = false;
        } else {
            nextHeapName[i] = '0';
            carry = true;
            --i;
        }
    } while (carry && i >= 0);
    if(i < 0 ) {
        nextHeapName = "1" + nextHeapName;
    }
}

std::string_view LogStore::addHeapAtom(unsigned size)
{
    std::string name = getNextHeapName();
    auto [iter, success] = heap.insert({name,std::make_unique<MappedAtom>(storagePool)});
    assert(success);
    return std::string_view(iter->first);
}

std::vector<DataStore::Action*> LogStore::getFullLog() const
{
    std::cout<<"Getting log"<<std::endl;
    auto previousLog = baseStore.getFullLog();
    std::cout<<"Previous length: "<<previousLog.size()<<std::endl;
    auto log = std::vector<Action*>();
    std::transform(
        actions.begin(),
        actions.end(),
        std::back_inserter(log),
        [](const std::unique_ptr<Action>& ptr) {return ptr.get();}
    );

    std::cout<<"Transformed"<<std::endl;
    std::cout<<"Log size "<<log.size()<<std::endl;

    if(previousLog.size() == 0) {
        std::cout<<"Returning own log"<<std::endl;
        return log;
    } else {
        previousLog.insert(previousLog.end(), log.begin(), log.end());
        return previousLog;
    }
}

// std::vector<MappedAtom*> LogStore::modelPointers(std::string_view name)
// {
//     const MappedAtom *atom = getLabel(name)->atom;
//     const unsigned size = atom->getSize();
//     std::vector<SymByteRef> bytes(size);

//     fillMissingBytes(bytes, atom);

//     return std::vector<MappedAtom*>();
// }

// void LogStore::fillMissingBytes(std::vector<SymByteRef> &bytes, const MappedAtom* atom) const
// {
//     for(int i = actions.size()-1; i >= 0; --i) {
//         switch(actions[i]->get_kind()) {
//         case Action::Kind::READ:
//         case Action::Kind::WRITE: {
//             auto action = static_cast<ReadWrite*>(actions[i].get());
//         } break;
//         }
//     }
// }




