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
        LogFlow<<"Starting data segment "<<data.GetName()<<" ("<<(data.GetName()=="const"?"readOnly":"read/write")<<")"<<End();
        unsigned segmentStartIdx = atomIdx;

        unsigned offset = 0;
        for(auto &atom: data) {
            LogTrace<<"Found label: "<<atom.GetName()<<" at offset "<<offset<<" with atomIdx "<<atomIdx<<End();
            labels.emplace(atom.GetName(), Label{atoms[atomIdx].get(),offset});
            for(auto item: atom) {
                switch(item->GetKind()) {
                case Item::Kind::ALIGN: {
                    unsigned align = item->GetAlign();
                    unsigned misalignment = offset % align;
                    if(misalignment != 0 ) {
                        LogDetail<<"Align "<<align<<": Moving from offset "<<offset;
                        unsigned shift = align - misalignment;
                        atoms[atomIdx]->addSpace(shift);
                        offset += shift;
                        LogDetail<<" to offset "<<offset<<End();
                    } else {
                        LogDetail<<"Align "<<align<<": Offset "<<offset<<" is already aligned"<<End();
                    }
                } break;
                case Item::Kind::END:
                    if(atoms[atomIdx]->getSize() > 0) {
                        LogDetail<<"Ending atom number "<<atomIdx<<End();
                        offset = 0;
                        ++atomIdx;
                        atoms.push_back(std::make_unique<MappedAtom>(storagePool));
                    } else {
                        LogDetail<<"Ignoring duplicated end"<<End();
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
                    LogDetail<<"Float "<<v->toString()<<End();
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
                    LogDetail<<"Int8 "<<v->toString()<<End();
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
                    LogDetail<<"Int16 "<<v->toString()<<End();
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
                    LogDetail<<"Int32 "<<v->toString()<<End();
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
                    LogDetail<<"Int64 "<<v->toString()<<End();
                    atoms[atomIdx]->addSymValue(*v);
                } break;
                case Item::Kind::SPACE:
                    LogDetail<<"Space "<<item->GetSpace()<<End();
                    atoms[atomIdx]->addSpace(item->GetSpace());
                    offset += item->GetSpace();
                    break;
                case Item::Kind::STRING:
                    LogDetail<<"String ";
                    for(auto &c: item->GetString()) {
                        LogDetail<<"'"<<c<<"' ";
                        auto v = storagePool.persist(new IntSymValue(c, Type::U8));
                        atoms[atomIdx]->addSymValue(*v);
                    }
                    offset += item->GetString().size();
                    break;
                case Item::Kind::SYMBOL: {
                    Global *g = item->GetSymbol();
                    switch(g->GetKind()) {
                    case Global::Kind::ATOM: {
                        LogDetail<<"Atom "<<g->GetName()<<End();
                        //We cannot guarantee that the referrenced atom has been
                        //reached yet, so give an invalid one now and update it later
                        auto v = storagePool.persist(
                            new StaticPtrSymValue(std::string_view("INVALID"), llvm::APInt(), 0, Type::U64)
                        );
                        atoms[atomIdx]->addSymValue(*v);
                        storedAddresses[v] = g->GetName();
                    } break;
                    case Global::Kind::BLOCK:
                        LogDetail<<"Block "<<g->GetName()<<End();
                        atoms[atomIdx]->addSymValue(*storagePool.persist(
                            new BlockRefSymValue(g->GetName(), Type::U64)
                        ));
                        break;
                    case Global::Kind::EXTERN: {
                        LogDetail<<"Extern "<<g->GetName()<<End();
                        auto v = storagePool.persist(
                            new ExternSymValue(g->GetName(), Type::U64)
                        );
                        atoms[atomIdx]->addSymValue(*v);
                    } break;
                    case Global::Kind::FUNC:
                        LogDetail<<"Func "<<g->GetName()<<End();
                        atoms[atomIdx]->addSymValue(*storagePool.persist(
                            new FuncRefSymValue(g->GetName(), Type::U64)
                        ));
                        break;
                    case Global::Kind::SYMBOL:
                        LogDetail<<"Symbol "<<g->GetName()<<End();
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
        LogDetail<<"Updating "<<v->get_name()<<End();
        new (v) StaticPtrSymValue(
            name,
            label.offset,
            label.atom->getSize(),
            Type::U64
        );
        LogDetail<<"Updated "<<v->get_name()<<" with offset "<<v->get_offset().toString(10, false)<<" and size "<<v->get_max()<<End();
    }
    LogDetail<<"Finished updating"<<End();
}

SymValue *BaseStore::read(SymValue *loc, size_t loadSize, Type type, FlowNode *node)
{
    if(loc->get_kind() == SymValue::Kind::STATICPTR) {
        StaticPtrSymValue *addr = static_cast<StaticPtrSymValue*>(loc);

        auto label = labels.find(addr->get_name());
        if(label==labels.end()) {
            LogWarning<<"Label not found"<<End();
            return storagePool.persist(
                new UnknownSymValue(type)
            );
        } else {
            MappedAtom *atom = label->second.atom;
            LogDetail<<"Reading from label "
                <<label->first
                <<" at atom offset of "<<addr->get_offset().getLimitedValue()
                <<End();
            
            return storagePool.persist(
                atom->get(addr->get_offset(), loadSize, type)
            );
        }
    } else {
        LogWarning<<"Attempting to read at non-address"<<End();
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
            LogWarning<<"Label not found"<<End();
            return result;
        } else {
            MappedAtom *atom = label->second.atom;
            LogDetail<<"Reading from label "
                <<label->first
                <<" at atom offset of "<<address->get_offset().getLimitedValue()
                <<End();
            

        }
    }
    return result; //TODO finish this
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
    SymExPool &pool
):
    DataStore(pool)
{
    LogDetail<<"LogStore constructor"<<End();
}

SymValue *LogStore::read(SymValue *loc, size_t loadSize, Type type, FlowNode *node)
{
    SymValue *value = nullptr;

    for(int i = actions.size()-1; i >= 0; --i) {
        if(actions[i]->get_kind() == Action::Kind::INVALIDATE) {
            LogDetail<<"Reached invalidate"<<End();
            value = new UnknownSymValue(type);
            break;
        } else if (actions[i]->get_kind() == Action::Kind::READ) {
            auto read = static_cast<Read*>(actions[i].get());
            if(SymComp::EQ(loc, read->get_addr(),node).first == SymComp::Result::TRUE) {
                value = read->get_value();
                if(value->get_type() != type) LogWarning<<"Load type mismatch with read"<<End();
                break;
            }
        } else { // action is a write
            auto write = static_cast<Write*>(actions[i].get());
            if(SymComp::EQ(loc,write->get_addr(), node).first == SymComp::Result::UNKNOWN) {
                LogDetail<<"Found potential match: "<<toString(loc)<<" ?= "<<toString(write->get_addr())<<End();
                value = new UnknownSymValue(type);
                break;
            } else if(SymComp::EQ(loc,write->get_addr(), node).first == SymComp::Result::TRUE) {
                LogDetail<<"Found match"<<End();
                value = write->get_value();
                if(value->get_type() != type) LogWarning<<"Load type mismatch with write"<<End();
                break;
            }
        }
    }
    return value;
}

void LogStore::recordRead(SymValue *loc, SymValue *value, Inst *inst)
{
    LogDetail<<"Recording read"<<End();
    actions.push_back(std::make_unique<Read>(loc, value, inst));
    LogDetail<<"Found value is "<<(value == nullptr ? "nullptr":toString(*value))<<End();
}

std::vector<SymValue*> LogStore::readSequence(SymValue *addr, unsigned size)
{
    LogError<<"Logstore readSequence is not implemented yet"<<End();
    return std::vector<SymValue*>();
}

void LogStore::write(
    SymValue *addr,
    SymValue *value,
    Inst *inst
) {
    LogDetail<<"log write logged: "<<toString(addr)<<" := "<<toString(value)<<End();
    actions.push_back(std::make_unique<Write>(addr,value, inst)); //TODO
}

void LogStore::invalidate(
    MappedAtom &startPoint,
    Inst *inst
) {
    LogDetail<<"logging invalidate"<<End();
    actions.push_back(std::make_unique<Invalidate>(startPoint, inst)); //TODO
}

std::string LogStore::nextHeapName = "0";

std::string LogStore::getNextHeapName()
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

std::vector<LogStore::Action*> LogStore::getLog() const
{
    LogDetail<<"Getting log"<<End();
    auto log = std::vector<Action*>();
    std::transform(
        actions.begin(),
        actions.end(),
        std::back_inserter(log),
        [](const std::unique_ptr<Action>& ptr) {return ptr.get();}
    );

    LogDetail<<"Transformed"<<End();
    LogDetail<<"Log size "<<log.size()<<End();

    return log;
}
