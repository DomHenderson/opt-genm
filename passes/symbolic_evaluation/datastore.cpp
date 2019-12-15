#include <exception>
#include <iostream>
#include <limits>

#include <llvm/ADT/APInt.h>
#include <llvm/ADT/StringRef.h>

#include "core/atom.h"
#include "core/data.h"
#include "core/prog.h"
#include "core/type.h"
#include "datastore.h"
#include "storagepool.h"
#include "symvalue.h"
#include "symvaluecomp.h"
#include "utilities.h"

void MappedAtom::addSpace(unsigned space)
{
    std::cout<<"Adding space from "<<size<<" to "<<size+space<<std::endl;
    size += space;
}

void MappedAtom::addSymValue(SymValue &value)
{
    unsigned length = byteLength(value.get_type());
    auto [a,b] = items.emplace(size, &value);
    if(b) {
        std::cout<<toString(*a->second)<<" added successfully"<<std::endl;
    } else {
        std::cout<<"Failed to add"<<std::endl;
    }
    std::cout<<"There are now "<<items.size()<<" items"<<std::endl;
    size += length;
}

SymValue *MappedAtom::get(unsigned offset, size_t loadSize, Type type)
{
    if(offset >= size) {
        std::cout<<"WARNING: Attempted to read at offset >= size"<<std::endl;
        return nullptr;
    }

    if(byteLength(type) < loadSize) {
        std::cout<<"WARNING: Attempting to read more data than can fit into type"<<std::endl;
    }

    auto [start, end] = FindSection(offset);
    auto iter = items.find(start);
    unsigned dataEnd;
    if(iter == items.end()) {
        std::cout<<"Data item contains only empty space"<<std::endl;
        dataEnd = start;
    } else {
        dataEnd = start + byteLength(iter->second->get_type());
    }
    if(offset < dataEnd) {
        if(offset == start) {
            std::cout<<"Reading from start of data item"<<std::endl;
            SymValue *value = iter->second;
            if(loadSize == byteLength(value->get_type())) {
                return value->copy_cast(type);
            } else {
                std::cout<<"WARNING: Reading only part of the item"<<std::endl;
                return new UnknownSymValue(type);
            }
        } else {
            std::cout<<"WARNING: Attempting to read from part way into an item"<<std::endl;
            return new UnknownSymValue(type);
        }
    } else {
        if(offset + loadSize <= end) {
            std::cout<<"Reading from empty space"<<std::endl;
            if(isIntType(type)) {
                return pool.persist(
                    new IntSymValue(0, type)
                );
            } else {
                std::cout<<"Attempting to read float from empty space"<<std::endl;
                return pool.persist(
                    new UnknownSymValue(type)
                );
            }
        } else {
            std::cout<<"WARNING: Reading past end of item"<<std::endl;
            return new UnknownSymValue(type);
        }
    }
}

SymValue *MappedAtom::get(llvm::APInt offset, size_t loadSize, Type type)
{
    auto o = offset.getLimitedValue();
    if(o > std::numeric_limits<unsigned>::max()) {
        std::cout<<"WARNING: attempting to read at very large offset"<<std::endl;
    }
    unsigned i = static_cast<unsigned>(o);
    return get(i, loadSize, type);
}

std::pair<unsigned, unsigned> MappedAtom::FindSection(unsigned offset) const
{
    std::cout<<"Finding section"<<std::endl;
    assert(offset < size);
    unsigned start = 0;
    unsigned end = 0;

    std::cout<<"This atom contains "<<items.size()<<" items"<<std::endl;

    for(auto &[k,v]: items) {
        std::cout<<"Checking k = "<<k<<" ("<<toString(*v)<<")"<<std::endl;
        if(k <= offset) {
            start = k;
        } else {
            end = k;
            break;
        }
    }
    if(end == 0) {
        std::cout<<"End was never set"<<std::endl;
        end = size;
    }

    std::cout<<"Found offset "<<offset<<" is in section ["<<start<<", "<<end<<")"<<std::endl;
    return std::pair{start, end};
}

unsigned MappedAtom::getSize() const
{
    return size;
}

//-----------------------------------------------------------------------------



BaseStore::BaseStore(
    Prog &prog,
    SymExPool &storagePool
): 
    storagePool(storagePool)
{
    std::unordered_map<AddrSymValue*, std::string_view> storedAddresses;

    unsigned atomIdx = 0;
    atoms.push_back(std::make_unique<MappedAtom>(storagePool));
    for(auto &data: prog.data()) {
        std::cout<<"Starting data segment "<<data.GetName()<<" ("<<(data.GetName()=="const"?"readOnly":"read/write")<<")"<<std::endl;
        dataSegments.push_back(
            DataSegmentInfo{
                data.GetName(),
                data.GetName() == "const"
            }
        );

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
                            new AddrSymValue(std::string_view("INVALID"), llvm::APInt(), 0, Type::U64)
                        );
                        atoms[atomIdx]->addSymValue(*v);
                        storedAddresses[v] = g->GetName();
                    } break;
                    case Global::Kind::BLOCK:
                        std::cout<<"Block "<<g->GetName()<<std::endl;
                        atoms[atomIdx]->addSymValue(*storagePool.persist(
                            new UnknownSymValue(Type::U64)
                        ));
                        break;
                    case Global::Kind::EXTERN: {
                        std::cout<<"Extern "<<g->GetName()<<std::endl;
                        auto v = storagePool.persist(
                            new ExternSymValue(g->GetName(), Type::U64)
                        );
                        atoms[atomIdx]->addSymValue(*v);
                    } break;
                    case Global::Kind::FUNC: {
                        std::cout<<"Func "<<g->GetName()<<std::endl;
                        auto v = storagePool.persist(
                            new FuncRefSymValue(g->GetName(), Type::U64)
                        );
                        atoms[atomIdx]->addSymValue(*v);
                    } break;
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
    }

    for(auto &[v, name]: storedAddresses) {
        Label &label = labels.at(name);
        std::cout<<"Updating "<<v->get_name()<<std::endl;
        new (v) AddrSymValue(
            name,
            label.offset,
            label.atom->getSize(),
            Type::U64
        );
        std::cout<<"Updated "<<v->get_name()<<" with offset "<<v->get_offset().toString(10, false)<<" and size "<<v->get_max()<<std::endl;
    }
    std::cout<<"Finished updating"<<std::endl;
}

SymValue *BaseStore::read(SymValue *loc, size_t loadSize, Type type, bool, unsigned debugCount)
{
    std::cout<<debugCount<<" delegations"<<std::endl;

    if(loc->get_kind() == SymValue::Kind::ADDR) {
        AddrSymValue *addr = static_cast<AddrSymValue*>(loc);

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

void BaseStore::write(
    SymValue *addr,
    SymValue *value
) {
    std::cout<<"base write not implemented"<<std::endl;
}

void BaseStore::invalidate()
{
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

LogStore::LogStore(
    DataStore &store
):
    baseStore(store)
{
    std::cout<<"Correct constructor"<<std::endl;
}

SymValue *LogStore::read(SymValue *loc, size_t loadSize, Type type, bool record, unsigned debugCount)
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
                std::cout<<"Found potential match"<<std::endl;
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
        value = baseStore.read(loc, loadSize, type, false, debugCount+1);
    }
    if(record) {
        std::cout<<"Recording read"<<std::endl;
        actions.push_back(std::make_unique<Read>(loc, value));
        std::cout<<"Found value is "<<(value == nullptr ? "nullptr":toString(*value))<<std::endl;
    }
    return value;
}

void LogStore::write(
    SymValue *addr,
    SymValue *value
) {
    std::cout<<"log write logged"<<std::endl;
    actions.push_back(std::make_unique<Write>(addr,value));
}

void LogStore::invalidate()
{
    std::cout<<"logging invalidate"<<std::endl;
    actions.push_back(std::make_unique<Invalidate>());
}

const Label *LogStore::getLabel(std::string_view name) const
{
    return baseStore.getLabel(name);
}
