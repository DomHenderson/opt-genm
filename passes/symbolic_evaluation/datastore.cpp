#include <iostream>

#include <llvm/ADT/StringRef.h>

#include "core/atom.h"
#include "core/data.h"
#include "core/prog.h"
#include "datastore.h"
#include "storagepool.h"
#include "symvalue.h"
#include "symvaluecomp.h"
#include "utilities.h"

void MappedAtom::add(Item *item)
{
    if(ended) {
        std::cout<<"Ignoring post-end ";
        switch(item->GetKind()) {
        case Item::Kind::ALIGN: std::cout<<"Align"<<std::endl; break;
        case Item::Kind::END: std::cout<<"End"<<std::endl; break;
        case Item::Kind::FLOAT64: std::cout<<"Float64"<<std::endl; break;
        case Item::Kind::INT16: std::cout<<"Int16"<<std::endl; break;
        case Item::Kind::INT32: std::cout<<"Int32"<<std::endl; break;
        case Item::Kind::INT64: std::cout<<"Int64"<<std::endl; break;
        case Item::Kind::INT8: std::cout<<"Int8"<<std::endl; break;
        case Item::Kind::SPACE: std::cout<<"Space"<<std::endl; break;
        case Item::Kind::STRING: std::cout<<"String"<<std::endl; break;
        case Item::Kind::SYMBOL: std::cout<<"Symbol"<<std::endl; break;
        }
        return;        
    }
    std::cout<<"adding ";
    switch(item->GetKind()) {
    case Item::Kind::ALIGN: { std::cout<<"align"<<std::endl;
        unsigned align = item->GetAlign();
        unsigned mod = next % align;
        unsigned shift = (align - mod) % align;
        next += shift;
    } break;
    
    case Item::Kind::END: std::cout<<"end"<<std::endl;
        items[next] = MappedItem();
        previous = next;
        ended = true;
        break;
    
    case Item::Kind::FLOAT64: std::cout<<"float64"<<std::endl;
        items[next] = MappedItem(
            pool.persist(new UnknownSymValue(Type::F64)),
            8
        );
        previous = next;
        next += 8;
        break;
    
    case Item::Kind::INT16: std::cout<<"int16"<<std::endl;
        items[next] = MappedItem(
            pool.persist(new IntSymValue(item->GetInt16(), Type::U16)),
            2
        );
        previous = next;
        next += 2;
        break;
    
    case Item::Kind::INT32: std::cout<<"int32"<<std::endl;
        items[next] = MappedItem(
            pool.persist(new IntSymValue(item->GetInt32(), Type::U32)),
            4
        );
        previous = next;
        next += 4;
        break;

    case Item::Kind::INT64: std::cout<<"int64"<<std::endl;
        items[next] = MappedItem(
            pool.persist(new IntSymValue(item->GetInt64(), Type::U64)),
            8
        );
        previous = next;
        next += 8;
        break;

    case Item::Kind::INT8: std::cout<<"int8"<<std::endl;
        items[next] = MappedItem(
            pool.persist(new IntSymValue(item->GetInt8(), Type::U8)),
            1
        );
        previous = next;
        next += 1;
        break;
    
    case Item::Kind::SPACE: std::cout<<"space"<<std::endl;
        next += item->GetSpace();
        break;
    
    case Item::Kind::STRING: std::cout<<"string"<<std::endl;
        items[next] = MappedItem(
            //Type?
            pool.persist(new StringSymValue(item->GetString(), Type::U64)),
            item->GetString().size()
        );
        previous = next;
        next += item->GetString().size();
        break;
    
    case Item::Kind::SYMBOL: { std::cout<<"symbol"<<std::endl;
        Global *g = item->GetSymbol();
        SymValue *value;
        switch(g->GetKind()) {
        case Global::Kind::ATOM: {
            Atom *a = static_cast<Atom*>(g);
            value = new AddrSymValue(a->GetName(), 0, Type::U64);
        } break;
        case Global::Kind::BLOCK: {
            Block *b = static_cast<Block*>(g);
            std::cout<<"Block "<<b->GetName()<<std::endl;
            value = new UnknownSymValue(Type::U64);
        } break;
        case Global::Kind::EXTERN: {
            Extern *e = static_cast<Extern*>(g);
            std::cout<<"Extern "<<e->GetName()<<std::endl;
            value = new UnknownSymValue(Type::U64);
        } break;
        case Global::Kind::FUNC: {
            Func *f = static_cast<Func*>(g);
            value = new FuncRefSymValue(f->GetName(), Type::U64);
        } break;
        case Global::Kind::SYMBOL: {
            Symbol *s = static_cast<Symbol*>(g);
            std::cout<<"Symbol "<<s->GetName()<<std::endl;
        } break;
        }
        items[next] = MappedItem(
            pool.persist(value),
            8
        );
        previous = next;
        next += 8;
    } break;
    }
    std::cout<<"Previous: "<<previous<<" Next: "<<next<<std::endl;
}

SymValue *MappedAtom::get(int offset, Type type)
{
    std::cout<<"getting"<<std::endl;
    if(offset < 0) {
        std::cout<<"Negative offset "<<offset<<std::endl;
        return new UnknownSymValue(type);
    }
    int idx = -1;
    for(const auto &[k,v]: items) {
        if(offset < k) {
            break;
        }
        idx = k;
    }
    if(idx == -1) {
        std::cout<<"Non-negative offset "
            <<offset
            <<" before first item "
            <<items.begin()->first
            <<std::endl;
            return new UnknownSymValue(type);
    } else {
        auto &item = items[idx];
        if(item.get_kind() == MappedItem::Kind::END) {
            std::cout<<"Attemptd to read at or beyond end (offset "
                <<offset
                <<")"
                <<std::endl;
            return new UnknownSymValue(type);
        } else {
            SymValue *value = item.get_value();
            std::cout<<"Found ";
            switch(value->get_kind()) {
            case SymValue::Kind::ADDR: std::cout<<"addr"; break;
            case SymValue::Kind::BOOL: std::cout<<"bool"; break;
            case SymValue::Kind::FLOAT: std::cout<<"float"; break;
            case SymValue::Kind::FUNCREF: std::cout<<"funcref"; break;
            case SymValue::Kind::INT: std::cout<<"int"; break;
            case SymValue::Kind::STR: std::cout<<"str"; break;
            case SymValue::Kind::UNKNOWN: std::cout<<"unknown"; break;
            }
            if(value->get_type()==type) {
                std::cout<<" of correct type"<<std::endl;
            } else {
                std::cout<<" of incorrect type. Expected "<<toString(type)<<", got "<<toString(value->get_type())<<std::endl;
                std::cout<<(typeLength(type)==typeLength(value->get_type())?"types same length":"not same length")<<std::endl;
            }
            return value;
        }
    }
    
}

//-----------------------------------------------------------------------------

BaseStore::BaseStore(
    Prog &prog,
    SymExPool &storagePool
): 
    storagePool(storagePool)
{
    for(auto &data: prog.data()) {
        for(auto &atom: data) {
            std::string_view name = atom.GetName();
            MappedAtom mappedAtom(storagePool);
            std::cout<<"New atom "<<name<<std::endl;
            for(auto item: atom) {
                mappedAtom.add(item);
            }
            atoms.emplace(name,std::move(mappedAtom));
        }
    }
}

SymValue *BaseStore::read(SymValue *loc, size_t loadSize, Type type, bool)
{
    switch(loc->get_kind()) {
    case SymValue::Kind::ADDR: {
        AddrSymValue *addr = static_cast<AddrSymValue*>(loc);
        auto a = atoms.find(addr->get_name());
        MappedAtom *atom = a == atoms.end()
            ? nullptr
            : &(a->second);
        std::cout<<"Reading from atom "
            <<(atom==nullptr?"not found":addr->get_name())
            <<std::endl;
        std::cout<<"Reading at offset of "<<addr->get_offset()<<std::endl;
        return storagePool.persist(
            (atom==nullptr
                ? new UnknownSymValue(type)
                : atom->get(addr->get_offset(), type)
            )
        );
    } break;
    default:
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

//-----------------------------------------------------------------------------

LogStore::LogStore(
    DataStore &store
):
    baseStore(store)
{
    std::cout<<"Correct constructor"<<std::endl;
}

SymValue *LogStore::read(SymValue *loc, size_t loadSize, Type type, bool record)
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
        std::cout<<"log read delegated"<<std::endl;
        value = baseStore.read(loc, loadSize, type, false);
    }
    if(record) {
        std::cout<<"Recording read"<<std::endl;
        actions.push_back(std::make_unique<Read>(loc, value));
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
