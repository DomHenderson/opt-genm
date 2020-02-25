#include <iostream>
#include <limits>

#include "mappedatom.h"
#include "storagepool.h"
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

std::vector<SymValue*> MappedAtom::getAll()
{
    return std::vector<SymValue*>(); //TODO do this
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