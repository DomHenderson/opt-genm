#pragma once

#include <map>

#include <llvm/ADT/APInt.h>

#include "core/type.h"

class SymExPool;
class SymValue;

class MappedAtom {
public:
    MappedAtom(SymExPool &pool): pool(pool) {} 
    SymValue *get(unsigned offset, size_t loadSize, Type type);
    SymValue *get(llvm::APInt offset, size_t loadSize, Type type);
    std::vector<SymValue*> getAll();

    void addSymValue(SymValue &symValue);
    void addSpace(unsigned space);

    unsigned getSize() const;
private:
    std::pair<unsigned, unsigned> FindSection(unsigned offset) const;

    std::map<unsigned,SymValue*> items;

    unsigned size = 0;

    bool invalid = false;

    SymExPool &pool;
};
