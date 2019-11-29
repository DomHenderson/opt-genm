#pragma once

#include <memory>
#include <unordered_set>
#include <utility>

#include "symvalue.h"

class ValuePool {
public:
    void persist(SymValue* value) {
        pool.insert(std::unique_ptr<SymValue>(value));
    }
private:
    std::unordered_set<std::unique_ptr<SymValue>> pool;
};