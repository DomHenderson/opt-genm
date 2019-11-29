#pragma once

#include <memory>
#include <unordered_set>
#include <utility>

#include "symvalue.h"

class ValuePool {
public:
    void persist_value(std::unique_ptr<SymValue> value) {
        pool.insert(std::move(value));
    }
private:
    std::unordered_set<std::unique_ptr<SymValue>> pool;
};