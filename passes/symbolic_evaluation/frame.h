#pragma once

#include <optional>
#include <stack>
#include <unordered_map>
#include <vector>

#include "core/inst.h"
#include "location.h"
#include "symvalue.h"

struct Frame {
    Frame(std::vector<SymValue*> args): args(args) {}
    std::vector<SymValue*> args;
    Frame *previous;
    // COMMENT!!!!!!!!!
    Inst *return_addr;
    // return_addr->getParent()
    std::optional<Location> resume_location = std::nullopt;
};