#pragma once

#include <iostream>
#include <unordered_map>
#include <vector>

#include <llvm/ADT/ilist.h>

#include "core/block.h"
#include "core/func.h"
#include "core/inst.h"
#include "location.h"
#include "symvalue.h"

class Frame;

struct FlowNode {
    using inst_iterator = llvm::ilist<Inst>::iterator;
    using block_iterator = llvm::ilist<Block>::iterator;
    using func_iterator = llvm::ilist<Func>::iterator;

    FlowNode() {}

    void AllocateResult(Inst *inst, SymValue *value);
    SymValue *GetResult(Inst *inst);

    Location location;

    FlowNode *previousNode;

    Frame *currentFrame;

    std::unordered_map<Inst*,unsigned> reg_allocs;
    std::vector<SymValue*> values;

    SymValue *Load(SymValue *loc) {std::cout<<"Load undefined"<<std::endl; return nullptr; }
    void Store(SymValue *addr, SymValue *value) {std::cout<<"Store undefined"<<std::endl;}
    std::vector<std::pair<SymValue*, SymValue*>> stores;

    std::string_view name;
};