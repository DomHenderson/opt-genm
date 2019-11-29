#pragma once

#include <unordered_map>
#include <vector>

#include <llvm/ADT/ilist.h>

#include "core/inst.h"
#include "symvalue.h"

class Frame;

class FlowNode {
private:
    using inst_iterator = llvm::ilist<Inst>::iterator;

public:
    FlowNode(inst_iterator startInst) : program_counter(startInst) {}
    Inst *get_current_inst();

    void AllocateResult(Inst *inst, SymValue *value);
    SymValue *GetResult(Inst* inst);

private:
    inst_iterator program_counter;

    FlowNode *previousNode;

    Frame *currentFrame;

    std::unordered_map<Inst*,unsigned> reg_allocs;
    std::vector<SymValue*> values;
};