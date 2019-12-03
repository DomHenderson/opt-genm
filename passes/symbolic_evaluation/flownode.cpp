#include "flownode.h"

void FlowNode::AllocateResult(Inst *inst, SymValue *value)
{
    reg_allocs[inst] = values.size();
    values.push_back(value);
}

SymValue *FlowNode::GetResult(Inst *inst)
{
    auto iter = reg_allocs.find(inst);

    if(iter != reg_allocs.end()) {
        return values[iter->second];
    }

    if(previousNode == nullptr) {
        return nullptr;
    }

    return previousNode->GetResult(inst);
}