#include <iostream>
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

SymValue *FlowNode::Load(SymValue *loc)
{
     std::cout<<"Load undefined"<<std::endl;
     return nullptr;
}

void FlowNode::Store(SymValue *addr, SymValue *value)
{
     std::cout<<"Store undefined"<<std::endl;
}

SymValue *RootNode::Load(SymValue *loc)
{
     std::cout<<"Root node load undefined"<<std::endl;
     return nullptr;
}

void RootNode::Store(SymValue *addr, SymValue *value)
{
     std::cout<<"Root node store undefined"<<std::endl;
}