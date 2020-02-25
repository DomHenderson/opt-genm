#include <algorithm>
#include <iostream>
#include <sstream>
#include <vector>

#include "core/constant.h"
#include "core/insts_call.h"
#include "flownode.h"
#include "storagepool.h"
#include "utilities.h"

using Inst_iterator = FlowNode::Inst_iterator;
using Block_iterator = FlowNode::Block_iterator;
using Func_iterator = FlowNode::Func_iterator;

Frame &CreateBaseFrame(Func &func, SymExPool &pool);
std::unique_ptr<LogStore> CreateLogStore(FlowNode &previous, SymExPool &pool);
Inst_iterator NextInst(Inst_iterator i);

//-----------------------------------------------------------------------------
// FlowNode
//-----------------------------------------------------------------------------

FlowNode::FlowNode(
    Frame &frame,
    SymExPool &pool
) :
    currentFrame(frame),
    pool(pool)
{
}

SuccessorFlowNode *FlowNode::CreateBlockNode(Block &block)
{
    return new SuccessorFlowNode(
        block.begin(),
        get_frame(),
        *this,
        pool
    );
}

SuccessorFlowNode *FlowNode::CreateFunctionNode(
    Func &func,
    std::vector<SymValue*> args,
    Inst_iterator caller
) {
    return new SuccessorFlowNode(
        func.begin()->begin(),
        *pool.persist(new Frame(
            args,
            &get_frame(),
            &*caller,
            NextInst(caller)
        )),
        *this,
        pool
    );
}

SuccessorFlowNode *FlowNode::CreateTailCallNode(
    Func &func,
    std::vector<SymValue*> args
) {
    return new SuccessorFlowNode(
        func.begin()->begin(),
        *pool.persist(new Frame(
            args,
            get_frame().get_previous(),
            get_frame().get_caller(),
            get_frame().get_resume_inst()
        )),
        *this,
        pool
    );
}

void FlowNode::AllocateResult(
    Inst *inst,
    SymValue *value
) {
    if(inst->IsVoid()) {
        std::cout<<"WARNING: Assigning result to void instruction"<<std::endl;
    } else if(inst->GetType(0) != value->get_type()) {
        std::cout<<"Copy casting from "<<toString(value->get_type())<<" to "<<toString(inst->GetType(0))<<std::endl;
        value = pool.persist(
            value->copy_cast(inst->GetType(0))
        );
    }
    vreg_allocs[inst] = values.size();
    values.push_back(value);
}

Frame &FlowNode::get_frame()
{
    return currentFrame;
}

SymValue *FlowNode::GetRegister(ConstantReg::Kind reg)
{
    auto iter = registers.find(reg);
    if(iter == registers.end()) {
        registers[reg] = pool.persist(new UnknownSymValue(Type::U64));
        return registers[reg];
    }
    return iter->second;
}

void FlowNode::SetRegister(
    ConstantReg::Kind reg,
    SymValue *value
) {
    registers[reg] = value;
}

//-----------------------------------------------------------------------------
// SuccessorFlowNode
//-----------------------------------------------------------------------------

SuccessorFlowNode::SuccessorFlowNode(
    Inst_iterator startingInst,
    Frame &frame,
    FlowNode &previous,
    SymExPool &pool
) :
    FlowNode(frame, pool),
    startingInst(startingInst),
    previousNode(previous),
    dataStore(CreateLogStore(previous, pool))
{
}

SuccessorFlowNode *SuccessorFlowNode::CreateReturnNode()
{
    std::cout<<"Creating return node"<<std::endl;
    if(!get_frame().get_resume_inst().has_value()) {
        std::cout<<"Frame does not have resume inst"<<std::endl;
        return nullptr;
    }

    Frame &frame = *get_frame().get_previous();

    SuccessorFlowNode *node = new SuccessorFlowNode(
        get_frame().get_resume_inst().value(),
        frame,
        *this,
        pool
    );

    return node;
}

Block *SuccessorFlowNode::ResolvePhiBlocks(std::vector<Block*> blocks, bool includeSelf)
{
    if(includeSelf) {
        std::cout<<"Resolving phi"<<std::endl;
        Block *block = &*get_block();
        auto iter = std::find(blocks.begin(), blocks.end(), block);
        if(iter == blocks.end()) {
            std::cout<<"Delegating"<<std::endl;
            return previousNode.ResolvePhiBlocks(blocks);
        } else {
            std::cout<<"Resolved"<<std::endl;
            return *iter;
        }
    } else {
        std::cout<<"Looking at previous node for phi"<<std::endl;
        return previousNode.ResolvePhiBlocks(blocks, true);
    }
}

SymValue *SuccessorFlowNode::GetResult(Inst *inst)
{
    auto iter = vreg_allocs.find(inst);

    if(iter != vreg_allocs.end()) {
        return values[iter->second];
    }

    return previousNode.GetResult(inst);
}

LogStore &SuccessorFlowNode::get_store()
{
    return *dataStore;
}

std::string_view SuccessorFlowNode::AllocateHeapBlock(unsigned size)
{
    return dataStore->addHeapAtom(size);
}

Inst_iterator SuccessorFlowNode::get_starting_inst()
{
    return startingInst;
}

Block_iterator SuccessorFlowNode::get_block()
{
    return startingInst
        ->getParent()
        ->getIterator();
}

Func_iterator SuccessorFlowNode::get_func()
{
    return startingInst
        ->getParent()
        ->getParent()
        ->getIterator();
}

std::string SuccessorFlowNode::get_name()
{
    std::ostringstream stream;
    stream << static_cast<void*>(&*get_starting_inst())
        <<" "
        <<get_block()->GetName()
        <<" "
        <<get_func()->GetName();
    return stream.str();
}

//-----------------------------------------------------------------------------
// RootFlowNode
//-----------------------------------------------------------------------------

RootFlowNode::RootFlowNode(
    Func &func,
    Prog &prog,
    SymExPool &pool
) :
    FlowNode(CreateBaseFrame(func, pool), pool),
    func(func),
    baseStore(prog, pool),
    dataStore(baseStore, pool)
{
}

SuccessorFlowNode *RootFlowNode::CreateReturnNode()
{
    std::cout<<"Returning from root"<<std::endl;
    return nullptr;
}

Block *RootFlowNode::ResolvePhiBlocks(std::vector<Block*> blocks, bool includeSelf)
{
    if(includeSelf) {
        std::cout<<"Reached root resolving phi"<<std::endl;
        Block *block = &*get_block();
        auto iter = std::find(blocks.begin(), blocks.end(), block);
        if(iter == blocks.end()) {
            std::cout<<"Failed to resolve"<<std::endl;
            return nullptr;
        } else {
            std::cout<<"Resolved"<<std::endl;
            return *iter;
        }
    } else {
        std::cout<<"No previous node with which to resolve phi"<<std::endl;
        return nullptr;
    }
}

SymValue *RootFlowNode::GetResult(Inst *inst)
{
    std::cout<<"Getting result for "<<toString(inst)<<std::endl;
    auto iter = vreg_allocs.find(inst);

    if(iter != vreg_allocs.end()) {
        std::cout<<"Found "<<toString(values[iter->second])<<std::endl;
        return values[iter->second];
    }

    std::cout<<"Result not found for "<<toString(inst)<<std::endl;
    if(inst->IsVoid()) {
        return nullptr;
    } else {
        UnknownSymValue *result = pool.persist(new UnknownSymValue(inst->GetType(0)));
        if(inst->GetNumRets()>1) std::cout<<"Instruction had more than one return value"<<std::endl;
        AllocateResult(inst, result);
        return result;
    }
}

LogStore &RootFlowNode::get_store()
{
    return dataStore;
}

std::string_view RootFlowNode::AllocateHeapBlock(unsigned size)
{
    return dataStore.addHeapAtom(size);
}

Inst_iterator RootFlowNode::get_starting_inst()
{
    return func.begin()->begin();
}

Block_iterator RootFlowNode::get_block()
{
    return func.begin();
}

Func_iterator RootFlowNode::get_func()
{
    return func.getIterator();;
}

std::string RootFlowNode::get_name()
{
    std::ostringstream stream;
    stream << "Base "
        <<static_cast<void*>(&*get_starting_inst())
        <<" "
        <<get_block()->GetName()
        <<" "
        <<get_func()->GetName();
    return stream.str();
}

//-----------------------------------------------------------------------------

Frame &CreateBaseFrame(
    Func &func, 
    SymExPool &pool
) {
    std::vector<SymValue*> args(func.params().size());
    std::transform(
        func.params().begin(), func.params().end(),
        args.begin(),
        [](auto t) {return new UnknownSymValue(t);}
    );
    
    return *pool.persist(new Frame(
        args,
        nullptr,
        nullptr,
        std::nullopt
    ));
}

std::unique_ptr<LogStore> CreateLogStore(FlowNode &previous, SymExPool &pool)
{
    DataStore &store = previous.get_store();
    LogStore *newStore = new LogStore(store, pool);
    return std::unique_ptr<LogStore>(newStore);
}

//Make a copy of an iterator and increment it
Inst_iterator NextInst(Inst_iterator i)
{
    std::cout<<"Attempting to increment iterator"<<std::endl;
    std::cout<<"Current value "<<toString(*i)<<std::endl;
    ++i;
    std::cout<<"Incremented value "<<toString(*i)<<std::endl;
    return i;
}
