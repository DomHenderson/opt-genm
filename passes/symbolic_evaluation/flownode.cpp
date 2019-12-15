#include <algorithm>
#include <iostream>
#include <sstream>
#include <vector>

#include "core/insts_call.h"
#include "flownode.h"
#include "storagepool.h"
#include "utilities.h"

using Inst_iterator = FlowNode::Inst_iterator;
using Block_iterator = FlowNode::Block_iterator;
using Func_iterator = FlowNode::Func_iterator;

Frame &CreateBaseFrame(Func &func, SymExPool &pool);
std::unique_ptr<LogStore> CreateLogStore(FlowNode &previous);
Inst_iterator NextInst(Inst_iterator i);

//-----------------------------------------------------------------------------

FlowNode::FlowNode(Frame &frame) :
    currentFrame(frame)
{
}

SuccessorFlowNode *FlowNode::CreateBlockNode(Block &block)
{
    return new SuccessorFlowNode(
        block.begin(),
        get_frame(),
        *this
    );
}

SuccessorFlowNode *FlowNode::CreateFunctionNode(
    Func &func,
    std::vector<SymValue*> args,
    Inst_iterator caller,
    SymExPool &pool
) {
    return new SuccessorFlowNode(
        func.begin()->begin(),
        *pool.persist(new Frame(
            args,
            &get_frame(),
            &*caller,
            NextInst(caller)
        )),
        *this
    );
}

SuccessorFlowNode *FlowNode::CreateTailCallNode(
    Func &func,
    std::vector<SymValue*> args,
    SymExPool &pool
) {
    return new SuccessorFlowNode(
        func.begin()->begin(),
        *pool.persist(new Frame(
            args,
            get_frame().get_previous(),
            get_frame().get_caller(),
            get_frame().get_resume_inst()
        )),
        *this
    );
}

void FlowNode::AllocateResult(
    Inst *inst,
    SymValue *value
) {
    reg_allocs[inst] = values.size();
    values.push_back(value);
}

Frame &FlowNode::get_frame()
{
    return currentFrame;
}

//-----------------------------------------------------------------------------

SuccessorFlowNode::SuccessorFlowNode(
    Inst_iterator startingInst,
    Frame &frame,
    FlowNode &previous
) :
    FlowNode(frame),
    startingInst(startingInst),
    previousNode(previous),
    dataStore(CreateLogStore(previous))
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
        *this
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
    auto iter = reg_allocs.find(inst);

    if(iter != reg_allocs.end()) {
        return values[iter->second];
    }

    return previousNode.GetResult(inst);
}

LogStore &SuccessorFlowNode::get_store()
{
    return *dataStore;
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

RootFlowNode::RootFlowNode(
    Func &func,
    Prog &prog,
    SymExPool &pool
) :
    FlowNode(CreateBaseFrame(func, pool)),
    func(func),
    baseStore(prog, pool),
    dataStore(baseStore),
    pool(pool)
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
    auto iter = reg_allocs.find(inst);

    if(iter != reg_allocs.end()) {
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

std::unique_ptr<LogStore> CreateLogStore(FlowNode &previous)
{
    DataStore &store = previous.get_store();
    LogStore *newStore = new LogStore(store);
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
