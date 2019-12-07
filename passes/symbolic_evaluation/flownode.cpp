#include <iostream>
#include <sstream>

#include "core/insts_call.h"
#include "flownode.h"
#include "storagepool.h"

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
    if(!get_frame().get_resume_inst().has_value()) {
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

SymValue *RootFlowNode::GetResult(Inst *inst)
{
    auto iter = reg_allocs.find(inst);

    if(iter != reg_allocs.end()) {
        return values[iter->second];
    }

    std::cout<<"Result not found"<<std::endl;
    UnknownSymValue *result = pool.persist(new UnknownSymValue());
    AllocateResult(inst, pool.persist(new UnknownSymValue()));
    return result;
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
    unsigned argCount = func.params().size();

    std::vector<SymValue*> args;
    args.reserve(argCount);

    for(unsigned i = 0; i < argCount; ++i) {
        SymValue *value = new UnknownSymValue();
        pool.persist(value);
        args.push_back(value);
    }
    
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
    return ++i;
}
