#include <algorithm>
#include <iostream>
#include <sstream>
#include <vector>

#include "z3++.h"

#include "core/constant.h"
#include "core/insts.h"
#include "core/insts_call.h"
#include "flownode.h"
#include "storagepool.h"
#include "symvaluecomp.h"
#include "utilities.h"

using Inst_iterator = FlowNode::Inst_iterator;
using Block_iterator = FlowNode::Block_iterator;
using Func_iterator = FlowNode::Func_iterator;

Frame &CreateBaseFrame(Func &func, SymExPool &pool);
std::unique_ptr<LogStore> CreateLogStore(FlowNode &previous, SymExPool &pool);
std::unique_ptr<LogStore> CreateJoinLogStore(FlowNode &previousLeft, FlowNode &previousRight, SymExPool &pool);
Inst_iterator NextInst(Inst_iterator i);

//-----------------------------------------------------------------------------
// FlowNode
//-----------------------------------------------------------------------------

FlowNode::FlowNode(
    Frame &frame,
    SymExPool &pool,
    z3::context &context,
    z3::expr pathConstraint
) :
    currentFrame(frame),
    pool(pool),
    context(context),
    pathConstraint(pathConstraint)
{
}

SuccessorFlowNode *FlowNode::CreateBlockNode(Block &block, std::optional<z3::expr> constraint)
{
    auto newPathConstraint = constraint.has_value()
        ? pathConstraint && *constraint
        : pathConstraint;

    auto node = new SuccessorFlowNode(
        block.begin(),
        get_frame(),
        *this,
        pool,
        context,
        newPathConstraint
    );

    return node;
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
        pool,
        context,
        pathConstraint
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
        pool,
        context,
        pathConstraint
    );
}

SymValue *FlowNode::CreateSymValue(
    Value *value,
    Type type
) {
    SymValue *result;
    switch(value->GetKind()) {
    case Value::Kind::GLOBAL: {
        auto g = static_cast<Global*>(value);
        switch(g->GetKind()) {
        case Global::Kind::ATOM: {
            LogDetail<<"Allocating global atom (label) "<<g->GetName()<<End();
            const Label *label = get_store().getLabel(g->GetName());
            if(label == nullptr) {
                LogWarning<<"Attempted to create address symvalue to unknown label"<<End();
            } else {
                result = pool.persist(new StaticPtrSymValue(
                    g->GetName(),
                    llvm::APInt(bitLength(type),label->offset, isSigned(type)),
                    label->atom->getSize(),
                    type
                ));
            }
        } break;
        case Global::Kind::BLOCK:
            LogDetail<<"Alocating global block "<<g->GetName()<<End();
            result = pool.persist(new BlockRefSymValue(g->GetName(), type));
            break;

        case Global::Kind::EXTERN:
            LogDetail<<"Allocating global extern "<<g->GetName()<<End();
            result = pool.persist(new ExternSymValue(g->GetName(), type));
            break;

        case Global::Kind::FUNC:
            LogDetail<<"Allocating global func "<<g->GetName()<<End();
            result = pool.persist(new FuncRefSymValue(g->GetName(), type));
            break;
        
        default:
            LogDetail<<"Allocating unknown global "<<g->GetName()<<End();
            result = pool.persist(new UnknownSymValue(type));
            break;
        }
    } break;

    case Value::Kind::INST: {
        LogDetail<<"Allocating inst"<<End();
        auto i = static_cast<Inst*>(value);
        result = GetResult(i);
    } break;

    case Value::Kind::CONST: {
        Constant *c = static_cast<Constant*>(value);
        switch(c->GetKind()) {
        case Constant::Kind::INT: {
            ConstantInt *ci = static_cast<ConstantInt*>(c);
            result = new IntSymValue(ci->GetValue(), type);
            if(!isIntType(type)) { LogWarning<<"Allocating constant int of non-int type"<<End(); }
            pool.persist(result);
            LogDetail<<"Allocating int "<<ci->GetValue()<<End();
        } break;
        
        case Constant::Kind::FLOAT: {
            ConstantFloat *cf = static_cast<ConstantFloat*>(c);
            result = new FloatSymValue(cf->GetValue(), type);
            if(isIntType(type)) { LogWarning<<"Allocating constant float of non-float type"<<End(); }
            pool.persist(result);
            LogDetail<<"Allocating float "<<cf->GetValue()<<End();
        } break;

        default:
            result = new UnknownSymValue(type);
            pool.persist(result);
            LogDetail<<"Allocating const"<<End();
            break;
        }
    } break;

    case Value::Kind::EXPR: {
        result = new UnknownSymValue(type);
        pool.persist(result);
        LogDetail<<"Allocating expr"<<End();
    }
    }
    return result;
}

void FlowNode::AllocateResult(
    Inst *inst,
    SymValue *value
) {
    if(inst->IsVoid()) {
        LogWarning<<"Assigning result to void instruction"<<End();
    } else if(inst->GetType(0) != value->get_type()) {
        LogDetail<<"Copy casting from "<<toString(value->get_type())<<" to "<<toString(inst->GetType(0))<<End();
        value = pool.persist(
            value->copy_cast(inst->GetType(0))
        );
    }
    vreg_allocs[inst] = values.size();
    values.push_back(value);
}

Frame &FlowNode::get_frame() const
{
    return currentFrame;
}

z3::context &FlowNode::get_context() const
{
    return context;
}

z3::expr FlowNode::get_path_constraint() const
{
    return pathConstraint;
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

void FlowNode::AddConstraint(
    z3::expr constraint
) {
    constraints.push_back(constraint);
}

SymValue *FlowNode::TryLocalPhiResolution(PhiInst *phiInst)
{
    Block *block = &*get_block();

    for(int i = 0; i < phiInst->GetNumIncoming(); ++i) {
        if(phiInst->GetBlock(i) == block) {
            return CreateSymValue(
                phiInst->GetValue(block),
                phiInst->GetType()
            );
        }
    }
    return nullptr;
}

//-----------------------------------------------------------------------------
// SuccessorFlowNode
//-----------------------------------------------------------------------------

SuccessorFlowNode::SuccessorFlowNode(
    Inst_iterator startingInst,
    Frame &frame,
    FlowNode &previous,
    SymExPool &pool,
    z3::context &context,
    z3::expr pathConstraint
) :
    FlowNode(frame, pool, context, pathConstraint),
    startingInst(startingInst),
    previousNode(previous),
    dataStore(CreateLogStore(previous, pool))
{
}

SuccessorFlowNode *SuccessorFlowNode::CreateReturnNode()
{
    LogDetail<<"Creating return node"<<End();
    if(!get_frame().get_resume_inst().has_value()) {
        LogFlow<<"Frame does not have resume inst"<<End();
        return nullptr;
    }

    Frame &frame = *get_frame().get_previous();

    SuccessorFlowNode *node = new SuccessorFlowNode(
        get_frame().get_resume_inst().value(),
        frame,
        *this,
        pool,
        context,
        pathConstraint
    );

    return node;
}

SymValue *SuccessorFlowNode::ResolvePhi(PhiInst *phiInst, bool includeSelf)
{
    if(includeSelf) {
        //std::cout<<"Resolving phi"<<std::endl;
        LogDetail<<"Resolving phi"<<End();
        //std::cout<<"Trying local phi resolution"<<std::endl;
        SymValue *symvalue = TryLocalPhiResolution(phiInst);
        //std::cout<<"Found "<<toString(symvalue)<<std::endl;
        return symvalue == nullptr
            ? previousNode.ResolvePhi(phiInst, true)
            : symvalue;
    } else {
        LogDetail<<"Looking at previous node for phi"<<End();
        return previousNode.ResolvePhi(phiInst, true);
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

LogStore &SuccessorFlowNode::get_store() const
{
    return *dataStore;
}

std::string_view SuccessorFlowNode::AllocateHeapBlock(unsigned size)
{
    return dataStore->addHeapAtom(size);
}

Inst_iterator SuccessorFlowNode::get_starting_inst() const
{
    return startingInst;
}

Block_iterator SuccessorFlowNode::get_block() const
{
    return startingInst
        ->getParent()
        ->getIterator();
}

Func_iterator SuccessorFlowNode::get_func() const
{
    return startingInst
        ->getParent()
        ->getParent()
        ->getIterator();
}

std::string SuccessorFlowNode::get_name() const
{
    std::ostringstream stream;
    stream << static_cast<void*>(&*get_starting_inst())
        <<" "
        <<get_block()->GetName()
        <<" "
        <<get_func()->GetName();
    return stream.str();
}

void SuccessorFlowNode::AssertStateConstraints(
    z3::solver &solver
) const {
    for(auto& c: constraints) {
        solver.add(implies(pathConstraint, c));
    }
    previousNode.AssertStateConstraints(solver);
}

//-----------------------------------------------------------------------------
// JoinFlowNode
//-----------------------------------------------------------------------------

JoinFlowNode::JoinFlowNode(
    Inst_iterator startingInst,
    Frame &frame,
    FlowNode &previous1,
    FlowNode &previous2,
    SymExPool &pool,
    z3::context &context
):
    FlowNode(frame, pool, context, (previous1.get_path_constraint() || previous2.get_path_constraint())),
    startingInst(startingInst),
    previous{&previous1, &previous2},
    dataStore(CreateJoinLogStore(previous1, previous2, pool))
{
}

SuccessorFlowNode *JoinFlowNode::CreateReturnNode()
{
    LogDetail<<"Creating return node"<<End();
    if(!get_frame().get_resume_inst().has_value()) {
        LogFlow<<"Frame does not have resume inst"<<End();
        return nullptr;
    }

    Frame &frame = *get_frame().get_previous();

    SuccessorFlowNode *node = new SuccessorFlowNode(
        get_frame().get_resume_inst().value(),
        frame,
        *this,
        pool,
        context,
        pathConstraint
    );

    return node;
}

SymValue *JoinFlowNode::ResolvePhi(PhiInst *phiInst, bool includeSelf)
{
    //std::cout<<"Resolving join phi"<<std::endl;
    if(includeSelf) {
        //std::cout<<"Included self"<<std::endl;
        LogDetail<<"Resolving phi"<<End();
        SymValue *symvalue = TryLocalPhiResolution(phiInst);
        if(symvalue != nullptr) {
            //std::cout<<"Found"<<std::endl;
            return symvalue;
        }
    }
    //std::cout<<"Delegating"<<std::endl;
    LogDetail<<"Delegating (split)"<<End();
    //std::cout<<"left ("<<previous[0]->get_name()<<")"<<std::endl;
    SymValue *left = previous[0]->ResolvePhi(phiInst, true);
    //std::cout<<"right ("<<previous[1]->get_name()<<")"<<std::endl;
    SymValue *right = previous[1]->ResolvePhi(phiInst, true);
    //std::cout<<"Finished recursion"<<std::endl;
    //std::cout<<"Left:"<<std::endl;
    //std::cout<<"  "<<toString(left)<<std::endl;
    //std::cout<<"Right:"<<std::endl;
    //std::cout<<"  "<<toString(right)<<std::endl;
    //std::cout<<"Comparing"<<std::endl;
    auto [result, constraint] = SymComp::EQ(left, right, this);
    //std::cout<<"compared"<<std::endl;
    if(result == SymComp::Result::TRUE) {
        //std::cout<<"equal"<<std::endl;
        LogDetail<<"Values equal"<<End();
        LogDetail<<"   "<<toString(left)<<End();
        LogDetail<<"   "<<toString(right)<<End();
        return left;
    } else {
        //std::cout<<"unequal"<<std::endl;
        return pool.persist(
            new CondSymValue(
                left,
                right,
                previous[0]->get_path_constraint()
            )
        );
    }
}

SymValue *JoinFlowNode::GetResult(Inst *inst)
{
    //Active comparison strategy
    /*auto iter = vreg_allocs.find(inst);

    if(iter != vreg_allocs.end()) {
        return values[iter->second];
    }

    LogDetail<<"Splitting GetResult"<<End();

    SymValue *left = previous[0]->GetResult(inst);
    SymValue *right = previous[1]->GetResult(inst);

    if(SymComp::EQ(left, right, this).first == SymComp::Result::TRUE) {
        LogDetail<<"Found equal results"<<End();
        return left;
    } else {
        LogDetail<<"Creating conditional"<<End();
        return pool.persist(
            new CondSymValue(
                left,
                right,
                previous[0]->get_path_constraint()
            )
        );
    }*/
    //Passive strategy
    auto iter = vreg_allocs.find(inst);

    if(iter != vreg_allocs.end()) {
        return values[iter->second];
    }

    LogDetail<<"Splitting GetResult"<<End();

    SymValue *left = previous[0]->GetResult(inst);
    SymValue *right = previous[1]->GetResult(inst);

    LogDetail<<"Creating conditional"<<End();
    return pool.persist(
        new CondSymValue(
            left,
            right,
            previous[0]->get_path_constraint()
        )
    );
}

LogStore &JoinFlowNode::get_store() const
{
    return *dataStore;
}

std::string_view JoinFlowNode::AllocateHeapBlock(unsigned size)
{
    return dataStore->addHeapAtom(size);
}

Inst_iterator JoinFlowNode::get_starting_inst() const
{
    return startingInst;
}

Block_iterator JoinFlowNode::get_block() const
{
    return startingInst
        ->getParent()
        ->getIterator();
}

Func_iterator JoinFlowNode::get_func() const
{
    return startingInst
        ->getParent()
        ->getParent()
        ->getIterator();
}

std::string JoinFlowNode::get_name() const
{
    std::ostringstream stream;
    stream << static_cast<void*>(&*get_starting_inst())
        <<" "
        <<get_block()->GetName()
        <<" "
        <<get_func()->GetName()
        <<" join";
    return stream.str();
}

void JoinFlowNode::AssertStateConstraints(
    z3::solver &solver
) const {
    for(auto& c: constraints) {
        solver.add(implies(pathConstraint,c));
    }
    for(auto& p: previous) {
        p->AssertStateConstraints(solver);
    }
}

//-----------------------------------------------------------------------------
// RootFlowNode
//-----------------------------------------------------------------------------

RootFlowNode::RootFlowNode(
    Func &func,
    Prog &prog,
    SymExPool &pool,
    z3::context &context
) :
    FlowNode(CreateBaseFrame(func, pool), pool, context, context.bool_val(true)),
    func(func),
    baseStore(prog, pool),
    dataStore(std::make_unique<LogStore>(baseStore, pool))
{
}

SuccessorFlowNode *RootFlowNode::CreateReturnNode()
{
    LogFlow<<"Returning from root"<<End();
    return nullptr;
}

SymValue *RootFlowNode::ResolvePhi(PhiInst *phiInst, bool includeSelf)
{
    //std::cout<<"Reached root"<<std::endl;
    if(includeSelf) {
        LogDetail<<"Reached root resolving phi"<<End();
        //std::cout<<"Trying local"<<std::endl;
        SymValue *symvalue = TryLocalPhiResolution(phiInst);
        if(symvalue == nullptr) {
            LogWarning<<"failed to resolve phi"<<End();
        }
        //std::cout<<"Found "<<toString(symvalue)<<std::endl;
        return symvalue;
    } else {
        LogWarning<<"No previous node with which to resolve phi"<<End();
        return nullptr;
    }
}

SymValue *RootFlowNode::GetResult(Inst *inst)
{
    LogDetail<<"Getting result for "<<toString(inst)<<End();
    auto iter = vreg_allocs.find(inst);

    if(iter != vreg_allocs.end()) {
        LogDetail<<"Found "<<toString(values[iter->second])<<End();
        return values[iter->second];
    }

    LogDetail<<"Result not found for "<<toString(inst)<<End();
    if(inst->IsVoid()) {
        return nullptr;
    } else {
        UnknownSymValue *result = pool.persist(new UnknownSymValue(inst->GetType(0)));
        if(inst->GetNumRets()>1) LogWarning<<"Instruction has more than one return value"<<End();
        AllocateResult(inst, result);
        return result;
    }
}

LogStore &RootFlowNode::get_store() const
{
    return *dataStore;
}

std::string_view RootFlowNode::AllocateHeapBlock(unsigned size)
{
    return dataStore->addHeapAtom(size);
}

Inst_iterator RootFlowNode::get_starting_inst() const
{
    return func.begin()->begin();
}

Block_iterator RootFlowNode::get_block() const
{
    return func.begin();
}

Func_iterator RootFlowNode::get_func() const
{
    return func.getIterator();;
}

std::string RootFlowNode::get_name() const
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

void RootFlowNode::AssertStateConstraints(
    z3::solver &solver
) const {
    for(auto& c: constraints) {
        solver.add(implies(pathConstraint,c));
    }
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

std::unique_ptr<LogStore> CreateJoinLogStore(FlowNode &previousLeft, FlowNode &previousRight, SymExPool &pool)
{
    // DataStore &leftStore = previousLeft.get_store();
    // DataStore &rightStore = previousRight.get_store();
    // LogStore *newStore = new JoinStore(leftStore, rightStore, pool);
    // return std::unique_ptr<LogStore>(newStore);
    LogWarning<<"Join store not implemented"<<End();
    return std::unique_ptr<LogStore>();
}

//Make a copy of an iterator and increment it
Inst_iterator NextInst(Inst_iterator i)
{
    LogDetail<<"Attempting to increment iterator"<<End();
    LogDetail<<"Current value "<<toString(*i)<<End();
    ++i;
    LogDetail<<"Incremented value "<<toString(*i)<<End();
    return i;
}
