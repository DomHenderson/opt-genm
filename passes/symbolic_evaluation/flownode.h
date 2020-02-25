#pragma once

#include <iostream>
#include <optional>
#include <unordered_map>
#include <vector>

#include <llvm/ADT/ilist.h>

#include "z3++.h"

#include "core/block.h"
#include "core/constant.h"
#include "core/func.h"
#include "core/insts_call.h"
#include "datastore.h"
#include "frame.h"
#include "symvalue.h"

class SuccessorFlowNode;
class SymExPool;

class FlowNode {
public:
    using Inst_iterator = llvm::ilist<Inst>::iterator;
    using Block_iterator = llvm::ilist<Block>::iterator;
    using Func_iterator = llvm::ilist<Func>::iterator;

    FlowNode(Frame &frame, SymExPool &pool, z3::context &context, z3::expr pathConstraint);
    virtual ~FlowNode() {}

    virtual SuccessorFlowNode *CreateBlockNode(
        Block &block,
        std::optional<z3::expr> constraint = std::nullopt
    );
    virtual SuccessorFlowNode *CreateFunctionNode(
        Func &func,
        std::vector<SymValue*> args,
        Inst_iterator caller
    );
    virtual SuccessorFlowNode *CreateReturnNode() = 0;
    virtual SuccessorFlowNode *CreateTailCallNode(
        Func &func,
        std::vector<SymValue*> args
    );

    virtual SymValue *ResolvePhi(PhiInst *phiInst, bool includeSelf = false) = 0;

    virtual SymValue *CreateSymValue(Value *value, Type type);
    virtual void AllocateResult(Inst *inst, SymValue *value);
    virtual SymValue *GetResult(Inst *inst) = 0;

    virtual LogStore &get_store() const = 0;

    virtual std::string_view AllocateHeapBlock(unsigned size) = 0;

    virtual Inst_iterator get_starting_inst() const = 0;
    virtual Block_iterator get_block() const = 0;
    virtual Func_iterator get_func() const = 0;

    virtual Frame &get_frame() const;

    virtual z3::context &get_context() const;

    virtual z3::expr get_path_constraint() const;

    virtual std::string get_name() const = 0;

    virtual SymValue *GetRegister(ConstantReg::Kind reg);
    virtual void SetRegister(ConstantReg::Kind reg, SymValue* value);

    virtual void AddConstraint(z3::expr constraint);
    virtual void AssertStateConstraints(z3::solver &solver) const =0;
protected:
    virtual SymValue *TryLocalPhiResolution(PhiInst *phiInst);

    std::unordered_map<Inst*,unsigned> vreg_allocs;
    std::vector<SymValue*> values;
    std::unordered_map<ConstantReg::Kind,SymValue*> registers;
    z3::expr pathConstraint;
    std::vector<z3::expr> constraints;
    Frame &currentFrame;
    SymExPool &pool;
    z3::context &context;
};

class SuccessorFlowNode : public FlowNode {
public:
    SuccessorFlowNode(
        Inst_iterator startingInst,
        Frame &frame,
        FlowNode &previous,
        SymExPool &pool,
        z3::context &context,
        z3::expr pathConstraint
    );
    ~SuccessorFlowNode() = default;

    virtual SuccessorFlowNode *CreateReturnNode() override;

    virtual SymValue *ResolvePhi(PhiInst *phiInst, bool includeSelf = false) override;

    virtual SymValue *GetResult(Inst *inst) override;

    virtual LogStore &get_store() const override;

    virtual std::string_view AllocateHeapBlock(unsigned size) override;

    virtual Inst_iterator get_starting_inst() const override;
    virtual Block_iterator get_block() const override;
    virtual Func_iterator get_func() const override;

    virtual std::string get_name() const override;

    virtual void AssertStateConstraints(z3::solver &solver) const override;
private:
    Inst_iterator startingInst;

    FlowNode &previousNode;

    std::unique_ptr<LogStore> dataStore;
};

class JoinFlowNode: public FlowNode {
public:
    JoinFlowNode(
        Inst_iterator startInst,
        Frame &frame,
        FlowNode &previous1,
        FlowNode &previous2,
        SymExPool &pool,
        z3::context &context
    );
    ~JoinFlowNode() = default;

    virtual SuccessorFlowNode *CreateReturnNode() override;
    virtual SymValue *ResolvePhi(PhiInst *phiInst, bool includeSelf = false) override;
    virtual SymValue *GetResult(Inst *inst) override;
    virtual LogStore &get_store() const override;
    virtual std::string_view AllocateHeapBlock(unsigned size) override;

    virtual Inst_iterator get_starting_inst() const override;
    virtual Block_iterator get_block() const override;
    virtual Func_iterator get_func() const override;

    virtual std::string get_name() const override;

    virtual void AssertStateConstraints(z3::solver &solver) const override;
private:
    Inst_iterator startingInst;
    FlowNode *previous[2];
    std::unique_ptr<LogStore> dataStore;
};

class RootFlowNode : public FlowNode {
public:
    RootFlowNode(Func &func, Prog &prog, SymExPool &pool, z3::context &context);
    ~RootFlowNode() = default;

    virtual SuccessorFlowNode *CreateReturnNode() override;

    virtual SymValue *ResolvePhi(PhiInst *phiInst, bool includeSelf = false) override;

    virtual SymValue *GetResult(Inst *inst) override;

    virtual LogStore &get_store() const override;

    virtual std::string_view AllocateHeapBlock(unsigned size) override;

    virtual Inst_iterator get_starting_inst() const override;
    virtual Block_iterator get_block() const override;
    virtual Func_iterator get_func() const override;

    virtual std::string get_name() const override;

    virtual void AssertStateConstraints(z3::solver &solver) const override;
private:
    Func &func;
    
    BaseStore baseStore;
    std::unique_ptr<LogStore> dataStore;
};
