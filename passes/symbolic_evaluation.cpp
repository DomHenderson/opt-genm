#include <algorithm>
#include <iostream>
#include <map>
#include <memory>
#include <optional>
#include <string>
#include <unordered_map>
#include <unordered_set>
#include <vector>

#include <llvm/ADT/APFloat.h>
#include <llvm/ADT/APInt.h>

#include "core/atom.h"
#include "core/block.h"
#include "core/constant.h"
#include "core/data.h"
#include "core/func.h"
#include "core/global.h"
#include "core/inst.h"
#include "core/insts.h"
#include "core/insts_call.h"
#include "core/insts_const.h"
#include "core/pass_manager.h"
#include "core/prog.h"
#include "core/value.h"
#include "passes/symbolic_evaluation.h"
#include "symbolic_evaluation/frame.h"
#include "symbolic_evaluation/storagepool.h"
#include "symbolic_evaluation/symvalue.h"
#include "symbolic_evaluation/symvaluecomp.h"
#include "symbolic_evaluation/utilities.h"

//-----------------------------------------------------------------------------

const char *SymbolicEvaluation::kPassID = "symbolic_evaluation";

SymbolicEvaluation::SymbolicEvaluation(PassManager *passManager) : Pass(passManager) {}

const char *SymbolicEvaluation::GetPassName() const
{
  return "Symbolic Evaluation";
}

void SymbolicEvaluation::Run(Prog *program)
{
    PrintCodeInfo(program);

    // ---------------------------------------------------------------
    prog = program;
    std::cout<<"Starting"<<std::endl;
    frontier.push(CreateRootNode());
    std::cout<<"Done"<<std::endl;

    std::cout<<std::endl;
    std::cout<<"----------------------------------------"<<std::endl;
    std::cout<<"EXECUTION START"<<std::endl;
    std::cout<<"----------------------------------------"<<std::endl;
    std::cout<<std::endl;

    while(!frontier.empty() && count < limit) {
        FlowNode *node = frontier.top();
        frontier.pop();
        std::cout<<"Stepping node "<<node->get_name()<<std::endl;
        StepNode(node);
    }

    if(!frontier.empty()) {
        std::cout<<"Finished early"<<std::endl;
    }

    std::cout<<std::endl;
    std::cout<<"----------------------------------------"<<std::endl;
    std::cout<<"EXECUTION END"<<std::endl;
    std::cout<<"----------------------------------------"<<std::endl;
    std::cout<<std::endl;
}

void SymbolicEvaluation::StepNode(FlowNode *node)
{
    auto inst = node->get_starting_inst();
    auto block = node->get_block();
    auto func = node->get_func();
    std::optional<std::unordered_set<FlowNode*>> next;

    while(inst != block->end() && next == std::nullopt) {
        next = RunInst(*inst, node);
        ++inst;
    }

    if(next != std::nullopt) {
        auto nextNodes = next.value();
        for(auto n: nextNodes) {
            frontier.push(n);
        }
    }

    ++count;
}

std::optional<std::unordered_set<FlowNode*>> SymbolicEvaluation::RunInst(
    Inst &inst,
    FlowNode *node
) {
    switch(inst.GetKind()) {
    case Inst::Kind::ADD:
        std::cout<<"Running add"<<std::endl;
        return Add(static_cast<AddInst*>(&inst), node);

    case Inst::Kind::ARG:
        std::cout<<"Running arg"<<std::endl;
        Arg(static_cast<ArgInst*>(&inst), node);
        return std::nullopt;
    
    case Inst::Kind::CALL: 
        std::cout<<"Running call"<<std::endl;
        return Call(static_cast<CallInst*>(&inst), node);
    
    case Inst::Kind::CMP:
        std::cout<<"Running cmp"<<std::endl;
        Cmp(static_cast<CmpInst*>(&inst), node);
        return std::nullopt;
    
    case Inst::Kind::JCC:
        std::cout<<"Running jcc"<<std::endl;
        return JumpCond(static_cast<JumpCondInst*>(&inst), node);
    
    case Inst::Kind::JMP:
        std::cout<<"Running jmp"<<std::endl;
        return Jump(static_cast<JumpInst*>(&inst), node);

    case Inst::Kind::LD:
        std::cout<<"Running load"<<std::endl;
        Load(static_cast<LoadInst*>(&inst), node);
        return std::nullopt;

    case Inst::Kind::MOV:
        std::cout<<"Running mov"<<std::endl;
        Mov(static_cast<MovInst*>(&inst), node);
        return std::nullopt;
    
    case Inst::Kind::MUL:
        std::cout<<"Running mul"<<std::endl;
        Mul(static_cast<MulInst*>(&inst), node);
        return std::nullopt;

    case Inst::Kind::PHI:
        std::cout<<"Running phi"<<std::endl;
        Phi(static_cast<PhiInst*>(&inst), node);
        return std::nullopt;

    case Inst::Kind::REM:
        std::cout<<"Running rem"<<std::endl;
        Rem(static_cast<RemInst*>(&inst), node);
        return std::nullopt;
        
    case Inst::Kind::RET:
        std::cout<<"Running ret"<<std::endl;
        return Ret(static_cast<ReturnInst*>(&inst), node);

    case Inst::Kind::SLL:
        std::cout<<"Running sll"<<std::endl;
        LeftLogicalShift(static_cast<SllInst*>(&inst), node);
        return std::nullopt;

    case Inst::Kind::SRA:
        std::cout<<"Running sra"<<std::endl;
        RightArithmeticShift(static_cast<SraInst*>(&inst), node);
        return std::nullopt;

    case Inst::Kind::SRL:
        std::cout<<"Running srl"<<std::endl;
        RightLogicalShift(static_cast<SrlInst*>(&inst), node);
        return std::nullopt;
    
    case Inst::Kind::ST:
        std::cout<<"Running store"<<std::endl;
        Store(static_cast<StoreInst*>(&inst), node);
        return std::nullopt;

    case Inst::Kind::TCALL:
        std::cout<<"Running tail call"<<std::endl;
        return TCall(static_cast<TailCallInst*>(&inst), node);

    default:
        std::cout<<"Skipping "<<toString(inst)<<std::endl;
        return std::nullopt;
    }
}

//-----------------------------------------------------------------------------

RootFlowNode *SymbolicEvaluation::CreateRootNode()
{
    PrintDataInfo(prog);
    auto startFunc = FindFuncByName("caml_program", prog);
    if(startFunc == prog->end()) {
        std::cout<<"Failed to create root node"<<std::endl;
        return nullptr;
    }

    return storagePool.persist(
        new RootFlowNode(*startFunc, *prog, storagePool)
    );
}

SuccessorFlowNode *SymbolicEvaluation::CreateTailCallFlowNode(
    Func_iterator func,
    std::vector<SymValue*> args,
    FlowNode *previous
) {
    return storagePool.persist(
        previous->CreateTailCallNode(
            *func,
            args,
            storagePool
        )
    );
}

SuccessorFlowNode *SymbolicEvaluation::CreateReturnFlowNode(
    FlowNode *previous
) {
    return storagePool.persist(previous->CreateReturnNode());
}

SuccessorFlowNode *SymbolicEvaluation::CreateBlockFlowNode(
    Block_iterator block,
    FlowNode *previous
) {
    return storagePool.persist(previous->CreateBlockNode(*block));
}

SuccessorFlowNode *SymbolicEvaluation::CreateFunctionFlowNode(
    Func_iterator func,
    std::vector<SymValue*> args,
    Inst_iterator caller,
    FlowNode *previous
) {
    return storagePool.persist(
        previous->CreateFunctionNode(
            *func,
            args,
            caller,
            storagePool
        )
    );
}

// -----------------------------------------------------------------------------

std::optional<std::unordered_set<FlowNode*>> SymbolicEvaluation::Add(
    AddInst *addInst,
    FlowNode *node
) {
    SymValue *LHS = node->GetResult(addInst->GetLHS());
    SymValue *RHS = node->GetResult(addInst->GetRHS());

    SymValue *result;
    Type resultType = addInst->GetType();

    try {
        switch(LHS->get_kind()) {
        case SymValue::Kind::ADDR: {
            AddrSymValue *laddr = static_cast<AddrSymValue*>(LHS);
            switch(RHS->get_kind()) {
            case SymValue::Kind::INT: {
                IntSymValue *rint = static_cast<IntSymValue*>(RHS);
                result = new AddrSymValue(laddr->get_name(), laddr->get_offset()+rint->get_value(), laddr->get_max(), resultType);
                std::cout<<laddr->toString()<<"+"<<rint->toString()<<" = "<<static_cast<AddrSymValue*>(result)->toString()<<std::endl;
            } break;
            default:
                std::cout<<"WARNING: Adding ADDR and not INT"<<std::endl;
                result = new UnknownSymValue(resultType);
                break;
            }
        } break;
        case SymValue::Kind::INT: {
            IntSymValue *lint = static_cast<IntSymValue*>(LHS);
            switch(RHS->get_kind()) {
            case SymValue::Kind::ADDR: {
                AddrSymValue *raddr = static_cast<AddrSymValue*>(RHS);
                result = new AddrSymValue(raddr->get_name(), raddr->get_offset()+lint->get_value(), raddr->get_max(), resultType);
                std::cout<<lint->toString()<<"+"<<raddr->toString()<<" = "<<static_cast<AddrSymValue*>(result)->toString()<<std::endl;
            } break;
            case SymValue::Kind::FLOAT: {
                auto rf = static_cast<FloatSymValue*>(RHS);
                if(resultType == Type::F64) {
                    auto resultValue = llvm::APFloat(static_cast<double>(0));
                    resultValue.convertFromAPInt(lint->get_value(), isSigned(lint->get_type()), llvm::APFloatBase::rmNearestTiesToEven);
                    resultValue = resultValue + rf->get_value();
                    result = new FloatSymValue(resultValue, resultType);
                    std::cout<<lint->toString()<<"+"<<rf->toString()<<" = "<<static_cast<FloatSymValue*>(result)->toString()<<std::endl;
                }else if (resultType == Type::F32) {
                    auto resultValue = llvm::APFloat(static_cast<float>(0));
                    resultValue.convertFromAPInt(lint->get_value(), isSigned(lint->get_type()), llvm::APFloatBase::rmNearestTiesToEven);
                    resultValue = resultValue + rf->get_value();
                    result = new FloatSymValue(resultValue, resultType);
                    std::cout<<lint->toString()<<"+"<<rf->toString()<<" = "<<static_cast<FloatSymValue*>(result)->toString()<<std::endl;
                } else {
                    std::cout<<"WARNING: Adding int and float and not expecting float as a result"<<std::endl;
                    result = new UnknownSymValue(resultType);
                }
            } break;
            case SymValue::Kind::INT: {
                IntSymValue *rint = static_cast<IntSymValue*>(RHS);
                result = new IntSymValue(lint->get_value()+rint->get_value(), resultType);
                std::cout<<lint->toString()<<"+"<<rint->toString()<<" = "<<static_cast<IntSymValue*>(result)->toString()<<std::endl;
            } break;
            default:
                std::cout<<"Unable to calculate INT plus "<<toString(RHS->get_kind())<<std::endl;
                result = new UnknownSymValue(resultType);
                break;
            }
        } break;
        default:
            std::cout<<"Unable to calculate "<<toString(LHS->get_kind())<<" plus "<<toString(RHS->get_kind())<<std::endl;
            result = new UnknownSymValue(resultType);
            break;
        }

        storagePool.persist(result);
        node->AllocateResult(addInst, result);
        return std::nullopt;
    } catch (OffsetOutOfBoundsException e) {
        return std::unordered_set<FlowNode*>();
    }
    
}

void SymbolicEvaluation::Arg(
    ArgInst *argInst,
    FlowNode *node
) {
    unsigned int idx = argInst->GetIdx();
    SymValue *result = node->get_frame().get_arg(idx);
    std::cout<<(result==nullptr?"nullptr":toString(*result))<<" at index "<<idx<<std::endl;

    node->AllocateResult(
        argInst,
        result
    );
}

std::optional<std::unordered_set<FlowNode*>> SymbolicEvaluation::Call(
    CallInst *callInst,
    FlowNode *node
) {
    SymValue *v = node->GetResult(callInst->GetCallee());

    if ( v == nullptr ) {
        std::cout<<"Attempted to call nullptr"<<std::endl;
        if(!callInst->IsVoid()) {
            node->AllocateResult(callInst,storagePool.persist(new UnknownSymValue(*callInst->GetType())));
        }
        return std::unordered_set<FlowNode*>();
    }

    switch(v->get_kind()) {
    case SymValue::Kind::FUNCREF: {
        auto funcRef = static_cast<FuncRefSymValue*>(v);

        std::vector<SymValue*> args;
        std::transform(
            callInst->arg_begin(), callInst->arg_end(),
            std::back_inserter(args),
            [this, node](auto x) { return node->GetResult(x); }
        );
        
        SuccessorFlowNode *newNode = storagePool.persist(
            node->CreateFunctionNode(
                *FindFuncByName(funcRef->get_name(), prog),
                args,
                callInst->getIterator(),
                storagePool
            )
        );

        std::unordered_set<FlowNode*> result;
        result.insert(newNode);
        return result;
    }

    case SymValue::Kind::EXTERN: {
        auto ext = static_cast<ExternSymValue*>(v);
        std::cout<<"Calling extern "<<ext->get_name()<<std::endl;
        if(!knownSafeExtern(ext->get_name())) {
            std::cout<<"Invalidating store"<<std::endl;
            node->get_store().invalidate();
        } else {
            std::cout<<"Not invalidating store"<<std::endl;
        }
        
        if(!callInst->IsVoid()){
            node->AllocateResult(callInst, storagePool.persist(new UnknownSymValue(*callInst->GetType())));
        }
        return std::nullopt;
    }

    default:
        std::cout<<"Attempted to call something of unhandled type"<<std::endl;
        return std::unordered_set<FlowNode*>();
    }
}

void SymbolicEvaluation::Cmp(
    CmpInst *cmpInst,
    FlowNode *node
) {
    Cond c = cmpInst->GetCC();
    Inst *lhs = cmpInst->GetLHS();
    Inst *rhs = cmpInst->GetRHS();
    SymValue *lv = node->GetResult(lhs);
    SymValue *rv = node->GetResult(rhs);
    SymComp::Result result;

    std::cout<<"lhs: "<<toString(lhs->GetKind())<<" rhs: "<<toString(rhs->GetKind())<<std::endl;

    switch(c) {
    case Cond::EQ:
        std::cout<<"Comparing EQ"<<std::endl;
        result = SymComp::EQ(lv, rv);
        break;
    
    case Cond::NE:
        std::cout<<"Comparing NE"<<std::endl;
        result = SymComp::NEQ(lv, rv);
        break;
    
    case Cond::LT:
        std::cout<<"Comparing LT"<<std::endl;
        result = SymComp::LT(lv, rv);
        break;
    
    case Cond::GT:
        std::cout<<"Comparing GT"<<std::endl;
        result = SymComp::GT(lv, rv);
        break;
    
    case Cond::LE:
        std::cout<<"Comparing LE"<<std::endl;
        result = SymComp::LE(lv, rv);
        break;
    
    case Cond::GE:
        std::cout<<"Comparing GE"<<std::endl;
        result = SymComp::GE(lv, rv);
        break;
    
    default:
        std::cout<<"Comparing "<<(int)c<<std::endl;
        result = SymComp::Result::UNKNOWN;
        break;
    }

    SymValue *value = SymComp::ToSymValue(result, cmpInst->GetType());
    storagePool.persist(value);
    node->AllocateResult(cmpInst, value);
}

std::unordered_set<FlowNode*> SymbolicEvaluation::Jump(
    JumpInst *jumpInst,
    FlowNode *node
) {
    auto block = jumpInst->GetTarget()->getIterator();
    std::unordered_set<FlowNode*> result;
    result.insert(
        CreateBlockFlowNode(
            block,
            node
        )
    );
    return result;
}

std::unordered_set<FlowNode*> SymbolicEvaluation::JumpCond(
    JumpCondInst *jumpCondInst,
    FlowNode *node
) {
    std::unordered_set<FlowNode*> result;

    Inst *cond = jumpCondInst->GetCond();
    Block *trueBlock = jumpCondInst->GetTrueTarget();
    Func *trueFunc = trueBlock->getParent();
    Block *falseBlock = jumpCondInst->GetFalseTarget();
    Func *falseFunc = falseBlock->getParent();

    SymValue *symCond = node->GetResult(cond);
    if(symCond->get_kind() == SymValue::Kind::BOOL) {
        auto b = static_cast<BoolSymValue*>(symCond);
        std::cout<<"Condition is bool "<<(b->get_value()?"true":"false")<<std::endl;
        Block *block;
        Func *func;
        if(b->get_value()) {
            block = trueBlock;
            func = trueFunc;
        } else {
            block = falseBlock;
            func = falseFunc;
        }
        if(func->GetName() != node->get_func()->GetName()) {
            std::cout<<"Jumping from "<<node->get_func()->GetName()<<" to "<<func->GetName()<<std::endl;
        }
        FlowNode *newNode = CreateBlockFlowNode(block->getIterator(), node);
        result.insert(newNode);
    } else if (symCond->get_kind() == SymValue::Kind::INT) {
        auto i = static_cast<IntSymValue*>(symCond);
        std::cout<<"Condition is int "<<i->toString()<<std::endl;
        Block *block;
        Func *func;
        if(i->get_value().getBoolValue()) {
            block = trueBlock;
            func = trueFunc;
        } else {
            block = falseBlock;
            func = falseFunc;
        }
        if(func->GetName() != node->get_func()->GetName()) {
            std::cout<<"Jumping from "<<node->get_func()->GetName()<<" to "<<func->GetName()<<std::endl;
        }
        FlowNode *newNode = CreateBlockFlowNode(block->getIterator(), node);
        result.insert(newNode);
    } else {
        std::cout<<"Taking both branches"<<std::endl;
        result.insert(
            CreateBlockFlowNode(trueBlock->getIterator(), node)
        );
        result.insert(
            CreateBlockFlowNode(falseBlock->getIterator(), node)
        );
    }
    return result;
}

void SymbolicEvaluation::LeftLogicalShift(
    SllInst *sllInst,
    FlowNode *node
) {
    auto lhs = node->GetResult(sllInst->GetLHS());
    auto rhs = node->GetResult(sllInst->GetRHS());

    SymValue *result;
    Type resultType = sllInst->GetType();

    switch(rhs->get_kind()) {
    case SymValue::Kind::INT: {
        auto r = static_cast<IntSymValue*>(rhs);
        switch(lhs->get_kind()) {
        case SymValue::Kind::INT: {
            auto l = static_cast<IntSymValue*>(lhs);
            result = new IntSymValue(l->get_value() << r->get_value(), resultType);
            std::cout<<l->toString()<<"<<"<<r->toString()<<" = "<<static_cast<IntSymValue*>(result)->toString()<<std::endl;
        } break;
        default:
            std::cout<<"Unable to calculate "<<toString(lhs->get_kind())<<" shifted left by "<<r->toString()<<std::endl;
            result = new UnknownSymValue(resultType);
            break;
        }
    } break;
    default :
        std::cout<<"Unable to calculate "<<toString(lhs->get_kind())<<" shifted left by "<<toString(rhs->get_kind())<<std::endl;
        result = new UnknownSymValue(resultType);
        break;
    }

    storagePool.persist(result);
    node->AllocateResult(
        sllInst,
        result
    );
}

void SymbolicEvaluation::Load(
    LoadInst *loadInst,
    FlowNode *node
) {
    SymValue *addr = node->GetResult(loadInst->GetAddr());
    node->AllocateResult(loadInst,node->get_store().read(addr, loadInst->GetLoadSize(), loadInst->GetType()));
}

void SymbolicEvaluation::Mov(
    MovInst *movInst,
    FlowNode *node
) {
    AllocateValue(
        movInst,
        movInst->GetArg(),
        movInst->GetType(),
        node
    );
}

void SymbolicEvaluation::Mul(
    MulInst *mulInst,
    FlowNode *node
) {
    auto lhs = node->GetResult(mulInst->GetLHS());
    auto rhs = node->GetResult(mulInst->GetRHS());

    Type resultType = mulInst->GetType();

    switch(lhs->get_kind()) {
    case SymValue::Kind::FLOAT: {
        auto l = static_cast<FloatSymValue*>(lhs);
        std::cout<<"Multiplying float "<<l->toString();
        switch(rhs->get_kind()) {
        case SymValue::Kind::FLOAT: {
            auto r = static_cast<FloatSymValue*>(rhs);
            std::cout<<" with float "<<r->toString();
            auto result = new FloatSymValue(
                l->get_value() * r->get_value(),
                resultType
            );
            std::cout<<" to produce "<<result->toString();
            node->AllocateResult(
                mulInst,
                storagePool.persist(
                    result
                )
            );
        } break;
        case SymValue::Kind::INT: {
            auto r = static_cast<IntSymValue*>(rhs);
            std::cout<<" with int "<<r->toString();
            SymValue *result;
            if(resultType == Type::F64) {
                auto resultValue = llvm::APFloat(static_cast<double>(0));
                resultValue.convertFromAPInt(r->get_value(), isSigned(r->get_type()), llvm::APFloatBase::rmNearestTiesToEven);
                resultValue = resultValue + l->get_value();
                result = new FloatSymValue(resultValue, resultType);
                std::cout<<" to produce "<<static_cast<FloatSymValue*>(result)->toString();
            }else if (resultType == Type::F32) {
                auto resultValue = llvm::APFloat(static_cast<float>(0));
                resultValue.convertFromAPInt(r->get_value(), isSigned(r->get_type()), llvm::APFloatBase::rmNearestTiesToEven);
                resultValue = resultValue + l->get_value();
                result = new FloatSymValue(resultValue, resultType);
                std::cout<<" to produce "<<static_cast<FloatSymValue*>(result)->toString();
            } else {
                std::cout<<std::endl;
                std::cout<<"WARNING: Adding int and float and not expecting float as a result"<<std::endl;
                result = new UnknownSymValue(resultType);
            }
            
            node->AllocateResult(
                mulInst,
                storagePool.persist(
                    result
                )
            );
        } break;
        default: {
            std::cout<<" with "<<(rhs->get_kind()==SymValue::Kind::UNKNOWN?"symbolic":"non-numeric")<<" value";
            node->AllocateResult(
                mulInst,
                storagePool.persist(
                    new UnknownSymValue(resultType)
                )
            );
        } break;
        }
    } break;
    case SymValue::Kind::INT: {
        auto l = static_cast<IntSymValue*>(lhs);
        std::cout<<"Multiplying int "<<l->toString();
        switch(rhs->get_kind()) {
        case SymValue::Kind::FLOAT: {
            auto r = static_cast<FloatSymValue*>(rhs);
            std::cout<<" with float "<<r->toString();
            SymValue *result;
            if(resultType == Type::F64) {
                auto resultValue = llvm::APFloat(static_cast<double>(0));
                resultValue.convertFromAPInt(l->get_value(), isSigned(l->get_type()), llvm::APFloatBase::rmNearestTiesToEven);
                resultValue = resultValue + r->get_value();
                result = new FloatSymValue(resultValue, resultType);
                std::cout<<" to produce "<<static_cast<FloatSymValue*>(result)->toString();
            }else if (resultType == Type::F32) {
                auto resultValue = llvm::APFloat(static_cast<float>(0));
                resultValue.convertFromAPInt(l->get_value(), isSigned(l->get_type()), llvm::APFloatBase::rmNearestTiesToEven);
                resultValue = resultValue + r->get_value();
                result = new FloatSymValue(resultValue, resultType);
                std::cout<<" to produce "<<static_cast<FloatSymValue*>(result)->toString();
            } else {
                std::cout<<std::endl;
                std::cout<<"WARNING: Adding int and float and not expecting float as a result"<<std::endl;
                result = new UnknownSymValue(resultType);
            }
            node->AllocateResult(
                mulInst,
                storagePool.persist(
                    result
                )
            );
        } break;
        case SymValue::Kind::INT: {
            auto r = static_cast<IntSymValue*>(rhs);
            std::cout<<" with int "<<r->toString();
            IntSymValue *result = new IntSymValue(
                l->get_value() * r->get_value(),
                resultType
            );
            std::cout<<" to produce "<<result->toString();
            node->AllocateResult(
                mulInst,
                storagePool.persist(
                    result
                )
            );
        } break;
        default: {
            std::cout<<" with "<<(rhs->get_kind()==SymValue::Kind::UNKNOWN?"symbolic":"non-numeric")<<" value";
            node->AllocateResult(
                mulInst,
                storagePool.persist(
                    new UnknownSymValue(resultType)
                )
            );
        } break;
        }
    } break;
    default:
        std::cout<<"Multiplying "<<(lhs->get_kind()==SymValue::Kind::UNKNOWN?"symbolic":"non-numeric")<<" value";
        std::cout<<" with "<<(rhs->get_kind()==SymValue::Kind::UNKNOWN?"symbolic":"non-numeric")<<" value";
        node->AllocateResult(
            mulInst,
            storagePool.persist(
                new UnknownSymValue(resultType)
            )
        );
        break;
    }
    std::cout<<std::endl;
}

void SymbolicEvaluation::Rem(
    RemInst *remInst,
    FlowNode *node
) {
    auto lhs = node->GetResult(remInst->GetLHS());
    auto rhs = node->GetResult(remInst->GetRHS());

    Type resultType = remInst->GetType();

    if(isIntType(resultType)) {
        if(lhs->get_kind() == SymValue::Kind::INT && rhs->get_kind() == SymValue::Kind::INT) {
            auto l = static_cast<IntSymValue*>(lhs);
            auto r = static_cast<IntSymValue*>(rhs);
            std::cout<<l->toString()<<(isSigned(resultType)?" srem ":" urem ")<<r->toString()<<" ";
            node->AllocateResult(
                remInst,
                storagePool.persist(
                    new IntSymValue(
                        isSigned(resultType) 
                            ? l->get_value().srem(r->get_value())
                            : l->get_value().urem(r->get_value()),
                        resultType
                    )
                )
            );
        } else {
            node->AllocateResult(
                remInst,
                storagePool.persist(
                    new UnknownSymValue(resultType)
                )
            );
        }
    } else {
        if(lhs->get_kind() == SymValue::Kind::FLOAT && rhs->get_kind() == SymValue::Kind::FLOAT) {
            auto l = static_cast<FloatSymValue*>(lhs);
            auto r = static_cast<FloatSymValue*>(rhs);
            std::cout<<l->toString()<<" frem "<<r->toString()<<" ";
            llvm::APFloat resultValue = l->get_value();
            auto status = resultValue.remainder(r->get_value());
            if(status == llvm::APFloatBase::opOK) {
                node->AllocateResult(
                    remInst,
                    storagePool.persist(
                        new FloatSymValue(
                            resultValue,
                            resultType
                        )
                    )
                );
            } else {
                node->AllocateResult(
                remInst,
                storagePool.persist(
                    new UnknownSymValue(resultType)
                )
            );
            }
        } else {
            node->AllocateResult(
                remInst,
                storagePool.persist(
                    new UnknownSymValue(resultType)
                )
            );
        }
    }
    std::cout<<toString(*node->GetResult(remInst))<<std::endl;
}

std::unordered_set<FlowNode*> SymbolicEvaluation::Ret(
    ReturnInst *returnInst,
    FlowNode *node
) {
    Inst *caller = node->get_frame().get_caller();
    FlowNode *newNode = CreateReturnFlowNode(node);
    if(!caller->IsVoid()) {
        SymValue *returnValue = node->GetResult(returnInst->GetValue());
        newNode->AllocateResult(caller, returnValue);
    }
    std::unordered_set<FlowNode*> nodes;
    nodes.insert(newNode);
    return nodes;
}

void SymbolicEvaluation::RightArithmeticShift(
    SraInst *sraInst,
    FlowNode *node
) {
    auto lhs = node->GetResult(sraInst->GetLHS());
    auto rhs = node->GetResult(sraInst->GetRHS());

    SymValue *result;
    Type resultType = sraInst->GetType();

    switch(rhs->get_kind()) {
    case SymValue::Kind::INT: {
        auto r = static_cast<IntSymValue*>(rhs);
        switch(lhs->get_kind()) {
        case SymValue::Kind::INT: {
            auto l = static_cast<IntSymValue*>(lhs);
            result = new IntSymValue(l->get_value().ashr(r->get_value()), resultType);
            std::cout<<"Shifted "<<l->toString()<<" right arithmetically by "<<r->toString()<<" to produce "<<static_cast<IntSymValue*>(result)->toString()<<std::endl;
        } break;
        default:
            std::cout<<"Unable to calculate "<<toString(lhs->get_kind())<<" shifted right by "<<r->toString()<<std::endl;
            result = new UnknownSymValue(resultType);
            break;
        }
    } break;
    default :
        std::cout<<"Unable to calculate "<<toString(lhs->get_kind())<<" shifted right by "<<toString(rhs->get_kind())<<std::endl;
        result = new UnknownSymValue(resultType);
        break;
    }

    storagePool.persist(result);
    node->AllocateResult(
        sraInst,
        result
    );
}

void SymbolicEvaluation::RightLogicalShift(
    SrlInst *srlInst,
    FlowNode *node
) {
    auto lhs = node->GetResult(srlInst->GetLHS());
    auto rhs = node->GetResult(srlInst->GetRHS());

    SymValue *result;
    Type resultType = srlInst->GetType();

    switch(rhs->get_kind()) {
    case SymValue::Kind::INT: {
        auto r = static_cast<IntSymValue*>(rhs);
        switch(lhs->get_kind()) {
        case SymValue::Kind::INT: {
            auto l = static_cast<IntSymValue*>(lhs);
            result = new IntSymValue(l->get_value().lshr(r->get_value()), resultType);
            std::cout<<"Shifted "<<l->toString()<<" right logically by "<<r->toString()<<" to produce "<<static_cast<IntSymValue*>(result)->toString()<<std::endl;
        } break;
        default:
            std::cout<<"Unable to calculate "<<toString(lhs->get_kind())<<" shifted right by "<<r->toString()<<std::endl;
            result = new UnknownSymValue(resultType);
            break;
        }
    } break;
    default :
        std::cout<<"Unable to calculate "<<toString(lhs->get_kind())<<" shifted right by "<<toString(rhs->get_kind())<<std::endl;
        result = new UnknownSymValue(resultType);
        break;
    }

    storagePool.persist(result);
    node->AllocateResult(
        srlInst,
        result
    );
}

void SymbolicEvaluation::Store(
    StoreInst *storeInst,
    FlowNode *node
) {
    SymValue *addr = node->GetResult(storeInst->GetAddr());
    SymValue *value = node->GetResult(storeInst->GetVal());
    node->get_store().write(addr, value);
}

std::unordered_set<FlowNode*> SymbolicEvaluation::TCall(
    TailCallInst *tailCallInst,
    FlowNode *node
) {
    SymValue *v = node->GetResult(tailCallInst->GetCallee());

    if ( v == nullptr || v->get_kind() != SymValue::Kind::FUNCREF ) {
        std::cout<<"Called something that isn't a func ref"<<std::endl;
        if(!tailCallInst->IsVoid()) {
            node->AllocateResult(tailCallInst,storagePool.persist(new UnknownSymValue(*tailCallInst->GetType())));
        }
        return std::unordered_set<FlowNode*>();
    }

    FuncRefSymValue *funcRef = static_cast<FuncRefSymValue*>(v);

    std::vector<SymValue*> args;
    std::transform(
        tailCallInst->arg_begin(), tailCallInst->arg_end(),
        std::back_inserter(args),
        [this, node](auto x) { return node->GetResult(x); }
    );
    
    SuccessorFlowNode *newNode = storagePool.persist(
        node->CreateTailCallNode(
            *FindFuncByName(funcRef->get_name(), prog),
            args,
            storagePool
        )
    );

    std::unordered_set<FlowNode*> result;
    result.insert(newNode);
    return result;
}

void SymbolicEvaluation::Phi(
    PhiInst *phiInst,
    FlowNode *node
) {
    unsigned numPredecessors = phiInst->GetNumIncoming();

    std::vector<Block*> blocks;
    for(int i = 0; i < numPredecessors; ++i) {
        blocks.push_back(phiInst->GetBlock(i));
    }

    Block *block = node->ResolvePhiBlocks(blocks);

    if(block == nullptr) {
        std::cout<<"Phi resolution failed. Not allocating"<<std::endl;
        return;
    }

    AllocateValue(
        phiInst,
        phiInst->GetValue(block),
        phiInst->GetType(),
        node
    );
}

void SymbolicEvaluation::AllocateValue(
    Inst *inst,
    Value *value,
    Type type,
    FlowNode *node
) {
    SymValue *result;
    switch(value->GetKind()) {
    case Value::Kind::GLOBAL: {
        auto g = static_cast<Global*>(value);
        switch(g->GetKind()) {
        case Global::Kind::ATOM: {
            std::cout<<"Allocating global atom (label) "<<g->GetName()<<std::endl;
            const Label *label = node->get_store().getLabel(g->GetName());
            if(label == nullptr) {
                std::cout<<"WARNING: Attempted to create address symvalue to unknown label"<<std::endl;
            } else {
                result = storagePool.persist(new AddrSymValue(
                    g->GetName(),
                    llvm::APInt(bitLength(type),label->offset, isSigned(type)),
                    label->atom->getSize(),
                    type
                ));
            }
        } break;
        
        case Global::Kind::EXTERN:
            std::cout<<"Allocating global extern "<<g->GetName()<<std::endl;
            result = storagePool.persist(new ExternSymValue(g->GetName(), type));
            break;

        case Global::Kind::FUNC:
            std::cout<<"Allocating global func "<<g->GetName()<<std::endl;
            result = storagePool.persist(new FuncRefSymValue(g->GetName(), type));
            break;
        
        default:
            std::cout<<"Allocating unknown global "<<g->GetName()<<std::endl;
            result = storagePool.persist(new UnknownSymValue(type));
            break;
        }
    } break;

    case Value::Kind::INST: {
        std::cout<<"Allocating inst"<<std::endl;
        auto i = static_cast<Inst*>(value);
        result = node->GetResult(i);
    } break;

    case Value::Kind::CONST: {
        Constant *c = static_cast<Constant*>(value);
        switch(c->GetKind()) {
        case Constant::Kind::INT: {
            ConstantInt *ci = static_cast<ConstantInt*>(c);
            result = new IntSymValue(ci->GetValue(), type);
            if(!isIntType(type)) { std::cout<<"Allocating constant int of non-int type"<<std::endl; }
            storagePool.persist(result);
            std::cout<<"Allocating int "<<ci->GetValue()<<std::endl;
        } break;
        
        case Constant::Kind::FLOAT: {
            ConstantFloat *cf = static_cast<ConstantFloat*>(c);
            result = new FloatSymValue(cf->GetValue(), type);
            if(isIntType(type)) { std::cout<<"Allocating constant float of non-float type"<<std::endl; }
            storagePool.persist(result);
            std::cout<<"Allocating float "<<cf->GetValue()<<std::endl;
        } break;

        default:
            result = new UnknownSymValue(type);
            storagePool.persist(result);
            std::cout<<"Allocating const"<<std::endl;
            break;
        }
    } break;

    case Value::Kind::EXPR: {
        result = new UnknownSymValue(type);
        storagePool.persist(result);
        std::cout<<"Allocating expr"<<std::endl;
    }
    }

    node->AllocateResult(inst, result);
}
