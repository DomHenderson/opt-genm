#include <algorithm>
#include <iostream>
#include <map>
#include <memory>
#include <optional>
#include <string>
#include <unordered_map>
#include <vector>

#include "core/block.h"
#include "core/constant.h"
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
#include "symbolic_evaluation/stack.h"
#include "symbolic_evaluation/symvalue.h"

const char *SymbolicEvaluation::kPassID = "symbolic_evaluation";

SymbolicEvaluation::SymbolicEvaluation(PassManager *passManager) : Pass(passManager) {
    results.push_back(std::make_unique<UnknownSymValue>());
}

std::string toString(Inst::Kind k)
{
    switch(k) {
        // Control flow.
        case Inst::Kind::CALL: return "call"; break;
        case Inst::Kind::TCALL: return "tcall"; break;
        case Inst::Kind::INVOKE: return "invoke"; break;
        case Inst::Kind::TINVOKE: return "tinvoke"; break;
        case Inst::Kind::RET: return "ret"; break;
        case Inst::Kind::JCC: return "jcc"; break;
        case Inst::Kind::JI: return "ji"; break;
        case Inst::Kind::JMP: return "jmp"; break;
        case Inst::Kind::SWITCH: return "switch"; break;
        case Inst::Kind::TRAP: return "trap"; break;
    // Memory.
        case Inst::Kind::LD: return "ld"; break;
        case Inst::Kind::ST: return "st"; break;
    // Atomic exchange.
        case Inst::Kind::XCHG: return "xchg"; break;
    // Set register.
        case Inst::Kind::SET: return "set"; break;
    // Variable argument lists.
        case Inst::Kind::VASTART: return "vastart"; break;
    // Dynamic stack allcoation.
        case Inst::Kind::ALLOCA: return "alloca"; break;
    // Constants.
        case Inst::Kind::ARG: return "arg"; break;
        case Inst::Kind::FRAME: return "frame"; break;
        case Inst::Kind::UNDEF: return "undef"; break;
    // Conditional.
        case Inst::Kind::SELECT: return "select"; break;
    // Unary instructions.
        case Inst::Kind::ABS: return "abs"; break;
        case Inst::Kind::NEG: return "neg"; break;
        case Inst::Kind::SQRT: return "sqrt"; break;
        case Inst::Kind::SIN: return "sin"; break;
        case Inst::Kind::COS: return "cos"; break;
        case Inst::Kind::SEXT: return "sext"; break;
        case Inst::Kind::ZEXT: return "zext"; break;
        case Inst::Kind::FEXT: return "fext"; break;
        case Inst::Kind::MOV: return "mov"; break;
        case Inst::Kind::TRUNC: return "trunc"; break;
    // Binary instructions.
        case Inst::Kind::ADD: return "add"; break;
        case Inst::Kind::AND: return "and"; break;
        case Inst::Kind::CMP: return "cmp"; break;
        case Inst::Kind::DIV: return "div"; break;
        case Inst::Kind::REM: return "rem"; break;
        case Inst::Kind::MUL: return "mul"; break;
        case Inst::Kind::OR: return "or"; break;
        case Inst::Kind::ROTL: return "rotl"; break;
        case Inst::Kind::SLL: return "sll"; break;
        case Inst::Kind::SRA: return "sra"; break;
        case Inst::Kind::SRL: return "srl"; break;
        case Inst::Kind::SUB: return "sub"; break;
        case Inst::Kind::XOR: return "xor"; break;
        case Inst::Kind::POW: return "pow"; break;
        case Inst::Kind::COPYSIGN: return "copysign"; break;
    // Overflow tests.
        case Inst::Kind::UADDO: return "uaddo"; break;
        case Inst::Kind::UMULO: return "umulo"; break;
    // PHI node.
        case Inst::Kind::PHI: return "phi"; break;
        default: return "default"; break;
    }
}

auto FindFuncByName(std::string_view name, Prog *prog)
{
    auto result = std::find_if(
        prog->begin(),
        prog->end(),
        [&name](auto& f) {
            return f.GetName() == name;
        }
    );
    if(result == prog->end()) {
        std::cout<<"Failed to find function "<<name<<std::endl;
    } else {
        std::cout<<"Found "<<name<<std::endl;
    }
    return result;
}

void PrintLearningInfo(Prog *prog)
{
    std::unordered_map<int, std::optional<int>> registers;
    std::map<Inst::Kind, bool> seen; //TEMP
    for (auto &func : *prog) {
        std::cout<<func.GetName()<<"---";
        std::cout<<func.size()<<std::endl;
        for (auto &block : func) {
            std::cout<<block.GetName()<<"---"<<std::endl;
            for (auto &inst : block) {
                std::cout<<(void*)&inst<<toString(inst.GetKind())<<" ";
                seen[inst.GetKind()] = true;
                auto begin = inst.op_begin();
                auto end = inst.op_end();
                for ( auto i = begin; i < end; ++i ) {
                    auto v = i->get();
                    Value::Kind k = v->GetKind();
                    switch(k) {
                    case Value::Kind::CONST: {
                        Constant *c = (Constant*)v;
                        switch(c->GetKind()) {
                        case Constant::INT: {
                            ConstantInt *ci = (ConstantInt*)c;
                            std::cout<<"Constant"<<ci->GetValue()<<" ";
                        } break;
                        case Constant::FLOAT: {
                            ConstantFloat *cf = (ConstantFloat*)c;
                            std::cout<<"Constant"<<cf->GetValue()<<" ";
                        } break;
                        case Constant::REG:
                            std::cout<<"ConstantReg"<<std::endl;
                            break;
                        }
                    } break;
                    
                    case Value::Kind::EXPR: {
                        std::cout<<"Expr"<<" ";
                    } break;
                    
                    case Value::Kind::GLOBAL: {
                        std::cout<<"Global";
                        Global *g = (Global*)v;
                        switch(g->GetKind()) {
                        case Global::Kind::ATOM:
                            std::cout<<"Atom";
                            break;
                        
                        case Global::Kind::BLOCK:
                            std::cout<<"Block";
                            break;
                        
                        case Global::Kind::EXTERN:
                            std::cout<<"Extern";
                            break;
                        
                        case Global::Kind::FUNC:
                            std::cout<<"Func";
                            break;
                        
                        case Global::Kind::SYMBOL:
                            std::cout<<"Symbol";
                            break;
                        }
                        std::cout<<g->GetName()<<" ";
                    } break;
                    
                    case Value::Kind::INST:
                        Inst *i = (Inst*)v;
                        std::cout<<"Inst"<<(void*)i<<toString(i->GetKind())<<" ";
                        break;
                    }
                }
                std::cout<<std::endl;
            }
        }
    }

    for(auto &entry: seen) {
        std::cout<<toString(entry.first)<<" "<<entry.second<<std::endl;
    }
}

void SymbolicEvaluation::Run(Prog *prog)
{
    // PrintLearningInfo(prog);

    // ---------------------------------------------------------------
    // auto startFunc = prog->begin();
    std::cout<<"Starting"<<std::endl;
    auto startFunc = FindFuncByName("caml_program", prog);
    auto argCount = startFunc->params().size();
    std::cout<<argCount<<std::endl;
    RunFunction(&*startFunc, std::vector<SymValue*>(argCount, results[0].get()), prog);
}

// -----------------------------------------------------------------------------
const char *SymbolicEvaluation::GetPassName() const
{
  return "Symbolic Evaluation";
}

SymValue *SymbolicEvaluation::RunFunction(Func *func, std::vector<SymValue*> funcArgs, Prog *prog)
{
    stack.push_frame(funcArgs);
    std::cout<<"Running function "<<func->GetName()<<std::endl;
    Block *block = &func->getEntryBlock();
    SymValue *result = nullptr;
    while( block != nullptr ) {
        result = RunBlock(*block, prog);
        block = block->getNextNode();
        std::cout<<(void*)block<<std::endl;
        if(!carryOn) {
            std::cout<<"Stopped function because at limit"<<std::endl;
            return nullptr;
        }
    }
    std::cout<<"Finished function "<<func->GetName()<<std::endl;
    stack.pop_frame();
    return result;
}

SymValue *SymbolicEvaluation::RunBlock(Block &block, Prog *prog)
{
    if(!carryOn) {
        std::cout<<"Stopped block because at limit"<<std::endl;
        return nullptr;
    }
    std::cout<<"Block start"<<std::endl;
    for(auto &inst: block) {
        RunInst(inst, prog);
        if(!carryOn) {
            std::cout<<"Stopped block because at limit"<<std::endl;
            return nullptr;
        }
    }
    std::cout<<"Block end"<<std::endl;
    return nullptr;
}

SymValue *SymbolicEvaluation::RunInst(Inst &inst, Prog *prog)
{
    switch(inst.GetKind()) {
    case Inst::Kind::ARG:
        std::cout<<"Running arg"<<std::endl;
        Arg(static_cast<ArgInst*>(&inst));
        break;

    case Inst::Kind::MOV:
        std::cout<<"Running mov"<<std::endl;
        Mov(static_cast<MovInst*>(&inst));
        break;
    
    case Inst::Kind::CALL:
        std::cout<<"Running call"<<std::endl;
        Call(static_cast<CallInst*>(&inst), prog);
        break;
    
    case Inst::Kind::CMP:
        std::cout<<"Running cmp"<<std::endl;
        Cmp(static_cast<CmpInst*>(&inst));
        break;

    case Inst::Kind::TCALL: {
        std::cout<<"Running tail call"<<std::endl;
        TCall(static_cast<TailCallInst*>(&inst), prog);
        SymValue *result = GetResult(&inst);
        std::cout<<"Result "<<(void*)result<<" "<<(result==nullptr)<<std::endl;
    } break;

    case Inst::Kind::RET:
        std::cout<<"Running ret"<<std::endl;
        Ret(static_cast<ReturnInst*>(&inst));
        break;
    
    default:
        std::cout<<"Skipping"<<std::endl;
        break;
    }
    return GetResult(&inst);
}

// -----------------------------------------------------------------------------
void SymbolicEvaluation::Arg(ArgInst *argInst)
{
    unsigned int idx = argInst->GetIdx();
    unsigned int arg_id = stack.get_local_arg(idx);
    AllocateResult(argInst, std::make_unique<ArgSymValue>(arg_id));
}

void SymbolicEvaluation::Call(CallInst *callInst, Prog *prog)
{
    Inst *callee = callInst->GetCallee();
    SymValue *v = GetResult(callee);
    if ( v->get_kind() != SymValue::Kind::FUNCREF ) {
        std::cout<<"Called something that isn't a func ref"<<std::endl;
        AllocateResult(callInst, std::make_unique<UnknownSymValue>());
        return;
    }
    FuncRefSymValue *fvp = (FuncRefSymValue*)v;
    std::string_view name = fvp->get_name();

    auto func = FindFuncByName(name, prog);

    if ( func == prog->end() ) {
        std::cout<<"No function with name " << name << " found"<<std::endl;
    }

    std::vector<SymValue*> args;
    std::transform(
        callInst->arg_begin(), callInst->arg_end(),
        std::back_inserter(args),
        [this](auto x) { return GetResult(x); }
    );

    SymValue *result = RunFunction(&*func, args, prog);
    AllocateResult(callInst, std::unique_ptr<SymValue>(result));
}

void SymbolicEvaluation::Cmp(CmpInst *cmpInst)
{
    Cond c = cmpInst->GetCC();
    Inst *lhs = cmpInst->GetLHS();
    Inst *rhs = cmpInst->GetRHS();
    switch(c) {
    case Cond::EQ:
        std::cout<<"Comparing EQ"<<std::endl;
        break;
    
    case Cond::NE:
        std::cout<<"Comparing NE"<<std::endl;
        break;
    
    case Cond::LT:
        std::cout<<"Comparing LT"<<std::endl;
        break;
    
    case Cond::GT:
        std::cout<<"Comparing GT"<<std::endl;
        break;
    
    case Cond::LE:
        std::cout<<"Comparing LE"<<std::endl;
        break;
    
    case Cond::GE:
        std::cout<<"Comparing GE"<<std::endl;
        break;
    
    default:
        std::cout<<"Comparing "<<(int)c<<std::endl;
        break;
    }
    std::cout<<"lhs: "<<toString(lhs->GetKind())<<" rhs: "<<toString(rhs->GetKind())<<std::endl;

    if(GetResult(lhs)->get_kind() == SymValue::Kind::UNKNOWN) {
        std::cout<<"lhs unknown"<<std::endl;
    } else if (GetResult(rhs)->get_kind() == SymValue::Kind::UNKNOWN) {
        std::cout<<"rhs unknown"<<std::endl;
    } else {
        std::cout<<"compile time comparison"<<std::endl;
    }

    AllocateResult(cmpInst, std::make_unique<UnknownSymValue>());
}

void SymbolicEvaluation::JumpCond(JumpCondInst *jumpCondInst)
{
    Inst *cond = jumpCondInst->GetCond();
    Block *trueBlock = jumpCondInst->GetTrueTarget();
    Block *falseBlock = jumpCondInst->GetFalseTarget();

    SymValue *symCond = GetResult(cond);
    if(symCond->get_kind() == SymValue::Kind::UNKNOWN) {
        Branch((CmpInst*)cond, trueBlock, falseBlock);
    }
}

void SymbolicEvaluation::Mov(MovInst *movInst)
{
    Value *arg = movInst->GetArg();
    std::unique_ptr<SymValue> result = std::make_unique<UnknownSymValue>();
    switch(arg->GetKind()) {
    case Value::Kind::GLOBAL: {
        std::cout<<"Moving global"<<std::endl;
        Global *g = (Global*)arg;
        switch(g->GetKind()) {
        case Global::Kind::FUNC:
            result = std::make_unique<FuncRefSymValue>(g->GetName());
            break;
        }
    } break;

    case Value::Kind::INST: {
        std::cout<<"Moving inst"<<std::endl;
    } break;

    case Value::Kind::CONST: {
        Constant *c = (Constant*)arg;
        switch(c->GetKind()) {
        case Constant::Kind::INT: {
            ConstantInt *ci = (ConstantInt*)c;
            result = std::make_unique<IntSymValue>(ci->GetValue());
            std::cout<<"Moving int "<<ci->GetValue()<<std::endl;
        } break;
        
        case Constant::Kind::FLOAT: {
            ConstantFloat *cf = (ConstantFloat*)c;
            result = std::make_unique<FloatSymValue>(cf->GetValue());
            std::cout<<"Moving float "<<cf->GetValue()<<std::endl;
        } break;

        default:
            std::cout<<"Moving const"<<std::endl;
            break;
        }
    } break;

    case Value::Kind::EXPR: {
        std::cout<<"Moving expr"<<std::endl;
    }
    }

    AllocateResult(movInst, std::move(result));
}

void SymbolicEvaluation::Ret(ReturnInst *returnInst)
{
    AllocateResult(
        returnInst,
        std::make_unique<SymValue>(*GetResult(returnInst->GetValue()))
    );
}

void SymbolicEvaluation::TCall(TailCallInst *tailCallInst, Prog *prog)
{
    Inst *callee = tailCallInst->GetCallee();
    SymValue *v = GetResult(callee);
    if ( v->get_kind() != SymValue::Kind::FUNCREF ) {
        std::cout<<"Tail Called something that isn't a func ref"<<std::endl;
        AllocateResult(tailCallInst, std::make_unique<UnknownSymValue>());
        return;
    }
    FuncRefSymValue *fvp = (FuncRefSymValue*)v;
    std::string_view name = fvp->get_name();

    auto func = FindFuncByName(name, prog);

    if ( func == prog->end() ) {
        std::cout<<"No function with name " << name << " found"<<std::endl;
    }

    std::vector<SymValue*> args;
    std::transform(
        tailCallInst->arg_begin(), tailCallInst->arg_end(),
        std::back_inserter(args),
        [this](auto x) { return GetResult(x); }
    );

    SymValue *result = RunFunction(&*func, args, prog);
    AllocateResult(tailCallInst, std::unique_ptr<SymValue>(result));
    std::cout<<tailCallInst->getNumSuccessors()<<" successors to tail call"<<std::endl;
}

// -----------------------------------------------------------------------------
void SymbolicEvaluation::AllocateResult(Inst *inst, std::unique_ptr<SymValue> value)
{
    reg_allocs[inst] = results.size();
    results.push_back(std::move(value));
    if(results.size() > limit) {
        carryOn = false;
    }
}

SymValue *SymbolicEvaluation::GetResult(Inst *inst)
{
    return results[reg_allocs[inst]].get();
}