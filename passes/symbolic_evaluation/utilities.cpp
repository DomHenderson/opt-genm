#include <algorithm>
#include <iostream>
#include <map>
#include <sstream>
#include <string>
#include <string_view>
#include <unordered_map>

#include <llvm/ADT/ilist.h>

#include "core/atom.h"
#include "core/block.h"
#include "core/constant.h"
#include "core/data.h"
#include "core/func.h"
#include "core/global.h"
#include "core/inst.h"
#include "core/prog.h"
#include "core/value.h"

#include "utilities.h"

llvm::ilist<Func>::iterator FindFuncByName(
    std::string_view name,
    Prog *prog
) {
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

std::string toString(Inst &inst)
{
    std::ostringstream stream;

    stream<<(void*)&inst<<toString(inst.GetKind())<<" ";
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
                stream<<"Constant"<<ci->GetValue()<<" ";
            } break;
            case Constant::FLOAT: {
                ConstantFloat *cf = (ConstantFloat*)c;
                stream<<"Constant"<<cf->GetValue()<<" ";
            } break;
            case Constant::REG: {
                stream<<"ConstantReg"<<" ";
                ConstantReg *cr = static_cast<ConstantReg*>(c);
                switch(cr->GetValue()) {
                case ConstantReg::Kind::RAX: stream<<"RAX"; break;
                case ConstantReg::Kind::RBX: stream<<"RBX"; break;
                case ConstantReg::Kind::RCX: stream<<"RCX"; break;
                case ConstantReg::Kind::RDX: stream<<"RDX"; break;
                case ConstantReg::Kind::RSI: stream<<"RSI"; break;
                case ConstantReg::Kind::RDI: stream<<"RDI"; break;
                case ConstantReg::Kind::RSP: stream<<"RSP"; break;
                case ConstantReg::Kind::RBP: stream<<"RBP"; break;
                case ConstantReg::Kind::R8: stream<<"R8"; break;
                case ConstantReg::Kind::R9: stream<<"R9"; break;
                case ConstantReg::Kind::R10: stream<<"R10"; break;
                case ConstantReg::Kind::R11: stream<<"R11"; break;
                case ConstantReg::Kind::R12: stream<<"R12"; break;
                case ConstantReg::Kind::R13: stream<<"R13"; break;
                case ConstantReg::Kind::R14: stream<<"R14"; break;
                case ConstantReg::Kind::R15: stream<<"R15"; break;
                case ConstantReg::Kind::RET_ADDR: stream<<"RET_ADDR"; break;
                case ConstantReg::Kind::FRAME_ADDR: stream<<"FRAME_ADDR"; break;
                case ConstantReg::Kind::PC: stream<<"PC"; break;
                default: stream<<"Unknown";
                }
            } break;
            }
        } break;
        
        case Value::Kind::EXPR: {
            stream<<"Expr"<<" ";
        } break;
        
        case Value::Kind::GLOBAL: {
            stream<<"Global";
            Global *g = (Global*)v;
            switch(g->GetKind()) {
            case Global::Kind::ATOM:
                stream<<"Atom";
                break;
            
            case Global::Kind::BLOCK:
                stream<<"Block";
                break;
            
            case Global::Kind::EXTERN:
                stream<<"Extern";
                break;
            
            case Global::Kind::FUNC:
                stream<<"Func";
                break;
            
            case Global::Kind::SYMBOL:
                stream<<"Symbol";
                break;
            }
            stream<<g->GetName()<<" ";
        } break;
        
        case Value::Kind::INST:
            Inst *i = (Inst*)v;
            stream<<"Inst"<<(void*)i<<toString(i->GetKind())<<" ";
            break;
        }
    }      

    return stream.str(); 
}

void PrintCodeInfo(Prog *prog)
{
    std::cout<<"----------------------------------------"<<std::endl;
    std::cout<<"CODE START"<<std::endl;
    std::cout<<"----------------------------------------"<<std::endl;
    std::cout<<std::endl;

    std::unordered_map<int, std::optional<int>> registers;
    std::map<Inst::Kind, bool> seen;
    for (auto &func : *prog) {
        std::cout<<func.GetName()<<"---";
        std::cout<<func.size()<<std::endl;
        for (auto &block : func) {
            std::cout<<block.GetName()<<"---"<<std::endl;
            for (auto &inst : block) {
                seen[inst.GetKind()] = true;
                std::cout<<toString(inst)<<std::endl;
            }
        }
    }

    std::cout<<std::endl;
    std::cout<<"----------------------------------------"<<std::endl;
    std::cout<<"CODE END"<<std::endl;
    std::cout<<"----------------------------------------"<<std::endl;
    std::cout<<std::endl;
    std::cout<<"----------------------------------------"<<std::endl;
    std::cout<<"INSTRUCTIONS START"<<std::endl;
    std::cout<<"----------------------------------------"<<std::endl;
    std::cout<<std::endl;

    for(auto &entry: seen) {
        std::cout<<toString(entry.first)<<(entry.second?" seen":" not seen")<<std::endl;
    }

    std::cout<<std::endl;
    std::cout<<"----------------------------------------"<<std::endl;
    std::cout<<"INSTRUCTIONS END"<<std::endl;
    std::cout<<"----------------------------------------"<<std::endl;
    std::cout<<std::endl;
}

void PrintDataInfo(Prog *prog)
{
    std::cout<<std::endl;
    std::cout<<"----------------------------------------"<<std::endl;
    std::cout<<"DATA START"<<std::endl;
    std::cout<<"----------------------------------------"<<std::endl;
    std::cout<<std::endl;
    for(auto &data: prog->data()) {
        std::cout<<"Start data segment: "<<data.GetName()<<std::endl;
        for(auto &atom: data) {
            std::cout<<"Start: "<<atom.GetName()<<std::endl;
            for (auto &item: atom) {
                switch(item->GetKind()) {
                case Item::Kind::ALIGN:
                    std::cout<<"Align "<<item->GetAlign()<<std::endl;
                    break;
                
                case Item::Kind::END:
                    std::cout<<"End"<<std::endl;
                    break;
                
                case Item::Kind::FLOAT64:
                    std::cout<<"Float64 "<<item->GetFloat64()<<std::endl;
                    break;
                
                case Item::Kind::INT16:
                    std::cout<<"Int16 "<<item->GetInt16()<<std::endl;
                    break;

                case Item::Kind::INT32:
                    std::cout<<"Int32 "<<item->GetInt32()<<std::endl;
                    break;
                
                case Item::Kind::INT64:
                    std::cout<<"Int64 "<<item->GetInt64()<<std::endl;
                    break;
                
                case Item::Kind::INT8:
                    std::cout<<"Int8 "<<item->GetInt8()<<std::endl;
                    break;
                
                case Item::Kind::SPACE:
                    std::cout<<"Space "<<item->GetSpace()<<std::endl;
                    break;
                
                case Item::Kind::STRING:
                    std::cout<<"String "<<static_cast<std::string>(item->GetString())<<std::endl;
                    break;
                
                case Item::Kind::SYMBOL:
                    std::cout<<"Symbol "<<item->GetSymbol()<<std::endl;
                    break;
                }
            }
            std::cout<<"End atom"<<std::endl;
        }
    }
    std::cout<<std::endl;
    std::cout<<"----------------------------------------"<<std::endl;
    std::cout<<"DATA END"<<std::endl;
    std::cout<<"----------------------------------------"<<std::endl;
    std::cout<<std::endl;
}