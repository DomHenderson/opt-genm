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

#include "flownode.h"
#include "utilities.h"
#include "symvalue.h"

Atom *FindAtomByName(std::string_view name, Prog *prog)
{
    for(auto& segment: prog->data()) {
        for(auto& atom: segment) {
            if(atom.GetName() == name) {
                return &atom;
            }
        }
    }
    return nullptr;
}

llvm::ilist<Block>::iterator FindBlockByName(
    std::string_view name,
    Prog *prog
) {
    for(auto &func: *prog) {
        for(auto &block: func) {
            if(block.GetName() == name) {
                std::cout<<"Found "<<name<<std::endl;
                return block.getIterator();
            }
        }
    }
    std::cout<<"Failed to find block "<<name<<std::endl;
    return prog->end()->end();
}

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

std::string toString(ConstantReg *reg)
{
    if(reg == nullptr) {
        return "nullptr";
    }
    switch(reg->GetValue()) {
        case ConstantReg::Kind::RAX: return "RAX";
        case ConstantReg::Kind::RBX: return "RBX";
        case ConstantReg::Kind::RCX: return "RCX";
        case ConstantReg::Kind::RDX: return "RDX";
        case ConstantReg::Kind::RSI: return "RSI";
        case ConstantReg::Kind::RDI: return "RDI";
        case ConstantReg::Kind::RSP: return "RSP";
        case ConstantReg::Kind::RBP: return "RBP";
        case ConstantReg::Kind::R8: return "R8";
        case ConstantReg::Kind::R9: return "R9";
        case ConstantReg::Kind::R10: return "R10";
        case ConstantReg::Kind::R11: return "R11";
        case ConstantReg::Kind::R12: return "R12";
        case ConstantReg::Kind::R13: return "R13";
        case ConstantReg::Kind::R14: return "R14";
        case ConstantReg::Kind::R15: return "R15";
        case ConstantReg::Kind::RET_ADDR: return "RET_ADDR";
        case ConstantReg::Kind::FRAME_ADDR: return "FRAME_ADDR";
        case ConstantReg::Kind::PC: return "PC";
        default: return "Unknown";
    }
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
                stream<<"ConstantReg ";
                ConstantReg *cr = static_cast<ConstantReg*>(c);
                stream<<toString(cr);
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

    stream<<(inst.IsVoid()
        ?"(Void)"
        :("("+toString(inst.GetType(0))+")"));

    return stream.str(); 
}

std::string toString(Inst *inst)
{
    return inst == nullptr
        ? "nullptr"
        : toString(*inst);
}

std::string toString(SymValue::Kind k)
{
    switch(k) {
    case SymValue::Kind::STATICPTR: return "STATICPTR";
    case SymValue::Kind::BOOL: return "BOOl";
    case SymValue::Kind::EXTERN: return "EXTERN";
    case SymValue::Kind::FLOAT: return "FLOAT";
    case SymValue::Kind::FUNCREF: return "FUNCREF";
    case SymValue::Kind::INT: return "INT";
    case SymValue::Kind::UNKNOWN: return "UNKNOWN";
    default: return "Failed string conversion of symvalue kind";
    }
}

std::string toString(SymValue &value)
{
    std::ostringstream stream;

    switch(value.get_kind()) {
    case SymValue::Kind::STATICPTR: {
        auto addr = static_cast<StaticPtrSymValue&>(value);
        stream<<"addr "<<addr.toString();
    } break;
    case SymValue::Kind::BLOCKREF: {
        auto b = static_cast<BlockRefSymValue&>(value);
        stream<<"block "<<b.get_name();
    } break;
    case SymValue::Kind::BOOL: {
        auto b = static_cast<BoolSymValue&>(value);
        stream<<"bool "<<(b.get_value()?"true":"false");
    } break;
    case SymValue::Kind::EXTERN: {
        auto ext = static_cast<ExternSymValue&>(value);
        stream<<"extern "<<ext.get_name();
    } break;
    case SymValue::Kind::FLOAT: {
        auto f = static_cast<FloatSymValue&>(value);
        stream<<"float "<<f.toString();
    } break;
    case SymValue::Kind::FUNCREF: {
        auto f = static_cast<FuncRefSymValue&>(value);
        stream<<"funcref "<<f.get_name();
    } break;
    case SymValue::Kind::INT: {
        auto i = static_cast<IntSymValue&>(value);
        stream<<"int "<<i.toString();
    } break;
    case SymValue::Kind::UNKNOWN: {
        auto unknownValue = static_cast<UnknownSymValue&>(value);
        stream<<"symbolic value "<<unknownValue.get_name();
    }
    }

    stream<<" ("<<toString(value.get_type())<<")";

    return stream.str();
}

std::string toString(SymValue *value)
{
    return value==nullptr
        ? "nullptr"
        : toString(*value);
}

std::string toString(Type type)
{
    switch(type) {
    case Type::F32: return "F32";
    case Type::F64: return "F64";
    case Type::I128: return "I128";
    case Type::I16: return "I16";
    case Type::I32: return "I32";
    case Type::I64: return "I64";
    case Type::I8: return "I8";
    case Type::U128: return "U128";
    case Type::U16: return "U16";
    case Type::U32: return "U32";
    case Type::U64: return "U64";
    case Type::U8: return "U8";
    default: return "Unknown type";
    }
}

std::string ptrToString(void *ptr)
{
    std::stringstream stream;
    stream << ptr;
    return stream.str();
}

bool isIntType(Type type)
{
    switch(type) {
    case Type::F32:
    case Type::F64:
        return false;
    }
    return true;
}

bool isSigned(Type type)
{
    switch(type) {
    case Type::F32:
    case Type::F64:
    case Type::I8:
    case Type::I16:
    case Type::I32:
    case Type::I64:
    case Type::I128:
        return true;
    }
    return false;
}

unsigned bitLength(Type type)
{
    return 8*byteLength(type);
}

unsigned byteLength(Type type)
{
    switch(type) {
    case Type::I8:
    case Type::U8:
        return 1;
    
    case Type::I16:
    case Type::U16:
        return 2;
    
    case Type::F32:
    case Type::I32:
    case Type::U32:
        return 4;
    
    case Type::F64:
    case Type::I64:
    case Type::U64:
        return 8;
    
    case Type::I128:
    case Type::U128:
        return 16;

    default:
        std::cout<<"Attempted to find length of unknown type"<<std::endl;
        return -1;
    }
}

std::optional<Type> unsignedOfLength(unsigned length)
{
    switch(length) {
    case 1: return Type::U8;
    case 2: return Type::U16;
    case 4: return Type::U32;
    case 8: return Type::U64;
    case 16: return Type::U128;
    default: return std::nullopt;
    }
}

bool knownSafeExtern(std::string_view name)
{
    std::cout<<"checking if "<<name<<" is safe"<<std::endl;
    return name == "getenv"
        || name == "secure_getenv"
        || name == "strlen";
}

std::pair<SymValue*,SymValue*> getOperandValues(
    BinaryInst *inst,
    FlowNode *node
) {
    return std::pair(
        node->GetResult(inst->GetLHS()),
        node->GetResult(inst->GetRHS())
    );
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
                    std::cout<<"Symbol "<<item->GetSymbol()->GetName()<<std::endl;
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