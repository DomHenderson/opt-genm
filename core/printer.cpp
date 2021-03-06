// This file if part of the genm-opt project.
// Licensing information can be found in the LICENSE file.
// (C) 2018 Nandor Licker. All rights reserved.

#include "core/printer.h"
#include "core/block.h"
#include "core/constant.h"
#include "core/data.h"
#include "core/func.h"
#include "core/insts_call.h"
#include "core/prog.h"
#include "core/symbol.h"



// -----------------------------------------------------------------------------
void Printer::Print(const Prog *prog)
{
  // Print the text segment.
  os_ << "\t.code\n";
  for (const Func &f : *prog) {
    Print(&f);
  }
  os_ << "\n";

  // Print all data segments.
  for (const Data &data : prog->data()) {
    os_ << "\t.data\t" << data.getName() << "\n";
    Print(&data);
    os_ << "\n";
  }

  // Print the extern segment.
  for (const Extern *ext : prog->externs()) {
    os_ << "\t.extern\t" << ext->getName() << "\n";
  }
}

// -----------------------------------------------------------------------------
void Printer::Print(const Data *data)
{
  for (auto &atom : *data) {
    os_ << atom.getName() << ":\n";
    for (auto &item : atom) {
      switch (item->GetKind()) {
        case Item::Kind::INT8: {
          os_ << "\t.byte\t"   << item->GetInt8();
          break;
        }
        case Item::Kind::INT16: {
          os_ << "\t.short\t"  << item->GetInt16();
          break;
        }
        case Item::Kind::INT32: {
          os_ << "\t.long\t"   << item->GetInt32();
          break;
        }
        case Item::Kind::INT64: {
          os_ << "\t.quad\t"   << item->GetInt64();
          break;
        }
        case Item::Kind::FLOAT64: {
          os_ << "\t.double\t" << item->GetFloat64();
          break;
        }
        case Item::Kind::SYMBOL: {
          if (auto *symbol = item->GetSymbol()) {
            os_ << "\t.quad\t" << symbol->getName();
            if (auto offset = item->GetOffset()) {
              if (offset < 0) {
                os_ << "-" << -offset;
              }
              if (offset > 0) {
                os_ << "+" << +offset;
              }
            }
          } else {
            os_ << "\t.quad\t" << 0ull;
          }
          break;
        }
        case Item::Kind::ALIGN: {
          os_ << "\t.align\t" << item->GetAlign();
          break;
        }
        case Item::Kind::SPACE: {
          os_ << "\t.space\t" << item->GetSpace();
          break;
        }
        case Item::Kind::STRING: {
          os_ << "\t.ascii\t\"";
          for (const uint8_t c : item->GetString()) {
            switch (c) {
              case '\t': os_ << "\\t"; break;
              case '\n': os_ << "\\n"; break;
              case '\\': os_ << "\\\\"; break;
              case '\"': os_ << "\\\""; break;
              default: {
                if (isprint(c)) {
                  os_ << c;
                } else {
                  os_ << "\\";
                  os_ << static_cast<char>('0' + ((c / 8 / 8) % 8));
                  os_ << static_cast<char>('0' + ((c / 8) % 8));
                  os_ << static_cast<char>('0' + (c % 8));
                }
              }
              break;
            }
          }
          os_ << "\"";
          break;
        }
        case Item::Kind::END: {
          os_ << "\t.end";
          break;
        }
      }
      os_ << "\n";
    }
    os_ << "\n";
  }
}

// -----------------------------------------------------------------------------
void Printer::Print(const Func *func)
{
  os_ << func->getName() << ":\n";
  os_ << "\t.visibility\t"; Print(func->GetVisibility()); os_ << "\n";
  os_ << "\t.call\t"; Print(func->GetCallingConv()); os_ << "\n";
  if (auto stackSize = func->GetStackSize()) {
    os_ << "\t.stack\t" << stackSize << "\n";
  }
  os_ << "\t.args\t" << func->IsVarArg();
  for (const auto type : func->params()) {
    os_ << ", "; Print(type);
  }
  os_ << "\n";
  for (const Block &b : *func) {
    for (const Inst &i : b) {
      insts_.emplace(&i, insts_.size());
    }
  }
  for (const Block &b : *func) {
    Print(&b);
  }
  insts_.clear();
  os_ << "\n";
}

// -----------------------------------------------------------------------------
void Printer::Print(const Block *block)
{
  os_ << block->getName() << ":\n";
  for (const Inst &i : *block) {
    Print(&i);
  }
}

// -----------------------------------------------------------------------------
static const char *kNames[] =
{
  "call", "tcall", "invoke", "tinvoke", "ret",
  "jcc", "ji", "jmp", "switch", "trap",
  "ld", "st",
  "xchg",
  "set",
  "vastart",
  "alloca",
  "arg", "frame", "undef",
  "select",
  "abs", "neg",  "sqrt", "sin", "cos",
  "sext", "zext", "fext",
  "mov", "trunc",
  "add", "and", "cmp", "div", "rem", "mul", "or",
  "rotl", "sll", "sra", "srl", "sub", "xor",
  "pow", "copysign",
  "uaddo", "umulo",
  "phi",
};

// -----------------------------------------------------------------------------
void Printer::Print(const Inst *inst)
{
  os_ << "\t" << kNames[static_cast<uint8_t>(inst->GetKind())];
  // Print the data width the instruction is operating on.
  if (auto size = inst->GetSize()) {
    os_ << "." << *size;
  }
  // Print instruction-specific attributes.
  switch (inst->GetKind()) {
    case Inst::Kind::INVOKE:
    case Inst::Kind::TINVOKE:
    case Inst::Kind::TCALL: {
      os_ << ".";
      auto *TermInst = static_cast<const CallSite<TerminatorInst> *>(inst);
      Print(TermInst->GetCallingConv());
      break;
    }
    case Inst::Kind::CALL: {
      os_ << ".";
      Print(static_cast<const CallInst *>(inst)->GetCallingConv());
      break;
    }
    default: {
      break;
    }
  }
  // Print the return value (if it exists).
  if (auto numRet = inst->GetNumRets()) {
    for (unsigned i = 0; i < numRet; ++i) {
      os_ << ".";
      Print(inst->GetType(i));
    }
    os_ << "\t";
    auto it = insts_.find(inst);
    if (it == insts_.end()) {
      os_ << "$<" << inst << ">";
    } else {
      os_ << "$" << it->second;
    }
  } else {
    os_ << "\t";
  }
  // Print the arguments.
  for (auto it = inst->value_op_begin(); it != inst->value_op_end(); ++it) {
    if (inst->GetNumRets() || it != inst->value_op_begin()) {
      os_ << ", ";
    }
    Print(*it);
  }
  // Print any annotations.
  for (const auto &annot : inst->annots()) {
    os_ << " ";
    switch (annot) {
      case CAML_FRAME: os_ << "@caml_frame"; break;
      case CAML_ROOT:  os_ << "@caml_root";  break;
      case CAML_VALUE: os_ << "@caml_value"; break;
      case CAML_ADDR:  os_ << "@caml_addr";  break;
    }
  }
  os_ << "\n";
}

// -----------------------------------------------------------------------------
void Printer::Print(const Value *val)
{
  if (reinterpret_cast<uintptr_t>(val) & 1) {
    os_ << "<" << (reinterpret_cast<uintptr_t>(val) >> 1) << ">";
    return;
  }

  switch (val->GetKind()) {
    case Value::Kind::INST: {
      auto it = insts_.find(static_cast<const Inst *>(val));
      if (it == insts_.end()) {
        os_ << "$<" << val << ">";
      } else {
        os_ << "$" << it->second;
      }
      break;
    }
    case Value::Kind::GLOBAL: {
      os_ << static_cast<const Global *>(val)->getName();
      break;
    }
    case Value::Kind::EXPR: {
      Print(static_cast<const Expr *>(val));
      break;
    }
    case Value::Kind::CONST: {
      switch (static_cast<const Constant *>(val)->GetKind()) {
        case Constant::Kind::INT: {
          os_ << static_cast<const ConstantInt *>(val)->GetValue();
          break;
        }
        case Constant::Kind::FLOAT: {
          os_ << static_cast<const ConstantFloat *>(val)->GetValue();
          break;
        }
        case Constant::Kind::REG: {
          switch (static_cast<const ConstantReg *>(val)->GetValue()) {
            case ConstantReg::Kind::RAX:        os_ << "$rax";        break;
            case ConstantReg::Kind::RBX:        os_ << "$rbx";        break;
            case ConstantReg::Kind::RCX:        os_ << "$rcx";        break;
            case ConstantReg::Kind::RDX:        os_ << "$rdx";        break;
            case ConstantReg::Kind::RSI:        os_ << "$rsi";        break;
            case ConstantReg::Kind::RDI:        os_ << "$rdi";        break;
            case ConstantReg::Kind::RSP:        os_ << "$rsp";        break;
            case ConstantReg::Kind::RBP:        os_ << "$rbp";        break;
            case ConstantReg::Kind::R8:         os_ << "$r8";         break;
            case ConstantReg::Kind::R9:         os_ << "$r9";         break;
            case ConstantReg::Kind::R10:        os_ << "$r10";        break;
            case ConstantReg::Kind::R11:        os_ << "$r11";        break;
            case ConstantReg::Kind::R12:        os_ << "$r12";        break;
            case ConstantReg::Kind::R13:        os_ << "$r13";        break;
            case ConstantReg::Kind::R14:        os_ << "$r14";        break;
            case ConstantReg::Kind::R15:        os_ << "$r15";        break;
            case ConstantReg::Kind::RET_ADDR:   os_ << "$ret_addr";   break;
            case ConstantReg::Kind::FRAME_ADDR: os_ << "$frame_addr"; break;
            case ConstantReg::Kind::PC:         os_ << "$pc";         break;
          }
          break;
        }
      }
      break;
    }
  }
}

// -----------------------------------------------------------------------------
void Printer::Print(const Expr *expr)
{
  switch (expr->GetKind()) {
    case Expr::Kind::SYMBOL_OFFSET: {
      auto *symExpr = static_cast<const SymbolOffsetExpr *>(expr);
      os_ << symExpr->GetSymbol()->getName();
      int64_t off = symExpr->GetOffset();
      if (off < 0) {
        os_ << " - " << -off;
      } else {
        os_ << " + " << +off;
      }
      break;
    }
  }
}

// -----------------------------------------------------------------------------
void Printer::Print(Type type)
{
  switch (type) {
    case Type::I8:    os_ << "i8";   break;
    case Type::I16:   os_ << "i16";  break;
    case Type::I32:   os_ << "i32";  break;
    case Type::I64:   os_ << "i64";  break;
    case Type::I128:  os_ << "i128"; break;
    case Type::U8:    os_ << "u8";   break;
    case Type::U16:   os_ << "u16";  break;
    case Type::U32:   os_ << "u32";  break;
    case Type::U64:   os_ << "u64";  break;
    case Type::U128:  os_ << "u128"; break;
    case Type::F32:   os_ << "f32";  break;
    case Type::F64:   os_ << "f64";  break;
  }
}

// -----------------------------------------------------------------------------
void Printer::Print(CallingConv conv)
{
  switch (conv) {
    case CallingConv::C:          os_ << "c";          break;
    case CallingConv::FAST:       os_ << "fast";       break;
    case CallingConv::CAML:       os_ << "caml";       break;
    case CallingConv::CAML_EXT:   os_ << "caml_ext";   break;
    case CallingConv::CAML_ALLOC: os_ << "caml_alloc"; break;
    case CallingConv::CAML_GC:    os_ << "caml_gc";    break;
    case CallingConv::CAML_RAISE: os_ << "caml_raise"; break;
  }
}

// -----------------------------------------------------------------------------
void Printer::Print(Visibility visibility)
{
  switch (visibility) {
    case Visibility::EXTERN: os_ << "extern"; break;
    case Visibility::HIDDEN: os_ << "hidden"; break;
  }
}
