// This file if part of the genm-opt project.
// Licensing information can be found in the LICENSE file.
// (C) 2018 Nandor Licker. All rights reserved.

#pragma once

#include <ostream>
#include <unordered_map>

#include <llvm/Support/raw_ostream.h>

#include "core/attr.h"
#include "core/inst.h"

class Block;
class Data;
class Func;
class Prog;


/**
 * Prints a program.
 */
class Printer {
public:
  /// Initialises the printer.
  Printer(llvm::raw_ostream &os) : os_(os) {}

  /// Prints a whole program.
  void Print(const Prog *prog);
  /// Prints a data segment.
  void Print(const Data *data);
  /// Prints a function.
  void Print(const Func *func);
  /// Prints a block.
  void Print(const Block *block);
  /// Prints an instruction.
  void Print(const Inst *inst);
  /// Prints a value.
  void Print(const Value *val);
  /// Prints an expression.
  void Print(const Expr *expr);
  /// Prints a type.
  void Print(Type type);
  /// Print a calling convention.
  void Print(CallingConv conv);
  /// Print a calling convention.
  void Print(Visibility visibility);

private:
  /// Output stream.
  llvm::raw_ostream &os_;
  /// Instruction to identifier map.
  std::unordered_map<const Inst *, unsigned> insts_;
};
