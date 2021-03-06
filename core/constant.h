// This file if part of the genm-opt project.
// Licensing information can be found in the LICENSE file.
// (C) 2018 Nandor Licker. All rights reserved.

#pragma once

#include "core/value.h"



/**
 * Base class of non-mutable values.
 */
class Constant : public Value {
public:
  /**
   * Enumeration of constant kinds.
   */
  enum Kind {
    INT,
    FLOAT,
    REG
  };

  Constant(Kind kind) : Value(Value::Kind::CONST), kind_(kind) {}

  virtual ~Constant();

  Kind GetKind() const { return kind_; }

  bool Is(Kind kind) { return GetKind() == kind; }

private:
  /// Returns the kind of the constant.
  Kind kind_;
};


/**
 * Constant integer.
 */
class ConstantInt final : public Constant {
public:
  /// Kind of the constant.
  static constexpr Constant::Kind kConstKind = Constant::Kind::INT;

public:
  ConstantInt(int64_t v) : Constant(Constant::Kind::INT), v_(v) {}

  int64_t GetValue() const { return v_; }

private:
  int64_t v_;
};


/**
 * Constant float.
 */
class ConstantFloat final : public Constant {
public:
  ConstantFloat(double v) : Constant(Constant::Kind::FLOAT), v_(v) {}

  double GetValue() const { return v_; }

private:
  double v_;
};


/**
 * Register reference.
 */
class ConstantReg final : public Constant {
public:
  /// Kind of the constant.
  static constexpr Constant::Kind kConstKind = Constant::Kind::REG;

  /// Enumeration of hardware registers.
  enum class Kind {
    /// X86 Architectural registers.
    RAX, RBX, RCX, RDX, RSI, RDI, RSP, RBP,
    R8, R9, R10, R11, R12, R13, R14, R15,
    /// Virtual register taking the value of the return address.
    RET_ADDR,
    /// Virtual register taking the value of the top of the stack.
    FRAME_ADDR,
    // Current program counter.
    PC,
  };

  ConstantReg(Kind kind) : Constant(Constant::Kind::REG), kind_(kind) {}

  Kind GetValue() const { return kind_; }

private:
  Kind kind_;
};
