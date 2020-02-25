#pragma once

#include "core/type.h"
#include "symvalue.h"

namespace SymComp {
    enum class Result {
        TRUE,
        UNKNOWN,
        FALSE
    };
    Result FromBool(bool b);
    SymValue *ToSymValue(Result r, Type type);
    Result Not(Result r);
    Result EQ(SymValue *lv, SymValue *rv);
    Result NEQ(SymValue *lv, SymValue *rv);
    Result LT(SymValue* lv, SymValue *rv);
    std::pair<Result, z3::expr> GT(SymValue *lv, SymValue *rv, FlowNode *node = nullptr);
    std::pair<Result, z3::expr> LE(SymValue *lv, SymValue *rv, FlowNode *node = nullptr);
    Result GE(SymValue *lv, SymValue *rv);
}