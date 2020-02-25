#pragma once

#include <optional>

#include "z3++.h"

#include "core/type.h"
#include "symvalue.h"

namespace SymComp {
    enum class Result {
        TRUE,
        UNKNOWN,
        FALSE
    };
    Result BoolToResult(bool b);
    std::pair<Result, std::optional<z3::expr>> BoolToResultPair(bool b, z3::context &c);
    SymValue *ToSymValue(Result r, Type type);
    Result Not(Result r);
    std::pair<Result, std::optional<z3::expr>> EQ(SymValue *lv, SymValue *rv, FlowNode *node, std::optional<z3::expr> assumptions = std::nullopt);
    std::pair<Result, std::optional<z3::expr>> NEQ(SymValue *lv, SymValue *rv, FlowNode *node);
    Result LT(SymValue* lv, SymValue *rv);
    std::pair<Result, std::optional<z3::expr>> GT(SymValue *lv, SymValue *rv, FlowNode *node);
    std::pair<Result, std::optional<z3::expr>> LE(SymValue *lv, SymValue *rv, FlowNode *node);
    Result GE(SymValue *lv, SymValue *rv);
}