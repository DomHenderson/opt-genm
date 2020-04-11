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
    constexpr Result BoolToResult(bool b);
    std::pair<Result, std::optional<z3::expr>> BoolToResultPair(bool b);
    SymValue *ToSymValue(Result r, Type type);
    Result Not(Result r);

    std::pair<Result, std::optional<z3::expr>> EQ(SymValue *lv, SymValue *rv, FlowNode *node);
    std::pair<Result, std::optional<z3::expr>> NEQ(SymValue *lv, SymValue *rv, FlowNode *node);
    std::pair<Result, std::optional<z3::expr>> LT(SymValue *lv, SymValue *rv, FlowNode *node);
    std::pair<Result, std::optional<z3::expr>> GT(SymValue *lv, SymValue *rv, FlowNode *node);
    std::pair<Result, std::optional<z3::expr>> LE(SymValue *lv, SymValue *rv, FlowNode *node);
    std::pair<Result, std::optional<z3::expr>> GE(SymValue *lv, SymValue *rv, FlowNode *node);
}