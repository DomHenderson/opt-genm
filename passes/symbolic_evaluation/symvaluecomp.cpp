#include <iostream>
#include <optional>

#include "core/type.h"
#include "flownode.h"
#include "symvalue.h"
#include "symvaluecomp.h"
#include "utilities.h"

using SymComp::Result;

const std::optional<z3::expr> negateOptExpr(std::optional<z3::expr> expr);
std::optional<z3::expr> CreateExpr(SymValue *v, z3::context &c);
Result EvaluateSat(z3::expr, FlowNode *node);
std::pair<Result, std::optional<z3::expr>> InvokeSmt(SymValue *lv, SymValue *rv, std::function<z3::expr(z3::expr, z3::expr)> expr, FlowNode *node);
enum class ComparisonType {EQ, GT, LT};
std::pair<Result, std::optional<z3::expr>> Compare(ComparisonType type, SymValue *lv, SymValue *rv, FlowNode *node, std::optional<z3::expr> assumptions = std::nullopt);

//------------------------------------------------------------------------------
// Interface functions
//------------------------------------------------------------------------------

std::pair<Result, std::optional<z3::expr>> SymComp::EQ(SymValue *lv, SymValue *rv, FlowNode *node)
{
    return Compare(ComparisonType::EQ, lv, rv, node);
}

std::pair<Result, std::optional<z3::expr>> SymComp::NEQ(SymValue *lv, SymValue *rv, FlowNode *node)
{
    auto [eqResult, eqConstraint] = SymComp::EQ(lv, rv, node);
    return {SymComp::Not(eqResult), negateOptExpr(eqConstraint)};
}

std::pair<Result, std::optional<z3::expr>> SymComp::LT(SymValue *lv, SymValue *rv, FlowNode *node)
{
    return Compare(ComparisonType::GT, lv, rv, node);
}

std::pair<Result, std::optional<z3::expr>> SymComp::GT(SymValue *lv, SymValue *rv, FlowNode *node)
{
    return Compare(ComparisonType::GT, lv, rv, node);
}

std::pair<Result, std::optional<z3::expr>> SymComp::LE(SymValue *lv, SymValue *rv, FlowNode *node)
{
    auto [gtResult, gtConstraint] = SymComp::GT(lv, rv, node);
    return {SymComp::Not(gtResult), negateOptExpr(gtConstraint)};
}

std::pair<Result, std::optional<z3::expr>> SymComp::GE(SymValue *lv, SymValue *rv, FlowNode *node)
{
    auto [ltResult, ltConstraint] = SymComp::LT(lv, rv, node);
    return {SymComp::Not(ltResult), negateOptExpr(ltConstraint)};
}

//-----------------------------------------------------------------------------
// Base comparison functions
//-----------------------------------------------------------------------------

std::pair<Result, std::optional<z3::expr>> EQ_Impl(SymValue *lv, SymValue *rv, FlowNode *node, std::optional<z3::expr> assumptions)
{
    assert(lv->get_kind() != SymValue::Kind::CONDITIONAL);
    assert(rv->get_kind() != SymValue::Kind::CONDITIONAL);

    auto &c = node->get_context();
    //std::cout<<"Obtained context"<<std::endl;
    if ( lv == rv ) {
        return {
            Result::TRUE,
            c.bool_val(true)
        };
    } else if (lv->get_kind() == rv->get_kind()) {
        //std::cout<<"Same type"<<std::endl;
        switch(lv->get_kind()) {
        case SymValue::Kind::STATICPTR: {
            auto l = static_cast<StaticPtrSymValue*>(lv);
            auto r = static_cast<StaticPtrSymValue*>(rv);
            return SymComp::BoolToResultPair(l->get_name() == r->get_name() && l->get_offset() == r->get_offset());
        }

        case SymValue::Kind::BLOCKREF: {
            auto l = static_cast<BlockRefSymValue*>(lv);
            auto r = static_cast<BlockRefSymValue*>(rv);
            return SymComp::BoolToResultPair(l->get_name() == r->get_name());
        }

        case SymValue::Kind::BOOL: {
            auto l = static_cast<BoolSymValue*>(lv);
            auto r = static_cast<BoolSymValue*>(rv);
            return SymComp::BoolToResultPair(l->get_value() == r->get_value());
        }

        case SymValue::Kind::FLOAT: {
            auto l = static_cast<FloatSymValue*>(lv);
            auto r = static_cast<FloatSymValue*>(rv);
            bool result = l->get_value().compare(r->get_value()) == llvm::APFloatBase::cmpEqual;
            return SymComp::BoolToResultPair(l->get_value().compare(r->get_value()) == llvm::APFloatBase::cmpEqual);
        }

        case SymValue::Kind::FUNCREF: {
            auto l = static_cast<FuncRefSymValue*>(lv);
            auto r = static_cast<FuncRefSymValue*>(rv);
            bool result = l->get_name() == r->get_name();
            return SymComp::BoolToResultPair(l->get_name() == r->get_name());
        }

        case SymValue::Kind::INT: {
            auto l = static_cast<IntSymValue*>(lv);
            auto r = static_cast<IntSymValue*>(rv);
            bool result = l->get_value() == r->get_value();
            return SymComp::BoolToResultPair(l->get_value() == r->get_value());
        }
        }
    }

    auto result = InvokeSmt(lv, rv, [](auto l, auto r){return l == r;}, node);
    return result;
}

std::pair<Result, std::optional<z3::expr>> LT_Impl(SymValue *lv, SymValue *rv, FlowNode *node, std::optional<z3::expr> assumptions)
{
    assert(lv->get_kind() != SymValue::Kind::CONDITIONAL);
    assert(rv->get_kind() != SymValue::Kind::CONDITIONAL);

    auto &c = node->get_context();

    if ( lv == rv ) {
        return SymComp::BoolToResultPair(false);
    } else if (lv->get_kind() == rv->get_kind()) {
        switch(lv->get_kind()) {
        case SymValue::Kind::FLOAT: {
            auto l = static_cast<FloatSymValue*>(lv);
            auto r = static_cast<FloatSymValue*>(rv);
            return SymComp::BoolToResultPair(l->get_value().compare(r->get_value()) == llvm::APFloatBase::cmpLessThan);
        }

        case SymValue::Kind::INT: {
            auto l = static_cast<IntSymValue*>(lv);
            auto r = static_cast<IntSymValue*>(rv);
            bool result = isSigned(l->get_type())
                ? l->get_value().slt(r->get_value())
                : l->get_value().ult(r->get_value());
            return SymComp::BoolToResultPair(result);
        }
        }
    }

    return isSigned(lv->get_type())
        ? InvokeSmt(lv, rv, [](auto l, auto r){return l < r;}, node)
        : InvokeSmt(lv, rv, [](auto l, auto r){return z3::ult(l, r);}, node);
}

std::pair<Result, std::optional<z3::expr>> GT_Impl(SymValue *lv, SymValue *rv, FlowNode *node, std::optional<z3::expr> assumptions)
{
    return LT_Impl(rv, lv, node, assumptions);
}

//------------------------------------------------------------------------------
// Conditional handling
//------------------------------------------------------------------------------

using ComparisonFunction = std::function<std::pair<Result, std::optional<z3::expr>>(SymValue*, SymValue*, FlowNode*, std::optional<z3::expr>)>;

std::unordered_map<ComparisonType, ComparisonFunction> funcMap = {
    {ComparisonType::EQ, EQ_Impl},
    {ComparisonType::GT, GT_Impl},
    {ComparisonType::LT, LT_Impl}
};

std::pair<Result, std::optional<z3::expr>> CombineResults(z3::expr condition, Result lResult, std::optional<z3::expr> lExpr, Result rResult, std::optional<z3::expr> rExpr)
{
    if(lResult == rResult && lResult != Result::UNKNOWN) {
        return {lResult, std::nullopt};
    }

    if(lExpr.has_value()) {
        if(rExpr.has_value()) {
            return {Result::UNKNOWN, (z3::implies(condition, *lExpr)&&z3::implies(!condition, *rExpr))};
        } else {
            return {Result::UNKNOWN, (z3::implies(condition, *lExpr))};
        }
    } else {
        if(rExpr.has_value()) {
            return {Result::UNKNOWN, (z3::implies(!condition, *rExpr))};
        } else {
            return {Result::UNKNOWN, std::nullopt};
        }
    }
}

std::pair<Result, std::optional<z3::expr>> Compare(ComparisonType type, SymValue *lv, SymValue *rv, FlowNode *node, std::optional<z3::expr> assumptions)
{
    if(lv->get_kind() == SymValue::Kind::CONDITIONAL) {
        CondSymValue* condSymValue = static_cast<CondSymValue*>(lv);
        z3::expr condition = condSymValue->get_condition();
        auto [lResult, lExpr] = Compare(type, condSymValue->if_true(), rv, node, assumptions.has_value() ? *assumptions && condition : condition);
        auto [rResult, rExpr] = Compare(type, condSymValue->if_false(), rv, node, assumptions.has_value() ? *assumptions && !condition : !condition);
        return CombineResults(condition, lResult, lExpr, rResult, rExpr);
    }

    if(rv->get_kind() == SymValue::Kind::CONDITIONAL) {
        CondSymValue* condSymValue = static_cast<CondSymValue*>(rv);
        z3::expr condition = condSymValue->get_condition();
        auto [lResult, lExpr] = Compare(type, lv, condSymValue->if_true(), node, assumptions.has_value() ? *assumptions && condition : condition);
        auto [rResult, rExpr] = Compare(type, lv, condSymValue->if_false(), node, assumptions.has_value() ? *assumptions && !condition : !condition);
        return CombineResults(condition, lResult, lExpr, rResult, rExpr);
    }

    return funcMap[type](lv, rv, node, assumptions);
}

//-----------------------------------------------------------------------------
// SMT functions
//-----------------------------------------------------------------------------

std::optional<z3::expr> CreateExpr(SymValue *v, z3::context &c)
{
    //std::cout<<"Creating expr"<<std::endl;
    if(v == nullptr) {
        LogWarning<<"Attempted to create expr of nullptr"<<End();
        return std::nullopt;
    }
    switch(v->get_kind()) {
    case SymValue::Kind::CONDITIONAL: {
        auto condValue = static_cast<CondSymValue*>(v);
        //std::cout<<"Cast"<<std::endl;
        //std::cout<<"Creating expr for "<<toString(condValue->if_true())<<std::endl;
        auto ifTrue = CreateExpr(condValue->if_true(), c);
        //std::cout<<"Created ifTrue"<<std::endl;
        //std::cout<<"Creating expr for "<<toString(condValue->if_false())<<std::endl;
        auto ifFalse = CreateExpr(condValue->if_false(), c);
        //std::cout<<"Created ifFalse"<<std::endl;
        if(!ifTrue.has_value() || !ifFalse.has_value()) {
            return std::nullopt;
        } else {
            return z3::ite(
                condValue->get_condition(),
                *ifTrue,
                *ifFalse
            );
        }
    }
    case SymValue::Kind::INT: {
        //std::cout<<"Creating int expr"<<std::endl;
        auto intValue = static_cast<IntSymValue*>(v);
        //std::cout<<"Cast"<<std::endl;
        uint64_t val = intValue->get_value().getLimitedValue();
        //std::cout<<"Value is "<<val<<std::endl;
        return c.bv_val(
            val,
            bitLength(v->get_type())
        );
    }
    case SymValue::Kind::UNKNOWN:
        return c.bv_const(
            static_cast<UnknownSymValue*>(v)->get_name().c_str(),
            bitLength(v->get_type())
        );
    default:
        //std::cout<<"Returning nullopt"<<std::endl;
        return std::nullopt;
    }
}

std::pair<Result, std::optional<z3::expr>> InvokeSmt(SymValue *lv, SymValue *rv, std::function<z3::expr(z3::expr, z3::expr)> expr, FlowNode *node)
{
    //std::cout<<"Invoking smt"<<std::endl;
    z3::context &c = node->get_context();
    //std::cout<<"Obtained context"<<std::endl;
    std::optional<z3::expr> l = CreateExpr(lv, c);
    //std::cout<<"Created l expr"<<std::endl;
    std::optional<z3::expr> r = CreateExpr(rv, c);
    //std::cout<<"Created r expr"<<std::endl;
    if(!l.has_value()) {
        LogWarning<<"Failed to create expression for "<<toString(lv)<<End();
        return {Result::UNKNOWN, std::nullopt};
    }
    if(!r.has_value()) {
        LogWarning<<"Failed to create expression for "<<toString(rv)<<End();
        return {Result::UNKNOWN, std::nullopt};
    }
    z3::expr expression = expr(*l, *r);
    //std::cout<<"Created full expr"<<std::endl;
    return {EvaluateSat(expression, node), expression};

}

Result EvaluateSat(z3::expr expr, FlowNode *node)
{
    //std::cout<<"Evaluating sat"<<std::endl;
    z3::context &c = node->get_context();
    z3::solver s(c);
    z3::check_result p,n;
    node->AssertStateConstraints(s);
    s.add(node->get_path_constraint());
    s.push();
    s.add(expr);
    Logger(20)<<s.to_smt2()<<End();
    Logger(20)<<"-"<<End();
    p = s.check();
    s.pop();
    s.add(!expr);
    Logger(20)<<s.to_smt2()<<End();
    Logger(20)<<"-"<<End();
    n = s.check();
    switch(p) {
    case z3::sat: LogTrace<<"expr sat"<<End(); break;
    case z3::unsat: LogTrace<<"expr unsat"<<End(); break;
    case z3::unknown: LogTrace<<"expr unknown"<<End(); break;
    }

    switch(n) {
    case z3::sat: LogTrace<<"¬expr sat"<<End(); break;
    case z3::unsat: LogTrace<<"¬expr unsat"<<End(); break;
    case z3::unknown: LogTrace<<"¬expr unknown"<<End(); break;
    }

    switch(p) {
    case z3::sat: //expr could be true
        switch(n) {
        case z3::sat: //expr could be false
        case z3::unknown:
            return Result::UNKNOWN;
        case z3::unsat: //l==r can't be false
            return Result::TRUE;
        }
    case z3::unsat: //expr cannot be true
        switch(n) {
        case z3::sat: //expr could be false
        case z3::unknown:
            return Result::FALSE;
        case z3::unsat: //expr cannot be false
            LogError<<"expr and ¬expr both unsat"<<End(true);
            assert(false);
        }
    }
}

//------------------------------------------------------------------------------
// Utility functions
//------------------------------------------------------------------------------

const std::optional<z3::expr> negateOptExpr(std::optional<z3::expr> expr)
{
    if(expr.has_value()) {
        return !*expr;
    } else {
        return std::nullopt;
    }
}

constexpr Result SymComp::BoolToResult(bool b)
{
     return b ? Result::TRUE : Result::FALSE;
}

std::pair<Result, std::optional<z3::expr>> SymComp::BoolToResultPair(bool b)
{
    return {BoolToResult(b), std::nullopt};
}

SymValue *SymComp::ToSymValue(Result r, Type type)
{
    switch(r) {
    case Result::TRUE: return new BoolSymValue(true, type);
    case Result::UNKNOWN: return new UnknownSymValue(type);
    case Result::FALSE: return new BoolSymValue(false, type);
    default: LogWarning<<"Unhandled result type to sym value"<<End(); return nullptr;
    }
}

Result SymComp::Not(Result r)
{
    switch(r) {
    case Result::TRUE: return Result::FALSE;
    case Result::UNKNOWN: return Result::UNKNOWN;
    case Result::FALSE: return Result::TRUE;
    default: LogWarning<<"Unhandled result type to invert"<<End(); return Result::UNKNOWN;
    }
}