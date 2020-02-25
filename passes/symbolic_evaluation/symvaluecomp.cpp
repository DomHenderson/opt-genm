#include <iostream>
#include <optional>

#include "core/type.h"
#include "flownode.h"
#include "symvalue.h"
#include "symvaluecomp.h"
#include "utilities.h"

using SymComp::Result;

std::optional<z3::expr> CreateExpr(SymValue *v, z3::context &c);
Result EvaluateSat(z3::expr, FlowNode *node);
std::pair<Result, std::optional<z3::expr>> InvokeSmt(SymValue *lv, SymValue *rv, std::function<z3::expr(z3::expr, z3::expr)> expr, FlowNode *node);

const std::optional<z3::expr> negateOptExpr(std::optional<z3::expr> expr)
{
    if(expr.has_value()) {
        return !*expr;
    } else {
        return std::nullopt;
    }
}

Result SymComp::BoolToResult(bool b)
{
     return b ? Result::TRUE : Result::FALSE;
}

std::pair<Result, std::optional<z3::expr>> SymComp::BoolToResultPair(bool b, z3::context &c)
{
    //std::cout<<"Creating result pair"<<std::endl;
    //std::cout<<"Bool to result"<<std::endl;
    Result r = BoolToResult(b);
    //std::cout<<"BV"<<std::endl;
    c.bv_const("x1", 8);
    //std::cout<<"True"<<std::endl;
    c.bool_val(true);
    //std::cout<<"False"<<std::endl;
    c.bool_val(false);
    //std::cout<<"Constraint"<<std::endl;
    std::optional<z3::expr> expr = c.bool_val(b);
    //std::cout<<"Pair"<<std::endl;
    std::pair<Result, std::optional<z3::expr>> pair = std::make_pair(r,expr);
    //std::cout<<"Return"<<std::endl;
    return pair;
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

// -----------------------------------------------------------------------------
Result SymComp::Not(Result r)
{
    switch(r) {
    case Result::TRUE: return Result::FALSE;
    case Result::UNKNOWN: return Result::UNKNOWN;
    case Result::FALSE: return Result::TRUE;
    default: LogWarning<<"Unhandled result type to invert"<<End(); return Result::UNKNOWN;
    }
}

std::pair<Result, std::optional<z3::expr>> SymComp::EQ(SymValue *lv, SymValue *rv, FlowNode *node, std::optional<z3::expr> assumptions)
{
    if(lv->get_kind() == SymValue::Kind::CONDITIONAL) {
        std::cout<<"Recursing on left conditional"<<std::endl;
        auto condValue = static_cast<CondSymValue*>(lv);
        auto condition = condValue->get_condition();
        auto [lResult, lExpr] = EQ(condValue->if_true(), rv, node, assumptions ? *assumptions && condition : condition);
        auto [rResult, rExpr] = EQ(condValue->if_false(), rv, node, assumptions ? *assumptions && !condition : !condition);
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

    if(rv->get_kind() == SymValue::Kind::CONDITIONAL) {
        std::cout<<"Recursing on right conditional"<<std::endl;
        auto condValue = static_cast<CondSymValue*>(rv);
        auto condition = condValue->get_condition();
        auto [lResult, lExpr] = EQ(lv, condValue->if_true(), node, assumptions ? *assumptions && condition : condition);
        auto [rResult, rExpr] = EQ(lv, condValue->if_false(), node, assumptions ? *assumptions && !condition : !condition);
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
    std::cout<<"Base "<<toString(lv)<<" "<<toString(rv)<<std::endl;

    auto &c = node->get_context();
    //std::cout<<"Obtained context"<<std::endl;
    if ( lv == rv ) {
        //std::cout<<"Reference equality"<<std::endl;
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
            return BoolToResultPair(l->get_name() == r->get_name() && l->get_offset() == r->get_offset(), c);
        }

        case SymValue::Kind::BLOCKREF: {
            auto l = static_cast<BlockRefSymValue*>(lv);
            auto r = static_cast<BlockRefSymValue*>(rv);
            return BoolToResultPair(l->get_name() == r->get_name(), c);
        }

        case SymValue::Kind::BOOL: {
            auto l = static_cast<BoolSymValue*>(lv);
            auto r = static_cast<BoolSymValue*>(rv);
            return BoolToResultPair(l->get_value() == r->get_value(), c);
        }

        case SymValue::Kind::FLOAT: {
            auto l = static_cast<FloatSymValue*>(lv);
            auto r = static_cast<FloatSymValue*>(rv);
            bool result = l->get_value().compare(r->get_value()) == llvm::APFloatBase::cmpEqual;
            return BoolToResultPair(l->get_value().compare(r->get_value()) == llvm::APFloatBase::cmpEqual, c);
        }

        case SymValue::Kind::FUNCREF: {
            auto l = static_cast<FuncRefSymValue*>(lv);
            auto r = static_cast<FuncRefSymValue*>(rv);
            bool result = l->get_name() == r->get_name();
            return BoolToResultPair(l->get_name() == r->get_name(), c);
        }

        case SymValue::Kind::INT: {
            auto l = static_cast<IntSymValue*>(lv);
            auto r = static_cast<IntSymValue*>(rv);
            bool result = l->get_value() == r->get_value();
            //TODO fix Z3 segfault
            //return BoolToResultPair(l->get_value() == r->get_value(), c);
            return std::make_pair(BoolToResult(result), std::nullopt);
        }
        }
    }

    return InvokeSmt(lv, rv, [](auto l, auto r){return l == r;}, node);
}

std::pair<Result, std::optional<z3::expr>> SymComp::NEQ(SymValue *lv, SymValue *rv, FlowNode *node)
{
    auto [eqResult, eqConstraint] = SymComp::EQ(lv, rv, node);
    return {SymComp::Not(eqResult), negateOptExpr(eqConstraint)};
}

Result SymComp::LT(SymValue* lv, SymValue *rv)
{
    if ( lv == rv ) {
        return Result::FALSE;
    } else if (lv->get_kind() == rv->get_kind()) {
        switch(lv->get_kind()) {
        case SymValue::Kind::FLOAT: {
            auto l = static_cast<FloatSymValue*>(lv);
            auto r = static_cast<FloatSymValue*>(rv);
            return BoolToResult(l->get_value().compare(r->get_value()) == llvm::APFloatBase::cmpLessThan);
        }

        case SymValue::Kind::INT: {
            auto l = static_cast<IntSymValue*>(lv);
            auto r = static_cast<IntSymValue*>(rv);
            return BoolToResult(l->get_value().slt(r->get_value()));
        }

        default:
            Logger(20)<<"Default case used in LT"<<End();
        case SymValue::Kind::STATICPTR:
        case SymValue::Kind::BOOL:
        case SymValue::Kind::FUNCREF:
        case SymValue::Kind::UNKNOWN:
            return Result::UNKNOWN;
        }
    } else {
        return Result::UNKNOWN;
    }
}

std::pair<Result, std::optional<z3::expr>> SymComp::GT(SymValue *lv, SymValue *rv, FlowNode *node)
{
    auto &c = node->get_context();
    if ( lv == rv ) {
        return {
            Result::FALSE,
            c.bool_val(false)
        };
    } else if (lv->get_kind() == rv->get_kind()) {
        switch(lv->get_kind()) {
        case SymValue::Kind::FLOAT: {
            auto l = static_cast<FloatSymValue*>(lv);
            auto r = static_cast<FloatSymValue*>(rv);
            bool result = l->get_value().compare(r->get_value()) == llvm::APFloatBase::cmpGreaterThan;
            return {
                BoolToResult(result),
                c.bool_val(result)
            };
        }

        case SymValue::Kind::INT: {
            auto l = static_cast<IntSymValue*>(lv);
            auto r = static_cast<IntSymValue*>(rv);
            bool result = l->get_value().sgt(r->get_value());
            return {
                BoolToResult(result),
                c.bool_val(result)
            };
        }
        }
    }

    return InvokeSmt(lv, rv, [](auto l, auto r){return l>r;}, node);
}

std::pair<Result, std::optional<z3::expr>> SymComp::LE(SymValue *lv, SymValue *rv, FlowNode *node)
{
    auto [gtResult, gtConstraint] = SymComp::GT(lv, rv, node);
    return {SymComp::Not(gtResult), negateOptExpr(gtConstraint)};
}

Result SymComp::GE(SymValue *lv, SymValue *rv)
{
    return SymComp::Not(SymComp::LT(lv, rv));
}

std::optional<z3::expr> CreateExpr(SymValue *v, z3::context &c)
{
    //std::cout<<"Creating expr"<<std::endl;
    if(v == nullptr) {
        LogWarning<<"Attempted to create expr of nullptr"<<End();
        return std::nullopt;
    }
    switch(v->get_kind()) {
    case SymValue::Kind::CONDITIONAL: {
        std::cout<<"Creating conditional expr"<<std::endl;
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