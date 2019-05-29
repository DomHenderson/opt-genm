// This file if part of the genm-opt project.
// Licensing information can be found in the LICENSE file.
// (C) 2018 Nandor Licker. All rights reserved.

#include <cstdlib>

#include <memory>
#include <unordered_map>
#include <unordered_set>

#include <llvm/ADT/PostOrderIterator.h>
#include <llvm/ADT/SmallPtrSet.h>

#include "core/block.h"
#include "core/cast.h"
#include "core/cfg.h"
#include "core/constant.h"
#include "core/data.h"
#include "core/func.h"
#include "core/insts.h"
#include "core/prog.h"
#include "passes/pta.h"

#include "pta/node.h"
#include "pta/solver.h"



/**
 * Global context, building and solving constraints.
 */
class GlobalContext final {
public:
  /// Initialises the context, scanning globals.
  GlobalContext(Prog *prog);

  /// Explores the call graph starting from a function.
  void Explore(Func *func)
  {
    queue_.emplace_back(std::vector<Inst *>{}, func);
    while (!queue_.empty()) {
      while (!queue_.empty()) {
        auto [calls, func] = queue_.back();
        queue_.pop_back();
        BuildConstraints(calls, func);
      }
      for (auto &func : solver.Expand()) {
        queue_.push_back(func);
      }
    }
  }

  /// Checks if a function can be invoked.
  bool Reachable(Func *func) const
  {
    return explored_.count(func) != 0;
  }

private:
  /// Context for a function - mapping instructions to constraints.
  class LocalContext {
  public:
    /// Adds a new mapping.
    void Map(Inst &inst, Node *c)
    {
      if (c) {
        values_[&inst] = c;
      }
    }

    /// Finds a constraint for an instruction.
    Node *Lookup(Inst *inst)
    {
      return values_[inst];
    }

  private:
    /// Mapping from instructions to constraints.
    std::unordered_map<Inst *, Node *> values_;
  };

  /// Builds constraints for a single function.
  void BuildConstraints(const std::vector<Inst *> &calls, Func *func);
  /// Builds a constraint for a single global.
  Node *BuildGlobal(Global *g);
  // Builds a constraint from a value.
  Node *BuildValue(LocalContext &ctx, Value *v);
  // Creates a constraint for a call.
  template<typename T>
  Node *BuildCall(
      const std::vector<Inst *> &calls,
      LocalContext &ctx,
      Inst *caller,
      Inst *callee,
      llvm::iterator_range<typename CallSite<T>::arg_iterator> &&args
  );
  // Creates a constraint for a potential allocation site.
  template<typename T>
  Node *BuildAlloc(
      LocalContext &ctx,
      const std::vector<Inst *> &calls,
      const std::string_view &name,
      llvm::iterator_range<typename CallSite<T>::arg_iterator> &args
  );

  /// Extracts an integer from a potential mov instruction.
  std::optional<int> ToInteger(Inst *inst);
  /// Extracts a global from a potential mov instruction.
  Global *ToGlobal(Inst *inst);

private:
  /// Set of explored constraints.
  ConstraintSolver solver;
  /// Work queue for functions to explore.
  std::vector<std::pair<std::vector<Inst *>, Func *>> queue_;
  /// Set of explored functions.
  std::unordered_set<Func *> explored_;
};


// -----------------------------------------------------------------------------
GlobalContext::GlobalContext(Prog *prog)
{
  std::vector<std::tuple<Atom *, Atom *>> fixups;
  std::unordered_map<Atom *, RootNode *> chunks;

  RootNode *chunk = nullptr;
  for (auto &data : prog->data()) {
    for (auto &atom : data) {
      chunk = chunk ? chunk : solver.Root();
      solver.Chunk(&atom, chunk);
      chunks.emplace(&atom, chunk);

      for (auto *item : atom) {
        switch (item->GetKind()) {
          case Item::Kind::INT8:    break;
          case Item::Kind::INT16:   break;
          case Item::Kind::INT32:   break;
          case Item::Kind::INT64:   break;
          case Item::Kind::FLOAT64: break;
          case Item::Kind::SPACE:   break;
          case Item::Kind::STRING:  break;
          case Item::Kind::ALIGN:   break;
          case Item::Kind::SYMBOL: {
            auto *global = item->GetSymbol();
            switch (global->GetKind()) {
              case Global::Kind::SYMBOL: {
                assert(!"not implemented");
                break;
              }
              case Global::Kind::EXTERN: {
                auto *ext = static_cast<Extern *>(global);
                solver.Store(solver.Lookup(&atom), solver.Lookup(ext));
                break;
              }
              case Global::Kind::FUNC: {
                auto *func = static_cast<Func *>(global);
                solver.Store(solver.Lookup(&atom), solver.Lookup(func));
                break;
              }
              case Global::Kind::BLOCK: {
                assert(!"not implemented");
                break;
              }
              case Global::Kind::ATOM: {
                fixups.emplace_back(static_cast<Atom *>(global), &atom);
                break;
              }
            }
            break;
          }
          case Item::Kind::END: {
            chunk = nullptr;
            break;
          }
        }
      }
    }
  }

  for (auto &fixup : fixups) {
    auto [item, atom] = fixup;
    solver.Store(solver.Lookup(atom), solver.Lookup(item));
  }
}

// -----------------------------------------------------------------------------
void GlobalContext::BuildConstraints(
    const std::vector<Inst *> &calls,
    Func *func)
{
  // Constraint sets for the function.
  auto &funcSet = solver.Lookup(calls, func);
  if (funcSet.Expanded) {
    return;
  }
  funcSet.Expanded = true;

  // Mark the function as explored.
  explored_.insert(func);

  // Context storing local instruction - constraint mappings.
  LocalContext ctx;

  // For each instruction, generate a constraint.
  for (auto *block : llvm::ReversePostOrderTraversal<Func*>(func)) {
    for (auto &inst : *block) {
      switch (inst.GetKind()) {
        // Call - explore.
        case Inst::Kind::CALL: {
          auto &call = static_cast<CallInst &>(inst);
          auto *callee = call.GetCallee();
          if (auto *c = BuildCall<ControlInst>(calls, ctx, &inst, callee, call.args())) {
            ctx.Map(call, c);
          }
          break;
        }
        // Invoke Call - explore.
        case Inst::Kind::INVOKE: {
          auto &call = static_cast<InvokeInst &>(inst);
          auto *callee = call.GetCallee();
          if (auto *c = BuildCall<TerminatorInst>(calls, ctx, &inst, callee, call.args())) {
            ctx.Map(call, c);
          }
          break;
        }
        // Tail Call - explore.
        case Inst::Kind::TCALL:
        case Inst::Kind::TINVOKE: {
          auto &call = static_cast<CallSite<TerminatorInst>&>(inst);
          auto *callee = call.GetCallee();
          if (auto *c = BuildCall<TerminatorInst>(calls, ctx, &inst, callee, call.args())) {
            solver.Subset(c, funcSet.Return);
          }
          break;
        }
        // Return - generate return constraint.
        case Inst::Kind::RET: {
          auto &retInst = static_cast<ReturnInst &>(inst);
          if (auto *val = retInst.GetValue()) {
            if (auto *c = ctx.Lookup(val)) {
              solver.Subset(c, funcSet.Return);
            }
          }
          break;
        }
        // Indirect jump - funky.
        case Inst::Kind::JI: {
          // Nothing to do here - transfers control to an already visited
          // function, without any data dependencies.
          break;
        }
        // Load - generate read constraint.
        case Inst::Kind::LD: {
          auto &loadInst = static_cast<LoadInst &>(inst);
          ctx.Map(loadInst, solver.Load(ctx.Lookup(loadInst.GetAddr())));
          break;
        }
        // Store - generate write constraint.
        case Inst::Kind::ST: {
          auto &storeInst = static_cast<StoreInst &>(inst);
          auto *storeVal = storeInst.GetVal();
          if (auto *value = ctx.Lookup(storeVal)) {
            solver.Store(ctx.Lookup(storeInst.GetAddr()), value);
          }
          break;
        }
        // Exchange - generate read and write constraint.
        case Inst::Kind::XCHG: {
          auto &xchgInst = static_cast<ExchangeInst &>(inst);
          auto *addr = ctx.Lookup(xchgInst.GetAddr());
          if (auto *value = ctx.Lookup(xchgInst.GetVal())) {
            solver.Store(addr, value);
          }
          ctx.Map(xchgInst, solver.Load(addr));
          break;
        }
        // Register set - extra funky.
        case Inst::Kind::SET: {
          // Nothing to do here - restores the stack, however it does not
          // introduce any new data dependencies.
          break;
        }
        // Returns the current function's vararg state.
        case Inst::Kind::VASTART: {
          auto &vaStartInst = static_cast<VAStartInst &>(inst);
          if (auto *value = ctx.Lookup(vaStartInst.GetVAList())) {
            solver.Subset(funcSet.VA, value);
          }
          break;
        }
        // Pointers to the stack frame.
        case Inst::Kind::FRAME:
        case Inst::Kind::ALLOCA: {
          ctx.Map(inst, funcSet.Frame);
          break;
        }

        // Unary instructions - propagate pointers.
        case Inst::Kind::ABS:
        case Inst::Kind::NEG:
        case Inst::Kind::SQRT:
        case Inst::Kind::SIN:
        case Inst::Kind::COS:
        case Inst::Kind::SEXT:
        case Inst::Kind::ZEXT:
        case Inst::Kind::FEXT:
        case Inst::Kind::TRUNC: {
          auto &unaryInst = static_cast<UnaryInst &>(inst);
          if (auto *arg = ctx.Lookup(unaryInst.GetArg())) {
            ctx.Map(unaryInst, arg);
          }
          break;
        }

        // Binary instructions - union of pointers.
        case Inst::Kind::ADD:
        case Inst::Kind::SUB:
        case Inst::Kind::AND:
        case Inst::Kind::OR:
        case Inst::Kind::ROTL:
        case Inst::Kind::SLL:
        case Inst::Kind::SRA:
        case Inst::Kind::SRL:
        case Inst::Kind::XOR:
        case Inst::Kind::CMP:
        case Inst::Kind::DIV:
        case Inst::Kind::REM:
        case Inst::Kind::MUL:
        case Inst::Kind::POW:
        case Inst::Kind::COPYSIGN:
        case Inst::Kind::UADDO:
        case Inst::Kind::UMULO: {
          auto &binaryInst = static_cast<BinaryInst &>(inst);
          auto *lhs = ctx.Lookup(binaryInst.GetLHS());
          auto *rhs = ctx.Lookup(binaryInst.GetRHS());
          if (auto *c = solver.Union(lhs, rhs)) {
            ctx.Map(binaryInst, c);
          }
          break;
        }

        // Select - union of return values.
        case Inst::Kind::SELECT: {
          auto &selectInst = static_cast<SelectInst &>(inst);
          auto *vt = ctx.Lookup(selectInst.GetTrue());
          auto *vf = ctx.Lookup(selectInst.GetFalse());
          if (auto *c = solver.Union(vt, vf)) {
            ctx.Map(selectInst, c);
          }
          break;
        }

        // PHI - create an empty set.
        case Inst::Kind::PHI: {
          ctx.Map(inst, solver.Empty());
          break;
        }

        // Mov - introduce symbols.
        case Inst::Kind::MOV: {
          if (auto *c = BuildValue(ctx, static_cast<MovInst &>(inst).GetArg())) {
            ctx.Map(inst, c);
          }
          break;
        }

        // Arg - tie to arg constraint.
        case Inst::Kind::ARG: {
          auto &argInst = static_cast<ArgInst &>(inst);
          unsigned idx = argInst.GetIdx();
          if (idx < funcSet.Args.size()) {
            ctx.Map(argInst, funcSet.Args[idx]);
          } else {
            throw std::runtime_error(
                "Argument " + std::to_string(idx) + " out of range in " +
                std::string(func->GetName())
            );
          }
          break;
        }

        // Undefined - +-inf.
        case Inst::Kind::UNDEF: {
          break;
        }

        // Control flow - ignored.
        case Inst::Kind::JCC:
        case Inst::Kind::JMP:
        case Inst::Kind::SWITCH:
        case Inst::Kind::TRAP: {
          break;
        }
      }
    }
  }

  for (auto &block : *func) {
    for (auto &phi : block.phis()) {
      std::vector<Node *> ins;
      for (unsigned i = 0; i < phi.GetNumIncoming(); ++i) {
        if (auto *c = BuildValue(ctx, phi.GetValue(i))) {
          if (std::find(ins.begin(), ins.end(), c) == ins.end()) {
            ins.push_back(c);
          }
        }
      }

      auto *pc = ctx.Lookup(&phi);
      for (auto *c : ins) {
        solver.Subset(c, pc);
      }
    }
  }
}

// -----------------------------------------------------------------------------
Node *GlobalContext::BuildValue(LocalContext &ctx, Value *v)
{
  switch (v->GetKind()) {
    case Value::Kind::INST: {
      // Instruction - propagate.
      return ctx.Lookup(static_cast<Inst *>(v));
    }
    case Value::Kind::GLOBAL: {
      // Global - set with global.
      return solver.Lookup(static_cast<Global *>(v));
    }
    case Value::Kind::EXPR: {
      // Expression - set with offset.
      switch (static_cast<Expr *>(v)->GetKind()) {
        case Expr::Kind::SYMBOL_OFFSET: {
          auto *symExpr = static_cast<SymbolOffsetExpr *>(v);
          return solver.Lookup(symExpr->GetSymbol());
        }
      }
    }
    case Value::Kind::CONST: {
      // Constant value - no constraint.
      return nullptr;
    }
  }
};


// -----------------------------------------------------------------------------
template<typename T>
Node *GlobalContext::BuildCall(
    const std::vector<Inst *> &calls,
    LocalContext &ctx,
    Inst *caller,
    Inst *callee,
    llvm::iterator_range<typename CallSite<T>::arg_iterator> &&args)
{
  std::vector<Inst *> callString(calls);
  callString.push_back(caller);

  if (auto *global = ToGlobal(callee)) {
    if (auto *calleeFunc = ::dyn_cast_or_null<Func>(global)) {
      // If the function is an allocation site, stop and
      // record it. Otherwise, recursively traverse callees.
      if (auto *c = BuildAlloc<T>(ctx, calls, calleeFunc->GetName(), args)) {
        explored_.insert(calleeFunc);
        return c;
      } else {
        auto &funcSet = solver.Lookup(callString, calleeFunc);
        unsigned i = 0;
        for (auto *arg : args) {
          if (auto *c = ctx.Lookup(arg)) {
            if (i >= funcSet.Args.size()) {
              if (calleeFunc->IsVarArg()) {
                solver.Subset(c, funcSet.VA);
              }
            } else {
              solver.Subset(c, funcSet.Args[i]);
            }
          }
          ++i;
        }
        queue_.emplace_back(callString, calleeFunc);
        return funcSet.Return;
      }
    }
    if (auto *ext = ::dyn_cast_or_null<Extern>(global)) {
      if (auto *c = BuildAlloc<T>(ctx, calls, ext->GetName(), args)) {
        return c;
      } else {
        auto *externs = solver.External();
        for (auto *arg : args) {
          if (auto *c = ctx.Lookup(arg)) {
            solver.Subset(c, externs);
          }
        }
        return externs;
      }
    }
    throw std::runtime_error("Attempting to call invalid global");
  } else {
    std::vector<Node *> argConstraint;
    for (auto *arg : args) {
      argConstraint.push_back(ctx.Lookup(arg));
    }
    return solver.Call(callString, ctx.Lookup(callee), argConstraint);
  }
};

// -----------------------------------------------------------------------------
template<typename T>
Node *GlobalContext::BuildAlloc(
    LocalContext &ctx,
    const std::vector<Inst *> &calls,
    const std::string_view &name,
    llvm::iterator_range<typename CallSite<T>::arg_iterator> &args)
{
  static const char *allocs[] = {
    "caml_alloc1",
    "caml_alloc2",
    "caml_alloc3",
    "caml_allocN",
    "caml_alloc_young",
    "caml_fl_allocate",
    "caml_stat_alloc_noexc",
    "malloc",
  };
  static const char *reallocs[] = {
    "realloc",
    "caml_stat_resize_noexc"
  };

  for (size_t i = 0; i < sizeof(allocs) / sizeof(allocs[0]); ++i) {
    if (allocs[i] == name) {
      return solver.Alloc(calls);
    }
  }

  for (size_t i = 0; i <sizeof(reallocs) / sizeof(reallocs[0]); ++i) {
    if (allocs[i] == name) {
      return ctx.Lookup(*args.begin());
    }
  }

  return nullptr;
};

// -----------------------------------------------------------------------------
std::optional<int> GlobalContext::ToInteger(Inst *inst)
{
  if (auto *movInst = ::dyn_cast_or_null<MovInst>(inst)) {
    if (auto *intConst = ::dyn_cast_or_null<ConstantInt>(movInst->GetArg())) {
      return intConst->GetValue();
    }
  }
  return std::nullopt;
}

// -----------------------------------------------------------------------------
Global *GlobalContext::ToGlobal(Inst *inst)
{
  if (auto *movInst = ::dyn_cast_or_null<MovInst>(inst)) {
    if (auto *global = ::dyn_cast_or_null<Global>(movInst->GetArg())) {
      return global;
    }
  }
  return nullptr;
}


// -----------------------------------------------------------------------------
void PointsToAnalysis::Run(Prog *prog)
{
  GlobalContext graph(prog);

  for (auto &func : *prog) {
    if (func.GetVisibility() == Visibility::EXTERN) {
      graph.Explore(&func);
    }
  }

  for (auto &func : *prog) {
    if (graph.Reachable(&func)) {
      reachable_.insert(&func);
    }
  }
}

// -----------------------------------------------------------------------------
const char *PointsToAnalysis::GetPassName() const
{
  return "Points-To Analysis";
}

// -----------------------------------------------------------------------------
char AnalysisID<PointsToAnalysis>::ID;