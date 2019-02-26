// This file if part of the genm-opt project.
// Licensing information can be found in the LICENSE file.
// (C) 2018 Nandor Licker. All rights reserved.

#pragma once

#include <set>
#include <unordered_map>
#include <unordered_set>

class Node;
class RootNode;



// -----------------------------------------------------------------------------
class ConstraintSolver final {
public:
  /// Arguments & return values to a function.
  struct FuncSet {
    /// Argument sets.
    std::vector<Node *> Args;
    /// Return set.
    Node *Return;
    /// Frame of the function.
    Node *Frame;
    /// Variable argument glob.
    Node *VA;
    /// True if function was expanded.
    bool Expanded;
  };

public:
  /// Initialises the solver.
  ConstraintSolver();

  /// Returns a load constraint.
  Node *Load(Node *ptr);

  /// Generates a subset constraint.
  void Subset(Node *from, Node *to);

  /// Constructs a root node.
  RootNode *Root();
  /// Constructs a root node, with a single function.
  RootNode *Root(Func *item);
  /// Constructs a root node, with a single extern.
  RootNode *Root(Extern *item);
  /// Constructs a root node, with a single node.
  RootNode *Root(RootNode *item);

  /// Constructs an empty node.
  Node *Empty();

  /// Constructs a root node for an atom.
  RootNode *Chunk(Atom *atom, RootNode *chunk);

public:
  /// Creates a store constraint.
  void Store(Node *ptr, Node *val)
  {
    Subset(val, Load(ptr));
  }

  /// Returns a binary set union.
  Node *Union(Node *a, Node *b)
  {
    if (!a) {
      return b;
    }
    if (!b) {
      return a;
    }

    auto *node = Empty();
    Subset(a, node);
    Subset(b, node);
    return node;
  }

  /// Returns a ternary set union.
  Node *Union(Node *a, Node *b, Node *c)
  {
    return Union(a, Union(b, c));
  }

  /// Indirect call, to be expanded.
  RootNode *Call(
      const std::vector<Inst *> &context,
      Node *callee,
      const std::vector<Node *> &args
  );

  /// Allocation site.
  Node *Alloc(const std::vector<Inst *> &context)
  {
    return Empty();
  }

  /// Extern function context.
  Node *External()
  {
    return extern_;
  }

  /// Returns the node attached to a global.
  Node *Lookup(Global *global);

  /// Returns the constraints attached to a function.
  FuncSet &Lookup(const std::vector<Inst *> &calls, Func *func);

  /// Simplifies the constraints.
  void Progress();

  /// Simplifies the whole batch.
  std::vector<std::pair<std::vector<Inst *>, Func *>> Expand();

private:
  /// Creates a root node with an item.
  RootNode *Root(SetNode *set);
  /// Creates a root from a node.
  RootNode *Anchor(Node *node);
  /// Creates a node.
  template<typename T, typename... Args>
  T *Make(Args... args);

  /// Solves the constraints until a fixpoint is reached.
  void Solve();

  void Collapse(SetNode *node);

  std::set<SetNode *> collapsed_;

private:
  /// Call site information.
  struct CallSite {
    /// Call context.
    std::vector<Inst *> Context;
    /// Called function.
    RootNode *Callee;
    /// Arguments to call.
    std::vector<RootNode *> Args;
    /// Return value.
    RootNode *Return;
    /// Expanded callees at this site.
    std::set<Func *> Expanded;

    CallSite(
        const std::vector<Inst *> &context,
        RootNode *callee,
        std::vector<RootNode *> args,
        RootNode *ret)
      : Context(context)
      , Callee(callee)
      , Args(args)
      , Return(ret)
    {
    }
  };

  /// List of pending nodes.
  std::vector<std::unique_ptr<GraphNode>> pending_;
  /// List of all nodes.
  std::vector<std::unique_ptr<GraphNode>> nodes_;
  /// List of root nodes.
  std::vector<std::unique_ptr<RootNode>> roots_;

  /// Function argument/return constraints.
  std::map<Func *, std::unique_ptr<FuncSet>> funcs_;
  /// Mapping from atoms to their nodes.
  std::unordered_map<Atom *, RootNode *> atoms_;
  /// Global variables.
  std::unordered_map<Global *, RootNode *> globals_;
  /// Node representing external values.
  RootNode *extern_;
  /// Call sites.
  std::vector<CallSite> calls_;
};
