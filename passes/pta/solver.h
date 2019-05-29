// This file if part of the genm-opt project.
// Licensing information can be found in the LICENSE file.
// (C) 2018 Nandor Licker. All rights reserved.

#pragma once

#include <unordered_map>
#include <unordered_set>
#include <vector>

#include "passes/pta/graph.h"
#include "passes/pta/queue.h"
#include "passes/pta/scc.h"

class Atom;
class Inst;
class Node;
class Global;

class RootNode;



/**
 * Class to keep track of constraints & solve them.
 */
class ConstraintSolver final {
public:
  /// Arguments & return values to a function.
  struct FuncSet {
    /// Argument sets.
    std::vector<RootNode *> Args;
    /// Return set.
    RootNode *Return;
    /// Frame of the function.
    RootNode *Frame;
    /// Variable argument glob.
    RootNode *VA;
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

  /// Indirect call, to be expanded.
  RootNode *Call(
      const std::vector<Inst *> &context,
      Node *callee,
      const std::vector<Node *> &args
  );

  /// Allocation site.
  Node *Alloc(const std::vector<Inst *> &context)
  {
    auto *set = Set();
    set->AddNode(Set()->GetID());
    return set;
  }

  /// Extern function context.
  RootNode *External()
  {
    return extern_;
  }

  /// Returns the node attached to a global.
  RootNode *Lookup(Global *global);

  /// Returns the constraints attached to a function.
  FuncSet &Lookup(const std::vector<Inst *> &calls, Func *func);

  /// Simplifies the whole batch.
  std::vector<std::pair<std::vector<Inst *>, Func *>> Expand();

  /// Creates a root node with an item.
  RootNode *Root(SetNode *set);
  /// Creates a root from a node.
  RootNode *Anchor(Node *node);
  /// Creates a deref node.
  DerefNode *Deref(SetNode *set);
  /// Creates a set node.
  SetNode *Set();

  /// Maps a function to an ID.
  ID<Func *> Map(Func *func);
  /// Maps an extern to an ID.
  ID<Extern *> Map(Extern *ext);

  /// Solves the constraints until a fixpoint is reached.
  void Solve();

private:
  /// Nodes and derefs are friends.
  friend class RootNode;

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
    std::unordered_set<Func *> Expanded;

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

  /// Constraint graph.
  Graph graph_;

  /// Mapping of functions to IDs.
  std::unordered_map<Func *, ID<Func *>> funcToID_;
  /// Mapping of IDs to functions.
  std::vector<Func *> idToFunc_;

  /// Mapping of externs to IDs.
  std::unordered_map<Extern *, ID<Extern *>> extToID_;
  /// Mapping of IDs to externs.
  std::vector<Extern *> idToExt_;

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

  /// Cycle detector.
  SCCSolver scc_;
  /// Set of nodes to start the next traversal from.
  Queue<SetNode *> queue_;
};