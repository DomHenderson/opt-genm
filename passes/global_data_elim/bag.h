// This file if part of the genm-opt project.
// Licensing information can be found in the LICENSE file.
// (C) 2018 Nandor Licker. All rights reserved.

#pragma once

#include <functional>
#include <optional>
#include <unordered_map>
#include <unordered_set>
#include <utility>

class Node;
class Func;
class Extern;

#include <llvm/Support/raw_ostream.h>


/**
 * Bag of possible nodes.
 */
class Bag {
public:
  /// Represents an item to load from in a bag.
  class Item {
  private:
    enum class Kind {
      FUNC,
      EXT,
      NODE
    };

  public:
    Item(Node *node)
      : kind_(Kind::NODE)
      , nodeVal_(node)
    {
    }

    Item(Node *node, unsigned off)
      : kind_(Kind::NODE)
      , nodeVal_(node)
      , off_(off)
    {
    }

    Item(Extern *ext)
      : kind_(Kind::EXT)
      , extVal_(ext)
    {
    }

    Item(Func *func)
      : kind_(Kind::FUNC)
      , funcVal_(func)
    {
    }

    Func *GetFunc() const
    {
      return kind_ == Kind::FUNC ? funcVal_ : nullptr;
    }

    Extern *GetExtern() const
    {
      return kind_ == Kind::EXT ? extVal_ : nullptr;
    }

    using NodeTy = std::pair<Node *, std::optional<unsigned>>;
    std::optional<NodeTy> GetNode() const
    {
      if (kind_ == Kind::NODE) {
        return std::optional<NodeTy>{ NodeTy{ nodeVal_, off_ } };
      } else {
        return std::nullopt;
      }
    }

    /// Dereferences the item.
    void Load(std::function<void(const Item&)> &&f) const;

    /// Offsets an item.
    std::optional<Item> Offset(const std::optional<int64_t> &off) const;

    /// Updates the item.
    bool Store(const Item &item) const;

  private:
    /// Bag should read everything.
    friend class Bag;

    /// Kind of the item.
    Kind kind_;

    /// Pointer to the item.
    union {
      Extern *extVal_;
      Func *funcVal_;
      Node *nodeVal_;
    };

    /// Offset into the item.
    std::optional<unsigned> off_;
  };

  /// Constructs an empty bag.
  Bag()
  {
  }

  /// Singleton node pointer.
  Bag(Node *node)
  {
    nodes_.emplace(node);
  }

  /// Singleton specific offset.
  Bag(Node *node, unsigned off)
  {
    auto set = std::make_unique<std::unordered_set<unsigned>>();
    set->insert(off);
    offs_.emplace(node, std::move(set));
  }

  /// Singleton external pointer.
  Bag(Extern *ext)
  {
    exts_.emplace(ext);
  }

  /// Singleton function pointer.
  Bag(Func *func)
  {
    funcs_.emplace(func);
  }

  /// Stores an item into the bag.
  bool Store(const Item &item);

  /// Checks if the bag is empty.
  bool IsEmpty() const
  {
    return funcs_.empty() && exts_.empty() && nodes_.empty() && offs_.empty();
  }

  /// Checks if the bag contains an item.
  bool Contains(const Item &item) const
  {
    switch (item.kind_) {
      case Item::Kind::FUNC: {
        return funcs_.count(item.funcVal_) != 0;
      }
      case Item::Kind::EXT: {
        return exts_.count(item.extVal_) != 0;
      }
      case Item::Kind::NODE: {
        if (nodes_.count(item.nodeVal_) != 0) {
          return true;
        }
        if (item.off_) {
          auto it = offs_.find(item.nodeVal_);
          if (it == offs_.end()) {
            return false;
          }
          return it->second->count(*item.off_) != 0;
        } else {
          return false;
        }
      }
    }
  }

  /// Iterates over nodes.
  void ForEach(const std::function<void(const Item &)> &&f)
  {
    ForEach(f);
  }

  void ForEach(const std::function<void(const Item &)> &f)
  {
    for (auto *func : funcs_) {
      f(Item(func));
    }
    for (auto *ext : exts_) {
      f(Item(ext));
    }
    for (auto *node : nodes_) {
      f(Item(node));
    }
    for (auto &off : offs_) {
      for (auto idx : *off.second) {
        f(Item(off.first, idx));
      }
    }
  }

private:
  /// Stored items.
  std::unordered_set<Func *> funcs_;
  /// Stored exts.
  std::unordered_set<Extern *> exts_;
  /// Stored nodes - inf pointers.
  std::unordered_set<Node *> nodes_;
  /// Nodes with offsets.
  std::unordered_map<Node *, std::unique_ptr<std::unordered_set<unsigned>>> offs_;
};