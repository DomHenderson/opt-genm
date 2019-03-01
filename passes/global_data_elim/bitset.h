// This file if part of the genm-opt project.
// Licensing information can be found in the LICENSE file.
// (C) 2018 Nandor Licker. All rights reserved.

#pragma once

#include <cassert>
#include <cstdint>
#include <map>



/**
 * Sparse bit set implementation.
 */
template<typename T>
class BitSet final {
private:
  /// Node in the sparse bitset.
  struct Node {
    Node() : Fst(0ull), Snd(0ull) {}

    Node(Node &&node) = delete;

    Node(const Node &node) : Fst(node.Fst), Snd(node.Snd) {}

    void operator = (const Node &node) = delete;

    /// First 64 elements.
    uint64_t Fst;
    /// Last 64 elements.
    uint64_t Snd;

    bool operator == (const Node &that) const
    {
      return Fst == that.Fst && Snd == that.Snd;
    }
  };

  /// Iterator over the structure holding the node.
  using NodeIt = typename std::map<uint32_t, Node>::const_iterator;

public:
  /// Integer type identifying items.
  using Item = uint32_t;

  /// Iterator over the bitset items.
  class iterator final {
  public:
    iterator(const iterator &that)
      : set_(that.set_)
      , it_(that.it_)
      , current_(that.current_)
    {
    }

    iterator(const BitSet<T> &set, int64_t current)
      : set_(set)
      , it_(set.nodes_.find(current >> 7))
      , current_(current)
    {
    }

    Item operator * () const
    {
      return current_;
    }

    iterator operator ++ ()
    {
      for (;;) {
        ++current_;
        if (current_ > set_.last_) {
          return *this;
        }

        int64_t pos = current_ & ((1 << 7) - 1);
        if (pos == 0) {
          ++it_;
          current_ = it_->first << 7;
        }

        if (pos < 64) {
          if ((it_->second.Fst & (1ull << pos)) != 0) {
            return *this;
          }
        } else {
          if ((it_->second.Snd & (1ull << (pos - 64ull))) != 0) {
            return *this;
          }
        }
      }

      return *this;
    }

    bool operator != (const iterator &that) const
    {
      return current_ != that.current_;
    }

  private:
    /// Reference to the set.
    const BitSet<T> &set_;
    /// Iterator over the hash map.
    NodeIt it_;
    /// Current item.
    int64_t current_;
  };

  /// Reverse iterator over the bitset items.
  class reverse_iterator final {
  public:
    reverse_iterator(const reverse_iterator &that)
      : set_(that.set_)
      , it_(that.it_)
      , current_(that.current_)
    {
    }

    reverse_iterator(const BitSet<T> &set, int64_t current)
      : set_(set)
      , it_(set.nodes_.find(current >> 7))
      , current_(current)
    {
    }

    Item operator * () const
    {
      return current_;
    }

    reverse_iterator operator ++ ()
    {
      for (;;) {
        --current_;
        if (current_ < set_.first_) {
          return *this;
        }

        int64_t pos = current_ & ((1 << 7) - 1);
        if (pos == 127) {
          --it_;
          current_ = (it_->first << 7) + ((1 << 7) - 1);
        }

        if (pos < 64) {
          if ((it_->second.Fst & (1ull << pos)) != 0) {
            return *this;
          }
        } else {
          if ((it_->second.Snd & (1ull << (pos - 64ull))) != 0) {
            return *this;
          }
        }
      }

      return *this;
    }

    bool operator != (const iterator &that) const
    {
      return current_ != that.current_;
    }

  private:
    /// Reference to the set.
    const BitSet<T> &set_;
    /// Iterator over the hash map.
    NodeIt it_;
    /// Current item.
    int64_t current_;
  };

  /// Constructs a new bitset.
  explicit BitSet()
    : first_(std::numeric_limits<Item>::max())
    , last_(std::numeric_limits<Item>::min())
  {
  }

  /// Deletes the bitset.
  ~BitSet()
  {
  }

  /// Start iterator.
  iterator begin() const
  {
    return Empty() ? end() : iterator(*this, first_);
  }

  /// End iterator.
  iterator end() const
  {
    return iterator(*this, static_cast<int64_t>(last_) + 1ull);
  }

  /// Reverse start iterator.
  reverse_iterator rbegin() const
  {
    return Empty() ? rend() : reverse_iterator(*this, last_);
  }

  /// Reverse end iterator.
  reverse_iterator rend() const
  {
    return reverse_iterator(*this, static_cast<int64_t>(first_) - 1ull);
  }

  /// Checks if the set is empty.
  bool Empty() const
  {
    return first_ > last_;
  }

  /// Clears these set.
  void Clear()
  {
    first_ = std::numeric_limits<Item>::max();
    last_ = std::numeric_limits<Item>::min();
    nodes_.clear();
  }

  /// Inserts an item into the bitset.
  bool Insert(Item item)
  {
    first_ = std::min(first_, item);
    last_ = std::max(last_, item);

    auto &node = nodes_[item >> 7];
    uint64_t idx = item & ((1 << 7) - 1);
    if (idx < 64) {
      const uint64_t mask = (1ull << (idx - 0));
      bool inserted = !(node.Fst & mask);
      node.Fst |= mask;
      return inserted;
    } else {
      const uint64_t mask = (1ull << (idx - 64));
      bool inserted = !(node.Snd & mask);
      node.Snd |= mask;
      return inserted;
    }
  }

  /// Erases a bit.
  void Erase(Item item)
  {
    if (item == first_ && item == last_) {
      first_ = std::numeric_limits<Item>::max();
      last_ = std::numeric_limits<Item>::min();
    } else if (item == first_) {
      first_ = *++begin();
    } else if (item == last_) {
      last_ = *++rbegin();
    }

    auto &node = nodes_[item >> 7];
    uint64_t idx = item & ((1 << 7) - 1);
    if (idx < 64) {
      node.Fst &= ~(1ull << (idx - 0));
    } else {
      node.Snd &= ~(1ull << (idx - 64));
    }

    if (node.Fst == 0 && node.Snd == 0) {
      nodes_.erase(item >> 7);
    }
  }

  /// Efficiently computes the union of two bitsets.
  bool Union(const BitSet &that)
  {
    bool changed = false;

    for (auto &thatNode : that.nodes_) {
      auto &thisNode = nodes_[thatNode.first];
      uint64_t oldFst = thisNode.Fst;
      uint64_t oldSnd = thisNode.Snd;

      thisNode.Fst |= thatNode.second.Fst;
      thisNode.Snd |= thatNode.second.Snd;

      changed |= oldFst != thisNode.Fst;
      changed |= oldSnd != thisNode.Snd;
    }

    first_ = std::min(first_, that.first_);
    last_ = std::max(last_, that.last_);

    return changed;
  }

  /// Checks if two bitsets are equal.
  bool operator == (const BitSet &that) const
  {
    if (first_ != that.first_) {
      return false;
    }
    if (last_ != that.last_) {
      return false;
    }

    return std::equal(
        nodes_.begin(), nodes_.end(),
        that.nodes_.begin(), that.nodes_.end()
    );
  }

  /// Disallow copy and assign.
  BitSet(BitSet &&) = delete;
  BitSet(const BitSet &) = delete;
  void operator = (const BitSet &) = delete;
  void operator = (BitSet &&) = delete;

private:
  /// First element.
  Item first_;
  /// Last element.
  Item last_;
  /// Nodes stored in the bit set.
  std::map<uint32_t, Node> nodes_;
};
