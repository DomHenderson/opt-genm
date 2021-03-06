// This file if part of the genm-opt project.
// Licensing information can be found in the LICENSE file.
// (C) 2018 Nandor Licker. All rights reserved.

#pragma once

#include <string>
#include <string_view>
#include <vector>

#include <llvm/ADT/ilist.h>
#include <llvm/ADT/ilist_node.h>
#include <llvm/ADT/iterator_range.h>
#include <llvm/IR/CFG.h>
#include <llvm/Support/raw_ostream.h>

#include "core/inst.h"

class Func;
class PhiInst;


/**
 * Traits to handle parent links from instructions.
 */
template <> struct llvm::ilist_traits<Inst> {
private:
  using instr_iterator = simple_ilist<Inst>::iterator;

public:
  void addNodeToList(Inst *inst);

  void removeNodeFromList(Inst *inst);

  void transferNodesFromList(
      ilist_traits &from,
      instr_iterator first,
      instr_iterator last
  );

  void deleteNode(Inst *inst);

  Block *getParent();
};


/**
 * Basic block.
 */
class Block : public llvm::ilist_node_with_parent<Block, Func>, public Global {
public:
  /// Kind of the global.
  static constexpr Global::Kind kGlobalKind = Global::Kind::BLOCK;

public:
  // Type of the instruction list.
  using InstListType = llvm::ilist<Inst>;

  // Iterator types over instructions.
  using iterator = InstListType::iterator;
  using reverse_iterator = InstListType::reverse_iterator;
  using const_iterator = InstListType::const_iterator;
  using const_reverse_iterator = InstListType::const_reverse_iterator;

  // Iterator wrapper.
  template<typename T>
  using iter_fwd = std::iterator<std::forward_iterator_tag, T, ptrdiff_t, T *, T *>;

  /// Iterator over the predecessors of a block.
  template <class BlockT, class UseIterator>
  class PredIterator : public iter_fwd<BlockT> {
  private:
    using Self = PredIterator<BlockT, UseIterator>;
    UseIterator use_;

  public:
    using pointer = typename iter_fwd<BlockT>::pointer;
    using reference = typename iter_fwd<BlockT>::reference;

    PredIterator() = default;

    inline PredIterator(BlockT *bb)
      : use_(bb->user_begin())
    {
      skipToTerminator();
    }

    inline PredIterator(BlockT *bb, bool)
      : use_(bb->user_end())
    {
    }

    inline bool operator==(const PredIterator& x) const { return use_ == x.use_; }
    inline bool operator!=(const PredIterator& x) const { return !operator==(x); }

    inline reference operator*() const
    {
      return static_cast<const TerminatorInst *>(*use_)->getParent();
    }

    inline pointer *operator->() const
    {
      return &operator*();
    }

    inline PredIterator& operator++()
    {
      ++use_;
      skipToTerminator();
      return *this;
    }

    inline PredIterator operator++(int) {
      PredIterator tmp = *this;
      ++*this;
      return tmp;
    }

  private:
    inline void skipToTerminator()
    {
      while (!use_.atEnd()) {
        if (!*use_ || !(*use_)->Is(Value::Kind::INST)) {
          ++use_;
          continue;
        }

        if (!static_cast<const Inst *>(*use_)->IsTerminator()) {
          ++use_;
          continue;
        }

        break;
      }
    }
  };

  // Iterator over connected basic blocks.
  using succ_iterator = llvm::SuccIterator<TerminatorInst, Block>;
  using const_succ_iterator = llvm::SuccIterator<const TerminatorInst, const Block>;
  using pred_iterator = PredIterator<Block, Value::user_iterator>;
  using const_pred_iterator = PredIterator<const Block, Value::const_user_iterator>;

  // Forward iterator wrapper.
  template<typename It, typename T>
  using facade_fwd = llvm::iterator_facade_base<It, std::forward_iterator_tag, T>;

  /// Iterator over PHI nodes.
  template<typename PhiT, typename IterT>
  class PhiIterator : public facade_fwd<PhiIterator<PhiT, IterT>, PhiT> {
  public:
    /// Convert from non-const to const.
    template <typename PhiU, typename IterU>
    PhiIterator(const PhiIterator<PhiU, IterU> &rhs)
      : phi_(rhs.phi_)
    {
    }

    /// Check for end of iteration.
    bool operator == (const PhiIterator &rhs) const { return phi_ == rhs.phi_; }

    /// Return PHI node.
    PhiT &operator * () const { return *phi_; }

    /// Pre-increment.
    PhiIterator &operator ++ ()
    {
      auto it = std::next(IterT(phi_));
      if (it->Is(Inst::Kind::PHI)) {
        phi_ = static_cast<PhiT *>(&*it);
      } else {
        phi_ = nullptr;
      }
      return *this;
    }

    /// Post-increment.
    PhiIterator &operator ++ (int)
    {
      PhiIterator it = *this;
      *this++;
      return it;
    }

  private:
    /// Block can create an iterator.
    PhiIterator(PhiT *phi_) : phi_(phi_) {}

  private:
    friend Block;
    PhiT *phi_;
  };

  using phi_iterator = PhiIterator<PhiInst, iterator>;
  using const_phi_iterator = PhiIterator<const PhiInst, const_iterator>;

public:
  /**
   * Creates a new basic block.
   *
   * @param parent Parent function.
   * @param name   Name of the basic block.
   */
  Block(const std::string_view name);

  /**
   * Erases a basic block.
   */
  ~Block();

  /// Removes a block from the parent.
  void eraseFromParent();

  /// Adds an instruction to the basic block.
  void AddInst(Inst *inst, Inst *before = nullptr);

  /// Adds a PHI instruction to the basic block.
  void AddPhi(PhiInst *phi);

  /// Returns the name of the basic block.
  std::string_view GetName() const { return name_; }

  /// Returns the name of the basic block for LLVM.
  llvm::StringRef getName() const { return name_; }

  /// Returns a pointer to the parent block.
  Func *getParent() const { return parent_; }

  /// Checks if the block is empty.
  bool IsEmpty() const { return insts_.empty(); }

  /// Returns the terminator of the block.
  TerminatorInst *GetTerminator();
  const TerminatorInst *GetTerminator() const;

  /// Removes an instruction.
  void remove(iterator it);
  /// Erases an instruction.
  void erase(iterator it);
  /// Erases a range of instructions.
  void erase(iterator first, iterator last);

  // Iterator over the instructions.
  iterator begin() { return insts_.begin(); }
  iterator end() { return insts_.end(); }
  const_iterator begin() const { return insts_.begin(); }
  const_iterator end() const { return insts_.end(); }
  reverse_iterator rbegin() { return insts_.rbegin(); }
  reverse_iterator rend() { return insts_.rend(); }
  const_reverse_iterator rbegin() const { return insts_.rbegin(); }
  const_reverse_iterator rend() const { return insts_.rend(); }

  /// Returns the size of the block.
  size_t size() const { return insts_.size(); }

  // Iterator over the successors.
  succ_iterator succ_begin();
  succ_iterator succ_end();
  inline llvm::iterator_range<succ_iterator> successors()
  {
    return llvm::make_range(succ_begin(), succ_end());
  }

  const_succ_iterator succ_begin() const;
  const_succ_iterator succ_end() const;
  inline llvm::iterator_range<const_succ_iterator> successors() const
  {
    return llvm::make_range(succ_begin(), succ_end());
  }

  inline unsigned succ_size() const
  {
    return std::distance(succ_begin(), succ_end());
  }

  inline bool succ_empty() const { return succ_begin() == succ_end(); }

  // Iterator over the predecessors.
  pred_iterator pred_begin();
  pred_iterator pred_end();
  inline llvm::iterator_range<pred_iterator> predessors()
  {
    return llvm::make_range(pred_begin(), pred_end());
  }

  const_pred_iterator pred_begin() const;
  const_pred_iterator pred_end() const;
  inline llvm::iterator_range<const_pred_iterator> predecessors() const
  {
    return llvm::make_range(pred_begin(), pred_end());
  }

  inline unsigned pred_size() const
  {
    return std::distance(pred_begin(), pred_end());
  }

  // Iterator over PHI nodes.
  llvm::iterator_range<const_phi_iterator> phis() const {
    return const_cast<Block *>(this)->phis();
  }
  llvm::iterator_range<phi_iterator> phis();

  /// Split the block at the given iterator.
  Block *splitBlock(iterator I);

  // LLVM: Debug printing.
  void printAsOperand(llvm::raw_ostream &O, bool PrintType = true) const;

private:
  friend struct llvm::ilist_traits<Block>;
  /// Updates the parent node.
  void setParent(Func *parent) { parent_ = parent; }

private:
  friend struct llvm::ilist_traits<Inst>;
  /// Parent function.
  Func *parent_;
  /// List of instructions.
  InstListType insts_;
  /// Name of the block.
  std::string name_;
};
