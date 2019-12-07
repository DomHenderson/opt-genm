#pragma once

#include <memory>
#include <type_traits>
#include <unordered_set>
#include <utility>

#include "flownode.h"
#include "frame.h"
#include "symvalue.h"

//This allows persist to be used inline in an initialisation without having 
//to declare the pointer as a base pointer
template<typename Base, typename Derived>
using DerivedPtr = typename std::enable_if<std::is_base_of<Base,Derived>::value,Derived*>::type;

template <typename T>
class StoragePool {
public:
    template <typename P>
    DerivedPtr<T,P> persist(P* value) {
        if(pool.find(value) == pool.end()) {
            pool[value] = std::move(std::unique_ptr<T>(value));
        }
        return value;
    }
private:
    std::unordered_map<T*,std::unique_ptr<T>> pool;
};

class SymExPool {
public:
    template <typename V>
    DerivedPtr<SymValue,V> persist(V *value) {
        return valuePool.persist(value);
    }
    template <typename N>
    DerivedPtr<FlowNode,N> persist(N *node) {
        return nodePool.persist(node);
    }
    template <typename F>
    DerivedPtr<Frame,F> persist(F *frame) {
        return framePool.persist(frame);
    }
private:
    StoragePool<SymValue> valuePool;
    StoragePool<FlowNode> nodePool;
    StoragePool<Frame> framePool;
};
