#include <iostream>
#include <vector>

#include "stack.h"

void Stack::push_frame(std::vector<SymValue*> new_args)
{
    std::cout<<"Pushing new frame"<<std::endl;
    unsigned arg_base = args.size();
    std::cout<<"Arg_base "<<arg_base<<std::endl;
    args.insert(args.end(), new_args.begin(), new_args.end());
    std::cout<<"New args size "<<args.size()<<std::endl;
    stack.push(Frame(arg_base));
}

void Stack::pop_frame()
{
    std::cout<<"Popping frame"<<std::endl;
    stack.pop();
}

Frame &Stack::top_frame()
{
    return stack.top();
}

unsigned Stack::get_local_arg(unsigned id)
{
    unsigned arg_base = top_frame().get_arg_base();
    return arg_base + id;
}