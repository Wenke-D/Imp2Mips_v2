# Project

## Directory Structure
**test** repository contains test code

## Project progress
- Evaluation by register and stack - done
- Optimization to detect which register cost - not started


## Temporary Register State
The states of registers for temporary values are no longer important in the following situation.

1. Between instructions.
2. Between each argument of a function call.

### Implementation
The call to `tr_expr` means to evaluate the expresion begining from the first register. we call `tr_expr` at `tr_instr`.

For the argument evaluation, we call `aux 0 arg` for each argument, which is equivlent.


## Mixing Registers and the Stack
### Strategy
In `mips.ml`, `expression_registers` defines available registers for temporary values, they are `$t2 ~ $t7`.

There's a function `t: int -> register` to simplify the access, so `(t 0)` will give us `$t2`.

Register `$t0` and `$t1` are preserved to 
1. hold intermiedirea values from stack while evaluate a binary operation.
2.  other occasional values.

No assupetions on these 2 registers.

### Implemation
Within the `tr_expr` function, there are 2 functions, `aux` and `aux_stack`. 

Function `aux_stack` translates expression only by stack, the detail will be explain later.

Function `aux` is responsible for implementation of registers manage and call for `aux_stack` when no register available. The return of `aux` is a tuple: `(code, count)`, `code` contains mips code, `count` is the number of register comsumed for this expression. `aux` calls itself for sub-expressions.


## Binary expression
### Strategy
While evaluate binary expression, the register for the first operand will be used for the final result, so that each a binary operation consumes 2 registers instead of 3.

While using the stack, `$t0` and `$t1` are for intermediare value.

### Implementation
the `Binop` branch of `aux` function


## Register state among function call.

### Strategy
While enconter a function call: 
1. push all used registers (`$t2` ~ `$t7`) to stack.
2. push arguments, make the call.
3. retrive return value from `(t 0)`, save to `$t0`.
4. clear argument.
5. restore used registers.
6. put return value to the register for current expression.

### Implementation
the `Call` branch of `aux` function.


## When registers are full

### Strategy
when no registers availables, all intermediare data will be store to **the stack**.

The function call becomes simpler in this situation, as the value of each argument is already in the stack after evaluation.

### Implementation
Function `aux_stack` is responsible for the implmentation, it will be called by `aux` when there are no register available.





