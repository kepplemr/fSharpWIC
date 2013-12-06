// INSTRUCTIONS file - implements the instructions called by the WIC code.
// Author: Michael Kepple
// Date: Oct 13th, 2011
module instructions

open System;
open table;

// Initialize the symbol table
let symTab = [];;

// Function: Get
// Description: Prompts user to enter a vlaue that will be associated in the symbol table with the String operand.
// Params: State of the program
// Returns: New state of the program with updated PC and Symbol table.
// Modifies: Symbol table.
let Get state =
  printf "Enter %s: " (fetchOperand state.PC state.Instructions)
  let x = int32(System.Console.ReadLine())
  {state with ST = store (fetchOperand state.PC state.Instructions) x state.ST; PC = state.PC+1}

// Function: Put
// Description: Displays the indicated variable and the value associated with it in the Symbol table.
// Params: State of the program
// Returns: New state of the program with incrememted PC value
// Modifies: Nothing
let Put state =
  printfn "%s = %d" (fetchOperand state.PC state.Instructions) (retrieve (fetchOperand state.PC state.Instructions) state.ST)
  {state with PC = state.PC+1}

// Function: Push
// Description: Pushes a value on to the top of the stack. The value can either be an immediate integer value or a Symbol Table variable. In
//                the case it is a ST variable, it's associated value will be retrieved and pushed onto the stack.
// Params: State of the program
// Returns: New state of the program with incremented PC value and modified stack.
// Modifies: The stack.
let Push state =
  match Decimal.TryParse (fetchOperand state.PC state.Instructions) with 
  | (false, _) -> {state with PC = state.PC+1; state.Stack = (retrieve (fetchOperand state.PC state.Instructions) state.ST)::state.Stack}
  | (_, value) -> {state with PC = state.PC+1; state.Stack = (int32(fetchOperand state.PC state.Instructions))::state.Stack}

// Function: Pop
// Description: Pops the top value off of the stack, stores this value in the Symbol table with its key being the Operand of this Pop instruction.
// Params: State of the program
// Returns: New state of the program with incremented PC value and updated ST, Stack.
// Modifies: Symbol table, Stack.
let Pop state =
  {state with ST = store (fetchOperand state.PC state.Instructions) (List.head state.Stack) state.ST; state.Stack = (List.tail state.Stack); PC = state.PC+1}

// Function: Test
// Description: Pops the top value off the stack and compares it to zero. Depending on the specific Opcode, returns 1 if the value on top of the stack satisfied
//                the test.
// Params: State of the program
// Returns: New state of the program with incremented PC value and either a '1' or '0' on top of the stack, depending on whether or not the stack value satisfied
//            the test conditions.
// Modifies: The stack.
let Test state =
  match (fetchOpcode state.PC state.Instructions) with
  | "tsteq" -> let value = if state.Stack.Head = 0 then 1 else 0
               {state with PC = state.PC+1; Stack = value::state.Stack.Tail}
  | "tstne" -> let value = if state.Stack.Head <> 0 then 1 else 0
               {state with PC = state.PC+1; Stack = value::state.Stack.Tail}
  | "tstgt" -> let value = if state.Stack.Head > 0 then 1 else 0
               {state with PC = state.PC+1; Stack = value::state.Stack.Tail}
  | "tstge" -> let value = if state.Stack.Head >= 0 then 1 else 0
               {state with PC = state.PC+1; Stack = value::state.Stack.Tail}
  | "tstlt" -> let value = if state.Stack.Head < 0 then 1 else 0
               {state with PC = state.PC+1; Stack = value::state.Stack.Tail}
  | "tstle" -> let value = if state.Stack.Head <= 0 then 1 else 0
               {state with PC = state.PC+1; Stack = value::state.Stack.Tail}

// Function: Logic
// Description: Implementation of the three Logic functions. Given either a number (not) or two numbers (and, or) on top of the stack, makes logical comparisons 
//                (zero or non-zero) and returns with a '1' or a '0' on top of the stack. 
// Params: State of the program.
// Returns: New state of the program with incremented PC value and eith a '1' or a '0' on top of the stack, depending on the result of the logical comparisons.
// Modifies: The stack.
let Logic state =
  match (fetchOpcode state.PC state.Instructions) with
  | "not" -> let TOS = state.Stack.Head
             let value =
               if TOS <> 0 then
                 0
               else
                 1
             {state with PC = state.PC+1; Stack = value::state.Stack.Tail}
  | "and" -> let TOS = state.Stack.Head
             let TOS2 = state.Stack.Tail.Head
             let value =
               if (TOS <> 0 && TOS2 <> 0) then
                 1
               else
                 0
             {state with PC = state.PC+1; Stack = value::state.Stack.Tail.Tail}
  | "or" -> let TOS = state.Stack.Head
            let TOS2 = state.Stack.Tail.Head
            let value =
              if (TOS <> 0 || TOS2 <> 0) then
                1
              else
                0
            {state with PC = state.PC+1; Stack = value::state.Stack.Tail.Tail}

// Function: Math
// Description: Performs arithmetic operations specified by a particular opcode. 
// Params: State of the program.
// Returns: New state of the program with incremented PC value and the result of the arithmetic operation pushed on the top of the stack.
// Modifies: The stack.
let Math state =
  try
  let x = state.Stack.Head
  let y = state.Stack.Tail.Head
  match (fetchOpcode state.PC state.Instructions) with
  | "add" -> {state with PC = state.PC+1; state.Stack = y+x::state.Stack.Tail.Tail}
  | "sub" -> {state with PC = state.PC+1; state.Stack = y-x::state.Stack.Tail.Tail}
  | "mul" -> {state with PC = state.PC+1; state.Stack = y*x::state.Stack.Tail.Tail}
  | "div" -> try
               {state with PC = state.PC+1; state.Stack = y/x::state.Stack.Tail.Tail}  
             // Throw exception upon attempted Divide-by-zero
             with
               | :? System.DivideByZeroException as ex -> printfn "Exception! %s " (ex.Message); {state with PC = state.PC+1}
  with
  // Throw an exception if there are not enough values on the stack to perform the specified operations.
  | :? Exception -> printfn "Error: Not enough values on the stack to perform specified operation!"
                    {state with PC = state.PC+1}

// Function: Jump
// Description: Potentially modifies PC value based on stack value, function operand (label in the JT)
// Params: State of the program.
// Returns: New state of the program with PC value based on result of function execution.
// Modifies: The Program Counter.
let Jump state =
  match (fetchOpcode state.PC state.Instructions) with
  | "j"-> let operand = fetchOperand state.PC state.Instructions
          let value = retrieve operand state.JT
          {state with PC = value}
  | "jf" -> if (state.Stack.Head = 0) then
              let operand = fetchOperand state.PC state.Instructions
              let value = retrieve operand state.JT
              {state with PC = value}
            else
              {state with PC = state.PC+1}