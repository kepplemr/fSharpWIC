// INTERPRETER file - main program file. Includes the macro-level code to get, process, and execute the WIC code. Also includes the main recursive method
//                      that runs the WIC code. 
// Author: Michael Kepple
// Date: October 13th, 2011
module interpreter

open System;
open System.IO;
open System.Collections.Generic;
open System.Diagnostics;
open System.Windows.Forms;
open instructions;
open table;
open preprocess;

// Function: executeInstructions
// Description: Recursive method that iterates through the WIC code, passing each subsequent recursion the overall state of the program.
// Params: State of the program
// Returns: State of the program after each individual instruction is carried out, status when finished
// Modifies: Program state
let rec executeInstructions state = 
  // Base case/Halt instruction
  if (fetchOpcode state.PC state.Instructions = "halt") then
    printfn "\n*** Program execution halted! ***"
    0
  else
  match (fetchOpcode state.PC state.Instructions) with
  | "get" -> executeInstructions (Get state)
  | "put" -> executeInstructions (Put state)
  | "push" -> executeInstructions (Push state)
  | "pop" -> executeInstructions (Pop state)
   // Use same method for the "Math" operators
  | "add" -> executeInstructions (Math state)
  | "sub" -> executeInstructions (Math state)
  | "mul" -> executeInstructions (Math state)
  | "div" -> executeInstructions (Math state)
  // Six test instructions
  | "tsteq" -> executeInstructions (Test state)
  | "tstne" -> executeInstructions (Test state)
  | "tstlt" -> executeInstructions (Test state)
  | "tstle" -> executeInstructions (Test state)
  | "tstgt" -> executeInstructions (Test state)
  | "tstge" -> executeInstructions (Test state)
  // Logic instructions
  | "and" -> executeInstructions (Logic state)
  | "or" -> executeInstructions (Logic state)
  | "not" -> executeInstructions (Logic state)
  // Jump instructions
  | "j" -> executeInstructions (Jump state)
  | "jf" -> executeInstructions (Jump state)
  // Label/Nop/Other handlers - allows for two different label syntax's
  | "nop" -> executeInstructions {state with state.PC = state.PC+1}
  | _ -> if (fetchOperand state.PC state.Instructions <> "label") then
           printfn "*** Code error: Unrecognized command (%s). Exiting Program! ***" (fetchOperand state.PC state.Instructions)
           1
         else 
           executeInstructions {state with PC = state.PC+1}
         
// Lets the compiler know where we want the program to begin execution.
[<STAThread>]
// Absolutely need the following do() for this code to work on many/most machines. Why? I have no idea.
do()

// Function: main
// Description: Main program function that ties together the various parts of overall program functionality. Gets the user-inputted file, sends it off
//                to be processed, created and initializes tables, calls recursive execution function and keeps track of how long it needs to finish.
// Params: None.
// Returns: Nothing.
// Modifies: Lots.
let main =
  // Get the lines of code from the user-inputted file and bind them to this list.
  let codeList: string list = readFile
  // Bind the formatted list of (Opcode, Operand) to instList
  let instList = processInput codeList
  // Create the jump table that we'll use for the rest of program execution.
  let jumpTab = createJT instList 0
  // Initial state of the program. As of now we have a list of instructions and a finalized jump table. PC is at zero, nothing is yet
  //   on the Stack or in the Symbol table.
  let State = {Instructions = instList; PC = 0; Stack = []; ST = []; JT = jumpTab}:state
  // Start out stopwatch here
  let time = Stopwatch.StartNew()
  executeInstructions State
  // Stop timing when executeInstructions is done.
  time.Stop()
  // Print out our F# time result
  printfn "Time Elapsed: %f" time.Elapsed.TotalMilliseconds;