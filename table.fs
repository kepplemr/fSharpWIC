// TABLE file - defines the different types of tables used in program executions, implements methods to add functionality to these
//                different type declarations.
// Author: Michael Kepple
// Date: Oct 13th, 2011
module table

//Table type definition
type Table = 
    | List of string * int
    | Empty

//Instruction list type definition
type Instruction = 
    | List of string * string
    | Empty

// Definition of the state type, tracks status of the executing program.
type state = 
    {
    Instructions: (string * string) list;
    PC: int;
    Stack: int list;
    ST: (string * int) list;
    JT: (string * int) list;
    }

// Function: retrieve
// Description: Retrieves value associated with inputted identifier
// Params: The identifier to look up and the particular table to look for it in.
// Returns: The integer value associated with the passed in identifier if found.
// Modifies: Nothing.
let retrieve identifier table = 
    let exists = List.filter (fun x -> fst x = identifier) table 
    match exists with                                               
    | [] -> failwith "The identifier was not found!"
    | _ -> snd exists.Head;;

// Function: store
// Description: Stores a passed in (identifier, value) tuple in the specified passed in table.
// Params: The identifier and value to store, the table to store it in.
// Returns: New table
// Modified: Passed in table.
let rec store identifier value table =
    match table with
    | [] -> (identifier, value)::table                                      
    | hd::tl -> if ((fst hd) = identifier) then store identifier value tl   
                else hd::(store identifier value tl);;                     

// Function: fetchOpcode
// Description: Given a list of instructions and an index into that list, returns the Opcode at the specified index.
// Params: List of instructions and index into that list.
// Returns: Opcode at specified position.
// Modified: Nothing.
let fetchOpcode location (instructions:List<string*string>) =            
    if(location > instructions.Length) then failwith "Invalid Location!"         
    else fst (List.nth instructions location);;

// Function: fetchOperand
// Description: Given a list of instructions and an index into that list, returns the Operand at the specified index.
// Params: List of instructions and index into that list.
// Returns: Operand at specified position.
// Modified: Nothing.
let fetchOperand location (instructions:List<string*string>) = 
    if(location > instructions.Length) then failwith "Invalid Location!"     
    else snd (List.nth instructions location);;                                



