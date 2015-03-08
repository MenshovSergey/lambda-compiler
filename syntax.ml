module Variables = Map.Make(String)

type typeVariable = 
    | Bool
    | Integral
    | Auto

type foundType = 
    | Math
    | Logic
    | Assign 
    | None
    | Error of string
    
let rec pretty_print = function
    | Math -> "Math"
    | Logic -> "Logic"
    | Assign -> "Assign"
    | None -> "None"
    | Error (v) -> v

let getType tree = 
    let rec checkType left right expectType foundType typeArgument = 
        if (foundType == expectType) 
                then begin 
                    getType left typeArgument;
                    getType right typeArgument;
                end
            else if (foundType == None) 
                    then begin
                        getType left typeArgument;
                        getType right typeArgument;
                    end
                else 
                    Error ("Expected " ^ pretty_print expectType ^  "type but found " ^ (pretty_print foundType));
    and getType curNode foundType = 
        match curNode with 
        | Ast.Or (left, right) -> (checkType left right Logic foundType Logic)            
        | Ast.And (left, right) -> (checkType left right Logic foundType Logic)   
        | Ast.Not (right) -> (checkType right right Logic foundType Logic)

        | Ast.Div (left, right) -> (checkType left right Math foundType Math)
        | Ast.Mod (left, right) -> (checkType left right Math foundType Math)
        | Ast.Plus (left, right) -> (checkType left right Math foundType Math)
        | Ast.Minus (left, right) -> (checkType left right Math foundType Math)
        | Ast.Pow (left, right) -> (checkType left right Math foundType Math)
        | Ast.Number(x) -> if (foundType == Math || foundType == None) then Math 
                            else Error ("Expected " ^ pretty_print foundType ^ " type but found Math") 
        
        | _ -> None
    in getType tree None