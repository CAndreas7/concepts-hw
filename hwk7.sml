datatype exp = 
    Int of int
  | Plus of exp * exp
  | Less of exp * exp
  | If of exp * exp * exp
  | Var of string
  | Let of string * exp * exp
  | App of exp * exp
  | Fn of string * exp;

datatype value = 
    CVal of int
  | BVal of bool
  | FnVal of string * exp * (string * value) list
  | Error of string;

(* 
 * 1. implement 
 *      toString: exp -> string 
 *)

fun toString e = 
  case e of
    Int i => Int.toString i
  | Plus (e1, e2) => "(" ^ toString e1 ^ " + " ^ toString e2 ^ ")"
  | Less (e1, e2) => "(" ^ toString e1 ^ " - " ^ toString e2 ^ ")"
  | If (e1, e2, e3) => "(if " ^ toString e1 ^ " then " ^ toString e2 ^ " else " ^ toString e3 ^ ")"
  | Var x => x
  | Let (x, e1, e2) => "(let val " ^ x ^ "=" ^ toString e1 ^ " in " ^ toString e2 ^ " end)"
  | App (e1, e2) => "(" ^ toString e1 ^ " " ^ toString e2 ^ ")"
  | Fn (x, e) => "(fn " ^ x ^ " => " ^ toString e ^ ")"

(* 2. implement 
 *       toStringValue: value -> string 
 *)

fun toStringValue v = 
  case v of
    CVal i => Int.toString i
  | BVal b => if b then "true" else "false"
  | FnVal (x, e, env) => "(fn " ^ x ^ " => " ^ toString e ^ ")"
  | Error s => "Error: " ^ s
   
(* 
 * you may want to implement the helper function 
 *       lookup: (string * value) list -> string -> value 
 *)

fun lookup env x = 
  case env of
    [] => Error ("unbound variable: " ^ x)
  | (y, v)::rest => if x = y then v else lookup rest x

(*
 * 3. implement the function 
 *       eval: exp -> (string * value) list -> value
 *
 * you may find 'case expression' useful in your implementation.
 *)

fun eval e env = 
  case e of
    Int i => CVal i
  | Plus (e1, e2) => 
    let val v1 = eval e1 env
        val v2 = eval e2 env
    in
      case (v1, v2) of
        (CVal i1, CVal i2) => CVal (i1 + i2)
      | (CVal _, _) => Error "Plus error: right operand is not a number"
      | (_, CVal _) => Error "Plus error: left operand is not a number"
      | _ => Error "Plus error: both operands are not numbers"
    end
  | Less (e1, e2) => 
    let val v1 = eval e1 env
        val v2 = eval e2 env
    in
      case (v1, v2) of
        (CVal i1, CVal i2) => BVal (i1 < i2)
      | (CVal _, _) => Error "Less error: right operand is not a number"
      | (_, CVal _) => Error "Less error: left operand is not a number"
      | _ => Error "Less error: both operands are not numbers"
    end
  | If (e1, e2, e3) => 
    let val v1 = eval e1 env
    in
      case v1 of
        BVal true => eval e2 env
      | BVal false => eval e3 env
      | _ => Error "If-then-else error: condition is not a Boolean"
    end
  | Var x => lookup env x
  | Let (x, e1, e2) => 
    let val v1 = eval e1 env
    in eval e2 ((x,v1)::env)
    end
  | App (e1, e2) =>
    let val v1 = eval e1 env
        val v2 = eval e2 env
    in 
      case v1 of 
        FnVal(x,e3,closure) => eval e3 ((x,v2)::closure)
      | _ => Error "Application error: left operand is not a function"
    end
  | Fn (x,e) => FnVal(x,e,env)

(* Test Code *)

(*
*val _ = Control.Print.printDepth := 100;
*val _ = Control.Print.stringDepth := 200;
*)

fun p (n, v) = print (n ^ " = " ^ toStringValue (eval v []) ^ "\n")

val t1 = Let("y", Int 10, 
                  Let("f", Fn("x", Plus(Var "x", Var "y")), 
                           Let("y", Int 20,
                                    App(Var "f", Int 5))));
val t2 = Let("y", Int 10, 
                  Let("f", Fn("x", Plus(Var "x", Var "z")), 
                           Let("y", Int 20,
                                    App(Var "f", Int 5))));
val t3 = Less(Int 10, Less(Int 1, Int 2));
val t4 = Plus(Int 10, Plus (Int 20, Fn("x", Int 3)));
val t5 = App(Int 10, Int 20);
val t6 = Let("f", Int 10, App(Var "f", Int 20));
val t7 = Let("x", Plus(Int 10, Fn("x", Var "x")), Plus(Int 0, Int 20));
val t8 = If(Plus (Int 1, Int 2), Int 4, Int 5);

val _ = p ("t1", t1);
val _ = p ("t2", t2);
val _ = p ("t3", t3);
val _ = p ("t4", t4);
val _ = p ("t5", t5);
val _ = p ("t6", t6);
val _ = p ("t7", t7);
val _ = p ("t8", t8);
