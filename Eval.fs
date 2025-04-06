module Interpreter.Eval

    open Result
    open Language
    open Interpreter.StateMonad


    let readFromConsole () = System.Console.ReadLine().Trim()
    let tryParseInt (str : string) = System.Int32.TryParse str
    let rec readInt (u: unit) =
        let fromConsole = (readFromConsole ())
        match tryParseInt fromConsole with
        | (true, b) -> b
        | _ -> 
            printfn "%s is not an integer" fromConsole
            readInt ()

    let apply (n1: 'a option) (n2: 'a option) (operation: 'a -> 'a -> 'b)= 
        match n1,n2 with
        |  Some x, Some y -> Some (operation x y)
        | _ -> None

    let split (s1 : string) (s2 : string) = s2 |> s1.Split |> Array.toList

    let rec arithEval (a: aexpr) : int stateMonad = 
        match a with
        | Num n -> ret n  
        | Var v -> getVar v //when st.m.ContainsKey v
        | Add (x,y) ->
                arithEval x >>= (fun intLeft -> arithEval y >>= (fun intRight -> ret (intLeft + intRight))) 
        | Mul (x,y) -> 
                arithEval x >>= (fun intLeft -> arithEval y >>= (fun intRight -> ret (intLeft * intRight))) 
        | Div (x,y) ->
                arithEval y >>= (fun denom -> 
                        match denom with
                        | 0 -> fail
                        | _ ->  arithEval x >>= (fun num -> ret (num / denom))
                        )
        | Mod (x,y) -> fail
        | MemRead (e1) -> 
                (arithEval e1) >>= (fun value -> 
                        getMem value
                        )
        | Random -> random
        | Read -> arithEval (Num (readInt ()))
        | Cond(b, a1, a2) -> 
                boolEval b >>= ( fun bBool ->
                match bBool with
                | true -> arithEval a1
                | false -> arithEval a2
                )           
        | _ -> fail 

    and boolEval (b: bexpr) : bool stateMonad = 
        match b with
        | TT -> ret true
        | Eq (a,c)-> 
                arithEval a >>= (fun intLeft -> arithEval c >>= (fun intRight -> ret (intLeft = intRight)))
        | Lt (a,c) -> 
                arithEval a >>= (fun intLeft -> arithEval c >>= (fun intRight -> ret (intLeft < intRight)))
        | Conj (a,c) -> 
                boolEval a >>= (fun boolLeft -> boolEval c >>= (fun boolRight -> ret (boolLeft && boolRight)))
        | Not a -> 
                boolEval a >>= (fun bBool -> ret (not (bBool)))

    let rec mergeString (e: list<aexpr>) (s: string) : string stateMonad = 
        match e with 
        | [] -> ret ("")
        | xs :: x -> 
                ret ("")
                (*
                match arithEval xs with
                | Some y -> 
                       match split s "%" with 
                       | zs :: z -> mergeString x ((zs :: string(y) :: z ) |> List.fold(fun s acc -> s + acc) "") st
                       | _ -> None
                | None -> None*)
                
    let rec stmntEval s : 'a stateMonad = 
        match s with 
        | Skip -> ret ()
        | Declare s -> declare s
        | Assign(v,a) -> 
                arithEval a >>= (fun value -> setVar v value )
        | Seq (s1,s2) ->
                stmntEval s1 >>>= stmntEval s2
        | If (gaurd, s1, s2) -> 
                boolEval gaurd >>= (
                        fun bool ->
                        match bool with
                        | true -> stmntEval s1
                        | false -> stmntEval s2
                )
        | While (gaurd, s') -> 
                boolEval gaurd >>= ( fun bBool ->
                        match bBool with
                        | true -> stmntEval s' >>>= (stmntEval(While (gaurd,s')))
                        | false -> ret()
                )
        (*
        | While (gaurd, s') -> 
                match boolEval gaurd st with
                | Some true -> 
                        match stmntEval s' st with
                        | Some st'' -> stmntEval (While (gaurd, s')) st''
                        | None -> None
                | Some false -> Some st
                | None -> None
        *)
        | Alloc (x,e) -> 
                arithEval e >>= (fun value -> alloc x value)
        | Free (e1,e2) -> 
                arithEval e1 >>= (fun ptr -> arithEval e2 >>= (fun value -> free ptr value))
        | MemWrite(e1,e2) -> 
                arithEval e1 >>= (fun ptr -> arithEval e2 >>= (fun value -> setMem ptr value))
        | Print(es,s) -> 
                match mergeString es s with 
                | _ -> 
                        ret ()
                | _ -> fail
        | _ -> fail
    