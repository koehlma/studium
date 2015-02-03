(*=START= *)
(*==End== *)

(*=START= lexer Typen
datatype tokenTypes = BOOL | INT | ARROW | LPAR | RPAR
fun lexTypes nil = nil
  | lexTypes (#" "::cr) = lexTypes cr
  | lexTypes (#"\t"::cr) = lexTypes cr
  | lexTypes (#"\n"::cr) = lexTypes cr
  | lexTypes (#"b":: #"o":: #"o":: #"l"::cr) = BOOL :: lexTypes cr
  | lexTypes (#"i":: #"n":: #"t"::cr) = INT :: lexTypes cr
  | lexTypes (#"-":: #">":: cr) = ARROW :: lexTypes cr
  | lexTypes (#"("::cr) = LPAR :: lexTypes cr
  | lexTypes (#")"::cr) = RPAR :: lexTypes cr
  | lexTypes _ = raise Error "lexTypes"
==End== lexer Typen*)

(*=START= Datentypen für konkrete Syntax*)
datatype token = TRUE | FALSE | IC of int | ID of string
               | ADD | SUB | MUL | LEQ 
               | ARROW | LPAR | RPAR 
               | IF | THEN | ELSE 
               | BOOL | INT
               | FN | OF | ABS
(*==End== Datentypen für konkrete Syntax*)

(*=START= Datentypen für statische/dynamische Semantik*)
datatype con = False | True | Ic of int
type id = string
datatype opr = Add | Sub | Mul | Leq
datatype ty =
    Bool
  | Int
  | Arrow of ty * ty
datatype exp =
    Con of con
  | Id of id
  | Opr of opr * exp * exp
  | If of exp * exp * exp
  | Abs of id * ty * exp
  | App of exp * exp
type 'a env = id -> 'a
datatype elab = T of ty | SE of string
datatype value =
    IV of int
  | Proc of id * exp * value env
(*==END== Datentypen für statische/dynamische Semantik*)

(*=START= Converter*)
fun convertvalue (IV x) = Int.toString x
  | convertvalue (Proc(i,x,v)) = i ^ " (function)"
fun convertty (Bool) = "bool"
  | convertty (Int) = "int"
  | convertty (Arrow(x,y)) = (convertty x)^"->"^(convertty y)
(*==End== Converter*)

(*=START= Exceptions*)
exception Error of string
exception Unbound of id
(*==End== Exceptions*)

(*=START= Lexer*)
fun lex nil = nil
  | lex (#" "::cr) = lex cr
  | lex (#"\t"::cr) = lex cr
  | lex (#"\n"::cr) = lex cr
  | lex (#"("::cr) = LPAR :: lex cr
  | lex (#")"::cr) = RPAR :: lex cr
  | lex (#"-":: #">":: cr) = ARROW :: lex cr
  | lex (#"=":: #">":: cr) = ABS :: lex cr
  | lex (#"+"::cr) = ADD :: lex cr
  | lex (#"-"::cr) = SUB :: lex cr
  | lex (#"*"::cr) = MUL :: lex cr
  | lex (#"<":: #"=":: cr) = LEQ :: lex cr
  | lex (#"t":: #"r":: #"u":: #"e":: cr) = TRUE :: lex cr
  | lex (#"b":: #"o":: #"o":: #"l":: cr) = BOOL :: lex cr
  | lex (#"i":: #"n":: #"t":: cr) = INT :: lex cr
  | lex (#"f":: #"a":: #"l":: #"s":: #"e" ::cr) = FALSE :: lex cr
  | lex (#"i":: #"f":: cr) = IF :: lex cr
  | lex (#"t":: #"h":: #"e":: #"n":: cr) = THEN :: lex cr
  | lex (#"e":: #"l":: #"s":: #"e" ::cr) = ELSE :: lex cr
  | lex (#"f":: #"n":: cr) = FN :: lex cr
  | lex (#":":: cr) = OF :: lex cr
  | lex (#"~":: c:: cr) = if Char.isDigit c 
                          then lexInt ~1 0 (c::cr)
                          else raise Error "~"
  | lex (c::cr) = if Char.isDigit c then lexInt 1 0 (c::cr)
                  else if Char.isAlpha c then lexId [c] cr
                  else raise Error "lex"
and lexInt s v cs = if null cs orelse not(Char.isDigit (hd cs))
                    then IC(s*v) :: lex cs
                    else lexInt s (10*v+(ord(hd cs)-ord#"0")) (tl cs)
and lexId cs cs' = if null cs' orelse not(Char.isAlpha (hd cs'))
                   then ID(implode(rev cs)) :: lex cs'
                   else lexId (hd cs' ::cs) (tl cs')
(*==End== Lexer*)

(*=START= Hilfsfunktionen*)
fun match (a,ts) t = if null ts orelse hd ts <> t
                     then raise Error "match"
                     else (a, tl ts)
fun extend (a,ts) p f = let val (a',tr) = p ts in (f(a,a'),tr) end
fun extendop (a,ts) p f = let val (a',tr) = p ts in (Opr (f,a,a'),tr) end
(*==End== Hilfsfunktionen*)

(*=START= Parser*)
fun exp (IF::xr) =  ifexp xr
  | exp (FN::xr) =  fnexp xr
  | exp (x::xr) = let
                      val (x1,xr1) = aexp (x::xr)
                  in
                      case xr1 of
                          (LEQ::xr) => extendop (x1,xr) aexp Leq
                        | _ => (x1,xr1)
                  end
  | exp _ = raise Error "parseExp"
and aexp (x::xr) = let
                       val (x1,xr1) = mexp (x::xr)
                   in
                       case xr1 of
                          (ADD::xr) => extendop (x1,xr) aexp Add
                        | (SUB::xr) => extendop (x1,xr) aexp Sub
                        | _ => (x1,xr1)
                   end
  | aexp _ = raise Error "parseAexp"
and mexp (x::xr) = let
                       val (x1,xr1) = sexp (x::xr)
                   in
                       case xr1 of
                          (MUL::xr) => extendop (x1,xr) mexp Mul
                        | _ => (x1,xr1)
                   end
  | mexp _ = raise Error "parseMexp"
and sexp (x::xr) = let
                       val (x1,xr1) = pexp (x::xr)
                   in
                       case xr1 of
                          (FALSE::xr) => extend (x1,xr1) sexp App (*Ist das richtig?*)
                        | (TRUE::xr) => extend (x1,xr1) sexp App (*Ist das richtig?*)
                        | ((ID s)::xr) => extend (x1,xr1) sexp App (*Ist das richtig?*)
                        | ((IC s)::xr) => extend (x1,xr1) sexp App (*Ist das richtig?*)
                        | (LPAR::xr) => extend (x1,xr1) sexp App (*Ist das richtig?*)
                        | _ => (x1,xr1)
                   end
  | sexp _ = raise Error "parseSexp"
and pexp (FALSE::xr) = ((Con False),xr)
  | pexp (TRUE::xr) = ((Con True),xr)
  | pexp ((IC x)::xr) = ((Con (Ic x)),xr)
  | pexp ((ID x)::xr) = ((Id x),xr)
  | pexp (LPAR::xr) = match (exp xr) RPAR
  | pexp _ = raise Error "parsePexp"
and ifexp xr = let
                   val (x1, xr1) = match (exp xr) THEN
                   val (x2, xr2) = match (exp xr1) ELSE
                   val (x3, xr3) = exp xr2
               in
                   (If(x1,x2,x3),xr3)
               end
and id ((ID str)::xr) = (str,xr)
  | id _ = raise Error "parseId"
and pty (INT::xr) = (Int,xr)
  | pty (BOOL::xr) = (Bool,xr)
  | pty (LPAR::xr) = match (ty xr) RPAR
  | pty _ = raise Error "parsePty"
and ty (x::xr) = let
                       val (x1,xr1) = pty (x::xr)
                   in
                       case xr1 of
                          (ARROW::xr) => extend (x1,xr) ty Arrow
                        | _ => (x1,xr1)
                   end
  | ty _ = raise Error "parseTy"
and fnexp xr = let
                   val (x1, xr1) = match (id xr) OF
                   val (x2, xr2) = match (ty xr1) ABS
                   val (x3, xr3) = exp xr2
               in
                   (Abs(x1,x2,x3),xr3)
               end
(*==End== Parser*)

(*=START= Elaborierer*)
fun empty x = raise Unbound x
fun update env x a y = if y=x then a else env y
fun elabCon True = Bool
  | elabCon False = Bool
  | elabCon (Ic _) = Int
fun elabOpr Add Int Int = Int
  | elabOpr Sub Int Int = Int
  | elabOpr Mul Int Int = Int
  | elabOpr Leq Int Int = Bool
  | elabOpr  _   _   _  = raise Error "T Opr"
fun elab f (Con c) = elabCon c
  | elab f (Id x) = f x
  | elab f (Opr(opr,e1,e2)) = elabOpr opr (elab f e1) (elab f e2)
  | elab f (If(e1,e2,e3)) =
                (case (elab f e1, elab f e2, elab f e3) of
                    (Bool, t2, t3) => if t2=t3 then t2
                                      else raise Error "T If1"
                  | _ => raise Error "T If2")
  | elab f (Abs(x,t,e)) = Arrow(t, elab (update f x t) e)
  | elab f (App(e1,e2)) = (case elab f e1 of
                                Arrow(t,t') => if t = elab f e2 then t'
                                               else raise Error "T App1"
                              | _ => raise Error "T App2")
fun elab' f e = T(elab f e) handle
    Unbound s => SE("Unbound "^s)
  | Error s => SE("Error "^s)
(*==End== Elaborierer*)

(*=START= Evaluierer*)
fun evalCon False = IV 0
  | evalCon True = IV 1
  | evalCon (Ic x) = IV x
fun evalOpr Add (IV x1) (IV x2) = IV(x1+x2)
  | evalOpr Sub (IV x1) (IV x2) = IV(x1-x2)
  | evalOpr Mul (IV x1) (IV x2) = IV(x1*x2)
  | evalOpr Leq (IV x1) (IV x2) = IV(if x1<=x2 then 1 else 0)
  | evalOpr  _     _       _    = raise Error "R Opr"
fun eval f (Con c) = evalCon c
  | eval f (Id x) = f x
  | eval f (Opr(opr,e1,e2)) = evalOpr opr (eval f e1) (eval f e2)
  | eval f (If(e1,e2,e3)) = (case eval f e1 of
                                IV 1 => eval f e2
                              | IV 0 => eval f e3
                              |   _  => raise Error "R If")
  | eval f (Abs(x,t,e)) = Proc(x,e,f)
  | eval f (App(e1,e2)) = (case (eval f e1, eval f e2) of
                                (Proc(x,e,f'),v) => eval (update f' x v) e
                              |  _ => raise Error "R App")
(*==End== Evaluierer*)

fun exec code = let
    val (e,ex) = exp (lex (explode code))
in 
    if null ex then
        let
            val types = elab empty e
            val result = eval empty e
        in
            "val it = "^(convertvalue result)^" : "^(convertty types)
        end
    else
        raise Error "not completely parsed"
end

val code = "(fn f : int => f + 5) 4"
;
exec code

;
