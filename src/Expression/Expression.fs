/// Copyright (C) 2016 The Authors.
module Expression

type Terminal =
    Add | Mul | Pwr | Root | Sub | Div | Lpar | Rpar | Int of int | Float of float | Var of string

type Expr =
    FNum of float | FVar of string | FAdd of Expr * Expr | FMult of Expr * Expr | FSub of Expr * Expr | FDiv of Expr * Expr | FExponent of Expr * Expr | FRoot of Expr * Expr

/// <summary>
/// Raised in case of an exception during scanning.
/// </summary>
exception ScanErrorException

/// <summary>
/// Raised in case of an exception during parsing.
/// </summary>
exception ParseErrorException

/// <summary>
/// Check if a character is blank, i.e. whitespace.
/// </summary>
let isblank c = System.Char.IsWhiteSpace c

/// <summary>
/// Check if a character is a letter.
/// </summary>
let isletter c = System.Char.IsLetter c

/// <summary>
/// Check if a character is a digit.
/// </summary>
let isdigit c  = System.Char.IsDigit c

/// <summary>
/// Check if a character is a letter or a digit.
/// </summary>
let isletterdigit c = System.Char.IsLetterOrDigit c

/// <summary>
/// Convert a sequence of elements to a list of elements.
/// </summary>
let explode s = [for e in s -> e]

/// <summary>
/// Convert a character to its floating point version.
/// </summary>
let floatval (c : char) = float (int c - int '0')

/// <summary>
/// Convert a character to its integer version.
/// </summary>
let intval (c : char) = int c - int '0'

/// <summary>
/// Scan a character sequence and construct numerical value.
/// </summary>
/// <param name=cs>The character sequence to scan.</param>
/// <param name=value>The accumulating numerical value.</param>
/// <returns>The remaining characters and the value accumulated so far.</returns>
let rec scnum (cs, value) =
    match cs with
    | '.' :: c :: cr when isdigit c -> scfrac(c :: cr, float value, 0.1)
    | c :: cr when isdigit c -> scnum(cr, 10 * value + intval c)
    | _ -> (cs, Int value)

/// <summary>
/// Scan a character sequence and construct a fractional value.
/// </summary>
/// <param name=cs>The character sequence to scan.</param>
/// <param name=value>The accumulating fractional value.</param>
/// <param name=wt>The place value of the fractional value.</param>
/// <returns>The remaining characters and the value accumulated so far.</returns>
and scfrac (cs, value, wt) =
    match cs with
    | c :: cr when isdigit c -> scfrac(cr, value + wt * floatval c, wt / 10.0)
    | _ -> (cs, Float value)

/// <summary>
/// Scan a character sequence and construct a named value.
/// </summary>
/// <param name=cs>The character sequence to scan.</param>
/// <param name=value>The accumulating named value.</param>
/// <returns>The remaining characters and the value accumulated so far.</returns>
let rec scname (cs, value) =
    match cs with
    | c :: cr when isletterdigit c -> scname(cr, value + c.ToString())
    | _ -> (cs, value)

/// <summary>
/// Scan a character sequence and construct a terminal list.
/// </summary>
/// <param name=s>The character sequence to scan.</param>
/// <returns>The constructed terminal list.</returns>
let scan s =
    let rec sc cs =
        match cs with
        | '+' :: cr -> Add  :: sc cr
        | '*' :: cr -> Mul  :: sc cr
        | '/' :: cr -> Div  :: sc cr
        | '^' :: cr -> Pwr  :: sc cr
        | '_' :: cr -> Root :: sc cr
        | '(' :: cr -> Lpar :: sc cr
        | ')' :: cr -> Rpar :: sc cr
        | '-' :: cr -> Sub  :: sc cr
        | c :: cr when isdigit  c -> let (ct, t) = scnum(cr, intval c) in t :: sc ct
        | c :: cr when isletter c -> let (ct, n) = scname(cr, string c) in Var n :: sc ct
        | c :: cr when isblank  c -> sc cr
        | [] -> []
        | _ -> raise ScanErrorException
    sc (explode s)

/// <summary>
/// Insert implicit negative numbers in a terminal list.
/// </summary>
/// <param name=ts>The terminal list to insert negative numbers into.</param>
/// <returns>The terminal list with explicit negative numbers inserted.</returns>
let insertNegativeNum (ts : Terminal list) =
    let rec inn = function
        | (Add | Sub | Div | Mul | Pwr | Lpar) as t :: Sub :: (Float x) :: tss -> t :: Float (-1.0 * x) :: inn tss
        | (Add | Sub | Div | Mul | Pwr | Lpar) as t :: Sub :: (Int x) :: tss -> t :: Int (-1 * x) :: inn tss
        | t :: ts -> t :: inn ts
        | [] -> []
    match ts with
    | Sub :: Int x :: tss -> inn (Int (-1 * x) :: tss)
    | Sub :: Float x :: tss -> inn (Float (-1.0 * x) :: tss)
    | [] -> []
    | _  -> inn ts

/// <summary>
/// Insert implicit negation of expressions and variables in a terminal list.
/// </summary>
/// <param name=ts>The terminal list to insert negation into.</param>
/// <returns>The terminal list with explicit negation inserted.</returns>
let insertNegation (ts : Terminal list) =
    let rec ine = function
        | (Add | Sub | Div | Mul | Pwr | Lpar) as t :: Sub :: ((Var _) as tr) :: tss -> t :: Float -1.0 :: tr :: ine tss
        | t :: ts -> t :: ine ts
        | [] -> []
    match ts with
    | Sub :: ((Var _ | Lpar) as t) :: tss -> ine (Float -1.0 :: t :: tss)
    | [] -> []
    | _  -> ine ts

/// <summary>
/// Insert explicit multiplications in a terminal list.
/// </summary>
/// <param name=ts>The terminal list to insert multiplications into.</param>
/// <returns>The terminal list with explicit multiplications inserted.</returns>
let rec insertMult = function
    | (Int _ | Float _ | Var _ | Rpar) as t :: ((Int _ | Float _ | Var _ | Lpar) :: _ as ts) -> t :: Mul :: insertMult ts
    | t :: ts -> t :: insertMult ts
    | [] -> []

/// <summary>
/// Parse terminal list to expression.
/// </summary>
/// <remarks>
/// E = T Eopt .
/// </remarks>
/// <param name=ts>The terminal list to parse.</param>
/// <returns>Tuple of the remaining elements in terminal list and the parsed expression.</returns>
let rec E (ts : Terminal list) = (T >> Eopt) ts

/// <remarks>
/// Eopt = "+" T Eopt | e .
/// </remarks>
and Eopt (ts, inval) =
    match ts with
    | Add :: tr -> let (tt, tv) = T tr in Eopt (tt, FAdd (inval, tv))
    | Sub :: tr -> let (tt, tv) = T tr in Eopt (tt, FSub (inval, tv))
    | _ -> (ts, inval)

/// <remarks>
/// T = F Topt .
/// </remarks>
and T ts = (F >> Topt) ts

/// <remarks>
/// Topt = "*" F Topt | e .
/// </remarks>
and Topt (ts, inval) =
    match ts with
    | Mul :: tr -> let (tf, tv) = F tr in Topt (tf, FMult (inval, tv))
    | Div :: tr -> let (tf, tv) = F tr in Topt (tf, FDiv (inval, tv))
    | _ -> (ts, inval)

/// <remarks>
/// F = P Fopt .
/// </remarks>
and F ts = (P >> Fopt) ts

/// <remarks>
/// Fopt = "^" Int | e .
/// </remarks>
and Fopt (ts, inval) =
    match ts with
    | Pwr  :: tr -> let (tf, tv) = F tr in Fopt (tf, FExponent (inval, tv))
    | Root :: tr -> let (tf, tv) = F tr in Fopt (tf, FRoot (inval, tv))
    | _ -> (ts, inval)

/// <remarks>
/// P = Int | Float | Var | "(" E ")" .
/// </remarks>
and P ts =
    match ts with
    | Int i   :: tr -> (tr, FNum(float i))
    | Float r :: tr -> (tr, FNum(r))
    | Var x   :: tr -> (tr, FVar(x))
    | Lpar    :: tr -> let (te, tv) = E tr
                       match te with
                       | Rpar :: tr -> (tr, tv)
                       | _          -> raise ParseErrorException
    | _ -> raise ParseErrorException

/// <summary>
/// Parse a terminal list and construct an abstract syntax tree.
/// </summary>
/// <param name=ts>The terminal list to parse.</param>
/// <returns>The constructed abstract syntax tree.</returns>
let parseTerminal ts =
    match E ts with
    | ([], result) -> result
    | _ -> raise ParseErrorException

/// <summary>
/// Parse a character sequence and construct an abstract syntax tree.
/// </summary>
/// <param name=s>The character sequence to parse.</param>
/// <returns>The constructed abstract syntax tree.</returns>
let parse s = (scan >> insertNegativeNum >> insertNegation >> insertMult >> parseTerminal) s
