/// Copyright (C) 2016 The Authors.
module Expression.Test

open Xunit;
open FsUnit.Xunit;

[<Fact>]
let ``simple explicit parsing`` () =
    Expression.parse "2*3"              |> should equal (FMult (FNum 2.0,FNum 3.0))
    Expression.parse "4+3"              |> should equal (FAdd (FNum 4.0,FNum 3.0))
    Expression.parse "2.0 * 5"          |> should equal (FMult (FNum 2.0,FNum 5.0))

[<Fact>]
let ``usage of negative numbers`` () =
    Expression.parse "-2"               |> should equal (FNum -2.0)
    Expression.parse "-2*3"             |> should equal (FMult (FNum -2.0,FNum 3.0))
    Expression.parse "-2*-3.0"          |> should equal (FMult (FNum -2.0,FNum -3.0))
    Expression.parse "4*-2"             |> should equal (FMult (FNum 4.0,FNum -2.0))
    // '- 2' -> '- (2)' -> -1 * (2) -> -2
    Expression.parse "- 2"              |> should equal (FNum -2.0)

[<Fact>]
let ``usage of subtraction`` () =
    Expression.parse "5-2"              |> should equal (FSub (FNum 5.0,FNum 2.0))
    Expression.parse "5--2"             |> should equal (FSub (FNum 5.0,FNum -2.0))

[<Fact>]
let ``usage of division`` () =
    Expression.parse "4/2"              |> should equal (FDiv (FNum 4.0,FNum 2.0))
    Expression.parse "-5/-6"            |> should equal (FDiv (FNum -5.0,FNum -6.0))
    Expression.parse "8/(6.0 + 12)"     |> should equal (FDiv (FNum 8.0, FAdd (FNum 6.0, FNum 12.0)))

[<Fact>]
let ``usage of both addition and multiplication`` () =
    Expression.parse "2+3*4"            |> should equal (FAdd (FNum 2.0,FMult (FNum 3.0,FNum 4.0)))

[<Fact>]
let ``division and multiplication should be calculated before addition and subtraction`` () =
    Expression.parse "2+3*4+5.0"        |> should equal (FAdd (FAdd (FNum 2.0,FMult (FNum 3.0,FNum 4.0)),FNum 5.0))
    Expression.parse "5*2+3/4+5.0"      |> should equal (FAdd (FAdd (FMult (FNum 5.0,FNum 2.0),FDiv (FNum 3.0,FNum 4.0)),FNum 5.0))

[<Fact>]
let ``exponent should be calculated before multiplication, division, addition and substraction`` () =
    Expression.parse "5+9^2+2"          |> should equal (FAdd (FAdd (FNum 5.0,FExponent (FNum 9.0,2)),FNum 2.0))
    Expression.parse "5*9^2*2"          |> should equal (FMult (FMult (FNum 5.0,FExponent (FNum 9.0,2)),FNum 2.0))
    Expression.parse "5-9^2-2"          |> should equal (FSub (FSub (FNum 5.0,FExponent (FNum 9.0,2)),FNum 2.0))
    Expression.parse "5/9^2/2"          |> should equal (FDiv (FDiv (FNum 5.0,FExponent (FNum 9.0,2)),FNum 2.0))

let ``expressions in parentheses should be evaluated before any other expression`` () =
    Expression.parse "9^2(2/2)"         |> should equal ((FExponent (FNum 9.0,2),FDiv (FNum 2.0,FNum 2.0)))
    Expression.parse "5-9*(2/2)"        |> should equal (FSub (FNum 5.0,FMult (FNum 9.0,FDiv (FNum 2.0,FNum 2.0))))

[<Fact>]
let ``usage of negative numbers in combination with addtion and multiplication`` () =
    Expression.parse "2+-3*-4"          |> should equal (FAdd (FNum 2.0,FMult (FNum -3.0,FNum -4.0)))
    Expression.parse "2+-3^-4"          |> should equal (FAdd (FNum 2.0,FExponent (FNum -3.0,-4)))

[<Fact>]
let ``usage of floats`` () =
    Expression.parse "(2.0+3)*4"        |> should equal (FMult (FAdd (FNum 2.0,FNum 3.0),FNum 4.0))
    Expression.parse "(2.0+3.0)*4.0"    |> should equal (FMult (FAdd (FNum 2.0,FNum 3.0),FNum 4.0))
    Expression.parse "(2.7+3.5553)*4.98"|> should equal (FMult (FAdd (FNum 2.7,FNum 3.5553),FNum 4.98))

[<Fact>]
let ``implicit inserting of multiplication for parentheses`` () =
    Expression.parse "-1(2+3)"          |> should equal (FMult (FNum -1.0,FAdd (FNum 2.0,FNum 3.0)))
    Expression.parse "(2+3)4"           |> should equal (FMult (FAdd (FNum 2.0,FNum 3.0),FNum 4.0))

[<Fact>]
let ``usage of exponent`` () =
    Expression.parse "2^4"              |> should equal (FExponent (FNum 2.0,4))
    Expression.parse "x^2"              |> should equal (FExponent (FVar "x",2))
    Expression.parse "x^-2"             |> should equal (FExponent (FVar "x",-2))

[<Fact>]
let ``variable before number, should be read as a name`` () =
    Expression.parse "x2"               |> should equal (FVar "x2")
    Expression.parse "x2example"        |> should equal (FVar "x2example")

[<Fact>]
let ``variable after number should insert an implicit multiplication between numbers and variables`` () =
    Expression.parse "2x"               |> should equal (FMult (FNum 2.0,FVar "x"))
    Expression.parse "-1x"              |> should equal (FMult (FNum -1.0,FVar "x"))
    Expression.parse "2.0xy"            |> should equal (FMult (FNum 2.0,FVar "xy"))

[<Fact>]
let ``ignore whitespace`` () =
    Expression.parse "2 x"              |> should equal (FMult (FNum 2.0,FVar "x"))
    Expression.parse "2 x y"            |> should equal (FMult (FMult (FNum 2.0,FVar "x"),FVar "y"))
    Expression.parse "2    x y"         |> should equal (FMult (FMult (FNum 2.0,FVar "x"),FVar "y"))

[<Fact>]
let ``longer and more complex expressions`` () =
    Expression.parse "2x(3x)"           |> should equal (FMult (FMult (FNum 2.0,FVar "x"),FMult (FNum 3.0,FVar "x")))
    Expression.parse "2 x 2 y(2 x(-2))" |> should equal (FMult (FMult (FMult (FMult (FNum 2.0,FVar "x"),FNum 2.0),FVar "y"), FMult (FMult (FNum 2.0,FVar "x"),FNum -2.0)))
    Expression.parse "2x^2*2y^2"        |> should equal (FMult (FMult (FMult (FNum 2.0,FExponent (FVar "x",2)),FNum 2.0), FExponent (FVar "y",2)))
    Expression.parse "2x^2(2y^2)"       |> should equal (FMult (FMult (FNum 2.0,FExponent (FVar "x",2)), FMult (FNum 2.0,FExponent (FVar "y",2))))

[<Fact>]
let ``implicit negation of variables and parentheses`` () =
    Expression.parse "-x"               |> should equal (FMult (FNum -1.0,FVar "x"))
    Expression.parse "-(2+3)"           |> should equal (FMult (FNum -1.0,FAdd (FNum 2.0,FNum 3.0)))
    Expression.parse "- (2+3)"          |> should equal (FMult (FNum -1.0,FAdd (FNum 2.0,FNum 3.0)))

[<Fact>]
let ``usage of root`` () =
    Expression.parse "2_3"              |> should equal (FRoot (FNum 2.0,3))
    Expression.parse "5*2_3"            |> should equal (FMult (FNum 5.0,FRoot (FNum 2.0,3)))

[<Fact>]
let ``invalid expressions`` () =
    // Multiple operators in succession (except for operator followed by -).
    (fun () -> Expression.parse "2++3"      |> ignore)  |> shouldFail
    (fun () -> Expression.parse "2*/3"      |> ignore)  |> shouldFail
    (fun () -> Expression.parse "2-+3"      |> ignore)  |> shouldFail
    (fun () -> Expression.parse "2*/3"      |> ignore)  |> shouldFail
    // Odd number of left and right parentheses.
    (fun () -> Expression.parse "2*(2"      |> ignore)  |> shouldFail
    (fun () -> Expression.parse "2+2)"      |> ignore)  |> shouldFail
    // Usage of comma instead of dot.
    (fun () -> Expression.parse "2,0"       |> ignore)  |> shouldFail
    // Empty expression.
    (fun () -> Expression.parse "()"        |> ignore)  |> shouldFail
    // Float exponent / root
    (fun () -> Expression.parse "2^2.0"     |> ignore)  |> shouldFail
    (fun () -> Expression.parse "2_2.0"     |> ignore)  |> shouldFail
    // Multiple exponent / root
    (fun () -> Expression.parse "2^3^4"     |> ignore)  |> shouldFail
    (fun () -> Expression.parse "2^3_4"     |> ignore)  |> shouldFail
    (fun () -> Expression.parse "2_3_4"     |> ignore)  |> shouldFail
    // Expression in exponent / root
    (fun () -> Expression.parse "2^x"       |> ignore)  |> shouldFail
    (fun () -> Expression.parse "2_x"       |> ignore)  |> shouldFail
    (fun () -> Expression.parse "2_(2+3)"   |> ignore)  |> shouldFail
    (fun () -> Expression.parse "2^(2+3)"   |> ignore)  |> shouldFail

