app "parser-basic"
    packages {
        cli: "https://github.com/roc-lang/basic-cli/releases/download/0.7.0/bkGby8jb0tmZYsy2hg1E_B2QrCgcSTxdUlHtETwm5m4.tar.br",
        parser: "https://github.com/lukewilliamboswell/roc-parser/releases/download/0.2.0/dJQSsSmorujhiPNIvJKlQoI92RFIG_JQwUfIxZsCSwE.tar.br",
    }
    imports [
        cli.Stdout,
        parser.Core.{ Parser, many, oneOf, map },
        parser.String.{ parseStr, codeunit, anyCodeunit },
    ]
    provides [main] to cli

firstTask : Text -> Int
firstTask = 

main =
    many letterParser
    |> parseStr inputStr
    |> Result.map countLetterAs
    |> Result.map \count -> "I counted \(count) letter A's!"
    |> Result.withDefault "Ooops, something went wrong parsing"
    |> Stdout.line

Letter : [A, B, C, Other]

inputStr = "AAAiBByAABBwBtCCCiAyArBBx"

# Helper to check if a letter is an A tag
isA = \l -> l == A

# Count the number of Letter A's
countLetterAs : List Letter -> Str
countLetterAs = \letters ->
    letters
    |> List.countIf isA
    |> Num.toStr

CalibrationValue : {first: Nat, second: Nat}

pCalibrationValue : Parser (List U8) CalibrationValue 
pCalibrationValue = 


# Parser to convert utf8 input into Letter tags
letterParser : Parser (List U8) Letter
letterParser =
    oneOf [
        codeunit 'A' |> map \_ -> A,
        codeunit 'B' |> map \_ -> B,
        codeunit 'C' |> map \_ -> C,
        anyCodeunit |> map \_ -> Other,
    ]

# Test we can parse a single B letter
expect
    input = "B"
    parser = letterParser
    result = parseStr parser input
    result == Ok B

# Test we can parse a number of different letters
expect
    input = "BCXA"
    parser = many letterParser
    result = parseStr parser input
    result == Ok [B, C, Other, A]
