// source: https://leetcode.com/problems/valid-parentheses/description/
let (|StartsWith|_|) (f:string) (s:string) =
    if s.StartsWith(f) then
        Some(s.Substring(f.Length))
    else
        None

let valid_parentheses string =
    let rec validParens s roundCount curlyCount squareCount =
        match s with
            | "" when roundCount = 0 && curlyCount = 0 && squareCount = 0 -> true
            | _  when roundCount < 0 || curlyCount < 0 || squareCount < 0 -> false
            | StartsWith "(" rest -> validParens rest (roundCount+1)  curlyCount     squareCount
            | StartsWith "{" rest -> validParens rest roundCount      (curlyCount+1) squareCount
            | StartsWith "[" rest -> validParens rest roundCount      curlyCount     (squareCount+1)
            | StartsWith ")" rest -> validParens rest (roundCount-1)  curlyCount     squareCount
            | StartsWith "}" rest -> validParens rest roundCount      (curlyCount-1) squareCount
            | StartsWith "]" rest -> validParens rest roundCount      curlyCount     (squareCount-1)
            | _   -> false

    validParens string 0 0 0


printfn "%b" (valid_parentheses "(())")
printfn "%b" (valid_parentheses "([])")
printfn "%b" (valid_parentheses "(({)")
printfn "%b" (valid_parentheses "}")
printfn "%b" (valid_parentheses "}")
