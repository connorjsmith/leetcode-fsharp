// problem: https://leetcode.com/problems/roman-to-integer/description/

let romanToInteger (roman:string) =
    let convertDigit firstDigit secondDigit =
        match secondDigit with
            | 'I' -> 1
            // We take off double the preceeding letter to compensate for the previous addition
            | 'V' when firstDigit = 'I' -> 3
            | 'X' when firstDigit = 'I' -> 8
            | 'L' when firstDigit = 'X' -> 30
            | 'C' when firstDigit = 'X' -> 80
            | 'D' when firstDigit = 'C' -> 300
            | 'M' when firstDigit = 'C' -> 800
            | 'V' -> 5
            | 'X' -> 10
            | 'L' -> 50
            | 'C' -> 100
            | 'D' -> 500
            | 'M' -> 1000
            | other -> failwithf "Invalid character: '%c'" other

    let rec romanToInt charList currentTotal lastChar =
        match charList with
            // Could use an active pattern matcher here to better handle nonsensical input
            | first :: remainder -> romanToInt remainder (currentTotal+(convertDigit lastChar first)) first
            | _ -> currentTotal

    let charList = roman.ToCharArray() |> Array.toList;
    let dummy:char = ' '
    romanToInt charList 0 dummy

printfn "%d" (romanToInteger "III")
printfn "%d" (romanToInteger "IV")
printfn "%d" (romanToInteger "IX")
printfn "%d" (romanToInteger "LVIII")
printfn "%d" (romanToInteger "MCMXCIV")
printfn "%d" (romanToInteger "")
printfn "%d" (romanToInteger "nonsense")
