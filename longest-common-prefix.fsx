// Problem Source: https://leetcode.com/problems/longest-common-prefix/description/

let longestCommonPrefix (strList:string list) =
    let prefixLength (string1:string) (string2:string) =
        let minStringLength = min string1.Length string2.Length
        let list1 = string1.Substring(0, minStringLength).ToCharArray() |> Array.toList
        let list2 = string2.Substring(0, minStringLength).ToCharArray() |> Array.toList
        let zipped = List.zip list1 list2

        let predicate (first, second) =
            not (first.Equals(second))
        match List.tryFindIndex predicate zipped with
            | Some value -> value
            | None -> minStringLength

    match strList with
        | [] -> ""
        | first :: rest ->
            // Could probably convert all the strings to char lists here and save some runtime in prefixLength
            List.fold (fun acc curr -> acc.Substring(0, prefixLength acc curr)) first rest


// Testing
// let tests = [
//     ([""; "abc"], "");
//     (["abc"; "a"; "ab"], "a");
//     (["abc"; "ab"; "ab"], "ab");
//     (["abc"; "abc"], "abc");
//     ([], "");
//     ([""], "");
//     (["abc"], "abc")
// ]

// for (test, expected) in tests do
//     printfn "\"%s\" = \"%s\"" expected (longestCommonPrefix test)
