// Two-Sum problem
// https://leetcode.com/problems/two-sum/description/


let nums = [2; 3; 4; 2];
let target = 4;

let twoSum numbers target =
    // Create a list of (value, index) for each element in list
    let indexed =
        numbers
        |> List.mapi(fun i v -> (v, i));

    // Create a map of (value, index) for each element in list
    let map =
        indexed
        |> Map.ofList;

    indexed
    |> List.tryPick(fun (value, idx) ->
                    let searchedIdx = map.TryFind (target-value)
                    match searchedIdx with
                    // Ignore cases where I'd use the same number twice
                    | Some x when x = idx -> None
                    // Found two indices which satisfy problem
                    | Some x -> Some (idx, x)
                    // Found nothing
                    | _ -> None)


let result = twoSum nums target;

match result with
    | Some (first, second) -> printfn "Found %d %d" first second
    | None -> printfn "Found nothing"
