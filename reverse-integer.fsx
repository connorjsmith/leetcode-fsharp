open System;


let reverseInteger (num:int32) =
    let sign =
        match num with
        | num when num < 0 -> -1;
        | _ -> 1;
    let numString = num.ToString();
    let revString =
        match num with
            | num when num < 0 -> (numString.Substring 1).ToCharArray()
            | _ -> numString.ToCharArray()
        |> Array.toList
        |> List.rev
        |> List.map(fun c -> c.ToString())
        |> String.concat "";
        
    let _, parsed = Int32.TryParse(revString); // parsed = 0 on failure
    sign * parsed;

printfn "Reverse of %d is %d" -12345678 (reverseInteger -12345678);
