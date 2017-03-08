let upto _to =
    Fusion.Stream.from 0 (fun x ->
        match x + 1 with
        | n when n < _to -> Some n
        | _ -> None)

let upto_10 = [0; 1; 2; 3; 4; 5; 6; 7; 8; 9]

let test_from t =
    Test.check t "from" (fun () ->
        let s = upto 10 in
        Fusion.Stream.to_list s) upto_10

let test_map t =
    Test.check t "map" (fun () ->
        let s = upto 10 in
        let s = Fusion.Stream.map (fun x -> x * 2) s in
        Fusion.Stream.to_list s) (List.map (fun x -> x * 2) upto_10)

let test_from_list t =
    Test.check t "from_list" (fun () ->
        let s = Fusion.Stream.from_list upto_10 in
        Fusion.Stream.to_list s) upto_10

let test_filter t =
    Test.check t "filter" (fun () ->
        let s = upto 10 in
        let s = Fusion.Stream.filter (fun x -> x > 4) s in
        Fusion.Stream.to_list s) [5; 6; 7; 8; 9]

let test_foldl t =
    Test.check t "foldl" (fun () ->
        let s = upto 10 in
        Fusion.Stream.foldl (fun acc x -> acc + x) 0 s) 45

let test_foldr t =
    Test.check t "foldr" (fun () ->
        let s = upto 10 in
        Fusion.Stream.foldr (fun x acc -> acc + x) 0 s) 45

let test_zip t =
    Test.check t "zip" (fun () ->
        let s = upto 10 in
        let t = upto 20 in
        let s = Fusion.Stream.zip s t in
        Fusion.Stream.to_list s) (List.combine upto_10 upto_10)

let test_append t =
    Test.check t "append" (fun () ->
        let s = upto 10 in
        let t = upto 10 in
        let s = Fusion.Stream.append s t in
        Fusion.Stream.to_list s) (upto_10 @ upto_10)

let test_return t =
    Test.check t "return" (fun () ->
        let s = Fusion.Stream.return 1 in
        Fusion.Stream.to_list s) [1]

let test_take t =
    Test.check t "take" (fun () ->
        let s = upto 50 in
        let s = Fusion.Stream.take 10 s in
        Fusion.Stream.to_list s) upto_10

let test_skip t =
    Test.check t "skip" (fun () ->
        let s = upto 20 in
        let s = Fusion.Stream.skip 10 s in
        Fusion.Stream.to_list s) [10; 11; 12; 13; 14; 15; 16; 17; 18; 19]
let _ =
    let t = Test.start () in
    let _ = test_from t in
    let _ = test_map t in
    let _ = test_from_list t in
    let _ = test_filter t in
    let _ = test_foldl t in
    let _ = test_foldr t in
    let _ = test_zip t in
    let _ = test_append t in
    let _ = test_return t in
    let _ = test_take t in
    let _ = test_skip t in
    Test.finish t

