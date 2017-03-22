module Stream = struct
    type ('a, 's) step =
        | Done
        | Yield of ('a * 's)
        | Skip of 's

    type 'a t = Stream : (('s -> ('a, 's) step) * 's) -> 'a t

    let from_list xs0 =
        let next = function
            | [] -> Done
            | x::xs -> Yield (x, xs)
        in Stream (next, xs0)

    let to_list (Stream (next0, s0)) =
        let rec unfold s =
            match next0 s with
            | Done -> []
            | Skip s' -> unfold s'
            | Yield (x, s') -> x :: unfold s'
        in unfold s0

    let map fn (Stream (next0, s0)) =
        let next s =
            match next0 s with
            | Done -> Done
            | Skip s' -> Skip s'
            | Yield (x, s') -> Yield (fn x, s')
        in Stream (next, s0)

    let filter pred (Stream (next0, s0)) =
        let next s =
            match next0 s with
            | Done -> Done
            | Skip s' -> Skip s'
            | Yield (x, s') when pred x -> Yield (x, s')
            | Yield (x, s') -> Skip s'
        in Stream (next, s0)

    let foldr fn acc (Stream (next0, s0)) =
        let rec go s =
            match next0 s with
            | Done -> acc
            | Skip s' -> go s'
            | Yield (x, s') -> fn x (go s')
        in go s0

    let foldl fn acc (Stream (next0, s0)) =
        let rec go acc s =
            match next0 s with
            | Done -> acc
            | Skip s' -> go acc s'
            | Yield (x, s') -> go (fn acc x) s'
        in go acc s0

    let zip (Stream (nexta, sa0)) (Stream (nextb, sb0)) =
        let next = function
            | (sa, sb, None) -> begin match nexta sa with
                | Done -> Done
                | Skip sa' -> Skip (sa', sb, None)
                | Yield (a, sa') -> Skip (sa', sb, Some a) end
            | (sa', sb, Some a) -> begin match nextb sb with
                | Done -> Done
                | Skip sb' -> Skip (sa', sb', Some a)
                | Yield (b, sb') -> Yield ((a, b), (sa', sb', None)) end
        in Stream (next, (sa0, sb0, None))

    let append (Stream (nexta, sa0)) (Stream (nextb, sb0)) =
        let next = function
            | `Left sa -> begin match nexta sa with
                | Done -> Skip (`Right sb0)
                | Skip sa' -> Skip (`Left sa')
                | Yield (x, sa') -> Yield (x, `Left sa')
            end

            | `Right sb -> begin match nextb sb with
                | Done -> Done
                | Skip sb' -> Skip (`Right sb')
                | Yield (x, sb') -> Yield (x, `Right sb')
            end
        in Stream (next, `Left sa0)

    let return x =
        let next = function
            | `True -> Yield (x, `False)
            | `False -> Done
        in Stream (next, `True)

    let concat_map fn (Stream (nexta, sa0)) =
        let next = function
            | (sa, None) -> begin match nexta sa with
                | Done -> Done
                | Skip sa' -> Skip (sa', None)
                | Yield (a, sa') -> Skip (sa', Some (fn a)) end
            | (sa, Some (Stream (nextb, sb))) -> begin match nextb sb with
                | Done -> Skip (sa, None)
                | Skip sb' -> Skip (sa, Some (Stream (nextb, sb')))
                | Yield (b, sb') -> Yield (b, (sa, Some (Stream (nextb, sb')))) end
        in Stream (next, (sa0, None))

    let from (fn : unit -> 'b option) : 'b t =
        let rec next = function
            | None -> Done
            | Some n -> Yield (n, fn ())
        in Stream (next, fn ())

    let push s x = append s (from_list x)

    let take n s =
        let next = function
            | (0, s) -> Done
            | (n, Stream (next, s)) -> begin match next s with
                | Done -> Done
                | Skip s' -> Skip (n, Stream (next, s'))
                | Yield (x, s') -> Yield (x, (n - 1, Stream (next, s'))) end
        in Stream (next, (n, s))

    let skip n s =
        let nn = ref n in
        filter (fun _ ->
            let x = !nn in
            let _ = if x > 0 then
                nn := x - 1 in
            x <= 0) s
end

class ['a] stream init = object(self)
    val mutable s : 'a Stream.t = init
    val mutable skipped : int = 0

    method get () = s
    method to_list () = Stream.to_list s
    method update x = s <- x
    method append s' = self#update (Stream.append s s')
    method push l = self#update (Stream.push s l)
    method skip n = skipped <- skipped + n; self#update (Stream.skip skipped s)
    method take n = self#update (Stream.take n s)
    method map fn = new stream (Stream.map fn s)
    method filter fn = self#update (Stream.filter fn s)
    method foldr : 'b . ('a -> 'b -> 'b) -> 'b -> 'b = fun fn x -> Stream.foldr fn x s
    method foldl : 'b . ('b -> 'a -> 'b) -> 'b -> 'b = fun fn x -> Stream.foldl fn x s
end

let empty () = new stream (Stream.from_list [])
let from fn =  new stream (Stream.from fn)
let from_list l = new stream (Stream.from_list l)
