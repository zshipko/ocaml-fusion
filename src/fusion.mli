module Stream :
  sig
    type ('a, 's) step = Done | Yield of ('a * 's) | Skip of 's
    type 'a t = Stream : (('s -> ('a, 's) step) * 's) -> 'a t
    val from_list : 'a list -> 'a t
    val to_list : 'a t -> 'a list
    val map : ('a -> 'b) -> 'a t -> 'b t
    val filter : ('a -> bool) -> 'a t -> 'a t
    val foldr : ('a -> 'b -> 'b) -> 'b -> 'a t -> 'b
    val foldl : ('a -> 'b -> 'a) -> 'a -> 'b t -> 'a
    val zip : 'a t -> 'b t -> ('a * 'b) t
    val append : 'a t -> 'a t -> 'a t
    val return : 'a -> 'a t
    val concat_map : ('a -> 'b t) -> 'a t -> 'b t
    val from : ('a -> 'a option) -> 'a option -> 'a t
    val push : 'a t -> 'a list -> 'a t
    val take : int -> 'a t -> 'a t
    val skip : int -> 'a t -> 'a t
  end
