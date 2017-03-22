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
    val from : (unit -> 'b option) -> 'b t
    val push : 'a t -> 'a list -> 'a t
    val take : int -> 'a t -> 'a t
    val skip : int -> 'a t -> 'a t
  end
class ['a] stream :
  'a Stream.t ->
  object
    val mutable s : 'a Stream.t
    method append : 'a Stream.t -> unit
    method filter : ('a -> bool) -> unit
    method foldl : ('b -> 'a -> 'b) -> 'b -> 'b
    method foldr : ('a -> 'b -> 'b) -> 'b -> 'b
    method get : unit -> 'a Stream.t
    method map : ('a -> 'a) -> 'a stream
    method push : 'a list -> unit
    method skip : int -> unit
    method take : int -> unit
    method to_list : unit -> 'a list
    method update : 'a Stream.t -> unit
  end
val empty : unit -> 'a stream
val from : (unit -> 'a option) -> 'a stream
val from_list : 'a list -> 'a stream
