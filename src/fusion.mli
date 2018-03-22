module Stream :
  sig
    type ('a, 's) step =
        | Done
        | Yield of ('a * 's)
        | Skip of 's

    type 'a t = Stream : (('s -> ('a, 's) step) * 's) -> 'a t

    (** Create a new stream from a list of items *)
    val from_list : 'a list -> 'a t

    (** Consume stream into a list *)
    val to_list : 'a t -> 'a list

    (** Apply a function to each item of a stream *)
    val iter : ('a -> unit) -> 'a t -> unit

    (** Apply a function to each item in a stream, creating a new stream *)
    val map : ('a -> 'b) -> 'a t -> 'b t

    (** Remove items from a stream based on the given predicate *)
    val filter : ('a -> bool) -> 'a t -> 'a t

    (** Reduce a stream using the given initializer *)
    val foldr : ('a -> 'b -> 'b) -> 'b -> 'a t -> 'b
    val foldl : ('a -> 'b -> 'a) -> 'a -> 'b t -> 'a

    (** Join two streams into a stream of tuples *)
    val zip : 'a t -> 'b t -> ('a * 'b) t

    (** Join two streams *)
    val append : 'a t -> 'a t -> 'a t

    (** Create a stream with the given value as the only item *)
    val return : 'a -> 'a t

    (** Add a single item to the end of the stream *)
    val add : 'a t -> 'a -> 'a t

    (** Concat nested streams *)
    val concat_map : ('a -> 'b t) -> 'a t -> 'b t

    (** Create a new stream lazily using a generator function *)
    val from : ('a -> 'a option) -> 'a option -> 'a t

    (** Append a list to the end of a stream *)
    val push : 'a t -> 'a list -> 'a t

    (** Create a shorter stream of the given length *)
    val take : int -> 'a t -> 'a t

    (** Ignore the given number of items *)
    val skip : int -> 'a t -> 'a t
  end
