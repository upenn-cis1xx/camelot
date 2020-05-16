module type State = sig
  type 'a t
  val append : 'a -> 'a t -> 'a t
  val fresh : 'a t
end
