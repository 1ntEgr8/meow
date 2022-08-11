module type Checker = sig
  type t

  type context

  val typeof : t -> context -> Types.t
end
