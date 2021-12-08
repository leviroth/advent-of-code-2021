open! Core_kernel

module type S = sig
  type t

  val of_string : string -> t
  val load : string -> t
end

module Parseable = struct
  module type Basic = sig
    type t

    val parser : t Angstrom.t
  end

  module type S = sig
    include Basic
    include S with type t := t
  end
end

module type Input = sig
  module type S = S

  module Parseable = Parseable
  module Make_parseable (Parser : Parseable.Basic) : Parseable.S with type t = Parser.t

  module Make_parseable_many (Parser : Parseable.Basic) : sig
    include Parseable.S with type t = Parser.t list
    module Single : Parseable.S with type t = Parser.t
  end

  module Int_list : S with type t = int list
  module String_list : S with type t = string list
  module String : S with type t = string
end
