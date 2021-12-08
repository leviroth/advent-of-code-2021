open! Core_kernel
open! Import

module type Output = sig
  type t

  val to_string : t -> string
end

module type Part = sig
  module Input : Input.S
  module Output : Output

  val solve : Input.t -> Output.t
end
