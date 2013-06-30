open Camlp4
open Pa_deriving_common
open Utils

module Id : Sig.Id =
struct
  let name = "pa_dom"
  let version = "0.1"
end

module Description : Defs.ClassDescription = struct
  let classname = "Dom_type"
  let runtimename = "Dom_type_dummy"
  let default_module = None
  let alpha = None
  let allow_private = false
  let predefs = [
  ]

  let depends = []
end


module Builder(Loc : Defs.Loc) = struct

  module Helpers = Base.AstHelpers(Loc)
  module Generator = Base.Generator(Loc)(Description)

  open Loc
  open Camlp4.PreCast
  open Description

  let generator = (object(self)

  inherit Generator.generator

    method proxy () =
      None, [
      ]

    method record ?eq ctxt tname params constraints (fields : Pa_deriving_common.Type.field list) =
      [
        <:str_item< value f _ =  assert False >>
      ]

    method tuple ctxt tys =
      [
        <:str_item< value f _ =  assert False >>
      ]

    method sum ?eq ctxt tname params constraints summands =
      [
        <:str_item< value f _ =  assert False >>
      ]


    method variant ctxt tname params constraints (_, tags) =
      [
        <:str_item< value f _ =  assert False >>
      ]


end :> Generator.generator)

let generate = Generator.generate generator
let generate_sigs = Generator.generate_sigs generator

end

module Dom_type = Base.Register(Description)(Builder)
