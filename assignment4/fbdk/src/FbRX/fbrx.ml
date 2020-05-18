module Language = struct

  let name = "FbRX"
  module Parser = Fbrxparser
  module Lexer = Fbrxlexer
  module Ast = Fbrxast
  module Pp = Fbrxpp
  module Options = Fbrxoptions
  module Interpreter = Fbrxinterp
  module Typechecker = Fbrxtype

end;;

module Application = Application.Make(Language);;

Application.main ();;
