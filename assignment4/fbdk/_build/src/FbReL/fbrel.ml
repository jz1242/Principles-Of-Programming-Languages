module Language = struct
  
  let name = "FbRel"
  module Parser = Fbrelparser
  module Lexer = Fbrellexer
  module Ast = Fbrelast
  module Pp = Fbrelpp
  module Options = Fbreloptions
  module Interpreter = Fbrelinterp
	module Typechecker = Fbreltype

end;;

module Application = Application.Make(Language);;

Application.main ();;
