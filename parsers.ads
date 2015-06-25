with Lexical_Analyzers, Programs;
use Lexical_Analyzers, Programs;

package Parsers is

   parser_exception: exception;

   type Parser is private;

   function create_parser (file_name: in String) return Parser;

   procedure parse (p: in out Parser; prog: out Program);

private
   type Parser is
      record
         lex: Lexical_Analyzer;
      end record;

end Parsers;
