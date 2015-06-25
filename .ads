with Lexical_Analyzers, Programs;
use Lexical_Analyzers, Programs;

package Parsers is

   parser_exception: exception;

   type Parser is private;

   function create_parser (file_name: in String) return Parser;

   procedure parse (p: in out Parser; prog: out Program);

   procedure match(tok: in Token; expected: Token_Type);

   procedure get_arithmetic_operator (lex: in out Lexical_Analyzer; op: out Arithmetic_Operator);

   procedure get_relative_operator(lex: in out Lexical_Analyzer; op: out Relational_Operator);

   procedure get_arithmetic_expression(lex: in out Lexical_Analyzer; expr: out Expression_Access;);

   function is_valid_start_of_statement(tok: in Token) return boolean;

   procedure get_statement_block(lex: in out Lexical_Analyzer, sBlock: out Statement_Block);

   procedure get_if_statement(lex: in out Lexical_Analyzer, stmt: out Statement_Access);

   procedure get_statement(lex: in out Lexical_Analyzer; s: out Statement_Access);

   procedure get_statement_list(lex: in out Lexical_Analyzer, sList: out Statement_List);

   procedure get_while_statement(lex: in out Lexical_Analyzer, stmt: out Statement_Access);

   procedure get_print_statement(lex: in out Lexical_Analyzer, s: out Statement_Access);

   procedure get_literal_constant(lex: in out Lexical_Analyzer, int: Literal_Integer);

   procedure get_Id(lex: Lexical_Analyzer, id: out Id);

   function create_parser(file_name: in String) return Parser;

   procedure parse(p: in out Parser, prog: out Program);

private
   type Parser is
      record
         lex: Lexical_Analyzer;
      end record;

end Parsers;
