package Tokens is

   type Token_Type is (MAIN_TOK, LEFT_PAREN_TOK, RIGHT_PAREN_TOK, EOLN_TOK,
                       INDENT_TOK, DEDENT_TOK, IF_TOK, COLON_TOK, ELSE_TOK,
                       ELIF_TOK, WHILE_TOK, ID_TOK, PRINT_TOK, CONST_TOK,
                       GE_TOK, GT_TOK, LE_TOK, LT_TOK, EQ_TOK, NE_TOK, ADD_TOK,
                       SUB_TOK, MUL_TOK, DIV_TOK, ASSIGN_TOK, EOS_TOK);

   LEXEME_SIZE: constant Positive := 10;

   type Lexeme is new String (1 .. LEXEME_SIZE);

   type Token is private;

   function create_token (tok_type: in Token_Type; lex: in Lexeme;
                          row_num: in Positive; col_num: in Positive)
                          return Token;

   function get_token_type (tok: in Token) return Token_Type;

   function get_lexeme (tok: in Token) return Lexeme;

   function get_row_number (tok: in Token) return Positive;

   function get_column_number(tok: in Token) return Positive;

   function to_lexeme (s: in String) return Lexeme;

   function lexeme_length (lex: in Lexeme) return Positive;

private
   type Token is
      record
         tok_type: Token_Type;
         lex: Lexeme;
         row_number: Positive;
         column_number: Positive;
      end record;

end Tokens;
