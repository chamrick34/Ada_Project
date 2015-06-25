with Lexical_Analyzers, Tokens, Statements, Boolean_Expressions,
     Arithmetic_Expressions, Literal_Integers, Ids, Programs;
use Lexical_Analyzers, Tokens, Statements, Boolean_Expressions,
    Arithmetic_Expressions, Literal_Integers, Ids, Programs;

package body Parsers is

   procedure match(tok: in Token; expected: Token_Type);
   procedure get_arithmetic_operator (lex: in out Lexical_Analyzer; op: out Arithmetic_Operator);
   procedure get_relative_operator(lex: in out Lexical_Analyzer; op: out Relational_Operator);
   procedure get_arithmetic_expression(lex: in out Lexical_Analyzer; expr: out Expression_Access);
   function is_valid_start_of_statement(tok: in Token) return boolean;
   procedure get_if_statement(lex: in out Lexical_Analyzer; stmt: out Statement_Access);
   procedure get_statement(lex: in out Lexical_Analyzer; s: out Statement_Access);
   procedure get_statement_list(lex: in out Lexical_Analyzer; sList: out Statement_List);
   procedure get_while_statement(lex: in out Lexical_Analyzer; stmt: out Statement_Access);
   procedure get_print_statement(lex: in out Lexical_Analyzer; s: out Statement_Access);
   procedure get_literal_constant(lex: in out Lexical_Analyzer; int: out Literal_Integer);


   --------------------
   --match--
   --------------------

   procedure match(tok: in Token; expected: Token_Type) is

   begin
      if get_token_type(tok) /= expected then
         raise parser_exception with "expected at row " & Positive'Image(get_row_number(tok)) &
           " and column " & Positive'Image(get_column_number(tok));
      end if;
   end match;

   --------------------
   --arithmetic_operator--
   --------------------

   procedure get_arithmetic_operator (lex: in out Lexical_Analyzer; op: out Arithmetic_Operator) is
      tok: Token;
   begin
      get_next_token(lex => lex, tok => tok);
      if get_token_type(tok => tok) = ADD_TOK then
         op := ADD_OP;
      elsif get_token_type(tok => tok) = SUB_TOK then
         op := SUB_OP;
      elsif get_token_type(tok => tok) = DIV_TOK then
         op := DIV_OP;
      elsif get_token_type(tok => tok) = MUL_TOK then
         op := MUL_OP;
      else
         raise parser_exception with "arithmetic operator expected at row: " & Positive'Image(get_row_number(tok)) &
           " and column: " & Positive'Image(get_column_number(tok));
      end if;
   end get_arithmetic_operator;


   --------------------
   --relative_operator--
   --------------------

   procedure get_relative_operator(lex: in out Lexical_Analyzer; op: out Relational_Operator) is
      tok: Token;
   begin
      get_next_token(lex => lex, tok => tok);
      if get_token_type(tok => tok) = GT_TOK then
         op := GT_OP;
      elsif get_token_type(tok => tok) = LT_TOK then
         op := LT_OP;
      elsif get_token_type(tok => tok) = EQ_TOK then
         op := EQ_OP;
      elsif get_token_type(tok => tok) = GE_TOK then
         op := GE_OP;
      elsif get_token_type(tok => tok) = LE_TOK then
         op := LE_OP;
      elsif get_token_type(tok => tok) = NE_TOK then
         op := NE_OP;
      else
         raise parser_exception with "relational operator expected at row: " & Positive'Image(get_row_number(tok)) &
           " and column: " & Positive'Image(get_column_number(tok));
      end if;
   end get_relative_operator;


   --------------------
   --arithmetic_expression--
   --------------------

   procedure get_arithmetic_expression(lex: in out Lexical_Analyzer; expr: out Expression_Access) is
      tok: Token;
      expr1: Expression_Access;
      expr2: Expression_Access;
      op: Arithmetic_Operator;
      var: Id;
      int: Literal_Integer;

   begin
      tok := get_lookahead_token(lex => lex);
      if get_token_type(tok => tok) = ID_TOK then
         get_next_token(lex => lex, tok => tok);
         var := create_id(Character(get_lexeme(tok)(1)));
         expr := create_var_expression(var);
      elsif get_token_type(tok => tok) = CONST_TOK then
         get_next_token(lex => lex, tok => tok);
         int := create_literal_integer(Integer'Value(String(get_lexeme(tok))));
         expr := create_constant_expression(int);
      else
         get_arithmetic_operator(lex => lex, op => op);
         get_arithmetic_expression(lex => lex, expr => expr1);
         get_arithmetic_expression(lex => lex, expr => expr2);
         expr := create_binary_expression(op => op, expr1 => expr1, expr2 => expr2);
      end if;
   end get_arithmetic_expression;


    --------------------
   --get_Id--
   --------------------

   procedure get_Id(lex: in out Lexical_Analyzer; var: out Id) is
      tok: Token;

   begin
      get_next_token(lex => lex, tok => tok);
      match(tok => tok, expected => ID_TOK);
      var := create_id(ch => Character(get_lexeme(tok)(1)));

   end get_Id;

    --------------------
   --get_assignment_statement--
   --------------------

   procedure get_assignment_statement(lex: in out Lexical_Analyzer; s: out Statement_Access) is
      tok: Token;
      var: Id;
      expr: Expression_Access;

   begin
      get_Id(lex => lex, var => var);
      get_next_token(lex => lex, tok => tok);
      match(tok => tok, expected => ASSIGN_TOK);
      get_arithmetic_expression(lex => lex, expr => expr);
      get_next_token(lex => lex, tok => tok);
      match(tok => tok, expected => EOLN_TOK);
      s := create_assignment_statement(var => var, expr => expr);
   end get_assignment_statement;

   --------------------
   --valid_start_of_statement--
   --------------------

   function is_valid_start_of_statement(tok: in Token) return boolean is
   begin
      return get_token_type(tok => tok) = ID_TOK or get_token_type(tok => tok) = IF_TOK
        or get_token_type(tok => tok) = WHILE_TOK or get_token_type(tok => tok) = PRINT_TOK;
   end is_valid_start_of_statement;

   --------------------
   --get_boolean_expression--
   --------------------

   procedure get_boolean_expression(lex: in out Lexical_Analyzer; expr: out Boolean_Expression) is
      op: Relational_Operator;
      expr1: Expression_Access;
      expr2: Expression_Access;

   begin
      get_relative_operator(lex => lex, op => op);
      get_arithmetic_expression(lex => lex, expr => expr1);
      get_arithmetic_expression(lex => lex, expr => expr2);
      expr := create_boolean_expression(op => op, expr1 => expr1, expr2 => expr2);
   end get_boolean_expression;


   --------------------
   --get_statement_block--
   --------------------


   procedure get_statement_block(lex: in out Lexical_Analyzer; sBlock: out Statement_Block) is

      sList: Statement_List;
      blk: Statement_Block;
      tok: Token;

   begin
         get_next_token(lex => lex, tok => tok);
         match(tok, INDENT_TOK);
         get_statement_list(lex => lex, sList => sList);
         get_next_token(lex => lex, tok => tok);
         match(tok, DEDENT_TOK);
      blk := create_statement_block(stmts => sList);
      sBlock := blk;
   end get_statement_block;



   --------------------
   --get_if_statement--
   --------------------

   procedure get_if_statement(lex: in out Lexical_Analyzer; stmt: out Statement_Access) is
      tok: Token;
      bool_expr: Boolean_Expression;
      blk: Statement_Block;

   begin
      stmt := create_if_statement;
      get_next_token(lex => lex, tok => tok);
      match(tok => tok, expected => IF_TOK);
      get_boolean_expression(lex => lex, expr => bool_expr);
         add_expression(s => stmt, expr => bool_expr);
         get_next_token(lex => lex, tok => tok);
         match(tok => tok, expected => COLON_TOK);
         get_next_token(lex => lex, tok => tok);
         match(tok => tok, expected => EOLN_TOK);
         get_statement_block(lex => lex, sBlock => blk);
         add_block(s => stmt, blk => blk);
         get_next_token(lex => lex, tok => tok);

         while get_token_type(tok => tok) = ELIF_TOK loop
            match(tok => tok, expected => ELIF_TOK);
            get_boolean_expression(lex => lex, expr => bool_expr);
            get_next_token(lex => lex, tok => tok);
            match(tok => tok, expected => COLON_TOK);
            get_next_token(lex => lex, tok => tok);
            match(tok => tok, expected => EOLN_TOK);
            get_statement_block(lex => lex, sBlock => blk);
            add_block(s => stmt, blk => blk);
            get_next_token(lex => lex, tok => tok);
         end loop;

         match(tok => tok, expected => ELSE_TOK);
         get_next_token(lex => lex, tok => tok);
         match(tok => tok, expected => COLON_TOK);
         get_next_token(lex => lex, tok => tok);
         match(tok => tok, expected => EOLN_TOK);
         get_statement_block(lex => lex, sBlock => blk);
         add_block(s => stmt, blk => blk);

   end get_if_statement;

   --------------------
   --get_statement--
   --------------------

   procedure get_statement(lex: in out Lexical_Analyzer; s: out Statement_Access) is
      tok: Token;

      begin
         tok := get_lookahead_token(lex => lex);
         if get_token_type(tok => tok) = IF_TOK then
            get_if_statement(lex => lex, stmt => s);
         elsif get_token_type(tok => tok) = ID_TOK then
            get_assignment_statement(lex => lex, s => s);
         elsif get_token_type(tok => tok) = PRINT_TOK then
            get_print_statement(lex => lex, s => s);
         elsif get_token_type(tok => tok) = WHILE_TOK then
            get_while_statement(lex => lex, stmt => s);
         else
            raise parser_exception with "statement expected at row: " & Positive'Image(get_row_number(tok))
              & " and column: " & Positive'Image(get_column_number(tok));
         end if;
      end get_statement;


   --------------------
   --get_statement_list--
   --------------------

   procedure get_statement_list(lex: in out Lexical_Analyzer; sList: out Statement_List) is
         s: Statement_Access;
         tok: Token;

      begin
         sList := create_statement_list;
         get_statement(lex => lex, s => s);
         add(stmts => sList, s => s);
         tok := get_lookahead_token(lex => lex);
         while is_valid_start_of_statement(tok) loop
            get_statement(lex => lex, s => s);
            add(stmts => sList, s => s);
            tok := get_lookahead_token(lex => lex);
         end loop;
      end get_statement_list;


   --------------------
   --get_while_statement--
   --------------------

   procedure get_while_statement(lex: in out Lexical_Analyzer; stmt: out Statement_Access) is
         tok: Token;
         expr: Boolean_Expression;
         sBlock: Statement_Block;

   begin
         get_next_token(lex => lex, tok => tok);
         match(tok => tok, expected => WHILE_TOK);
         get_boolean_expression(lex => lex, expr => expr);
         get_next_token(lex => lex, tok => tok);
         match(tok => tok, expected => COLON_TOK);
         get_next_token(lex => lex, tok => tok);
         match(tok => tok, expected => EOLN_TOK);
         get_statement_block(lex => lex, sBlock => sBlock);
         stmt := create_while_statement(expr => expr, blk => sBlock);
   end get_while_statement;



   --------------------
   --get_print_statement--
   --------------------

   procedure get_print_statement(lex: in out Lexical_Analyzer; s: out Statement_Access) is
            tok: Token;
            expr: Expression_Access;
   begin
            get_next_token(lex => lex, tok => tok);
            match(tok => tok, expected => PRINT_TOK);
            get_next_token(lex => lex, tok => tok);
            match(tok => tok, expected => LEFT_PAREN_TOK);
            get_arithmetic_expression(lex => lex, expr => expr);
            get_next_token(lex => lex, tok => tok);
            match(tok => tok, expected => RIGHT_PAREN_TOK);
            get_next_token(lex => lex, tok => tok);
            match(tok => tok, expected => EOLN_TOK);
            s := create_print_statement(expr => expr);
   end get_print_statement;



   --------------------
   --get_literal_constant--
   --------------------

   procedure get_literal_constant(lex: in out Lexical_Analyzer; int: out Literal_Integer) is
         tok: Token;
         x: Integer;

   begin
         get_next_token(lex => lex, tok => tok);
         match(tok => tok, expected => CONST_TOK);
         x := Integer'Value(String(get_lexeme(tok => tok)));
         int := create_literal_integer(value => x);
   end get_literal_constant;


   --------------------
   --create_parser--
   --------------------

   function create_parser(file_name: in String) return Parser is
        par: Parser;

   begin
       par.lex := create_lexical_analyzer(file_name => file_name);
       return par;
   end create_parser;


   --------------------
   --parse--
   --------------------

   procedure parse(p: in out Parser; prog: out Program) is
      tok: Token;
      sBlock: Statement_Block;
      lex: Lexical_Analyzer;

   begin
      get_next_token(lex => lex, tok => tok);
      match(tok => tok, expected => MAIN_TOK);
      get_next_token(lex => lex, tok => tok);
      match(tok => tok, expected => LEFT_PAREN_TOK);
      get_next_token(lex => lex, tok => tok);
      match(tok => tok, expected => RIGHT_PAREN_TOK);
      get_next_token(lex => lex, tok => tok);
      match(tok => tok, expected => EOLN_TOK);
      get_statement_block(lex => lex, sBlock => sBlock);
      get_next_token(lex => lex, tok => tok);
      if (get_token_type(tok => tok) /= EOS_TOK) then
         raise parser_exception with "invalid lexemes at end of file";
      end if;
      prog := create_program(blk => sBlock);
   end parse;
end Parsers;
