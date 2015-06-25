with Ada.Text_IO, Ada.Characters.Latin_1, Ada.Characters.Handling;
use Ada.Text_IO, Ada.Characters.Handling;

package body Lexical_Analyzers is

   MAX_LINE_SIZE: constant Positive := 150;

   ---------------------------------------------------------------------------------------------------------------

   function all_digits (lx: in Lexeme) return Boolean is
      i: Positive := 1;
   begin
      while i <= lexeme_length(lx) and then is_digit (lx(i)) loop
         i := i + 1;
      end loop;
      return i > lexeme_length(lx);
   end all_digits;

   ---------------------------------------------------------------------------------------------------------------

   function more_tokens (lex: in Lexical_Analyzer) return Boolean is

   begin
      return not Token_Lists.is_empty (lex.l);
   end more_tokens;

   -------------------------------------------------------------------------------------------------------------

   function find_num_preceding_tabs (s: in String) return Natural is
      num: Natural := 0;

   begin
      while num < s'Length and then s(num + 1) = Ada.Characters.Latin_1.HT loop
         num := num + 1;
      end loop;
      return num;
   end find_num_preceding_tabs;

   -----------------------------------------------------------------------------------------------------------------------

   function is_white_space (ch: in Character) return Boolean  is

   begin
      return ch = Ada.Characters.Latin_1.HT or ch = Ada.Characters.Latin_1.space;
   end is_white_space;

   -----------------------------------------------------------------------------------------------------------------------

   function skip_white_space (line: in String; location: in Positive) return Positive is
      index: Positive := location;
   begin
      while index <= line'length and then is_white_space (line(index)) loop
         index := index + 1;
      end loop;
      return index;
   end skip_white_space;

   ------------------------------------------------------------------------------------------------------------------------

   function get_new_lexeme (line: in String; index: in Positive) return Lexeme is

      i: Positive := index;
   begin
      while i <= line'length and then not is_white_space (line(i)) loop
         i := i + 1;
      end loop;
      return to_lexeme (line(index..i-1));
   end get_new_lexeme;

   ------------------------------------------------------------------------------------------------------------------------

   function get_lexeme_type (lx: in Lexeme; line_number: in Positive; index: in Positive)
                             return Token_Type is

      tok_type: Token_Type;
   begin
      if is_letter (lx(1)) then
         if lexeme_length (lx) = 1 then
            tok_type := ID_TOK;
         elsif lx = to_lexeme ("main") then
            tok_type := MAIN_TOK;
         elsif lx = to_lexeme ("if") then
            tok_type := IF_TOK;
         elsif lx = to_lexeme ("else") then
            tok_type := ELSE_TOK;
         elsif lx = to_lexeme ("elif") then
            tok_type := ELIF_TOK;
         elsif lx = to_lexeme ("while") then
            tok_type := WHILE_TOK;
         elsif lx = to_lexeme ("print") then
            tok_type := PRINT_TOK;
         else
            raise lexical_exception with "reserved word expected at line " &
              Positive'Image(line_number) & " and column " & Positive'Image(index);
         end if;
      elsif is_digit (lx(1)) then
         if all_digits (lx) then
            tok_type := CONST_TOK;
         else
            raise lexical_exception with "literal integer expected at line " &
              Positive'Image(line_number) & " and column " & Positive'Image(index);
         end if;
      elsif lx = to_lexeme ("(") then
         tok_type := LEFT_PAREN_TOK;
      elsif lx = to_lexeme (")") then
         tok_type := RIGHT_PAREN_TOK;
      elsif lx = to_lexeme (":") then
         tok_type := COLON_TOK;
      elsif lx = to_lexeme ("==") then
         tok_type := EQ_TOK;
      elsif lx = to_lexeme ("!=") then
         tok_type := NE_TOK;
      elsif lx = to_lexeme ("<") then
         tok_type := LT_TOK;
      elsif lx = to_lexeme ("<=") then
         tok_type := LE_TOK;
      elsif lx = to_lexeme (">") then
         tok_type := GT_TOK;
      elsif lx = to_lexeme (">=") then
         tok_type := GE_TOK;
      elsif lx = to_lexeme ("+") then
         tok_type :=ADD_TOK;
      elsif lx = to_lexeme ("-") then
         tok_type :=SUB_TOK;
      elsif lx = to_lexeme ("*") then
         tok_type :=MUL_TOK;
      elsif lx = to_lexeme ("/") then
         tok_type := DIV_TOK;
      elsif lx = to_lexeme ("=") then
         tok_type := ASSIGN_TOK;
      else
         raise lexical_exception with "invalid lexeme at line " &
              Positive'Image(line_number) & " and column " & Positive'Image(index);
      end if;
      return tok_type;
   end get_lexeme_type;

   ---------------------------------------------------------------------------------------------------------------------------

   procedure process_line (lex: in out Lexical_Analyzer; line: in String; line_number: in Positive;
                           num_tabs: in Natural) is

      index: Positive;
      lx: Lexeme;
      tok_type: Token_Type;

   begin
      index := skip_white_space (line, num_tabs + 1);
      while index <= line'Length loop
         lx := get_new_lexeme (line, index);
         tok_type := get_lexeme_type (lx, line_number, index);
         Token_Lists.append (lex.l, create_token (tok_type => tok_type,  lex => lx,
                                                  row_num  => line_number, col_num  => index));
         index := index + lexeme_length (lx);
         index := skip_white_space (line, index);
      end loop;
      Token_Lists.append (lex.l, create_token (tok_type => EOLN_TOK,
                                               lex => to_lexeme ("_EOLN_"),
                                               row_num  => line_number,
                                               col_num  => index));
   end process_line;

   -----------------------------
   -- create_lexical_analyzer --
   -----------------------------

   function create_lexical_analyzer (file_name: in String) return Lexical_Analyzer is
      lex: Lexical_Analyzer;
      f: File_Type;
      line_number: Positive := 1;
      indention: Natural := 0;
      line: String (1 .. MAX_LINE_SIZE);
      last: Natural;
      num_tabs: Natural;
   begin
      open (File => f,  Mode => In_File, Name => file_name);
      while not End_Of_File (f) loop
         get_line (f, line, last);
         num_tabs := find_num_preceding_tabs (line (1..last));
         if num_tabs > indention then
            for i in 1 .. num_tabs - indention loop
               Token_Lists.Append (lex.l, create_token (tok_type => INDENT_TOK,
                                                        lex      => to_lexeme("_INDENT_"),
                                                        row_num  => line_number,
                                                        col_num  => i));
               indention := num_tabs;
            end loop;
         elsif indention > num_tabs then
            for i in 1 .. indention - num_tabs loop
               Token_Lists.Append (lex.l, create_token (tok_type => DEDENT_TOK,
                                                        lex      => to_lexeme ("_DEDENT_"),
                                                        row_num  => line_number,
                                                        col_num  => i));
               indention := num_tabs;
            end loop;
         end if;
         process_line (lex, line (1 .. last), line_number, num_tabs);
         line_number := line_number + 1;
      end loop;
      for i in 1 .. indention loop
         Token_Lists.Append (lex.l, create_token (tok_type => DEDENT_TOK,
                                                  lex => to_lexeme ("_DEDENT_"),
                                                  row_num  => line_number,
                                                  col_num => 1));
      end loop;
      Token_Lists.Append (lex.l, create_token (tok_type => EOS_TOK,
                                               lex      => to_lexeme ("_EOS_"),
                                               row_num  => line_number,
                                               col_num => 1));
      close (f);
--        begin
--           declare
--              tok: Token;
--           begin
--              while more_tokens (lex) loop
--                 tok := get_lookahead_token(lex);
--                 get_next_token (lex, tok);
--                 put(String (get_lexeme(tok)));
--                 put (" ");
--                 put_line (Token_Type'Image (get_token_type(tok)));
--              end loop;
--           end;
--        end;

      return lex;
   end create_lexical_analyzer;

   -------------------------
   -- get_lookahead_token --
   -------------------------

   function get_lookahead_token (lex: in Lexical_Analyzer) return Token is
   begin
      return Token_Lists.First_Element (lex.l);
   end get_lookahead_token;

   --------------------
   -- get_next_token --
   --------------------

   procedure get_next_token (lex: in out Lexical_Analyzer; tok: out Token) is
   begin
      tok := Token_Lists.first_element (lex.l);
      Token_Lists.Delete_First (lex.l);
   end get_next_token;

end Lexical_Analyzers;
