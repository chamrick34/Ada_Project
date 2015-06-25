with Memory, Ada.Text_IO;
use Memory, Ada.Text_IO;

package body Statements is

   -------------------------
   -- create_if_statement --
   -------------------------

   function create_if_statement return Statement_Access is
      s: Statement_Access;

   begin
      s := new Statement (IF_STMT);
      return s;
   end create_if_statement;

   ---------------------------------
   -- create_assignment_statement --
   ---------------------------------

   function create_assignment_statement  (var: in Id;  expr: in Expression_Access)
                                          return Statement_Access is

      s: Statement_Access;

   begin
      s := new Statement (ASSN_STMT);
      s.assn_var := var;
      s.assn_expr := expr;
      return s;
   end create_assignment_statement;

   ----------------------------
   -- create_print_statement --
   ----------------------------

   function create_print_statement (expr: in Expression_Access) return Statement_Access is

      s: Statement_Access;

   begin
      s := new Statement (PRINT_STMT);
      s.print_expr := expr;
      return s;
   end create_print_statement;

   ----------------------------
   -- create_while_statement --
   ----------------------------

   function create_while_statement (expr: in Boolean_Expression; blk: Statement_Block)
                                    return Statement_Access is

      s: Statement_Access;

   begin
      s := new Statement (WHILE_STMT);
      s.while_expr := expr;
      s.while_blk := blk;
      return s;
   end create_while_statement;

   -------------
   -- execute --
   -------------

   procedure execute  (stmt: in Statement_Access)  is

      use Boolean_Expression_Lists;

   begin
      case stmt.stmt_type is
         when ASSN_STMT =>
            store (var => stmt.assn_var,  value => evaluate(stmt.assn_expr));
         when IF_STMT =>
            if Statement_Block_Lists.is_empty(stmt.blk_List) then
               raise invalid_if_statement;
            end if;
            if Boolean_Expression_Lists.Is_Empty(stmt.expr_list) then
               raise invalid_if_statement;
            end if;
            declare
               expr_cursor: Boolean_Expression_Lists.Cursor := Boolean_Expression_Lists.First(stmt.expr_list);
               blk_cursor: Statement_Block_Lists.Cursor := Statement_Block_Lists.first (stmt.blk_List);
            begin
               loop
                  exit when evaluate(Boolean_Expression_Lists.Element (expr_cursor));
                  Statement_Block_Lists.Next(blk_cursor);
                  exit when expr_cursor = Boolean_Expression_Lists.Last(stmt.expr_list);
                  Boolean_Expression_Lists.next (expr_cursor);
               end loop;
               execute (Statement_Block_Lists.Element (blk_cursor));
            end;
         when WHILE_STMT =>
            while evaluate (stmt.while_expr) loop
               execute (stmt.while_blk);
            end loop;
         when PRINT_STMT =>
            put (Integer'Image (evaluate (stmt.print_expr)));
      end case;
   end execute;

   --------------------
   -- add_expression --
   --------------------

   procedure add_expression  (s: in out Statement_Access; expr: in Boolean_Expression)  is

   begin
      Boolean_Expression_Lists.Append (s.expr_list, expr);
   end add_expression;

   ---------------
   -- add_block --
   ---------------

   procedure add_block (s: in out Statement_Access; blk: in Statement_Block) is

   begin
      Statement_Block_Lists.Append (s.blk_List, blk);
   end add_block;

   ---------------------------
   -- create_statement_list --
   ---------------------------

   function create_statement_list return Statement_List is
      stmts: Statement_List;
   begin
      return stmts;
   end create_statement_list;

   -------------
   -- execute --
   -------------

   procedure execute (stmts: in Statement_List) is
      current: Statement_Lists.Cursor;

   begin
      current := Statement_Lists.First (stmts.l);
      loop
         execute (Statement_Lists.Element (current));
         exit when current = Statement_Lists.Last (stmts.l);
         Statement_Lists.Next(current);
      end loop;

   end execute;

   ---------
   -- add --
   ---------

   procedure add  (stmts: in out Statement_List;  s: in Statement_Access) is
   begin
      Statement_Lists.Append (stmts.l, s);
   end add;

   ----------------------------
   -- create_statement_block --
   ----------------------------

   function create_statement_block (stmts: in Statement_List) return Statement_Block is
      blk: Statement_Block;

   begin
      blk.stmts := stmts;
      return blk;
   end create_statement_block;

   -------------
   -- execute --
   -------------

   procedure execute (blk: in Statement_Block) is
   begin
      execute (blk.stmts);
   end execute;

end Statements;
