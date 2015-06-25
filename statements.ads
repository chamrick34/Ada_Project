with Boolean_Expressions, Arithmetic_Expressions, Ids;
use Boolean_Expressions, Arithmetic_Expressions, Ids;
private with Ada.Containers.Doubly_Linked_Lists;
package Statements is

   invalid_if_statement: exception;

   type Statement_Type is (IF_STMT, ASSN_STMT, WHILE_STMT, PRINT_STMT);

   type Statement (stmt_type: Statement_Type) is private;

   type Statement_Access is access Statement;

   type Statement_List is private;

   type Statement_Block is private;

   function create_if_statement return Statement_Access;

   function create_assignment_statement (var: in Id; expr: in Expression_Access)
                                         return Statement_Access
     with pre => expr /= null;

   function create_print_statement (expr: in Expression_Access)
                                    return Statement_Access
     with pre => expr /= null;

   function create_while_statement (expr: in Boolean_Expression;
                                    blk: Statement_Block)
                                    return Statement_Access;

   procedure execute (stmt: in Statement_Access)
     with pre => stmt /= null;

   procedure add_expression (s: in out Statement_Access;
                             expr: in Boolean_Expression)
     with pre => s /= null and then s.stmt_type = IF_STMT;

   procedure add_block (s: in out Statement_Access; blk: in Statement_Block)
     with pre => s /= null and then s.stmt_type = IF_STMT;

   function create_statement_list return Statement_List;

   procedure execute (stmts: in Statement_List);

   procedure add (stmts: in out Statement_List; s: in Statement_Access)
     with pre => s /= null;

   function create_statement_block (stmts: in Statement_List)
                                    return Statement_Block;

   procedure execute (blk: in Statement_Block);

private

   package Statement_Lists is
     new Ada.Containers.Doubly_Linked_Lists (Statement_Access);

   use Statement_Lists;

   type Statement_List is
      record
         l: Statement_Lists.List;
      end record;

   type Statement_Block is
      record
         stmts: Statement_List;
      end record;

   package Boolean_Expression_Lists is
     new Ada.Containers.Doubly_Linked_Lists (Boolean_Expression);

   package Statement_Block_Lists is
     new Ada.Containers.Doubly_Linked_Lists (Statement_Block);

   type Statement (stmt_type: Statement_Type) is
      record
         case stmt_type is
            when IF_STMT =>
               expr_list: Boolean_Expression_Lists.List;
               blk_List: Statement_Block_Lists.List;
            when ASSN_STMT =>
               assn_var: Id;
               assn_expr: Expression_Access;
            when WHILE_STMT =>
               while_expr: Boolean_Expression;
               while_blk: Statement_Block;
            when PRINT_STMT =>
               print_expr: Expression_Access;
         end case;
      end record;


end Statements;
