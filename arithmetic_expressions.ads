with Literal_Integers, Ids;
use Literal_Integers, Ids;

package Arithmetic_Expressions is

   type Arithmetic_Operator is (ADD_OP, SUB_OP, MUL_OP, DIV_OP);

   type Expression_Type is (CONST_EXPR, VAR_EXPR, BINARY_EXPR);

   type Expression (expr_type: Expression_Type) is private;

   type Expression_Access is access Expression;

   function create_constant_expression (li: in Literal_Integer) return Expression_Access;

   function create_var_expression (var: Id) return Expression_Access;

   function create_binary_expression (op: in Arithmetic_Operator;
                                      expr1, expr2: in Expression_Access)
                                      return Expression_Access
     with pre => expr1 /= null and expr2 /= null;

   function evaluate (expr: Expression_Access) return Integer
     with pre => expr /= null;

private
   type Expression (expr_type: Expression_Type) is
      record
         case expr_type is
            when CONST_EXPR =>
               li: Literal_Integer;
            when VAR_EXPR =>
               var: Id;
            when BINARY_EXPR =>
               op: Arithmetic_Operator;
               expr1: Expression_Access;
               expr2: Expression_Access;
         end case;
      end record;

end Arithmetic_Expressions;
