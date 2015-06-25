package body Arithmetic_Expressions is

   --------------------------------
   -- create_constant_expression --
   --------------------------------

   function create_constant_expression  (li: in Literal_Integer)  return Expression_Access is

      expr: Expression_Access;

   begin
      expr := new Expression (CONST_EXPR);
      expr.li := li;
      return expr;
   end create_constant_expression;

   ---------------------------
   -- create_var_expression --
   ---------------------------

   function create_var_expression (var: Id) return Expression_Access is
      expr: Expression_Access;

   begin
      expr := new Expression (VAR_EXPR);
      expr.var := var;
      return expr;
   end create_var_expression;

   ------------------------------
   -- create_binary_expression --
   ------------------------------

   function create_binary_expression  (op: in Arithmetic_Operator;
                                       expr1, expr2: in Expression_Access)
                                       return Expression_Access is
      expr: Expression_Access;
   begin
      expr := new Expression (BINARY_EXPR);
      expr.op := op;
      expr.expr1 := expr1;
      expr.expr2 := expr2;
      return expr;
   end create_binary_expression;

   --------------
   -- evaluate --
   --------------

   function evaluate  (expr: Expression_Access)  return Integer is
      value: Integer;
   begin
      case expr.expr_type is
         when CONST_EXPR =>
            value := evaluate (expr.li);
         when VAR_EXPR =>
            value := evaluate (expr.var);
         when BINARY_EXPR =>
            case expr.op is
               when ADD_OP =>
                  value := evaluate (expr.expr1) + evaluate (expr.expr2);
               when SUB_OP =>
                  value := evaluate (expr.expr1) - evaluate (expr.expr2);
               when MUL_OP =>
                  value := evaluate (expr.expr1) * evaluate (expr.expr2);
               when DIV_OP =>
                  value := evaluate (expr.expr1) / evaluate (expr.expr2);
            end case;
      end case;
      return value;
   end evaluate;

end Arithmetic_Expressions;
