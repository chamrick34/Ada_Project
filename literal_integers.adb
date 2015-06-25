package body Literal_Integers is

   ----------------------------
   -- create_literal_integer --
   ----------------------------

   function create_literal_integer  (value: in Integer) return Literal_Integer is
      li: Literal_Integer;
   begin
      li.value := value;
      return li;
   end create_literal_integer;

   --------------
   -- evaluate --
   --------------

   function evaluate (li: in Literal_Integer) return Integer is
   begin
      return li.value;
   end evaluate;

end Literal_Integers;
