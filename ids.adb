with Memory;
use Memory;

package body Ids is

   ---------------
   -- create_id --
   ---------------

   function create_id (ch: in Character) return Id is
      var: Id;
      begin
         var.ch := ch;
         return var;
   end create_id;

   --------------
   -- evaluate --
   --------------

   function evaluate (var: in Id) return Integer is
   begin
      return fetch (var);
   end evaluate;

   ---------------------------------------------------------------------

   function get_char (var: in Id) return Character is
   begin
      return var.ch;
   end get_char;

end Ids;
