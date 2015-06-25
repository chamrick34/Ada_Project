package Ids is

   type Id is private;

   function create_id (ch: in Character) return Id;

   function evaluate (var: in Id) return Integer;

   function get_char (var: in Id) return Character;

private
   type Id is
      record
         ch: Character;
      end record;

end Ids;
