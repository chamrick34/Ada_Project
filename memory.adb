with Ada.Characters.Handling;
use Ada.Characters.Handling;

package body Memory is

   type Memory_Block is array (Character range <>) of Integer;
   mem1: Memory_Block ('a' .. 'z') := (others => 0);
   mem2: Memory_Block ('A' .. 'Z') := (others => 0);
   -----------
   -- fetch --
   -----------

   function fetch (var: in Id) return Integer is
      value: Integer;
      ch: Character;
   begin
      ch := get_char (var);
      if Is_Upper (ch) then
         value := mem2(ch);
      else
         value := mem1(ch);
      end if;
      return value;
   end fetch;

   -----------
   -- store --
   -----------

   procedure store (var: in Id; value: in Integer) is
      ch: Character;
   begin
      ch := get_char (var);
      if is_upper (ch) then
         mem2(ch) := value;
      else
         mem1(ch) := value;
      end if;
   end store;

end Memory;
