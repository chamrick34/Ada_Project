package body Programs is

   --------------------
   -- create_program --
   --------------------

   function create_program (blk: in Statement_Block) return Program is
      prog: Program;

   begin
      prog.blk := blk;
      return prog;
   end create_program;

   -------------
   -- execute --
   -------------

   procedure execute (prog: in Program) is
   begin
      execute (prog.blk);
   end execute;

end Programs;
