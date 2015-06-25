with Statements;
use Statements;

package Programs is

   type Program is private;

   function create_program (blk: in Statement_Block) return Program;

   procedure execute (prog: in Program);

private
   type Program is
      record
         blk: Statement_Block;
      end record;

end Programs;
