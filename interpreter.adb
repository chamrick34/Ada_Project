with Parsers, Programs, Lexical_Analyzers, Ada.Text_IO, Ada.Exceptions;
use Parsers, Programs, Lexical_Analyzers, Ada.Text_IO, Ada.Exceptions;

procedure Interpreter is

   p: Parser;
   prog: Program;

begin
   p := create_parser ("test4.py");
   parse (p, prog);
   execute (prog);
exception
   when e: lexical_exception =>
      put_line (exception_message (e));
   when e: parser_exception =>
      put_line (exception_message (e));
--   when others =>
--         put_line ("unknown error occurred - terminating");
end Interpreter;

--  		if (blockList.size() == 0)
--  			throw new RuntimeException ("illegal if statement");
--  		if (exprList.size() + 1 != blockList.size())
--  			throw new RuntimeException ("illegal if statement");
--  		int i = 0;
--  		while (i < exprList.size() && !exprList.get(i).evaluate())
--  			i++;
--  		if (i < exprList.size())
--  			blockList.get(i).execute();
--  		else
--  			blockList.get(blockList.size() - 1).execute();
