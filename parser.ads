with Parse_Tree;
use Parse_Tree;
with Lexer;
with mssyst.object;
package Parser is
   Syntax_Error : exception;
   procedure Raise_Exception(T : Lexer.Token_Pointer;
      Message : String);
   -- in case of error, what token were we looking at?
   procedure Set_Expression_Object(o : in mssyst.object.ref);
   function Get_Current_Token return Lexer.Token_Pointer;
   -- Statement => (Proc_Call | Assignment) [;] End_Input
   -- Is_Call_Box means that a Subchart is appropriate here
   function Parse_Statement(Is_Call_Box : Boolean) return Statement_Pointer;
   -- Assignment_Statement => Assignment [;] End_Input
   function Parse_Assignment_Statement return Statement_Pointer;
   -- Call_Statement => Proc_Call [;] End_Input
   function Parse_Call_Statement return Statement_Pointer;
   -- Proc_Call => proc_id(Parameter_List)
   function Parse_Proc_Call return Proc_Call_Pointer;
   -- Plugin_Proc_Call => plugin_proc_id(Parameter_List)
   function Parse_Plugin_Proc_Call return Plugin_Proc_Call_Pointer;
   -- Lhs => id[\[Expression[,Expression]\]]
   -- Lsuffix => . Lhs Lsuffix | lambda
   -- Assignment => Lhs LSuffix [=|:=] Expression
   function Parse_Assignment return Assignment_Pointer;
   -- Rhs => id[\[Expression[,Expression]\]] | id(Expression_List)
   -- Rsuffix => . Rhs Rsuffix | lambda
   -- Expon => Rhs RSuffix | num | (Expression)
   --          | new id | new id(Expression_List)
   --          | func_id(Expression_List) | func_id0 | -Expon
   --          | plugin_func_id(Expression_List)
   function Parse_Expon return Expon_Pointer;
   -- Mult => Expon ^ Expon | Expon
   function Parse_Mult return Mult_Pointer;
   -- Add => Mult * Add | Mult / Add | Mult mod Add | Add
   function Parse_Add return Add_Pointer;
   -- Expression => Add + Expression | Add - Expression | Add
   function Parse_Expression return Expression_Pointer;
   -- Relation => Expression > Expression | >=,<,<=,=
   function Parse_Relation return Relation_Pointer;
   -- Boolean2 => [ (BE) | Relation | Boolean_Func] [and BE2 | lambda]
   function Parse_Boolean2 return Boolean2_Pointer;
   -- Boolean_Expression => Boolean2 or Boolean_Expression | Boolean2
   function Parse_Boolean_Expression return Boolean_Expression_Pointer;
   -- Condition => Boolean_Expression End_Input
   function Parse_Condition return Boolean_Expression_Pointer;
   -- Output_Statement => Output End_Input
   function Parse_Output_Statement(New_Line : in Boolean) return Output_Pointer;
   -- Output => Expression|String
   function Parse_Output(New_Line : in Boolean) return Output_Pointer;
   -- Input => Input End_Input
   function Parse_Input_Statement return Input_Pointer;
   -- Input => Lhs LSuffix
   function Parse_Input return Input_Pointer;
   -- Parameter_List => Output [, Parameter_List | Lambda]
   function Parse_Parameter_List(
      Subprogram_Name   : in String;
      Allow_String_Args : in Boolean := False) return Parameter_List_Pointer;
   function Count_Parameters(L : in Parameter_List_Pointer) return Natural;
end Parser;
