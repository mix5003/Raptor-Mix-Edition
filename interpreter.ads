with Mssyst.String;
use Mssyst.String;
with Parse_Tree;
with mssyst.reflection.emit.ilgenerator;
-- this with is extraneous, but added to get token_helpers into the DLL
with token_helpers;
with Generate_Interface;
with mssyst.object;
with Numbers;

package Interpreter is
   type Syntax_Result is record
      Valid : Boolean;
      Message : access Mssyst.String.Typ;
      Location : Natural;
      Tree : Parse_Tree.Parseable_Pointer;
   end record;

   type Suggestion_Result is record
      Bold_Start : Natural;
      Bold_Finish : Integer; -- may be -1
      Suggestions : MSSyst.String.Ref_Array;
   end record;

   function Assignment_Syntax(Text : in MSSyst.String.Ref;
      Text2 : in MSSyst.String.Ref;
      component :mssyst.object.ref) return Syntax_Result;
   function Call_Syntax(Text : in MSSyst.String.Ref;
      component :mssyst.object.ref) return Syntax_Result;
   function Statement_Syntax(Text : in MSSyst.String.Ref;
      Is_Call_Box : in Boolean;
      component :mssyst.object.ref) return Syntax_Result;
   function Conditional_Syntax(Text : in MSSyst.String.Ref;
      component :mssyst.object.ref) return Syntax_Result;
   function Input_Syntax(Text : in MSSyst.String.Ref;
      component :mssyst.object.ref) return Syntax_Result;
   function Output_Syntax(Text : in MSSyst.String.Ref;
      New_Line : in Boolean;
      component :mssyst.object.ref) return Syntax_Result;

   procedure Run_Assignment(
      Tree : Parse_Tree.Parseable_Pointer;
      Text : in MSSyst.String.Ref);
   procedure Run_Input(
      Tree : Parse_Tree.Parseable_Pointer;
      Text : in MSSyst.String.Ref;
      Prompt : in MSSyst.String.Ref;
      Input_Expression : Boolean;
      Prompt_Tree : Parse_Tree.Parseable_Pointer);
   procedure Run_Output(
      Tree : Parse_Tree.Parseable_Pointer;
      Text : in MSSyst.String.Ref);
   function Run_Boolean(
      Tree : Parse_Tree.Parseable_Pointer;
      Text : in MSSyst.String.Ref) return Boolean;
   -- returns true if this is going to create a subchart call, false
   -- otherwise
   function Run_Expon(
      Tree: Parse_Tree.Expon_Pointer;
      Text : in MSSyst.String.Ref;
      Component : in MSSyst.Object.Ref) return Boolean;
   function Run_Return_Value(
      Tree : Parse_Tree.Parseable_Pointer;
      Text : in MSSyst.String.Ref) return Numbers.Value;
   subtype Scenario is Integer range 1..3;
   Call_Dialog : Scenario := 1;
   Expr_Dialog : Scenario := 2;
   Lhs_Dialog  : Scenario := 3;

   function Suggestion(
      Text : in MSSyst.String.Ref;
      Kind : in Scenario) return Suggestion_Result;

   function Get_Name(
      Tree : Parse_Tree.Assignment;
      Text : in MSSyst.String.Ref) return MSSyst.String.Ref;
   function Get_Name_Call(
      Tree : Parse_Tree.Procedure_Call;
      Text : in MSSyst.String.Ref) return MSSyst.String.Ref;

   procedure Emit_Code(
         Tree : Parse_Tree.Parseable_Pointer;
         Text : in MSSyst.String.Ref;
         Gen  : in Generate_Interface.Generator);
   procedure Compile_Pass1(
         Tree : Parse_Tree.Parseable_Pointer;
         Text : in MSSyst.String.Ref;
         Gen  : in Generate_Interface.Generator);
end Interpreter;
