with Mssyst.String;
use Mssyst.String;
with Parse_Tree;
use Parse_Tree;
with Parser;
with Lexer;
with Ada.Exceptions;
use Ada.Exceptions;
with raptor.Runtime;
-- for debugging
with Ada.Text_IO;
use ada.Text_IO;
with Suggestions;
with Raptor_Files;
with Numbers;
with Raptor.Plugins;
--with Raptor.Component;
with Raptor.ParseHelpers;

package body Interpreter is
   function Is_Plugin_Procedure(Name : in Mssyst.String.Ref) return boolean is
   begin
      return raptor.Plugins.Is_Procedure(name);
   end Is_Plugin_Procedure;

   function Plugin_Parameter_Count(Name : in Mssyst.String.Ref) return Natural is
   begin
      return raptor.Plugins.Parameter_Count(name);
   end Plugin_Parameter_Count;

   procedure Emit_Code(
         Tree : Parse_Tree.Parseable_Pointer;
         Text : in MSSyst.String.Ref;
         Gen  : in Generate_Interface.Generator) is
      Ada_Text : String := +Text;
   begin
      Lexer.Initialize(Text => Ada_Text);
      Parse_Tree.Emit_Code(This => Tree.all, Gen => Gen);
   end Emit_Code;
   procedure Compile_Pass1(
         Tree : Parse_Tree.Parseable_Pointer;
         Text : in MSSyst.String.Ref;
         Gen  : in Generate_Interface.Generator) is
      Ada_Text : String := +Text;
   begin
      Lexer.Initialize(Text => Ada_Text);
      Parse_Tree.Compile_Pass1(This => Tree.all, Gen => Gen);
   end Compile_Pass1;

   function Call_Syntax(Text : in MSSyst.String.Ref;
      component :mssyst.object.ref) return Syntax_Result is
      Ada_Text : String := +Text;
      Result : Syntax_Result;
   begin
      Lexer.Initialize(Text => Ada_Text);
      Parser.Set_Expression_Object(Component);

      Result.Tree := Parseable_Pointer(Parser.Parse_Call_Statement);
      Parser.Set_Expression_Object(Component);
      Result.Valid := True;
      return Result;
   exception
      when Constraint_Error =>
         Result.Valid := False;
         Result.Location := 1;
         Result.Message := +"Can not have empty string";
         return Result;
      when E:Parser.Syntax_Error =>
         Result.Valid := False;
         Result.Location := Parser.Get_Current_Token.Start;
         Result.Message := +Ada.Exceptions.Exception_Message(E);
         return Result;
      when E:Lexer.Bad_Token =>
         Result.Valid := False;
         Result.Location := Lexer.Get_Current_Location;
         Result.Message := +Ada.Exceptions.Exception_Message(E);
         return Result;
   end Call_Syntax;

   function Assignment_Syntax(Text : in MSSyst.String.Ref;
         Text2 : in MSSyst.String.Ref;
      component :mssyst.object.ref) return Syntax_Result is
      Lhs_Ada_Text : String := +Text;
      Ada_Text : String := +Text & ":=" & (+Text2);
      Result : Syntax_Result;
   begin

      Lexer.Initialize(Text => Ada_Text);
      Parser.Set_Expression_Object(Component);

      Result.Tree := Parseable_Pointer(Parser.Parse_Assignment_Statement);
      Result.Valid := True;
      return Result;
   exception
      when Constraint_Error =>
         Result.Valid := False;
         Result.Location := 1;
         Result.Message := +"Can not have empty string";
         return Result;
      when E:Parser.Syntax_Error =>
         Result.Valid := False;
         Result.Location := Parser.Get_Current_Token.Start;
         Result.Message := +Ada.Exceptions.Exception_Message(E);
         return Result;
      when E:Lexer.Bad_Token =>
         Result.Valid := False;
         Result.Location := Lexer.Get_Current_Location;
         Result.Message := +Ada.Exceptions.Exception_Message(E);
         return Result;
   end Assignment_Syntax;

   function Statement_Syntax(Text : in MSSyst.String.Ref;
         Is_Call_Box : Boolean;
      component :mssyst.object.ref) return Syntax_Result is
      Ada_Text : String := +Text;
      Result : Syntax_Result;
   begin
      Lexer.Initialize(Text => Ada_Text);
      Parser.Set_Expression_Object(Component);

      Result.Tree := Parseable_Pointer(Parser.Parse_Statement(Is_Call_Box));
      Result.Valid := True;
      return Result;
   exception
      when Constraint_Error =>
         Result.Valid := False;
         Result.Location := 1;
         Result.Message := +"Can not have empty string";
         return Result;
      when E:Parser.Syntax_Error =>
         Result.Valid := False;
         Result.Location := Parser.Get_Current_Token.Start;
         Result.Message := +Ada.Exceptions.Exception_Message(E);
         return Result;
      when E:Lexer.Bad_Token =>
         Result.Valid := False;
         Result.Location := Lexer.Get_Current_Location;
         Result.Message := +Ada.Exceptions.Exception_Message(E);
         return Result;
   end Statement_Syntax;

   function Conditional_Syntax(Text : in MSSyst.String.Ref;
      component :mssyst.object.ref) return Syntax_Result is
      Ada_Text : String := +Text;
      Result : Syntax_Result;
   begin
      Lexer.Initialize(Text => Ada_Text);
      Parser.Set_Expression_Object(Component);

      Result.Tree := Parseable_Pointer(Parser.Parse_Condition);
      Result.Valid := True;
      return Result;
   exception
      when Constraint_Error =>
         Result.Valid := False;
         Result.Location := 1;
         Result.Message := +"Can not have empty string";
         return Result;
      when E:Parser.Syntax_Error =>
         Result.Valid := False;
         Result.Location := Parser.Get_Current_Token.Start;
         Result.Message := +Ada.Exceptions.Exception_Message(E);
         return Result;
      when E:Lexer.Bad_Token =>
         Result.Valid := False;
         Result.Location := Lexer.Get_Current_Location;
         Result.Message := +Ada.Exceptions.Exception_Message(E);
         return Result;
   end Conditional_Syntax;

   function Input_Syntax(Text : in MSSyst.String.Ref;
      component :mssyst.object.ref) return Syntax_Result is
      Ada_Text : String := +Text;
      Result : Syntax_Result;
   begin
      Lexer.Initialize(Text => Ada_Text);
      Parser.Set_Expression_Object(Component);

      Result.Tree := Parseable_Pointer(Parser.Parse_Input_Statement);
      Result.Valid := True;
      return Result;
   exception
      when Constraint_Error =>
         Result.Valid := False;
         Result.Location := 1;
         Result.Message := +"Can not have empty string";
         return Result;
      when E:Parser.Syntax_Error =>
         Result.Valid := False;
         Result.Location := Parser.Get_Current_Token.Start;
         Result.Message := +Ada.Exceptions.Exception_Message(E);
         return Result;
      when E:Lexer.Bad_Token =>
         Result.Valid := False;
         Result.Location := Lexer.Get_Current_Location;
         Result.Message := +Ada.Exceptions.Exception_Message(E);
         return Result;
   end Input_Syntax;

   function Output_Syntax(Text : in MSSyst.String.Ref;
         New_Line : in Boolean;
      component :mssyst.object.ref) return Syntax_Result is
      Ada_Text : String := +Text;
      Result : Syntax_Result;
   begin
      Lexer.Initialize(Text => Ada_Text);
      Parser.Set_Expression_Object(Component);

      Result.Tree := Parseable_Pointer(Parser.Parse_Output_Statement(New_Line));
      Result.Valid := True;
      return Result;
   exception
      when Constraint_Error =>
         Result.Valid := False;
         Result.Location := 1;
         Result.Message := +"Can not have empty string";
         return Result;
      when E:Parser.Syntax_Error =>
         Result.Valid := False;
         Result.Location := Parser.Get_Current_Token.Start;
         Result.Message := +Ada.Exceptions.Exception_Message(E);
         return Result;
      when E:Lexer.Bad_Token =>
         Result.Valid := False;
         Result.Location := Lexer.Get_Current_Location;
         Result.Message := +Ada.Exceptions.Exception_Message(E);
         return Result;
   end Output_Syntax;

   procedure Run_Assignment(
      Tree : Parse_Tree.Parseable_Pointer;
      Text : in MSSyst.String.Ref) is
      Ada_Text : String := +Text;
   begin
      Lexer.Initialize(Text => Ada_Text);
      declare
         Stmt_Tree : Statement_Pointer := Statement_Pointer(Tree);
      begin
         Execute (Stmt_Tree.All);
      end;
   end Run_Assignment;

   function Run_Expon(
         Tree: Parse_Tree.Expon_Pointer;
         Text : in MSSyst.String.Ref;
         Component : in MSSyst.Object.Ref) return Boolean is
      Ada_Text : String := +Text;
      Value : Numbers.Value;
   BEGIN
      Raptor.Runtime.SetContext(null);
      Lexer.Initialize(Text => Ada_Text);
      Parse_Tree.Did_Method_Call := False;
      Parse_Tree.Did_Function_Call := False;
      Value := Execute(Tree.All);
      -- I'm sure I had a good reason for doing this, but I don't know
      -- what it is.
      -- See the code in Step_Helpers.Step_Once
      if not Parse_Tree.Did_Function_Call then
         Raptor.ParseHelpers.AddValue(Component,Value);
      end if;
      return Parse_Tree.Did_Method_Call;
   end Run_Expon;


   procedure Run_Input(
      Tree : Parse_Tree.Parseable_Pointer;
      Text : in MSSyst.String.Ref;
      Prompt : in MSSyst.String.Ref;
      Input_Expression : Boolean;
      Prompt_Tree : Parse_Tree.Parseable_Pointer) is
      Ada_Text : String := +Text;
      Ada_Prompt_Text : String := +Prompt;
   begin
      Lexer.Initialize(Text => Ada_Text);
      declare
         Stmt_Tree : Input_Pointer := Input_Pointer(Tree);
      begin
         if Input_Expression then
            Lexer.Initialize(Text => Ada_Prompt_Text);
            declare
               Prompt_Value : Numbers.Value := Execute(
                  Parse_Tree.Expr_Output(Prompt_Tree.all).Expr.all);
            begin
               Lexer.Initialize(Text => Ada_Text);
               if Numbers.Is_String(Prompt_Value) then
                  Execute (Stmt_Tree.All, Numbers.String_Of(Prompt_Value));
               else
                  Execute (Stmt_Tree.All, +Numbers.Number_String(Prompt_Value));
               end if;
            end;
         else
            Execute (Stmt_Tree.All, Prompt);
         end if;
      end;
   end Run_Input;

   procedure Run_Output(
      Tree : Parse_Tree.Parseable_Pointer;
      Text : in MSSyst.String.Ref) is
      Ada_Text : String := +Text;
   begin
      Lexer.Initialize(Text => Ada_Text);
      declare
         Stmt_Tree : Output_Pointer := Output_Pointer(Tree);
      begin
         Execute (Stmt_Tree.All);
      end;
   end Run_Output;

   function Run_Return_Value(
      Tree : Parse_Tree.Parseable_Pointer;
      Text : in MSSyst.String.Ref) return Numbers.Value is
      Ada_Text : String := +Text;
   begin
      Lexer.Initialize(Text => Ada_Text);
      declare
         Stmt_Tree : Output_Pointer := Output_Pointer(Tree);
      begin
         return Execute_Return_Value (Stmt_Tree.All);
      end;
   end Run_Return_Value;

   function Run_Boolean(
      Tree : Parse_Tree.Parseable_Pointer;
      Text : in MSSyst.String.Ref) return Boolean is
      Ada_Text : String := +Text;
   begin
      Lexer.Initialize(Text => Ada_Text);
      declare
         Stmt_Tree : Boolean_Expression_Pointer :=
            Boolean_Expression_Pointer(Tree);
      begin
         return Execute (Stmt_Tree.All);
      end;
   end Run_Boolean;

   function Get_Name(
      Tree : Parse_Tree.Assignment;
         Text : in MSSyst.String.Ref) return MSSyst.String.Ref is
      Ada_Text : String := +Text;
   begin
      Lexer.Initialize(Text => Ada_Text);
      return +Tree.Get_Name;
   end Get_Name;
   function Get_Name_Call(
      Tree : Parse_Tree.Procedure_Call;
      Text : in MSSyst.String.Ref) return MSSyst.String.Ref is
      Ada_Text : String := +Text;
   begin
      Lexer.Initialize(Text => Ada_Text);
      return +Tree.Get_Name;
   end Get_Name_Call;
   function Get_Name_Input(
      Tree : Parse_Tree.Input;
      Text : in MSSyst.String.Ref) return MSSyst.String.Ref is
      Ada_Text : String := +Text;
   begin
      Lexer.Initialize(Text => Ada_Text);
      return +Tree.Get_Name;
   end Get_Name_Input;
   function Suggestion(
      Text : in MSSyst.String.Ref;
      Kind : in Scenario) return Suggestion_Result is
   begin
      return Suggestions.Suggestion(Text,Kind);
   end Suggestion;


end Interpreter;
