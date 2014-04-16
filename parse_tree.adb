with raptor.Runtime;
with raptor.Plugins;
with Numbers;
use Numbers;
with Lexer;
with MSSyst.String;
use MSSyst.String;
with Ada.Exceptions;
use Ada.Exceptions;
with Parser;
with Ada.Long_Float_Text_IO;
use Ada.Long_Float_Text_IO;
with Ada.Strings.Fixed;
use Ada.Strings.Fixed;
with Ada.Characters.Handling;
use Ada.Characters.Handling;
with Dotnetgraphlibrary.Dotnetgraph;
with Dotnetgraphlibrary.Mouse_Button;
with Dotnetgraphlibrary.Color_Type;
with MSSyst.Random;
with Ada.Unchecked_Conversion;
with Raptor_Files;
with Raptor.Step_Helpers;
with Ada.Text_Io;
use Ada.Text_Io;
with MSSyst.Object;
with Ada.Numerics;
with Raptor.Generate_IL;
with Raptor.Compile_Helpers;
with Generate_Interface;
-- even though this isn't used, need to with it here
-- to get it into the dll
with Ada_Runtime;
with Ada.Characters.Conversions;
with Mssyst.Diagnostics.Debug;
with Raptor.ParseHelpers;
with Generate_Interface_Oo;
with Ada.Tags;

package body Parse_Tree is
   function To_Gen_Oo is new Ada.Unchecked_Conversion(Generate_Interface.Generator,
      generate_interface_oo.generator);
   type Conversions is (To_Integer, To_Float, To_String, Number_To_String, To_Bool, To_Color,
      Char_To_Int, Int_To_Char);
   type Context_Type is (Assign_Context, Call_Context, Input_Context);
   type Variable_Kind is (Variable, Array_1D, Array_2D);
   Emit_Context : Context_Type := Assign_Context;
   Emit_Kind : Variable_Kind := Variable;
   Current_Prompt : access MSSyst.String.Typ;
   procedure Set_Prompt(S : in MSSyst.String.Ref) is
   begin
      Current_Prompt := S;
   end Set_Prompt;

   function Check_Parameter_Number (This : in Parse_Tree.Output_Pointer)
         return Value is
   begin
      if This.All in String_Output'Class then
         declare
            S : Token_Pointer := String_Output(This.All).Str;
         begin
            Raise_Exception(Runtime_Error'Identity,
               Get_Text(S.Start..S.Finish) & " is not a valid parameter");
         end;

      end if;
      return Execute (Expr_Output(This.all).Expr.all);
   end Check_Parameter_Number;

   procedure Emit_Parameter_Number (This : in Parse_Tree.Output_Pointer; Gen : access Generate_Interface.Typ'Class) is
   begin
      if This.All in String_Output'Class then
         declare
            S : Token_Pointer := String_Output(This.All).Str;
         begin
            Raise_Exception(Runtime_Error'Identity,
               Get_Text(S.Start..S.Finish) & " is not a valid parameter");
         end;

      end if;
      Emit_Code (Expr_Output(This.all).Expr.all,Gen);
   end Emit_Parameter_Number;

   -- replace \" or "" with "
   -- replace \\ with \
   function Dequote(S : in String) return String is
      T : String := S;
      I : Natural := S'First;
      J : Natural := T'First-1;
   begin
      while I<=S'Last loop
         J := J + 1;
         if S(I)='\' then
            if I<S'Last and then (S(I+1)='"' or S(I+1)='\') then
               T(J) := S(I+1);
               I := I + 1;
            else
               T(J) := S(I);
            end if;
         elsif S(I) = '"' then
            if I<S'Last and then S(I+1)='"' then
               T(J) := S(I+1);
               I := I + 1;
            else
               T(J) := S(I);
            end if;
         else
            T(J) := S(I);
         end if;
         I := I + 1;
      end loop;
      return T(T'First..J);
   end Dequote;

   function Check_Parameter_String (This : in Parse_Tree.Output_Pointer)
         return String is
   begin
      if This.All in String_Output'Class then
         declare
            S : Token_Pointer := String_Output(This.All).Str;
         begin
            return Dequote(Get_Text(S.Start+1..S.Finish-1));
         end;
      end if;
      declare
         V : Value;
         S : String(1..600);
      begin
         V := Execute (Expr_Output(This.All).Expr.All);
         if Is_String(V) then
               return +String_Of(V);
         else
               Put(
                  Item => Long_Float_Of(V),
                  Aft  => 4,
                  To   => S);
               Raise_Exception(Runtime_Error'Identity,
                  S(Index_Non_Blank(S)..S'Last)
                  & " is not a valid parameter");
         end if;
      end;
   end Check_Parameter_String;
   procedure Emit_Parameter_String (This : in Parse_Tree.Output_Pointer; Gen : access Generate_Interface.Typ'Class) is
   begin
      if This.All in String_Output'Class then
         declare
            S : Token_Pointer := String_Output(This.All).Str;
         begin
            Gen.Emit_Load_String(
               Dequote(Get_Text(S.Start+1..S.Finish-1)));
         end;
      end if;
   end Emit_Parameter_String;


   function Length_Of (This : in Output_Pointer) return Natural is
      Expr1 : Expression_Pointer;
      Add1  : Value_Parseable_Pointer;
      Mult1 : Value_Parseable_Pointer;
      Expon1 : Value_Parseable_Pointer;
      T : Lexer.Token_Pointer;
      use type ada.tags.tag;
   begin
      Expr1 := Expr_Output(This.All).Expr;
      if Expr1.All in Add_Expression'Class or
            Expr1.All in Minus_Expression'Class then
         declare
            V : Numbers.Value;
            S : access MSSyst.String.Typ;
         begin
            V := Parse_Tree.Execute(This => Expr1.All);
            if Numbers.Is_String(V) then
               return MSSyst.String.get_Length(Numbers.String_Of(V));
            end if;
         end;
         Raise_Exception(Runtime_Error'Identity,"Can only take length of string or 1D array.");
      end if;
      Add1 := Expr1.Left;
      if Add1.All in Div_Add'Class or
            Add1.All in Mod_Add'Class or
            Add1.All in Rem_Add'Class or
            Add1.All in Mult_Add'Class then
         Raise_Exception(Runtime_Error'Identity,"Can only take length of string or 1D array.");
      end if;
      Mult1 := Add_Pointer(Add1).Left;
      if Mult1.All in Expon_Mult'Class then
         Raise_Exception(Runtime_Error'Identity,"Can only take length of string or 1D array.");
      end if;
      Expon1 := Mult_Pointer(Mult1).Left;
      if Expon1.All in String_Expon'Class then
         T := String_Expon(Expon1.all).S;
         return T.Finish-T.Start+1-2;
      end if;
      if not(Expon1.All in Rhs_Expon'Class) then
         Raise_Exception(Runtime_Error'Identity,"Can only take length of string or 1D array.");
      end if;
      if not (Rhs_Expon(Expon1.All).RSuffix.all in Empty_Rsuffix'Class) then
         Raise_Exception(Runtime_Error'Identity,"Can only take length of local variable.");
      end if;

      if not(Rhs_Expon(Expon1.All).Rhs'Tag = Id_Rhs'Tag) then
         Raise_Exception(Runtime_Error'Identity,"Can only take length of string or 1D array.");
      end if;
      T := Id_Rhs(Rhs_Expon(Expon1.All).Rhs.all).Id;
      return raptor.Runtime.getArraySize(
         +To_Upper(Get_Text(T.Start..T.Finish)));
   end Length_Of;

   procedure Assign_To (
         This   : in Output_Pointer;
         Value  : in Numbers.Value;
         Prefix : in String) is
      Expr1 : Expression_Pointer;
      Add1  : Value_Parseable_Pointer;
      Mult1 : Value_Parseable_Pointer;
      Expon1 : Value_Parseable_Pointer;
      T : Lexer.Token_Pointer;
      M_Str : access MSSyst.String.Typ;
      V,V2 : Numbers.Value;
      use type Ada.Tags.Tag;
      Rhs1 : Rhs_Pointer;
   begin
      Expr1 := Expr_Output(This.All).Expr;
      if Expr1.All in Add_Expression'Class or
            Expr1.All in Minus_Expression'Class then
         Raise_Exception(Runtime_Error'Identity,"Can't have arithmetic operator here.");
      end if;
      Add1 := Expr1.Left;
      if Add1.All in Div_Add'Class or
            Add1.All in Mod_Add'Class or
            Add1.All in Rem_Add'Class or
            Add1.All in Mult_Add'Class then
         Raise_Exception(Runtime_Error'Identity,"Can't have arithmetic operator here.");
      end if;
      Mult1 := Add_Pointer(Add1).Left;
      if Mult1.All in Expon_Mult'Class then
         Raise_Exception(Runtime_Error'Identity,"Can't have arithmetic operator here.");
      end if;
      Expon1 := Mult_Pointer(Mult1).Left;
      if not(Expon1.All in Rhs_Expon'Class) then
         Raise_Exception(Runtime_Error'Identity,"Can only use local variable.");
      end if;
      if not (Rhs_Expon(Expon1.All).RSuffix.all in Empty_Rsuffix'Class) then
         Raise_Exception(Runtime_Error'Identity,"Can only assign to local variable.");
      end if;
      if not(Rhs_Expon(Expon1.All).Rhs.all in Id_Rhs'Class) then
         Raise_Exception(Runtime_Error'Identity,"Can only take length of string or 1D array.");
      end if;
      Rhs1 := Rhs_Expon(Expon1.All).Rhs;
      -- either an array ref or a variable
      if Rhs1.All in Array_2D_Ref_Rhs'Class then
         T := Array_2D_Ref_Rhs(Rhs1.all).Id;
         M_Str := +To_Upper(Get_Text(T.Start..T.Finish));
         V := Execute (Array_2D_Ref_Rhs(Rhs1.all).Reference.All);
         V2 := Execute (Array_2D_Ref_Rhs(Rhs1.all).Reference2.All);
         if not Is_Integer(V) then
            Raise_Exception(Runtime_Error'Identity,
               Long_Float_Full_Image(Long_Float_Of(V)) & " not a valid array location--must be integer");
         end if;
         if not Is_Integer(V2) then
            Raise_Exception(Runtime_Error'Identity,
               Long_Float_Full_Image(Long_Float_Of(V2)) & " not a valid array location--must be integer");
         end if;
         raptor.Runtime.Set2DArrayElement(
            m_str,
            Integer_Of(V),
            Integer_Of(V2),
            Value);
      elsif Rhs1.All in Array_Ref_Rhs'Class then
         T := Array_Ref_Rhs(Rhs1.all).Id;
         m_str := +To_Upper(get_text(t.start..t.finish));
         V := Execute (Array_Ref_Rhs(Rhs1.all).Reference.All);
         if not Is_Integer(V) then
            Raise_Exception(Runtime_Error'Identity,
               Long_Float_Full_Image(Long_Float_Of(V)) & " not a valid array location--must be integer");
         end if;
         raptor.Runtime.SetArrayElement(
            m_str,
            Integer_Of(V),
            Value);
      elsif Rhs1.All'Tag = Id_Rhs'Tag then
         T := Id_Rhs(Rhs1.all).Id;
         raptor.Runtime.SetVariable(
            +To_Upper(Get_Text(T.Start..T.Finish)),
            Value);
      else
          Raise_Exception(Runtime_Error'Identity,"Can only assign to local variable.");
      end if;

   exception
      when e:Runtime_Error =>
         Raise_Exception(Runtime_Error'Identity,
            Prefix & " " & Ada.Exceptions.Exception_Message(e));
      when others =>
         Raise_Exception(Runtime_Error'Identity,
            Prefix & " is not a variable");
   end Assign_To;

--   procedure Assign_To (
--         This   : in Output_Pointer;
--         Value  : in String;
--         Prefix : in String) is
--      Expr1 : Expression_Pointer;
--      Add1  : Value_Parseable_Pointer;
--      Mult1 : Value_Parseable_Pointer;
--      Expon1 : Value_Parseable_Pointer;
--      T : Lexer.Token_Pointer;
--      M_Str : access MSSyst.String.Typ;
--      V,V2 : Numbers.Value;
--   begin
--      Expr1 := Expr_Output(This.All).Expr;
--      if Expr1.All in Add_Expression'Class or
--            Expr1.All in Minus_Expression'Class then
--         Raise_Exception(Runtime_Error'Identity,"Can't have arithmetic operator here.");
--      end if;
--      Add1 := Expr1.Left;
--      if Add1.All in Div_Add'Class or
--            Add1.All in Mod_Add'Class or
--            Add1.All in Rem_Add'Class or
--            Add1.All in Mult_Add'Class then
--         Raise_Exception(Runtime_Error'Identity,"Can't have arithmetic operator here.");
--      end if;
--      Mult1 := Add_Pointer(Add1).Left;
--      if Mult1.All in Expon_Mult'Class then
--         Raise_Exception(Runtime_Error'Identity,"Can't have arithmetic operator here.");
--      end if;
--      Expon1 := Mult_Pointer(Mult1).Left;
--      if not(Expon1.All in Id_Expon'Class) then
--         Raise_Exception(Runtime_Error'Identity,"Can't have arithmetic operator here.");
--      end if;
--      if Expon1.All in Func0_Expon'Class or
--            Expon1.All in Func_Expon'Class or
--            Expon1.All in Plugin_Func_Expon'Class then
--         Raise_Exception(Runtime_Error'Identity,"Can't have function here.");
--      end if;
--         -- either an array ref or a variable
--      if Expon1.All in Array_2D_Ref_Expon'Class then
--         Raise_Exception(Runtime_Error'Identity,
--             Value & " is not a number");
--      elsif Expon1.All in Array_Ref_Expon'Class then
--         Raise_Exception(Runtime_Error'Identity,
--             Value & " is not a number");
--      else
--         T := Id_Expon(Expon1.all).Id;
--         raptor.Runtime.SetVariable(
--            +To_Upper(Get_Text(T.Start..T.Finish)),
--            +Value);
--      end if;
--
--   exception
--      when e:Runtime_Error =>
--         Raise_Exception(Runtime_Error'Identity,
--            Prefix & " " & Ada.Exceptions.Exception_Message(e));
--      when others =>
--         Raise_Exception(Runtime_Error'Identity,
--            Prefix & " is not a variable");
--   end Assign_To;

   procedure MS_Assign_To (
         This   : in Output_Pointer;
         Value  : in Numbers.Value;
         Prefix : in MSSyst.String.Ref) is
      Ada_Text : String := +Prefix;
   begin
      Assign_To(This,Value,Ada_Text);
   end MS_Assign_To;

   -------------
   -- Execute --
   -------------

   procedure Execute (This : in Proc_Call) is
      Kind : Lexer.Proc_Token_Types := This.Id.Kind;
      function Convert is new Ada.Unchecked_Conversion(
         integer,Dotnetgraphlibrary.Color_Type.ValueType);
      function Convert is new Ada.Unchecked_Conversion(
         integer,Dotnetgraphlibrary.Mouse_Button.ValueType);
   begin
      case Kind is
         when Lexer.Delay_For =>
            delay Duration(Numbers.Long_Float_Of(
                  Check_Parameter_Number (This.Param_List.Parameter)));
         when Lexer.Set_Precision =>
            Numbers.Set_Precision(Numbers.Integer_Of(
               Check_Parameter_Number(This.Param_List.Parameter)));
         when Lexer.Redirect_Input =>
            if This.Param_List.Parameter.All in String_Output'Class then
               Raptor.Runtime.Redirect_Input(
                  Check_Parameter_String (This.Param_List.Parameter));
            else
               declare
                  q : integer;
               begin
                  q := Numbers.Integer_Of(
                     Check_Parameter_Number (This.Param_List.Parameter));
                  Raptor.Runtime.Redirect_Input(q);

               exception
                  when e:others =>
                     Raptor.Runtime.Redirect_Input(
                        Check_Parameter_String (This.Param_List.Parameter));
               end;
            end if;
         when Lexer.Redirect_Output_Append =>
            if This.Param_List.Parameter.All in String_Output'Class then
               Raptor.Runtime.Redirect_Output(
                  Check_Parameter_String (This.Param_List.Parameter));
            else
               begin
                  Raptor.Runtime.Redirect_Output_Append(
                     Numbers.Integer_Of(
                        Check_Parameter_Number (This.Param_List.Parameter)));
               exception
                  when others =>
                     Raptor.Runtime.Redirect_Output_Append(
                        Check_Parameter_String (This.Param_List.Parameter));
               end;
            end if;
         when Lexer.Redirect_Output =>
            if This.Param_List.Parameter.All in String_Output'Class then
               Raptor.Runtime.Redirect_Output(
                  Check_Parameter_String (This.Param_List.Parameter));
            else
               begin
                  Raptor.Runtime.Redirect_Output(
                     Numbers.Integer_Of(
                        Check_Parameter_Number (This.Param_List.Parameter)));
               exception
                  when others =>
                     Raptor.Runtime.Redirect_Output(
                        Check_Parameter_String (This.Param_List.Parameter));
               end;
            end if;
         when Lexer.Clear_Console =>
            Raptor.Runtime.ConsoleClear;
         when Lexer.Draw_Bitmap =>
               Dotnetgraphlibrary.Dotnetgraph.Draw_Bitmap(
                  Numbers.Integer_Of(
                     Check_Parameter_Number (This.Param_List.Parameter)),
                  Numbers.Integer_Of(
                     Check_Parameter_Number (This.Param_List.Next.Parameter)),
                  Numbers.Integer_Of(
                     Check_Parameter_Number (This.Param_List.Next.Next.Parameter)),
                  Numbers.Integer_Of(
                        Check_Parameter_Number (
                          This.Param_List.Next.Next.Next.Parameter)),
                  Numbers.Integer_Of(
                        Check_Parameter_Number (
                          This.Param_List.Next.Next.Next.Next.Parameter)));
         when Lexer.Clear_Window =>
            Dotnetgraphlibrary.Dotnetgraph.Clear_Window(
               --Dotnetgraphlibrary.Color_Type.ValueType'Val(
               convert(
               Numbers.Integer_Of(
                  Check_Parameter_Number (This.Param_List.Parameter))));
         when Lexer.Close_Graph_Window =>
            Dotnetgraphlibrary.Dotnetgraph.Close_Graph_Window;
         when Lexer.Freeze_Graph_Window =>
            Dotnetgraphlibrary.Dotnetgraph.FreezeGraphWindow;
         when Lexer.Unfreeze_Graph_Window =>
            Dotnetgraphlibrary.Dotnetgraph.UnfreezeGraphWindow;
         when Lexer.Update_Graph_Window =>
            Dotnetgraphlibrary.Dotnetgraph.UpdateGraphWindow;
         when Lexer.Set_Font_Size =>
            Dotnetgraphlibrary.Dotnetgraph.SetFontSize(
               Numbers.Integer_Of(
                  Check_Parameter_Number (This.Param_List.Parameter)));
         when Lexer.Display_Number =>
            if Numbers.Is_Integer(Check_Parameter_Number (This.Param_List.Next.Next.Parameter)) then
               Dotnetgraphlibrary.Dotnetgraph.Display_Text(
                  Numbers.Integer_Of(
                     Check_Parameter_Number (This.Param_List.Parameter)),
                  Numbers.Integer_Of(
                     Check_Parameter_Number (This.Param_List.Next.Parameter)),
                  Integer_Image(Numbers.Integer_Of(
                     Check_Parameter_Number (This.Param_List.Next.Next.Parameter))),
--                  Dotnetgraphlibrary.Color_Type.ValueType'Val(
                     Convert(
                     Numbers.Integer_Of(
                        Check_Parameter_Number (
                           This.Param_List.Next.Next.Next.Parameter))));
            else
               Dotnetgraphlibrary.Dotnetgraph.Display_Text(
                  Numbers.Integer_Of(
                     Check_Parameter_Number (This.Param_List.Parameter)),
                  Numbers.Integer_Of(
                     Check_Parameter_Number (This.Param_List.Next.Parameter)),
                  Long_Float_Image(Numbers.Long_Float_Of(
                        Check_Parameter_Number (This.Param_List.Next.Next.Parameter))),
                  Convert(
--                  Dotnetgraphlibrary.Color_Type.ValueType'Val(
                     Numbers.Integer_Of(
                        Check_Parameter_Number (
                           This.Param_List.Next.Next.Next.Parameter))));
            end if;
         when Lexer.Display_Text =>
            Dotnetgraphlibrary.Dotnetgraph.Display_Text(
               Numbers.Integer_Of(
                  Check_Parameter_Number (This.Param_List.Parameter)),
               Numbers.Integer_Of(
                  Check_Parameter_Number (This.Param_List.Next.Parameter)),
               Check_Parameter_String (This.Param_List.Next.Next.Parameter),
               Convert(
--               Dotnetgraphlibrary.Color_Type.ValueType'Val(
                  Numbers.Integer_Of(
                     Check_Parameter_Number (
                        This.Param_List.Next.Next.Next.Parameter))));
         when Lexer.Draw_Arc =>
            Dotnetgraphlibrary.Dotnetgraph.Draw_Arc(
               Numbers.Integer_Of(
                  Check_Parameter_Number (This.Param_List.Parameter)),
               Numbers.Integer_Of(
                  Check_Parameter_Number (This.Param_List.Next.Parameter)),
               Numbers.Integer_Of(
                  Check_Parameter_Number (
                     This.Param_List.Next.Next.Parameter)),
               Numbers.Integer_Of(
                  Check_Parameter_Number (
                     This.Param_List.Next.Next.Next.Parameter)),
               Numbers.Integer_Of(
                  Check_Parameter_Number (
                     This.Param_List.Next.Next.Next.Next.Parameter)),
               Numbers.Integer_Of(
                  Check_Parameter_Number (
                     This.Param_List.Next.Next.Next.Next.Next.Parameter)),
               Numbers.Integer_Of(
                  Check_Parameter_Number (
                     This.Param_List.Next.Next.Next.Next.Next.Next.Parameter)),
               Numbers.Integer_Of(
                  Check_Parameter_Number (
                     This.Param_List.Next.Next.Next.Next.Next.Next.Next.Parameter)),
               Convert(
               --Dotnetgraphlibrary.Color_Type.ValueType'Val(
                  Numbers.Integer_Of(
                     Check_Parameter_Number (
                        This.Param_List.Next.Next.Next.Next.
                           Next.Next.Next.Next.Parameter))));
         when Lexer.Draw_Box =>
            Dotnetgraphlibrary.Dotnetgraph.Draw_Box(
               Numbers.Integer_Of(
                  Check_Parameter_Number (This.Param_List.Parameter)),
               Numbers.Integer_Of(
                  Check_Parameter_Number (This.Param_List.Next.Parameter)),
               Numbers.Integer_Of(
                  Check_Parameter_Number (This.Param_List.Next.Next.Parameter)),
               Numbers.Integer_Of(
                  Check_Parameter_Number (This.Param_List.Next.Next.Next.Parameter)),
               Convert(
               --Dotnetgraphlibrary.Color_Type.ValueType'Val(
                  Numbers.Integer_Of(
                     Check_Parameter_Number (
                        This.Param_List.Next.Next.Next.Next.Parameter))),
               Boolean'Val(
                  Numbers.Integer_Of(
                     Check_Parameter_Number (
                        This.Param_List.Next.Next.Next.Next.Next.Parameter))));
         when Lexer.Draw_Circle =>
            Dotnetgraphlibrary.Dotnetgraph.Draw_Circle(
               Numbers.Integer_Of(
                  Check_Parameter_Number (This.Param_List.Parameter)),
               Numbers.Integer_Of(
                  Check_Parameter_Number (This.Param_List.Next.Parameter)),
               Numbers.Integer_Of(
                  Check_Parameter_Number (This.Param_List.Next.Next.Parameter)),
               Convert(
               --Dotnetgraphlibrary.Color_Type.ValueType'Val(
                  Numbers.Integer_Of(
                     Check_Parameter_Number (
                        This.Param_List.Next.Next.Next.Parameter))),
               Boolean'Val(
                  Numbers.Integer_Of(
                     Check_Parameter_Number (
                        This.Param_List.Next.Next.Next.Next.Parameter))));
         when Lexer.Draw_Ellipse =>
            Dotnetgraphlibrary.Dotnetgraph.Draw_Ellipse(
               Numbers.Integer_Of(
                  Check_Parameter_Number (This.Param_List.Parameter)),
               Numbers.Integer_Of(
                  Check_Parameter_Number (This.Param_List.Next.Parameter)),
               Numbers.Integer_Of(
                  Check_Parameter_Number (This.Param_List.Next.Next.Parameter)),
               Numbers.Integer_Of(
                  Check_Parameter_Number (This.Param_List.Next.Next.Next.Parameter)),
               Convert(
               --Dotnetgraphlibrary.Color_Type.ValueType'Val(
                  Numbers.Integer_Of(
                     Check_Parameter_Number (
                        This.Param_List.Next.Next.Next.Next.Parameter))),
               Boolean'Val(
                  Numbers.Integer_Of(
                     Check_Parameter_Number (
                        This.Param_List.Next.Next.Next.Next.Next.Parameter))));
         when Lexer.Draw_Ellipse_Rotate =>
            Dotnetgraphlibrary.Dotnetgraph.Draw_Ellipse_Rotate(
               Numbers.Integer_Of(
                  Check_Parameter_Number (This.Param_List.Parameter)),
               Numbers.Integer_Of(
                  Check_Parameter_Number (This.Param_List.Next.Parameter)),
               Numbers.Integer_Of(
                  Check_Parameter_Number (This.Param_List.Next.Next.Parameter)),
               Numbers.Integer_Of(
                  Check_Parameter_Number (This.Param_List.Next.Next.Next.Parameter)),
               Numbers.Long_Float_Of(
                  Check_Parameter_Number (This.Param_List.Next.Next.Next.Next.Parameter)),
               Convert(
               --Dotnetgraphlibrary.Color_Type.ValueType'Val(
                  Numbers.Integer_Of(
                     Check_Parameter_Number (
                        This.Param_List.Next.Next.Next.Next.Next.Parameter))),
               Boolean'Val(
                  Numbers.Integer_Of(
                     Check_Parameter_Number (
                        This.Param_List.Next.Next.Next.Next.Next.Next.Parameter))));
         when Lexer.Draw_Line =>
            Dotnetgraphlibrary.Dotnetgraph.Draw_Line(
               Numbers.Integer_Of(
                  Check_Parameter_Number (This.Param_List.Parameter)),
               Numbers.Integer_Of(
                  Check_Parameter_Number (This.Param_List.Next.Parameter)),
               Numbers.Integer_Of(
                  Check_Parameter_Number (This.Param_List.Next.Next.Parameter)),
               Numbers.Integer_Of(
                  Check_Parameter_Number (This.Param_List.Next.Next.Next.Parameter)),
               Convert(
               --Dotnetgraphlibrary.Color_Type.ValueType'Val(
                  Numbers.Integer_Of(
                     Check_Parameter_Number (
                        This.Param_List.Next.Next.Next.Next.Parameter))));
         when Lexer.Flood_Fill =>
            Dotnetgraphlibrary.Dotnetgraph.Flood_Fill(
               Numbers.Integer_Of(
                  Check_Parameter_Number (This.Param_List.Parameter)),
               Numbers.Integer_Of(
                  Check_Parameter_Number (This.Param_List.Next.Parameter)),
               Convert(
               --Dotnetgraphlibrary.Color_Type.ValueType'Val(
                  Numbers.Integer_Of(
                     Check_Parameter_Number (This.Param_List.Next.Next.Parameter))));
         when Lexer.Open_Graph_Window =>
            Dotnetgraphlibrary.Dotnetgraph.Open_Graph_Window(
               Numbers.Integer_Of(
                  Check_Parameter_Number (This.Param_List.Parameter)),
               Numbers.Integer_Of(
                  Check_Parameter_Number (This.Param_List.Next.Parameter)));
            Dotnetgraphlibrary.Dotnetgraph.Set_Window_Title(+("RAPTORGraph" &
               ASCII.NUL));
         when Lexer.Wait_For_Key =>
            Dotnetgraphlibrary.Dotnetgraph.Wait_For_Key;
         when Lexer.Wait_For_Mouse_Button =>
            Dotnetgraphlibrary.Dotnetgraph.Wait_For_Mouse_Button(
               Convert(
--               Dotnetgraphlibrary.Mouse_Button.ValueType'Val(
               Numbers.Integer_Of(
                  Check_Parameter_Number (This.Param_List.Parameter))));
         when Lexer.Put_Pixel =>
            Dotnetgraphlibrary.Dotnetgraph.Put_Pixel(
               Numbers.Integer_Of(
                  Check_Parameter_Number (This.Param_List.Parameter)),
               Numbers.Integer_Of(
                  Check_Parameter_Number (This.Param_List.Next.Parameter)),
               Convert(
               --Dotnetgraphlibrary.Color_Type.ValueType'Val(
                  Numbers.Integer_Of(
                     Check_Parameter_Number (This.Param_List.Next.Next.Parameter))));
         when Lexer.Set_Window_Title =>
            Dotnetgraphlibrary.Dotnetgraph.Set_Window_Title(
               Check_Parameter_String (This.Param_List.Parameter));
         when Lexer.Save_Graph_Window =>
            Dotnetgraphlibrary.Dotnetgraph.Save_Bitmap(
               Check_Parameter_String (This.Param_List.Parameter));
         when Lexer.Get_Mouse_Button =>
            declare
               X,Y : Integer;
            begin
               Dotnetgraphlibrary.Dotnetgraph.Get_Mouse_Button(
                  Convert(
--                  Dotnetgraphlibrary.Mouse_Button.ValueType'Val(
                     Numbers.Integer_Of(
                        Check_Parameter_Number (This.Param_List.Parameter))));
               X := Dotnetgraphlibrary.dotnetgraph.Get_Click_X;
               Y := Dotnetgraphlibrary.Dotnetgraph.Get_Click_Y;
               Assign_To(This.Param_List.Next.Parameter,Numbers.Make_Value(X),
                  "Get_Mouse_Button's first parameter");
               Assign_To(This.Param_List.Next.Next.Parameter,Numbers.Make_Value(Y),
                  "Get_Mouse_Button's second parameter");
            end;
      end case;
   end Execute;

   procedure Execute (This : in Plugin_Proc_Call) is
      T : Lexer.Token_Pointer;
   begin
      T := This.Id;
      raptor.Plugins.Invoke(+Get_Text(T.Start..T.Finish),
         This.Param_List);
   end Execute;

   procedure Execute (This : in Tabid_Proc_Call) is
      T : Lexer.Token_Pointer;
   begin
      T := This.Id;
      raptor.Step_Helpers.Invoke_Tab_Procedure(+Get_Text(T.Start..T.Finish),
         This.Param_List);
   end Execute;

   function Is_Tab_Call(This : in Tabid_Proc_Call) return Boolean is
   begin
      return True;
   end Is_Tab_Call;

   function Is_Tab_Call(This : in Procedure_Call) return Boolean is
   begin
      return False;
   end Is_Tab_Call;

   function Get_Name(This : in Id_Lhs) return String is
      T : Lexer.Token_Pointer;
   begin
      T := This.Id;
      return Get_Text(T.Start..T.Finish);
   end Get_Name;

   function Get_Name(This : in Procedure_Call) return String is
      T : Lexer.Token_Pointer;
   begin
      T := This.Id;
      return Get_Text(T.Start..T.Finish);
   end Get_Name;

--   -------------
--   -- Execute --
--   -------------

--   procedure Execute (This : in Expr_Assignment) is
--      V : Value;
--      T : Lexer.Token_Pointer;
--   begin
--      V := Execute (This.Expr_Part.All);
--      T := This.Id;
--      Raptor.Runtime.Setvariable(
--            S => +To_Upper(Get_Text(T.Start..T.Finish)),
--            F => V);
--   end Execute;


--   -------------
--   -- Execute --
--   -------------

--   procedure Execute (This : in Array_Assignment) is
--      V : Value;
--      Rhs : Value;
--      T : Lexer.Token_Pointer;
--   begin
--      T := This.Id;
--      V := Execute (This.Reference.All);
--      if not Is_Integer(V) then
--         Raise_Exception(Runtime_Error'Identity,
--            Long_Float_Full_Image(Long_Float_Of(V)) & " not a valid array location--must be integer");
--      end if;
--      Rhs := Execute (This.Expr_Part.All);
--      raptor.Runtime.SetArrayElement(
--         +To_Upper(Get_Text(T.Start..T.Finish)),
--         Integer_Of(V),
--         Rhs);
--   end Execute;

--   -------------
--   -- Execute --
--   -------------

--   procedure Execute (This : in Array_2D_Assignment) is
--      V,V2 : Value;
--      Rhs : Value;
--      T : Lexer.Token_Pointer;
--   begin
--      T := This.Id;
--      V := Execute (This.Reference.All);
--      if not Is_Integer(V) then
--         Raise_Exception(Runtime_Error'Identity,
--            Long_Float_Full_Image(Long_Float_Of(V)) & " not a valid array location--must be integer");
--      end if;
--      V2 := Execute (This.Reference2.All);
--      if not Is_Integer(V2) then
--         Raise_Exception(Runtime_Error'Identity,
--            Long_Float_Full_Image(Long_Float_Of(V2)) & " not a valid array location--must be integer");
--      end if;
--      Rhs := Execute (This.Expr_Part.All);
--      raptor.Runtime.Set2DArrayElement(
--         +To_Upper(Get_Text(T.Start..T.Finish)),
--         Integer_Of(V),
--         Integer_Of(V2),
--         Rhs);
--   end Execute;
   -------------
   -- Execute --
   -------------

   function Execute (This : in Number_Expon) return Value is
      T : Lexer.Token_Pointer;
   begin
      T := This.Number;
      return Make_Value(Get_Text(T.Start..T.Finish));
   end Execute;


--   -------------
--   -- Execute --
--   -------------

--   function Execute (This : in Array_Ref_Expon) return Value is
--      V : Value;
--      T : Lexer.Token_Pointer;
--      m_str : access MSSyst.String.Typ;
--   begin
--      T := This.Id;
--      m_str := +To_Upper(get_text(t.start..t.finish));
--      V := Execute (This.Reference.All);
--      if not Is_Integer(V) then
--         Raise_Exception(Runtime_Error'Identity,
--            Long_Float_Full_Image(Long_Float_Of(V)) & " not a valid array location--must be integer");
--      end if;
--      return raptor.Runtime.GetArrayElement(
--            m_str,
--            Integer_Of(V));
--   end Execute;
--   -------------
--   -- Execute --
--   -------------

--   function Execute (This : in Array_2D_Ref_Expon) return Value is
--      V,V2 : Value;
--      T : Lexer.Token_Pointer;
--      m_str : access MSSyst.String.Typ;
--   begin
--      T := This.Id;
--      m_str := +To_Upper(get_text(t.start..t.finish));
--      V := Execute (This.Reference.All);
--      V2 := Execute (This.Reference2.All);
--      if not Is_Integer(V) then
--         Raise_Exception(Runtime_Error'Identity,
--            Long_Float_Full_Image(Long_Float_Of(V)) & " not a valid array location--must be integer");
--      end if;
--      if not Is_Integer(V2) then
--         Raise_Exception(Runtime_Error'Identity,
--            Long_Float_Full_Image(Long_Float_Of(V2)) & " not a valid array location--must be integer");
--      end if;
--      return raptor.Runtime.Get2DArrayElement(
--            m_str,
--            Integer_Of(V),
--            Integer_Of(V2));
--   end Execute;
--   -------------
--   -- Execute --
--   -------------

--   function Execute (This : in Id_Expon) return Value is
--      T : Lexer.Token_Pointer;
--   begin
--      T := This.Id;
--      return
--         raptor.Runtime.GetVariable(
--               +To_Upper(Get_Text(T.Start..T.Finish)));
--   EXCEPTION WHEN OTHERS =>
--         if Raptor.Runtime.Is_2D_Array(+To_Upper(Get_Text(T.Start..T.Finish))) THEN
--            Raise_Exception(Runtime_Error'Identity,
--               "2D Array " & Get_Text(T.Start..T.Finish) &
--               " must be indexed [].");
--         elsif Raptor.Runtime.isArray(+To_Upper(Get_Text(T.Start..T.Finish))) THEN
--            Raise_Exception(Runtime_Error'Identity,
--               "Array " & Get_Text(T.Start..T.Finish) &
--               " must be indexed [].");
--         else
--            Raise_Exception(Runtime_Error'Identity,
--               "Variable " & Get_Text(T.Start..T.Finish) &
--               " doesn't have a value.");
--         end if;
--   end Execute;

   -------------
   -- Execute --
   -------------

   function Execute (This : in String_Expon) return Value is
      T : Lexer.Token_Pointer;
   begin
      T := This.S;
      return Make_String_Value (S =>
         +(Dequote(Get_Text(T.Start+1..T.Finish-1))));
   end Execute;

   function Execute (This : in Character_Expon) return Value is
      T : Lexer.Token_Pointer := This.S;
      S : String := Get_Text(T.Start+1..T.Finish-1);
   begin
      return Make_Value (Ada.Characters.Conversions.To_Wide_Character(S(S'First)));
   end Execute;
   function Execute (This : in Negative_Expon) return Value is
   begin
      return -Execute (This.E.all);
   end Execute;



   -------------
   -- Execute --
   -------------

   function Execute (This : in Paren_Expon) return Value is
   begin
      return Execute (This.Expr_Part.all);
   end Execute;

   -------------
   -- Execute --
   -------------
   Generator : MSSyst.Random.Ref := MSSyst.Random.New_Random;

   function Execute (This : in Func0_Expon) return Value is
      Kind : Lexer.Func_Id0 := This.Id.Kind;
   begin
      case Kind is
         when Lexer.Random =>
            return Numbers.Make_Value(F => Long_Float(
                  Mssyst.Random.Nextdouble(Generator)));
         when Lexer.Random_Color =>
            return Numbers.Make_Value(F => Long_Float'Floor(Long_Float(
                  Mssyst.Random.Nextdouble(Generator))*16.0));
         when Lexer.Random_Extended_Color =>
            return Numbers.Make_Value(F => Long_Float'Floor(Long_Float(
                  Mssyst.Random.Nextdouble(Generator))*242.0));
         when Lexer.Pi =>
            return Numbers.Pi;
         when Lexer.E =>
            return Numbers.E;
         when Lexer.Black..Lexer.White =>
            return Numbers.Make_Value(I =>
               Lexer.Token_Type'Pos(Kind)-Lexer.Token_Type'Pos(Lexer.Black));
         when Lexer.Left_Button =>
            return Numbers.Make_Value(0);
         when Lexer.Right_Button =>
            return Numbers.Make_Value(1);
         when Lexer.Filled|Lexer.Yes|Lexer.True =>
            return Numbers.Make_Value(1);
         when Lexer.Unfilled|Lexer.No|Lexer.False =>
            return Numbers.Make_Value(0);
         when Lexer.Get_Window_Height =>
            return Numbers.Make_Value(
               dotnetgraphlibrary.dotnetgraph.Get_Window_Height);
         when Lexer.Get_Window_Width =>
            return Numbers.Make_Value(
               dotnetgraphlibrary.dotnetgraph.Get_Window_Width);
         when Lexer.Get_Font_Height =>
            return Numbers.Make_Value(
               dotnetgraphlibrary.dotnetgraph.Get_Font_Height);
         when Lexer.Get_Font_Width =>
            return Numbers.Make_Value(
               dotnetgraphlibrary.dotnetgraph.Get_Font_Width);
         when Lexer.Get_Max_Width =>
            return Numbers.Make_Value(
               dotnetgraphlibrary.dotnetgraph.Get_Max_X);
         when Lexer.Get_Max_Height =>
            return Numbers.Make_Value(
               dotnetgraphlibrary.dotnetgraph.Get_Max_Y);
         when Lexer.Get_Mouse_X =>
            return Numbers.Make_Value(
               dotnetgraphlibrary.dotnetgraph.Get_Mouse_X);
         when Lexer.Get_Mouse_Y =>
            return Numbers.Make_Value(
               Dotnetgraphlibrary.Dotnetgraph.Get_Mouse_Y);
         when Lexer.Get_Key_String =>
            return Numbers.Make_String_Value(mssyst.string.ref(
               dotnetgraphlibrary.dotnetgraph.Get_Key_String));
         when Lexer.Get_Key =>
            return Numbers.Make_Value(Wide_Character'Pos(
               dotnetgraphlibrary.dotnetgraph.Get_Key));
      end case;
   end Execute;

   -------------
   -- Execute --
   -------------
--      Sin,Cos,Tan,Cot,Arcsin,Arccos,
--      Arctan,Arccot,
--      Sinh,Tanh,Cosh,Arccosh,Arcsinh,Arctanh,
--      Coth,Arccoth,Sqrt,Log,Floor,Ceiling,Abs_F,

   function Execute (This : in Boolean1) return Boolean is
      function Convert is new Ada.Unchecked_Conversion(
         integer,Dotnetgraphlibrary.Mouse_Button.ValueType);
   begin
      case This.Kind is
         when Key_Down =>
            return Dotnetgraphlibrary.Dotnetgraph.Key_Down_String(
               String_Of(Execute (This.Parameter.all)));
         when Mouse_Button_Down =>
            return Dotnetgraphlibrary.Dotnetgraph.Mouse_Button_Down(
               Convert(
--               Dotnetgraphlibrary.Mouse_Button.ValueType'Val(
                  Integer_Of(Execute(This.Parameter.all))));
         when Mouse_Button_Pressed =>
            return Dotnetgraphlibrary.Dotnetgraph.Mouse_Button_Pressed(
               Convert(
--               Dotnetgraphlibrary.Mouse_Button.ValueType'Val(
                  Integer_Of(Execute(This.Parameter.all))));
         when Mouse_Button_Released =>
            return Dotnetgraphlibrary.Dotnetgraph.Mouse_Button_Released(
               Convert(
--               Dotnetgraphlibrary.Mouse_Button.ValueType'Val(
                  Integer_Of(Execute(This.Parameter.all))));
      end case;
   end Execute;

   function Execute (This : in Boolean_Reflection) return Boolean is
      T : Lexer.Token_Pointer;
      Temp : Boolean;
   begin
      T := This.Id;
      case This.Kind is
         when Is_Character =>
            return Raptor.Runtime.is_Character(+Get_Text(T.Start..T.Finish));
         when Is_Number =>
            return Raptor.Runtime.is_Scalar(+Get_Text(T.Start..T.Finish));
         when Is_String =>
            return Raptor.Runtime.is_String(+Get_Text(T.Start..T.Finish));
         when Is_Array =>
            return Raptor.Runtime.isArray(+Get_Text(T.Start..T.Finish));
         when Is_2D_Array =>
            return Raptor.Runtime.is_2D_Array(+Get_Text(T.Start..T.Finish));
      end case;
   end Execute;

   function Execute (This : in Boolean_Plugin) return Boolean is
      T : Lexer.Token_Pointer;
      Y : Numbers.Value;
   begin
      T := This.Id;
      y := raptor.Plugins.Invoke_Function(+Get_Text(T.Start..T.Finish),
         This.Parameters);
      return numbers.long_float_of(y) > 0.5;
   end Execute;

   function Execute (This : in Boolean0) return Boolean is
   begin
      case This.Kind is
         when Is_Open =>
            return Dotnetgraphlibrary.Dotnetgraph.Is_Open;
         when Key_Hit =>
            return Dotnetgraphlibrary.Dotnetgraph.Key_Hit;
         when End_Of_Input =>
            return Raptor.Runtime.End_Of_Input;
      end case;
   end Execute;

   function Execute(This : in Boolean_Constant) return Boolean is
   begin
      return This.Value;
   end Execute;

   function Execute (This : in Plugin_Func_Expon) return Value is
      T : Lexer.Token_Pointer;
      f : numbers.value;
   begin
      T := This.Id;
      f := raptor.Plugins.Invoke_Function(+Get_Text(T.Start..T.Finish),
         This.Parameters);
      return f;
   end Execute;


   function Execute (This : in Func_Expon) return Value is
      Function_Name : Lexer.Func_Token_Types;
      function Convert is new Ada.Unchecked_Conversion(
         Dotnetgraphlibrary.Color_Type.Valuetype,Integer);
      result : Dotnetgraphlibrary.Color_Type.Valuetype;
      result_int : integer;
   begin
      Function_Name := This.Id.Kind;

      case Function_Name is
         when Lexer.Func_Id0 =>
            -- should be handled above in Func0_Expon
            raise Constraint_Error;
         when Lexer.To_Ascii =>
            return Numbers.Make_Value(I=>
               Integer_Of(Check_Parameter_Number(This.Parameters.Parameter)));
         when Lexer.To_Character =>
            return Numbers.Make_Value(C=>
               Wide_Character'Val(Integer_Of(Check_Parameter_Number(This.Parameters.Parameter))));
         when Lexer.Length_Of =>
            return Numbers.Make_Value(I=>Length_Of(This.Parameters.Parameter));
         when Lexer.Closest_Color =>
            Result := Dotnetgraphlibrary.Dotnetgraph.Closest_Color(
                     Integer_Of(Check_Parameter_Number (
                        This.Parameters.Parameter)),
                     Integer_Of(Check_Parameter_Number (
                        This.Parameters.Next.Parameter)),
                     Integer_Of(Check_Parameter_Number (
                        This.Parameters.Next.Next.Parameter)));
--               Dotnetgraphlibrary.Color_Type.ValueType'Pos(
            result_int := convert(result);
            return Numbers.Make_Value(result_int);
         when Lexer.Get_Pixel =>
            Result := Dotnetgraphlibrary.Dotnetgraph.Get_Pixel(
                     Integer_Of(Check_Parameter_Number (
                        This.Parameters.Parameter)),
                     Integer_Of(Check_Parameter_Number (
                        This.Parameters.Next.Parameter)));
--               Dotnetgraphlibrary.Color_Type.ValueType'Pos(
            result_int := convert(result);
            return Numbers.Make_Value(result_int);
         when Lexer.Load_Bitmap =>
            return Numbers.Make_Value(
               Dotnetgraphlibrary.Dotnetgraph.Load_Bitmap(
                     Check_Parameter_String (
                           This.Parameters.Parameter)));
         when Lexer.Sinh =>
            return Numbers.Sinh (Check_Parameter_Number(This.Parameters.Parameter));
         when Lexer.Tanh =>
            return Numbers.Tanh (Check_Parameter_Number (This.Parameters.Parameter));
         when Lexer.Cosh =>
            return Numbers.Cosh (Check_Parameter_Number (This.Parameters.Parameter));
         when Lexer.Arccosh =>
            return Numbers.Arccosh (Check_Parameter_Number (This.Parameters.Parameter));
         when Lexer.Arcsinh =>
            return Numbers.Arcsinh (Check_Parameter_Number (This.Parameters.Parameter));
         when Lexer.Arctanh =>
            return Numbers.Arctanh (Check_Parameter_Number (This.Parameters.Parameter));
         when Lexer.Coth =>
            return Numbers.Coth (Check_Parameter_Number (This.Parameters.Parameter));
         when Lexer.Arccoth =>
            return Numbers.Arccoth (Check_Parameter_Number (This.Parameters.Parameter));
         when Lexer.Sqrt =>
            return Numbers.Sqrt (Check_Parameter_Number (This.Parameters.Parameter));
         when Lexer.Floor =>
            return Numbers.Floor (Check_Parameter_Number (This.Parameters.Parameter));
         when Lexer.Ceiling =>
            return Numbers.Ceiling (Check_Parameter_Number (This.Parameters.Parameter));
         when Lexer.Abs_F =>
            return Numbers."abs" (Check_Parameter_Number (This.Parameters.Parameter));
         when Lexer.Min =>
            return Numbers.Min (
               Check_Parameter_Number (This.Parameters.Parameter),
               Check_Parameter_Number (This.Parameters.Next.Parameter));
         when Lexer.Max =>
            return Numbers.Max (
               Check_Parameter_Number (This.Parameters.Parameter),
               Check_Parameter_Number (This.Parameters.Next.Parameter));
         when Lexer.Log =>
            if Parser.Count_Parameters(This.Parameters) = 1 then
               return Numbers.Log (Check_Parameter_Number (This.Parameters.Parameter));
            elsif Parser.Count_Parameters(This.Parameters) = 2 then
               return Numbers.Log (
                  Check_Parameter_Number (This.Parameters.Parameter),
                  Check_Parameter_Number (This.Parameters.Next.Parameter));
            else
               return Numbers.Pi; -- not possible
            end if;
         when Lexer.Sin =>
            if Parser.Count_Parameters(This.Parameters) = 1 then
               return Numbers.Sin (Check_Parameter_Number (This.Parameters.Parameter));
            elsif Parser.Count_Parameters(This.Parameters) = 2 then
               return Numbers.Sin (
                  Check_Parameter_Number (This.Parameters.Parameter),
                  Check_Parameter_Number (This.Parameters.Next.Parameter));
            else
               return Numbers.Pi; -- not possible
            end if;
         when Lexer.Cos =>
            if Parser.Count_Parameters(This.Parameters) = 1 then
               return Numbers.Cos (Check_Parameter_Number (This.Parameters.Parameter));
            elsif Parser.Count_Parameters(This.Parameters) = 2 then
               return Numbers.Cos (
                  Check_Parameter_Number (This.Parameters.Parameter),
                  Check_Parameter_Number (This.Parameters.Next.Parameter));
            else
               return Numbers.Pi; -- not possible
            end if;
         when Lexer.Tan =>
            if Parser.Count_Parameters(This.Parameters) = 1 then
               return Numbers.Tan (Check_Parameter_Number (This.Parameters.Parameter));
            elsif Parser.Count_Parameters(This.Parameters) = 2 then
               return Numbers.Tan (
                  Check_Parameter_Number (This.Parameters.Parameter),
                  Check_Parameter_Number (This.Parameters.Next.Parameter));
            else
               return Numbers.Pi; -- not possible
            end if;
         when Lexer.Cot =>
            if Parser.Count_Parameters(This.Parameters) = 1 then
               return Numbers.Cot (Check_Parameter_Number (This.Parameters.Parameter));
            elsif Parser.Count_Parameters(This.Parameters) = 2 then
               return Numbers.Cot (
                  Check_Parameter_Number (This.Parameters.Parameter),
                  Check_Parameter_Number (This.Parameters.Next.Parameter));
            else
               return Numbers.Pi; -- not possible
            end if;
         when Lexer.Arcsin =>
            if Parser.Count_Parameters(This.Parameters) = 1 then
               return Numbers.Arcsin (Check_Parameter_Number (This.Parameters.Parameter));
            elsif Parser.Count_Parameters(This.Parameters) = 2 then
               return Numbers.Arcsin (
                  Check_Parameter_Number (This.Parameters.Parameter),
                  Check_Parameter_Number (This.Parameters.Next.Parameter));
            else
               return Numbers.Pi; -- not possible
            end if;
         when Lexer.Arccos =>
            if Parser.Count_Parameters(This.Parameters) = 1 then
               return Numbers.Arccos (Check_Parameter_Number (This.Parameters.Parameter));
            elsif Parser.Count_Parameters(This.Parameters) = 2 then
               return Numbers.Arccos (
                  Check_Parameter_Number (This.Parameters.Parameter),
                  Check_Parameter_Number (This.Parameters.Next.Parameter));
            else
               return Numbers.Pi; -- not possible
            end if;
         when Lexer.Arctan =>
            if Parser.Count_Parameters(This.Parameters) = 1 then
               return Numbers.Arctan (Check_Parameter_Number (This.Parameters.Parameter));
            elsif Parser.Count_Parameters(This.Parameters) = 2 then
               return Numbers.Arctan (
                  Check_Parameter_Number (This.Parameters.Parameter),
                  Check_Parameter_Number (This.Parameters.Next.Parameter));
            elsif Parser.Count_Parameters(This.Parameters) = 3 then
               return Numbers.Arctan (
                  Check_Parameter_Number (This.Parameters.Parameter),
                  Check_Parameter_Number (This.Parameters.Next.Parameter),
                  Check_Parameter_Number (This.Parameters.Next.Next.Parameter));
            else
               return Numbers.Pi; -- not possible
            end if;
         when Lexer.Arccot =>
            if Parser.Count_Parameters(This.Parameters) = 1 then
               return Numbers.Arccot (Check_Parameter_Number (This.Parameters.Parameter));
            elsif Parser.Count_Parameters(This.Parameters) = 2 then
               return Numbers.Arccot (
                  Check_Parameter_Number (This.Parameters.Parameter),
                  Check_Parameter_Number (This.Parameters.Next.Parameter));
            elsif Parser.Count_Parameters(This.Parameters) = 3 then
               return Numbers.Arccot (
                  Check_Parameter_Number (This.Parameters.Parameter),
                  Check_Parameter_Number (This.Parameters.Next.Parameter),
                  Check_Parameter_Number (This.Parameters.Next.Next.Parameter));
            else
               return Numbers.Pi; -- not possible
            end if;
      end case;
   end Execute;

   -------------
   -- Execute --
   -------------

   function Execute (This : in Mult) return Value is
   begin
      return Execute (This.Left.all);
   end Execute;

   -------------
   -- Execute --
   -------------

   function Execute (This : in Expon_Mult) return Value is
   begin
      return Execute (This.Left.all) ** Execute (This.Right.all);
   end Execute;

   -------------
   -- Execute --
   -------------

   function Execute (This : in Add) return Value is
   begin
      return Execute (This.Left.all);
   end Execute;

   -------------
   -- Execute --
   -------------

   function Execute (This : in Div_Add) return Value is
   begin
      return Execute (This.Left.all) / Execute (This.Right.all);
   end Execute;

   -------------
   -- Execute --
   -------------

   function Execute (This : in Mult_Add) return Value is
   begin
      return Execute (This.Left.all) * Execute (This.Right.all);
   end Execute;

   -------------
   -- Execute --
   -------------

   function Execute (This : in Mod_Add) return Value is
   begin
      return Execute (This.Left.all) mod Execute (This.Right.all);
   end Execute;

   -------------
   -- Execute --
   -------------

   function Execute (This : in Rem_Add) return Value is
   begin
      return Execute (This.Left.all) rem Execute (This.Right.all);
   end Execute;

   -------------
   -- Execute --
   -------------

   function Execute (This : in Expression) return Value is
   begin
      return Execute (This.Left.all);
   end Execute;

   -------------
   -- Execute --
   -------------

   function Execute (This : in Add_Expression) return Value is
   begin
      return Execute (This.Left.all) + Execute (This.Right.all);
   end Execute;

   -------------
   -- Execute --
   -------------

   function Execute (This : in Minus_Expression) return Value is
   begin
      return Execute (This.Left.all) - Execute (This.Right.all);
   end Execute;

   -------------
   -- Execute --
   -------------

   function Execute (This : in Relation) return Boolean is
      This_Kind : Lexer.Relation := This.Kind;
      L : Value := Execute (This.Left.all);
      R : Value := Execute (This.Right.All);
   begin
      case This_Kind is
         when Greater =>
            return L > R;
         when Greater_Equal =>
            return L >= R;
         when Equal =>
            return L = R;
         when Less =>
            return L < R;
         when Less_Equal =>
            return L <= R;
         when Not_Equal =>
            return not (L = R);
      end case;
   end Execute;

   -------------
   -- Execute --
   -------------

   function Execute (This : in Boolean2) return Boolean is
   begin
      if This.Negated then
         return not Execute (This.Left.All);
      else
         return Execute (This.Left.All);
      end if;
   end Execute;

   -------------
   -- Execute --
   -------------

   function Execute (This : in And_Boolean2) return Boolean is
   begin
      if This.Negated then
         return not Execute (This.Left.All) and then Execute (This.Right.all);
      else
         return Execute (This.Left.All) and then Execute (This.Right.all);
      end if;
   end Execute;

   -------------
   -- Execute --
   -------------

   function Execute (This : in Boolean_Expression) return Boolean is
   begin
      return Execute (This.Left.all);
   end Execute;

   -------------
   -- Execute --
   -------------

   function Execute (This : in Or_Boolean) return Boolean is
   begin
      return Execute (This.Left.all) or else Execute (This.Right.all);
   end Execute;

   -------------
   -- Execute --
   -------------

   function Execute (This : in Xor_Boolean) return Boolean is
   begin
      return Execute (This.Left.all) xor Execute (This.Right.all);
   end Execute;

   -------------
   -- Execute --
   -------------
   function Get_Name(This : in Input) return String is
   begin
      return Get_Name(This.Lhs.all);
   end Get_Name;

   procedure Execute (This : in Input; Prompt : in MSSyst.String.Ref) is
      Result : access MSSyst.String.Typ;
      Rhs : Value;
      Context : MSSyst.Object.Ref;
   begin
      Result := Raptor.Runtime.Promptdialog(Prompt);
      Rhs := Numbers.Make_Value(+Result);
      Context := Execute(This.Lhs.All, This.LSuffix, null, Rhs);
      Execute(This.LSuffix.all, Context, Rhs);
   end Execute;

--   procedure Execute (This : in Input_Array; Prompt : in MSSyst.String.Ref) is
--      V : Value;
--      Rhs : Value;
--      Result : access MSSyst.String.Typ;
--      T : Lexer.Token_Pointer;
--   begin
--      T := This.Id;
--      V := Execute (This.Reference.All);
--      if not Is_Integer(V) then
--         Raise_Exception(Runtime_Error'Identity,
--            Long_Float_Full_Image(Long_Float_Of(V)) & " not a valid array location--must be integer");
--      end if;
--      loop
--         begin
--            Result := raptor.Runtime.promptDialog(Prompt);
--            Rhs := Numbers.Make_Value(+Result);
--            exit;
--         exception when Ada.Numerics.Argument_Error =>
--            if Raptor_Files.Input_Redirected then
--               Raise_Exception(Runtime_Error'Identity,
--                  "Found: """ & (+Result) & """ instead of number in file.");
--            end if;
--         end;
--      end loop;
--      raptor.Runtime.SetArrayElement(
--         +To_Upper(Get_Text(T.Start..T.Finish)),
--         Integer_Of(V),
--         Rhs);
--   end Execute;

--   procedure Execute (This : in Input_2D_Array; Prompt : in MSSyst.String.Ref) is
--      V,V2 : Value;
--      Rhs : Value;
--      T : Lexer.Token_Pointer;
--      Result : access MSSyst.String.Typ;
--   begin
--      T := This.Id;
--      V := Execute (This.Reference.All);
--      if not Is_Integer(V) then
--         Raise_Exception(Runtime_Error'Identity,
--            Long_Float_Full_Image(Long_Float_Of(V)) & " not a valid array location--must be integer");
--      end if;
--      V2 := Execute (This.Reference2.All);
--      if not Is_Integer(V2) then
--         Raise_Exception(Runtime_Error'Identity,
--            Long_Float_Full_Image(Long_Float_Of(V2)) & " not a valid array location--must be integer");
--      end if;
--      loop
--         begin
--            Result := raptor.Runtime.promptDialog(Prompt);
--            Rhs := Numbers.Make_Value(+Result);
--            exit;
--         exception when Ada.Numerics.Argument_Error =>
--            if Raptor_Files.Input_Redirected then
--               Raise_Exception(Runtime_Error'Identity,
--                  "Found: """ & (+Result) & """ instead of number in file.");
--            end if;
--         end;
--      end loop;
--      raptor.Runtime.Set2DArrayElement(
--         +To_Upper(Get_Text(T.Start..T.Finish)),
--         Integer_Of(V),
--         Integer_Of(V2),
--         Rhs);
--   end Execute;

   function Get_String(This : in Expr_Output) return MSSyst.String.Ref is
      Expr1 : Expression_Pointer;
      Add1  : Value_Parseable_Pointer;
      Mult1 : Value_Parseable_Pointer;
      Expon1 : Value_Parseable_Pointer;
      Rhs_Expon1 : Rhs_Pointer;
      T : Lexer.Token_Pointer;
   begin
      Expr1 := This.Expr;
      if Expr1.All in Add_Expression'Class or
            Expr1.All in Minus_Expression'Class then
         raise Constraint_Error;
      end if;
      Add1 := Expr1.Left;
      if Add1.All in Div_Add'Class or
            Add1.All in Mod_Add'Class or
            Add1.All in Rem_Add'Class or
            Add1.All in Mult_Add'Class then
         raise Constraint_Error;
      end if;
      Mult1 := Add_Pointer(Add1).Left;
      if Mult1.All in Expon_Mult'Class then
         raise Constraint_Error;
      end if;
      Expon1 := Mult_Pointer(Mult1).Left;
      if not(Expon1.All in Rhs_Expon'Class) then
         raise Constraint_Error;
      end if;
      Rhs_Expon1 := Rhs_Pointer(Rhs_Expon(Expon1.all).Rhs);
      if Rhs_Expon1.All in Array_2D_Ref_Rhs'Class then
         T := Array_2D_Ref_Rhs(Rhs_Expon1.all).Id;
         Raise_Exception(Runtime_Error'Identity,
               get_text(t.start..t.finish) & " should not have [,]");
      elsif Rhs_Expon1.All in Array_Ref_Rhs'Class then
         T := Array_Ref_Rhs(Rhs_Expon1.all).Id;
         Raise_Exception(Runtime_Error'Identity,
               get_text(t.start..t.finish) & " should not have []");
      else
         T := Id_Rhs(Rhs_Expon1.all).Id;
         return +To_Upper(Get_Text(T.Start..T.Finish));
      end if;
   end Get_String;

   function Execute_Return_Value(This : in Expr_Output) return Value is
      V : Value;
   BEGIN
      Raptor.Runtime.SetContext(null);
      V := Execute(This.Expr.All);
      return V;
   end Execute_Return_Value;

   -------------
   -- Execute --
   -------------

   procedure Execute (This : in Expr_Output) is
      V : Value;
      M_Str : access MSSyst.String.Typ;
      I : Long_Long_Integer;
   begin
      V := Execute(This.Expr.All);
      if Is_Long_Long_Integer(V) then
         I := Long_Long_Integer_Of(V);
         if I < 0 then
            M_Str := +Long_Long_Integer'Image(I);
         else
            declare
               S : String := Long_Long_Integer'Image(I);
            begin
               M_Str := +S(S'First+1..S'Last);
            end;
         end if;
         if This.New_Line then
            Raptor.Runtime.ConsoleWriteln(M_Str);
         else
            Raptor.Runtime.ConsoleWrite(M_Str);
         end if;
      elsif Is_Character(V) then
         if This.New_Line then
            Raptor.Runtime.Consolewriteln(MSSyst.String.New_String(
               c=>numbers.character_of(v),count=>1));
         else
            Raptor.Runtime.Consolewrite(MSSyst.String.New_String(
               c=>numbers.character_of(v),count=>1));
         end if;
      elsif Is_String(V) then
         if This.New_Line then
            Raptor.Runtime.Consolewriteln(String_Of(V));
         else
            Raptor.Runtime.Consolewrite(String_Of(V));
         end if;
      else
         M_Str := +Long_Float_Image(Long_Float_Of(V));

         if This.New_Line then
            Raptor.Runtime.ConsoleWriteln(M_Str);
         else
            Raptor.Runtime.ConsoleWrite(M_Str);
         end if;
      end if;
   end Execute;

   -------------
   -- Execute --
   -------------

   procedure Execute (This : in String_Output) is
      T : Lexer.Token_Pointer;
      M_Str : access MSSyst.String.Typ;
   begin
      T := This.Str;
      M_Str := +Dequote(Get_Text(T.Start+1..T.Finish-1));
      if This.New_Line then
         Raptor.Runtime.ConsoleWriteln(M_Str);
      else
         Raptor.Runtime.ConsoleWrite(M_Str);
      end if;
   end Execute;

   function Get_String(This : in String_Output) return MSSyst.String.Ref is
      T : Lexer.Token_Pointer;
   begin
      T := This.Str;
      return +Dequote(Get_Text(T.Start+1..T.Finish-1));
   end Get_String;


   procedure Fix_Associativity(This : in out Mult_Pointer) is
      Temp : Mult_Pointer;
   begin
      if This.all in Expon_Mult'Class then
         if Binary_Mult_Pointer(This).Right.all in Expon_Mult'Class then
            Temp := Binary_Mult_Pointer(This).Right;
            Binary_Mult_Pointer(This).Right :=
               new Mult'(Left => Binary_Mult_Pointer(This).Right.Left);
            Temp.Left := Value_Parseable_Pointer(This);
            This := Temp;
            Fix_Associativity(This);
         end if;
      end if;
   end Fix_Associativity;

   procedure Fix_Associativity(This : in out Add_Pointer) is
      Temp : Add_Pointer;
   begin
      if This.all in Binary_Add'Class then
         if Binary_Add_Pointer(This).Right.all in Binary_Add'Class then
            Temp := Binary_Add_Pointer(This).Right;
            Binary_Add_Pointer(This).Right :=
               new Add'(Left => Binary_Add_Pointer(This).Right.Left);
            Temp.Left := Value_Parseable_Pointer(This);
            This := Temp;
            Fix_Associativity(This);
         end if;
      end if;
   end Fix_Associativity;

   procedure Fix_Associativity(This : in out Expression_Pointer) is
      Temp : Expression_Pointer;
   begin
      if This.all in Binary_Expression'Class then
         if Binary_Expression_Pointer(This).Right.all in Binary_Expression'Class then
            Temp := Binary_Expression_Pointer(This).Right;
            Binary_Expression_Pointer(This).Right :=
               new Expression'(Left => Binary_Expression_Pointer(This).Right.Left);
            Temp.Left := Value_Parseable_Pointer(This);
            This := Temp;
            Fix_Associativity(This);
         end if;
      end if;
   end Fix_Associativity;

   -------------------------------------------------------------------------------------
   -- Emit_Code
   -------------------------------------------------------------------------------------
   procedure Emit_Store_Dotnetgraph_Int_Func(
         This   : in Output_Pointer;
         Gen    : access Generate_Interface.Typ'Class;
         Func   : in Integer) is
      Expr1 : Expression_Pointer;
      Add1  : Value_Parseable_Pointer;
      Mult1 : Value_Parseable_Pointer;
      Expon1 : Value_Parseable_Pointer;
      Rhs1 : Rhs_Pointer;
      T : Lexer.Token_Pointer;
      M_Str : access MSSyst.String.Typ;
      V,V2 : Numbers.Value;
      use type ada.Tags.Tag;
   begin
      Expr1 := Expr_Output(This.All).Expr;
      if Expr1.All in Add_Expression'Class or
            Expr1.All in Minus_Expression'Class then
         Raise_Exception(Runtime_Error'Identity,"Can't have arithmetic operator here.");
      end if;
      Add1 := Expr1.Left;
      if Add1.All in Div_Add'Class or
            Add1.All in Mod_Add'Class or
            Add1.All in Rem_Add'Class or
            Add1.All in Mult_Add'Class then
         Raise_Exception(Runtime_Error'Identity,"Can't have arithmetic operator here.");
      end if;
      Mult1 := Add_Pointer(Add1).Left;
      if Mult1.All in Expon_Mult'Class then
         Raise_Exception(Runtime_Error'Identity,"Can't have arithmetic operator here.");
      end if;
      Expon1 := Mult_Pointer(Mult1).Left;

      if Expon1.All in Func0_Expon'Class or
            Expon1.All in Func_Expon'Class or
            Expon1.All in Plugin_Func_Expon'Class then
         Raise_Exception(Runtime_Error'Identity,"Can't have function here.");
      end if;
      if not (Expon1.All in Rhs_Expon'Class) or else
         not (Rhs_Expon(Expon1.All).Rsuffix.all in Empty_RSuffix'Class) then
         Raise_Exception(Runtime_Error'Identity,"Can only compile dotnetgraph store to local var.");
      end if;
      Rhs1 := Rhs_Expon(Expon1.All).Rhs;

         -- either an array ref or a variable
      if Rhs1.All in Array_2D_Ref_Rhs'Class then
         T := Array_2D_Ref_Rhs(Rhs1.all).Id;
         M_Str := +Get_Text(T.Start..T.Finish);
         Gen.Array_2D_Assignment_Start(M_Str);
         Emit_Code (Array_2D_Ref_Rhs(Rhs1.All).Reference.All,Gen);
         Gen.Array_2D_Assignment_Between_Indices;
         Emit_Code (Array_2d_Ref_Rhs(Rhs1.All).Reference2.All,Gen);
         Gen.Array_2D_Assignment_After_Indices;
         Gen.Emit_Get_Click(Func);
         Gen.Array_2D_Assignment_PastRHS;
      elsif Rhs1.All in Array_Ref_Rhs'Class then
         T := Array_Ref_Rhs(Rhs1.all).Id;
         m_str := +get_text(t.start..t.finish);
         Gen.Array_1D_Assignment_Start(M_Str);
         Emit_Code (Array_Ref_Rhs(Rhs1.all).Reference.All,Gen);
         Gen.Array_1D_Assignment_After_Index;
         Gen.Emit_Get_Click(Func);
         Gen.Array_1D_Assignment_PastRHS;
      elsif Rhs1.All'Tag = Id_Rhs'Tag then
         T := Id_Rhs(Rhs1.all).Id;
         M_Str := +Get_Text(T.Start..T.Finish);
         Gen.Variable_Assignment_Start(M_Str);
         Gen.Emit_Get_Click(Func);
         Gen.Variable_Assignment_PastRHS;
      else
         raise_exception(runtime_error'identity,"not a variable");
      end if;
   exception when e:others =>
      Raise_Exception(Runtime_Error'Identity,
            "bad parameter to get_mouse_button:" &
            ada.Exceptions.Exception_Message(e));
   end Emit_Store_Dotnetgraph_Int_Func;
   procedure Emit_Conversion(Name : Integer;
         Gen    : Raptor.Generate_Il.Ref) is
      Kind : Conversions := Conversions'Val(Name);
   begin
      case Kind is
         when Char_To_Int =>
            Gen.Emit_Method(
               Package_K => "numbers_pkg",
               Name      => "character_of");
            Gen.Emit_Method(
               Package_K => "numbers_pkg",
               Name      => "make_value__3");
         when Int_To_Char =>
            Gen.Emit_Method(
               Package_K => "numbers_pkg",
               Name      => "integer_of");
            Gen.Emit_Method(
               Package_K => "numbers_pkg",
               Name      => "make_value__4");
         when To_Integer|To_Color|To_Bool =>
            Gen.Emit_Method(
               Package_K => "numbers_pkg",
               Name      => "integer_of");
         when To_Float =>
            Gen.Emit_Method(
               Package_K => "numbers_pkg",
               Name      => "long_float_of");
         when To_String =>
            Gen.Emit_Method(
               Package_K => "numbers_pkg",
               Name      => "string_of");
         when Number_To_String =>
            Gen.Emit_Method(+"numbers_pkg",
               +"msstring_image");
      end case;
   end Emit_Conversion;

   procedure Emit_Method_Call_IL (Name : Integer;
         Parameter_Count : Integer;
         Gen    : in Raptor.Generate_IL.Ref) is
      Kind : Lexer.Subprogram := Lexer.Subprogram'Val(Name);
   begin
      case Kind is
         when Lexer.Length_Of =>
            raise Constraint_Error;
         when Lexer.Is_Number..Lexer.Is_2D_Array =>
            raise Constraint_Error;
         when Lexer.Delay_For =>
            raise Constraint_Error;
         when Lexer.Random =>
            Gen.Emit_Random;
         when Lexer.Random_Color =>
            Gen.Emit_Random_2(0.0,16.0);
         when Lexer.Random_Extended_Color =>
            Gen.Emit_Random_2(0.0,242.0);
         when Lexer.To_Ascii =>
            null;
         when Lexer.To_Character =>
            null;
         when Lexer.Redirect_Output_Append =>
            Gen.Emit_Method(
               Package_K => "ada_runtime_pkg",
               Name      => "redirect_output_append");
         when Lexer.Set_Precision=>
            Gen.Emit_Method(+"numbers_pkg",
               +"set_precision");
         when Lexer.Redirect_Input =>
            Gen.Emit_Method(
               Package_K => "ada_runtime_pkg",
               Name      => "redirect_input");
         when Lexer.Redirect_Output =>
            Gen.Emit_Method(
               Package_K => "ada_runtime_pkg",
               Name      => "redirect_output");
        when Lexer.Clear_Console =>
            Gen.Emit_Method(+"raptor.Runtime",+"consoleClear");
        when Lexer.Clear_Window =>
            Gen.Emit_Method(+"dotnetgraphlibrary.dotnetgraph",
               +"Clear_Window");
         when Lexer.Close_Graph_Window =>
            Gen.Emit_Method(+"dotnetgraphlibrary.dotnetgraph",
               +"Close_Graph_Window");
         when Lexer.Freeze_Graph_Window =>
            Gen.Emit_Method(+"dotnetgraphlibrary.dotnetgraph",
               +"FreezeGraphWindow");
         when Lexer.Unfreeze_Graph_Window =>
            Gen.Emit_Method(+"dotnetgraphlibrary.dotnetgraph",
               +"UnfreezeGraphWindow");
         when Lexer.Update_Graph_Window =>
            Gen.Emit_Method(+"dotnetgraphlibrary.dotnetgraph",
               +"UpdateGraphWindow");
         when Lexer.Set_Font_Size =>
            Gen.Emit_Method(+"dotnetgraphlibrary.dotnetgraph",
               +"SetFontSize");
         when Lexer.Display_Number =>
            Gen.Emit_Method(+"dotnetgraphlibrary.dotnetgraph",
               +"Display_Text");
         when Lexer.Display_Text =>
            Gen.Emit_Method(+"dotnetgraphlibrary.dotnetgraph",
               +"Display_Text");
         when Lexer.Draw_Bitmap =>
            Gen.Emit_Method(+"dotnetgraphlibrary.dotnetgraph",
               +"Draw_Bitmap");
         when Lexer.Draw_Arc =>
            Gen.Emit_Method(+"dotnetgraphlibrary.dotnetgraph",
               +"Draw_Arc");
         when Lexer.Draw_Box =>
            Gen.Emit_Method(+"dotnetgraphlibrary.dotnetgraph",
               +"Draw_Box");
         when Lexer.Draw_Circle =>
            Gen.Emit_Method(+"dotnetgraphlibrary.dotnetgraph",
               +"Draw_Circle");
         when Lexer.Draw_Ellipse =>
            Gen.Emit_Method(+"dotnetgraphlibrary.dotnetgraph",
               +"Draw_Ellipse");
         when Lexer.Draw_Ellipse_Rotate =>
            Gen.Emit_Method(+"dotnetgraphlibrary.dotnetgraph",
               +"Draw_Ellipse_Rotate");
         when Lexer.Draw_Line =>
            Gen.Emit_Method(+"dotnetgraphlibrary.dotnetgraph",
               +"Draw_Line");
         when Lexer.Flood_Fill =>
            Gen.Emit_Method(+"dotnetgraphlibrary.dotnetgraph",
               +"Flood_Fill");
         when Lexer.Open_Graph_Window =>
            Gen.Emit_Method(+"dotnetgraphlibrary.dotnetgraph",
               +"Open_Graph_Window");
            Gen.Emit_Load_String_Const(+("RAPTORGraph" &
               ASCII.NUL));
            Gen.Emit_Method(+"dotnetgraphlibrary.dotnetgraph",
               +"Set_Window_Title");
         when Lexer.Wait_For_Key =>
            Gen.Emit_Method(+"dotnetgraphlibrary.dotnetgraph",
               +"Wait_For_Key");
         when Lexer.Wait_For_Mouse_Button =>
            Gen.Emit_Method(+"dotnetgraphlibrary.dotnetgraph",
               +"Wait_For_Mouse_Button");
         when Lexer.Put_Pixel =>
            Gen.Emit_Method(+"dotnetgraphlibrary.dotnetgraph",
               +"Put_Pixel");
         when Lexer.Set_Window_Title =>
            Gen.Emit_Method(+"dotnetgraphlibrary.dotnetgraph",
               +"Set_Window_Title");
         when Lexer.Save_Graph_Window =>
            Gen.Emit_Method(+"dotnetgraphlibrary.dotnetgraph",
               +"Save_Bitmap");
         when Lexer.Get_Mouse_Button =>
            raise Constraint_Error;
         when Lexer.Pi =>
            Gen.Emit_Load_Static(
               "numbers_pkg","pi");
         when Lexer.E =>
            Gen.Emit_Load_Static(
               "numbers_pkg","e");
         when Lexer.Black..Lexer.White =>
            Gen.Emit_Load_Number(
               Long_Float(Lexer.Token_Type'Pos(Kind)-Lexer.Token_Type'Pos(Lexer.Black)));
         when Lexer.Left_Button =>
            Gen.Emit_Load_Number(0.0);
         when Lexer.Right_Button =>
            Gen.Emit_Load_Number(1.0);
         when Lexer.Filled|Lexer.Yes|Lexer.True =>
            Gen.Emit_Load_Number(1.0);
         when Lexer.Unfilled|Lexer.No|Lexer.False =>
            Gen.Emit_Load_Number(0.0);
         when Lexer.Get_Window_Height =>
            Gen.Emit_Method(+"dotnetgraphlibrary.dotnetgraph",
               +"Get_Window_Height");
            Gen.Emit_Method(+"numbers_pkg",
               +"make_value__3");
         when Lexer.Get_Window_Width =>
            Gen.Emit_Method(+"dotnetgraphlibrary.dotnetgraph",
               +"Get_Window_Width");
            Gen.Emit_Method(+"numbers_pkg",
               +"make_value__3");
         when Lexer.Get_Font_Height =>
            Gen.Emit_Method(+"dotnetgraphlibrary.dotnetgraph",
               +"Get_Font_Height");
            Gen.Emit_Method(+"numbers_pkg",
               +"make_value__3");
         when Lexer.Get_Font_Width =>
            Gen.Emit_Method(+"dotnetgraphlibrary.dotnetgraph",
               +"Get_Font_Width");
            Gen.Emit_Method(+"numbers_pkg",
               +"make_value__3");
         when Lexer.Get_Max_Width =>
            Gen.Emit_Method(+"dotnetgraphlibrary.dotnetgraph",
               +"Get_Max_X");
            Gen.Emit_Method(+"numbers_pkg",
               +"make_value__3");
         when Lexer.Get_Max_Height =>
            Gen.Emit_Method(+"dotnetgraphlibrary.dotnetgraph",
               +"Get_Max_Y");
            Gen.Emit_Method(+"numbers_pkg",
               +"make_value__3");
         when Lexer.Get_Mouse_X =>
            Gen.Emit_Method(+"dotnetgraphlibrary.dotnetgraph",
               +"Get_Mouse_X");
            Gen.Emit_Method(+"numbers_pkg",
               +"make_value__3");
         when Lexer.Get_Mouse_Y =>
            Gen.Emit_Method(+"dotnetgraphlibrary.dotnetgraph",
               +"Get_Mouse_Y");
            Gen.Emit_Method(+"numbers_pkg",
               +"make_value__3");
         when Lexer.Get_Key =>
            Gen.Emit_Method(+"dotnetgraphlibrary.dotnetgraph",
               +"Get_Key");
            Gen.Emit_To_Integer;
            Gen.Emit_Method(+"numbers_pkg",
               +"make_value__3");
         when Lexer.Get_Key_String =>
            Gen.Emit_Method(+"dotnetgraphlibrary.dotnetgraph",
               +"Get_Key_String");
            Gen.Emit_Method(+"numbers_pkg",
               +"make_string_value");
         when Lexer.Get_Pixel =>
            Gen.Emit_Method(+"dotnetgraphlibrary.dotnetgraph",
               +"Get_Pixel");
            Gen.Emit_Method(+"numbers_pkg",
               +"make_value__3");
         when Lexer.Load_Bitmap =>
            Gen.Emit_Method(+"dotnetgraphlibrary.dotnetgraph",
               +"Load_Bitmap");
            Gen.Emit_Method(+"numbers_pkg",
               +"make_value__3");
         when Lexer.Closest_Color =>
            Gen.Emit_Method(+"dotnetgraphlibrary.dotnetgraph",
               +"Closest_Color");
            Gen.Emit_Method(+"numbers_pkg",
               +"make_value__3");
         when Lexer.Sinh =>
            Gen.Emit_Method(+"numbers_pkg",
               +"sinh");
         when Lexer.Tanh =>
            Gen.Emit_Method(+"numbers_pkg",
               +"tanh");
         when Lexer.Cosh =>
            Gen.Emit_Method(+"numbers_pkg",
               +"cosh");
         when Lexer.Arccosh =>
            Gen.Emit_Method(+"numbers_pkg",
               +"arccosh");
         when Lexer.Arcsinh =>
            Gen.Emit_Method(+"numbers_pkg",
               +"arcsinh");
         when Lexer.Arctanh =>
            Gen.Emit_Method(+"numbers_pkg",
               +"arctanh");
         when Lexer.Coth =>
            Gen.Emit_Method(+"numbers_pkg",
               +"coth");
         when Lexer.Arccoth =>
            Gen.Emit_Method(+"numbers_pkg",
               +"arccoth");
         when Lexer.Sqrt =>
            Gen.Emit_Method(+"numbers_pkg",
               +"sqrt");
         when Lexer.Floor =>
            Gen.Emit_Method(+"numbers_pkg",
               +"floor");
         when Lexer.Ceiling =>
            Gen.Emit_Method(+"numbers_pkg",
               +"ceiling");
         when Lexer.Abs_F =>
            Gen.Emit_Method(+"numbers_pkg",
               +"Oabs");
         when Lexer.Log =>
            if Parameter_Count = 1 then
               Gen.Emit_Load_Static(
                  Package_K => "numbers_pkg",
                  Field     => "e");
            end if;
               Gen.Emit_Method(+"numbers_pkg",
                  +"log");
         when Lexer.Min =>
            Gen.Emit_Method(+"numbers_pkg",
               +"min");
         when Lexer.Max =>
            Gen.Emit_Method(+"numbers_pkg",
               +"max");
         when Lexer.Sin =>
            if Parameter_Count = 1 then
               Gen.Emit_Load_Static(
                  Package_K => "numbers_pkg",
                  Field     => "two_pi");
            end if;
               Gen.Emit_Method(+"numbers_pkg",
                  +"sin");
         when Lexer.Cos =>
            if Parameter_Count = 1 then
               Gen.Emit_Load_Static(
                  Package_K => "numbers_pkg",
                  Field     => "two_pi");
            end if;
               Gen.Emit_Method(+"numbers_pkg",
                  +"cos");
         when Lexer.Tan =>
            if Parameter_Count = 1 then
               Gen.Emit_Load_Static(
                  Package_K => "numbers_pkg",
                  Field     => "two_pi");
            end if;
               Gen.Emit_Method(+"numbers_pkg",
                  +"tan");
         when Lexer.Cot =>
            if Parameter_Count = 1 then
               Gen.Emit_Load_Static(
                  Package_K => "numbers_pkg",
                  Field     => "two_pi");
            end if;
               Gen.Emit_Method(+"numbers_pkg",
                  +"cot");
         when Lexer.Arcsin =>
            if Parameter_Count = 1 then
               Gen.Emit_Load_Static(
                  Package_K => "numbers_pkg",
                  Field     => "two_pi");
            end if;
               Gen.Emit_Method(+"numbers_pkg",
                  +"arcsin");
         when Lexer.Arccos =>
            if Parameter_Count = 1 then
               Gen.Emit_Load_Static(
                  Package_K => "numbers_pkg",
                  Field     => "two_pi");
            end if;
               Gen.Emit_Method(+"numbers_pkg",
                  +"arccos");
         when Lexer.Arctan =>
            if Parameter_Count = 1 then
               Gen.Emit_Load_Static(
                  Package_K => "numbers_pkg",
                  Field     => "one");
               Gen.Emit_Load_Static(
                  Package_K => "numbers_pkg",
                  Field     => "two_pi");
            elsif Parameter_Count = 2 then
               Gen.Emit_Load_Static(
                  Package_K => "numbers_pkg",
                  Field     => "two_pi");
            end if;
               Gen.Emit_Method(+"numbers_pkg",
                  +"arctan");
         when Lexer.Arccot =>
            if Parameter_Count = 1 then
               Gen.Emit_Load_Static(
                  Package_K => "numbers_pkg",
                  Field     => "one");
               Gen.Emit_Load_Static(
                  Package_K => "numbers_pkg",
                  Field     => "two_pi");
            elsif Parameter_Count = 2 then
               Gen.Emit_Load_Static(
                  Package_K => "numbers_pkg",
                  Field     => "two_pi");
            end if;
               Gen.Emit_Method(+"numbers_pkg",
               +"arccot");
         when Is_Open =>
            Gen.Emit_Method("dotnetgraphlibrary.dotnetgraph",
               "Is_Open");
         when Key_Hit =>
            Gen.Emit_Method("dotnetgraphlibrary.dotnetgraph",
               "Key_Hit");
         when Key_Down =>
            Gen.Emit_Method("dotnetgraphlibrary.dotnetgraph",
               "Key_Down_String");
         when End_Of_Input =>
            Gen.Emit_Method("raptor.Runtime",
               "End_Of_Input");
         when Mouse_Button_Down =>
            Gen.Emit_Method("dotnetgraphlibrary.dotnetgraph",
               "Mouse_Button_Down");
         when Mouse_Button_Pressed =>
            Gen.Emit_Method("dotnetgraphlibrary.dotnetgraph",
               "Mouse_Button_Pressed");
         when Mouse_Button_Released =>
            Gen.Emit_Method("dotnetgraphlibrary.dotnetgraph",
               "Mouse_Button_Released");
      end case;
   end Emit_Method_Call_IL;

   procedure Emit_Code (This : in Proc_Call; Gen : access Generate_Interface.Typ'Class) is
      Kind : Lexer.Proc_Token_Types := This.Id.Kind;
      Int_Kind : Integer := Lexer.Proc_Token_Types'Pos(Kind);
      O : access MSSyst.Object.Typ'Class;
      Illegal_Code : exception;

   begin
      -- added 2/13/07 for security
      if Kind=Lexer.Open_Graph_Window and Raptor_Files.Network_Redirected then
         raise illegal_code with "Can't use graphics when testing against server";
      end if;
      if Kind/=Lexer.Delay_For then
         O := Gen.Emit_Call_Method(Int_Kind);
      end if;
      case Kind is
         when Lexer.Delay_For =>
            Gen.Emit_Sleep;
            Emit_Parameter_Number(This.Param_List.Parameter,Gen);
            Gen.Emit_Past_Sleep;
         when Lexer.Redirect_Input =>
            if This.Param_List.Parameter.All in String_Output'Class then
               Emit_Parameter_String(This.Param_List.Parameter,Gen);
            else
               Emit_Parameter_Number(This.Param_List.Parameter,Gen);
            end if;
            Gen.Emit_Last_Parameter(O);
         when Lexer.Set_Precision=>
            Gen.Emit_Conversion(Conversions'Pos(To_Integer));
            Emit_Parameter_Number(This.Param_List.Parameter,Gen);
            Gen.Emit_End_Conversion(Conversions'Pos(To_Integer));
            Gen.Emit_Last_Parameter(O);
         when Lexer.Redirect_Output|Lexer.Redirect_Output_Append =>
            if This.Param_List.Parameter.All in String_Output'Class then
               Emit_Parameter_String(This.Param_List.Parameter,Gen);
            else
               Emit_Parameter_Number(This.Param_List.Parameter,Gen);
            end if;
            Gen.Emit_Last_Parameter(O);
         when Lexer.Clear_Console =>
            Gen.Emit_No_Parameters(O);
         when Lexer.Clear_Window =>
            Gen.Emit_Conversion(Conversions'Pos(To_Color));
            Emit_Parameter_Number(This.Param_List.Parameter,Gen);
            Gen.Emit_End_Conversion(Conversions'Pos(To_Color));
            Gen.Emit_Last_Parameter(O);
         when Lexer.Close_Graph_Window =>
            Gen.Emit_No_Parameters(O);
         when Lexer.Freeze_Graph_Window =>
            Gen.Emit_No_Parameters(O);
         when Lexer.Unfreeze_Graph_Window =>
            Gen.Emit_No_Parameters(O);
         when Lexer.Update_Graph_Window =>
            Gen.Emit_No_Parameters(O);
         when Lexer.Set_Font_Size =>
            Gen.Emit_Conversion(Conversions'Pos(To_Integer));
            Emit_Parameter_Number(This.Param_List.Parameter,Gen);
            Gen.Emit_End_Conversion(Conversions'Pos(To_Integer));
            Gen.Emit_Last_Parameter(O);
         when Lexer.Display_Number =>
            Gen.Emit_Conversion(Conversions'Pos(To_Integer));
            Emit_Parameter_Number(This.Param_List.Parameter,Gen);
            Gen.Emit_End_Conversion(Conversions'Pos(To_Integer));
            Gen.Emit_Next_Parameter(O);
            Gen.Emit_Conversion(Conversions'Pos(To_Integer));
            Emit_Parameter_Number(This.Param_List.Next.Parameter,Gen);
            Gen.Emit_End_Conversion(Conversions'Pos(To_Integer));
            Gen.Emit_Next_Parameter(O);
            Gen.Emit_Conversion(Conversions'Pos(Number_To_String));
            Emit_Parameter_Number(This.Param_List.Next.Next.Parameter,Gen);
            Gen.Emit_End_Conversion(Conversions'Pos(Number_To_String));
            Gen.Emit_Next_Parameter(O);
            Gen.Emit_Conversion(Conversions'Pos(To_Color));
            Emit_Parameter_Number(This.Param_List.Next.Next.Next.Parameter,Gen);
            Gen.Emit_End_Conversion(Conversions'Pos(To_Color));
            Gen.Emit_Last_Parameter(O);
         when Lexer.Display_Text =>
            Gen.Emit_Conversion(Conversions'Pos(To_Integer));
            Emit_Parameter_Number(This.Param_List.Parameter,Gen);
            Gen.Emit_End_Conversion(Conversions'Pos(To_Integer));
            Gen.Emit_Next_Parameter(O);
            Gen.Emit_Conversion(Conversions'Pos(To_Integer));
            Emit_Parameter_Number(This.Param_List.Next.Parameter,Gen);
            Gen.Emit_End_Conversion(Conversions'Pos(To_Integer));
            Gen.Emit_Next_Parameter(O);
            Gen.Emit_Conversion(Conversions'Pos(To_String));
            Emit_Parameter_Number(This.Param_List.Next.Next.Parameter,Gen);
            Gen.Emit_End_Conversion(Conversions'Pos(To_String));
            Gen.Emit_Next_Parameter(O);
            Gen.Emit_Conversion(Conversions'Pos(To_Color));
            Emit_Parameter_Number(This.Param_List.Next.Next.Next.Parameter,Gen);
            Gen.Emit_End_Conversion(Conversions'Pos(To_Color));
            Gen.Emit_Last_Parameter(O);
         when Lexer.Draw_Bitmap =>
            Gen.Emit_Conversion(Conversions'Pos(To_Integer));
            Emit_Parameter_Number(This.Param_List.Parameter,Gen);
            Gen.Emit_End_Conversion(Conversions'Pos(To_Integer));
            Gen.Emit_Next_Parameter(O);
            Gen.Emit_Conversion(Conversions'Pos(To_Integer));
            Emit_Parameter_Number(This.Param_List.Next.Parameter,Gen);
            Gen.Emit_End_Conversion(Conversions'Pos(To_Integer));
            Gen.Emit_Next_Parameter(O);
            Gen.Emit_Conversion(Conversions'Pos(To_Integer));
            Emit_Parameter_Number(This.Param_List.Next.Next.Parameter,Gen);
            Gen.Emit_End_Conversion(Conversions'Pos(To_Integer));
            Gen.Emit_Next_Parameter(O);
            Gen.Emit_Conversion(Conversions'Pos(To_Integer));
            Emit_Parameter_Number(This.Param_List.Next.Next.Next.Parameter,Gen);
            Gen.Emit_End_Conversion(Conversions'Pos(To_Integer));
            Gen.Emit_Next_Parameter(O);
            Gen.Emit_Conversion(Conversions'Pos(To_Integer));
            Emit_Parameter_Number(This.Param_List.Next.Next.Next.Next.Parameter,Gen);
            Gen.Emit_End_Conversion(Conversions'Pos(To_Integer));
            Gen.Emit_Last_Parameter(O);
         when Lexer.Draw_Arc =>
            Gen.Emit_Conversion(Conversions'Pos(To_Integer));
            Emit_Parameter_Number(This.Param_List.Parameter,Gen);
            Gen.Emit_End_Conversion(Conversions'Pos(To_Integer));
            Gen.Emit_Next_Parameter(O);
            Gen.Emit_Conversion(Conversions'Pos(To_Integer));
            Emit_Parameter_Number(This.Param_List.Next.Parameter,Gen);
            Gen.Emit_End_Conversion(Conversions'Pos(To_Integer));
            Gen.Emit_Next_Parameter(O);
            Gen.Emit_Conversion(Conversions'Pos(To_Integer));
            Emit_Parameter_Number(This.Param_List.Next.Next.Parameter,Gen);
            Gen.Emit_End_Conversion(Conversions'Pos(To_Integer));
            Gen.Emit_Next_Parameter(O);
            Gen.Emit_Conversion(Conversions'Pos(To_Integer));
            Emit_Parameter_Number(This.Param_List.Next.Next.Next.Parameter,Gen);
            Gen.Emit_End_Conversion(Conversions'Pos(To_Integer));
            Gen.Emit_Next_Parameter(O);
            Gen.Emit_Conversion(Conversions'Pos(To_Integer));
            Emit_Parameter_Number(This.Param_List.Next.Next.Next.Next.Parameter,Gen);
            Gen.Emit_End_Conversion(Conversions'Pos(To_Integer));
            Gen.Emit_Next_Parameter(O);
            Gen.Emit_Conversion(Conversions'Pos(To_Integer));
            Emit_Parameter_Number(This.Param_List.Next.Next.Next.Next.Next.Parameter,Gen);
            Gen.Emit_End_Conversion(Conversions'Pos(To_Integer));
            Gen.Emit_Next_Parameter(O);
            Gen.Emit_Conversion(Conversions'Pos(To_Integer));
            Emit_Parameter_Number(This.Param_List.Next.Next.Next.Next.Next.Next.Parameter,Gen);
            Gen.Emit_End_Conversion(Conversions'Pos(To_Integer));
            Gen.Emit_Next_Parameter(O);
            Gen.Emit_Conversion(Conversions'Pos(To_Integer));
            Emit_Parameter_Number(This.Param_List.Next.Next.Next.Next.Next.Next.Next.Parameter,Gen);
            Gen.Emit_End_Conversion(Conversions'Pos(To_Integer));
            Gen.Emit_Next_Parameter(O);
            Gen.Emit_Conversion(Conversions'Pos(To_Color));
            Emit_Parameter_Number(This.Param_List.Next.Next.Next.Next.Next.Next.Next.Next.Parameter,Gen);
            Gen.Emit_End_Conversion(Conversions'Pos(To_Color));
            Gen.Emit_Last_Parameter(O);
         when Lexer.Draw_Box =>
            Gen.Emit_Conversion(Conversions'Pos(To_Integer));
            Emit_Parameter_Number(This.Param_List.Parameter,Gen);
            Gen.Emit_End_Conversion(Conversions'Pos(To_Integer));
            Gen.Emit_Next_Parameter(O);
            Gen.Emit_Conversion(Conversions'Pos(To_Integer));
            Emit_Parameter_Number(This.Param_List.Next.Parameter,Gen);
            Gen.Emit_End_Conversion(Conversions'Pos(To_Integer));
            Gen.Emit_Next_Parameter(O);
            Gen.Emit_Conversion(Conversions'Pos(To_Integer));
            Emit_Parameter_Number(This.Param_List.Next.Next.Parameter,Gen);
            Gen.Emit_End_Conversion(Conversions'Pos(To_Integer));
            Gen.Emit_Next_Parameter(O);
            Gen.Emit_Conversion(Conversions'Pos(To_Integer));
            Emit_Parameter_Number(This.Param_List.Next.Next.Next.Parameter,Gen);
            Gen.Emit_End_Conversion(Conversions'Pos(To_Integer));
            Gen.Emit_Next_Parameter(O);
            Gen.Emit_Conversion(Conversions'Pos(To_Color));
            Emit_Parameter_Number(This.Param_List.Next.Next.Next.Next.Parameter,Gen);
            Gen.Emit_End_Conversion(Conversions'Pos(To_Color));
            Gen.Emit_Next_Parameter(O);
            Gen.Emit_Conversion(Conversions'Pos(To_Bool));
            Emit_Parameter_Number(This.Param_List.Next.Next.Next.Next.Next.Parameter,Gen);
            Gen.Emit_End_Conversion(Conversions'Pos(To_Bool));
            Gen.Emit_Last_Parameter(O);
         when Lexer.Draw_Circle =>
            Gen.Emit_Conversion(Conversions'Pos(To_Integer));
            Emit_Parameter_Number(This.Param_List.Parameter,Gen);
            Gen.Emit_End_Conversion(Conversions'Pos(To_Integer));
            Gen.Emit_Next_Parameter(O);
            Gen.Emit_Conversion(Conversions'Pos(To_Integer));
            Emit_Parameter_Number(This.Param_List.Next.Parameter,Gen);
            Gen.Emit_End_Conversion(Conversions'Pos(To_Integer));
            Gen.Emit_Next_Parameter(O);
            Gen.Emit_Conversion(Conversions'Pos(To_Integer));
            Emit_Parameter_Number(This.Param_List.Next.Next.Parameter,Gen);
            Gen.Emit_End_Conversion(Conversions'Pos(To_Integer));
            Gen.Emit_Next_Parameter(O);
            Gen.Emit_Conversion(Conversions'Pos(To_Color));
            Emit_Parameter_Number(This.Param_List.Next.Next.Next.Parameter,Gen);
            Gen.Emit_End_Conversion(Conversions'Pos(To_Color));
            Gen.Emit_Next_Parameter(O);
            Gen.Emit_Conversion(Conversions'Pos(To_Bool));
            Emit_Parameter_Number(This.Param_List.Next.Next.Next.Next.Parameter,Gen);
            Gen.Emit_End_Conversion(Conversions'Pos(To_Bool));
            Gen.Emit_Last_Parameter(O);
         when Lexer.Draw_Ellipse =>
            Gen.Emit_Conversion(Conversions'Pos(To_Integer));
            Emit_Parameter_Number(This.Param_List.Parameter,Gen);
            Gen.Emit_End_Conversion(Conversions'Pos(To_Integer));
            Gen.Emit_Next_Parameter(O);
            Gen.Emit_Conversion(Conversions'Pos(To_Integer));
            Emit_Parameter_Number(This.Param_List.Next.Parameter,Gen);
            Gen.Emit_End_Conversion(Conversions'Pos(To_Integer));
            Gen.Emit_Next_Parameter(O);
            Gen.Emit_Conversion(Conversions'Pos(To_Integer));
            Emit_Parameter_Number(This.Param_List.Next.Next.Parameter,Gen);
            Gen.Emit_End_Conversion(Conversions'Pos(To_Integer));
            Gen.Emit_Next_Parameter(O);
            Gen.Emit_Conversion(Conversions'Pos(To_Integer));
            Emit_Parameter_Number(This.Param_List.Next.Next.Next.Parameter,Gen);
            Gen.Emit_End_Conversion(Conversions'Pos(To_Integer));
            Gen.Emit_Next_Parameter(O);
            Gen.Emit_Conversion(Conversions'Pos(To_Color));
            Emit_Parameter_Number(This.Param_List.Next.Next.Next.Next.Parameter,Gen);
            Gen.Emit_End_Conversion(Conversions'Pos(To_Color));
            Gen.Emit_Next_Parameter(O);
            Gen.Emit_Conversion(Conversions'Pos(To_Bool));
            Emit_Parameter_Number(This.Param_List.Next.Next.Next.Next.Next.Parameter,Gen);
            Gen.Emit_End_Conversion(Conversions'Pos(To_Bool));
            Gen.Emit_Last_Parameter(O);
         when Lexer.Draw_Ellipse_Rotate =>
            Gen.Emit_Conversion(Conversions'Pos(To_Integer));
            Emit_Parameter_Number(This.Param_List.Parameter,Gen);
            Gen.Emit_End_Conversion(Conversions'Pos(To_Integer));
            Gen.Emit_Next_Parameter(O);
            Gen.Emit_Conversion(Conversions'Pos(To_Integer));
            Emit_Parameter_Number(This.Param_List.Next.Parameter,Gen);
            Gen.Emit_End_Conversion(Conversions'Pos(To_Integer));
            Gen.Emit_Next_Parameter(O);
            Gen.Emit_Conversion(Conversions'Pos(To_Integer));
            Emit_Parameter_Number(This.Param_List.Next.Next.Parameter,Gen);
            Gen.Emit_End_Conversion(Conversions'Pos(To_Integer));
            Gen.Emit_Next_Parameter(O);
            Gen.Emit_Conversion(Conversions'Pos(To_Integer));
            Emit_Parameter_Number(This.Param_List.Next.Next.Next.Parameter,Gen);
            Gen.Emit_End_Conversion(Conversions'Pos(To_Integer));
            Gen.Emit_Next_Parameter(O);
            Gen.Emit_Conversion(Conversions'Pos(To_Float));
            Emit_Parameter_Number(This.Param_List.Next.Next.Next.Next.Parameter,Gen);
            Gen.Emit_End_Conversion(Conversions'Pos(To_Float));
            Gen.Emit_Next_Parameter(O);
            Gen.Emit_Conversion(Conversions'Pos(To_Color));
            Emit_Parameter_Number(This.Param_List.Next.Next.Next.Next.Next.Parameter,Gen);
            Gen.Emit_End_Conversion(Conversions'Pos(To_Color));
            Gen.Emit_Next_Parameter(O);
            Gen.Emit_Conversion(Conversions'Pos(To_Bool));
            Emit_Parameter_Number(This.Param_List.Next.Next.Next.Next.Next.Next.Parameter,Gen);
            Gen.Emit_End_Conversion(Conversions'Pos(To_Bool));
            Gen.Emit_Last_Parameter(O);
         when Lexer.Draw_Line =>
            Gen.Emit_Conversion(Conversions'Pos(To_Integer));
            Emit_Parameter_Number(This.Param_List.Parameter,Gen);
            Gen.Emit_End_Conversion(Conversions'Pos(To_Integer));
            Gen.Emit_Next_Parameter(O);
            Gen.Emit_Conversion(Conversions'Pos(To_Integer));
            Emit_Parameter_Number(This.Param_List.Next.Parameter,Gen);
            Gen.Emit_End_Conversion(Conversions'Pos(To_Integer));
            Gen.Emit_Next_Parameter(O);
            Gen.Emit_Conversion(Conversions'Pos(To_Integer));
            Emit_Parameter_Number(This.Param_List.Next.Next.Parameter,Gen);
            Gen.Emit_End_Conversion(Conversions'Pos(To_Integer));
            Gen.Emit_Next_Parameter(O);
            Gen.Emit_Conversion(Conversions'Pos(To_Integer));
            Emit_Parameter_Number(This.Param_List.Next.Next.Next.Parameter,Gen);
            Gen.Emit_End_Conversion(Conversions'Pos(To_Integer));
            Gen.Emit_Next_Parameter(O);
            Gen.Emit_Conversion(Conversions'Pos(To_Color));
            Emit_Parameter_Number(This.Param_List.Next.Next.Next.Next.Parameter,Gen);
            Gen.Emit_End_Conversion(Conversions'Pos(To_Color));
            Gen.Emit_Last_Parameter(O);
         when Lexer.Flood_Fill =>
            Gen.Emit_Conversion(Conversions'Pos(To_Integer));
            Emit_Parameter_Number(This.Param_List.Parameter,Gen);
            Gen.Emit_End_Conversion(Conversions'Pos(To_Integer));
            Gen.Emit_Next_Parameter(O);
            Gen.Emit_Conversion(Conversions'Pos(To_Integer));
            Emit_Parameter_Number(This.Param_List.Next.Parameter,Gen);
            Gen.Emit_End_Conversion(Conversions'Pos(To_Integer));
            Gen.Emit_Next_Parameter(O);
            Gen.Emit_Conversion(Conversions'Pos(To_Color));
            Emit_Parameter_Number(This.Param_List.Next.Next.Parameter,Gen);
            Gen.Emit_End_Conversion(Conversions'Pos(To_Color));
            Gen.Emit_Last_Parameter(O);
         when Lexer.Open_Graph_Window =>
            Gen.Emit_Conversion(Conversions'Pos(To_Integer));
            Emit_Parameter_Number(This.Param_List.Parameter,Gen);
            Gen.Emit_End_Conversion(Conversions'Pos(To_Integer));
            Gen.Emit_Next_Parameter(O);
            Gen.Emit_Conversion(Conversions'Pos(To_Integer));
            Emit_Parameter_Number(This.Param_List.Next.Parameter,Gen);
            Gen.Emit_End_Conversion(Conversions'Pos(To_Integer));
            Gen.Emit_Last_Parameter(O);
         when Lexer.Wait_For_Key =>
            Gen.Emit_No_Parameters(O);
         when Lexer.Wait_For_Mouse_Button =>
            Gen.Emit_Conversion(Conversions'Pos(To_Integer));
            Emit_Parameter_Number(This.Param_List.Parameter,Gen);
            Gen.Emit_End_Conversion(Conversions'Pos(To_Integer));
            Gen.Emit_Last_Parameter(O);
         when Lexer.Put_Pixel =>
            Gen.Emit_Conversion(Conversions'Pos(To_Integer));
            Emit_Parameter_Number(This.Param_List.Parameter,Gen);
            Gen.Emit_End_Conversion(Conversions'Pos(To_Integer));
            Gen.Emit_Next_Parameter(O);
            Gen.Emit_Conversion(Conversions'Pos(To_Integer));
            Emit_Parameter_Number(This.Param_List.Next.Parameter,Gen);
            Gen.Emit_Conversion(Conversions'Pos(To_Integer));
            Gen.Emit_Next_Parameter(O);
            Gen.Emit_End_Conversion(Conversions'Pos(To_Color));
            Emit_Parameter_Number(This.Param_List.Next.Next.Parameter,Gen);
            Gen.Emit_End_Conversion(Conversions'Pos(To_Color));
            Gen.Emit_Last_Parameter(O);
         when Lexer.Set_Window_Title =>
            Gen.Emit_Conversion(Conversions'Pos(To_String));
            Emit_Parameter_Number(This.Param_List.Parameter,Gen);
            Gen.Emit_End_Conversion(Conversions'Pos(To_String));
            Gen.Emit_Last_Parameter(O);
         when Lexer.Save_Graph_Window =>
            Gen.Emit_Conversion(Conversions'Pos(To_String));
            Emit_Parameter_Number(This.Param_List.Parameter,Gen);
            Gen.Emit_End_Conversion(Conversions'Pos(To_String));
            Gen.Emit_Last_Parameter(O);
         when Lexer.Get_Mouse_Button =>
            Gen.Emit_Conversion(Conversions'Pos(To_Integer));
            Emit_Parameter_Number(This.Param_List.Parameter,Gen);
            Gen.Emit_End_Conversion(Conversions'Pos(To_Integer));
            -- don't call emit_last_parameter b/c special
            Gen.Emit_Get_Mouse_Button;
            Emit_Store_Dotnetgraph_Int_Func(This.Param_List.Next.Parameter,Gen,
               0);
            Emit_Store_Dotnetgraph_Int_Func(This.Param_List.Next.Next.Parameter,Gen,
               1);
      end case;
   end Emit_Code;
   procedure Emit_Code (This : in Plugin_Proc_Call; Gen : access Generate_Interface.Typ'Class) is
      T : Token_Pointer;
      M_Str : access MSSyst.String.Typ;
   begin
      T := This.Id;
      M_Str := +Get_Text(T.Start..T.Finish);
      Gen.Emit_Plugin_Call(M_Str, This.Param_List);
      --Raptor.Plugins.Emit_Invoke_Function(M_Str,This.Param_List,Gen);
   end Emit_Code;
   procedure Emit_Code (This : in Tabid_Proc_Call; Gen : access Generate_Interface.Typ'Class) is
   begin
      null;
   end Emit_Code;
   procedure Emit_Code (This : in Expr_Assignment; Gen : access Generate_Interface.Typ'Class) is
   begin
      Emit_Context := Assign_Context;
      IF (Gen.all IN Generate_Interface_OO.Typ'Class) THEN
         Gen.Indent;
      END IF;

      Emit_Code(This.Lhs.All,This.Lsuffix,Gen);
      Emit_Code(This.LSuffix.all,Gen);

      Emit_Code(This.Expr_Part.All,Gen);
      case Emit_Kind is
         when Variable =>
            Gen.Variable_Assignment_PastRHS;
         when Array_1D =>
            Gen.Array_1D_Assignment_PastRHS;
         when Array_2D =>
            Gen.Array_2D_Assignment_PastRHS;
      end case;
   end Emit_Code;
--   procedure Emit_Code (This : in Array_Assignment; Gen : access Generate_Interface.Typ'Class) is
--      T : Lexer.Token_Pointer;
--   begin
--      T := This.Id;
--      Gen.Array_1D_Assignment_Start(Get_Text(T.Start..T.Finish));
--      Emit_Code(This.Reference.All,Gen);
--      Gen.Array_1D_Assignment_After_Index;
--      Emit_Code(This.Expr_Part.all,Gen);
--      Gen.Array_1D_Assignment_PastRHS;
--   end Emit_Code;
--   procedure Emit_Code (This : in Array_2D_Assignment; Gen : access Generate_Interface.Typ'Class) is
--      T : Lexer.Token_Pointer;
--   begin
--      T := This.Id;
--      Gen.Array_2D_Assignment_Start(Get_Text(T.Start..T.Finish));
--      Emit_Code(This.Reference.all,Gen);
--      Gen.Array_2D_Assignment_Between_Indices;
--      Emit_Code(This.Reference2.all,Gen);
--      Gen.Array_2D_Assignment_After_Indices;
--      Emit_Code(This.Expr_Part.all,Gen);
--      Gen.Array_2D_Assignment_PastRHS;
--   end Emit_Code;
   procedure Emit_Code (This : in Number_Expon; Gen : access Generate_Interface.Typ'Class) is
      T : Lexer.Token_Pointer;
   begin
      T := This.Number;
      Gen.Emit_Load_Number(Long_Float'Value(
            Get_Text(T.Start..T.Finish)));
   end Emit_Code;
   procedure Emit_Code (This : in Negative_Expon; Gen : access Generate_Interface.Typ'Class) is
   begin
      if Gen.Is_Postfix then
         Emit_Code(This.E.all,Gen);
         Gen.Emit_Unary_Minus;
      else
         Gen.Emit_Unary_Minus;
         Emit_Code(This.E.All,Gen);
      end if;
   end Emit_Code;

   procedure Emit_Code (This : in String_Expon; Gen : access Generate_Interface.Typ'Class) is
      T : Lexer.Token_Pointer;
   begin
      T := This.S;
      Gen.Emit_Load_String(Dequote(Get_Text(T.Start+1..T.Finish-1)));
   end Emit_Code;
   procedure Emit_Code (This : in Character_Expon; Gen : access Generate_Interface.Typ'Class) is
      T : Lexer.Token_Pointer := This.S;
      S : String := Get_Text(T.Start+1..T.Finish-1);
   begin
      Gen.Emit_Load_Character(Ada.Characters.Conversions.To_Wide_Character(S(S'First)));
   end Emit_Code;


   procedure Emit_Code (This : in Paren_Expon; Gen : access Generate_Interface.Typ'Class) is
   begin
      Gen.Emit_Left_Paren;
      Emit_Code(This.Expr_Part.all,Gen);
      Gen.Emit_Right_Paren;
   end Emit_Code;
   procedure Emit_Code (This : in Func0_Expon; Gen : access Generate_Interface.Typ'Class) is
      Kind : Lexer.Func_Id0 := This.Id.Kind;
      Int_Kind : Integer := Lexer.Func_Id0'Pos(Kind);
      O : access MSSyst.Object.Typ'Class;
   begin
      if Kind = Lexer.Random then
         Gen.Emit_Random;
      elsif Kind= Lexer.Random_Color then
         Gen.Emit_Random_2(0.0,16.0);
      elsif Kind=Lexer.Random_Extended_Color then
         Gen.Emit_Random_2(0.0,242.0);
      else
         O := Gen.Emit_Call_Method(Int_Kind);
         Gen.Emit_No_Parameters(O);
      end if;

   end Emit_Code;

   procedure Emit_Length_Of (This : in Output_Pointer; Gen : access Generate_Interface.Typ'Class) is
      Expr1 : Expression_Pointer;
      Add1  : Value_Parseable_Pointer;
      Mult1 : Value_Parseable_Pointer;
      Expon1 : Value_Parseable_Pointer;
      T : Lexer.Token_Pointer;
      use type ada.tags.tag;
   begin
      Expr1 := Expr_Output(This.All).Expr;
      if Expr1.All in Add_Expression'Class or
            Expr1.All in Minus_Expression'Class then
         Gen.Emit_Conversion(Conversions'Pos(To_String));
         Emit_Code(This => Expr1.All, Gen => Gen);
         Gen.Emit_End_Conversion(Conversions'Pos(To_String));
         Gen.Emit_String_Length;
         return;
         --Raise_Exception(Runtime_Error'Identity,"Can only take length of string or 1D array.");
      end if;
      Add1 := Expr1.Left;
      if Add1.All in Div_Add'Class or
            Add1.All in Mod_Add'Class or
            Add1.All in Rem_Add'Class or
            Add1.All in Mult_Add'Class then
         Raise_Exception(Runtime_Error'Identity,"Can only take length of string or 1D array.");
      end if;
      Mult1 := Add_Pointer(Add1).Left;
      if Mult1.All in Expon_Mult'Class then
         Raise_Exception(Runtime_Error'Identity,"Can only take length of string or 1D array.");
      end if;
      Expon1 := Mult_Pointer(Mult1).Left;
      if Expon1.All in String_Expon'Class then
         T := String_Expon(Expon1.all).S;
         Gen.Emit_Load_Number(Long_Float(T.Finish-T.Start+1-2));
         return;
      end if;
      if Expon1.All in Func0_Expon'Class or
            Expon1.All in Func_Expon'Class or
            Expon1.All in Plugin_Func_Expon'Class then
         Raise_Exception(Runtime_Error'Identity,"Can only take length of string or 1D array.");
      end if;
      if not(Expon1.All in Rhs_Expon'Class) then
         Raise_Exception(Runtime_Error'Identity,"Can only take length of string or 1D array.");
      end if;
      if not (Rhs_Expon(Expon1.All).RSuffix.all in Empty_Rsuffix'Class) then
         Raise_Exception(Runtime_Error'Identity,"Can only take length of local variable.");
      end if;

      if not(Rhs_Expon(Expon1.All).Rhs'Tag = Id_Rhs'Tag) then
         Raise_Exception(Runtime_Error'Identity,"Can only take length of string or 1D array.");
      end if;
      T := Id_Rhs(Rhs_Expon(Expon1.All).Rhs.all).Id;
      Gen.Emit_Array_Size(
         +(Get_Text(T.Start..T.Finish)));
   end Emit_Length_Of;
   procedure Emit_Code (This : in Func_Expon; Gen : access Generate_Interface.Typ'Class) is
      Function_Name : Lexer.Func_Token_Types;
      Int_Kind : Integer;
      O : access MSSyst.Object.Typ'Class;
   begin
      Function_Name := This.Id.Kind;
      Int_Kind := Lexer.Func_Token_Types'Pos(Function_Name);
      if Function_Name /= Lexer.Length_Of and
            Function_Name /= Lexer.To_Ascii and
            Function_Name /= Lexer.to_character then
         O := Gen.Emit_Call_Method(Int_Kind);
      end if;

      case Function_Name is
         when Lexer.Func_Id0 =>
            -- should be handled above in Func0_Expon
            raise Constraint_Error;
         when Lexer.To_Ascii =>
            Gen.Emit_Conversion(Conversions'Pos(Char_To_Int));
            Emit_Parameter_Number(This.Parameters.Parameter,Gen);
            Gen.Emit_End_Conversion(Conversions'Pos(Char_To_Int));
            --don't call emit_last_parameter for to_ascii
            --Gen.Emit_Last_Parameter(O);

         when Lexer.To_Character =>
            Gen.Emit_Conversion(Conversions'Pos(Int_To_Char));
            Emit_Parameter_Number(This.Parameters.Parameter,Gen);
            Gen.Emit_End_Conversion(Conversions'Pos(Int_To_Char));
            --don't call emit_last_parameter for to_character
            --Gen.Emit_Last_Parameter(O);

         when Lexer.Length_Of =>
            Emit_Length_Of(This.Parameters.Parameter,Gen);
            --don't call emit_last_parameter for length_of
            --Gen.Emit_Last_Parameter(O);
         when Lexer.Get_Pixel =>
            Gen.Emit_Conversion(Conversions'Pos(To_Integer));
            Emit_Parameter_Number(This.Parameters.Parameter,Gen);
            Gen.Emit_End_Conversion(Conversions'Pos(To_Integer));
            Gen.Emit_Next_Parameter(O);
            Gen.Emit_Conversion(Conversions'Pos(To_Integer));
            Emit_Parameter_Number(This.Parameters.Next.Parameter,Gen);
            Gen.Emit_End_Conversion(Conversions'Pos(To_Integer));
            Gen.Emit_Last_Parameter(O);
         when Lexer.Load_Bitmap =>
            Gen.Emit_Conversion(Conversions'Pos(To_String));
            Emit_Parameter_Number(This.Parameters.Parameter,Gen);
            Gen.Emit_End_Conversion(Conversions'Pos(To_String));
            Gen.Emit_Last_Parameter(O);
         when Lexer.Closest_Color =>
            Gen.Emit_Conversion(Conversions'Pos(To_Integer));
            Emit_Parameter_Number(This.Parameters.Parameter,Gen);
            Gen.Emit_End_Conversion(Conversions'Pos(To_Integer));
            Gen.Emit_Next_Parameter(O);
            Gen.Emit_Conversion(Conversions'Pos(To_Integer));
            Emit_Parameter_Number(This.Parameters.Next.Parameter,Gen);
            Gen.Emit_End_Conversion(Conversions'Pos(To_Integer));
            Gen.Emit_Next_Parameter(O);
            Gen.Emit_Conversion(Conversions'Pos(To_Integer));
            Emit_Parameter_Number(This.Parameters.Next.Next.Parameter,Gen);
            Gen.Emit_End_Conversion(Conversions'Pos(To_Integer));
            Gen.Emit_Last_Parameter(O);
         when Lexer.Sinh =>
            Emit_Parameter_Number(This.Parameters.Parameter,Gen);
            Gen.Emit_Last_Parameter(O);
         when Lexer.Tanh =>
            Emit_Parameter_Number(This.Parameters.Parameter,Gen);
            Gen.Emit_Last_Parameter(O);
         when Lexer.Cosh =>
            Emit_Parameter_Number(This.Parameters.Parameter,Gen);
            Gen.Emit_Last_Parameter(O);
         when Lexer.Arccosh =>
            Emit_Parameter_Number(This.Parameters.Parameter,Gen);
            Gen.Emit_Last_Parameter(O);
         when Lexer.Arcsinh =>
            Emit_Parameter_Number(This.Parameters.Parameter,Gen);
            Gen.Emit_Last_Parameter(O);
         when Lexer.Arctanh =>
            Emit_Parameter_Number(This.Parameters.Parameter,Gen);
            Gen.Emit_Last_Parameter(O);
         when Lexer.Coth =>
            Emit_Parameter_Number(This.Parameters.Parameter,Gen);
            Gen.Emit_Last_Parameter(O);
         when Lexer.Arccoth =>
            Emit_Parameter_Number(This.Parameters.Parameter,Gen);
            Gen.Emit_Last_Parameter(O);
         when Lexer.Sqrt =>
            Emit_Parameter_Number(This.Parameters.Parameter,Gen);
            Gen.Emit_Last_Parameter(O);
         when Lexer.Floor =>
            Emit_Parameter_Number(This.Parameters.Parameter,Gen);
            Gen.Emit_Last_Parameter(O);
         when Lexer.Ceiling =>
            Emit_Parameter_Number(This.Parameters.Parameter,Gen);
            Gen.Emit_Last_Parameter(O);
         when Lexer.Abs_F =>
            Emit_Parameter_Number(This.Parameters.Parameter,Gen);
            Gen.Emit_Last_Parameter(O);
         when Lexer.Log =>
            if Parser.Count_Parameters(This.Parameters) = 1 then
               Emit_Parameter_Number(This.Parameters.Parameter,Gen);
            elsif Parser.Count_Parameters(This.Parameters) = 2 then
               Emit_Parameter_Number(This.Parameters.Parameter,Gen);
               Gen.Emit_Next_Parameter(O);
               Emit_Parameter_Number(This.Parameters.Next.Parameter,Gen);
            end if;
            Gen.Emit_Last_Parameter(O);
         when Lexer.Min =>
            Emit_Parameter_Number(This.Parameters.Parameter,Gen);
            Gen.Emit_Next_Parameter(O);
            Emit_Parameter_Number(This.Parameters.Next.Parameter,Gen);
            Gen.Emit_Last_Parameter(O);
         when Lexer.Max =>
            Emit_Parameter_Number(This.Parameters.Parameter,Gen);
            Gen.Emit_Next_Parameter(O);
            Emit_Parameter_Number(This.Parameters.Next.Parameter,Gen);
            Gen.Emit_Last_Parameter(O);
         when Lexer.Sin =>
            if Parser.Count_Parameters(This.Parameters) = 1 then
               Emit_Parameter_Number(This.Parameters.Parameter,Gen);
            elsif Parser.Count_Parameters(This.Parameters) = 2 then
               Emit_Parameter_Number(This.Parameters.Parameter,Gen);
               Gen.Emit_Next_Parameter(O);
               Emit_Parameter_Number(This.Parameters.Next.Parameter,Gen);
            end if;
            Gen.Emit_Last_Parameter(O);
         when Lexer.Cos =>
            if Parser.Count_Parameters(This.Parameters) = 1 then
               Emit_Parameter_Number(This.Parameters.Parameter,Gen);
            elsif Parser.Count_Parameters(This.Parameters) = 2 then
               Emit_Parameter_Number(This.Parameters.Parameter,Gen);
               Gen.Emit_Next_Parameter(O);
               Emit_Parameter_Number(This.Parameters.Next.Parameter,Gen);
            end if;
            Gen.Emit_Last_Parameter(O);
         when Lexer.Tan =>
            if Parser.Count_Parameters(This.Parameters) = 1 then
               Emit_Parameter_Number(This.Parameters.Parameter,Gen);
            elsif Parser.Count_Parameters(This.Parameters) = 2 then
               Emit_Parameter_Number(This.Parameters.Parameter,Gen);
               Gen.Emit_Next_Parameter(O);
               Emit_Parameter_Number(This.Parameters.Next.Parameter,Gen);
            end if;
            Gen.Emit_Last_Parameter(O);
         when Lexer.Cot =>
            if Parser.Count_Parameters(This.Parameters) = 1 then
               Emit_Parameter_Number(This.Parameters.Parameter,Gen);
            elsif Parser.Count_Parameters(This.Parameters) = 2 then
               Emit_Parameter_Number(This.Parameters.Parameter,Gen);
               Gen.Emit_Next_Parameter(O);
               Emit_Parameter_Number(This.Parameters.Next.Parameter,Gen);
            end if;
            Gen.Emit_Last_Parameter(O);
         when Lexer.Arcsin =>
            if Parser.Count_Parameters(This.Parameters) = 1 then
               Emit_Parameter_Number(This.Parameters.Parameter,Gen);
            elsif Parser.Count_Parameters(This.Parameters) = 2 then
               Emit_Parameter_Number(This.Parameters.Parameter,Gen);
               Gen.Emit_Next_Parameter(O);
               Emit_Parameter_Number(This.Parameters.Next.Parameter,Gen);
            end if;
            Gen.Emit_Last_Parameter(O);
         when Lexer.Arccos =>
            if Parser.Count_Parameters(This.Parameters) = 1 then
               Emit_Parameter_Number(This.Parameters.Parameter,Gen);
            elsif Parser.Count_Parameters(This.Parameters) = 2 then
               Emit_Parameter_Number(This.Parameters.Parameter,Gen);
               Gen.Emit_Next_Parameter(O);
               Emit_Parameter_Number(This.Parameters.Next.Parameter,Gen);
            end if;
            Gen.Emit_Last_Parameter(O);
         when Lexer.Arctan =>
            if Parser.Count_Parameters(This.Parameters) = 1 then
               Emit_Parameter_Number(This.Parameters.Parameter,Gen);
            elsif Parser.Count_Parameters(This.Parameters) = 2 then
               Emit_Parameter_Number(This.Parameters.Parameter,Gen);
               Gen.Emit_Next_Parameter(O);
               Emit_Parameter_Number(This.Parameters.Next.Parameter,Gen);
            elsif Parser.Count_Parameters(This.Parameters) = 3 then
               Emit_Parameter_Number(This.Parameters.Parameter,Gen);
               Gen.Emit_Next_Parameter(O);
               Emit_Parameter_Number(This.Parameters.Next.Parameter,Gen);
               Gen.Emit_Next_Parameter(O);
               Emit_Parameter_Number(This.Parameters.Next.Next.Parameter,Gen);
            end if;
            Gen.Emit_Last_Parameter(O);
         when Lexer.Arccot =>
            if Parser.Count_Parameters(This.Parameters) = 1 then
               Emit_Parameter_Number(This.Parameters.Parameter,Gen);
            elsif Parser.Count_Parameters(This.Parameters) = 2 then
               Emit_Parameter_Number(This.Parameters.Parameter,Gen);
               Gen.Emit_Next_Parameter(O);
               Emit_Parameter_Number(This.Parameters.Next.Parameter,Gen);
            elsif Parser.Count_Parameters(This.Parameters) = 3 then
               Emit_Parameter_Number(This.Parameters.Parameter,Gen);
               Gen.Emit_Next_Parameter(O);
               Emit_Parameter_Number(This.Parameters.Next.Parameter,Gen);
               Gen.Emit_Next_Parameter(O);
               Emit_Parameter_Number(This.Parameters.Next.Next.Parameter,Gen);
            end if;
            Gen.Emit_Last_Parameter(O);
      end case;
   end Emit_Code;
   procedure Emit_Code (This : in Plugin_Func_Expon; Gen : access Generate_Interface.Typ'Class) is
      T : Token_Pointer;
      M_Str : access MSSyst.String.Typ;
   begin
      T := This.Id;
      M_Str := +Get_Text(T.Start..T.Finish);
      Gen.Emit_Plugin_Call(M_Str,This.Parameters);
   end Emit_Code;
   procedure Emit_Code (This : in Mult; Gen : access Generate_Interface.Typ'Class) is
   begin
      Emit_Code(This.Left.all,Gen);
   end Emit_Code;
   procedure Emit_Code (This : in Expon_Mult; Gen : access Generate_Interface.Typ'Class) is
   begin
      if Gen.Is_Postfix then
         Emit_Code(This.Left.all,Gen);
         Emit_Code(This.Right.All,Gen);
         Gen.Emit_Exponentiation;
      else
         Emit_Code(This.Left.all,Gen);
         Gen.Emit_Exponentiation;
         Emit_Code(This.Right.All,Gen);
      end if;
   end Emit_Code;
   procedure Emit_Code (This : in Add; Gen : access Generate_Interface.Typ'Class) is
   begin
      Emit_Code(This.Left.all,Gen);
   end Emit_Code;
   procedure Emit_Code (This : in Div_Add; Gen : access Generate_Interface.Typ'Class) is
   begin
      if Gen.Is_Postfix then
         Emit_Code(This.Left.all,Gen);
         Emit_Code(This.Right.All,Gen);
         Gen.Emit_Divide;
      else
         Emit_Code(This.Left.all,Gen);
         Gen.Emit_Divide;
         Emit_Code(This.Right.All,Gen);
      end if;
   end Emit_Code;
   procedure Emit_Code (This : in Mult_Add; Gen : access Generate_Interface.Typ'Class) is
   begin
      if Gen.Is_Postfix then
         Emit_Code(This.Left.all,Gen);
         Emit_Code(This.Right.All,Gen);
         Gen.Emit_Times;
      else
         Emit_Code(This.Left.all,Gen);
         Gen.Emit_Times;
         Emit_Code(This.Right.All,Gen);
      end if;
   end Emit_Code;
   procedure Emit_Code (This : in Mod_Add; Gen : access Generate_Interface.Typ'Class) is
   begin
      if Gen.Is_Postfix then
         Emit_Code(This.Left.all,Gen);
         Emit_Code(This.Right.All,Gen);
         Gen.Emit_Mod;
      else
         Emit_Code(This.Left.all,Gen);
         Gen.Emit_Mod;
         Emit_Code(This.Right.All,Gen);
      end if;
   end Emit_Code;
   procedure Emit_Code (This : in Rem_Add; Gen : access Generate_Interface.Typ'Class) is
   begin
      if Gen.Is_Postfix then
         Emit_Code(This.Left.all,Gen);
         Emit_Code(This.Right.All,Gen);
         Gen.Emit_Rem;
      else
         Emit_Code(This.Left.all,Gen);
         Gen.Emit_Rem;
         Emit_Code(This.Right.All,Gen);
      end if;
   end Emit_Code;
   procedure Emit_Code (This : in Expression; Gen : access Generate_Interface.Typ'Class) is
   begin
      Emit_Code(This.Left.all,Gen);
   end Emit_Code;
   procedure Emit_Code (This : in Add_Expression; Gen : access Generate_Interface.Typ'Class) is
   begin
      if Gen.Is_Postfix then
         Emit_Code(This.Left.all,Gen);
         Emit_Code(This.Right.All,Gen);
         Gen.Emit_Plus;
      else
         Emit_Code(This.Left.all,Gen);
         Gen.Emit_Plus;
         Emit_Code(This.Right.All,Gen);
      end if;
   end Emit_Code;
   procedure Emit_Code (This : in Minus_Expression; Gen : access Generate_Interface.Typ'Class) is
   begin
      if Gen.Is_Postfix then
         Emit_Code(This.Left.all,Gen);
         Emit_Code(This.Right.All,Gen);
         Gen.Emit_Minus;
      else
         Emit_Code(This.Left.all,Gen);
         Gen.Emit_Minus;
         Emit_Code(This.Right.All,Gen);
      end if;
   end Emit_Code;
   procedure Emit_Code (This : in Relation; Gen : access Generate_Interface.Typ'Class) is
      This_Kind : Lexer.Relation := This.Kind;
   begin
      Emit_Code(This.Left.All,Gen);
      if Gen.Is_Postfix then
         Emit_Code(This.Right.All,Gen);
      end if;
      case This_Kind is
         when Greater =>
            Gen.Emit_Relation(1);
         when Greater_Equal =>
            Gen.Emit_Relation(2);
         when Equal =>
            Gen.Emit_Relation(5);
         when Less =>
            Gen.Emit_Relation(3);
         when Less_Equal =>
            Gen.Emit_Relation(4);
         when Not_Equal =>
            Gen.Emit_Relation(6);
      end case;
      if not Gen.Is_Postfix then
         Emit_Code(This.Right.All,Gen);
      end if;
   end Emit_Code;
   procedure Emit_Code (This : in Boolean0; Gen : access Generate_Interface.Typ'Class) is
      Int_Kind : Integer;
      O : access MSSyst.Object.Typ'Class;
   begin
      Int_Kind := Lexer.Subprogram'Pos(This.Kind);
      O := Gen.Emit_Call_Method(Int_Kind);
      Gen.Emit_No_Parameters(O);
   end Emit_Code;
   procedure Emit_Code (This : in Boolean_Constant; Gen : access Generate_Interface.Typ'Class) is
   begin
      Gen.Emit_Load_Boolean(This.value);
   end Emit_Code;
   procedure Emit_Code (This : in Boolean1; Gen : access Generate_Interface.Typ'Class) is
      Int_Kind : Integer;
      O : access MSSyst.Object.Typ'Class;
   begin
      Int_Kind := Lexer.Subprogram'Pos(This.Kind);
      O := Gen.Emit_Call_Method(Int_Kind);
      case This.Kind is
         when Key_Down =>
            Gen.Emit_Conversion(Conversions'Pos(To_String));
            Emit_Code(This.Parameter.all,Gen);
            Gen.Emit_End_Conversion(Conversions'Pos(To_String));
         when Mouse_Button_Down =>
            Gen.Emit_Conversion(Conversions'Pos(To_Integer));
            Emit_Code(This.Parameter.all,Gen);
            Gen.Emit_End_Conversion(Conversions'Pos(To_Integer));
         when Mouse_Button_Pressed =>
            Gen.Emit_Conversion(Conversions'Pos(To_Integer));
            Emit_Code(This.Parameter.all,Gen);
            Gen.Emit_End_Conversion(Conversions'Pos(To_Integer));
         when Mouse_Button_Released =>
            Gen.Emit_Conversion(Conversions'Pos(To_Integer));
            Emit_Code(This.Parameter.all,Gen);
            Gen.Emit_End_Conversion(Conversions'Pos(To_Integer));
      end case;
      Gen.Emit_Last_Parameter(O);
   end Emit_Code;
   procedure Emit_Code (This : in Boolean_Reflection; Gen : access Generate_Interface.Typ'Class) is
      T : Lexer.Token_Pointer;
   begin
      T := This.Id;
      case This.Kind is
         when Is_Character =>
            Gen.Emit_Is_Character(+Get_Text(T.Start..T.Finish));
         when Is_Number =>
            Gen.Emit_Is_Number(+Get_Text(T.Start..T.Finish));
         when Is_String =>
            Gen.Emit_Is_String(+Get_Text(T.Start..T.Finish));
         when Is_Array =>
            Gen.Emit_Is_Array(+Get_Text(T.Start..T.Finish));
         when Is_2d_Array =>
            Gen.Emit_Is_Array2D(+Get_Text(T.Start..T.Finish));
      end case;
   end Emit_Code;
   procedure Emit_Code (This : in Boolean_Plugin; Gen : access Generate_Interface.Typ'Class) is
      T : Token_Pointer;
      M_Str : access MSSyst.String.Typ;
   begin
      T := This.Id;
      M_Str := +Get_Text(T.Start..T.Finish);
      Gen.Emit_Plugin_Call(M_Str,This.Parameters);
      --Raptor.Plugins.Emit_Invoke_Function(M_Str,This.Parameters,Gen);
   end Emit_Code;
   procedure Emit_Code (This : in Boolean2; Gen : access Generate_Interface.Typ'Class) is
   begin
      if Gen.Is_Postfix then
         Emit_Code(This.Left.All,Gen);
         if (This.Negated) then
            Gen.Emit_Not;
         end if;
      else
         if (This.Negated) then
            Gen.Emit_Not;
         end if;
         Emit_Code(This.Left.All,Gen);
      end if;
   end Emit_Code;
   procedure Emit_Code (This : in And_Boolean2; Gen : access Generate_Interface.Typ'Class) is
   begin
      Gen.Emit_And_Shortcut(
         Left         => This.Left,
         Right        => This.Right,
         Left_Negated => This.Negated);
   end Emit_Code;
   procedure Emit_Code (This : in Boolean_Expression; Gen : access Generate_Interface.Typ'Class) is
   begin
      Emit_Code(This.Left.All,Gen);
   end Emit_Code;
   procedure Emit_Code (This : in Xor_Boolean; Gen : access Generate_Interface.Typ'Class) is
   begin
      Emit_Code(This.Left.All,Gen);
      Emit_Code(This.Right.All,Gen);
      Gen.Emit_Xor;
   end Emit_Code;
   procedure Emit_Code (This : in Or_Boolean; Gen : access Generate_Interface.Typ'Class) is
   begin
      Gen.Emit_Or_Shortcut(
         Left  => This.Left,
         Right => This.Right);
   end Emit_Code;
   procedure Emit_Load_Prompt(Gen : access Generate_Interface.Typ'Class) is
   begin
      if Current_Prompt /= null then
         Gen.Emit_Load_String_Const(Current_Prompt);
      else
         -- maintain with parallelogram.cs
         Gen.Emit_Load(+"raptor_prompt_variable_zzyz");
      end if;
   end Emit_Load_Prompt;
   procedure Emit_Code (This : in Input; Gen : access Generate_Interface.Typ'Class) is
      T : Lexer.Token_Pointer;
   begin
      Emit_Context := Input_Context;
      Emit_Code(This.Lhs.All,This.LSuffix,Gen);
      Emit_Code(This.LSuffix.All,Gen);
      --Gen.Input_Start_Variable(M_Str);
      Emit_Load_Prompt(Gen);
      Gen.Input_Past_Prompt;
   end Emit_Code;
--   procedure Emit_Code (This : in Input_Array; Gen : access Generate_Interface.Typ'Class) is
--      T : Lexer.Token_Pointer;
--      M_Str : access MSSyst.String.Typ;
--      Result : access MSSyst.String.Typ;
--      Rhs : Value;
--   begin
--      T := This.Id;
--      M_Str := +Get_Text(T.Start..T.Finish);
--      Gen.Input_Start_Array_1D(M_Str,This.Reference);
--      Emit_Load_Prompt(Gen);
--      Gen.Input_Past_Prompt;
--   end Emit_Code;
--   procedure Emit_Code (This : in Input_2D_Array; Gen : access Generate_Interface.Typ'Class) is
--      T : Lexer.Token_Pointer;
--      M_Str : access MSSyst.String.Typ;
--   begin
--      T := This.Id;
--      M_Str := +Get_Text(T.Start..T.Finish);
--      Gen.Input_Start_Array_2D(M_Str,This.Reference,This.Reference2);
--      Emit_Load_Prompt(Gen);
--      Gen.Input_Past_Prompt;
--   end Emit_Code;
   procedure Emit_Code (This : in Expr_Output; Gen : access Generate_Interface.Typ'Class) is
   begin
      Gen.Output_Start(this.New_Line,false);
      Emit_Code(This.Expr.All,Gen);
      Gen.Output_Past_Expr(this.New_Line,false);
   end Emit_Code;
--   procedure Emit_Code (This : in String_Output; Gen : access Generate_Interface.Typ'Class) is
--      T : Lexer.Token_Pointer;
--   begin
--      T := This.Str;
--      Gen.Output_Start(this.New_Line,true);
--      Gen.Emit_Load_String_Const(
--         +Dequote(Get_Text(T.Start+1..T.Finish-1)));
--      Gen.Output_Past_Expr(this.New_Line,true);
--   end Emit_Code;
   -------------------------------------------------------
   -- Compile_Pass1
   -------------------------------------------------------
   procedure Compile_Pass1 (This : in Proc_Call; Gen : access Generate_Interface.Typ'Class) is
      Kind : Lexer.Proc_Token_Types := This.Id.Kind;
   begin
      case Kind is
        when Lexer.Delay_For =>
            Compile_Pass1(This.Param_List.Parameter.all,Gen);
        when Lexer.Set_Precision =>
            Compile_Pass1(This.Param_List.Parameter.All,Gen);
        when Lexer.Redirect_Input =>
            Compile_Pass1(This.Param_List.Parameter.all,Gen);
        when Lexer.Redirect_Output|Lexer.Redirect_Output_Append =>
            Compile_Pass1(This.Param_List.Parameter.all,Gen);
        when Lexer.Clear_Console =>
            null;
        when Lexer.Clear_Window =>
            Compile_Pass1(This.Param_List.Parameter.all,Gen);
         when Lexer.Close_Graph_Window|Lexer.Freeze_Graph_Window|Lexer.Unfreeze_Graph_Window|Lexer.Update_Graph_Window =>
            null;
         when Lexer.Set_Font_Size =>
            Compile_Pass1(This.Param_List.Parameter.all,Gen);
         when Lexer.Display_Number =>
            Compile_Pass1(This.Param_List.Parameter.all,Gen);
            Compile_Pass1(This.Param_List.Next.Parameter.all,Gen);
            Compile_Pass1(This.Param_List.Next.Next.Parameter.all,Gen);
            Compile_Pass1(This.Param_List.Next.Next.Next.Parameter.all,Gen);
         when Lexer.Draw_Bitmap =>
            Compile_Pass1(This.Param_List.Parameter.all,Gen);
            Compile_Pass1(This.Param_List.Next.Parameter.all,Gen);
            Compile_Pass1(This.Param_List.Next.Next.Parameter.all,Gen);
            Compile_Pass1(This.Param_List.Next.Next.Next.Parameter.all,Gen);
            Compile_Pass1(This.Param_List.Next.Next.Next.Next.Parameter.all,Gen);
         when Lexer.Display_Text =>
            Compile_Pass1(This.Param_List.Parameter.all,Gen);
            Compile_Pass1(This.Param_List.Next.Parameter.all,Gen);
            Compile_Pass1(This.Param_List.Next.Next.Parameter.all,Gen);
            Compile_Pass1(This.Param_List.Next.Next.Next.Parameter.all,Gen);
         when Lexer.Draw_Arc =>
            Compile_Pass1(This.Param_List.Parameter.all,Gen);
            Compile_Pass1(This.Param_List.Next.Parameter.all,Gen);
            Compile_Pass1(This.Param_List.Next.Next.Parameter.all,Gen);
            Compile_Pass1(This.Param_List.Next.Next.Next.Parameter.all,Gen);
            Compile_Pass1(This.Param_List.Next.Next.Next.Next.Parameter.all,Gen);
            Compile_Pass1(This.Param_List.Next.Next.Next.Next.Next.Parameter.all,Gen);
            Compile_Pass1(This.Param_List.Next.Next.Next.Next.Next.Next.Parameter.all,Gen);
            Compile_Pass1(This.Param_List.Next.Next.Next.Next.Next.Next.Next.Parameter.all,Gen);
            Compile_Pass1(This.Param_List.Next.Next.Next.Next.Next.Next.Next.Next.Parameter.all,Gen);
         when Lexer.Draw_Box =>
            Compile_Pass1(This.Param_List.Parameter.all,Gen);
            Compile_Pass1(This.Param_List.Next.Parameter.all,Gen);
            Compile_Pass1(This.Param_List.Next.Next.Parameter.all,Gen);
            Compile_Pass1(This.Param_List.Next.Next.Next.Parameter.all,Gen);
            Compile_Pass1(This.Param_List.Next.Next.Next.Next.Parameter.all,Gen);
            Compile_Pass1(This.Param_List.Next.Next.Next.Next.Next.Parameter.all,Gen);
         when Lexer.Draw_Circle =>
            Compile_Pass1(This.Param_List.Parameter.all,Gen);
            Compile_Pass1(This.Param_List.Next.Parameter.all,Gen);
            Compile_Pass1(This.Param_List.Next.Next.Parameter.all,Gen);
            Compile_Pass1(This.Param_List.Next.Next.Next.Parameter.all,Gen);
            Compile_Pass1(This.Param_List.Next.Next.Next.Next.Parameter.all,Gen);
         when Lexer.Draw_Ellipse =>
            Compile_Pass1(This.Param_List.Parameter.all,Gen);
            Compile_Pass1(This.Param_List.Next.Parameter.all,Gen);
            Compile_Pass1(This.Param_List.Next.Next.Parameter.all,Gen);
            Compile_Pass1(This.Param_List.Next.Next.Next.Parameter.all,Gen);
            Compile_Pass1(This.Param_List.Next.Next.Next.Next.Parameter.all,Gen);
            Compile_Pass1(This.Param_List.Next.Next.Next.Next.Next.Parameter.all,Gen);
         when Lexer.Draw_Ellipse_Rotate =>
            Compile_Pass1(This.Param_List.Parameter.all,Gen);
            Compile_Pass1(This.Param_List.Next.Parameter.all,Gen);
            Compile_Pass1(This.Param_List.Next.Next.Parameter.all,Gen);
            Compile_Pass1(This.Param_List.Next.Next.Next.Parameter.all,Gen);
            Compile_Pass1(This.Param_List.Next.Next.Next.Next.Parameter.all,Gen);
            Compile_Pass1(This.Param_List.Next.Next.Next.Next.Next.Parameter.all,Gen);
            Compile_Pass1(This.Param_List.Next.Next.Next.Next.Next.Next.Parameter.all,Gen);
         when Lexer.Draw_Line =>
            Compile_Pass1(This.Param_List.Parameter.all,Gen);
            Compile_Pass1(This.Param_List.Next.Parameter.all,Gen);
            Compile_Pass1(This.Param_List.Next.Next.Parameter.all,Gen);
            Compile_Pass1(This.Param_List.Next.Next.Next.Parameter.all,Gen);
            Compile_Pass1(This.Param_List.Next.Next.Next.Next.Parameter.all,Gen);
         when Lexer.Flood_Fill =>
            Compile_Pass1(This.Param_List.Parameter.all,Gen);
            Compile_Pass1(This.Param_List.Next.Parameter.all,Gen);
            Compile_Pass1(This.Param_List.Next.Next.Parameter.all,Gen);
         when Lexer.Open_Graph_Window =>
            Compile_Pass1(This.Param_List.Parameter.all,Gen);
            Compile_Pass1(This.Param_List.Next.Parameter.all,Gen);
         when Lexer.Wait_For_Key =>
            null;
         when Lexer.Wait_For_Mouse_Button =>
            Compile_Pass1(This.Param_List.Parameter.all,Gen);
         when Lexer.Put_Pixel =>
            Compile_Pass1(This.Param_List.Parameter.all,Gen);
            Compile_Pass1(This.Param_List.Next.Parameter.all,Gen);
            Compile_Pass1(This.Param_List.Next.Next.Parameter.all,Gen);
         when Lexer.Set_Window_Title|Lexer.Save_Graph_Window =>
            Compile_Pass1(This.Param_List.Parameter.all,Gen);
         when Lexer.Get_Mouse_Button =>
            Compile_Pass1(This.Param_List.Parameter.all,Gen);
            Compile_Pass1(This.Param_List.Next.Parameter.all,Gen);
            Compile_Pass1(This.Param_List.Next.Next.Parameter.all,Gen);
      end case;
   end Compile_Pass1;
   procedure Compile_Pass1 (This : in Plugin_Proc_Call; Gen : access Generate_Interface.Typ'Class) is
   begin
      null;
   end Compile_Pass1;
   procedure Compile_Pass1 (This : in Tabid_Proc_Call; Gen : access Generate_Interface.Typ'Class) is
   begin
      null;
   end Compile_Pass1;
--   procedure Compile_Pass1 (This : in Expr_Assignment; Gen : access Generate_Interface.Typ'Class) is
--      T : Lexer.Token_Pointer;
--   begin
--      Compile_Pass1(This.Expr_Part.All,Gen);
--      T := This.Id;
--      if not Raptor.Compile_Helpers.Start_New_Declaration(
--            +To_Upper(Get_Text(T.Start..T.Finish))) then
--         Gen.Declare_As_Variable(
--            Get_Text(T.Start..T.Finish));
--      end if;
--   end Compile_Pass1;
--   procedure Compile_Pass1 (This : in Array_Assignment; Gen : access Generate_Interface.Typ'Class) is
--      T : Lexer.Token_Pointer;
--   begin
--      T := This.Id;
--      Compile_Pass1(This.Reference.all,Gen);
--      Compile_Pass1(This.Expr_Part.All,Gen);
--      if not Raptor.Compile_Helpers.Start_New_Declaration(
--            +To_Upper(Get_Text(T.Start..T.Finish))) then
--         Gen.Declare_As_1D_Array(Get_Text(T.Start..T.Finish));
--      end if;
--   end Compile_Pass1;
--   procedure Compile_Pass1 (This : in Array_2D_Assignment; Gen : access Generate_Interface.Typ'Class) is
--      T : Lexer.Token_Pointer;
--   begin
--      T := This.Id;
--      Compile_Pass1(This.Reference.all,Gen);
--      Compile_Pass1(This.Reference2.all,Gen);
--      Compile_Pass1(This.Expr_Part.All,Gen);
--      if not Raptor.Compile_Helpers.Start_New_Declaration(
--            +To_Upper(Get_Text(T.Start..T.Finish))) then
--         Gen.Declare_As_2D_Array(Get_Text(T.Start..T.Finish));
--      end if;
--   end Compile_Pass1;
   procedure Compile_Pass1 (This : in Number_Expon; Gen : access Generate_Interface.Typ'Class) is
   begin
      null;
   end Compile_Pass1;
   procedure Compile_Pass1 (This : in Negative_Expon; Gen : access Generate_Interface.Typ'Class) is
   begin
      Compile_Pass1(This.E.all,Gen);
   end Compile_Pass1;

   procedure Compile_Pass1 (This : in String_Expon; Gen : access Generate_Interface.Typ'Class) is
      T : Lexer.Token_Pointer;
   begin
      -- this is a string like "bob", so ignore
      null;
   end Compile_Pass1;
   procedure Compile_Pass1 (This : in Character_Expon; Gen : access Generate_Interface.Typ'Class) is
      T : Lexer.Token_Pointer;
   begin
      -- this is a character like 'x', so ignore
      null;
   end Compile_Pass1;
   procedure Compile_Pass1 (This : in Id_Lhs; Gen : access Generate_Interface.Typ'Class) is
      T : Lexer.Token_Pointer;
   begin
      T := This.Id;
      if not Raptor.Compile_Helpers.Start_New_Declaration(
            +To_Upper(Get_Text(T.Start..T.Finish))) then
         Gen.Declare_As_Variable(Get_Text(T.Start..T.Finish));
      end if;
   end Compile_Pass1;
   procedure Compile_Pass1 (This : in Array_Ref_Lhs; Gen : access Generate_Interface.Typ'Class) is
      T : Lexer.Token_Pointer;
   begin
      T := This.Id;
      Compile_Pass1(This.Reference.all,Gen);
      if not Raptor.Compile_Helpers.Start_New_Declaration(
            +To_Upper(Get_Text(T.Start..T.Finish))) then
         Gen.Declare_As_1D_Array(Get_Text(T.Start..T.Finish));
      end if;
   end Compile_Pass1;
   procedure Compile_Pass1 (This : in Array_2D_Ref_Lhs; Gen : access Generate_Interface.Typ'Class) is
      T : Lexer.Token_Pointer;
   begin
      T := This.Id;
      Compile_Pass1(This.Reference.all,Gen);
      Compile_Pass1(This.Reference2.all,Gen);
      if not Raptor.Compile_Helpers.Start_New_Declaration(
            +To_Upper(Get_Text(T.Start..T.Finish))) then
         Gen.Declare_As_2D_Array(Get_Text(T.Start..T.Finish));
      end if;
   end Compile_Pass1;
   procedure Compile_Pass1 (This : in Id_Rhs; Gen : access Generate_Interface.Typ'Class) is
      T : Lexer.Token_Pointer;
   begin
      T := This.Id;
      if not Raptor.Compile_Helpers.Start_New_Declaration(
            +To_Upper(Get_Text(T.Start..T.Finish))) then
         Gen.Declare_As_Variable(Get_Text(T.Start..T.Finish));
      end if;
   end Compile_Pass1;
   procedure Compile_Pass1 (This : in Array_Ref_Rhs; Gen : access Generate_Interface.Typ'Class) is
      T : Lexer.Token_Pointer;
   begin
      T := This.Id;
      Compile_Pass1(This.Reference.all,Gen);
      if not Raptor.Compile_Helpers.Start_New_Declaration(
            +To_Upper(Get_Text(T.Start..T.Finish))) then
         Gen.Declare_As_1D_Array(Get_Text(T.Start..T.Finish));
      end if;
   end Compile_Pass1;
   procedure Compile_Pass1 (This : in Array_2D_Ref_Rhs; Gen : access Generate_Interface.Typ'Class) is
      T : Lexer.Token_Pointer;
   begin
      T := This.Id;
      Compile_Pass1(This.Reference.all,Gen);
      Compile_Pass1(This.Reference2.all,Gen);
      if not Raptor.Compile_Helpers.Start_New_Declaration(
            +To_Upper(Get_Text(T.Start..T.Finish))) then
         Gen.Declare_As_2D_Array(Get_Text(T.Start..T.Finish));
      end if;
   end Compile_Pass1;
   procedure Compile_Pass1 (This : in Paren_Expon; Gen : access Generate_Interface.Typ'Class) is
   begin
      Compile_Pass1(This.Expr_Part.all,Gen);
   end Compile_Pass1;
   procedure Compile_Pass1 (This : in Func0_Expon; Gen : access Generate_Interface.Typ'Class) is
   begin
      null;
   end Compile_Pass1;
   procedure Compile_Pass1 (This : in Func_Expon; Gen : access Generate_Interface.Typ'Class) is
      Function_Name : Lexer.Func_Token_Types;
   begin
      Function_Name := This.Id.Kind;

      case Function_Name is
         when Lexer.Func_Id0 =>
            -- should be handled above in Func0_Expon
            raise Constraint_Error;
         when Lexer.Length_Of =>
            --Compile_Pass1(This.Parameters.Parameter.all,Gen);
            null;
            -- don't do this here, as it causes an array to look like a single variable
         when Lexer.To_Ascii =>
            Compile_Pass1(This.Parameters.Parameter.all,Gen);
         when Lexer.To_Character =>
            Compile_Pass1(This.Parameters.Parameter.all,Gen);
         when Lexer.Load_Bitmap =>
            Compile_Pass1(This.Parameters.Parameter.all,Gen);
         when Lexer.Get_Pixel =>
            Compile_Pass1(This.Parameters.Parameter.all,Gen);
            Compile_Pass1(This.Parameters.Next.Parameter.all,Gen);
         when Lexer.Closest_Color =>
            Compile_Pass1(This.Parameters.Parameter.all,Gen);
            Compile_Pass1(This.Parameters.Next.Parameter.all,Gen);
            Compile_Pass1(This.Parameters.Next.Next.Parameter.all,Gen);
         when Lexer.Sinh =>
            Compile_Pass1(This.Parameters.Parameter.all,Gen);
         when Lexer.Tanh =>
            Compile_Pass1(This.Parameters.Parameter.all,Gen);
         when Lexer.Cosh =>
            Compile_Pass1(This.Parameters.Parameter.all,Gen);
         when Lexer.Arccosh =>
            Compile_Pass1(This.Parameters.Parameter.all,Gen);
         when Lexer.Arcsinh =>
            Compile_Pass1(This.Parameters.Parameter.all,Gen);
         when Lexer.Arctanh =>
            Compile_Pass1(This.Parameters.Parameter.all,Gen);
         when Lexer.Coth =>
            Compile_Pass1(This.Parameters.Parameter.all,Gen);
         when Lexer.Arccoth =>
            Compile_Pass1(This.Parameters.Parameter.all,Gen);
         when Lexer.Sqrt =>
            Compile_Pass1(This.Parameters.Parameter.all,Gen);
         when Lexer.Floor =>
            Compile_Pass1(This.Parameters.Parameter.all,Gen);
         when Lexer.Ceiling =>
            Compile_Pass1(This.Parameters.Parameter.all,Gen);
         when Lexer.Abs_F =>
            Compile_Pass1(This.Parameters.Parameter.all,Gen);
         when Lexer.Min|Lexer.Max =>
            Compile_Pass1(This.Parameters.Parameter.all,Gen);
            Compile_Pass1(This.Parameters.Next.Parameter.all,Gen);
         when Lexer.Log =>
            if Parser.Count_Parameters(This.Parameters) = 1 then
               Compile_Pass1(This.Parameters.Parameter.all,Gen);
            elsif Parser.Count_Parameters(This.Parameters) = 2 then
               Compile_Pass1(This.Parameters.Parameter.all,Gen);
               Compile_Pass1(This.Parameters.Next.Parameter.all,Gen);
            end if;
         when Lexer.Sin =>
            if Parser.Count_Parameters(This.Parameters) = 1 then
               Compile_Pass1(This.Parameters.Parameter.all,Gen);
            elsif Parser.Count_Parameters(This.Parameters) = 2 then
               Compile_Pass1(This.Parameters.Parameter.all,Gen);
               Compile_Pass1(This.Parameters.Next.Parameter.all,Gen);
            end if;
         when Lexer.Cos =>
            if Parser.Count_Parameters(This.Parameters) = 1 then
               Compile_Pass1(This.Parameters.Parameter.all,Gen);
            elsif Parser.Count_Parameters(This.Parameters) = 2 then
               Compile_Pass1(This.Parameters.Parameter.all,Gen);
               Compile_Pass1(This.Parameters.Next.Parameter.all,Gen);
            end if;
         when Lexer.Tan =>
            if Parser.Count_Parameters(This.Parameters) = 1 then
               Compile_Pass1(This.Parameters.Parameter.all,Gen);
            elsif Parser.Count_Parameters(This.Parameters) = 2 then
               Compile_Pass1(This.Parameters.Parameter.all,Gen);
               Compile_Pass1(This.Parameters.Next.Parameter.all,Gen);
            end if;
         when Lexer.Cot =>
            if Parser.Count_Parameters(This.Parameters) = 1 then
               Compile_Pass1(This.Parameters.Parameter.all,Gen);
            elsif Parser.Count_Parameters(This.Parameters) = 2 then
               Compile_Pass1(This.Parameters.Parameter.all,Gen);
               Compile_Pass1(This.Parameters.Next.Parameter.all,Gen);
            end if;
         when Lexer.Arcsin =>
            if Parser.Count_Parameters(This.Parameters) = 1 then
               Compile_Pass1(This.Parameters.Parameter.all,Gen);
            elsif Parser.Count_Parameters(This.Parameters) = 2 then
               Compile_Pass1(This.Parameters.Parameter.all,Gen);
               Compile_Pass1(This.Parameters.Next.Parameter.all,Gen);
            end if;
         when Lexer.Arccos =>
            if Parser.Count_Parameters(This.Parameters) = 1 then
               Compile_Pass1(This.Parameters.Parameter.all,Gen);
            elsif Parser.Count_Parameters(This.Parameters) = 2 then
               Compile_Pass1(This.Parameters.Parameter.all,Gen);
               Compile_Pass1(This.Parameters.Next.Parameter.all,Gen);
            end if;
         when Lexer.Arctan =>
            if Parser.Count_Parameters(This.Parameters) = 1 then
               Compile_Pass1(This.Parameters.Parameter.all,Gen);
            elsif Parser.Count_Parameters(This.Parameters) = 2 then
               Compile_Pass1(This.Parameters.Parameter.all,Gen);
               Compile_Pass1(This.Parameters.Next.Parameter.all,Gen);
            elsif Parser.Count_Parameters(This.Parameters) = 3 then
               Compile_Pass1(This.Parameters.Parameter.all,Gen);
               Compile_Pass1(This.Parameters.Next.Parameter.all,Gen);
               Compile_Pass1(This.Parameters.Next.Next.Parameter.all,Gen);
            end if;
         when Lexer.Arccot =>
            if Parser.Count_Parameters(This.Parameters) = 1 then
               Compile_Pass1(This.Parameters.Parameter.all,Gen);
            elsif Parser.Count_Parameters(This.Parameters) = 2 then
               Compile_Pass1(This.Parameters.Parameter.all,Gen);
               Compile_Pass1(This.Parameters.Next.Parameter.all,Gen);
            elsif Parser.Count_Parameters(This.Parameters) = 3 then
               Compile_Pass1(This.Parameters.Parameter.all,Gen);
               Compile_Pass1(This.Parameters.Next.Parameter.all,Gen);
               Compile_Pass1(This.Parameters.Next.Next.Parameter.all,Gen);
            end if;
      end case;
   end Compile_Pass1;
   procedure Compile_Pass1 (This : in Plugin_Func_Expon; Gen : access Generate_Interface.Typ'Class) is
   begin
      null;
   end Compile_Pass1;
   procedure Compile_Pass1 (This : in Mult; Gen : access Generate_Interface.Typ'Class) is
   begin
      Compile_Pass1(This.Left.all,Gen);
   end Compile_Pass1;
   procedure Compile_Pass1 (This : in Expon_Mult; Gen : access Generate_Interface.Typ'Class) is
   begin
      Compile_Pass1(This.Left.all,Gen);
      Compile_Pass1(This.Right.All,Gen);
   end Compile_Pass1;
   procedure Compile_Pass1 (This : in Add; Gen : access Generate_Interface.Typ'Class) is
   begin
      Compile_Pass1(This.Left.all,Gen);
   end Compile_Pass1;
   procedure Compile_Pass1 (This : in Div_Add; Gen : access Generate_Interface.Typ'Class) is
   begin
      Compile_Pass1(This.Left.all,Gen);
      Compile_Pass1(This.Right.All,Gen);
   end Compile_Pass1;
   procedure Compile_Pass1 (This : in Mult_Add; Gen : access Generate_Interface.Typ'Class) is
   begin
      Compile_Pass1(This.Left.all,Gen);
      Compile_Pass1(This.Right.All,Gen);
   end Compile_Pass1;
   procedure Compile_Pass1 (This : in Mod_Add; Gen : access Generate_Interface.Typ'Class) is
   begin
      Compile_Pass1(This.Left.all,Gen);
      Compile_Pass1(This.Right.All,Gen);
   end Compile_Pass1;
   procedure Compile_Pass1 (This : in Rem_Add; Gen : access Generate_Interface.Typ'Class) is
   begin
      Compile_Pass1(This.Left.all,Gen);
      Compile_Pass1(This.Right.All,Gen);
   end Compile_Pass1;
   procedure Compile_Pass1 (This : in Expression; Gen : access Generate_Interface.Typ'Class) is
   begin
      Compile_Pass1(This.Left.all,Gen);
   end Compile_Pass1;
   procedure Compile_Pass1 (This : in Add_Expression; Gen : access Generate_Interface.Typ'Class) is
   begin
      Compile_Pass1(This.Left.all,Gen);
      Compile_Pass1(This.Right.All,Gen);
   end Compile_Pass1;
   procedure Compile_Pass1 (This : in Minus_Expression; Gen : access Generate_Interface.Typ'Class) is
   begin
      Compile_Pass1(This.Left.all,Gen);
      Compile_Pass1(This.Right.All,Gen);
   end Compile_Pass1;
   procedure Compile_Pass1 (This : in Relation; Gen : access Generate_Interface.Typ'Class) is
   begin
      Compile_Pass1(This.Left.All,Gen);
      Compile_Pass1(This.Right.All,Gen);
   end Compile_Pass1;
   procedure Compile_Pass1 (This : in Boolean0; Gen : access Generate_Interface.Typ'Class) is
   begin
      null;
   end Compile_Pass1;
   procedure Compile_Pass1 (This : in Boolean_Constant; Gen : access Generate_Interface.Typ'Class) is
   begin
      null;
   end Compile_Pass1;
   procedure Compile_Pass1 (This : in Boolean1; Gen : access Generate_Interface.Typ'Class) is
   begin
      Compile_Pass1(This.Parameter.all,Gen);
   end Compile_Pass1;
   procedure Compile_Pass1 (This : in Boolean_Reflection; Gen : access Generate_Interface.Typ'Class) is
   begin
      null;
   end Compile_Pass1;
   procedure Compile_Pass1 (This : in Boolean_Plugin; Gen : access Generate_Interface.Typ'Class) is
   begin
      null;
   end Compile_Pass1;
   procedure Compile_Pass1 (This : in Boolean2; Gen : access Generate_Interface.Typ'Class) is
   begin
      Compile_Pass1(This.Left.All,Gen);
   end Compile_Pass1;
   procedure Compile_Pass1 (This : in And_Boolean2; Gen : access Generate_Interface.Typ'Class) is
   begin
      Compile_Pass1(This.Left.All,Gen);
      Compile_Pass1(This.Right.All,Gen);
   end Compile_Pass1;
   procedure Compile_Pass1 (This : in Boolean_Expression; Gen : access Generate_Interface.Typ'Class) is
   begin
      Compile_Pass1(This.Left.All,Gen);
   end Compile_Pass1;
   procedure Compile_Pass1 (This : in Xor_Boolean; Gen : access Generate_Interface.Typ'Class) is
   begin
      Compile_Pass1(This.Left.All,Gen);
      Compile_Pass1(This.Right.All,Gen);
   end Compile_Pass1;
   procedure Compile_Pass1 (This : in Or_Boolean; Gen : access Generate_Interface.Typ'Class) is
   begin
      Compile_Pass1(This.Left.All,Gen);
      Compile_Pass1(This.Right.All,Gen);
   end Compile_Pass1;
   procedure Compile_Pass1 (This : in Input; Gen : access Generate_Interface.Typ'Class) is
   begin
      Compile_Pass1(This.Lhs.All,Gen);
   end Compile_Pass1;
--   procedure Compile_Pass1 (This : in Input_Array; Gen : access Generate_Interface.Typ'Class) is
--      T : Lexer.Token_Pointer;
--      M_Str : access MSSyst.String.Typ;
--   begin
--      T := This.Id;
--      M_Str := +Get_Text(T.Start..T.Finish);
--      Compile_Pass1(This.Reference.All,Gen);
--      if not Raptor.Compile_Helpers.Start_New_Declaration(
--         +To_Upper(Get_Text(T.Start..T.Finish))) then
--         Gen.Declare_As_1D_Array(M_Str);
--      end if;
--   end Compile_Pass1;
--   procedure Compile_Pass1 (This : in Input_2D_Array; Gen : access Generate_Interface.Typ'Class) is
--      T : Lexer.Token_Pointer;
--      M_Str : access MSSyst.String.Typ;
--   begin
--      T := This.Id;
--      M_Str := +Get_Text(T.Start..T.Finish);
--      Compile_Pass1(This.Reference.All,Gen);
--      Compile_Pass1(This.Reference2.All,Gen);
--      if not Raptor.Compile_Helpers.Start_New_Declaration(
--            +To_Upper(Get_Text(T.Start..T.Finish))) then
--         Gen.Declare_As_2D_Array(M_Str);
--      end if;
--   end Compile_Pass1;
   procedure Compile_Pass1 (This : in Expr_Output; Gen : access Generate_Interface.Typ'Class) is
   begin
      Compile_Pass1(This.Expr.all,Gen);
   end Compile_Pass1;
   procedure Compile_Pass1 (This : in String_Output; Gen : access Generate_Interface.Typ'Class) is
   begin
      null;
   end Compile_Pass1;

   -- Called by Component.collect_variable_names
   function Get_Name(This : in Assignment) return String is
   begin
      return Get_Name(This.Lhs.all);
   end Get_Name;

   function Top_Level_Negated (This : in Boolean_Expression) return Boolean is
   begin
      return this.Left.Top_Level_Negated;
   end Top_Level_Negated;

   function Top_Level_Negated (This : in Or_Boolean) return Boolean is
   begin
      return false;
   end Top_Level_Negated;

   function Top_Level_Negated (This : in Xor_Boolean) return Boolean is
   begin
      return false;
   end Top_Level_Negated;

   function Top_Level_Negated (This : in Boolean2) return Boolean is
   begin
      return This.Negated;
   end Top_Level_Negated;

   function Top_Level_Negated (This : in And_Boolean2) return Boolean is
   begin
      return false;
   end Top_Level_Negated;

   function Remove_Negation (This : in Xor_Boolean) return Boolean_Expression_Pointer is
   begin
      raise Constraint_Error;
      return null;
   end Remove_Negation;

   function Remove_Negation (This : in Or_Boolean) return Boolean_Expression_Pointer is
   begin
      raise Constraint_Error;
      return null;
   end Remove_Negation;

   function Remove_Negation (This : in And_Boolean2) return Boolean2_Pointer is
   begin
      raise Constraint_Error;
      return null;
   end Remove_Negation;

   function Remove_Negation (This : in Boolean2) return Boolean2_Pointer is
   begin
      return new Boolean2'(Negated => False, Left => This.Left);
   end Remove_Negation;

   function Remove_Negation (This : in Boolean_Expression) return Boolean_Expression_Pointer is
   begin
      return new Boolean_Expression'(Left => This.Left.Remove_Negation);
   end Remove_Negation;



   function Execute (This : in Rhs_Expon) return Value is
      V : Value;
   begin
      V := Execute(This.Rhs.All, this.RSuffix);
      return Execute(This.RSuffix.All, V);
   end Execute;

   procedure Emit_Code (This : in Rhs_Expon; Gen : access Generate_Interface.Typ'Class) is
   begin
      Emit_Code(This.Rhs.All,Gen);
      Emit_Code(This.Rsuffix.All,gen);
   end Emit_Code;


   procedure Compile_Pass1 (This : in Rhs_Expon; Gen : access Generate_Interface.Typ'Class) is
   begin
      Compile_Pass1(This.Rhs.all,Gen);
   end Compile_Pass1;

   function Execute (This : in Class_Expon) return Value is
      T : Token_Pointer;
      V : Value;
   begin
      T := This.Id;
      V:=Raptor.Runtime.CreateObject(Get_Text(T.Start..T.Finish));
      Raptor.Runtime.SetContext(Numbers.Object_Of(V));
      -- we store "this" in context for runtime.invoke_constructor
      Did_Method_Call := Raptor.Step_Helpers.Invoke_Constructor(Get_Text(T.Start..T.Finish),NULL);
      Raptor.Runtime.setContext(null);
      return v;
   end Execute;

   procedure Emit_Code (This : in Class_Expon; Gen : access Generate_Interface.Typ'Class) is
      O : MSSyst.Object.Ref;
      T : Token_Pointer;
   begin
      T := This.Id;
      O := To_Gen_OO(Generate_Interface.Generator(Gen)).Create_Object
         (Get_Text(T.Start..T.Finish));
      Gen.Emit_No_Parameters(O);
   end Emit_Code;

   procedure Compile_Pass1 (This : in Class_Expon; Gen : access Generate_Interface.Typ'Class) is
   begin
      null;
   end Compile_Pass1;

   function Execute (This : in Class_Constructor_Expon) return Value is
      T : Token_Pointer;
      V : Value;
   begin
      T := This.Id;
      V:=Raptor.Runtime.CreateObject(Get_Text(T.Start..T.Finish));
      -- we store "this" in context for runtime.invoke_constructor
      Raptor.Runtime.SetContext(Numbers.Object_Of(V));
      Did_Method_Call := Raptor.Step_Helpers.Invoke_Constructor(Get_Text(T.Start..T.Finish),This.Parameters);
      Raptor.Runtime.setContext(null);
      return v;
   end Execute;

   procedure Emit_Code (This : in Class_Constructor_Expon; Gen : access Generate_Interface.Typ'Class) is
      O : MSSyst.Object.Ref;
      T : Token_Pointer;
      Walk : Parameter_List_Pointer;
   begin
      T := This.Id;
      O := To_Gen_OO(Generate_Interface.Generator(Gen)).Create_Object
         (Get_Text(T.Start..T.Finish));
      if This.Parameters=null then
         gen.emit_no_parameters(o);
      else
         Walk := This.Parameters;
         while (walk/=null) loop
            Emit_Code(Expr_Output(Walk.Parameter.All).Expr.all,Gen);
            if (Walk.Next/=null) then
               Gen.Emit_Next_Parameter(O);
            else
               Gen.Emit_Last_Parameter(O);
            end if;
            Walk := walk.next;
         end loop;
      end if;
   end Emit_Code;

   procedure Compile_Pass1 (This : in Class_Constructor_Expon; Gen : access Generate_Interface.Typ'Class) is
      O : MSSyst.Object.Ref;
      T : Token_Pointer;
   begin
      T := This.Id;
      O := To_Gen_OO(Generate_Interface.Generator(Gen)).Create_Object
         (Get_Text(T.Start..T.Finish));
      Gen.Emit_No_Parameters(O);
   end Compile_Pass1;

   function Execute(This : in Id_Lhs; LSuffix : in LSuffix_Pointer; Context : in MSSyst.Object.Ref; V : Numbers.Value)
         return MSSyst.Object.Ref is
      T : Lexer.Token_Pointer;
   begin
      T := This.Id;
      Raptor.Runtime.setContext(Context);
      if LSuffix.all in Empty_LSuffix'Class then
         Raptor.Runtime.SetVariable(
            S => +to_upper(get_text(t.start..t.finish)),
               F => V);
         return null;
      else
         return mssyst.object.ref(Raptor.Runtime.getVariableContext(
            +to_upper(get_text(t.start..t.finish))));
      end if;
   end Execute;

   function Execute(This : in Array_Ref_Lhs; LSuffix : in LSuffix_Pointer; Context : in MSSyst.Object.Ref; V : Numbers.Value)
      return MSSyst.Object.Ref is
      T : Lexer.Token_Pointer;
      Reference : Numbers.Value;
   begin
      T := This.Id;
      Reference := Execute (This.Reference.All);
      if not Is_Integer(Reference) then
         Raise_Exception(Runtime_Error'Identity,
            Long_Float_Full_Image(Long_Float_Of(Reference)) & " not a valid array location--must be integer");
      end if;
      Raptor.Runtime.setContext(Context);
      if LSuffix.all in Empty_LSuffix'Class then
         raptor.Runtime.SetArrayElement(
            +To_Upper(Get_Text(T.Start..T.Finish)),
            Integer_Of(reference),
            V);
         return null;
      else
         return mssyst.object.ref(Raptor.Runtime.getVariableContext1D(
            +to_upper(get_text(t.start..t.finish)),
            Integer_Of(reference)));
      end if;
   end Execute;

   function Execute(This : in Array_2d_Ref_Lhs; LSuffix : in LSuffix_Pointer; Context : in MSSyst.Object.Ref; V : Numbers.Value)
      return MSSyst.Object.Ref is
      T : Lexer.Token_Pointer;
      Ref, Ref2 : Numbers.Value;
   begin
      T := This.Id;
      Ref := Execute (This.Reference.All);
      if not Is_Integer(Ref) then
         Raise_Exception(Runtime_Error'Identity,
            Long_Float_Full_Image(Long_Float_Of(Ref)) & " not a valid array location--must be integer");
      end if;
      Ref2 := Execute (This.Reference2.All);
      if not Is_Integer(Ref2) then
         Raise_Exception(Runtime_Error'Identity,
            Long_Float_Full_Image(Long_Float_Of(Ref2)) & " not a valid array location--must be integer");
      end if;
      Raptor.Runtime.setContext(Context);
      if LSuffix.all in Empty_LSuffix'Class then
         raptor.Runtime.Set2DArrayElement(
            +To_Upper(Get_Text(T.Start..T.Finish)),
            Integer_Of(Ref),
            Integer_Of(Ref2),
            V);
         return null;
      else
         return mssyst.object.ref(Raptor.Runtime.getVariableContext2D(
            +to_upper(get_text(t.start..t.finish)),
            Integer_Of(ref),
            Integer_Of(ref2)));
      end if;
   end Execute;

   procedure Execute (This : in Empty_LSuffix; Context : in MSSyst.Object.Ref; V : Numbers.Value) is
      Context2 : MSSyst.Object.Ref;
   begin
      null;
   end Execute;

   procedure Execute (This : in Full_LSuffix; Context : in MSSyst.Object.Ref; V : Numbers.Value) is
      Context2 : MSSyst.Object.Ref;
   begin
      Context2 := Execute(This.Lhs.All, This.LSuffix, Context, V);
      Execute(This.LSuffix.all, Context2, V);
   end Execute;

   procedure Execute (This : in Expr_Assignment) is
      V : Value;
      Context : MSSyst.Object.Ref;
   begin
      Raptor.Runtime.SetContext(NULL);
      V := Execute (This.Expr_Part.All);
      Context := Execute(This.Lhs.All, This.LSuffix, null, V);
      Execute(This.LSuffix.All, Context, V);
      Raptor.Runtime.setContext(null);
   end Execute;


   procedure Compile_Pass1 (This : in Expr_Assignment; Gen : access Generate_Interface.Typ'Class) is
   begin
      Compile_Pass1(This.Lhs.all,Gen);
   end Compile_Pass1;

   function Execute(This : in Empty_RSuffix; V : Numbers.Value) return Numbers.Value is
   begin
      return V;
   end Execute;

   function Execute(This : in Full_RSuffix; V : Numbers.Value) return Numbers.Value is
      V2 : Value;
   begin
      V2 := Execute(This.Rhs.All, This.RSuffix);
      return Execute(This.RSuffix.All, V2);
   end Execute;

   function Execute (This : in Rhs_Method_Call; Suffix : in RSuffix_Pointer) return Value is
      T : Lexer.Token_Pointer;
      V : Numbers.Value;
   begin
      T := This.Id;
      if Suffix.All in Empty_RSuffix'Class then
         Did_Method_Call := True;
         Did_Function_Call := True;
         Raptor.Step_Helpers.Invoke_Method(Get_Text(T.Start..T.Finish),
            this.parameters);
         Raptor.Runtime.setContext(null);
         return V;
      else
         Raise_Exception(Runtime_Error'Identity,
            "can't dereference result of method call");
      end if;
      return Numbers.Pi;
   end Execute;


   function Execute (This : in Array_2D_Ref_Rhs; Suffix : in RSuffix_Pointer) return Value is
      T : Lexer.Token_Pointer;
      Ref, Ref2 : Numbers.Value;
      V : Numbers.Value;
      context : mssyst.object.ref;
   begin
      T := This.Id;
      Context := Mssyst.Object.Ref(Raptor.Runtime.GetContextObject);
      raptor.runtime.setContext(null);
      Ref := Execute (This.Reference.All);
      if not Is_Integer(Ref) then
         Raise_Exception(Runtime_Error'Identity,
            Long_Float_Full_Image(Long_Float_Of(Ref)) & " not a valid array location--must be integer");
      end if;
      raptor.runtime.setContext(null);
      Ref2 := Execute (This.Reference2.All);
      if not Is_Integer(Ref2) then
         Raise_Exception(Runtime_Error'Identity,
            Long_Float_Full_Image(Long_Float_Of(Ref2)) & " not a valid array location--must be integer");
      end if;
      raptor.runtime.setContext(context);
      if Suffix.all in Empty_RSuffix'Class then
         V:=raptor.Runtime.get2DArrayElement(
            +To_Upper(Get_Text(T.Start..T.Finish)),
            Integer_Of(Ref),
            Integer_Of(Ref2));
         Raptor.Runtime.setContext(null);
         return V;
      else
         Raptor.Runtime.setContext(mssyst.object.ref(Raptor.Runtime.getVariableContext2D(
            +to_upper(get_text(t.start..t.finish)),
            Integer_Of(ref),
               Integer_Of(Ref2))));
         return Numbers.Pi;
      end if;
   end Execute;


   function Execute (This : in Array_Ref_Rhs; Suffix : in RSuffix_Pointer) return Value is
      T : Lexer.Token_Pointer;
      Reference : Numbers.Value;
      V : Numbers.Value;
      context : mssyst.object.ref;
   begin
      T := This.Id;
      Context := Mssyst.Object.Ref(Raptor.Runtime.GetContextObject);
      raptor.runtime.setContext(null);
      Reference := Execute (This.Reference.All);
      if not Is_Integer(Reference) then
         Raise_Exception(Runtime_Error'Identity,
            Long_Float_Full_Image(Long_Float_Of(Reference)) & " not a valid array location--must be integer");
      END IF;
      raptor.runtime.setContext(context);
      if Suffix.all in Empty_RSuffix'Class then
         V:=raptor.Runtime.GetArrayElement(
            +To_Upper(Get_Text(T.Start..T.Finish)),
            Integer_Of(reference));
         Raptor.Runtime.setContext(null);
         return V;
      else
         Raptor.Runtime.setContext(mssyst.object.ref(Raptor.Runtime.getVariableContext1D(
            +to_upper(get_text(t.start..t.finish)),
                  Integer_Of(Reference))));
         return Numbers.Pi;
      end if;
   end Execute;
   function Execute (This : in Id_Rhs; Suffix : in RSuffix_Pointer) return Value is
      T : Lexer.Token_Pointer;
      V : Numbers.Value;
   begin
      T := This.Id;
      if Suffix.all in Empty_RSuffix'Class then
         V:=Raptor.Runtime.GetVariable(
            S => +to_upper(get_text(t.start..t.finish)));
         Raptor.Runtime.setContext(null);
         return V;
      else
         Raptor.Runtime.setContext(mssyst.object.ref(Raptor.Runtime.getVariableContext(
                  +To_Upper(Get_Text(T.Start..T.Finish)))));
         return Numbers.Pi;
      end if;
   end Execute;


   procedure Execute (This : in Method_Proc_Call) is
   begin
      -- do I want to do Method_Call or just execute here??
      -- If I do execute, can I then ask my context what its methods are and call one?
      if not raptor.Step_Helpers.am_leaving then
         Method_Call(This.Lhs.All);
      end if;
      Method_Call(This.MSuffix.All);
   end Execute;

   procedure Emit_Code (This : in Method_Proc_Call; Gen : access Generate_Interface.Typ'Class) is
   begin
      Gen.Indent;
      Emit_Method(This.Lhs.All,Gen);
      to_gen_oo(Generate_Interface.Generator(Gen)).Emit_Dereference;
      Emit_Code(This.MSuffix.All,Gen);
   end Emit_Code;

   procedure Compile_Pass1 (This : in Method_Proc_Call; Gen : access Generate_Interface.Typ'Class) is
   begin
      Compile_Pass1(This.Lhs.All,Gen);
   end Compile_Pass1;

   procedure Method_Call (This : in Full_MSuffix) is
   begin
      if not raptor.Step_Helpers.am_leaving then
         Method_Call(This.Lhs.All);
      end if;
      Method_Call(This.MSuffix.All);
   end Method_Call;

   procedure Method_Call (This : in NoParam_MSuffix) is
      T : Lexer.Token_Pointer;
   begin
      T := This.Id;
      Raptor.Step_Helpers.Invoke_Method(+To_Lower(Get_Text(T.Start..T.Finish)),null);
      Raptor.Runtime.setContext(null);
   end Method_Call;

   procedure Method_Call (This : in Param_MSuffix) is
      T : Lexer.Token_Pointer;
   begin
      T := This.Id;
      raptor.Step_Helpers.invoke_method(+To_Lower(Get_Text(T.Start..T.Finish)),this.param_list);
      Raptor.Runtime.setContext(null);
   end Method_Call;

   procedure Method_Call(this : in Id_Lhs) is
      T : Lexer.Token_Pointer;
   begin
      T := This.Id;
      Raptor.Runtime.setContext(Raptor.Runtime.getVariableContext(
            +to_upper(get_text(t.start..t.finish))));
   end Method_Call;

   procedure Method_Call(this : in Array_Ref_Lhs) is
      T : Lexer.Token_Pointer;
      Reference : Numbers.Value;
   begin
      T := This.Id;
      Reference := Execute (This.Reference.All);
      if not Is_Integer(Reference) then
         Raise_Exception(Runtime_Error'Identity,
            Long_Float_Full_Image(Long_Float_Of(Reference)) & " not a valid array location--must be integer");
      end if;
      Raptor.Runtime.setContext(Raptor.Runtime.getVariableContext1D(
            +to_upper(get_text(t.start..t.finish)),
            Integer_Of(reference)));
   end Method_Call;

   procedure Method_Call(this : in Array_2D_Ref_Lhs) is
      T : Lexer.Token_Pointer;
      Ref, Ref2 : Numbers.Value;
   begin
      T := This.Id;
      Ref := Execute (This.Reference.All);
      if not Is_Integer(Ref) then
         Raise_Exception(Runtime_Error'Identity,
            Long_Float_Full_Image(Long_Float_Of(Ref)) & " not a valid array location--must be integer");
      end if;
      Ref2 := Execute (This.Reference2.All);
      if not Is_Integer(Ref2) then
         Raise_Exception(Runtime_Error'Identity,
            Long_Float_Full_Image(Long_Float_Of(Ref2)) & " not a valid array location--must be integer");
      end if;
      Raptor.Runtime.setContext(Raptor.Runtime.getVariableContext2D(
            +to_upper(get_text(t.start..t.finish)),
            Integer_Of(ref),
            Integer_Of(ref2)));
   end Method_Call;
   function Count_Parameters(L : Parameter_List_Pointer) return Integer is
   begin
      if L=null then
         return 0;
      else
         return 1 + Count_Parameters(L.Next);
      end if;
   end Count_Parameters;
--   type Expon_Stub is new Expon with record
--      Component : MSSyst.Object.Ref;
--      Index     : Integer;
--      Expon_Parse_Tree : Expon_Pointer;
--   end record;
   function Execute (This : in Expon_Stub) return Value is
   begin
      return raptor.ParseHelpers.getValue(this.component,this.index);
   end Execute;

   procedure Emit_Code (This : in Expon_Stub; Gen : access Generate_Interface.Typ'Class) is
   begin
      Emit_Code(This.Expon_Parse_Tree.all,Gen);
   end Emit_Code;

   procedure Compile_Pass1 (This : in Expon_Stub; Gen : access Generate_Interface.Typ'Class) is
   begin
      Compile_Pass1(This.Expon_Parse_Tree.all,Gen);
   end Compile_Pass1;

   function Is_Method_Call(This : in Rhs_Method_Call) return Boolean is
   begin
      return True;
   end Is_Method_Call;
   function Is_Method_Call(This : in Rhs) return Boolean is
   begin
      return False;
   end Is_Method_Call;
   function Is_Method_Call(This : in Rhs_Expon) return Boolean is
      Y : access RSuffix'Class := This.RSuffix;
      Z : access Rhs'Class := This.Rhs;
   begin
      loop
         if Y.All in Empty_Rsuffix'Class then
            return Is_Method_Call(Z.All);
         end if;
         Z := Full_Rsuffix(Y.All).Rhs;
         Y := Full_Rsuffix(Y.All).Rsuffix;
      end loop;
   end Is_Method_Call;

   procedure Execute (This : in Method_Proc_Call_This) is
   begin
      Raptor.Step_Helpers.Invoke_This_Constructor(This.Param_List);
   end Execute;

   procedure Emit_Code (This : in Method_Proc_Call_This; Gen : access Generate_Interface.Typ'Class) is
   begin
      Gen.Indent;
      Emit_Code(This.MSuffix.All,Gen);
   end Emit_Code;

   procedure Compile_Pass1 (This : in Method_Proc_Call_This; Gen : access Generate_Interface.Typ'Class) is
   begin
      null;
   end Compile_Pass1;

   procedure Execute (This : in Method_Proc_Call_Super) is
   begin
      Raptor.Step_Helpers.Invoke_Super_Constructor(This.Param_List);
   end Execute;

   procedure Emit_Code (This : in Method_Proc_Call_Super; Gen : access Generate_Interface.Typ'Class) is
   begin
      Gen.Indent;
      Emit_Code(This.MSuffix.All,Gen);
   end Emit_Code;

   procedure Compile_Pass1 (This : in Method_Proc_Call_Super; Gen : access Generate_Interface.Typ'Class)is
   begin
      null;
   end Compile_Pass1;

   procedure Emit_Method (This : in Id_Lhs; Gen : access Generate_Interface.Typ'Class) is
      T : Token_Pointer;
      M_Str : MSSyst.String.Ref;
   begin
      T := This.Id;
      M_Str := +Get_Text(T.Start..T.Finish);
      Gen.Emit_Load(M_Str);
   end Emit_Method;

   procedure Emit_Method (This : in Array_Ref_Lhs; Gen : access Generate_Interface.Typ'Class) is
      T : Token_Pointer;
      M_Str : MSSyst.String.Ref;
      O : MSSyst.Object.Ref;
   begin
      T := This.Id;
      M_Str := +Get_Text(T.Start..T.Finish);
      Gen.Emit_Load_Array_Start(M_Str);
      Emit_Code(This.Reference.All,Gen);
      Gen.Emit_Load_Array_After_Index(M_Str);
   end Emit_Method;


   procedure Emit_Method (This : in Array_2D_Ref_Lhs; Gen : access Generate_Interface.Typ'Class) is
      T : Token_Pointer;
      M_Str : MSSyst.String.Ref;
      O : MSSyst.Object.Ref;
   begin
      T := This.Id;
      M_Str := +Get_Text(T.Start..T.Finish);
      Gen.Emit_Load_Array_2D_Start(M_Str);
      Emit_Code(This.Reference.All,Gen);
      Gen.Emit_Load_Array_2D_Between_Indices;
      Emit_Code(This.Reference2.All,Gen);
      Gen.Emit_Load_Array_2D_After_Indices(M_Str);
   end Emit_Method;

   procedure Emit_Code (This : in Id_Lhs; LSuffix : in LSuffix_Pointer; Gen : access Generate_Interface.Typ'Class) is
      T : Token_Pointer;
      M_Str : MSSyst.String.Ref;
      O : MSSyst.Object.Ref;
   begin
      T := This.Id;
      M_Str := +Get_Text(T.Start..T.Finish);
      if LSuffix.all in Empty_LSuffix'Class then
         case Emit_Context is
            when Assign_Context =>
               Gen.Variable_Assignment_Start(M_Str);
               Emit_Kind := Variable;
            when Input_Context =>
               Gen.Input_Start_Variable(M_Str);
            when Call_Context =>
               O := MSSyst.Object.Ref(Gen.Emit_Call_Subchart(M_Str));
               Gen.Emit_No_Parameters(O);
         end case;
      else
         Gen.Emit_Load(M_Str);
      end if;
   end Emit_Code;

   procedure Emit_Code (This : in Array_Ref_Lhs; LSuffix : in LSuffix_Pointer; Gen : access Generate_Interface.Typ'Class) is
      T : Token_Pointer;
      M_Str : MSSyst.String.Ref;
      O : MSSyst.Object.Ref;
   begin
      T := This.Id;
      M_Str := +Get_Text(T.Start..T.Finish);
      if LSuffix.all in Empty_LSuffix'Class then
         case Emit_Context is
            when Assign_Context =>
               Gen.Array_1D_Assignment_Start(M_Str);
               Emit_Code(This.Reference.All,Gen);
               Gen.Array_1D_Assignment_After_Index;
               Emit_Kind := Array_1D;
            when Input_Context =>
               Gen.Input_Start_Array_1D(M_Str,This.Reference);
            when Call_Context =>
               raise Constraint_Error;
         end case;
      else
         Gen.Emit_Load_Array_Start(M_Str);
         Emit_Code(This.Reference.All,Gen);
         Gen.Emit_Load_Array_After_Index(M_Str);
      end if;
   end Emit_Code;
   procedure Emit_Code (This : in Array_2D_Ref_Lhs; LSuffix : in LSuffix_Pointer; Gen : access Generate_Interface.Typ'Class) is
      T : Token_Pointer;
      M_Str : MSSyst.String.Ref;
      O : MSSyst.Object.Ref;
   begin
      T := This.Id;
      M_Str := +Get_Text(T.Start..T.Finish);
      if LSuffix.all in Empty_LSuffix'Class then
         case Emit_Context is
            when Assign_Context =>
               Gen.Array_2D_Assignment_Start(M_Str);
               Emit_Code(This.Reference.All,Gen);
               Gen.Array_2D_Assignment_Between_Indices;
               Emit_Code(This.Reference2.All,Gen);
               Gen.Array_2D_Assignment_After_Indices;
               Emit_Kind := Array_2D;
            when Input_Context =>
               Gen.Input_Start_Array_2D(M_Str,This.Reference,This.Reference2);
            when Call_Context =>
               raise Constraint_Error;
         end case;
      else
         Gen.Emit_Load_Array_2D_Start(M_Str);
         Emit_Code(This.Reference.All,Gen);
         Gen.Emit_Load_Array_2D_Between_Indices;
         Emit_Code(This.Reference2.All,Gen);
         Gen.Emit_Load_Array_2D_After_Indices(M_Str);
      end if;
   end Emit_Code;
   procedure Emit_Code (This : in Full_LSuffix; Gen : access Generate_Interface.Typ'Class) is
   begin
      to_gen_oo(Generate_Interface.Generator(Gen)).Emit_Dereference;
      Emit_Code(This.Lhs.All,This.LSuffix,Gen);
      Emit_Code(This.LSuffix.All,Gen);
   end Emit_Code;

   procedure Emit_Code (This : in Empty_LSuffix; Gen : access Generate_Interface.Typ'Class) is
   begin
      null;
   end Emit_Code;

   procedure Emit_Code (This : in Id_Rhs; Gen : access Generate_Interface.Typ'Class) is
      T : Lexer.Token_Pointer;
   begin
      T := This.Id;
      Gen.Emit_Load(Get_Text(T.Start..T.Finish));
   end Emit_Code;
   procedure Emit_Code (This : in Array_Ref_Rhs; Gen : access Generate_Interface.Typ'Class) is
      T : Lexer.Token_Pointer;
   begin
      T := This.Id;
      Gen.Emit_Load_Array_Start(Get_Text(T.Start..T.Finish));
      Emit_Code(This.Reference.all,Gen);
      Gen.Emit_Load_Array_After_Index(Get_Text(T.Start..T.Finish));
   end Emit_Code;
   procedure Emit_Code (This : in Array_2D_Ref_Rhs; Gen : access Generate_Interface.Typ'Class) is
      T : Lexer.Token_Pointer;
   begin
      T := This.Id;
      Gen.Emit_Load_Array_2D_Start(Get_Text(T.Start..T.Finish));
      Emit_Code(This.Reference.All,Gen);
      Gen.Emit_Load_Array_2D_Between_Indices;
      Emit_Code(This.Reference2.all,Gen);
      Gen.Emit_Load_Array_2D_After_Indices(Get_Text(T.Start..T.Finish));
   end Emit_Code;

   procedure Emit_Code (This : in Full_RSuffix; Gen : access Generate_Interface.Typ'Class) is
   begin
      to_gen_oo(Generate_Interface.Generator(Gen)).Emit_Dereference;
      Emit_Code(This.Rhs.all,Gen);
      Emit_Code(This.RSuffix.all,Gen);
   end Emit_Code;

   procedure Emit_Code (This : in Full_MSuffix; Gen : access Generate_Interface.Typ'Class) is
   begin
      Emit_Method(This.Lhs.all,Gen);
      to_gen_oo(Generate_Interface.Generator(Gen)).Emit_Dereference;
      Emit_Code(This.MSuffix.all,Gen);
   end Emit_Code;


   procedure Emit_Code (This : in Empty_RSuffix; Gen : access Generate_Interface.Typ'Class) is
   begin
      null;
   end Emit_Code;

   procedure Emit_Code (This : in NoParam_MSuffix; Gen : access Generate_Interface.Typ'Class) is
      T : Lexer.Token_Pointer;
      O : MSSyst.Object.Ref;
   begin
      T := This.Id;
      O := To_Gen_Oo(Generate_Interface.Generator(Gen)).Emit_Call_Oo_Method(
         Get_Text(T.Start..T.Finish),false);
      gen.emit_no_parameters(o);
   end Emit_Code;

   procedure Emit_Code (This : in Param_MSuffix; Gen : access Generate_Interface.Typ'Class) is
      T : Lexer.Token_Pointer;
      O : MSSyst.Object.Ref;
      walk : parameter_list_pointer;
   begin
      T := This.Id;
      O := To_Gen_Oo(Generate_Interface.Generator(Gen)).Emit_Call_Oo_Method(
         Get_Text(T.Start..T.Finish),false);
      if This.Param_List=null then
         gen.emit_no_parameters(o);
      else
         Walk := This.Param_List;
         while (walk/=null) loop
            Emit_Code(Expr_Output(Walk.Parameter.All).Expr.all,Gen);
            if (Walk.Next/=null) then
               Gen.Emit_Next_Parameter(O);
            else
               Gen.Emit_Last_Parameter(O);
            end if;
            Walk := Walk.Next;
         end loop;
      end if;
   end Emit_Code;

   procedure Emit_Code (This : in Rhs_Method_Call; Gen : access Generate_Interface.Typ'Class) is
      T : Lexer.Token_Pointer;
      O : MSSyst.Object.Ref;
      walk : parameter_list_pointer;
   begin
      T := This.Id;
      O := To_Gen_Oo(Generate_Interface.Generator(Gen)).Emit_Call_Oo_Method(
         Get_Text(T.Start..T.Finish),true);
      if This.Parameters=null then
         gen.emit_no_parameters(o);
      else
         Walk := This.Parameters;
         while (walk/=null) loop
            Emit_Code(Expr_Output(Walk.Parameter.All).Expr.all,Gen);
            if (Walk.Next/=null) then
               Gen.Emit_Next_Parameter(O);
            else
               Gen.Emit_Last_Parameter(O);
            end if;
            Walk := Walk.Next;
         end loop;
      end if;
   end Emit_Code;

   -- return left
   function Get_Class_Decl(This : in Expression) return MSSyst.String.Ref is
   begin
      return Get_Class_Decl(This.Left.All);
   end get_class_decl;
   -- return null
   function Get_Class_Decl(This : in Binary_Expression) return MSSyst.String.Ref is
   begin
      return null;
   end get_class_decl;
   -- return left
   function Get_Class_Decl(This : in Add) return MSSyst.String.Ref is
   begin
      return Get_Class_Decl(This.Left.All);
   end get_class_decl;
   -- return null
   function Get_Class_Decl(This : in Binary_Add) return MSSyst.String.Ref is
   begin
      return null;
   end get_class_decl;
   -- return left
   function Get_Class_Decl(This : in Mult) return MSSyst.String.Ref is
   begin
      return Get_Class_Decl(This.Left.All);
   end get_class_decl;
   -- return null
   function Get_Class_Decl(This : in Expon_Mult) return MSSyst.String.Ref is
   begin
      return null;
   end get_class_decl;
   -- return expon_parse_tree
   function Get_Class_Decl(This : in Expon_Stub) return MSSyst.String.Ref is
   begin
      return Get_Class_Decl(This.Expon_Parse_Tree.All);
   end Get_Class_Decl;
   -- return null
   function Get_Class_Decl(This : in Expon) return MSSyst.String.Ref is
   begin
      return null;
   end get_class_decl;
   -- return ID (token)
   function Get_Class_Decl(This : in Class_Expon) return MSSyst.String.Ref is
      T : Token_Pointer;
   begin
      T := This.Id;
      return (+Get_Text(T.Start..T.Finish));
   end Get_Class_Decl;

   -- return ID (token)
   function Get_Class_Decl(This : in Class_Constructor_Expon) return MSSyst.String.Ref is
      T : Token_Pointer;
   begin
      T := This.Id;
      return (+Get_Text(T.Start..T.Finish));
   end Get_Class_Decl;
end Parse_Tree;

