with Lexer;
use Lexer;
with Ada.Exceptions;
use Ada.Exceptions;

with raptor.Plugins;
with MSSyst.String;
use Mssyst.String;
with Raptor.Subchart;
with Raptor.Runtime;
with Raptor.ParseHelpers;
with Ada.Unchecked_Conversion;
with Ada.Characters.Handling;
use Ada.Characters.Handling;
with Raptor.Runtime;

package body Parser is
   Current_Token : Token_Pointer;
   function Get_Current_Token return Token_Pointer is
   begin
      return Current_Token;
   end Get_Current_Token;

   Expression_Object : MSSyst.Object.Ref;
   PROCEDURE Set_Expression_Object(O : IN Mssyst.Object.Ref) IS
      use type Mssyst.object.ref;
   begin
      Expression_Object := O;
      if O/=null then
         Raptor.ParseHelpers.ClearExpressions(O);
      end if;
   end Set_Expression_Object;

   procedure Raise_Exception(T : Token_Pointer;
         Message : String) is
   begin
      Current_Token := T;
      Raise_Exception(Syntax_Error'Identity,
         Message);
   end Raise_Exception;

   -- Lhs => id[\[Expression[,Expression]\]]
   function Parse_Lhs(Ident : Token_Pointer) return Lhs_Pointer is
      T : Token_Pointer;
      E,E2 : Expression_Pointer;
      Ref   : Array_Ref_Lhs_Pointer;
   begin
      T := Get_Token;
      if T.Kind = Left_Bracket then
         E := Parse_Expression;
         Fix_Associativity(E);
         T := Get_Token;
         if T.Kind = Comma then
            E2 := Parse_Expression;
            Fix_Associativity(E2);
            Ref := new Array_2D_Ref_Lhs;
            Ref.Id := Ident;
            Ref.Reference := E;
            Array_2d_Ref_Lhs_Pointer(Ref).Reference2 := E2;
            T := Get_Token;
         else
            Ref := new Array_Ref_Lhs;
            Ref.Id := Ident;
            Ref.Reference := E;
         end if;
         if T.Kind /= Right_Bracket then
            Raise_Exception(T,
               "missing ]");
         end if;
         return Lhs_Pointer(Ref);
      else
         Unget_Token(T);
         return new Id_Lhs'(Id => Ident);
      end if;   end Parse_Lhs;

   function Parse_LSuffix return Lsuffix_Pointer is
      T : Token_Pointer;
   begin
      T := Get_Token;
      if T.Kind = Dot then
         if not Raptor.Runtime.isObjectOriented then
            Raise_Exception(T,"can't use '.' in variable name");
         end if;
         T := Get_Token;
         if T.Kind /= Id then
            Raise_Exception(T, "expected name");
         end if;
         return new Full_LSuffix'(Lhs => Parse_Lhs(T), LSuffix => Parse_LSuffix);
      else
         Unget_Token(T);
         return new Empty_Lsuffix;
      end if;
   end Parse_Lsuffix;

   -- Assignment => Lhs LSuffix [=|:=] Expression
   function Parse_Assignment return Assignment_Pointer is
      Result : Assignment_Pointer := new Expr_Assignment;
      E : Expression_Pointer;
      T : Token_Pointer;
   begin
      T := Get_Token;
      if T.Kind /= Id then
         Raise_Exception(T,
            "assignment must begin with variable");
      end if;
      Result.Lhs := Parse_Lhs(T);
      Result.LSuffix := Parse_LSuffix;
      T := Get_Token;
      if T.Kind /= Equal and T.Kind /= Colon_Equal then
         Raise_Exception(T,
            "can only set 1 variable, found: " & lexer.Token_Type'image(t.kind));
      end if;
      E := Parse_Expression;
      Fix_Associativity(E);
      Expr_Assignment(Result.All).Expr_Part := E;
      return Result;
   end Parse_Assignment;

   function Count_Parameters(L : in Parameter_List_Pointer) return Natural is
   begin
      if L = null then
         return 0;
      else
         return 1 + Count_Parameters(L.Next);
      end if;
   end Count_Parameters;


   -- Rhs => id[\[Expression[,Expression]\]] | id(Expression_List)
   function Parse_Rhs(Ident : Token_Pointer) return Rhs_Pointer is
      T : Token_Pointer;
      E,E2 : Expression_Pointer;
      Ref   : Array_Ref_Rhs_Pointer;
   begin
      T := Get_Token;
      if T.Kind = Left_Paren then
         if not Raptor.Runtime.isObjectOriented then
            Raise_Exception(T,Lexer.Get_Text(Ident.Start..Ident.Finish) &
               " is not a function.");
         end if;
         declare
            Result : Rhs_Method_Call_Pointer :=
               new Rhs_Method_Call;
         begin
            Result.Id := Ident;
            Result.Parameters := Parse_Parameter_List(
               Lexer.Get_Text(Result.Id.Start..Result.Id.Finish),True);
            T := Get_Token;
            if T.Kind /= Right_Paren then
               Raise_Exception(T,
                  "missing right parenthesis");
            end if;
            return Rhs_Pointer(Result);
         end;
      elsif T.Kind = Left_Bracket then
         E := Parse_Expression;
         Fix_Associativity(E);
         T := Get_Token;
         if T.Kind = Comma then
            E2 := Parse_Expression;
            Fix_Associativity(E2);
            Ref := new Array_2D_Ref_Rhs;
            Ref.Id := Ident;
            Ref.Reference := E;
            Array_2d_Ref_Rhs_Pointer(Ref).Reference2 := E2;
            T := Get_Token;
         else
            Ref := new Array_Ref_Rhs;
            Ref.Id := Ident;
            Ref.Reference := E;
         end if;
         if T.Kind /= Right_Bracket then
            Raise_Exception(T,
               "missing ]");
         end if;
         return Rhs_Pointer(Ref);
      else
         Unget_Token(T);
         return new Id_Rhs'(Id => Ident);
      end if;   end Parse_Rhs;

   function Parse_RSuffix return Rsuffix_Pointer is
      T : Token_Pointer;
   begin
      T := Get_Token;
      if T.Kind = Dot then
         if not Raptor.Runtime.isObjectOriented then
            Raise_Exception(T,"can't use '.' in variable name");
         end if;

         T := Get_Token;
         if T.Kind /= Id then
            Raise_Exception(T, "expected name");
         end if;
         return new Full_RSuffix'(Rhs => Parse_Rhs(T), RSuffix => Parse_RSuffix);
      else
         Unget_Token(T);
         return new Empty_Rsuffix;
      end if;
   end Parse_Rsuffix;

   -- Rsuffix => . Rhs Rsuffix | lambda
   -- Expon => Rhs RSuffix | num | (Expression)
   --          | new id | new id(Expression_List)
   --          | func_id(Expression_List) | func_id0 | -Expon
   --          | plugin_func_id(Expression_List)
   function Parse_Expon return Expon_Pointer is
      T : Token_Pointer;
   begin
      T := Get_Token;
      if T.Kind = Id then
         declare
            Ident : Token_Pointer := T;
         begin
            T := Get_Token;
            if T.Kind = Left_Paren then
               if raptor.Plugins.Is_Function(
                     Name => +Get_Text(Ident.Start..Ident.Finish)) then
                  declare
                     Result : Plugin_Func_Expon_Pointer :=
                        new Plugin_Func_Expon;
                     Count : Natural;
                     Correct_Count : Integer;
                  begin
                     Result.Id := Ident;
                     Result.Parameters := Parse_Parameter_List(
                        Lexer.Get_Text(Result.Id.Start..Result.Id.Finish),False);
                     Count := Count_Parameters(Result.Parameters);
                     Correct_Count := raptor.Plugins.Parameter_Count(
                           +Lexer.Get_Text(Result.Id.Start..Result.Id.Finish));
                     if Count /= Correct_Count then
                        Raise_Exception(T,
                           Lexer.Get_Text(Result.Id.Start..Result.Id.Finish) &
                           " should have" & Integer'Image(Correct_Count) &
                           " parameters.");
                     end if;
                     T := Get_Token;
                     if T.Kind /= Right_Paren then
                        Raise_Exception(T,
                           "missing right parenthesis");
                     end if;
                     return Expon_Pointer(Result);
                  end;
               end if;
            end if;
            Unget_Token(T);
            if raptor.Plugins.Is_Function(
                  Name => +Get_Text(Ident.Start..Ident.Finish)) then
               declare
                  Result : Plugin_Func_Expon_Pointer :=
                     new Plugin_Func_Expon;
                  Count : Natural;
                  Correct_Count : Integer;
               begin
                  Result.Id := Ident;
                  Result.Parameters := null;
                  Count := Count_Parameters(Result.Parameters);
                  Correct_Count := raptor.Plugins.Parameter_Count(
                        +Lexer.Get_Text(Result.Id.Start..Result.Id.Finish));
                  if Count /= Correct_Count then
                     Raise_Exception(T,
                        Lexer.Get_Text(Result.Id.Start..Result.Id.Finish) &
                        " should have" & Integer'Image(Correct_Count) &
                        " parameters.");
                  end if;
                  return Expon_Pointer(Result);
               end;
            end if;
            declare
               Result : Rhs_Expon_Pointer;
               function toObject is new ada.Unchecked_Conversion(rhs_expon_pointer,mssyst.object.ref);
            begin
               Result := new Rhs_Expon'(
                     Rhs => Parse_Rhs(Ident => Ident),
                     Rsuffix => Parse_Rsuffix);
               if not result.is_method_call then
                  return Expon_Pointer(Result);
               else
                  return new Expon_Stub'(
                     Component => Expression_Object,
                     Index => raptor.ParseHelpers.addExpression(Expression_Object, toObject(Result)),
                     Expon_Parse_Tree => expon_pointer(Result));
               end if;
            end;
         end;
      -- create instance object of a class
      elsif T.Kind = New_T then
         T := Get_Token;
         if T.Kind /= Id then
            Raise_Exception(T,
               "Expected name of class, found: " &
               Lexer.Get_Text(T.Start..T.Finish));
         end if;
         -- mcc: TODO check that this is really a class
         -- this will require modifying the stuff for class deletion/renaming also
         declare
            Ident : Token_Pointer := T;
            Result : Expon_Pointer;
            Retval : Expon_Pointer;
            function to_object is new ada.Unchecked_Conversion(expon_Pointer,mssyst.object.ref);
         begin
            T := Get_Token;
            if T.Kind = Left_Paren then
               Result := new Class_Constructor_Expon'(Ident,Parse_Parameter_List(
                  Lexer.Get_Text(Ident.Start..Ident.Finish),True));
               T := Get_Token;
               if T.Kind /= Right_Paren then
                  Raise_Exception(T,
                     "missing right parenthesis");
               end if;
            else
               Unget_Token(T);
               Result := new Class_Expon;
               Class_Expon_Pointer(Result).Id := Ident;
            end if;
            Retval := new Expon_Stub'(
               Component => Expression_Object,
               Index => Raptor.ParseHelpers.AddExpression(Expression_Object,to_object(Result)),
               Expon_Parse_Tree => Result);
            return Retval;
         end;
      elsif T.Kind = Character_T then
         --Raise_Exception(T,"Can't use string here.");
         return new Character_Expon'(S => T);
      elsif T.Kind = String_T then
         --Raise_Exception(T,"Can't use string here.");
         return new String_Expon'(S => T);
      elsif T.Kind = Number then
         return new Number_Expon'(Number => T);
      elsif T.Kind = Left_Paren then
         declare
            E : Expression_Pointer := Parse_Expression;
         begin
            Fix_Associativity(E);
            T := Get_Token;
            if T.Kind /= Right_Paren then
               Raise_Exception(T,
                  "missing right parenthesis");
            end if;
            return new Paren_Expon'(Expr_Part => E);
         end;
      elsif T.Kind in Lexer.Func_Id0 then
         return new Func0_Expon'(Id => T);
      elsif T.Kind = Lexer.Minus then
         return new Negative_Expon'(E => Parse_Expon);
      elsif T.Kind in Lexer.Func_Token_Types then
         declare
            Func_Kind : Lexer.func_token_types := T.Kind;
            Result : Func_Expon_Pointer := new Func_Expon;
            Count : Natural;
         begin
            Result.Id := T;
            T := Get_Token;
            if T.Kind /= Left_Paren then
               Raise_Exception(T,
                  "missing left parenthesis");
            end if;
            Result.Parameters := Parse_Parameter_List(
               Lexer.Get_Text(Result.Id.Start..Result.Id.Finish),False);
            Count := Count_Parameters(Result.Parameters);
            case Func_Kind is
               when Lexer.Func_Id0 =>
                  if Count /= 0 then
                     Raise_Exception(T,
                        Lexer.Get_Text(Result.Id.Start..Result.Id.Finish) &
                        "should not have parameters");
                  end if;
               when Lexer.Func_Id1 | Lexer.Load_Bitmap =>
                  if Count /= 1 then
                     Raise_Exception(T,
                        Lexer.Get_Text(Result.Id.Start..Result.Id.Finish) &
                        "should have one parameter, not" &
                           integer'image(count));
                  end if;
               when Lexer.Func_Id1or2 =>
                  if Count /= 1 and Count /= 2 then
                     Raise_Exception(T,
                        Lexer.Get_Text(Result.Id.Start..Result.Id.Finish) &
                        "should have one or two parameters, not" &
                           integer'image(count));
                  end if;
               when Lexer.Func_Id2or3 =>
                  if Count < 2 or Count > 3 then
                     Raise_Exception(T,
                        Lexer.Get_Text(Result.Id.Start..Result.Id.Finish) &
                        "should have 2 or 3 parameters, not" &
                           integer'image(count));
                  end if;
               when Lexer.Other_Func_Id2 | Lexer.Graph_Func_Id2=>
                  if Count /= 2 then
                     Raise_Exception(T,
                        Lexer.Get_Text(Result.Id.Start..Result.Id.Finish) &
                        "should have two parameters, not " &
                           integer'image(count));
                  end if;
               when Lexer.Func_Id3 =>
                  if Count /= 3 then
                     Raise_Exception(T,
                        Lexer.Get_Text(Result.Id.Start..Result.Id.Finish) &
                        "should have three parameters, not " &
                           integer'image(count));
                  end if;
            end case;
            T := Get_Token;
            if T.Kind /= Right_Paren then
               Raise_Exception(T,
                  "missing right parenthesis");
            end if;
            return Expon_Pointer(Result);
         end;
      else
         Raise_Exception(T,
            "expected variable or number, found " &
            Token_Type'Image(T.Kind));
         return null; -- shut up compiler
      end if;
   end Parse_Expon;

   -- Mult => Expon ^ Mult | Expon
   function Parse_Mult return Mult_Pointer is
      E : Expon_Pointer;
      T : Token_Pointer;
   begin
      E := Parse_Expon;
      T := Get_Token;
      if T.Kind = Exponent then
         return new Expon_Mult'(
            Left  => Value_Parseable_Pointer(E),
            Right => Parse_Mult);
      else
         Unget_Token (T);
         return new Mult'(Left => Value_Parseable_Pointer(E));
      end if;
   end Parse_Mult;

   -- Add => Mult * Add | Mult / Add | Mult mod Add | Mult
   function Parse_Add return Add_Pointer is
      M : Mult_Pointer;
      T : Token_Pointer;
   begin
      M := Parse_Mult;
      Fix_Associativity(M);
      T := Get_Token;
      if T.Kind = Times then
         return new Mult_Add'(
            Left  => Value_Parseable_Pointer(M),
            Right => Parse_Add);
      elsif T.Kind = Divide then
         return new Div_Add'(
            Left  => Value_Parseable_Pointer(M),
            Right => Parse_Add);
      elsif T.Kind = Mod_T then
         return new Mod_Add'(
            Left  => Value_Parseable_Pointer(M),
            Right => Parse_Add);
      elsif T.Kind = Rem_T then
         return new Rem_Add'(
            Left  => Value_Parseable_Pointer(M),
            Right => Parse_Add);
      else
         Unget_Token (T);
         return new Add'(Left => Value_Parseable_Pointer(M));
      end if;
   end Parse_Add;

   -- Expression => Add + Expression | Add - Expression | Add
   function Parse_Expression return Expression_Pointer is
      A : Add_Pointer;
      T : Token_Pointer;
   begin
      A := Parse_Add;
      Fix_Associativity(A);
      T := Get_Token;
      if T.Kind = Plus then
         return new Add_Expression'(
            Left  => Value_Parseable_Pointer(A),
            Right => Parse_Expression);
      elsif T.Kind = Minus then
         return new Minus_Expression'(
            Left  => Value_Parseable_Pointer(A),
            Right => Parse_Expression);
      else
         Unget_Token (T);
         return new Expression'(Left => Value_Parseable_Pointer(A));
      end if;
   end Parse_Expression;

   -- Relation => Expression > Expression | >=,<,<=,=
   function Parse_Relation return Relation_Pointer is
      Result : Relation_Pointer := new Parse_Tree.Relation;
      T : Token_Pointer;
   begin
      T := Get_Token;
      if T.Kind = Lexer.Wait_For_Key then
         Raise_Exception(T,
            "Did you mean Key_Hit?");
      elsif T.Kind = Lexer.Wait_For_Mouse_Button then
         Raise_Exception(T,
            "Did you mean Mouse_Button_Pressed?");
      elsif T.Kind in Lexer.Proc_Token_Types then
         Raise_Exception(T,
            Lexer.Token_Type'Image(T.Kind) & " should be in call");
      end if;
      Unget_Token(T);
      Result.Left := Parse_Expression;
      Fix_Associativity(Result.Left);
      T := Get_Token;
      if T.Kind = Greater_Equal or
         T.Kind = Greater or
         T.Kind = Less_Equal or
         T.Kind = Equal or
         T.Kind = Not_Equal or
         T.Kind = Less then
         Result.Kind := T.Kind;
      else
         Raise_Exception(T,
            "Expected relational operator (<,>,=,...) not " &
               Lexer.Token_Type'Image(T.Kind));
      end if;
      Result.Right := Parse_Expression;
      Fix_Associativity(Result.Right);
      T := Get_Token;
      if T.Kind = Greater_Equal or
         T.Kind = Greater or
         T.Kind = Less_Equal or
         T.Kind = Equal or
         T.Kind = Not_Equal or
         T.Kind = Less then
         if Result.Kind = Less and T.Kind = Less then
            Raise_Exception(T,
               "X<Y<Z should be X<Y and Y<Z");
         elsif Result.Kind = Less and T.Kind = Less_Equal then
            Raise_Exception(T,
               "X<Y<=Z should be X<Y and Y<=Z");
         elsif Result.Kind = Less_Equal and T.Kind = Less then
            Raise_Exception(T,
               "X<=Y<Z should be X<=Y and Y<Z");
         elsif Result.Kind = Less_Equal and T.Kind = Less_Equal then
            Raise_Exception(T,
               "X<=Y<=Z should be X<=Y and Y<=Z");
         elsif Result.Kind = Greater and T.Kind = Greater then
            Raise_Exception(T,
               "X>Y>Z should be X>Y and Y>Z");
         elsif Result.Kind = Greater and T.Kind = Greater_Equal then
            Raise_Exception(T,
               "X>Y>=Z should be X>Y and Y>=Z");
         elsif Result.Kind = Greater_Equal and T.Kind = Greater then
            Raise_Exception(T,
               "X>=Y>Z should be X>=Y and Y>Z");
         elsif Result.Kind = Greater_Equal and T.Kind = Greater_Equal then
            Raise_Exception(T,
               "X>=Y>=Z should be X>=Y and Y>=Z");
         end if;
      end if;
      Lexer.Unget_Token(T);
      return Result;
   end Parse_Relation;

   -- Boolean2 => [ (BE) | Relation | Boolean_Func] [and BE2 | lambda]
   function Parse_Boolean2 return Boolean2_Pointer is
      Left : Boolean_Parseable_Pointer;
      Negated : Boolean;
      E : Expression_Pointer;
      Kind : Lexer.Token_Type;
      T : Token_Pointer;
   begin
      T := Get_Token;
      if T.Kind = Not_T then
         T := Get_Token;
         Negated := True;
      else
         Negated := False;
      end if;
      if T.Kind = Left_Paren then
         -- ok, so here we break being an LL(1) grammar
         -- in favor of not having a type checker
         -- I first guess that the paren is a boolean expression
         -- (thus not allowing paren_expon to generate a boolean)
         -- but if wrong, try again as just a relation
         begin
            Left := Boolean_Parseable_Pointer(Parse_Boolean_Expression);
            T := Get_Token;
            if T.Kind /= Right_Paren then
               Raise_Exception(T,
                  "Expected right parenthesis");
            end if;
         exception when Syntax_Error =>
            Lexer.Rewind(T);
            Lexer.Unget_Token(T);
            Left := Boolean_Parseable_Pointer(Parse_Relation);
         end;
      elsif T.Kind in Boolean_Func0_Types then
         Left := new Boolean0'(Kind => T.Kind);
      elsif T.Kind = Lexer.True then
         Left := new Boolean_Constant'(Value => True);
      elsif T.Kind = Lexer.False then
         Left := new Boolean_Constant'(Value => False);
      elsif T.Kind in Boolean_Reflection_Types then
         Kind := T.Kind;
         T := Get_Token;
         if T.Kind /= Left_Paren then
            Raise_Exception(T,
               "Expected left parenthesis");
         end if;
         T := Get_Token;
         if T.Kind /= Lexer.Id then
            Raise_Exception(T,
               "Must have variable name here");
         end if;
         Left := new Boolean_Reflection'(Kind => Kind,
            Id => T);
         T := Get_Token;
         if T.Kind /= Right_Paren then
            Raise_Exception(T,
               "Expected right parenthesis");
         end if;
      elsif T.Kind in Boolean_Func1_Types then
         Kind := T.Kind;
         T := Get_Token;
         if T.Kind /= Left_Paren then
            Raise_Exception(T,
               "Expected left parenthesis");
         end if;
         E := Parse_Expression;
         Fix_Associativity(E);
         Left := new Boolean1'(Kind => Kind,
            Parameter => E);
         T := Get_Token;
         if T.Kind /= Right_Paren then
            Raise_Exception(T,
               "Expected right parenthesis");
         end if;
      elsif T.Kind = Lexer.Id then
         if raptor.Plugins.Is_Boolean_Function(
               Name => +Get_Text(T.Start..T.Finish)) then
            declare
               Result : Boolean_Plugin_Pointer :=
                  new Boolean_Plugin;
               Count : Natural;
               Correct_Count : Integer;
            begin
               Result.Id := T;
               T := Get_Token;
               if T.Kind /= Left_Paren then
                  Raise_Exception(T,
                     "Expected left parenthesis");
               end if;
               Result.Parameters := Parse_Parameter_List(
                  Lexer.Get_Text(Result.Id.Start..Result.Id.Finish),False);
               Count := Count_Parameters(Result.Parameters);
               Correct_Count := raptor.Plugins.Parameter_Count(
                     +Lexer.Get_Text(Result.Id.Start..Result.Id.Finish));
               if Count /= Correct_Count then
                  Raise_Exception(T,
                     Lexer.Get_Text(Result.Id.Start..Result.Id.Finish) &
                     " should have" & Integer'Image(Correct_Count) &
                     " parameters.");
               end if;
               T := Get_Token;
               if T.Kind /= Right_Paren then
                  Raise_Exception(T,
                     "missing right parenthesis");
               end if;
               Left := Boolean_Parseable_Pointer(Result);
            end;
         else
            Unget_Token(T);
            Left := Boolean_Parseable_Pointer(Parse_Relation);
         end if;
      else
         Unget_Token(T);
         Left := Boolean_Parseable_Pointer(Parse_Relation);
      end if;
      T := Get_Token;
      if T.Kind = And_T then
         return new And_Boolean2'(
            Negated => Negated,
            Left  => Left,
            Right => Parse_Boolean2);
      else
         Unget_Token(T);
         return new Boolean2'(
            Negated => Negated,
            Left => Left);
      end if;
   end Parse_Boolean2;

   -- Boolean_Expression => Boolean2 or Boolean_Expression |
   --                       Boolean2 xor Boolean_Expression | Boolean2
   function Parse_Boolean_Expression return Boolean_Expression_Pointer is
      Left : Boolean2_Pointer;
      T : Token_Pointer;
   begin
      Left := Parse_Boolean2;
      T := Get_Token;
      if T.Kind = Or_T then
         return new Or_Boolean'(
            Left  => Left,
            Right => Parse_Boolean_Expression);
      elsif T.Kind = Xor_T then
         return new Xor_Boolean'(
            Left  => Left,
            Right => Parse_Boolean_Expression);
      else
         Unget_Token(T);
         return new Boolean_Expression'(Left => Left);
      end if;
   end Parse_Boolean_Expression;

   -- Condition => Boolean_Expression End_Input
   function Parse_Condition return Boolean_Expression_Pointer is
      Result : Boolean_Expression_Pointer;
      T : Token_Pointer;
   begin
      Result := Parse_Boolean_Expression;
      T := Get_Token;
      if T.Kind /= End_Input then
         Raise_Exception(T,
            Token_Type'Image(T.Kind) &
             " is unexpected");
      end if;
      return Result;
   end Parse_Condition;

   -- Output => [Expression | String]
   function Parse_Output(New_Line : in Boolean) return Output_Pointer is
      Result : Output_Pointer;
      E : Expression_Pointer;
      T : Token_Pointer;
   begin
      T := Get_Token;
      -- don't need String_Output anymore, since "" is an Expression
      --if T.Kind = String_T then
      --   Result := new String_Output'(Str => T, New_Line => New_Line);
      --else
         Unget_Token(T);
         E := Parse_Expression;
         Fix_Associativity(E);
         Result := new Expr_Output'(Expr => E, New_Line => New_Line);
      --end if;
      return Result;
   end Parse_Output;

   -- Input => Lhs LSuffix End_Input
   function Parse_Input return Input_Pointer is
      Result : Input_Pointer;
      T : Token_Pointer;
      Id : Token_Pointer;
   begin

      T := Get_Token;
      if T.Kind /= Lexer.Id then
         Raise_Exception(T,
            "Input must begin with variable");
      end if;
      Id := T;
      Result := new Input'(Lhs => Parse_Lhs(T), LSuffix => Parse_LSuffix);
      return Result;
   end Parse_Input;

   -- Parameter_List => Output [, Parameter_List | Lambda]
   function Parse_Parameter_List(
         Subprogram_Name   : in String;
         Allow_String_Args : in Boolean := False) return Parameter_List_Pointer is
      Result : Output_Pointer;
      E : Expression_Pointer;
      T : Token_Pointer;
   begin
      T := Get_Token;
      if T.Kind = Right_Paren then
         Unget_Token(T);
         return null;
      end if;
      Unget_Token(T);
      if not Allow_String_Args then
         E := Parse_Expression;
         Fix_Associativity(E);
         Result := new Expr_Output'(Expr => E, New_Line => False);
      else
         Result := Parse_Output(New_Line => False);
      end if;
      T := Get_Token;
      if T.Kind = Comma then
         return new Parameter_List'(
            Parameter => Result,
            Next      => Parse_Parameter_List(
               Subprogram_Name, Allow_String_Args));
      else
         Unget_Token(T);
         return new Parameter_List'(
            Parameter => Result,
            Next      => null);
      end if;
   end Parse_Parameter_List;

   -- Input_Statement => Input End_Input
   function Parse_Input_Statement return Input_Pointer is
      Result : Input_Pointer := Parse_Input;
      T : Token_Pointer;
   begin
      T := Get_Token;
      if T.Kind /= End_Input then
         Raise_Exception(T,
            Token_Type'Image(T.Kind) &
             " is unexpected");
      end if;
      return Result;
   end Parse_Input_Statement;

   -- Output_Statement => Output End_Input
   function Parse_Output_Statement(New_Line : Boolean) return Output_Pointer is
      Result : Output_Pointer := Parse_Output(New_Line);
      T : Token_Pointer;
   begin
      T := Get_Token;
      if T.Kind /= End_Input then
         Raise_Exception(T,
            Token_Type'Image(T.Kind) &
             " is unexpected");
      end if;
      return Result;
   end Parse_Output_Statement;

   -- Proc_Call => proc_id(Parameter_List)
   function Parse_Proc_Call return Proc_Call_Pointer is
      Result : Proc_Call_Pointer := new Proc_Call;
      T : Token_Pointer;
   begin
      T := Get_Token;
      if not (T.Kind in Proc_Token_Types) then
         Raise_Exception(T,
            Token_Type'Image(T.Kind) &
            " is unexpected in parse_proc_call");
      end if;
      Result.Id := T;
      T := Get_Token;
      if T.Kind = Left_Paren then
         Result.Param_List := Parse_Parameter_List(
            "",True);
         T := Get_Token;
         if T.Kind /= Right_Paren then
            Raise_Exception(T,
               "missing )");
         end if;
      else
         Result.Param_List := null;
         Unget_Token(T);
      end if;
      begin
         Lexer.Verify_Parameter_Count(
            Lexer.Get_Text(Result.Id.Start..Result.Id.Finish),
               Count_Parameters(Result.Param_List));
      exception when E:others =>
            Raise_Exception(
               Result.Id,Ada.Exceptions.Exception_Message(E));
      end;
      return Result;
   end Parse_Proc_Call;

   -- Plugin_Proc_Call => plugin_proc_id(Parameter_List)
   function Parse_Plugin_Proc_Call return Plugin_Proc_Call_Pointer is
      Result : Plugin_Proc_Call_Pointer := new Plugin_Proc_Call;
      Count : Integer;
      T : Token_Pointer;
   begin
      T := Get_Token;
      Result.Id := T;
      T := Get_Token;
      if T.Kind = Left_Paren then
         Result.Param_List := Parse_Parameter_List(
            "",True);
         T := Get_Token;
         if T.Kind /= Right_Paren then
            Raise_Exception(T,
               "missing )");
         end if;
      else
         Result.Param_List := null;
         Unget_Token(T);
      end if;
      Count := raptor.Plugins.Parameter_Count(
            Name => +Lexer.Get_Text(Result.Id.Start..Result.Id.Finish));
      if  Count /= Count_Parameters(Result.Param_List) then
         Raise_Exception(T,
            Lexer.Get_Text(Result.Id.Start..Result.Id.Finish) &
            " needs" & integer'image(Count) & " parameters.");
      end if;

      return Result;
   end Parse_Plugin_Proc_Call;

   -- Tab_Proc_Call => Tab_proc_id(Parameter_List)
   function Parse_Tab_Proc_Call return Tabid_Proc_Call_Pointer is
      Result : Tabid_Proc_Call_Pointer := new Tabid_Proc_Call;
      Count : Integer;
      T : Token_Pointer;
   begin
      T := Get_Token;
      Result.Id := T;
      T := Get_Token;
      if T.Kind = Left_Paren then
         Result.Param_List := Parse_Parameter_List(
            "",True);
         T := Get_Token;
         if T.Kind /= Right_Paren then
            Raise_Exception(T,
               "missing )");
         end if;
      else
         Result.Param_List := null;
         Unget_Token(T);
      end if;
      Count := raptor.Subchart.Parameter_Count(
            S => +Lexer.Get_Text(Result.Id.Start..Result.Id.Finish));
      if  Count /= Count_Parameters(Result.Param_List) then
         Raise_Exception(T,
            Lexer.Get_Text(Result.Id.Start..Result.Id.Finish) &
            " needs" & integer'image(Count) & " parameters.");
      end if;

      return Result;
   end Parse_Tab_Proc_Call;

   -- Statement => (Proc_Call | Assignment) [;] End_Input
   function Parse_Statement(Is_Call_Box : Boolean) return Statement_Pointer is
      Result : Statement_Pointer;
      T : Token_Pointer;
   begin
      T := Get_Token;
      Unget_Token(T);
      if T.Kind in Lexer.Proc_Token_Types then
         Result := Statement_Pointer(Parse_Proc_Call);
      elsif T.Kind in Lexer.Func_Token_Types then
         Raise_Exception(T,
            Lexer.Get_Text(T.Start..T.Finish) &
            " can only appear on right side of assignment");
      elsif T.Kind = Id then
         if raptor.Plugins.Is_Procedure(
               +Lexer.Get_Text(T.Start..T.Finish)) then
            Result := Statement_Pointer(Parse_Plugin_Proc_Call);
         elsif Raptor.Subchart.Is_Subchart_Name(
               +Lexer.Get_Text(T.Start..T.Finish)) and Is_Call_Box then
            Result := Statement_Pointer(Parse_Tab_Proc_Call);
         elsif Is_Call_Box then
            Result := Statement_Pointer(Parse_Call_Statement);
         else
            Result := Statement_Pointer(Parse_Assignment);
         end if;
      else
         Raise_Exception(T,
            Lexer.Get_Text(T.Start..T.Finish) &
            " can not begin a statement");
      end if;
      T := Get_Token;
      if T.Kind = Semicolon then
         T := Get_Token;
      end if;
      if T.Kind /= End_Input then
         Raise_Exception(T,
            Token_Type'Image(T.Kind) &
             " is unexpected");
      end if;
      return Result;
   end Parse_Statement;

   -- Assignment_Statement => Assignment [;] End_Input
   function Parse_Assignment_Statement return Statement_Pointer is
      Result : Statement_Pointer;
      T : Token_Pointer;
   begin
      T := Get_Token;
      Unget_Token(T);
      if T.Kind in Lexer.Proc_Token_Types then
         Raise_Exception(T,
            Lexer.Get_Text(T.Start..T.Finish) &
            " can not be used as a variable name");
      elsif T.Kind in Lexer.Func_Token_Types then
         Raise_Exception(T,
            Lexer.Get_Text(T.Start..T.Finish) &
            " can only appear on right side of assignment");
      elsif T.Kind = Id then
         if raptor.Plugins.Is_Procedure(
               +Lexer.Get_Text(T.Start..T.Finish)) then
            Raise_Exception(T,
               Lexer.Get_Text(T.Start..T.Finish) &
               " can not be used as a variable name");
         else
            Result := Statement_Pointer(Parse_Assignment);
         end if;
      elsif T.Kind = Colon_Equal then
         Raise_Exception(T,
            "Put a variable in the Set box");
      else
         Raise_Exception(T,
            Lexer.Get_Text(T.Start..T.Finish) &
            " can not begin an assignment");
      end if;
      T := Get_Token;
      if T.Kind = Semicolon then
         T := Get_Token;
      end if;
      if T.Kind /= End_Input then
         Raise_Exception(T,
            Token_Type'Image(T.Kind) &
             " is unexpected");
      end if;
      return Result;
   end Parse_Assignment_Statement;

   -- MSuffix => . Lhs MSuffix | . id | .id (Parameter_List)
   function Parse_MSuffix return MSuffix_Pointer is
      T, T2 : Token_Pointer;
      Result : MSuffix_Pointer;
   begin
      T := Get_Token;
      if not Raptor.Runtime.isObjectOriented then
            Raise_Exception(T,"can't use '.' in name");
      end if;

      if T.Kind /= Dot then
         Raise_Exception(T, "expected "".""");
      end if;
      T := Get_Token;
      if T.Kind /= Id then
         Raise_Exception(T, "expected name");
      end if;
      T2 := Get_Token;
      if T2.Kind = Dot or T2.Kind = Left_Bracket then
         Unget_Token(T2);
         return new Full_MSuffix'(Lhs => Parse_Lhs(T), MSuffix => Parse_MSuffix);
      elsif T2.Kind = Left_Paren then
         Result := new Param_MSuffix;
         Param_MSuffix_Pointer(Result).Id := T;
         Param_MSuffix_Pointer(Result).Param_List := Parse_Parameter_List(
            Lexer.Get_Text(T.Start..T.Finish),True);
         T := Get_Token;
         if T.Kind /= Right_Paren then
            Raise_Exception(T,
               "missing right parenthesis");
         end if;
      else
         Unget_Token(T2);
         Result := new Noparam_MSuffix;
         Noparam_Msuffix(Result.all).Id := T;
      end if;
      T := Get_Token;
      if T.Kind = Semicolon then
         T := Get_Token;
      end if;
      if T.Kind /= End_Input then
         Raise_Exception(T,
            Token_Type'Image(T.Kind) &
             " is unexpected");
      end if;
      return Result;
   end Parse_MSuffix;

   function Parse_Method_Call return Method_Proc_Call_Pointer is
      T : Token_Pointer;
      T2 : Token_Pointer;
      Result : Method_Proc_Call_Pointer;
   begin
      T := Get_Token;
      if T.Kind /= Id then
         Raise_Exception(T,
            Token_Type'Image(T.Kind) &
            " is unexpected, expected object name");
      end if;
      if To_Lower(Lexer.Get_Text(T.Start..T.Finish))="this" then
         T2 := Get_Token;
         if T2.Kind = Left_Paren then
            Result := new Method_Proc_Call_This;
         else
            Unget_Token(T2);
         end if;
      end if;
      if To_Lower(Lexer.Get_Text(T.Start..T.Finish))="super" then
         T2 := Get_Token;
         if T2.Kind = Left_Paren then
            Result := NEW Method_Proc_Call_Super;
         else
            Unget_Token(T2);
         end if;
      end if;

      if Result /= null then
         Result.Id := T;
         Result.Param_List := Parse_Parameter_List(
            Lexer.Get_Text(Result.Id.Start..Result.Id.Finish),True);
         Result.MSuffix := new Param_Msuffix'(t,result.param_list);
         T := Get_Token;
         if T.Kind /= Right_Paren then
            Raise_Exception(T,
               "missing right parenthesis");
         end if;
      else
         Result := new Method_Proc_Call;
         Result.Lhs := Parse_Lhs(T);
         Result.MSuffix := Parse_MSuffix;
      end if;
      T := Get_Token;
      if T.Kind = Semicolon then
         T := Get_Token;
      end if;
      if T.Kind /= End_Input then
         Raise_Exception(T,
            Token_Type'Image(T.Kind) &
             " is unexpected");
      end if;
      return Result;
   end Parse_Method_Call;

   -- Call_Statement => Proc_Call [;] End_Input
   function Parse_Call_Statement return Statement_Pointer is
      Result : Statement_Pointer;
      T : Token_Pointer;
   begin
      T := Get_Token;
      Unget_Token(T);
      if T.Kind in Lexer.Proc_Token_Types then
         Result := Statement_Pointer(Parse_Proc_Call);
      elsif T.Kind in Lexer.Func_Token_Types then
         Raise_Exception(T,
            Lexer.Get_Text(T.Start..T.Finish) &
            " should appear in assignment");
      elsif T.Kind in Lexer.Boolean_Func_Types then
         Raise_Exception(T,
            Lexer.Get_Text(T.Start..T.Finish) &
            " should appear in diamond");
      elsif T.Kind = Id then
         if raptor.Plugins.Is_Procedure(
               +Lexer.Get_Text(T.Start..T.Finish)) then
            Result := Statement_Pointer(Parse_Plugin_Proc_Call);
         elsif Raptor.Subchart.Is_Subchart_Name(
               +Lexer.Get_Text(T.Start..T.Finish)) then
            Result := Statement_Pointer(Parse_Tab_Proc_Call);
         elsif Raptor.runtime.isObjectOriented then
            Result := Statement_Pointer(Parse_Method_Call);
         else
            Raise_Exception(T,
               Lexer.Get_Text(T.Start..T.Finish) &
               " is not a valid procedure name");
         end if;
      else
         Raise_Exception(T,
            Lexer.Get_Text(T.Start..T.Finish) &
            " is not a valid procedure name");
      end if;
      T := Get_Token;
      if T.Kind = Semicolon then
         T := Get_Token;
      end if;
      if T.Kind /= End_Input then
         Raise_Exception(T,
            Token_Type'Image(T.Kind) &
             " is unexpected");
      end if;
      return Result;
   end Parse_Call_Statement;

end Parser;

