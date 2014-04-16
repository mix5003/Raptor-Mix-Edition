with Ada.Strings.Unbounded;
use Ada.Strings.Unbounded;
with Ada.Characters.Handling;
use Ada.Characters.Handling;
with Ada.Exceptions;
use Ada.Exceptions;
with Mssyst.String;
use Mssyst.String;

package body Lexer is
   Current_String : Unbounded_String;
   Current_Location : Positive;
   Ungotten_Token : Token_Pointer;

   function Get_Current_Location return Positive is
   begin
      return Current_Location;
   end Get_Current_Location;

   function Id_Kind(
         Lexeme : in String) return Token_Type is
      Answer : Subprogram;
   begin
      Answer := Subprogram'Value(Lexeme);
      return Answer;
   exception
      when others =>
         if To_Lower(Lexeme)="and" then
            return And_T;
         elsif To_Lower(Lexeme)="or" then
            return Or_T;
         elsif To_Lower(Lexeme)="xor" then
            return xor_T;
         elsif To_Lower(Lexeme)="mod" then
            return Mod_T;
         elsif To_Lower(Lexeme)="rem" then
            return Rem_T;
         elsif To_Lower(Lexeme)="not" then
            return Not_T;
         -- mcc: do I want to check for OO mode here?
         elsif To_Lower(Lexeme)="new" then
            return New_T;
         elsif To_Lower(Lexeme)="abs" then
            return Abs_F;
         else
            return Id;
         end if;
   end Id_Kind;

   Parameter_Count : constant Parameter_Count_Array :=
      (Clear_Console => 0,
      Delay_For => 1,
      Draw_Bitmap => 5,
      Redirect_Output_Append => 1,
      Set_Precision => 1,
      Redirect_Input => 1,
      Redirect_Output => 1,
      Clear_Window => 1,
      Close_Graph_Window => 0,
      Display_Number => 4,
      Display_Text => 4,
      Freeze_Graph_Window => 0,
      Unfreeze_Graph_Window => 0,
      Update_Graph_Window => 0,
      Draw_Box => 6,
      Draw_Arc => 9,
      Draw_Circle => 5,
      Draw_Ellipse => 6,
      Draw_Ellipse_Rotate => 7,
      Draw_Line => 5,
      Flood_Fill => 3,
      Get_Mouse_Button => 3,
      Open_Graph_Window => 2,
      Wait_For_Key => 0,
      Wait_For_Mouse_Button => 1,
      Put_Pixel => 3,
      Set_Font_Size => 1,
      Save_Graph_Window => 1,
      Set_Window_Title => 1);

   procedure Verify_Parameter_Count(
         Proc_Name : in String;
         Count     : in Natural) is
      Name : Proc_Token_Types := Proc_Token_Types'Value(Proc_Name);
   begin
      if Count /= Parameter_Count(Name) then
         declare
            message : string := Proc_Name & " should have" &
               Integer'Image(Parameter_Count(Name)) &
               " parameters.";
         begin
            Raise_Exception(Bad_Count'Identity,
               Message);
         end;
      end if;
   end Verify_Parameter_Count;
   ---------------
   -- Get_Token --
   ---------------

   function Get_Token return Token_Pointer is
      Current_Char : Character := ' ';
      Start_Location : Positive;
      Has_Decimal : Boolean := False;
      Has_Digit_After_Decimal : Boolean := False;
   begin
      if Ungotten_Token /= null then
         declare
            Result : Token_Pointer := Ungotten_Token;
         begin
            Ungotten_Token := null;
            return Result;
         end;
      end if;
      -- find the first non-blank character, returning End_Input
      -- if one is not found.
      while Is_Control(Current_Char) or Current_Char = ' ' loop
         if Current_Location > Length(Current_String) then
            return new Token'(
               Kind => End_Input,
               Start => Current_Location,
               Finish => Current_Location);
         end if;
         Current_Char := Element(
            Source => Current_String,
            Index  => Current_Location);
         Current_Location := Current_Location + 1;
      end loop;

      Start_Location := Current_Location - 1;
      case Current_Char is
         when ''' =>
            if Current_Location+1 <= Length(Current_String) and then
                  Element(Current_String,Current_Location+1) = ''' then
               Current_Location := Current_Location + 2;
               return new Token'(
                  Kind   => Character_T,
                  Start  => Current_Location-3,
                  Finish => Current_Location-1);
            else
               Raise_Exception(Bad_Token'Identity,
                  "Bad character, expected something like 'x'");
            end if;
         when '"' =>
            loop
               exit when Current_Location > Length (Current_String);
               Current_Char := Element(Current_String,Current_Location);
               if Current_Char = '"' then
                  exit when (Current_Location = Length (Current_String) or else
                     Element(Current_String,Current_Location+1) /= '"');
                  -- skip two on ""
                  Current_Location := Current_Location + 1;
               elsif Current_Char = '\' then
                  if Current_Location < Length(Current_String) and then
                        Element(Current_String,Current_Location+1) = '"' then
                     -- skip two on \"
                     Current_Location := Current_Location + 1;
                  end if;
               end if;
               Current_Location := Current_Location + 1;
            end loop;
            if Current_Location > Length (Current_String) then
               Raise_Exception(Bad_Token'Identity,"missing closing "" for string");
            end if;
            Current_Location := Current_Location + 1;
            return new Token'(
               Kind   => String_T,
               Start  => Start_Location,
               Finish => Current_Location-1);
         when '[' =>
            return new Token'(
               Kind   => Left_Bracket,
               Start  => Current_Location-1,
               Finish => Current_Location-1);
         when ']' =>
            return new Token'(
               Kind   => Right_Bracket,
               Start  => Current_Location-1,
               Finish => Current_Location-1);
         when ';' =>
            return new Token'(
               Kind   => Semicolon,
               Start  => Current_Location-1,
               Finish => Current_Location-1);
         when ',' =>
            return new Token'(
               Kind   => Comma,
               Start  => Current_Location-1,
               Finish => Current_Location-1);
         when '+' =>
            return new Token'(
               Kind   => Plus,
               Start  => Current_Location-1,
               Finish => Current_Location-1);
         when '-' =>
            return new Token'(
               Kind   => Minus,
               Start  => Current_Location-1,
               Finish => Current_Location-1);
         when '!' =>
            if Current_Location <= Length(Current_String) and then
                  Element(Current_String,Current_Location) = '=' then
               Current_Location := Current_Location + 1;
               return new Token'(
                  Kind   => Not_Equal,
                  Start  => Current_Location-2,
                  Finish => Current_Location-1);
            else
               Raise_Exception(Bad_Token'Identity,
                  "expected !=, found !");
            end if;
         when '/' =>
            if Current_Location <= Length(Current_String) and then
                  Element(Current_String,Current_Location) = '=' then
               Current_Location := Current_Location + 1;
               return new Token'(
                  Kind   => Not_Equal,
                  Start  => Current_Location-2,
                  Finish => Current_Location-1);
            else
               return new Token'(
                  Kind   => Divide,
                  Start  => Current_Location-1,
                  Finish => Current_Location-1);
            end if;
         when '*' =>
            if Current_Location <= Length(Current_String) and then
                  Element(Current_String,Current_Location) = '*' then
               Current_Location := Current_Location + 1;
               return new Token'(
                  Kind   => Exponent,
                  Start  => Current_Location-2,
                  Finish => Current_Location-1);
            else
               return new Token'(
                  Kind   => Times,
                  Start  => Current_Location-1,
                  Finish => Current_Location-1);
            end if;
         when '^' =>
            return new Token'(
               Kind   => Exponent,
               Start  => Current_Location-1,
               Finish => Current_Location-1);
         when '%' =>
            return new Token'(
               Kind   => Mod_T,
               Start  => Current_Location-1,
               Finish => Current_Location-1);
         when '&' =>
            if Current_Location <= Length(Current_String) and then
                  Element(Current_String,Current_Location) = '&' then
               Current_Location := Current_Location + 1;
               return new Token'(
                  Kind   => And_T,
                  Start  => Current_Location-2,
                  Finish => Current_Location-1);
            else
               Raise_Exception(Bad_Token'Identity,
                  "expected &&, found &");
            end if;
         when '|' =>
            if Current_Location <= Length(Current_String) and then
                  Element(Current_String,Current_Location) = '|' then
               Current_Location := Current_Location + 1;
               return new Token'(
                  Kind   => Or_T,
                  Start  => Current_Location-2,
                  Finish => Current_Location-1);
            else
               Raise_Exception(Bad_Token'Identity,
                  "expected ||, found |");
            end if;
         when ':' =>
            if Current_Location <= Length(Current_String) and then
                  Element(Current_String,Current_Location) = '=' then
               Current_Location := Current_Location + 1;
               return new Token'(
                  Kind   => Colon_Equal,
                  Start  => Current_Location-2,
                  Finish => Current_Location-1);
            else
               Raise_Exception(Bad_Token'Identity,
                  ": is not allowed");
            end if;
         when '>' =>
            if Current_Location <= Length(Current_String) and then
                  Element(Current_String,Current_Location) = '=' then
               Current_Location := Current_Location + 1;
               return new Token'(
                  Kind   => Greater_Equal,
                  Start  => Current_Location-2,
                  Finish => Current_Location-1);
            else
               return new Token'(
                  Kind   => Greater,
                  Start  => Current_Location-1,
                  Finish => Current_Location-1);
            end if;
         when '<' =>
            if Current_Location <= Length(Current_String) and then
                  Element(Current_String,Current_Location) = '=' then
               Current_Location := Current_Location + 1;
               return new Token'(
                  Kind   => Less_Equal,
                  Start  => Current_Location-2,
                  Finish => Current_Location-1);
            else
               return new Token'(
                  Kind   => Less,
                  Start  => Current_Location-1,
                  Finish => Current_Location-1);
            end if;
         when '(' =>
            return new Token'(
               Kind   => Left_Paren,
               Start  => Current_Location-1,
               Finish => Current_Location-1);
         when ')' =>
            return new Token'(
               Kind   => Right_Paren,
               Start  => Current_Location-1,
               Finish => Current_Location-1);
         when '=' =>
            if Current_Location <= Length(Current_String) and then
                  Element(Current_String,Current_Location) = '=' then
               Current_Location := Current_Location + 1;
               return new Token'(
                  Kind   => Equal,
                  Start  => Current_Location-2,
                  Finish => Current_Location-1);
            else
               return new Token'(
                  Kind   => Equal,
                  Start  => Current_Location-1,
                  Finish => Current_Location-1);
            end if;
         when 'a'..'z'|'A'..'Z' =>
            loop
               exit when Current_Location > Length (Current_String);
               Current_Char := Element(Current_String,Current_Location);
               exit when not Is_Alphanumeric(Current_Char) and
                  Current_Char /= '_';
               Current_Location := Current_Location + 1;
            end loop;
            return new Token'(
                  Kind => Id_Kind(Slice(
                     Current_String,
                     Start_Location,
                     Current_Location-1)),
                  Start  => Start_Location,
                  Finish => Current_Location-1);
         when '0'..'9'|'.' =>
            if Current_Char = '.' then
               Has_Decimal := True;
            end if;
            loop
               exit when Current_Location > Length(Current_String);
               Current_Char := Element(Current_String,Current_Location);
               if Current_Char = '.' then
                  if Has_Decimal then
                     Raise_Exception(Bad_Token'Identity,
                        "number has two decimal points");
                  else
                     Has_Decimal := True;
                  end if;
               else
                  exit when not Is_Digit (Current_Char);
                  if Has_Decimal then
                     Has_Digit_After_Decimal := True;
                  end if;
               end if;
               Current_Location := Current_Location + 1;
            end loop;
            if Has_Decimal and not Has_Digit_After_Decimal then
               if Current_Location=Start_Location+1 then
                  -- mcc: do I want to check for OO mode here?
                  return new Token'(
                     Kind   => Dot,
                     Start  => Current_Location-1,
                     Finish => Current_Location-1);
               else
                  Raise_Exception(Bad_Token'Identity,
                     "number may not end with decimal point");
               end if;
            end if;
            return new Token'(
               Kind   => Number,
               Start  => Start_Location,
               Finish => Current_Location-1);
         when '_' =>
            Raise_Exception(Bad_Token'Identity,
               "identifier can not begin with '_'");
         when others =>
            Raise_Exception(Bad_Token'Identity,
               "found invalid character: '" &
               Current_Char & "'");
      end case;

   end Get_Token;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize (Text : in String) is
   begin
      Current_String := To_Unbounded_String (Text);
      Current_Location := 1;
      Ungotten_Token := null;
   end Initialize;

   function Get_Text return String is
   begin
      return To_String(Current_String);
   end Get_Text;

   procedure Rewind (P : in Token_Pointer) is
   begin
      Current_Location := P.Finish + 1;
      Ungotten_Token := null;
   end Rewind;

   -----------------
   -- Unget_Token --
   -----------------

   procedure Unget_Token (P : in Token_Pointer) is
      Twice_Ungotten : exception;
   begin
      if Ungotten_Token /= null then
         raise Twice_Ungotten;
      else
         Ungotten_Token := P;
      end if;
   end Unget_Token;

   function Get_Image(Token : in Token_Type) return MSSyst.String.Ref is
      x : string := to_lower(token_type'image(token));
   begin
      if Token=Get_Mouse_Button then
         return +"wait_for_mouse_button";
      else
         return +X;
      end if;
   end Get_Image;
   function Has_Parameters(Token : in Token_Type) return Boolean is
   begin
      return not(Token in Boolean_Func0_Types or
         Token in Func_Id0 or
         Token in Graph_Proc0 or
         Token in Other_Proc0);
   end Has_Parameters;

   function Is_Procedure(Token : in Token_Type) return Boolean is
   begin
      return Token in Proc_Token_Types;
   end Is_Procedure;

end Lexer;

