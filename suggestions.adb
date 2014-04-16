with Ada.Characters.Handling;
use Ada.Characters.Handling;
with Ada.Strings.Fixed;
use Ada.Strings.Fixed;
with Lexer;
use Lexer;
-- for debugging
--with Ada.Text_IO;
--use Ada.Text_Io;
--with MSSyst.Console;
with Raptor.Prefix_Results;
use type Raptor.Prefix_Results.Ref;
with Raptor.Bold_Results;
with Raptor.Plugins;
--with Mssyst.Diagnostics.Trace;

package body Suggestions is

   type Suggestion_Array is array (Lexer.Subprogram) of MSSyst.String.Ref;
   Suggestions : Suggestion_Array;
   type Suggest_Array is array (Lexer.Subprogram) of Boolean;

   function Is_Prefix (
         Left,
         Right : in     String)
     return Boolean is
   begin
      if Right'Length >= Left'Length and then
            Right(Right'First..Right'First+Left'Length-1) =
            Left then
         return True;
      else
         return False;
      end if;

   end Is_Prefix;

   function Count (
         X : in     Suggest_Array)
     return Natural is
      Counter : Natural := 0;
   begin
      for I in X'range loop
         Counter := Counter + Boolean'Pos(X(I));
      end loop;
      return Counter;
   end Count;

   procedure Get_Bold_Locations (
         Subprogram  : in     Lexer.Subprogram;
         Comma_Count : in     Natural;
         Bold_Start  :    out Natural;
         Bold_Finish :    out Natural) is
   begin
      -- default values
      Bold_Start := 0;
      Bold_Finish := 0;
      case Subprogram is
         when Sin..No =>
            null;
         when Is_Character =>
            if Comma_Count = 0 then
               Bold_Start := 14;
               Bold_Finish := 21;
            end if;
         when Is_Number =>
            if Comma_Count = 0 then
               Bold_Start := 11;
               Bold_Finish := 18;
            end if;
         when Is_String =>
            if Comma_Count = 0 then
               Bold_Start := 11;
               Bold_Finish := 18;
            end if;
         when Is_Array =>
            if Comma_Count = 0 then
               Bold_Start := 10;
               Bold_Finish := 17;
            end if;
         when Is_2D_Array =>
            if Comma_Count = 0 then
               Bold_Start := 13;
               Bold_Finish := 20;
            end if;
         when Set_Font_Size =>
            if Comma_Count = 0 then
               Bold_Start := 15;
               Bold_Finish := 18;
            end if;
         when Delay_For =>
            if Comma_Count = 0 then
               Bold_Start := 11;
               Bold_Finish := 17;
            end if;
         when Set_Precision =>
            if Comma_Count = 0 then
               Bold_Start := 15;
               Bold_Finish := 23;
            end if;
         when Redirect_Input =>
            if Comma_Count = 0 then
               Bold_Start := 16;
               Bold_Finish := 35;
            end if;
         when Redirect_Output =>
            if Comma_Count = 0 then
               Bold_Start := 17;
               Bold_Finish := 36;
            end if;
         when Redirect_Output_Append =>
            if Comma_Count = 0 then
               Bold_Start := 24;
               Bold_Finish := 43;
            end if;
         when Closest_Color =>
            if Comma_Count = 0 then
               Bold_Start := 15;
               Bold_Finish := 17;
            elsif Comma_Count = 1 then
               Bold_Start := 19;
               Bold_Finish := 23;
            elsif Comma_Count = 2 then
               Bold_Start := 25;
               Bold_Finish := 28;
            end if;
         when Random_Color|Random_Extended_Color|Right_Button|Left_Button|
               Get_Max_Width|Get_Max_Height|Get_Mouse_X|Get_Mouse_Y|Get_Font_Width|
               Get_Window_Width|Get_Window_Height|
               Get_Font_Height|Get_Key|Get_Key_String|Close_Graph_Window|Wait_For_Key|
               Freeze_Graph_Window|Unfreeze_Graph_Window|Update_Graph_Window|
               Is_Open|Key_Hit|Clear_Console|End_Of_Input =>
            null;
         when Mouse_Button_Pressed =>
            if Comma_Count = 0 then
               Bold_Start := 22;
               Bold_Finish := 33;
            end if;
         when Key_Down =>
            if Comma_Count = 0 then
               Bold_Start := 10;
               Bold_Finish := 14;
            end if;
         when Mouse_Button_Down =>
            if Comma_Count = 0 then
               Bold_Start := 19;
               Bold_Finish := 30;
            end if;
         when Mouse_Button_Released =>
            if Comma_Count = 0 then
               Bold_Start := 23;
               Bold_Finish := 34;
            end if;
         when Get_Mouse_Button =>
            if Comma_Count = 0 then
               Bold_Start := 18;
               Bold_Finish := 29;
            elsif Comma_Count = 1 then
               Bold_Start := 31;
               Bold_Finish := 31;
            elsif Comma_Count = 2 then
               Bold_Start := 33;
               Bold_Finish := 33;
            end if;
         when Get_Pixel =>
            if Comma_Count = 0 then
               Bold_Start := 11;
               Bold_Finish := 11;
            elsif Comma_Count = 1 then
               Bold_Start := 13;
               Bold_Finish := 13;
            end if;
         when Clear_Window =>
            if Comma_Count = 0 then
               Bold_Start := 14;
               Bold_Finish := 18;
            end if;
         when Load_Bitmap =>
            if Comma_Count = 0 then
               Bold_Start := 13;
               Bold_Finish := 20;
            end if;
         when Draw_Bitmap =>
            if Comma_Count = 0 then
               Bold_Start := 13;
               Bold_Finish := 18;
            elsif Comma_Count = 1 then
               Bold_Start := 20;
               Bold_Finish := 20;
            elsif Comma_Count = 2 then
               Bold_Start := 22;
               Bold_Finish := 22;
            elsif Comma_Count = 3 then
               Bold_Start := 24;
               Bold_Finish := 28;
            elsif Comma_Count = 4 then
               Bold_Start := 30;
               Bold_Finish := 35;
            end if;
         when Display_Text =>
            if Comma_Count = 0 then
               Bold_Start := 14;
               Bold_Finish := 14;
            elsif Comma_Count = 1 then
               Bold_Start := 16;
               Bold_Finish := 16;
            elsif Comma_Count = 2 then
               Bold_Start := 18;
               Bold_Finish := 23;
            elsif Comma_Count = 3 then
               Bold_Start := 25;
               Bold_Finish := 29;
            end if;
         when Display_Number =>
            if Comma_Count = 0 then
               Bold_Start := 16;
               Bold_Finish := 16;
            elsif Comma_Count = 1 then
               Bold_Start := 18;
               Bold_Finish := 18;
            elsif Comma_Count = 2 then
               Bold_Start := 20;
               Bold_Finish := 25;
            elsif Comma_Count = 3 then
               Bold_Start := 27;
               Bold_Finish := 31;
            end if;
         when Draw_Arc =>
            if Comma_Count = 0 then
               Bold_Start := 10;
               Bold_Finish := 11;
            elsif Comma_Count = 1 then
               Bold_Start := 13;
               Bold_Finish := 14;
            elsif Comma_Count = 2 then
               Bold_Start := 16;
               Bold_Finish := 17;
            elsif Comma_Count = 3 then
               Bold_Start := 19;
               Bold_Finish := 20;
            elsif Comma_Count = 4 then
               Bold_Start := 22;
               Bold_Finish := 27;
            elsif Comma_Count = 5 then
               Bold_Start := 29;
               Bold_Finish := 34;
            elsif Comma_Count = 6 then
               Bold_Start := 36;
               Bold_Finish := 39;
            elsif Comma_Count = 7 then
               Bold_Start := 40;
               Bold_Finish := 43;
            elsif Comma_Count = 8 then
               Bold_Start := 45;
               Bold_Finish := 49;
            end if;
         when Draw_Box =>
            if Comma_Count = 0 then
               Bold_Start := 10;
               Bold_Finish := 11;
            elsif Comma_Count = 1 then
               Bold_Start := 13;
               Bold_Finish := 14;
            elsif Comma_Count = 2 then
               Bold_Start := 16;
               Bold_Finish := 17;
            elsif Comma_Count = 3 then
               Bold_Start := 19;
               Bold_Finish := 20;
            elsif Comma_Count = 4 then
               Bold_Start := 22;
               Bold_Finish := 26;
            elsif Comma_Count = 5 then
               Bold_Start := 28;
               Bold_Finish := 33;
            end if;
         when Draw_Circle =>
            if Comma_Count = 0 then
               Bold_Start := 13;
               Bold_Finish := 13;
            elsif Comma_Count = 1 then
               Bold_Start := 15;
               Bold_Finish := 15;
            elsif Comma_Count = 2 then
               Bold_Start := 17;
               Bold_Finish := 22;
            elsif Comma_Count = 3 then
               Bold_Start := 24;
               Bold_Finish := 28;
            elsif Comma_Count = 4 then
               Bold_Start := 30;
               Bold_Finish := 35;
            end if;
         when Draw_Ellipse =>
            if Comma_Count = 0 then
               Bold_Start := 14;
               Bold_Finish := 15;
            elsif Comma_Count = 1 then
               Bold_Start := 17;
               Bold_Finish := 18;
            elsif Comma_Count = 2 then
               Bold_Start := 20;
               Bold_Finish := 21;
            elsif Comma_Count = 3 then
               Bold_Start := 23;
               Bold_Finish := 24;
            elsif Comma_Count = 4 then
               Bold_Start := 26;
               Bold_Finish := 30;
            elsif Comma_Count = 5 then
               Bold_Start := 32;
               Bold_Finish := 37;
            end if;
         when Draw_Ellipse_Rotate =>
            if Comma_Count = 0 then
               Bold_Start := 21;
               Bold_Finish := 22;
            elsif Comma_Count = 1 then
               Bold_Start := 24;
               Bold_Finish := 25;
            elsif Comma_Count = 2 then
               Bold_Start := 27;
               Bold_Finish := 28;
            elsif Comma_Count = 3 then
               Bold_Start := 30;
               Bold_Finish := 31;
            elsif Comma_Count = 4 then
               Bold_Start := 33;
               Bold_Finish := 37;
            elsif Comma_Count = 5 then
               Bold_Start := 39;
               Bold_Finish := 43;
            elsif Comma_Count = 6 then
               Bold_Start := 45;
               Bold_Finish := 50;
            end if;
         when Draw_Line =>
            if Comma_Count = 0 then
               Bold_Start := 11;
               Bold_Finish := 12;
            elsif Comma_Count = 1 then
               Bold_Start := 14;
               Bold_Finish := 15;
            elsif Comma_Count = 2 then
               Bold_Start := 17;
               Bold_Finish := 18;
            elsif Comma_Count = 3 then
               Bold_Start := 20;
               Bold_Finish := 21;
            elsif Comma_Count = 4 then
               Bold_Start := 23;
               Bold_Finish := 27;
            end if;
         when Flood_Fill =>
            if Comma_Count = 0 then
               Bold_Start := 12;
               Bold_Finish := 12;
            elsif Comma_Count = 1 then
               Bold_Start := 14;
               Bold_Finish := 14;
            elsif Comma_Count = 2 then
               Bold_Start := 16;
               Bold_Finish := 20;
            end if;
         when Open_Graph_Window =>
            if Comma_Count = 0 then
               Bold_Start := 19;
               Bold_Finish := 23;
            elsif Comma_Count = 1 then
               Bold_Start := 25;
               Bold_Finish := 30;
            end if;
         when Wait_For_Mouse_Button =>
            if Comma_Count = 0 then
               Bold_Start := 23;
               Bold_Finish := 34;
            end if;
         when Put_Pixel =>
            if Comma_Count = 0 then
               Bold_Start := 11;
               Bold_Finish := 11;
            elsif Comma_Count = 1 then
               Bold_Start := 13;
               Bold_Finish := 13;
            elsif Comma_Count = 2 then
               Bold_Start := 15;
               Bold_Finish := 19;
            end if;
         when Set_Window_Title =>
            if Comma_Count = 0 then
               Bold_Start := 18;
               Bold_Finish := 24;
            end if;
         when Save_Graph_Window =>
            if Comma_Count = 0 then
               Bold_Start := 19;
               Bold_Finish := 26;
            end if;
      end case;
   end Get_Bold_Locations;


   function Make_Strings (
         Size : in     Integer)
     return MSSyst.String.Ref_Array;
   pragma Import(MSIL, Make_Strings,
      "[raptor]raptor.Constructors.Make_Strings");

   function Suggestion (
      Text : in     MSSyst.String.Ref;
      Kind : in Scenario)
     return Suggestion_Result is
      Ada_Text           : String                  := To_Upper (+ Text);
      My_Suggestions     : MSSyst.String.Ref_Array;
      To_Suggest         : Suggest_Array           := (others => False);
      Id_Start           : Natural                 := Ada_Text'Last + 1;
      Method_Start       : Natural;
      Number_Suggestions : Natural                 := 0;
      Comma_Count        : Natural                 := 0;
      Paren_Count        : Integer                 := 0;
      Bracket_Count      : Integer                 := 0;
      Quote_Count        : Natural                 := 0;
      Location           : Natural;
      Bold_Start         : Natural                 := 1;
      Bold_Finish        : Natural                 := 0;
      Plugin_Prefixes    :
      access Raptor.Prefix_Results.Typ;
      Bold_Results :
      access Raptor.Bold_Results.Typ;
      Counter : Natural;
      Bold_Suggestion : MSSyst.String.Ref;
   BEGIN
      -- find the start of an identifier at the end of the string
      while Id_Start > Ada_Text'First and then
            (Ada_Text(Id_Start-1)='_' or
               Ada_Text(Id_Start-1)='.' or
               Is_Letter(Ada_Text(Id_Start-1))) loop
         Id_Start := Id_Start - 1;
      end loop;


      -- Look for inside a parameter list
      Location := Ada_Text'Last;
      for I in Ada_Text'First..Location loop
         if Ada_Text(I)='"' then
            Quote_Count := (Quote_Count + 1) mod 2;
         end if;
      end loop;

      -- walk backward through the string to find first unmatched
      -- opening parenthesis.  Count commas only when not inside
      -- "", [] or ()
      loop
         exit when Location < Ada_Text'First;
         case Ada_Text(Location) is
            when ',' =>
               if Paren_Count = 0 and Quote_Count = 0 and
                     Bracket_Count = 0 then
                  Comma_Count := Comma_Count + 1;
               end if;
            when '[' =>
               if Quote_Count = 0 then
                  Bracket_Count := Bracket_Count + 1;
               end if;
            when ']' =>
               if Quote_Count = 0 then
                  Bracket_Count := Bracket_Count - 1;
               end if;
            when ')' =>
               if Quote_Count = 0 then
                  Paren_Count := Paren_Count + 1;
               end if;
            when '(' =>
               if Quote_Count = 0 then
                  Paren_Count := Paren_Count - 1;
               end if;
            when '"' =>
               Quote_Count := (Quote_Count + 1) mod 2;
            when others =>
               null;
         end case;
         Location := Location - 1;
         exit when Paren_Count = -1;
      end loop;

      if Location >= Ada_Text'First then
         -- ok, so if we've found something then try to find out what one it is
         if Paren_Count = -1 then
            Method_Start := Location;
            -- find the start of an identifier at the end of the string
            while Method_Start > Ada_Text'First and then
                  (Ada_Text(Method_Start-1)='_' or
                  Is_Letter(Ada_Text(Method_Start-1))) loop
               Method_Start := Method_Start - 1;
            end loop;
            if (Location >= Method_Start and Location <= Ada_Text'Last) then
               Bold_Results := Raptor.Plugins.Get_Bold_Locations(
                  +Ada_Text(Method_Start..Location),
                  Comma_Count);
               if Bold_Results.Found then
                  Number_Suggestions := Number_Suggestions + 1;
                  Bold_Suggestion := MSSyst.String.Ref(Bold_Results.Suggestion);
                  Bold_Start := Bold_Results.Bold_Start;
                  Bold_Finish := Bold_Results.Bold_Finish;
               else
                  for I in Suggest_Array'range loop
                     if Ada_Text(Method_Start..Location)=
                           Lexer.Subprogram'Image(I) then
                        Number_Suggestions := Number_Suggestions + 1;
                        Bold_Suggestion := Suggestions(I);
                        Get_Bold_Locations(
                           Subprogram  => I,
                           Comma_Count => Comma_Count,
                           Bold_Start  => Bold_Start,
                           Bold_Finish => Bold_Finish);
                     end if;
                  end loop;
               end if;
            end if;
         end if;
      end if;  -- Location > Ada_Text'First


      -- check through all of the subprograms to see if we have a
      -- prefix of one of them
      --if Index(Ada_Text(Id_Start..Ada_Text'Last),"_")>=Id_Start then
      if Ada_Text'Last>=Id_Start then
         if Bold_Suggestion /= null or Kind=Expr_Dialog then
            for I in Lexer.Func_Token_Types'range loop
               if Is_Prefix(Ada_Text(Id_Start..Ada_Text'Last),
                     Lexer.Subprogram'Image(I)) then
                  To_Suggest(I) := True;
               end if;
            end loop;
            for I in Lexer.Boolean_Func_Types'range loop
               if Is_Prefix(Ada_Text(Id_Start..Ada_Text'Last),
                     Lexer.Subprogram'Image(I)) then
                  To_Suggest(I) := True;
               end if;
            end loop;
         elsif Kind=Call_Dialog then
            for I in Lexer.Proc_Token_Types'range loop
               if Is_Prefix(Ada_Text(Id_Start..Ada_Text'Last),
                     Lexer.Subprogram'Image(I)) then
                  To_Suggest(I) := True;
               end if;
            end loop;
         end if;
         Number_Suggestions := Number_Suggestions + Count(To_Suggest);
      end if;
      if Ada_Text'Last-Id_Start >= 0 then
         Plugin_Prefixes := Raptor.Plugins.Prefix_Suggestions(
            +Ada_Text(Id_Start..Ada_Text'Last),
            Kind,
            False);
         Number_Suggestions := Number_Suggestions + Plugin_Prefixes.Count;
      end if;

      My_Suggestions := Make_Strings(Number_Suggestions);

      Counter := 0;

      if Bold_Suggestion/=null then
         My_Suggestions(Counter) := Bold_Suggestion;
         Counter := Counter + 1;
      end if;

      --Put_Line("suggestions:" & Integer'Image(Number_Suggestions));
      if Plugin_Prefixes /= null then
         for J in 1..Plugin_Prefixes.Count loop
            My_Suggestions(Counter) := Plugin_Prefixes.Suggestions(J-1);
            Counter := Counter + 1;
         end loop;
      end if;
      for I in To_Suggest'range loop
         if To_Suggest(I) then
            My_Suggestions(Counter) := Suggestions(I);
            Counter := Counter + 1;
         end if;
      end loop;
      -- suggest RANDOM on prefix
      --      if Ada_Text(Id_Start..Ada_Text'Last)="RAND" or
      --            Ada_Text(Id_Start..Ada_Text'Last)="RANDO" or
      --            Ada_Text(Id_Start..Ada_Text'Last)="RANDOM" then
      --         My_Suggestions(Number_Suggestions-3) := mssyst.string.ref(+"Random");
      --         My_Suggestions(Number_Suggestions-2) := Suggestions(Random_Color);
      --         My_Suggestions(Number_Suggestions-1) := Suggestions(Random_Extended_Color);
      --      end if;
      return Suggestion_Result'(
         Bold_Start  => Bold_Start,
         Bold_Finish => Bold_Finish,
         Suggestions => My_Suggestions);
   end Suggestion;
begin
   Suggestions :=
      (
      Sin                   => Mssyst.String.Ref (+ "sin(x)"),
      Cos                   => Mssyst.String.Ref (+ "cos(x)"),
      Tan                   => Mssyst.String.Ref (+ "tan(x)"),
      Cot                   => Mssyst.String.Ref (+ "cot(x)"),
      Arcsin                => Mssyst.String.Ref (+ "arcsin(x)"),
      Arccos                => Mssyst.String.Ref (+ "arccos(x)"),
      Log                   => Mssyst.String.Ref (+ "log(x)"),
      Arctan                => Mssyst.String.Ref (+ "arctan(y,x)"),
      Arccot                => Mssyst.String.Ref (+ "arccot(y,x)"),
      Min                   => Mssyst.String.Ref (+ "min(x,y)"),
      Max                   => Mssyst.String.Ref (+ "max(x,y)"),
      Sinh                  => Mssyst.String.Ref (+ "sinh(x)"),
      Tanh                  => Mssyst.String.Ref (+ "tanh(x)"),
      Cosh                  => Mssyst.String.Ref (+ "cosh(x)"),
      Arccosh               => Mssyst.String.Ref (+ "arccosh(x)"),
      Arcsinh               => Mssyst.String.Ref (+ "arcsinh(x)"),
      Arctanh               => Mssyst.String.Ref (+ "arctanh(x)"),
      Coth                  => Mssyst.String.Ref (+ "coth(x)"),
      Arccoth               => Mssyst.String.Ref (+ "arccoth(x)"),
      Sqrt                  => Mssyst.String.Ref (+ "sqrt(x)"),
      Floor                 => Mssyst.String.Ref (+ "floor(x)"),
      Ceiling               => Mssyst.String.Ref (+ "ceiling(x)"),
      To_Ascii              => Mssyst.String.Ref (+ "to_ascii(character)"),
      To_Character          => Mssyst.String.Ref (+ "to_character(ascii)"),
      Length_Of             => Mssyst.String.Ref (+ "length_of(array)"),
      Abs_F                 => Mssyst.String.Ref (+ "abs(x)"),
      E                     => Mssyst.String.Ref (+ "e"),
      Pi                    => Mssyst.String.Ref (+ "Pi"),
      Random                => Mssyst.String.Ref (+ "Random"),
      Black                 => Mssyst.String.Ref (+ "Black"),
      Blue                  => Mssyst.String.Ref (+ "Blue"),
      Green                 => Mssyst.String.Ref (+ "Green"),
      Cyan                  => Mssyst.String.Ref (+ "Cyan"),
      Red                   => Mssyst.String.Ref (+ "Red"),
      Magenta               => Mssyst.String.Ref (+ "Magenta"),
      Brown                 => Mssyst.String.Ref (+ "Brown"),
      Light_Gray            => Mssyst.String.Ref (+ "Light_Gray"),
      Dark_Gray             => Mssyst.String.Ref (+ "Dark_Gray"),
      Light_Blue            => Mssyst.String.Ref (+ "Light_Blue"),
      Light_Green           => Mssyst.String.Ref (+ "Light_Green"),
      Light_Cyan            => Mssyst.String.Ref (+ "Light_Cyan"),
      Light_Red             => Mssyst.String.Ref (+ "Light_Red"),
      Light_Magenta         => Mssyst.String.Ref (+ "Light_Magenta"),
      Yellow                => Mssyst.String.Ref (+ "Yellow"),
      White                 => Mssyst.String.Ref (+ "White"),
      Filled                => Mssyst.String.Ref (+ "Filled"),
      Unfilled              => Mssyst.String.Ref (+ "Unfilled"),
      True                  => Mssyst.String.Ref (+ "True"),
      False                 => Mssyst.String.Ref (+ "False"),
      Yes                   => Mssyst.String.Ref (+ "Yes"),
      No                    => Mssyst.String.Ref (+ "No"),
      Random_Color          => Mssyst.String.Ref (+ "Random_Color"),
      Random_Extended_Color => Mssyst.String.Ref (+ "Random_Extended_Color"),
      Left_Button           => Mssyst.String.Ref (+ "Left_Button"),
      Right_Button          => Mssyst.String.Ref (+ "Right_Button"),
      Closest_Color         => Mssyst.String.Ref (+ "Closest_Color(red,green,blue)"),
      Redirect_Input        => Mssyst.String.Ref (+ "Redirect_Input(yes/no or ""filename"")"),
      Redirect_Output       => Mssyst.String.Ref (+ "Redirect_Output(yes/no or ""filename"")"),
      Redirect_Output_Append=> Mssyst.String.Ref (+ "Redirect_Output_Append(yes/no or ""filename"")"),
      Freeze_Graph_Window   => Mssyst.String.Ref (+ "Freeze_Graph_Window"),
      Unfreeze_Graph_Window => Mssyst.String.Ref (+ "Unfreeze_Graph_Window"),
      Update_Graph_Window   => Mssyst.String.Ref (+ "Update_Graph_Window"),
      Delay_For             => Mssyst.String.Ref (+ "delay_for(seconds)"),
      Set_Precision         => Mssyst.String.Ref (+ "Set_Precision(precision)"),
      Get_Max_Width         => Mssyst.String.Ref (+ "Get_Max_Width"),
      Get_Max_Height        => Mssyst.String.Ref (+ "Get_Max_Height"),
      Get_Mouse_X           => Mssyst.String.Ref (+ "Get_Mouse_X"),
      Get_Mouse_Y           => Mssyst.String.Ref (+ "Get_Mouse_Y"),
      Get_Font_Width        => Mssyst.String.Ref (+ "Get_Font_Width"),
      Get_Font_Height       => Mssyst.String.Ref (+ "Get_Font_Height"),
      Get_Window_Width      => Mssyst.String.Ref (+ "Get_Window_Width"),
      Get_Window_Height     => Mssyst.String.Ref (+ "Get_Window_Height"),
      Get_Key               => Mssyst.String.Ref (+ "Get_Key"),
      Key_Down              => Mssyst.String.Ref (+ "Key_Down(""key"")"),
      Get_Key_String        => Mssyst.String.Ref (+ "Get_Key_String"),
      Get_Mouse_Button      => Mssyst.String.Ref (+ "Get_Mouse_Button(which_button,x,y)"),
      Get_Pixel             => Mssyst.String.Ref (+ "Get_Pixel(x,y)"),
      Clear_Window          => Mssyst.String.Ref (+ "Clear_Window(color)"),
      Clear_Console         => Mssyst.String.Ref (+ "Clear_Console"),
      Close_Graph_Window    => Mssyst.String.Ref (+ "Close_Graph_Window"),
      Display_Text          => Mssyst.String.Ref (+ "Display_Text(x,y,""text"",color)"),
      Display_Number        => Mssyst.String.Ref (+ "Display_Number(x,y,number,color)"),
      Draw_Arc              => Mssyst.String.Ref (+ "Draw_Arc(x1,y1,x2,y2,startx,starty,endx,endy,color)"),
      Draw_Box              => Mssyst.String.Ref (+ "Draw_Box(x1,y1,x2,y2,color,filled)"),
      Draw_Circle           => Mssyst.String.Ref (+ "Draw_Circle(x,y,radius,color,filled)"),
      Draw_Ellipse          => Mssyst.String.Ref (+ "Draw_Ellipse(x1,y1,x2,y2,color,filled)"),
      Draw_Ellipse_Rotate   => Mssyst.String.Ref (+ "Draw_Ellipse_Rotate(x1,y1,x2,y2,angle,color,filled)"),
      Draw_Line             => Mssyst.String.Ref (+ "Draw_Line(x1,y1,x2,y2,color)"),
      Flood_Fill            => Mssyst.String.Ref (+ "Flood_Fill(x,y,color)"),
      Load_Bitmap           => Mssyst.String.Ref (+ "Load_Bitmap(filename)"),
      Draw_Bitmap           => Mssyst.String.Ref (+ "Draw_Bitmap(bitmap,x,y,width,height)"),
      Open_Graph_Window     => Mssyst.String.Ref (+ "Open_Graph_Window(width,height)"),
      Wait_For_Key          => Mssyst.String.Ref (+ "Wait_For_Key"),
      Wait_For_Mouse_Button => Mssyst.String.Ref (+ "Wait_For_Mouse_Button(which_button)"),
      Put_Pixel             => Mssyst.String.Ref (+ "Put_Pixel(x,y,color)"),
      Set_Window_Title      => Mssyst.String.Ref (+ "Set_Window_Title(""Title"")"),
      Save_Graph_Window     => Mssyst.String.Ref (+ "Save_Graph_Window(filename)"),
      Mouse_Button_Pressed  => Mssyst.String.Ref (+ "Mouse_Button_Pressed(which_button)"),
      Mouse_Button_Down     => Mssyst.String.Ref (+ "Mouse_Button_Down(which_button)"),
      Mouse_Button_Released => Mssyst.String.Ref (+ "Mouse_Button_Released(which_button)"),
      Set_Font_Size         => Mssyst.String.Ref (+ "Set_Font_Size(size)"),
      Is_Array              => Mssyst.String.Ref (+ "Is_Array(variable)"),
      Is_Character          => Mssyst.String.Ref (+ "Is_Character(variable)"),
      Is_String             => Mssyst.String.Ref (+ "Is_String(variable)"),
      Is_2d_Array           => Mssyst.String.Ref (+ "Is_2D_Array(variable)"),
      Is_Number             => Mssyst.String.Ref (+ "Is_Number(variable)"),
      Is_Open               => Mssyst.String.Ref (+ "Is_Open"),
      Key_Hit               => Mssyst.String.Ref (+ "Key_Hit"),
      End_Of_Input          => Mssyst.String.Ref (+ "End_Of_Input"));
end Suggestions;
