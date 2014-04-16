package Lexer is
   type Token_Type is (Plus, Minus, Times, Divide, Exponent, Left_Paren,
      Right_Paren, Id, Number, Character_T,
      String_T, Comma,
      Semicolon, Colon_Equal, Left_Bracket, Right_Bracket, End_Input,
      New_T, Dot,
      -- relational operators
      Equal, Greater, Less, Not_Equal, Greater_Equal, Less_Equal,
      And_T, Or_T, Xor_T, Not_T, Mod_T, Rem_T,
      -- trig functions (1 or 2 params)
      Sin,Cos,Tan,Cot,Arcsin,Arccos,Log,
      -- trig functions (2, or 3 parameters)
      Arctan,Arccot,
      -- functions of 2 parameters
      Min,Max,
      -- functions of 1 parameter
      Sinh,Tanh,Cosh,Arccosh,Arcsinh,Arctanh,
      Coth,Arccoth,Sqrt,Floor,Ceiling,to_ascii,to_character,Length_Of,Abs_F,
      -- functions of 0 parameters
      E,Pi,Random,
      Black, Blue, Green, Cyan, Red, Magenta, Brown,
      Light_Gray, Dark_Gray, Light_Blue, Light_Green,
      Light_Cyan, Light_Red, Light_Magenta, Yellow, White,
      Filled,Unfilled,
      True,False,
      Yes,No,
      Random_Color,Random_Extended_Color,
      Left_Button, Right_Button,
      -- adagraph functions 0 parameters
      Get_Max_Width,Get_Max_Height,
      Get_Mouse_X,
      Get_Mouse_Y,
      Get_Window_Width, Get_Window_Height,
      Get_Key,Get_Key_String,Get_Font_Width,Get_Font_Height,
      -- adagraph functions 1 parameters
      Load_Bitmap,
      -- functions of 2 parameters
      Get_Pixel,
      -- functions of 3 parameters
      Closest_Color,
      -- regular procedures no params
      Clear_Console,
      -- regular procedures with params
      Delay_For, Redirect_Output_Append, Set_Precision, Redirect_Input, Redirect_Output,
      -- adagraph procedures
      Clear_Window,Display_Number,Display_Text,Draw_Arc,
      -- adagraph procs of 0 params
      Close_Graph_Window,Freeze_Graph_Window,Update_Graph_Window,
      Unfreeze_Graph_Window,
      -- end adagraph procs of 0 params
      Draw_Bitmap,
      Draw_Box,Draw_Circle,Draw_Ellipse,Draw_Ellipse_Rotate,Draw_Line,
      Flood_Fill,Open_Graph_Window,Wait_For_Key,
      Wait_For_Mouse_Button,
      Get_Mouse_Button,
      Set_Font_Size,
      Put_Pixel,Save_Graph_Window,Set_Window_Title,
      -- reflection boolean functions (1 param)
      Is_Number,Is_String,Is_Character,Is_Array,Is_2D_Array,
      -- adagraph boolean functions (1 param, then 0 param)
      Mouse_Button_Pressed,Mouse_Button_Down,Key_Down,Mouse_Button_Released,Is_Open,Key_Hit,
      -- other boolean functions (0 param)
      End_Of_Input);

   -- categories of tokens.
   subtype Relation is Token_Type range Equal..Less_Equal;
   subtype Subprogram is Token_Type range Sin..End_Of_Input;
   -- not necessarily just graphics, but ones with suggestions
   subtype Other_Proc0 is Subprogram range Clear_Console..Clear_Console;
   subtype Graph_Subprogram is Subprogram range Random_Color..End_Of_Input;
   subtype Graph_Proc0 is Graph_Subprogram range Close_Graph_Window..Unfreeze_Graph_Window;
   subtype Func_Token_Types is Subprogram range Sin..Closest_Color;
   subtype Func_Id1 is Func_Token_Types range Sinh..Abs_F;
   subtype Func_Id0 is Func_Token_Types range E..Get_Font_Height;
   subtype Func_Id1or2 is Func_Token_Types range Sin..Log;
   subtype Func_Id2or3 is Func_Token_Types range Arctan..Arccot;
   subtype Graph_Func_Id2 is Func_Token_Types range Get_Pixel..Get_Pixel;
   subtype Other_Func_Id2 is Func_Token_Types range Min..Max;
   subtype Func_Id3 is Func_Token_Types range Closest_Color..Closest_Color;
   subtype Proc_Token_Types is Subprogram range Clear_Console..Set_Window_Title;
   subtype Boolean_Func_Types is Subprogram range Is_Number..End_Of_Input;
   -- we don't mark these as func1 b/c they can only take a variable name
   subtype Boolean_Reflection_Types is Subprogram range
      Is_Number..Is_2D_Array;
   subtype Boolean_Func0_Types is Boolean_Func_Types range
      Is_Open..End_Of_Input;
   subtype Boolean_Func1_Types is Boolean_Func_Types range
      Mouse_Button_Pressed..Mouse_Button_Released;
   type Parameter_Count_Array is array(Proc_Token_Types) of Natural;

   type Token is record
      Kind  : Token_Type;
      Start : Natural;
      Finish: Natural;
   end record;
   type Token_Pointer is access all Token;

   -------------------------------------------------
   -- Initialize
   --
   -- preconditions: none
   -- postconditions:  Get_Token will read from this
   --                  string
   -------------------------------------------------
   procedure Initialize (Text : in String);
   function Get_Text return String;

   ---------------------------------------------------
   -- Get_Token
   --
   -- preconditions: Initialize has been called
   -- postconditions:  pointer in string advances by
   --                  one token
   --                  If Unget_Token was previously
   --                  called, that token is returned
   --                  instead.
   ---------------------------------------------------
   Bad_Token : exception;
   function Get_Token return Token_Pointer;

   ---------------------------------------------------
   -- Get_Current_Location
   --
   -- in case of Bad_Token, figure out where
   ---------------------------------------------------
   function Get_Current_Location return Positive;

   ---------------------------------------------------
   -- Rewind
   --
   -- preconditions: Get_Token has been called.
   -- postconditions: rewinds a number of tokens
   --                 until P was the last token gotten
   --                 allows a multiple unget feature
   ---------------------------------------------------
   procedure Rewind(P : in Token_Pointer);

   ---------------------------------------------------
   -- Unget_Token
   --
   -- preconditions: Get_Token has been called.  It
   --                is assumed that Unget_Token is
   --                given a P which was the last
   --                returned value from Get_Token
   --                CAN ONLY DO THIS 1X in a row.
   -- postconditions:  P is stored so that it will be
   --                  returned next by Get_Token
   ---------------------------------------------------
   procedure Unget_Token(P : in Token_Pointer);

   ---------------------------------------------------
   -- check to make sure this procedure has the right
   -- number of parameters.
   ---------------------------------------------------
   Bad_Count : exception;
   procedure Verify_Parameter_Count(
      Proc_Name : in String;
      Count     : in Natural);
end Lexer;
