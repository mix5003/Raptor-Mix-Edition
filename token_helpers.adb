with Lexer;
with Mssyst.String;
use MSSyst.String;
package body Token_Helpers is
   function Verify_Id(S : in Mssyst.String.Ref) return Boolean is
      P : Lexer.Token_Pointer;
      use type Lexer.Token_Type;
   begin
      Lexer.Initialize(Text => +s);
      P := Lexer.Get_Token;
      return P.Kind=Lexer.Id;
   end Verify_Id;
   
end Token_Helpers;
