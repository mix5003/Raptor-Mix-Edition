with Numbers;
with Mssyst.String;
use MSSyst.String;
package Ada_Runtime is
   procedure Redirect_Standard_Output;
   procedure Redirect_Standard_Input;
   procedure Redirect_Output (
         V : Numbers.Value);
   procedure Redirect_Output_Append (
      V : Numbers.Value);
   procedure Redirect_Input (
         V : Numbers.Value);
   function Prompt_Dialog (
         S            : in     Mssyst.String.Ref;
         Force_Number : in     Boolean)
     return Numbers.Value;
end Ada_Runtime;
