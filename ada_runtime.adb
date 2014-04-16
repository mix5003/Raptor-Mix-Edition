with Numbers;
with Mssyst.String;
use Mssyst.String;
with Raptor.Runtime;
with Raptor.Compile_Helpers;
with Raptor_Files;
with ada.Text_IO;
package body Ada_Runtime is

   procedure Redirect_Standard_Output is
   begin
      Raptor.Runtime.Redirect_Output(Filename => "Standard_Output");
   end Redirect_Standard_Output;

   procedure Redirect_Output(V : Numbers.Value) is
   begin
      if Numbers.Is_String(V) then
         Raptor.Runtime.Redirect_Output(Filename => Numbers.String_Of(V));
      else
         Raptor.Runtime.Redirect_Output(Yes_Or_No => Numbers.Integer_Of(v));
      end if;
   end Redirect_Output;

   procedure Redirect_Output_Append(V : Numbers.Value) is
   begin
      if Numbers.Is_String(V) then
         Raptor.Runtime.Redirect_Output_Append(Filename => Numbers.String_Of(V));
      else
         Raptor.Runtime.Redirect_Output_Append(Yes_Or_No => Numbers.Integer_Of(v));
      end if;
   end Redirect_Output_Append;


   procedure Redirect_Standard_Input is
   begin
      Raptor.Runtime.Redirect_Input("Standard_Input");
   end Redirect_Standard_Input;

   procedure Redirect_Input(V : Numbers.Value) is
   begin
      if Numbers.Is_String(V) then
         Raptor.Runtime.Redirect_Input(Filename => Numbers.String_Of(V));
      else
         Raptor.Runtime.Redirect_Input(Yes_Or_No => Numbers.Integer_Of(v));
      end if;
   end Redirect_Input;

   function Prompt_Dialog(S : in Mssyst.String.Ref;
         Force_Number : in Boolean) return Numbers.Value is
      Result_String : Mssyst.String.Ref;
      Is_Number : Boolean := False;
      Result : Numbers.Value;
   begin
      loop
         Result_String := mssyst.string.ref(Raptor.Runtime.Promptdialog(S));
         begin
            Result := Numbers.Make_Number_Value(Result_String);
            Is_Number := True;
         exception
            when others =>
               Result := Numbers.Make_String_Value(Result_String);
               if Raptor_Files.Input_Redirected and Force_Number then
                  raise;
               end if;
         end;
         -- don't keep trying if redirected
         exit when Raptor_Files.Input_Redirected or Is_Number or not Force_Number;
      end loop;
      return Result;
   end Prompt_Dialog;
end Ada_Runtime;
