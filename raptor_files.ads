with Msil_Types;
with Mssyst.String;
with MSSyst.Diagnostics.Process;

package Raptor_Files is
   procedure Redirect_Input(Filename : in MSSyst.String.Ref);
   procedure Stop_Redirect_Input;
   procedure Redirect_Output(Filename : in MSSyst.String.Ref; Append : in Boolean);
   procedure Stop_Redirect_Output;
   function End_Of_Input return Boolean;
   function Input_Redirected return Boolean;
   function Output_Redirected return Boolean;
   function Network_Redirected return Boolean;
   procedure Close_Files;
   function Get_Line return MSSyst.String.Ref;
   procedure Write(S : in MSSyst.String.Ref);
   procedure Writeln(S : in MSSyst.String.Ref);
   -- for redirecting to a TCP/IP socket server
   procedure Network_Redirect(Host : in MSSyst.String.Ref; Port : in Integer;
      Use_Tcp : in Boolean);
   procedure Stop_Network_Redirect;
   procedure Process_Redirect(Process : in MSSyst.Diagnostics.Process.Ref);
   procedure Stop_Process_Redirect;
end Raptor_Files;
