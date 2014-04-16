with Msil_Types;
with Mssyst.String;
use MSSyst.String;
with Ada.Text_Io;
with Ada.Io_Exceptions;
use Ada.Io_Exceptions;
with Ada.Exceptions;
with Ada.Long_Long_Float_Text_Io;
with MSSyst.Net.Sockets.Socket;
with Mssyst.Net.sockets.Addressfamily;
with Mssyst.Net.sockets.Sockettype;
with Mssyst.Net.sockets.Protocoltype;
with Mssyst.Net.Ipendpoint;
with Mssyst.Net.Ipaddress;
with Mssyst.Text.Encoding;
with mssyst.net.endpoint;
with MSSyst.Io.StreamWriter;
with MSSyst.Io.StreamReader;
with raptor.Runtime;
with Ada.Exceptions;
with Mssyst.Windows.Forms;
use Mssyst.Windows.Forms;
with Mssyst.Windows.Forms.Messagebox;
with Mssyst.Windows.Forms.Dialogresult;
with Ada.Characters.Handling;
with Ada.Unchecked_Conversion;
package body Raptor_Files is
   Input_Is_Redirected : Boolean := False;
   Output_Is_Redirected : Boolean := False;
   type File_Access is access all Ada.Text_IO.File_Type;
   function To_File_Access is new Ada.Unchecked_Conversion(Ada.Text_IO.File_Access,
      File_Access);
   Current_Input : File_Access;
   Current_Input_Name : MSSyst.String.Ref;
   Current_Output : File_Access;
   Current_Output_Name : Mssyst.String.Ref;
   Network_Is_Redirected : Boolean := False;
   Process_Redirected : Boolean := False;
   Current_Socket : MSSyst.Net.Sockets.Socket.Ref;
   Current_Process : MSSyst.Diagnostics.Process.Ref;

   Buffer  : Msil_Types.Unsigned_Int8_Arr(0..511);
   Mini_Buffer : Msil_Types.Unsigned_Int8_Arr(0..255);
   Buffer_Last : Natural;
   Buffer_Current : Natural := Buffer'Last+1;
   Max_Line_Length : constant := Buffer'Length/2;
   Process_Eof : Boolean := False;
   function Network_Redirected return Boolean is
   begin
      return Network_Is_Redirected;
   end Network_Redirected;

   function Network_Eof return Boolean is
      Mini_Last : Natural;
   begin
      if Buffer_Current > Buffer_Last-5 then
         Mini_Last := Current_Socket.Receive(Buffer);
         Mini_Last := Mini_Last - 1;
         Buffer(0..Buffer_Last-Buffer_Current) :=
            Buffer(Buffer_Current..Buffer_Last);
         for I in 0..Mini_Last loop
            Buffer(Buffer_Last-Buffer_Current+1+I) :=
               Mini_Buffer(I);
         end loop;
         Buffer_Last := Mini_Last+Buffer_Last-Buffer_Current;
         Buffer_Current := 0;
      end if;
      if Buffer_Current > Buffer_Last - 5 then
         return False;
      elsif Buffer_Current > Buffer_Last then
         return True;
      else
         return Character(Buffer(Buffer_Current))='E' and
                Character(Buffer(Buffer_Current+1))='O' and
                Character(Buffer(Buffer_Current+2))='F' and
                Character(Buffer(Buffer_Current+3))=ASCII.CR and
                Character(Buffer(Buffer_Current+4))=Ascii.Lf;
      end if;

   end Network_Eof;


   function Get_Line_Network return MSSyst.String.Ref is
      Result_Str : String(1..Max_Line_Length);
      Temp : Natural := 1;
      Last : Natural;
   begin
      -- loop until we find a CR or hit the end
      while Buffer_Current <= Buffer_Last and then
            Character(Buffer(Buffer_Current)) /= Ascii.Cr loop
         Result_Str(Temp) := Character(Buffer(Buffer_Current));
         Temp := Temp + 1;
         Buffer_Current := Buffer_Current + 1;
      end loop;
      -- if we need to refresh buffer current then do it.
      if Buffer_Current>Buffer_Last then
         Buffer_Last := Current_Socket.Receive(Buffer);
         Buffer_Last := Buffer_Last - 1;
         Buffer_Current := 0;
      end if;
      -- keep looping if we had to refresh buffer current
      while Character(Buffer(Buffer_Current)) /= Ascii.Cr
         and Character(Buffer(Buffer_Current)) /= Ascii.Ht loop
         Result_Str(Temp) := Character(Buffer(Buffer_Current));
         Temp := Temp + 1;
         Buffer_Current := Buffer_Current + 1;
      end loop;
      -- get the CR or HT
      Result_Str(Temp) := Character(Buffer(Buffer_Current));
      Temp := Temp + 1;
      Buffer_Current := Buffer_Current + 1;
      Last := Temp - 1;
      -- if we need to refresh buffer current then do it.
      if Buffer_Current>Buffer_Last then
         Buffer_Last := Current_Socket.Receive(Buffer);
         Buffer_Last := Buffer_Last - 1;
         Buffer_Current := 0;
      end if;
      if Result_Str(Temp-1) = ASCII.CR then
         -- get the LF
         Result_Str(Temp) := Character(Buffer(Buffer_Current));
         Buffer_Current := Buffer_Current + 1;
         Last := Last - 1;
      end if;
      -- check for EOF
      if Temp=5 and Result_Str(1..3)="EOF" then
         raise End_Error;
      end if;
      return +Result_Str(1..Last);
   exception when e:others =>
      raise End_Error;
   end Get_Line_Network;

   procedure Network_Redirect(Host : in MSSyst.String.Ref; Port : in Integer;
         Use_Tcp : in Boolean) is
      Remote_Ep : access Mssyst.Net.Ipendpoint.typ;
      Ipadd : access Mssyst.Net.Ipaddress.Typ;
      --result : dialogresult.valuetype;
   begin
      if Process_Redirected then
         Ada.Exceptions.Raise_Exception(Status_Error'Identity,
            "Output was already redirected to process.");
      end if;
      if Network_Is_Redirected then
         Ada.Exceptions.Raise_Exception(Status_Error'Identity,
            "Output was already redirected to network.");
      end if;
      -- mcc: 09/19/05 wipe buffer
      Buffer_Current := Buffer'Last+1;
      -- create the socket
      if Use_Tcp then
         Current_Socket := Mssyst.Net.Sockets.Socket.New_Socket(
            addressfamily => Mssyst.Net.sockets.Addressfamily.Internetwork,
            sockettype    => Mssyst.Net.sockets.Sockettype.Stream,
            Protocoltype  => Mssyst.Net.Sockets.Protocoltype.Tcp);
      else
         Current_Socket := Mssyst.Net.Sockets.Socket.New_Socket(
            addressfamily => Mssyst.Net.sockets.Addressfamily.Internetwork,
            sockettype    => Mssyst.Net.sockets.Sockettype.Stream,
            Protocoltype  => Mssyst.Net.Sockets.Protocoltype.Udp);
      end if;
      -- set the IP address and port
      Ipadd := Mssyst.Net.Ipaddress.Parse(Host);
      Remote_Ep := Mssyst.Net.Ipendpoint.New_Ipendpoint(
         Address => Ipadd,Port => Port);
      -- do the connection
      current_socket.connect(remote_ep);
      Network_Is_Redirected := True;
      --result := Messagebox.Show(text=>current_socket.toString);
   end Network_Redirect;

   procedure Stop_Network_Redirect is
   begin
      Network_Is_Redirected := False;
      -- mcc: 09/19/05 wipe buffer
      Buffer_Current := Buffer'Last+1;
      Current_Socket.close;
      Current_Socket := null;
   end Stop_Network_Redirect;

   procedure Process_Redirect(Process : in MSSyst.Diagnostics.Process.Ref) is
   begin
      if Network_Is_Redirected then
         Ada.Exceptions.Raise_Exception(Status_Error'Identity,
            "Output was already redirected to network.");
      end if;
      if Process_Redirected then
         Ada.Exceptions.Raise_Exception(Status_Error'Identity,
            "Output was already redirected to process.");
      end if;
      Process_Redirected := True;
      Current_Process := Process;
   end Process_Redirect;

   procedure Stop_Process_Redirect is
   begin
      Process_Redirected := False;
      Current_Process := null;
   end Stop_Process_Redirect;


   procedure Write(S : in MSSyst.String.Ref) is
      Byte_Data : Msil_Types.Unsigned_Int8_Array;
      Result : Integer;
   begin
      if Process_Redirected then
         Current_Process.get_StandardInput.Write(s);
      elsif Network_Is_Redirected then
         Byte_Data := Mssyst.Text.Encoding.Get_Ascii.Getbytes(S);
         declare
            bytes : msil_types.unsigned_int8_arr(0..byte_data.all'length-1) := byte_data.all;
         begin
            Result := Current_Socket.Send(Buffer => Bytes);
         end;
      else
         Ada.Text_Io.Put(File => Current_Output.all, Item => +S);
      end if;
   end Write;

   procedure Writeln(S : in MSSyst.String.Ref) is
   begin
      if Process_Redirected then
         Current_Process.get_StandardInput.Writeline(s);
      elsif Network_Is_Redirected then
         Write(Mssyst.String.Ref(
            mssyst.string.concat(s,+(ASCII.CR&ASCII.LF))));
      else
         Ada.Text_IO.Put_Line(File => Current_Output.all, Item => +S);
      end if;
   end Writeln;
   procedure My_Get_Line(File : in Ada.Text_IO.File_Type;
         Item : out String;
         Last : out Natural) is
      Char : Character;
      --result : dialogresult.valuetype;
   begin
      Last := Item'First - 1;
      loop
         exit when Ada.Text_IO.End_Of_File(File);
         if Ada.Text_IO.End_Of_Line(File) then
            Ada.Text_IO.Skip_Line(File);
            exit;
         end if;
         Ada.Text_IO.Get(File,Char);
         exit when Char = ASCII.HT;
         Last := Last + 1;
         Item(Last) := Char;
      end loop;
      --result := Messagebox.Show(text=>item);
   end My_Get_Line;



   function Get_Line return MSSyst.String.Ref is
      Item : String(1..256);
      Last : Natural;
      S : MSSyst.String.Ref;
      --result : dialogresult.valuetype;
   begin
      if Process_Redirected then
         S := MSSyst.String.Ref(Current_Process.Get_StandardOutput.ReadLine);
         if (S.Compareto("")=0) then
            S := MSSyst.String.Ref(Current_Process.Get_StandardOutput.ReadLine);
         end if;
         if (S.Compareto("EOF")=0) then
            Process_EOF := True;
            raise End_Error;
         else
            Process_EOF := False;
         end if;
         return s;
      elsif Network_Is_Redirected then
         return Get_Line_Network;
      else
         My_Get_Line(File => Current_Input.all, Item => Item, Last => Last);
      end if;

      return +Item(1..Last);
   exception when End_Error =>
         Ada.Exceptions.Raise_Exception(End_Error'Identity,
            "Attempted to read past end of file.");
   end Get_Line;

   function Input_Redirected return Boolean is
   begin
      return Input_Is_Redirected or Network_Is_Redirected or Process_Redirected;
   end Input_Redirected;

   function Output_Redirected return Boolean is
   begin
      return Output_Is_Redirected or Network_Is_Redirected or Process_Redirected;
   end Output_Redirected;


   procedure Stop_Redirect_Output is
      use type Ada.Text_IO.File_Access;
   begin
      if Output_Is_Redirected then
         Output_Is_Redirected := False;
         if Ada.Text_IO.File_Access(Current_Output)/=Ada.Text_IO.Standard_Output then
            Ada.Text_Io.Close(Current_Output.all);
         end if;
      end if;
   end Stop_Redirect_Output;

   procedure Stop_Redirect_Input is
      use type Ada.Text_IO.File_Access;
   begin
      if Input_Is_Redirected then
         Input_Is_Redirected := False;
         if Ada.Text_IO.File_Access(Current_Input)/=Ada.Text_IO.Standard_Input then
            Ada.Text_Io.Close(Current_Input.all);
         end if;
      end if;
   end Stop_Redirect_Input;

   -----------------
   -- Close_Files --
   -----------------

   procedure Close_Files is
   begin
      Stop_Redirect_Input;
      Stop_Redirect_Output;
   end Close_Files;

   ------------------
   -- End_Of_Input --
   ------------------

   function End_Of_Input return Boolean is
   begin
      if Process_Redirected then
         return Process_EOF or else Current_Process.get_StandardOutput.get_EndOfStream;
      elsif Network_Is_Redirected then
         return Network_Eof;
      elsif Input_Is_Redirected then
         return Ada.Text_Io.End_Of_File(Current_Input.all);
      else
         return False;
      end if;
   end End_Of_Input;

   --------------------
   -- Redirect_Input --
   --------------------

   procedure Redirect_Input (Filename : in MSSyst.String.Ref) is
   begin
      -- don't allow redirection if networked (ignore)
      if Network_Is_Redirected or Process_Redirected then
         return;
      end if;
      if Input_Is_Redirected then
         if Mssyst.String.op_Equality(MSSyst.String.ToLower(Filename),MSSyst.String.ToLower(Current_Input_Name)) then
            Ada.Exceptions.Raise_Exception(Status_Error'Identity,
               "Output was already redirected to: " & (+Filename));
         end if;
         Stop_Redirect_Input;
      end if;
      begin
         if Ada.Characters.Handling.To_Lower(+Filename)="standard_input" then
            Current_Input := To_File_Access(Ada.Text_IO.Standard_Input);
         else
            Current_Input := new Ada.text_io.file_type;
            Ada.Text_Io.Open(File => Current_Input.all, Name => +Filename, Mode => Ada.Text_Io.In_File);
         end if;
      exception when others =>
         Ada.Exceptions.Raise_Exception(Name_Error'Identity,
            "Unable to redirect input to: " & (+Filename));
      end;
      Current_Input_Name := mssyst.string.ref(Filename);
      Input_Is_Redirected := True;
   end Redirect_Input;

   ---------------------
   -- Redirect_Output --
   ---------------------

   procedure Redirect_Output (Filename : in MSSyst.String.Ref; Append : in Boolean) is
      --result : dialogresult.valuetype;
   begin
      --result := Messagebox.Show(text=>filename);
      -- don't allow redirection if networked (ignore)
      if Network_Is_Redirected or Process_Redirected then
         return;
      end if;
      if Output_Is_Redirected then
         if Mssyst.String.op_Equality(MSSyst.String.ToLower(Filename),MSSyst.String.ToLower(Current_Output_Name)) then
            Ada.Exceptions.Raise_Exception(Status_Error'Identity,
               "Output was already redirected to: " & (+Filename));
         end if;
         Stop_Redirect_Output;
      end if;
      begin
         if Ada.Characters.Handling.To_Lower(+Filename)="standard_output" then
            Current_Output := To_File_Access(Ada.Text_IO.Standard_Output);
         else
            Current_Output := new Ada.Text_IO.File_Type;
            if Append then
            begin
               Ada.Text_Io.Open(File => Current_Output.All, Name => +Filename, Mode => Ada.Text_Io.Append_File);
            exception when others =>
               Ada.Text_Io.Create(File => Current_Output.all, Name => +Filename, Mode => Ada.Text_Io.Out_File);
            end;
            else
            Ada.Text_Io.Create(File => Current_Output.all, Name => +Filename, Mode => Ada.Text_Io.Out_File);
            end if;
         end if;
      exception when others =>
         Ada.Exceptions.Raise_Exception(Name_Error'Identity,
            "Unable to redirect output to: " & (+Filename));
      end;
      Current_Output_Name := mssyst.string.ref(Filename);
      Output_Is_Redirected := True;
   end Redirect_Output;

end Raptor_Files;

