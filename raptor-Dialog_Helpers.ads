-- Autogenerated by MSIL2Ada v. 2
-- By: Martin C. Carlisle
--     Department of Computer Science
--     US Air Force Academy
--     carlislem@acm.org
with MSSyst.Object;
with MSIL_Types;
use MSIL_Types;
limited with MSSyst.Drawing.Font;
limited with MSSyst.Drawing.Graphics;
limited with MSSyst.String;
limited with MSSyst.Type_k;
limited with MSSyst.Windows.Forms.Label;
limited with MSSyst.Windows.Forms.MouseEventArgs;
limited with MSSyst.Windows.Forms.RichTextBox;
limited with MSSyst.Windows.Forms.TextBox;
limited with interpreter.suggestion_result;
package raptor.Dialog_Helpers is
   type Typ is new MSSyst.Object.Typ   with record
      null;
   end record;
   type Ref is access all Typ'Class;
   type Ref_addrof is access all Ref;
   type Ref_Arr is array(Natural range <>) of Ref;
   type Ref_Array is access all Ref_Arr;
   type Ref_Array_addrof is access all Ref_Array;
   function new_Dialog_Helpers(
      This : Ref := null) return Ref;
   procedure Check_Hint(
      textbox : access MSSyst.Windows.Forms.TextBox.Typ'Class;
      textBox1 : access MSSyst.Windows.Forms.RichTextBox.Typ'Class;
      kind : Integer;
      current_suggestion : access MSSyst.String.Ref;
      suggestion_result : access interpreter.suggestion_result.Ref;
      error : MSIL_Types.Bool_addrof;
      this_Font : access MSSyst.Drawing.Font.Typ'Class);
   function Complete_Suggestion(
      textbox : access MSSyst.Windows.Forms.TextBox.Typ'Class;
      kind : Integer;
      current_suggestion : access MSSyst.String.Typ'Class;
      suggestion_result : access interpreter.suggestion_result.Ref) return Standard.Boolean;
   procedure Init;
   function Is_Simple_String(
      s : access MSSyst.String.Typ'Class) return Standard.Boolean;
   procedure Paint_Helper(
      labelGraphics : access MSSyst.Drawing.Graphics.Typ'Class;
      text : access MSSyst.String.Typ'Class;
      label : access MSSyst.Windows.Forms.Label.Typ'Class;
      error_msg : access MSSyst.String.Typ'Class;
      location : Integer;
      error : Standard.Boolean);
   procedure suggestions_downarrow(
      textBox1 : access MSSyst.Windows.Forms.RichTextBox.Typ'Class;
      current_suggestion : access MSSyst.String.Ref);
   procedure suggestions_mousedown(
      textBox1 : access MSSyst.Windows.Forms.RichTextBox.Typ'Class;
      current_suggestion : access MSSyst.String.Ref;
      e : access MSSyst.Windows.Forms.MouseEventArgs.Typ'Class);
   procedure suggestions_uparrow(
      textBox1 : access MSSyst.Windows.Forms.RichTextBox.Typ'Class;
      current_suggestion : access MSSyst.String.Ref);
private
   pragma Convention(MSIL,Typ);
   pragma MSIL_Constructor(new_Dialog_Helpers);
   pragma Import(MSIL,Check_Hint,"Check_Hint");
   pragma Import(MSIL,Complete_Suggestion,"Complete_Suggestion");
   pragma Import(MSIL,Init,"Init");
   pragma Import(MSIL,Is_Simple_String,"Is_Simple_String");
   pragma Import(MSIL,Paint_Helper,"Paint_Helper");
   pragma Import(MSIL,suggestions_downarrow,"suggestions_downarrow");
   pragma Import(MSIL,suggestions_mousedown,"suggestions_mousedown");
   pragma Import(MSIL,suggestions_uparrow,"suggestions_uparrow");
end raptor.Dialog_Helpers;
pragma Import(MSIL,raptor.Dialog_Helpers,
   ".ver 4:0:5:2",
   "[raptor]raptor.Dialog_Helpers");
