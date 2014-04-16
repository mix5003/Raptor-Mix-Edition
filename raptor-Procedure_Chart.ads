-- Autogenerated by MSIL2Ada v. 2
-- By: Martin C. Carlisle
--     Department of Computer Science
--     US Air Force Academy
--     carlislem@acm.org
with MSSyst.Object;
with MSIL_Types;
use MSIL_Types;
limited with MSSyst.ComponentModel.CancelEventHandler;
with MSSyst.ComponentModel.Component;
with MSSyst.ComponentModel.IComponent;
limited with MSSyst.ComponentModel.IContainer;
limited with MSSyst.ComponentModel.ISite;
with MSSyst.ComponentModel.ISynchronizeInvoke;
limited with MSSyst.Delegate;
limited with MSSyst.Drawing.Bitmap;
limited with MSSyst.Drawing.Color;
limited with MSSyst.Drawing.Font;
limited with MSSyst.Drawing.Graphics;
limited with MSSyst.Drawing.Image;
limited with MSSyst.Drawing.Point;
limited with MSSyst.Drawing.Rectangle;
limited with MSSyst.Drawing.Region;
limited with MSSyst.Drawing.SizeF;
limited with MSSyst.Drawing.Size;
limited with MSSyst.EventArgs;
limited with MSSyst.EventHandler;
limited with MSSyst.IAsyncResult;
with MSSyst.IDisposable;
with MSSyst.MarshalByRefObject;
limited with MSSyst.Reflection.Emit.MethodBuilder;
limited with MSSyst.Runtime.Remoting.ObjRef;
limited with MSSyst.String;
limited with MSSyst.Type_k;
limited with MSSyst.Windows.Forms.AccessibleObject;
limited with MSSyst.Windows.Forms.AccessibleRole;
limited with MSSyst.Windows.Forms.AnchorStyles;
limited with MSSyst.Windows.Forms.AutoSizeMode;
limited with MSSyst.Windows.Forms.BindingContext;
limited with MSSyst.Windows.Forms.BorderStyle;
limited with MSSyst.Windows.Forms.BoundsSpecified;
limited with MSSyst.Windows.Forms.ContextMenuStrip;
limited with MSSyst.Windows.Forms.ContextMenu;
with MSSyst.Windows.Forms.Control;
limited with MSSyst.Windows.Forms.Control.ControlCollection;
limited with MSSyst.Windows.Forms.ControlBindingsCollection;
limited with MSSyst.Windows.Forms.ControlEventHandler;
limited with MSSyst.Windows.Forms.Cursor;
limited with MSSyst.Windows.Forms.DockStyle;
limited with MSSyst.Windows.Forms.DragDropEffects;
limited with MSSyst.Windows.Forms.DragEventHandler;
limited with MSSyst.Windows.Forms.Form;
limited with MSSyst.Windows.Forms.GetChildAtPointSkip;
limited with MSSyst.Windows.Forms.GiveFeedbackEventHandler;
limited with MSSyst.Windows.Forms.HScrollProperties;
limited with MSSyst.Windows.Forms.HelpEventHandler;
with MSSyst.Windows.Forms.IBindableComponent;
limited with MSSyst.Windows.Forms.IContainerControl;
with MSSyst.Windows.Forms.IDropTarget;
with MSSyst.Windows.Forms.IWin32Window;
limited with MSSyst.Windows.Forms.IWindowTarget;
limited with MSSyst.Windows.Forms.ImageLayout;
limited with MSSyst.Windows.Forms.ImeMode;
limited with MSSyst.Windows.Forms.InvalidateEventHandler;
limited with MSSyst.Windows.Forms.KeyEventHandler;
limited with MSSyst.Windows.Forms.KeyPressEventHandler;
limited with MSSyst.Windows.Forms.Layout.LayoutEngine;
limited with MSSyst.Windows.Forms.LayoutEventHandler;
with MSSyst.Windows.Forms.Message;
limited with MSSyst.Windows.Forms.MouseEventHandler;
limited with MSSyst.Windows.Forms.Padding;
limited with MSSyst.Windows.Forms.PaintEventHandler;
with MSSyst.Windows.Forms.Panel;
limited with MSSyst.Windows.Forms.PreProcessControlState;
limited with MSSyst.Windows.Forms.PreviewKeyDownEventHandler;
limited with MSSyst.Windows.Forms.QueryAccessibilityHelpEventHandler;
limited with MSSyst.Windows.Forms.QueryContinueDragEventHandler;
limited with MSSyst.Windows.Forms.RightToLeft;
limited with MSSyst.Windows.Forms.ScrollEventHandler;
with MSSyst.Windows.Forms.ScrollableControl;
limited with MSSyst.Windows.Forms.ScrollableControl.DockPaddingEdges;
with MSSyst.Windows.Forms.TabPage;
limited with MSSyst.Windows.Forms.UICuesEventHandler;
limited with MSSyst.Windows.Forms.VScrollProperties;
limited with raptor.Component;
with raptor.Subchart;
limited with raptor.Subchart_Kinds;
limited with raptor.Visual_Flow_Form;
package raptor.Procedure_Chart is
   type Typ is new raptor.Subchart.Typ
         and MSSyst.ComponentModel.IComponent.Typ
         and MSSyst.IDisposable.Typ
         and MSSyst.Windows.Forms.IDropTarget.Typ
         and MSSyst.ComponentModel.ISynchronizeInvoke.Typ
         and MSSyst.Windows.Forms.IWin32Window.Typ
         and MSSyst.Windows.Forms.IBindableComponent.Typ
   with record
      subMethodBuilder : access MSSyst.Reflection.Emit.MethodBuilder.Typ'Class;
      pragma Import(MSIL,subMethodBuilder,"subMethodBuilder");
   end record;
   type Ref is access all Typ'Class;
   type Ref_addrof is access all Ref;
   type Ref_Arr is array(Natural range <>) of Ref;
   type Ref_Array is access all Ref_Arr;
   type Ref_Array_addrof is access all Ref_Array;
   function new_Procedure_Chart(
      This : Ref := null;
      the_form : access raptor.Visual_Flow_Form.Typ'Class;
      name : access MSSyst.String.Typ'Class;
      param_count : Integer) return Ref;
   function new_Procedure_Chart(
      This : Ref := null;
      the_form : access raptor.Visual_Flow_Form.Typ'Class;
      name : access MSSyst.String.Typ'Class;
      incoming_param_names : access MSSyst.String.Ref_arr;
      is_input : MSIL_Types.Bool_Arr;
      is_output : MSIL_Types.Bool_Arr) return Ref;
   function get_num_params(
      This : access Typ) return Integer;
   function get_param_string(
      This : access Typ) return access MSSyst.String.Typ'Class;
   function getArgIsInput(
      This : access Typ) return MSIL_Types.Bool_Array;
   function getArgIsOutput(
      This : access Typ) return MSIL_Types.Bool_Array;
   function getArgs(
      This : access Typ) return access MSSyst.String.Ref_arr;
   function getFullName(
      This : access Typ) return access MSSyst.String.Typ'Class;
   function is_input_parameter(
      This : access Typ;
      i : Integer) return Standard.Boolean;
   function is_output_parameter(
      This : access Typ;
      i : Integer) return Standard.Boolean;
   function parameter_name(
      This : access Typ;
      i : Integer) return access MSSyst.String.Typ'Class;
   function parameter_string(
      This : access Typ;
      i : Integer) return access MSSyst.String.Typ'Class;
   function RunDialog(
      This : access Typ;
      name : access MSSyst.String.Typ'Class;
      form : access raptor.Visual_Flow_Form.Typ'Class) return access MSSyst.String.Typ'Class;
private
   pragma Convention(MSIL,Typ);
   pragma MSIL_Constructor(new_Procedure_Chart);
   pragma Import(MSIL,get_num_params,"get_num_params");
   pragma Import(MSIL,get_param_string,"get_param_string");
   pragma Import(MSIL,getArgIsInput,"getArgIsInput");
   pragma Import(MSIL,getArgIsOutput,"getArgIsOutput");
   pragma Import(MSIL,getArgs,"getArgs");
   pragma Import(MSIL,getFullName,"getFullName");
   pragma Import(MSIL,is_input_parameter,"is_input_parameter");
   pragma Import(MSIL,is_output_parameter,"is_output_parameter");
   pragma Import(MSIL,parameter_name,"parameter_name");
   pragma Import(MSIL,parameter_string,"parameter_string");
   pragma Import(MSIL,RunDialog,"RunDialog");
end raptor.Procedure_Chart;
pragma Import(MSIL,raptor.Procedure_Chart,
   ".ver 4:0:5:2",
   "[raptor]raptor.Procedure_Chart");
