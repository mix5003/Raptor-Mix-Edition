-- Autogenerated by MSIL2Ada v. 2
-- By: Martin C. Carlisle
--     Department of Computer Science
--     US Air Force Academy
--     carlislem@acm.org
with MSSyst.Object;
with MSIL_Types;
use MSIL_Types;
limited with MSSyst.String;
limited with MSSyst.Type_k;
limited with MSSyst.Windows.Forms.TreeNode;
limited with MSSyst.Windows.Forms.TreeView;
with numbers;
limited with raptor.MasterConsole;
limited with raptor.Oval;
limited with raptor.Procedure_Chart;
limited with raptor.Runtime.Add_Node_Delegate;
limited with raptor.Runtime.Delete_Scope_Delegate;
limited with raptor.Visual_Flow_Form;
package raptor.Runtime is
   type Typ is new MSSyst.Object.Typ   with record
      null;
   end record;
   processing_parameter_list : Standard.Boolean;
   pragma Import(MSIL,processing_parameter_list,"processing_parameter_list");
   method_return_value : numbers.value;
   pragma Import(MSIL,method_return_value,"method_return_value");
   add_delegate : access raptor.Runtime.Add_Node_Delegate.Typ'Class;
   pragma Import(MSIL,add_delegate,"add_delegate");
   add_at_front_delegate : access raptor.Runtime.Add_Node_Delegate.Typ'Class;
   pragma Import(MSIL,add_at_front_delegate,"add_at_front_delegate");
   add_to_heap_delegate : access raptor.Runtime.Add_Node_Delegate.Typ'Class;
   pragma Import(MSIL,add_to_heap_delegate,"add_to_heap_delegate");
   add_to_classes_delegate : access raptor.Runtime.Add_Node_Delegate.Typ'Class;
   pragma Import(MSIL,add_to_classes_delegate,"add_to_classes_delegate");
   delete_scope : access raptor.Runtime.Delete_Scope_Delegate.Typ'Class;
   pragma Import(MSIL,delete_scope,"delete_scope");
   parent : access raptor.Visual_Flow_Form.Typ'Class;
   pragma Import(MSIL,parent,"parent");
   type Ref is access all Typ'Class;
   type Ref_addrof is access all Ref;
   type Ref_Arr is array(Natural range <>) of Ref;
   type Ref_Array is access all Ref_Arr;
   type Ref_Array_addrof is access all Ref_Array;
   function new_Runtime(
      This : Ref := null) return Ref;
   procedure Add_Node_To_Classes(
      node : access MSSyst.Windows.Forms.TreeNode.Typ'Class);
   procedure Add_Node_To_Front_Of_WatchBox(
      node : access MSSyst.Windows.Forms.TreeNode.Typ'Class);
   procedure Add_Node_To_Heap(
      node : access MSSyst.Windows.Forms.TreeNode.Typ'Class);
   procedure Add_Node_To_WatchBox(
      node : access MSSyst.Windows.Forms.TreeNode.Typ'Class);
   procedure Clear_Updated;
   procedure Clear_Variables;
   procedure consoleClear;
   procedure consoleMessage(
      s : access MSSyst.String.Typ'Class);
   procedure consoleWrite(
      s : access MSSyst.String.Typ'Class);
   procedure consoleWriteln(
      s : access MSSyst.String.Typ'Class);
   function Count_Symbols(
      start : access raptor.Oval.Typ'Class) return Integer;
   function createObject(
      class_name : access MSSyst.String.Typ'Class) return numbers.value;
   procedure createStaticVariables;
   procedure Decrease_Scope;
   procedure Delete_Scope_From_WatchBox;
   function End_Of_Input return Standard.Boolean;
   function Find_Method_Set_Running(
      methodname : access MSSyst.String.Typ'Class;
      num_params : Integer) return access raptor.Procedure_Chart.Typ'Class;
   function findClassesScope return Integer;
   function findHeapScope return Integer;
   function get2DArray(
      s : access MSSyst.String.Typ'Class) return MSIL_Types.Float64_Array_Array;
   function get2DArrayElement(
      s : access MSSyst.String.Typ'Class;
      index1 : Integer;
      index2 : Integer) return numbers.value;
   function get2DIntArray(
      s : access MSSyst.String.Typ'Class) return MSIL_Types.Int32_Array_Array;
   function get2DValueArray(
      s : access MSSyst.String.Typ'Class) return access Integer;
   function getArray(
      s : access MSSyst.String.Typ'Class) return MSIL_Types.Float64_Array;
   function getArrayElement(
      s : access MSSyst.String.Typ'Class;
      index : Integer) return numbers.value;
   function getArraySize(
      s : access MSSyst.String.Typ'Class) return Integer;
   function getContextObject return access MSSyst.Object.Typ'Class;
   function getIntArray(
      s : access MSSyst.String.Typ'Class) return MSIL_Types.Int32_Array;
   function getValueArray(
      s : access MSSyst.String.Typ'Class) return access Integer;
   function getVariable(
      s : access MSSyst.String.Typ'Class) return numbers.value;
   function getVariableContext(
      s : access MSSyst.String.Typ'Class) return access MSSyst.Object.Typ'Class;
   function getVariableContext1D(
      s : access MSSyst.String.Typ'Class;
      index : Integer) return access MSSyst.Object.Typ'Class;
   function getVariableContext2D(
      s : access MSSyst.String.Typ'Class;
      index1 : Integer;
      index2 : Integer) return access MSSyst.Object.Typ'Class;
   procedure Increase_Scope(
      s : access MSSyst.String.Typ'Class);
   procedure Init(
      p : access raptor.Visual_Flow_Form.Typ'Class;
      MC : access raptor.MasterConsole.Typ'Class;
      watch : access MSSyst.Windows.Forms.TreeView.Typ'Class);
   function is_2D_Array(
      s : access MSSyst.String.Typ'Class) return Standard.Boolean;
   function is_Character(
      s : access MSSyst.String.Typ'Class) return Standard.Boolean;
   function is_Scalar(
      s : access MSSyst.String.Typ'Class) return Standard.Boolean;
   function is_String(
      s : access MSSyst.String.Typ'Class) return Standard.Boolean;
   function is_Value(
      s : access MSSyst.String.Typ'Class) return Standard.Boolean;
   function is_Variable(
      s : access MSSyst.String.Typ'Class) return Standard.Boolean;
   function isArray(
      s : access MSSyst.String.Typ'Class) return Standard.Boolean;
   function isObjectOriented return Standard.Boolean;
   function promptDialog(
      s : access MSSyst.String.Typ'Class) return access MSSyst.String.Typ'Class;
   procedure Redirect_Input(
      filename : access MSSyst.String.Typ'Class);
   procedure Redirect_Input(
      yes_or_no : Integer);
   procedure Redirect_Output(
      yes_or_no : Integer);
   procedure Redirect_Output(
      filename : access MSSyst.String.Typ'Class);
   procedure Redirect_Output_Append(
      filename : access MSSyst.String.Typ'Class);
   procedure Redirect_Output_Append(
      yes_or_no : Integer);
   procedure Set_Running(
      sc : access raptor.Procedure_Chart.Typ'Class);
   procedure set2DArrayElement(
      s : access MSSyst.String.Typ'Class;
      index1 : Integer;
      index2 : Integer;
      f : numbers.value);
   procedure setArrayElement(
      s : access MSSyst.String.Typ'Class;
      index : Integer;
      f : numbers.value);
   procedure setContext(
      c : access MSSyst.Object.Typ'Class);
   procedure setVariable(
      s : access MSSyst.String.Typ'Class;
      f : numbers.value);
   procedure ShowConsole;
   procedure updateWatchBox;
private
   pragma Convention(MSIL,Typ);
   pragma MSIL_Constructor(new_Runtime);
   pragma Import(MSIL,Add_Node_To_Classes,"Add_Node_To_Classes");
   pragma Import(MSIL,Add_Node_To_Front_Of_WatchBox,"Add_Node_To_Front_Of_WatchBox");
   pragma Import(MSIL,Add_Node_To_Heap,"Add_Node_To_Heap");
   pragma Import(MSIL,Add_Node_To_WatchBox,"Add_Node_To_WatchBox");
   pragma Import(MSIL,Clear_Updated,"Clear_Updated");
   pragma Import(MSIL,Clear_Variables,"Clear_Variables");
   pragma Import(MSIL,consoleClear,"consoleClear");
   pragma Import(MSIL,consoleMessage,"consoleMessage");
   pragma Import(MSIL,consoleWrite,"consoleWrite");
   pragma Import(MSIL,consoleWriteln,"consoleWriteln");
   pragma Import(MSIL,Count_Symbols,"Count_Symbols");
   pragma Import(MSIL,createObject,"createObject");
   pragma Import(MSIL,createStaticVariables,"createStaticVariables");
   pragma Import(MSIL,Decrease_Scope,"Decrease_Scope");
   pragma Import(MSIL,Delete_Scope_From_WatchBox,"Delete_Scope_From_WatchBox");
   pragma Import(MSIL,End_Of_Input,"End_Of_Input");
   pragma Import(MSIL,Find_Method_Set_Running,"Find_Method_Set_Running");
   pragma Import(MSIL,findClassesScope,"findClassesScope");
   pragma Import(MSIL,findHeapScope,"findHeapScope");
   pragma Import(MSIL,get2DArray,"get2DArray");
   pragma Import(MSIL,get2DArrayElement,"get2DArrayElement");
   pragma Import(MSIL,get2DIntArray,"get2DIntArray");
   pragma Import(MSIL,get2DValueArray,"get2DValueArray");
   pragma Import(MSIL,getArray,"getArray");
   pragma Import(MSIL,getArrayElement,"getArrayElement");
   pragma Import(MSIL,getArraySize,"getArraySize");
   pragma Import(MSIL,getContextObject,"getContextObject");
   pragma Import(MSIL,getIntArray,"getIntArray");
   pragma Import(MSIL,getValueArray,"getValueArray");
   pragma Import(MSIL,getVariable,"getVariable");
   pragma Import(MSIL,getVariableContext,"getVariableContext");
   pragma Import(MSIL,getVariableContext1D,"getVariableContext1D");
   pragma Import(MSIL,getVariableContext2D,"getVariableContext2D");
   pragma Import(MSIL,Increase_Scope,"Increase_Scope");
   pragma Import(MSIL,Init,"Init");
   pragma Import(MSIL,is_2D_Array,"is_2D_Array");
   pragma Import(MSIL,is_Character,"is_Character");
   pragma Import(MSIL,is_Scalar,"is_Scalar");
   pragma Import(MSIL,is_String,"is_String");
   pragma Import(MSIL,is_Value,"is_Value");
   pragma Import(MSIL,is_Variable,"is_Variable");
   pragma Import(MSIL,isArray,"isArray");
   pragma Import(MSIL,isObjectOriented,"isObjectOriented");
   pragma Import(MSIL,promptDialog,"promptDialog");
   pragma Import(MSIL,Redirect_Input,"Redirect_Input");
   pragma Import(MSIL,Redirect_Output,"Redirect_Output");
   pragma Import(MSIL,Redirect_Output_Append,"Redirect_Output_Append");
   pragma Import(MSIL,Set_Running,"Set_Running");
   pragma Import(MSIL,set2DArrayElement,"set2DArrayElement");
   pragma Import(MSIL,setArrayElement,"setArrayElement");
   pragma Import(MSIL,setContext,"setContext");
   pragma Import(MSIL,setVariable,"setVariable");
   pragma Import(MSIL,ShowConsole,"ShowConsole");
   pragma Import(MSIL,updateWatchBox,"updateWatchBox");
end raptor.Runtime;
pragma Import(MSIL,raptor.Runtime,
   ".ver 4:0:5:2",
   "[raptor]raptor.Runtime");
