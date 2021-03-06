-- Autogenerated by MSIL2Ada v. 2
-- By: Martin C. Carlisle
--     Department of Computer Science
--     US Air Force Academy
--     carlislem@acm.org
with MSSyst.Object;
with MSIL_Types;
use MSIL_Types;
limited with NClass.Core.ClassType;
limited with NClass.Core.Method;
with NClass.Core.RAPTORUpdater;
limited with MSSyst.String;
limited with MSSyst.Type_k;
limited with raptor.Visual_Flow_Form;
package raptor.UMLupdater is
   type Typ is new MSSyst.Object.Typ
         and NClass.Core.RAPTORUpdater.Typ
   with record
      null;
   end record;
   type Ref is access all Typ'Class;
   type Ref_addrof is access all Ref;
   type Ref_Arr is array(Natural range <>) of Ref;
   type Ref_Array is access all Ref_Arr;
   type Ref_Array_addrof is access all Ref_Array;
   function new_UMLupdater(
      This : Ref := null;
      form : access raptor.Visual_Flow_Form.Typ'Class) return Ref;
   procedure changeParameters(
      This : access Typ;
      theClass : access MSSyst.Object.Typ'Class;
      subchart : access MSSyst.Object.Typ'Class;
      num_params : Integer;
      param_names : access MSSyst.String.Ref_arr;
      param_is_input : MSIL_Types.Bool_Arr;
      param_is_output : MSIL_Types.Bool_Arr);
   function createClass(
      This : access Typ;
      name : access MSSyst.String.Typ'Class;
      ct : access NClass.Core.ClassType.Typ'Class) return access MSSyst.Object.Typ'Class;
   function createMethod(
      This : access Typ;
      theClass : access MSSyst.Object.Typ'Class;
      name : access MSSyst.String.Typ'Class;
      method : access NClass.Core.Method.Typ'Class) return access MSSyst.Object.Typ'Class;
   procedure deleteClass(
      This : access Typ;
      theClass : access MSSyst.Object.Typ'Class);
   procedure deleteMethod(
      This : access Typ;
      theClass : access MSSyst.Object.Typ'Class;
      subchart : access MSSyst.Object.Typ'Class);
   function makeAbstract(
      This : access Typ;
      theClass : access MSSyst.Object.Typ'Class;
      subchart : access MSSyst.Object.Typ'Class) return Standard.Boolean;
   procedure renameClass(
      This : access Typ;
      theClass : access MSSyst.Object.Typ'Class;
      name : access MSSyst.String.Typ'Class);
   procedure renameMethod(
      This : access Typ;
      theClass : access MSSyst.Object.Typ'Class;
      subchart : access MSSyst.Object.Typ'Class;
      name : access MSSyst.String.Typ'Class);
private
   pragma Convention(MSIL,Typ);
   pragma MSIL_Constructor(new_UMLupdater);
   pragma Import(MSIL,changeParameters,"changeParameters");
   pragma Import(MSIL,createClass,"createClass");
   pragma Import(MSIL,createMethod,"createMethod");
   pragma Import(MSIL,deleteClass,"deleteClass");
   pragma Import(MSIL,deleteMethod,"deleteMethod");
   pragma Import(MSIL,makeAbstract,"makeAbstract");
   pragma Import(MSIL,renameClass,"renameClass");
   pragma Import(MSIL,renameMethod,"renameMethod");
end raptor.UMLupdater;
pragma Import(MSIL,raptor.UMLupdater,
   ".ver 4:0:5:2",
   "[raptor]raptor.UMLupdater");
