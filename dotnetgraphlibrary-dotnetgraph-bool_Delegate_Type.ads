-- Autogenerated by MSIL2Ada v. 2
-- By: Martin C. Carlisle
--     Department of Computer Science
--     US Air Force Academy
--     carlislem@acm.org
with MSSyst.Object;
with MSIL_Types;
use MSIL_Types;
limited with MSSyst.AsyncCallback;
with MSSyst.Delegate;
limited with MSSyst.IAsyncResult;
with MSSyst.ICloneable;
with MSSyst.MulticastDelegate;
limited with MSSyst.Reflection.MethodInfo;
with MSSyst.Runtime.Serialization.ISerializable;
limited with MSSyst.Runtime.Serialization.SerializationInfo;
limited with MSSyst.Runtime.Serialization.StreamingContext;
limited with MSSyst.String;
limited with MSSyst.Type_k;
with dotnetgraphlibrary.dotnetgraph;
package dotnetgraphlibrary.dotnetgraph.bool_Delegate_Type is
   type Typ is new MSSyst.MulticastDelegate.Typ
         and MSSyst.ICloneable.Typ
         and MSSyst.Runtime.Serialization.ISerializable.Typ
   with record
      null;
   end record;
   type Ref is access all Typ'Class;
   type Ref_addrof is access all Ref;
   type Ref_Arr is array(Natural range <>) of Ref;
   type Ref_Array is access all Ref_Arr;
   type Ref_Array_addrof is access all Ref_Array;
   function new_bool_Delegate_Type(
      This : Ref := null;
      object : access MSSyst.Object.Typ'Class;
      method : MSIL_Types.native_int) return Ref;
   function BeginInvoke(
      This : access Typ;
      callback : access MSSyst.AsyncCallback.Typ'Class;
      object : access MSSyst.Object.Typ'Class) return access MSSyst.IAsyncResult.Typ'Class;
   function EndInvoke(
      This : access Typ;
      result : access MSSyst.IAsyncResult.Typ'Class) return Standard.Boolean;
   function Invoke(
      This : access Typ) return Standard.Boolean;
private
   pragma Convention(MSIL,Typ);
   pragma MSIL_Constructor(new_bool_Delegate_Type);
   pragma Import(MSIL,BeginInvoke,"BeginInvoke");
   pragma Import(MSIL,EndInvoke,"EndInvoke");
   pragma Import(MSIL,Invoke,"Invoke");
end dotnetgraphlibrary.dotnetgraph.bool_Delegate_Type;
pragma Import(MSIL,dotnetgraphlibrary.dotnetgraph.bool_Delegate_Type,
   ".ver 2:0:0:0",
   "[dotnetgraph]dotnetgraphlibrary.dotnetgraph/bool_Delegate_Type");
