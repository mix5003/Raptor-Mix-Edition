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
limited with dotnetgraphlibrary.Color_Type;
with dotnetgraphlibrary.dotnetgraph;
package dotnetgraphlibrary.dotnetgraph.Flood_Fill_Delegate_Type is
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
   function new_Flood_Fill_Delegate_Type(
      This : Ref := null;
      object : access MSSyst.Object.Typ'Class;
      method : MSIL_Types.native_int) return Ref;
   function BeginInvoke(
      This : access Typ;
      x : Integer;
      y : Integer;
      hue : dotnetgraphlibrary.Color_Type.Valuetype;
      callback : access MSSyst.AsyncCallback.Typ'Class;
      object : access MSSyst.Object.Typ'Class) return access MSSyst.IAsyncResult.Typ'Class;
   procedure EndInvoke(
      This : access Typ;
      result : access MSSyst.IAsyncResult.Typ'Class);
   procedure Invoke(
      This : access Typ;
      x : Integer;
      y : Integer;
      hue : dotnetgraphlibrary.Color_Type.Valuetype);
private
   pragma Convention(MSIL,Typ);
   pragma MSIL_Constructor(new_Flood_Fill_Delegate_Type);
   pragma Import(MSIL,BeginInvoke,"BeginInvoke");
   pragma Import(MSIL,EndInvoke,"EndInvoke");
   pragma Import(MSIL,Invoke,"Invoke");
end dotnetgraphlibrary.dotnetgraph.Flood_Fill_Delegate_Type;
pragma Import(MSIL,dotnetgraphlibrary.dotnetgraph.Flood_Fill_Delegate_Type,
   ".ver 2:0:0:0",
   "[dotnetgraph]dotnetgraphlibrary.dotnetgraph/Flood_Fill_Delegate_Type");
