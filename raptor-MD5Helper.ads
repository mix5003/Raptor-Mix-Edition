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
package raptor.MD5Helper is
   type Typ is new MSSyst.Object.Typ   with record
      null;
   end record;
   type Ref is access all Typ'Class;
   type Ref_addrof is access all Ref;
   type Ref_Arr is array(Natural range <>) of Ref;
   type Ref_Array is access all Ref_Arr;
   type Ref_Array_addrof is access all Ref_Array;
   function new_MD5Helper(
      This : Ref := null) return Ref;
   function CheckValueAgainstHash(
      md5Hash : access MSSyst.String.Typ'Class;
      inputValue : access MSSyst.String.Typ'Class) return Standard.Boolean;
   function ComputeHash(
      filename : access MSSyst.String.Typ'Class) return access MSSyst.String.Typ'Class;
private
   pragma Convention(MSIL,Typ);
   pragma MSIL_Constructor(new_MD5Helper);
   pragma Import(MSIL,CheckValueAgainstHash,"CheckValueAgainstHash");
   pragma Import(MSIL,ComputeHash,"ComputeHash");
end raptor.MD5Helper;
pragma Import(MSIL,raptor.MD5Helper,
   ".ver 4:0:5:2",
   "[raptor]raptor.MD5Helper");
