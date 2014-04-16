-- Autogenerated by MSIL2Ada v. 2
-- By: Martin C. Carlisle
--     Department of Computer Science
--     US Air Force Academy
--     carlislem@acm.org
with MSSyst.Object;
with MSIL_Types;
use MSIL_Types;
limited with MSSyst.String;
limited with MSSyst.Threading.ThreadStart;
limited with MSSyst.Type_k;
package raptor.Autoupdate is
   type Typ is new MSSyst.Object.Typ   with record
      null;
   end record;
   question_delegate : access MSSyst.Threading.ThreadStart.Typ'Class;
   pragma Import(MSIL,question_delegate,"question_delegate");
   type Ref is access all Typ'Class;
   type Ref_addrof is access all Ref;
   type Ref_Arr is array(Natural range <>) of Ref;
   type Ref_Array is access all Ref_Arr;
   type Ref_Array_addrof is access all Ref_Array;
   function new_Autoupdate(
      This : Ref := null) return Ref;
   procedure Ask_The_Question;
   function Autoupdate_Requested return Standard.Boolean;
private
   pragma Convention(MSIL,Typ);
   pragma MSIL_Constructor(new_Autoupdate);
   pragma Import(MSIL,Ask_The_Question,"Ask_The_Question");
   pragma Import(MSIL,Autoupdate_Requested,"Autoupdate_Requested");
end raptor.Autoupdate;
pragma Import(MSIL,raptor.Autoupdate,
   ".ver 4:0:5:2",
   "[raptor]raptor.Autoupdate");
