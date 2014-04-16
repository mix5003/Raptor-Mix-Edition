with Mssyst.String;
with MSSyst.Object;
with generate_interface;
package Generate_Interface_Oo is
   type Typ is interface and generate_interface.typ;
   pragma Convention(MSIL,Typ);
   type Generator is access all Typ'Class;
   function create_object(This : in Typ;
      Class_Name : in MSSyst.String.Ref) return mssyst.object.ref is abstract;
   function emit_call_oo_method(
      This : in Typ;
      Name : in Mssyst.String.Ref;
      Is_Function : in Boolean) return Mssyst.Object.Ref is abstract;
   procedure Emit_Dereference(This : in Typ) is abstract;
   procedure Start_Return(This : in Typ) is abstract;
   procedure End_Return(This : in Typ) is abstract;
end Generate_Interface_Oo;

