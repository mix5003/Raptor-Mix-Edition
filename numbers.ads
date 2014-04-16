with Ada.Numerics;
with Mssyst.String;
with MSSyst.Object;
package Numbers is
   procedure Set_Precision(I : in Integer);
   type Value is private;
   One    : constant Value;
   Pi     : constant Value;
   E      : constant Value;
   Two_Pi : constant Value;
   function "-"(L : Value) return Value;
   function "+" (
         L,
         R : Value )
     return Value;
   function "-" (
         L,
         R : Value )
     return Value;
   function "*" (
         L,
         R : Value )
     return Value;
   function "/" (
         L,
         R : Value )
     return Value;
   function "mod" (
         L,
         R : Value )
     return Value;
   function "rem" (
         L,
         R : Value )
     return Value;
   function "abs" (L : Value ) return Value;
   function "**" (
         L,
         R : Value )
     return Value;
   function Min (
         L,
         R : Value )
     return Value;
   function Max (
         L,
         R : Value )
     return Value;
   function Cos (
         X     : in     Value;
         Cycle : in     Value := Two_Pi )
     return Value;
   function Sin (
         X     : in     Value;
         Cycle : in     Value := Two_Pi )
     return Value;
   function Tan (
         X     : in     Value;
         Cycle : in     Value := Two_Pi )
     return Value;
   function Cot (
         X     : in     Value;
         Cycle : in     Value := Two_Pi )
     return Value;
   function Log (
         X    : in     Value;
         Base : in     Value := E )
     return Value;
   function Sqrt (
         X : Value )
     return Value;
   function Arccos (
         X     : in     Value;
         Cycle : in     Value := Two_Pi )
     return Value;
   function Arcsin (
         X     : in     Value;
         Cycle : in     Value := Two_Pi )
     return Value;
   function Arctan (
         Y     : in     Value;
         X     : in     Value := One;
         Cycle : in     Value := Two_Pi )
     return Value;
   function ArcCot (
         Y     : in     Value;
         X     : in     Value := One;
         Cycle : in     Value := Two_Pi )
      return Value;
   function Sinh (X : in Value) return Value;
   function Cosh (X : in Value) return Value;
   function Tanh (X : in Value) return Value;
   function Coth (X : in Value) return Value;
   function ArcSinh (X : in Value) return Value;
   function ArcCosh (X : in Value) return Value;
   function ArcTanh (X : in Value) return Value;
   function ArcCoth (X : in Value) return Value;
   function Floor (X : in Value) return Value;
   function Ceiling (X : in Value) return Value;
   -- return if this number looks like an integer
   function Is_Integer(X : Value) return Boolean;
   function Is_Long_Long_Integer(X : Value) return Boolean;
   function Is_String(X : Value) return Boolean;
   function Is_Character(X : Value) return Boolean;
   function Is_Object(X : Value) return Boolean;
   function Is_Ref_1D(X : Value) return Boolean;
   function Is_Ref_2D(X : Value) return Boolean;

   -- return the value of a number
   function Long_Float_Of(X : Value) return Long_Float;
   function Integer_Of(X : Value) return Integer;
   function Long_Long_Integer_Of(X : Value) return Long_Long_Integer;
   function String_Of(X : Value) return MSSyst.String.Ref;
   function Object_Of(X : Value) return MSSyst.Object.Ref;
   function Character_Of(X : Value) return Wide_Character;
   -- convert a string to a value
   -- warning (mcc: 11/27/03) RAPTOR's compile is dependent on this
   -- overloading staying in this order
   function Make_Value(S : in String) return Value;
   function Make_Value(F : in Long_Float) return Value;
   function Make_Value(I : in Integer) return Value;
   function Make_Value(C : in Wide_Character) return Value;
   function Make_String_Value(S : in MSSyst.String.Ref) return Value;
   function Make_Number_Value(S : in MSSyst.String.Ref) return Value;
   function Make_1d_ref(O : in MSSyst.Object.Ref) return Value;
   function Make_2d_ref(O : in MSSyst.Object.Ref) return Value;
   function Make_Object_Value(O : in MSSyst.Object.Ref) return Value;
   -- string functions
   function Length_Of(V : Value) return Natural;
   -- used for output of a value
   function Integer_Image(I : in Integer) return String;
   function Long_Float_Image(F : in Long_Float) return String;
   function Long_Float_Full_Image(F : in Long_Float) return String;
   function Number_String(X : Value) return String;
   -- used for compiled output
   function MSString_Image(X : Value) return MSSyst.String.Ref;
   -- used for view variables window
   function MSString_View_Image(X : Value) return MSSyst.String.Ref;
   -- relational operators
   function ">="(L,R : in Value) return Boolean;
   function ">"(L,R : in Value) return Boolean;
   function "<"(L,R : in Value) return Boolean;
   function "<="(L,R : in Value) return Boolean;
   function "="(L,R : in Value) return Boolean;
   procedure Copy(From : in Value; To : out Value);
private
   type Value_Kind is (Number_Kind, String_Kind, Character_Kind, Object_Kind,
      Ref_1D, Ref_2D);
   type Value is record
      V : Long_Float;
      S : Mssyst.String.Ref;
      C : Wide_Character;
      Object : MSSyst.Object.Ref;
      Kind : Value_Kind;
   end record;
   Zero    : constant Value := (V => 0.0, C=>' ', S=> null, Kind => Number_Kind, Object => null);
   One    : constant Value := (V => 1.0, C=>' ', S=> null, Kind => Number_Kind, Object => null);
   Pi     : constant Value := (V => Ada.Numerics.Pi, C=>' ', S=> null, Kind => Number_Kind, Object => null);
   E      : constant Value := (V => Ada.Numerics.E, C=>' ', S=> null, Kind => Number_Kind, Object => null);
   Two_Pi : constant Value := (V => 2.0 * Ada.Numerics.Pi, C=>' ', S=> null, Kind => Number_Kind, Object => null);
   Null_Ptr : constant Value := (V => 0.0, S=>null, C=> ' ', Object => null, Kind => Object_Kind);
end Numbers;
