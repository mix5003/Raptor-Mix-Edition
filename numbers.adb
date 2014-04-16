with Ada.Numerics.Long_Elementary_Functions;
use Ada.Numerics.Long_Elementary_Functions;
with Ada.Exceptions;
use Ada.Exceptions;
with Mssyst.String;
use Mssyst.String;
with Ada.Strings.Fixed;
use Ada.Strings.Fixed;
with Ada.Long_Float_Text_Io;
with Ada.Strings.Maps;
with ada.characters.conversions;

package body Numbers is
   Precision_Set : Boolean := False;
   Precision : Integer := 4;

   procedure Set_Precision(I : in Integer) is
   begin
      if I<0 then
         Precision_Set := False;
      else
         Precision_Set := True;
         Precision := I;
      end if;
   end Set_Precision;

   function Integer_Image(I : in Integer) return String is
      S : constant String := Integer'Image(I);
   begin
      return S(Index_Non_Blank(S)..S'Last);
   end Integer_Image;

   function Long_Float_Image(F : in Long_Float) return String is
      S : String(1..600);
      G : Long_Float := F;
      Aft : Natural := 4;
   begin
      if Precision_Set then
         Aft := Precision;
      else
         loop
            exit when Aft=14;
            g := g * 10.0;
            exit when abs(G)>1.0;
            Aft := Aft + 1;
         end loop;
      end if;
      Ada.Long_Float_Text_Io.Put(
         Item => F,
         Aft  => Aft,
         Exp  => 0,
         To   => S);
      return S(Index_Non_Blank(S)..S'Last);
   end Long_Float_Image;

   function Long_Float_Full_Image(F : in Long_Float) return String is
      S : String(1..600);
      Aft : constant Natural := 10;
   begin

      Ada.Long_Float_Text_Io.Put(
         Item => F,
         Aft  => Aft,
         Exp  => 0,
         To   => S);
      return Ada.Strings.Fixed.Trim(
         Source => S,
         Left   => Ada.Strings.Maps.To_Set(' '),
         Right  => Ada.Strings.Maps.To_set('0'));

   end Long_Float_Full_Image;

   function Number_String(X : Value) return String is
   begin
      if Is_Integer(X) and not Precision_Set then
         return Integer_Image(Integer_Of(X));
      else
         return Long_Float_Image(X.V);
      end if;
   end Number_String;


   function Object_Image(X : Value) return MSSyst.String.Ref is
   begin
      return +("[" & integer_image(mssyst.object.GetHashCode(x.object)) & "]");
   end Object_Image;

   function MSString_view_Image(X : Value) return MSSyst.String.Ref is
   begin
      if Is_String(X) then
         return +('"' & (+X.S) & '"');
      elsif X.Kind = Character_Kind then
         return
            mssyst.string.ref(Mssyst.String.Concat(
               arg0 => Mssyst.String.New_String(C => ''', Count => 1),
               arg1 => Mssyst.String.New_String(C => X.C, Count => 1),
               Arg2 => MSSyst.String.New_String(C => ''', Count => 1)));
      elsif X.Kind = Object_Kind or X.Kind = Ref_1D or X.Kind = Ref_2D then
         return object_image(x);
      else
         return +Number_String(X);
      end if;
   end MSString_view_Image;

   function MSString_Image(X : Value) return MSSyst.String.Ref is
   begin
      if Is_String(X) then
         return X.S;
      elsif X.Kind = Character_Kind then
         return
            mssyst.string.ref(Mssyst.String.New_String(C => X.C, Count => 1));
      else
         return +Number_String(X);
      end if;
   end MSString_Image;


   Close_Enough : constant := 0.0000000001;
   function Is_Integer(X : Value) return Boolean is
   begin
      if X.Kind = Number_Kind then
         return abs(Long_Float(Integer(X.V))-X.V) < Close_Enough;
      else
         return False;
      end if;
   exception when others=>
      return false;
   end Is_Integer;
   function Is_Long_Long_Integer(X : Value) return Boolean is
   begin
      if X.Kind = Number_Kind then
         return abs(Long_Float(Long_Long_Integer(X.V))-X.V) < Close_Enough;
      else
         return False;
      end if;
   exception when others=>
      return false;
   end Is_Long_Long_Integer;
   function Is_Number(X : Value) return Boolean is
   begin
      return X.Kind = Number_Kind;
   end Is_Number;
   function Is_String(X : Value) return Boolean is
   begin
      return X.Kind = String_Kind;
   end Is_String;
   function Is_Character(X : Value) return Boolean is
   begin
      return X.Kind = Character_Kind;
   end Is_Character;
   function Is_Object(X : Value) return Boolean is
   begin
      return X.Kind = Object_Kind;
   end Is_Object;
   function Is_Ref_1D(X : Value) return Boolean is
   begin
      return X.Kind = Ref_1D;
   end Is_Ref_1D;
   function Is_Ref_2D(X : Value) return Boolean is
   begin
      return X.Kind = Ref_2D;
   end Is_Ref_2D;
   function Long_Float_Of(X : Value) return Long_Float is
   begin
      case X.Kind is
         when Number_Kind =>
            return X.V;
         when others =>
            Raise_Exception(Ada.Numerics.Argument_Error'Identity,"variable is not a number");
      end case;
   end Long_Float_Of;

   function Long_Long_Integer_Of(X : Value) return Long_Long_Integer is
   begin
      case X.Kind is
         when Number_Kind =>
            return Long_Long_Integer(X.V);
         when others =>
            Raise_Exception(Ada.Numerics.Argument_Error'Identity,"variable is not a number");
      end case;
   end Long_Long_Integer_Of;

   function Integer_Of(X : Value) return Integer is
      z : integer;
   begin
      case X.Kind is
         when Number_Kind =>
            Z := Integer(X.V);
            return z;
         when Character_Kind =>
            return Wide_Character'Pos(X.C);
         when others =>
            Raise_Exception(Ada.Numerics.Argument_Error'Identity,"variable is not a number");
      end case;
   end Integer_Of;

   function Object_Of(X : Value) return MSSyst.Object.Ref is
   begin
      case X.Kind is
         when Object_Kind|Ref_1D|Ref_2D =>
            return X.Object;
         when others =>
            Raise_Exception(Ada.Numerics.Argument_Error'Identity,"Value is not an object reference");
      end case;
   end Object_Of;

   function String_Of(X : Value) return MSSyst.String.Ref is
   begin
      case X.Kind is
         when String_Kind =>
            return X.S;
         when others =>
            Raise_Exception(Ada.Numerics.Argument_Error'Identity,"Not a string");
      end case;
   end String_Of;

   function Character_Of(X : Value) return Wide_Character is
   begin
      case X.Kind is
         when Character_Kind =>
            return X.C;
         when others =>
            Raise_Exception(Ada.Numerics.Argument_Error'Identity,"Not a character");
      end case;
   end Character_Of;

   function Make_Object_Value(O : in MSSyst.Object.Ref) return Value is
   begin
      return Value'(V => 0.0, C=> ' ', Object => O, Kind => Object_Kind, S=>null);
   end Make_Object_Value;

   function Make_1d_ref(O : in MSSyst.Object.Ref) return Value is
   begin
      return Value'(V => 0.0, C=> ' ', Object => O, Kind => Ref_1D, S=>null);
   end Make_1D_Ref;


   function Make_2d_ref(O : in MSSyst.Object.Ref) return Value is
   begin
      return Value'(V => 0.0, C=> ' ', Object => O, Kind => Ref_2D, S=>null);
   end Make_2D_Ref;



   function Make_String_Value(S : in MSSyst.String.Ref) return Value is
   begin
      return Value'(V => 0.0, C=> ' ', S=> S, Kind => String_Kind, Object=>null);
   end Make_String_Value;

   function Make_Number_Value(S : in MSSyst.String.Ref) return Value is
   begin
      return Make_Value(+S);
   end Make_Number_Value;

   function Make_Value(S : in String) return Value is
   begin
      return Value'(V => Long_Float'Value(S), C => ' ', S=>null, Kind => Number_Kind, Object=>null);
   exception when others =>
         return Value'(V => 0.0, C=> ' ', S=> +S, Kind => String_Kind, Object=>null);
   end Make_Value;

   function Make_Value(F : in Long_Float) return Value is
   begin
      return Value'(V => F, C=> ' ', S=>null, Kind => Number_Kind, Object=>null);
   end Make_Value;

   function Make_Value(I : in Integer) return Value is
   begin
      return Value'(V => Long_Float(I), C=> ' ', S=>null, Kind => Number_Kind, Object=>null);
   end Make_Value;

   function Make_Value(C : in Wide_Character) return Value is
   begin
      return Value'(V => 0.0, C => C, S=>null, Kind => Character_Kind, Object=>null);
   end Make_Value;

   function "-"(L : Value) return Value is
   begin
      case L.Kind is
         when Number_Kind =>
            return Value'(V => -L.V, C=> ' ', S=>null, Kind => Number_Kind, Object=>null);
         when others =>
            Raise_Exception(Ada.Numerics.Argument_Error'Identity,
               +L.S & " can not be negated");
      end case;
   end "-";

   ---------
   -- "*" --
   ---------

   function "*"
     (L,
         R : Value)
      return Value
   is
   begin
      if L.Kind = Number_Kind and R.Kind = Number_Kind then
         return Value'(V => L.V*R.V, C=> ' ', S=> null, Kind => Number_Kind, Object=>null);
      elsif L.Kind = String_Kind then
         Raise_Exception(Ada.Numerics.Argument_Error'Identity,
               "Can't multiply string: " & (+L.S));
      elsif R.Kind = String_Kind then
         Raise_Exception(Ada.Numerics.Argument_Error'Identity,
            "Can't multiply string: " & (+R.S));
      elsif L.Kind = Character_Kind then
         Raise_Exception(Ada.Numerics.Argument_Error'Identity,
               "Can't multiply character: " & ada.Characters.conversions.To_Character(L.C));
      elsif R.Kind = Character_Kind then
         Raise_Exception(Ada.Numerics.Argument_Error'Identity,
            "Can't multiply string: " & ada.characters.conversions.To_Character(R.C));
      else
         Raise_Exception(Ada.Numerics.Argument_Error'Identity,
            "Bad arguments to *");
      end if;
   end "*";

   ---------
   -- Max --
   ---------

   function Max
      (L,
         R : Value)
      return Value
   is
   begin
      if L.Kind = Number_Kind and R.Kind = Number_Kind then
         return Value'(V => Long_Float'Max(L.V,R.V), C=> ' ', S=> null, Kind => Number_Kind, Object=>null);
      else
         Raise_Exception(Ada.Numerics.Argument_Error'Identity,
            "Bad arguments to Max");
      end if;
   end Max;

   ---------
   -- Min --
   ---------

   function Min
      (L,
         R : Value)
      return Value
   is
   begin
      if L.Kind = Number_Kind and R.Kind = Number_Kind then
         return Value'(V => Long_Float'Min(L.V,R.V), C=> ' ', S=> null, Kind => Number_Kind, Object=>null);
      else
         Raise_Exception(Ada.Numerics.Argument_Error'Identity,
            "Bad arguments to Min");
      end if;
   end Min;

   ----------
   -- "**" --
   ----------

   function "**"
     (L,
         R : Value)
      return Value
   is
   begin
      if L.Kind=Number_Kind and R.Kind=Number_Kind then
            begin
               if Is_Integer(R) then
                  return Value'(V => L.V**Integer(R.V), C=> ' ', S=> null, Kind => Number_Kind, Object=>null);
               else
                  return Value'(V => L.V**R.V, C => ' ', S=> null, Kind => Number_Kind, Object=>null);
               end if;
            exception when Ada.Numerics.Argument_Error =>
                  Raise_Exception(Ada.Numerics.Argument_Error'Identity,
                     "Can't raise a negative number to a fractional power");
            end;
      elsif L.Kind=String_Kind then
            Raise_Exception(Ada.Numerics.Argument_Error'Identity,
               "Can't exponentiate string: " & (+L.S));
      elsif R.Kind=String_Kind then
            Raise_Exception(Ada.Numerics.Argument_Error'Identity,
               "Can't raise to string: " & (+R.S));
      else
         Raise_Exception(Ada.Numerics.Argument_Error'Identity,
            "Bad arguments to **");
      end if;
   end "**";

   ---------
   -- "+" --
   ---------

   function "+"
     (L,
         R : Value)
      return Value
   is
   begin
      if L.Kind = Number_Kind and R.Kind = Number_Kind then
         return Value'(V => L.V+R.V, C=> ' ', S=> null, Kind => Number_Kind, Object=>null);
      elsif L.Kind = Character_Kind and R.Kind = Number_Kind then
         return Value'(V => 0.0, C=> Wide_Character'Val(Wide_Character'Pos(L.C)+
            Integer_Of(R)), S=> null, Kind => Character_Kind, Object=>null);
      elsif L.Kind = Number_Kind and R.Kind = Character_Kind then
         return Value'(V => 0.0, C=> Wide_Character'Val(Wide_Character'Pos(R.C)+
            Integer_Of(L)), S=> null, Kind => Character_Kind, Object=>null);
      elsif L.Kind = String_Kind and R.Kind = Character_Kind then
         return Value'(V => 0.0, C=> ' ', S=> Mssyst.String.Ref(
            MSSyst.String.Concat(L.S,Mssyst.String.New_String(C => R.C, Count => 1))),
            Kind => String_Kind, Object=>null);
      elsif L.Kind = Character_Kind and R.Kind = String_Kind then
         return Value'(V => 0.0, C=> ' ', S=> Mssyst.String.Ref(
            MSSyst.String.Concat(Mssyst.String.New_String(C => L.C, Count => 1),R.S)),
            Kind => String_Kind, Object=>null);
      elsif L.Kind = String_Kind and R.Kind = Number_Kind then
         return Value'(V => 0.0, C=> ' ', S=> Mssyst.String.Ref(
            MSSyst.String.Concat(L.S,+Number_String(R))),
            Kind => String_Kind, Object=>null);
      elsif L.Kind = Number_Kind and R.Kind = String_Kind then
         return Value'(V => 0.0, C=> ' ', S=> Mssyst.String.Ref(
            Mssyst.String.Concat(+Number_String(L),R.S)),
            Kind => String_Kind, Object=>null);
      elsif L.Kind = String_Kind and R.Kind = String_Kind then
         return Value'(V => 0.0, C=> ' ', S=> Mssyst.String.Ref(
            MSSyst.String.Concat(L.S,R.S)), Kind => String_Kind, Object=>null);
      else
         Raise_Exception(Ada.Numerics.Argument_Error'Identity,
            "Unknown parameter for +");
      end if;
   end "+";

   ---------
   -- "-" --
   ---------

   function "-"
     (L,
         R : Value)
      return Value
   is
   begin
      if L.Kind = Number_Kind and R.Kind = Number_Kind then
         return Value'(V => L.V-R.V, C=> ' ', S=> null, Kind => Number_Kind, Object=>null);
      elsif L.Kind = Character_Kind and R.Kind = Number_Kind then
         return Value'(V => 0.0, C=> Wide_Character'Val(Wide_Character'Pos(L.C)-
            Integer_Of(R)), S=> null, Kind => Character_Kind, Object=>null);
      elsif L.Kind = Number_Kind and R.Kind = Number_Kind then
         return Value'(V => 0.0, C=> Wide_Character'Val(Integer_Of(L) -
               Wide_Character'Pos(R.C)), S=> null, Kind => Character_Kind, Object=>null);
      elsif L.Kind = Character_Kind and R.Kind = Character_Kind then
         return Value'(V => Long_Float(Wide_Character'Pos(L.C)-
            Wide_Character'Pos(R.C)), C=> ' ', S => null, Kind=> Number_Kind, Object=>null);
      else
         Raise_Exception(Ada.Numerics.Argument_Error'Identity,
            "Can't mix number and string");
      end if;
   end "-";

   ---------
   -- "/" --
   ---------

   function "/"
     (L,
         R : Value)
      return Value
   is
   begin
      if L.Kind = Number_Kind and R.Kind = Number_Kind then
            if R.V=0.0 then
               Raise_Exception(Ada.Numerics.Argument_Error'Identity,
                  "Can't divide by zero.");
            end if;
            return Value'(V => L.V/R.V, C=> ' ', S=> null, Kind => Number_Kind, Object=>null);
      else
         Raise_Exception(Ada.Numerics.Argument_Error'Identity,
            "Can't mix number and string");
      end if;
   end "/";

   -----------
   -- "abs" --
   -----------

   function "abs" (L : Value) return Value is
   begin
      case L.Kind is
         when Number_Kind =>
            return Value'(V => abs(L.V), C=> ' ', S=> null, Kind => Number_Kind, Object=>null);
         when String_Kind =>
            Raise_Exception(Constraint_Error'Identity,
               "Can't compute abs of: " & (+L.S));
         when Object_Kind|Ref_1D|Ref_2D =>
            Raise_Exception(Constraint_Error'Identity,
               "Can't compute abs of object");
         when Character_Kind =>
            Raise_Exception(Constraint_Error'Identity,
               "Can't compute abs of: " & ada.Characters.conversions.To_Character(L.C));
      end case;
   end "abs";

   -----------
   -- "mod" --
   -----------

   function "mod"
     (L,
         R : Value)
      return Value
   is
      Quotient : Long_Float;
      Answer   : Long_Float;
   begin
      if L.Kind = Number_Kind and R.Kind = Number_Kind then
         if R.V=0.0 then
            Raise_Exception(Ada.Numerics.Argument_Error'Identity,
               "Right hand side of mod must not be zero.");
         end if;
         Quotient := Long_Float'Floor(L.V/R.V);
         Answer := L.V-Quotient*R.V;
         return Value'(V => Answer, C=> ' ', S => null, Kind => Number_Kind, Object=>null);
      else
         Raise_Exception(Ada.Numerics.Argument_Error'Identity,
            "Can't mix number and string");
      end if;
   end "mod";

   -----------
   -- "rem" --
   -----------

   function "rem"
     (L,
         R : Value)
      return Value
   is
      Quotient : Long_Float;
      Answer   : Long_Float;
   begin
      if L.Kind = Number_Kind and R.Kind = Number_Kind then
         if R.V=0.0 then
            Raise_Exception(Ada.Numerics.Argument_Error'Identity,
               "Right hand side of rem must not be zero.");
         end if;
         Quotient := Long_Float'Floor(L.V/R.V);
         Answer := L.V-Quotient*R.V;
         -- if the signs differ and the answer isn't zero, then mod isn't the same as rem
         -- in this case A rem B = (A mod B) - B
         if R.V*L.V<0.0 and abs(Answer)>Close_Enough then
            Answer := Answer - R.V;
         end if;
         return Value'(V => Answer, C=> ' ', S => null, Kind => Number_Kind, Object=>null);
      else
         Raise_Exception(Ada.Numerics.Argument_Error'Identity,
            "Can't mix number and string");
      end if;
   end "rem";

   ------------
   -- Arccos --
   ------------

   function Arccos
     (X     : in     Value;
      Cycle : in     Value := Two_Pi)
      return Value
   is
   begin
      if X.Kind = Number_Kind and Cycle.Kind = Number_Kind then
            return Value'(V => Arccos (X.V, Cycle.V), C=> ' ', S=> null, Kind => Number_Kind, Object=>null);
      else
         Raise_Exception(Ada.Numerics.Argument_Error'Identity,
            "Can't mix number and string");
      end if;
   exception when Ada.Numerics.Argument_Error =>
      Raise_Exception(Ada.Numerics.Argument_Error'Identity,
         "Bad parameters to arccos");
   end Arccos;

   -------------
   -- ArcCosh --
   -------------

   function ArcCosh (X : in Value) return Value is
   begin
      case X.Kind is
         when Number_Kind =>
            return Value'(V => ArcCosh (X.V), C=> ' ', S=> null, Kind => Number_Kind, Object=>null);
         when Object_Kind|Ref_1D|Ref_2D =>
            Raise_Exception(Constraint_Error'Identity,
               "Can't compute arccosh of object");
         when String_Kind =>
            Raise_Exception(Constraint_Error'Identity,
               "Can't compute arccosh of: " & (+X.S));
         when Character_Kind =>
            Raise_Exception(Constraint_Error'Identity,
               "Can't compute arccosh of: " & ada.Characters.conversions.To_Character(X.C));
      end case;
   exception when Ada.Numerics.Argument_Error =>
      Raise_Exception(Ada.Numerics.Argument_Error'Identity,
         "Bad parameters to arccosh");
   end ArcCosh;

   ------------
   -- ArcCot --
   ------------

   function ArcCot
     (Y     : in     Value;
      X     : in     Value := One;
      Cycle : in     Value := Two_Pi)
      return Value
   is
   begin
      if Y.Kind = Number_Kind and X.Kind = Number_Kind and Cycle.Kind = Number_Kind then
            return Value'(V => ArcCot (Y.V, X.V, Cycle.V), C=> ' ', S=> null, Kind => Number_Kind, Object=>null);
      else
         Raise_Exception(Constraint_Error'Identity,
            "Can't mix number and string");
      end if;
   exception when Ada.Numerics.Argument_Error =>
      Raise_Exception(Ada.Numerics.Argument_Error'Identity,
         "Bad parameters to arccot");
   end ArcCot;

   -------------
   -- ArcCoth --
   -------------

   function ArcCoth (X : in Value) return Value is
   begin
      case X.Kind is
         when Number_Kind =>
            return Value'(V => ArcCoth (X.V), C=> ' ', S=> null, Kind => Number_Kind, Object=>null);
         when String_Kind =>
            Raise_Exception(Constraint_Error'Identity,
               "Can't compute arccoth of: " & (+X.S));
         when Object_Kind|Ref_1D|Ref_2D =>
            Raise_Exception(Constraint_Error'Identity,
               "Can't compute arccoth of object");
         when Character_Kind =>
            Raise_Exception(Constraint_Error'Identity,
               "Can't compute arccoth of: " & ada.Characters.conversions.To_Character(X.C));
      end case;
   exception when Ada.Numerics.Argument_Error =>
      Raise_Exception(Ada.Numerics.Argument_Error'Identity,
         "Bad parameters to arccoth");
   end ArcCoth;

   ------------
   -- Arcsin --
   ------------

   function Arcsin
     (X     : in     Value;
      Cycle : in     Value := Two_Pi)
      return Value
   is
   begin
      if X.Kind = Number_Kind and Cycle.Kind = Number_Kind then
         return Value'(V => Arcsin (X.V, Cycle.V), C=> ' ', S=> null,
            Kind => Number_Kind, Object=>null);
      else
         Raise_Exception(Constraint_Error'Identity,
            "Can't mix number and string");
      end if;
   exception when Ada.Numerics.Argument_Error =>
      Raise_Exception(Ada.Numerics.Argument_Error'Identity,
         "Bad parameters to arcsin");
   end Arcsin;

   -------------
   -- ArcSinh --
   -------------

   function ArcSinh (X : in Value) return Value is
   begin
      case X.Kind is
         when Number_Kind =>
            return Value'(V => ArcSinh (X.V), C=> ' ', S=> null,
               Kind => Number_Kind, Object=>null);
         when Object_Kind|Ref_1D|Ref_2D =>
            Raise_Exception(Constraint_Error'Identity,
               "Can't compute arcsinh of object");
         when String_Kind =>
            Raise_Exception(Constraint_Error'Identity,
               "Can't compute arcsinh of: " & (+X.S));
         when Character_Kind =>
            Raise_Exception(Constraint_Error'Identity,
               "Can't compute arcsinh of: " & ada.Characters.conversions.To_Character(X.C));
      end case;
   exception when Ada.Numerics.Argument_Error =>
      Raise_Exception(Ada.Numerics.Argument_Error'Identity,
         "Bad parameters to arcsinh");
   end ArcSinh;

   ------------
   -- Arctan --
   ------------

   function Arctan
     (Y     : in     Value;
      X     : in     Value := One;
      Cycle : in     Value := Two_Pi)
      return Value
   is
   begin
      if Y.Kind = Number_Kind and X.Kind = Number_Kind and Cycle.Kind = Number_Kind then
         return Value'(V => Arctan (Y.V, X.V, Cycle.V), C=> ' ', S=> null,
            Kind => Number_Kind, Object=>null);
      else
         raise Ada.Numerics.Argument_Error;
      end if;
   exception when Ada.Numerics.Argument_Error =>
      Raise_Exception(Ada.Numerics.Argument_Error'Identity,
         "Bad parameters to arctan");
   end Arctan;

   -------------
   -- ArcTanh --
   -------------

   function ArcTanh (X : in Value) return Value is
   begin
      case X.Kind is
         when Number_Kind =>
            return Value'(V => ArcTanh (X.V), C=> ' ', S=> null,
               Kind => Number_Kind, Object=>null);
         when Object_Kind|Ref_1D|Ref_2D =>
            Raise_Exception(Constraint_Error'Identity,
               "Can't compute arctanh of object");
         when String_Kind =>
            Raise_Exception(Constraint_Error'Identity,
               "Can't take arctan of: " & (+X.S));
         when Character_Kind =>
            Raise_Exception(Constraint_Error'Identity,
               "Can't compute arctan of: " & ada.Characters.conversions.To_Character(X.C));
      end case;
   exception when Ada.Numerics.Argument_Error =>
      Raise_Exception(Ada.Numerics.Argument_Error'Identity,
         "Bad parameters to arctanh");
   end ArcTanh;

   -------------
   -- Ceiling --
   -------------

   function Ceiling
     (X     : in     Value)
      return Value
   is
   begin
      case X.Kind is
         when Number_Kind =>
            return Value'(V => Long_Float'Ceiling (X.V), C=> ' ', S=> null,
               Kind => Number_Kind, Object=>null);
         when Object_Kind|Ref_1D|Ref_2D =>
            Raise_Exception(Constraint_Error'Identity,
               "Can't compute ceiling of object");
         when String_Kind =>
            Raise_Exception(Ada.Numerics.Argument_Error'Identity,
               "Can't take ceiling of: " & (+X.S));
         when Character_Kind =>
            Raise_Exception(Constraint_Error'Identity,
               "Can't take ceiling of: " & ada.Characters.conversions.To_Character(X.C));
      end case;
   end Ceiling;

   -----------
   -- Floor --
   -----------

   function Floor
     (X     : in     Value)
      return Value
   is
   begin
      case X.Kind is
         when Number_Kind =>
            return Value'(V => Long_Float'Floor (X.V), C=> ' ', S=> null,
               Kind => Number_Kind, Object=>null);
         when Object_Kind|Ref_1D|Ref_2D =>
            Raise_Exception(Constraint_Error'Identity,
               "Can't compute floor of object");
         when String_Kind =>
            Raise_Exception(Ada.Numerics.Argument_Error'Identity,
               "Can't take floor of: " & (+X.S));
         when Character_Kind =>
            Raise_Exception(Constraint_Error'Identity,
               "Can't take floor of: " & ada.Characters.conversions.To_Character(X.C));
      end case;
   end Floor;

   ---------
   -- Cos --
   ---------

   function Cos
     (X     : in     Value;
      Cycle : in     Value := Two_Pi)
      return Value
   is
   begin
      if X.Kind = Number_Kind and Cycle.Kind = Number_Kind then
         return Value'(V => Cos (X.V, Cycle.V), C=> ' ', S=> null,
            Kind => Number_Kind, Object=>null);
      else
         raise Ada.Numerics.Argument_Error;
      end if;
   exception when Ada.Numerics.Argument_Error =>
      Raise_Exception(Ada.Numerics.Argument_Error'Identity,
         "Bad parameters to cos");
   end Cos;

   ----------
   -- Cosh --
   ----------

   function Cosh (X : in Value) return Value is
   begin
      case X.Kind is
         when Number_Kind =>
            return Value'(V => Cosh (X.V), C=> ' ', S=> null,
               Kind => Number_Kind, Object=>null);
         when others =>
            raise Ada.Numerics.Argument_Error;
      end case;
   exception when Ada.Numerics.Argument_Error =>
      Raise_Exception(Ada.Numerics.Argument_Error'Identity,
         "Bad parameters to cosh");
   end Cosh;

   ---------
   -- Cot --
   ---------

   function Cot
     (X     : in     Value;
      Cycle : in     Value := Two_Pi)
      return Value
   is
   begin
      if X.Kind = Number_Kind and Cycle.Kind = Number_Kind then
         return Value'(V => Cot (X.V, Cycle.V), C=> ' ', S=> null,
            Kind => Number_Kind, Object=>null);
      else
         raise Ada.Numerics.Argument_Error;
      end if;
   exception when Ada.Numerics.Argument_Error =>
      Raise_Exception(Ada.Numerics.Argument_Error'Identity,
         "Bad parameters to cot");
   end Cot;

   ----------
   -- Coth --
   ----------

   function Coth (X : in Value) return Value is
   begin
      case X.Kind is
         when Number_Kind =>
            return Value'(V => Coth (X.V), C=> ' ', S=> null,
               Kind => Number_Kind, Object=>null);
         when others =>
            raise Ada.Numerics.Argument_Error;
      end case;
   exception when Ada.Numerics.Argument_Error =>
      Raise_Exception(Ada.Numerics.Argument_Error'Identity,
         "Bad parameters to coth");
   end Coth;

   ---------
   -- Log --
   ---------

   function Log
     (X    : in     Value;
      Base : in     Value := E)
      return Value
   is
   begin
      if X.Kind = Number_Kind and Base.Kind = Number_Kind then
         return Value'(V => Log (X.V, Base.V), C=> ' ', S=> null,
            Kind => Number_Kind, Object=>null);
      else
         raise Ada.Numerics.Argument_Error;
      end if;
   exception when Ada.Numerics.Argument_Error =>
      Raise_Exception(Ada.Numerics.Argument_Error'Identity,
         "Bad parameters to log");
   end Log;

   ---------
   -- Sin --
   ---------

   function Sin
     (X     : in     Value;
      Cycle : in     Value := Two_Pi)
      return Value
   is
   begin
      if X.Kind = Number_Kind and Cycle.Kind = Number_Kind then
         return Value'(V => Sin (X.V, Cycle.V), C=> ' ', S=> null,
            Kind => Number_Kind, Object=>null);
      else
         raise Ada.Numerics.Argument_Error;
      end if;
   exception when Ada.Numerics.Argument_Error =>
      Raise_Exception(Ada.Numerics.Argument_Error'Identity,
         "Bad parameters to sin");
   end Sin;

   ----------
   -- Sinh --
   ----------

   function Sinh (X : in Value) return Value is
   begin
      case X.Kind is
         when Number_Kind =>
            return Value'(V => Sinh (X.V), C=> ' ', S=> null,
               Kind => Number_Kind, Object=>null);
         when others =>
            raise Ada.Numerics.Argument_Error;
      end case;
   exception when Ada.Numerics.Argument_Error =>
      Raise_Exception(Ada.Numerics.Argument_Error'Identity,
         "Bad parameters to sinh");
   end Sinh;

   ----------
   -- Sqrt --
   ----------

   function Sqrt
     (X : Value)
      return Value
   is
   begin
      case X.Kind is
         when Number_Kind =>
            begin
               return Value'(V => Sqrt (X.V), C=> ' ', S=> null, Kind => Number_Kind, Object=>null);
            exception when Ada.Numerics.Argument_Error =>
               Raise_Exception(Ada.Numerics.Argument_Error'Identity,
                     "Can't take square root of negative number");
            end;
         when Object_Kind|Ref_1D|Ref_2D =>
            Raise_Exception(Constraint_Error'Identity,
               "Can't compute sqrt of object");
         when String_Kind =>
               Raise_Exception(Ada.Numerics.Argument_Error'Identity,
                     "Can't take square root of string:" & (+X.S));
         when Character_Kind =>
               Raise_Exception(Ada.Numerics.Argument_Error'Identity,
                     "Can't take square root of character:" & ada.Characters.conversions.To_Character(X.C));
      end case;
   end Sqrt;

   ---------
   -- Tan --
   ---------

   function Tan
     (X     : in     Value;
      Cycle : in     Value := Two_Pi)
      return Value
   is
   begin
      if X.Kind = Number_Kind and Cycle.Kind = Number_Kind then
            return Value'(V => Tan (X.V, Cycle.V), C=> ' ', S=>null, Kind=>Number_Kind, Object=>null);
      else
         raise Ada.Numerics.Argument_Error;
      end if;
   exception when Ada.Numerics.Argument_Error =>
      Raise_Exception(Ada.Numerics.Argument_Error'Identity,
         "Bad parameters to tan");
   end Tan;

   ----------
   -- Tanh --
   ----------

   function Tanh (X : in Value) return Value is
   begin
      case X.Kind is
         when Number_Kind =>
            return Value'(V => Tanh (X.V), C=> ' ', S=>null, Kind => Number_Kind, Object=>null);
         when others =>
            raise Ada.Numerics.Argument_Error;
      end case;
   exception when Ada.Numerics.Argument_Error =>
      Raise_Exception(Ada.Numerics.Argument_Error'Identity,
         "Bad parameters to tanh");
   end Tanh;

   -- return positive number if L>R, 0 if equal, negative if L<R
   function Compare(L,R : in Value) return Integer is
      L_Value : Long_Float;
      R_Value : Long_Float;
      use type mssyst.object.ref;
   begin
      if L.Kind = Character_Kind and R.Kind = Number_Kind then
         if Long_Float(Wide_Character'Pos(L.C))=R.V then
            return 0;
         elsif Long_Float(Wide_Character'Pos(L.C))>R.V then
            return 1;
         else
            return -1;
         end if;
      elsif L.Kind = Number_Kind and R.Kind = Character_Kind then
         if L.V=Long_Float(Wide_Character'Pos(R.C)) then
            return 0;
         elsif L.V > Long_Float(Wide_Character'Pos(R.C)) then
            return 1;
         else
            return -1;
         end if;
      elsif L.Kind /= R.Kind then
         Raise_Exception(Ada.Numerics.Argument_Error'Identity,
            "Can't compare these values: " & Value_Kind'Image(L.Kind) & " " &
               Value_Kind'Image(R.Kind));
      else
         case L.Kind is
            when Object_Kind|Ref_1D|Ref_2D =>
               if L.Object=R.Object then
                  return 0;
               elsif L.Object.Gethashcode > R.Object.GetHashCode then
                  return 1;
               else
                  return -1;
               end if;
            when Character_Kind =>
               if L.C=R.C then
                  return 0;
               elsif L.C > R.C then
                  return 1;
               else
                  return -1;
               end if;
            when String_Kind =>
               return MSSyst.String.Compare(L.S,R.S);
            when Number_Kind =>
               if Is_Integer(L) then
                  L_Value := Long_Float(Integer(L.V));
               else
                  L_Value := L.V;
               end if;
               if Is_Integer(R) then
                  R_Value := Long_Float(Integer(R.V));
               else
                  R_Value := R.V;
               end if;
               if (L_Value=R_Value) then
                  return 0;
               elsif (L_Value > R_Value) then
                  return 1;
               else
                  return -1;
               end if;
         end case;
      end if;
   end Compare;

   function ">="(L,R : in Value) return Boolean is
   begin
      return compare(L,R)>=0;
   end ">=";

   function ">"(L,R : in Value) return Boolean is
   begin
      return compare(L,R)>0;
   end ">";

   function "<"(L,R : in Value) return Boolean is
   begin
      return compare(L,R)<0;
   end "<";

   function "<="(L,R : in Value) return Boolean is
   begin
      return compare(L,R)<=0;
   end "<=";

   function "="(L,R : in Value) return Boolean is
   begin
      return compare(L,R) = 0;
   end "=";
   procedure Copy(From : in Value; To : out Value) is
   begin
      To.V := From.V;
      To.S := From.S;
      To.C := From.C;
      To.Object := From.Object;
      To.Kind := From.Kind;
   end Copy;
   function Length_Of(V : in Value) return Natural is
   begin
      case V.Kind is
         when String_Kind =>
            return V.S.get_Length;
         when Character_Kind =>
            return 1;
         when Object_Kind|Ref_1D|Ref_2D =>
            Raise_Exception(Ada.Numerics.Argument_Error'Identity,
               "Can't compute length of Object");
            return 0;
         when Number_Kind =>
            Raise_Exception(Ada.Numerics.Argument_Error'Identity,
               "Can't compute length of number");
            return 0;
      end case;
   end Length_Of;


end Numbers;

