with Lexer;
use Lexer;
with Numbers;
use Numbers;
with Mssyst.String;
with MSSyst.Object;
limited with Generate_Interface;

package Parse_Tree is
   Runtime_Error : exception;
   Did_Method_Call : Boolean;
   Did_Function_Call : Boolean;
   type Expression;
   type Expression_Pointer is access all Expression'Class;
   type Parameter_List;
   type Parameter_List_Pointer is access all Parameter_List;

   type Parseable is abstract tagged null record;
   procedure Emit_Code (This : in Parseable; Gen : access Generate_Interface.Typ'Class) is abstract;
   procedure Compile_Pass1(This : in Parseable; Gen : access Generate_Interface.Typ'Class) is abstract;
   type Parseable_Pointer is access all Parseable'Class;

   type Value_Parseable is abstract new Parseable with null record;
   function Get_Class_Decl(This : in Value_Parseable) return MSSyst.String.Ref is abstract;
   function Execute (This : in Value_Parseable) return Value is abstract;
   procedure Emit_Code (This : in Value_Parseable; Gen : access Generate_Interface.Typ'Class) is abstract;
   type Value_Parseable_Pointer is access all Value_Parseable'Class;

   -- Statement => (Procedure_Call | Assignment) [;] End_Input
   type Statement is abstract new Parseable with null record;
   procedure Emit_Code (This : in Statement; Gen : access Generate_Interface.Typ'Class) is abstract;
   procedure Compile_Pass1 (This : in Statement; Gen : access Generate_Interface.Typ'Class) is abstract;
   type Statement_Pointer is access all Statement'Class;
   procedure Execute (This : in Statement) is abstract;

   type Lhs is tagged;
   type Lhs_Pointer is access all Lhs'Class;
   -- Msuffix => . Lhs Msuffix | .id | .id (Parameter_list)
   type Msuffix is abstract tagged null record;
   type MSuffix_Pointer is access all MSuffix'Class;
   procedure Method_Call (This : in MSuffix) is abstract;
   procedure Emit_Code (This : in MSuffix; Gen : access Generate_Interface.Typ'Class) is abstract;
   type Full_Msuffix is new Msuffix with record
      Lhs : Lhs_Pointer;
      MSuffix : Msuffix_Pointer;
   end record;
   procedure Method_Call (This : in Full_MSuffix);
   procedure Emit_Code (This : in Full_MSuffix; Gen : access Generate_Interface.Typ'Class);
   type Noparam_Msuffix is new Msuffix with record
      Id : Token_Pointer;
   end record;
   procedure Method_Call (This : in NoParam_MSuffix);
   procedure Emit_Code (This : in NoParam_MSuffix; Gen : access Generate_Interface.Typ'Class);
   type Param_Msuffix is new MSuffix with record
      Id         : Token_Pointer;
      Param_List : Parameter_List_Pointer;
   end record;
   type Param_Msuffix_Pointer is access all Param_MSuffix'Class;
   procedure Method_Call (This : in Param_MSuffix);
   procedure Emit_Code (This : in Param_MSuffix; Gen : access Generate_Interface.Typ'Class);


   -- Procedure_Call => proc_id(Parameter_List) | plugin_id(Parameter_List) | tab_id(Parameter_List) |
   -- lhs msuffix
   -- MSuffix => . Lhs MSuffix | . id | .id (Parameter_List)
   type Procedure_Call is abstract new Statement with record
      Id         : Token_Pointer;
      Param_List : Parameter_List_Pointer;
   end record;
   function Is_Tab_Call(This : in Procedure_Call) return Boolean;
   function Get_Name(This : in Procedure_Call) return String;

   type Proc_Call is new Procedure_Call with null record;
   procedure Execute (This : in Proc_Call);
   procedure Emit_Code (This : in Proc_Call; Gen : access Generate_Interface.Typ'Class);
   procedure Compile_Pass1 (This : in Proc_Call; Gen : access Generate_Interface.Typ'Class);
   type Proc_Call_Pointer is access all Proc_Call'Class;

   type Plugin_Proc_Call is new Procedure_Call with null record;
   procedure Execute (This : in Plugin_Proc_Call);
   procedure Emit_Code (This : in Plugin_Proc_Call; Gen : access Generate_Interface.Typ'Class);
   procedure Compile_Pass1 (This : in Plugin_Proc_Call; Gen : access Generate_Interface.Typ'Class);
   type Plugin_Proc_Call_Pointer is access all Plugin_Proc_Call'Class;

   type Tabid_Proc_Call is new Procedure_Call with null record;
   procedure Execute (This : in Tabid_Proc_Call);
   procedure Emit_Code (This : in Tabid_Proc_Call; Gen : access Generate_Interface.Typ'Class);
   procedure Compile_Pass1 (This : in Tabid_Proc_Call; Gen : access Generate_Interface.Typ'Class);
   function Is_Tab_Call(This : in Tabid_Proc_Call) return Boolean;
   type Tabid_Proc_Call_Pointer is access all Tabid_Proc_Call'Class;

   type Method_Proc_Call is new Procedure_Call with record
      Lhs : Lhs_Pointer;
      MSuffix : Msuffix_Pointer;
   end record;
   type Method_Proc_Call_Pointer is access all Method_Proc_Call'Class;
   procedure Execute (This : in Method_Proc_Call);
   procedure Emit_Code (This : in Method_Proc_Call; Gen : access Generate_Interface.Typ'Class);
   procedure Compile_Pass1 (This : in Method_Proc_Call; Gen : access Generate_Interface.Typ'Class);
   type Method_Proc_Call_This is new Method_Proc_Call with null record;
   procedure Execute (This : in Method_Proc_Call_This);
   procedure Emit_Code (This : in Method_Proc_Call_This; Gen : access Generate_Interface.Typ'Class);
   procedure Compile_Pass1 (This : in Method_Proc_Call_This; Gen : access Generate_Interface.Typ'Class);
   type Method_Proc_Call_Super is new Method_Proc_Call with null record;
   procedure Execute (This : in Method_Proc_Call_Super);
   procedure Emit_Code (This : in Method_Proc_Call_Super; Gen : access Generate_Interface.Typ'Class);
   procedure Compile_Pass1 (This : in Method_Proc_Call_Super; Gen : access Generate_Interface.Typ'Class);

   -- Lhs => id[\[Expression[,Expression]\]]
   type LSuffix is tagged;
   type Lsuffix_Pointer is access all Lsuffix'Class;

   type Lhs is abstract tagged null record;
   function Get_Name(This : in Lhs) return String is abstract;
   procedure Emit_Code (This : in Lhs; LSuffix : in LSuffix_Pointer; Gen : access Generate_Interface.Typ'Class) is abstract;
   procedure Emit_Method (This : in Lhs; Gen : access Generate_Interface.Typ'Class) is abstract;
   procedure Compile_Pass1 (This : in Lhs; Gen : access Generate_Interface.Typ'Class) is abstract;
   procedure Method_Call(this : in Lhs) is abstract;
   function Execute(This : in Lhs; LSuffix : in LSuffix_Pointer; Context : in MSSyst.Object.Ref; V : Numbers.Value)
      return MSSyst.Object.Ref is abstract;

   type Id_Lhs is new Lhs with record
      Id : Token_Pointer;
   end record;
   function Get_Name(This : in Id_Lhs) return String;
   procedure Method_Call(this : in Id_Lhs);
   function Execute(This : in Id_Lhs; LSuffix : in LSuffix_Pointer; Context : in MSSyst.Object.Ref; V : Numbers.Value)
      return MSSyst.Object.Ref;
   procedure Emit_Code (This : in Id_Lhs; LSuffix : in LSuffix_Pointer; Gen : access Generate_Interface.Typ'Class);
   procedure Emit_Method (This : in Id_Lhs; Gen : access Generate_Interface.Typ'Class);
   procedure Compile_Pass1 (This : in Id_Lhs; Gen : access Generate_Interface.Typ'Class);

   type Array_Ref_Lhs is new Id_Lhs with record
      Reference : Expression_Pointer;
   end record;
   procedure Method_Call(this : in Array_Ref_Lhs);
   function Execute(This : in Array_Ref_Lhs; LSuffix : in LSuffix_Pointer; Context : in MSSyst.Object.Ref; V : Numbers.Value)
      return MSSyst.Object.Ref;
   type Array_Ref_Lhs_Pointer is access all Array_Ref_Lhs'Class;
   procedure Emit_Code (This : in Array_Ref_Lhs; LSuffix : in LSuffix_Pointer; Gen : access Generate_Interface.Typ'Class);
   procedure Emit_Method (This : in Array_Ref_Lhs; Gen : access Generate_Interface.Typ'Class);
   procedure Compile_Pass1 (This : in Array_Ref_Lhs; Gen : access Generate_Interface.Typ'Class);

   type Array_2D_Ref_Lhs is new Array_Ref_Lhs with record
      Reference2 : Expression_Pointer;
   end record;
   procedure Method_Call(this : in Array_2D_Ref_Lhs);
   function Execute(This : in Array_2D_Ref_Lhs; LSuffix : in LSuffix_Pointer; Context : in MSSyst.Object.Ref; V : Numbers.Value)
      return MSSyst.Object.Ref;
   type Array_2D_Ref_Lhs_Pointer is access all Array_2D_Ref_Lhs'Class;
   procedure Emit_Code (This : in Array_2D_Ref_Lhs; LSuffix : in LSuffix_Pointer; Gen : access Generate_Interface.Typ'Class);
   procedure Emit_Method (This : in Array_2D_Ref_Lhs; Gen : access Generate_Interface.Typ'Class);
   procedure Compile_Pass1 (This : in Array_2D_Ref_Lhs; Gen : access Generate_Interface.Typ'Class);

   -- Lsuffix => . Lhs Lsuffix | lambda
   type Lsuffix is abstract tagged null record;
   procedure Execute (This : in LSuffix; Context : in MSSyst.Object.Ref; V : Numbers.Value) is abstract;
   procedure Emit_Code (This : in LSuffix; Gen : access Generate_Interface.Typ'Class) is abstract;
   type Full_Lsuffix is new Lsuffix with record
      Lhs : Lhs_Pointer;
      LSuffix : Lsuffix_Pointer;
   end record;
   procedure Emit_Code (This : in Full_LSuffix; Gen : access Generate_Interface.Typ'Class);
   procedure Execute (This : in Full_LSuffix; Context : in MSSyst.Object.Ref; V : Numbers.Value);
   type Empty_Lsuffix is new Lsuffix with null record;
   procedure Execute (This : in Empty_LSuffix; Context : in MSSyst.Object.Ref; V : Numbers.Value);
   procedure Emit_Code (This : in Empty_LSuffix; Gen : access Generate_Interface.Typ'Class);

   -- Assignment => Lhs LSuffix [=|:=] Expression
   type Assignment is abstract new Statement with record
      Lhs : Lhs_Pointer;
      LSuffix : LSuffix_Pointer;
   end record;
   procedure Execute (This : in Assignment) is abstract;
   function Get_Name(This : in Assignment) return String;
   type Assignment_Pointer is access all Assignment'Class;

   type Expr_Assignment is new Assignment with record
      Expr_Part : Expression_Pointer;
   end record;
   procedure Emit_Code (This : in Expr_Assignment; Gen : access Generate_Interface.Typ'Class);
   procedure Compile_Pass1 (This : in Expr_Assignment; Gen : access Generate_Interface.Typ'Class);
   procedure Execute (This : in Expr_Assignment);

   type RSuffix is tagged;
   type Rsuffix_Pointer is access all Rsuffix'Class;
   -- Rhs => id[\[Expression[,Expression]\]] | id(Expression_List)
   type Rhs is abstract tagged null record;
   function Execute(This : in Rhs; Suffix : in RSuffix_Pointer) return Numbers.Value is abstract;
   procedure Emit_Code (This : in Rhs; Gen : access Generate_Interface.Typ'Class) is abstract;
   procedure Compile_Pass1(This : in Rhs; Gen : access Generate_Interface.Typ'Class) is abstract;
   function Is_Method_Call(This : in Rhs) return Boolean;
   type Rhs_Pointer is access all Rhs'Class;
   type Id_Rhs is new Rhs with record
      Id : Token_Pointer;
   end record;
   function Execute (This : in Id_Rhs; Suffix : in RSuffix_Pointer) return Value;
   procedure Emit_Code (This : in Id_Rhs; Gen : access Generate_Interface.Typ'Class);
   procedure Compile_Pass1 (This : in Id_Rhs; Gen : access Generate_Interface.Typ'Class);

   type Array_Ref_Rhs is new Id_Rhs with record
      Reference : Expression_Pointer;
   end record;
   type Array_Ref_Rhs_Pointer is access all Array_Ref_Rhs'Class;
   function Execute (This : in Array_Ref_Rhs; Suffix : in RSuffix_Pointer) return Value;
   procedure Emit_Code (This : in Array_Ref_Rhs; Gen : access Generate_Interface.Typ'Class);
   procedure Compile_Pass1 (This : in Array_Ref_Rhs; Gen : access Generate_Interface.Typ'Class);

   type Array_2D_Ref_Rhs is new Array_Ref_Rhs with record
      Reference2 : Expression_Pointer;
   end record;
   type Array_2D_Ref_Rhs_Pointer is access all Array_2D_Ref_Rhs'Class;
   function Execute (This : in Array_2D_Ref_Rhs; Suffix : in RSuffix_Pointer) return Value;
   procedure Emit_Code (This : in Array_2D_Ref_Rhs; Gen : access Generate_Interface.Typ'Class);
   procedure Compile_Pass1 (This : in Array_2D_Ref_Rhs; Gen : access Generate_Interface.Typ'Class);

   type Rhs_Method_Call is new Id_Rhs with record
      Parameters : Parameter_List_Pointer;
   end record;
   type Rhs_Method_Call_Pointer is access all Rhs_Method_Call'Class;
   function Is_Method_Call(This : in Rhs_Method_Call) return Boolean;
   function Execute (This : in Rhs_Method_Call; Suffix : in RSuffix_Pointer) return Value;
   procedure Emit_Code (This : in Rhs_Method_Call; Gen : access Generate_Interface.Typ'Class);
--   procedure Compile_Pass1 (This : in Rhs_Method_Call; Gen : access Generate_Interface.Typ'Class);

   -- Rsuffix => . Rhs Rsuffix | lambda
   type Rsuffix is abstract tagged null record;
   function Execute(This : in RSuffix; V : Numbers.Value) return Numbers.Value is abstract;
   procedure Emit_Code (This : in RSuffix; Gen : access Generate_Interface.Typ'Class) is abstract;


   type Full_Rsuffix is new Rsuffix with record
      Rhs : Rhs_Pointer;
      RSuffix : Rsuffix_Pointer;
   end record;
   procedure Emit_Code (This : in Full_RSuffix; Gen : access Generate_Interface.Typ'Class);
   function Execute(This : in Full_RSuffix; V : Numbers.Value) return Numbers.Value;

   type Empty_Rsuffix is new Rsuffix with null record;
   function Execute(This : in Empty_RSuffix; V : Numbers.Value) return Numbers.Value;
   procedure Emit_Code (This : in Empty_RSuffix; Gen : access Generate_Interface.Typ'Class);


   -- Expon => Rhs RSuffix | num | (Expression)
   --          | new id | new id(Expression_List)
   --          | func_id(Expression_List) | func_id0 | -Expon
   --          | plugin_func_id(Expression_List)
   type Expon is abstract new Value_Parseable with null record;
   function Get_Class_Decl(This : in Expon) return MSSyst.String.Ref;
   type Expon_Pointer is access all Expon'Class;
   type Expon_Stub is new Expon with record
      Component : MSSyst.Object.Ref;
      Index     : Integer;
      Expon_Parse_Tree : Expon_Pointer;
   end record;
   function Get_Class_Decl(This : in Expon_Stub) return MSSyst.String.Ref;
   function Execute (This : in Expon_Stub) return Value;
   procedure Emit_Code (This : in Expon_Stub; Gen : access Generate_Interface.Typ'Class);
   procedure Compile_Pass1 (This : in Expon_Stub; Gen : access Generate_Interface.Typ'Class);

   type Rhs_Expon is new Expon with record
      Rhs : Rhs_Pointer;
      Rsuffix : Rsuffix_Pointer;
   end record;
   type Rhs_Expon_Pointer is access all Rhs_Expon'Class;
   function Is_Method_Call(This : in Rhs_Expon) return Boolean;
   function Execute (This : in Rhs_Expon) return Value;
   procedure Emit_Code (This : in Rhs_Expon; Gen : access Generate_Interface.Typ'Class);
   procedure Compile_Pass1 (This : in Rhs_Expon; Gen : access Generate_Interface.Typ'Class);

   type Number_Expon is new Expon with record
      Number : Token_Pointer;
   end record;
   function Execute (This : in Number_Expon) return Value;
   procedure Emit_Code (This : in Number_Expon; Gen : access Generate_Interface.Typ'Class);
   procedure Compile_Pass1 (This : in Number_Expon; Gen : access Generate_Interface.Typ'Class);

   type Negative_Expon is new Expon with record
      E : Expon_Pointer;
   end record;
   function Execute (This : in Negative_Expon) return Value;
   procedure Emit_Code (This : in Negative_Expon; Gen : access Generate_Interface.Typ'Class);
   procedure Compile_Pass1 (This : in Negative_Expon; Gen : access Generate_Interface.Typ'Class);

   type String_Expon is new Expon with record
      S : Token_Pointer;
   end record;
   function Execute (This : in String_Expon) return Value;
   procedure Emit_Code (This : in String_Expon; Gen : access Generate_Interface.Typ'Class);
   procedure Compile_Pass1 (This : in String_Expon; Gen : access Generate_Interface.Typ'Class);

   type Character_Expon is new Expon with record
      S : Token_Pointer;
   end record;
   function Execute (This : in Character_Expon) return Value;
   procedure Emit_Code (This : in Character_Expon; Gen : access Generate_Interface.Typ'Class);
   procedure Compile_Pass1 (This : in Character_Expon; Gen : access Generate_Interface.Typ'Class);


   type Paren_Expon is new Expon with record
      Expr_Part : Expression_Pointer;
   end record;
   function Execute (This : in Paren_Expon) return Value;
   procedure Emit_Code (This : in Paren_Expon; Gen : access Generate_Interface.Typ'Class);
   procedure Compile_Pass1 (This : in Paren_Expon; Gen : access Generate_Interface.Typ'Class);

   type Id_Expon is abstract new Expon with record
      Id : Token_Pointer;
   end record;

   type Func0_Expon is new Id_Expon with null record;
   function Execute (This : in Func0_Expon) return Value;
   procedure Emit_Code (This : in Func0_Expon; Gen : access Generate_Interface.Typ'Class);
   procedure Compile_Pass1 (This : in Func0_Expon; Gen : access Generate_Interface.Typ'Class);

   type Class_Expon is new Id_Expon with null record;
   type Class_Expon_Pointer is access all Class_Expon'Class;
   function Get_Class_Decl(This : in Class_Expon) return MSSyst.String.Ref;
   function Execute (This : in Class_Expon) return Value;
   procedure Emit_Code (This : in Class_Expon; Gen : access Generate_Interface.Typ'Class);
   procedure Compile_Pass1 (This : in Class_Expon; Gen : access Generate_Interface.Typ'Class);

   type Class_Constructor_Expon is new Id_Expon with record
      Parameters : Parameter_List_Pointer;
   end record;
   type Class_Constructor_Expon_Pointer is access all Class_Constructor_Expon'Class;
   function Get_Class_Decl(This : in Class_Constructor_Expon) return MSSyst.String.Ref;
   function Execute (This : in Class_Constructor_Expon) return Value;
   procedure Emit_Code (This : in Class_Constructor_Expon; Gen : access Generate_Interface.Typ'Class);
   procedure Compile_Pass1 (This : in Class_Constructor_Expon; Gen : access Generate_Interface.Typ'Class);



   type Func_Expon is new Id_Expon with record
      Parameters : Parameter_List_Pointer;
   end record;
   type Func_Expon_Pointer is access all Func_Expon'Class;
   function Execute (This : in Func_Expon) return Value;
   procedure Emit_Code (This : in Func_Expon; Gen : access Generate_Interface.Typ'Class);
   procedure Compile_Pass1 (This : in Func_Expon; Gen : access Generate_Interface.Typ'Class);

   type Plugin_Func_Expon is new Id_Expon with record
      Parameters : Parameter_List_Pointer;
   end record;
   type Plugin_Func_Expon_Pointer is access all Plugin_Func_Expon'Class;
   function Execute (This : in Plugin_Func_Expon) return Value;
   procedure Emit_Code (This : in Plugin_Func_Expon; Gen : access Generate_Interface.Typ'Class);
   procedure Compile_Pass1 (This : in Plugin_Func_Expon; Gen : access Generate_Interface.Typ'Class);

   -- Mult => Expon ^ Mult | Expon
   type Mult;
   type Mult_Pointer is access all Mult'Class;
   type Mult is new Value_Parseable with record
      Left : Value_Parseable_Pointer;
   end record;

   function Get_Class_Decl(This : in Mult) return MSSyst.String.Ref;
   function Execute (This : in Mult) return Value;
   procedure Emit_Code (This : in Mult; Gen : access Generate_Interface.Typ'Class);
   procedure Compile_Pass1 (This : in Mult; Gen : access Generate_Interface.Typ'Class);

   type Expon_Mult is new Mult with record
      Right : Mult_Pointer;
   end record;
   type Binary_Mult_Pointer is access all Expon_Mult'Class;
   function Get_Class_Decl(This : in Expon_Mult) return MSSyst.String.Ref;
   function Execute (This : in Expon_Mult) return Value;
   procedure Emit_Code (This : in Expon_Mult; Gen : access Generate_Interface.Typ'Class);
   procedure Compile_Pass1 (This : in Expon_Mult; Gen : access Generate_Interface.Typ'Class);

   procedure Fix_Associativity(This : in out Mult_Pointer);

   -- Add => Mult * Add | Mult / Add | Mult mod Add | Mult rem Add | Add
   type Add is new Value_Parseable with record
      Left : Value_Parseable_Pointer;
   end record;
   function Get_Class_Decl(This : in Add) return MSSyst.String.Ref;
   type Add_Pointer is access all Add'Class;
   function Execute (This : in Add) return Value;
   procedure Emit_Code (This : in Add; Gen : access Generate_Interface.Typ'Class);
   procedure Compile_Pass1 (This : in Add; Gen : access Generate_Interface.Typ'Class);

   type Binary_Add is abstract new Add with record
      Right : Add_Pointer;
   end record;
   function Get_Class_Decl(This : in Binary_Add) return MSSyst.String.Ref;
   type Binary_Add_Pointer is access all Binary_Add'Class;

   type Div_Add is new Binary_Add with null record;
   function Execute (This : in Div_Add) return Value;
   procedure Emit_Code (This : in Div_Add; Gen : access Generate_Interface.Typ'Class);
   procedure Compile_Pass1 (This : in Div_Add; Gen : access Generate_Interface.Typ'Class);

   type Mult_Add is new Binary_Add with null record;
   function Execute (This : in Mult_Add) return Value;
   procedure Emit_Code (This : in Mult_Add; Gen : access Generate_Interface.Typ'Class);
   procedure Compile_Pass1 (This : in Mult_Add; Gen : access Generate_Interface.Typ'Class);

   type Mod_Add is new Binary_Add with null record;
   function Execute (This : in Mod_Add) return Value;
   procedure Emit_Code (This : in Mod_Add; Gen : access Generate_Interface.Typ'Class);
   procedure Compile_Pass1 (This : in Mod_Add; Gen : access Generate_Interface.Typ'Class);

   type Rem_Add is new Binary_Add with null record;
   function Execute (This : in Rem_Add) return Value;
   procedure Emit_Code (This : in Rem_Add; Gen : access Generate_Interface.Typ'Class);
   procedure Compile_Pass1 (This : in Rem_Add; Gen : access Generate_Interface.Typ'Class);

   procedure Fix_Associativity(This : in out Add_Pointer);

   -- Expression => Add + Expression | Add - Expression | Add
   type Expression is new Value_Parseable with record
      Left : Value_Parseable_Pointer;
   end record;
   function Execute (This : in Expression) return Value;
   function Get_Class_Decl(This : in Expression) return MSSyst.String.Ref;
   procedure Emit_Code (This : in Expression; Gen : access Generate_Interface.Typ'Class);
   procedure Compile_Pass1 (This : in Expression; Gen : access Generate_Interface.Typ'Class);

   type Binary_Expression is abstract new Expression with record
      Right : Expression_Pointer;
   end record;
   function Get_Class_Decl(This : in Binary_Expression) return MSSyst.String.Ref;
   type Binary_Expression_Pointer is access all Binary_Expression'Class;

   type Add_Expression is new Binary_Expression with null record;
   function Execute (This : in Add_Expression) return Value;
   procedure Emit_Code (This : in Add_Expression; Gen : access Generate_Interface.Typ'Class);
   procedure Compile_Pass1 (This : in Add_Expression; Gen : access Generate_Interface.Typ'Class);

   type Minus_Expression is new Binary_Expression with null record;
   function Execute (This : in Minus_Expression) return Value;
   procedure Emit_Code (This : in Minus_Expression; Gen : access Generate_Interface.Typ'Class);
   procedure Compile_Pass1 (This : in Minus_Expression; Gen : access Generate_Interface.Typ'Class);

   procedure Fix_Associativity (This : in out Expression_Pointer);

   type Boolean_Parseable is abstract new Parseable with null record;
   type Boolean_Parseable_Pointer is access all Boolean_Parseable'Class;
   function Execute (This : in Boolean_Parseable) return Boolean is abstract;
   procedure Emit_Code (This : in Boolean_Parseable; Gen : access Generate_Interface.Typ'Class) is abstract;
   procedure Compile_Pass1 (This : in Boolean_Parseable; Gen : access Generate_Interface.Typ'Class) is abstract;

   -- Relation => Expression > Expression | >=,<,<=,=,/=
   type Relation is new Boolean_Parseable with record
      Left, Right : Expression_Pointer;
      Kind : Lexer.Relation;
   end record;
   type Relation_Pointer is access all Relation'Class;
   function Execute (This : in Relation) return Boolean;
   procedure Emit_Code (This : in Relation; Gen : access Generate_Interface.Typ'Class);
   procedure Compile_Pass1 (This : in Relation; Gen : access Generate_Interface.Typ'Class);


   -- Boolean_Func => Boolean0 | Boolean1 | Boolean_Constant | Boolean_Plugin
   type Boolean0 is new Boolean_Parseable with record
      Kind : Boolean_Func0_Types;
   end record;
   function Execute (This : in Boolean0) return Boolean;
   procedure Emit_Code (This : in Boolean0; Gen : access Generate_Interface.Typ'Class);
   procedure Compile_Pass1 (This : in Boolean0; Gen : access Generate_Interface.Typ'Class);

   -- true or false
   type Boolean_Constant is new Boolean_Parseable with record
      Value : Boolean;
   end record;
   function Execute (This : in Boolean_Constant) return Boolean;
   procedure Emit_Code (This : in Boolean_Constant; Gen : access Generate_Interface.Typ'Class);
   procedure Compile_Pass1 (This : in Boolean_Constant; Gen : access Generate_Interface.Typ'Class);

   type Boolean1 is new Boolean_Parseable with record
      Kind : Boolean_Func1_Types;
      Parameter : Expression_Pointer;
   end record;
   function Execute (This : in Boolean1) return Boolean;
   procedure Emit_Code (This : in Boolean1; Gen : access Generate_Interface.Typ'Class);
   procedure Compile_Pass1 (This : in Boolean1; Gen : access Generate_Interface.Typ'Class);


   type Boolean_Reflection is new Boolean_Parseable with record
      Kind : Boolean_Reflection_Types;
      Id : Token_Pointer;
   end record;
   function Execute (This : in Boolean_Reflection) return Boolean;
   procedure Emit_Code (This : in Boolean_Reflection; Gen : access Generate_Interface.Typ'Class);
   procedure Compile_Pass1 (This : in Boolean_Reflection; Gen : access Generate_Interface.Typ'Class);

   type Boolean_Plugin is new Boolean_Parseable with record
      Id : Token_Pointer;
      Parameters : Parameter_List_Pointer;
   end record;
   function Execute (This : in Boolean_Plugin) return Boolean;
   procedure Emit_Code (This : in Boolean_Plugin; Gen : access Generate_Interface.Typ'Class);
   procedure Compile_Pass1 (This : in Boolean_Plugin; Gen : access Generate_Interface.Typ'Class);
   type Boolean_Plugin_Pointer is access all Boolean_Plugin'Class;

   -- Boolean2 => [not] [ (BE) | Relation | Boolean_Func] [and BE2 | lambda]
   type Boolean2 is new Boolean_Parseable with record
      Negated : Boolean := False;
      Left    : Boolean_Parseable_Pointer;
   end record;
   type Boolean2_Pointer is access all Boolean2'Class;
   function Top_Level_Negated (This : in Boolean2) return Boolean;
   function Remove_Negation (This : in Boolean2) return Boolean2_Pointer;
   function Execute (This : in Boolean2) return Boolean;
   procedure Emit_Code (This : in Boolean2; Gen : access Generate_Interface.Typ'Class);
   procedure Compile_Pass1 (This : in Boolean2; Gen : access Generate_Interface.Typ'Class);

   type And_Boolean2 is new Boolean2 with record
      Right : Boolean2_Pointer;
   end record;
   function Top_Level_Negated (This : in And_Boolean2) return Boolean;
   function Remove_Negation (This : in And_Boolean2) return Boolean2_Pointer;
   function Execute (This : in And_Boolean2) return Boolean;
   procedure Emit_Code (This : in And_Boolean2; Gen : access Generate_Interface.Typ'Class);
   procedure Compile_Pass1 (This : in And_Boolean2; Gen : access Generate_Interface.Typ'Class);

   type Boolean_Expression;
   type Boolean_Expression_Pointer is access all Boolean_Expression'Class;

   -- Boolean_Expression => Boolean2 or Boolean_Expression |
   --                       Boolean2 xor Boolean_Expression | Boolean2
   type Boolean_Expression is new Boolean_Parseable with record
      Left : Boolean2_Pointer;
   end record;
   function Top_Level_Negated (This : in Boolean_Expression) return Boolean;
   function Remove_Negation (This : in Boolean_Expression) return Boolean_Expression_Pointer;
   function Execute (This : in Boolean_Expression) return Boolean;
   procedure Emit_Code (This : in Boolean_Expression; Gen : access Generate_Interface.Typ'Class);
   procedure Compile_Pass1 (This : in Boolean_Expression; Gen : access Generate_Interface.Typ'Class);

   type Xor_Boolean is new Boolean_Expression with record
      Right : Boolean_Expression_Pointer;
   end record;
   function Remove_Negation (This : in Xor_Boolean) return Boolean_Expression_Pointer;
   function Top_Level_Negated (This : in Xor_Boolean) return Boolean;
   function Execute (This : in Xor_Boolean) return Boolean;
   procedure Emit_Code (This : in Xor_Boolean; Gen : access Generate_Interface.Typ'Class);
   procedure Compile_Pass1 (This : in Xor_Boolean; Gen : access Generate_Interface.Typ'Class);

   type Or_Boolean is new Boolean_Expression with record
      Right : Boolean_Expression_Pointer;
   end record;
   function Remove_Negation (This : in Or_Boolean) return Boolean_Expression_Pointer;
   function Top_Level_Negated (This : in Or_Boolean) return Boolean;
   function Execute (This : in Or_Boolean) return Boolean;
   procedure Emit_Code (This : in Or_Boolean; Gen : access Generate_Interface.Typ'Class);
   procedure Compile_Pass1 (This : in Or_Boolean; Gen : access Generate_Interface.Typ'Class);

   -- Input => Lhs LSuffix
   type Input is new Parseable with record
      Lhs : Lhs_Pointer;
      LSuffix : LSuffix_Pointer;
      --Id : Token_Pointer;
   end record;
   type Input_Pointer is access all Input'Class;
   procedure Execute (This : in Input; Prompt : in MSSyst.String.Ref);
   procedure Emit_Code (This : in Input; Gen : access Generate_Interface.Typ'Class);
   procedure Compile_Pass1 (This : in Input; Gen : access Generate_Interface.Typ'Class);
   function Get_Name(This : in Input) return String;

--   type Input_Array is new Input with record
--      Reference : Expression_Pointer;
--   end record;
--   procedure Execute (This : in Input_Array; Prompt : in MSSyst.String.Ref);
--   procedure Emit_Code (This : in Input_Array; Gen : access Generate_Interface.Typ'Class);
--   procedure Compile_Pass1 (This : in Input_Array; Gen : access Generate_Interface.Typ'Class);

--   type Input_2D_Array is new Input_Array with record
--      Reference2 : Expression_Pointer;
--   end record;
--   procedure Execute (This : in Input_2D_Array; Prompt : in MSSyst.String.Ref);
--   procedure Emit_Code (This : in Input_2D_Array; Gen : access Generate_Interface.Typ'Class);
--   procedure Compile_Pass1 (This : in Input_2D_Array; Gen : access Generate_Interface.Typ'Class);

   -- Output => Expression | String
   type Output is abstract new Parseable with record
      New_Line : Boolean;
   end record;
   type Output_Pointer is access all Output'Class;
   function Execute_Return_Value(This : in Output) return Value is abstract;
   procedure Emit_Code (This : in Output; Gen : access Generate_Interface.Typ'Class) is abstract;
   procedure Execute (This : in Output) is abstract;

   type Expr_Output is new Output with record
      Expr : Expression_Pointer;
   end record;
   function Execute_Return_Value(This : in Expr_Output) return Value;
   procedure Execute (This : in Expr_Output);
   procedure Emit_Code (This : in Expr_Output; Gen : access Generate_Interface.Typ'Class);
   procedure Compile_Pass1 (This : in Expr_Output; Gen : access Generate_Interface.Typ'Class);

   -- assume that this expr_output is a variable name, and get
   -- just that name.
   function Get_String(This : in Expr_Output) return MSSyst.String.Ref;

   -- I made this abstract b/c it has been deprecated
   type String_Output is abstract new Output with record
      Str : Token_Pointer;
   end record;
   procedure Execute (This : in String_Output);
   procedure Compile_Pass1 (This : in String_Output; Gen : access Generate_Interface.Typ'Class);
   function Get_String(This : in String_Output) return MSSyst.String.Ref;

   -- Parameter_List => Output [, Parameter_List | Lambda]
   type Parameter_List is tagged record
      Parameter : Output_Pointer;
      Next      : Parameter_List_Pointer;
   end record;
   function Count_Parameters(L : Parameter_List_Pointer) return Integer;

   -- procedure Set_Prompt is used for compiled code
   procedure Set_Prompt(S : in MSSyst.String.Ref);

end Parse_Tree;
