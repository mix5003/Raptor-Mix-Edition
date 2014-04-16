// NClass - Free class diagram editor
// Copyright (C) 2006-2007 Balazs Tihanyi
// 
// This program is free software; you can redistribute it and/or modify it under 
// the terms of the GNU General Public License as published by the Free Software 
// Foundation; either version 3 of the License, or (at your option) any later version.
// 
// This program is distributed in the hope that it will be useful, but WITHOUT 
// ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS 
// FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License along with 
// this program; if not, write to the Free Software Foundation, Inc., 
// 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA

using System;
using System.Collections.Generic;
using NClass.Core;
using NClass.CSharp;

namespace NClass.CodeGenerator
{
	internal sealed class CSharpSourceFileGenerator : SourceFileGenerator
	{
		/// <exception cref="NullReferenceException">
		/// <paramref name="type"/> is null.
		/// </exception>
		internal CSharpSourceFileGenerator(TypeBase type) : base(type)
		{
		}

		protected override string Extension
		{
			get { return ".cs"; }
		}

		protected override void WriteFileContent()
		{
			WriteUsings();
			OpenNamespace();
			WriteType(Type);
			CloseNamespace();
		}

		private void WriteUsings()
		{
			List<string> importList = Settings.ImportList[CSharpLanguage.Instance];
			foreach (string usingElement in importList)
				WriteLine("using " + usingElement + ";");

			if (importList.Count > 0)
				AddBlankLine();
		}

		private void OpenNamespace()
		{
			if (!string.IsNullOrEmpty(Settings.DefaultNamespace)) {
				WriteLine("namespace " + Settings.DefaultNamespace);
				WriteLine("{");
				IndentLevel++;
			}
		}

		private void CloseNamespace()
		{
			if (!string.IsNullOrEmpty(Settings.DefaultNamespace)) {
				IndentLevel--;
				WriteLine("}");
			}
		}

		private void WriteType(TypeBase type)
		{
			if (type is CompositeType)
				WriteCompositeType((CompositeType) type);
			else if (type is EnumType)
				WriteEnum((EnumType) type);
			else if (type is DelegateType)
				WriteDelegate((DelegateType) type);
		}

		private void WriteCompositeType(CompositeType type)
		{
			// Writing type declaration
			WriteLine(type.GetDeclarationLine());
			WriteLine("{");
			IndentLevel++;

			if (type is ClassType) {
				foreach (TypeBase nestedType in ((ClassType) type).NestedChilds) {
					WriteType(nestedType);
					AddBlankLine();
				}
			}

			if (type.SupportsFields) {
				foreach (Field field in type.Fields)
					WriteField(field);
			}

			bool needBlankLine = (type.FieldCount > 0 && type.OperationCount > 0);

			foreach (Operation operation in type.Operations) {
				if (needBlankLine)
					AddBlankLine();
				needBlankLine = true;

				WriteOperation(operation);
			}

			// Writing closing bracket of the type block
			IndentLevel--;
			WriteLine("}");
		}

		private void WriteEnum(EnumType _enum)
		{
			// Writing type declaration
			WriteLine(_enum.GetDeclarationLine());
			WriteLine("{");
			IndentLevel++;

			int valuesRemained = _enum.ValueCount;
			foreach (EnumValue value in _enum.Values) {
				if (--valuesRemained > 0)
					WriteLine(value.GetDeclarationLine() + ",");
				else
					WriteLine(value.GetDeclarationLine());
			}

			// Writing closing bracket of the type block
			IndentLevel--;
			WriteLine("}");
		}

		private void WriteDelegate(DelegateType _delegate)
		{
			WriteLine(_delegate.GetDeclarationLine());
		}

		private void WriteField(Field field)
		{
			WriteLine(field.GetDeclarationLine());
		}

		private void WriteOperation(Operation operation)
		{
			WriteLine(operation.GetDeclarationLine());

			if (operation is Property) {
				WriteProperty((Property) operation);
			}
			else if (operation.HasBody)
			{
				if (operation is Event) {
					WriteLine("{");
					IndentLevel++;
					WriteLine("add {  }");
					WriteLine("remove {  }");
					IndentLevel--;
					WriteLine("}");
				}
				else {
					WriteLine("{");
					IndentLevel++;
					AddBlankLine(true);
					IndentLevel--;
					WriteLine("}");
				}
			}
		}

		private void WriteProperty(Property property)
		{
			WriteLine("{");
			IndentLevel++;

			if (!property.IsWriteonly) {
				if (property.HasImplementation) {
					WriteLine("get");
					WriteLine("{");
					IndentLevel++;
					AddBlankLine(true);
					IndentLevel--;
					WriteLine("}");
				}
				else {
					WriteLine("get;");
				}
			}
			if (!property.IsReadonly) {
				if (property.HasImplementation) {
					WriteLine("set");
					WriteLine("{");
					IndentLevel++;
					AddBlankLine(true);
					IndentLevel--;
					WriteLine("}");
				}
				else {
					WriteLine("set;");
				}
			}

			IndentLevel--;
			WriteLine("}");
		}
	}
}
