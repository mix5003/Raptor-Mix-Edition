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
using NClass.Java;

namespace NClass.CodeGenerator
{
	internal sealed class JavaSourceFileGenerator : SourceFileGenerator
	{
		/// <exception cref="NullReferenceException">
		/// <paramref name="type"/> is null.
		/// </exception>
		internal JavaSourceFileGenerator(TypeBase type) : base(type)
		{
		}

		protected override string Extension
		{
			get { return ".java"; }
		}

		protected override void WriteFileContent()
		{
			WritePackageDeclaration();
			WriteImportList();
			WriteType(Type);
		}

		private void WritePackageDeclaration()
		{
			if (!string.IsNullOrEmpty(Settings.DefaultNamespace)) {
				WriteLine("package " + Settings.DefaultNamespace + ";");
				AddBlankLine();
			}
		}

		private void WriteImportList()
		{
			List<string> importList = Settings.ImportList[JavaLanguage.Instance];
			foreach (string importElement in importList)
				WriteLine("import " + importElement + ";");

			if (importList.Count > 0)
				AddBlankLine();
		}

		private void WriteType(TypeBase type)
		{
			if (type is CompositeType)
				WriteCompositeType((CompositeType) type);
			else if (type is EnumType)
				WriteEnum((EnumType) type);
		}

		private void WriteCompositeType(CompositeType type)
		{
			// Writing type declaration
			WriteLine(type.GetDeclarationLine() + " {");
			AddBlankLine();
			IndentLevel++;

			if (type is ClassType) {
				foreach (TypeBase nestedType in ((ClassType) type).NestedChilds) {
					WriteType(nestedType);
					AddBlankLine();
				}
			}

			if (type.FieldCount > 0) {
				foreach (Field field in type.Fields)
					WriteField(field);
				AddBlankLine();
			}

			if (type.OperationCount > 0) {
				foreach (Method method in type.Operations) {
					WriteMethod(method);
					AddBlankLine();
				}
			}

			// Writing closing bracket of the type block
			IndentLevel--;
			WriteLine("}");
		}

		private void WriteEnum(EnumType _enum)
		{
			// Writing type declaration
			WriteLine(_enum.GetDeclarationLine() + " {");
			AddBlankLine();
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

		private void WriteField(Field field)
		{
			WriteLine(field.GetDeclarationLine());
		}

		private void WriteMethod(Method method)
		{
			if (method.HasBody) {
				WriteLine(method.GetDeclarationLine() + " {");
				IndentLevel++;
				AddBlankLine(true);
				IndentLevel--;
				WriteLine("}");
			}
			else{
				WriteLine(method.GetDeclarationLine());
			}
		}
	}
}
