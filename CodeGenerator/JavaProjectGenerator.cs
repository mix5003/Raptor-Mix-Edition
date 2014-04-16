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
using System.IO;
using System.Collections.Generic;
using NClass.Core;
using NClass.CSharp;

namespace NClass.CodeGenerator
{
	internal sealed class JavaProjectGenerator : ProjectGenerator
	{
		/// <exception cref="ArgumentNullException">
		/// <paramref name="project"/> is null.
		/// </exception>
		internal JavaProjectGenerator(ProjectCore project) : base(project)
		{
		}

		protected override SourceFileGenerator CreateSourceFileGenerator(TypeBase type)
		{
			return new JavaSourceFileGenerator(type);
		}

		protected override bool GenerateProjectFiles(string location, string projectName)
		{
			return true;
		}
	}
}
