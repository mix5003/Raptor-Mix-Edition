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
using NClass.Java;

namespace NClass.CodeGenerator
{
	public class Generator
	{
		ProjectGenerator projectGenerator;

		/// <exception cref="ArgumentNullException">
		/// <paramref name="project"/> is null.
		/// </exception>
		public Generator(ProjectCore project)
		{
			if (project == null)
				throw new ArgumentNullException("project");

			projectGenerator = CreateProjectGenerator(project);
		}

		protected virtual ProjectGenerator CreateProjectGenerator(ProjectCore project)
		{
			Language language = project.Language;

			if (language == CSharpLanguage.Instance)
				return new CSharpProjectGenerator(project);
			if (language == JavaLanguage.Instance)
				return new JavaProjectGenerator(project);

			throw new ArgumentException("The project is invalid: unknown language.");
		}

		/// <exception cref="ArgumentException">
		/// <paramref name="location"/> or <paramref name="projectName"/>
		/// contain invalid path characters.
		/// </exception>
		/// <exception cref="ArgumentNullException">
		/// <paramref name="location"/> is null.-or-
		/// <paramref name="projectName"/> is null.
		/// </exception>
		public virtual bool Generate(string location, string projectName)
		{
			return projectGenerator.Generate(location, projectName);
		}
	}
}
