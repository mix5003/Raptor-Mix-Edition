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

namespace NClass.CodeGenerator
{
	public abstract class ProjectGenerator
	{
		ProjectCore project;
		List<string> fileNames = new List<string>();

		/// <exception cref="ArgumentNullException">
		/// <paramref name="project"/> is null.
		/// </exception>
		protected ProjectGenerator(ProjectCore project)
		{
			if (project == null)
				throw new ArgumentNullException("project");

			this.project = project;
		}

		protected List<string> FileNames
		{
			get { return fileNames; }
		}

		/// <exception cref="ArgumentException">
		/// <paramref name="location"/> or <paramref name="projectName"/>
		/// contain invalid path characters.
		/// </exception>
		/// <exception cref="ArgumentNullException">
		/// <paramref name="location"/> is null.-or-
		/// <paramref name="projectName"/> is null.
		/// </exception>
		internal bool Generate(string location, string projectName)
		{
			bool succeed = true;

			succeed &= GenerateSourceFiles(location, projectName);
			succeed &= GenerateProjectFiles(location, projectName);

			return succeed;
		}

		private bool GenerateSourceFiles(string location, string projectName)
		{
			bool succeed = true;
			location = Path.Combine(location, projectName);

			fileNames.Clear();
			foreach (IEntity entity in project.Entities) {
				TypeBase type = entity as TypeBase;

				if (type != null && !type.IsNested) {
					SourceFileGenerator sourceFile = CreateSourceFileGenerator(type);

					try {
						string fileName = sourceFile.Generate(location);
						fileNames.Add(fileName);
					}
					catch (FileGenerationException) {
						succeed = false;
					}
				}
			}
			SourceFileGenerator.FinishWork();

			return succeed;
		}

		protected abstract SourceFileGenerator CreateSourceFileGenerator(TypeBase type);

		protected abstract bool GenerateProjectFiles(string location, string projectName);
	}
}
