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
using System.Xml;
using System.Text;
using System.Collections.Generic;
using System.Windows.Forms;
using NClass.Core;
using NClass.CSharp;

namespace NClass.CodeGenerator
{
	internal sealed class CSharpProjectGenerator : ProjectGenerator
	{
		/// <exception cref="ArgumentNullException">
		/// <paramref name="project"/> is null.
		/// </exception>
		internal CSharpProjectGenerator(ProjectCore project) : base(project)
		{
		}

		protected override SourceFileGenerator CreateSourceFileGenerator(TypeBase type)
		{
			return new CSharpSourceFileGenerator(type);
		}

		protected override bool GenerateProjectFiles(string location, string projectName)
		{
			try {
				string templatePath = Path.Combine(Application.StartupPath, "template.csproj");
				string projectFileDir = Path.Combine(location, projectName);
				string projectFilePath = Path.Combine(projectFileDir, projectName + ".csproj");

				using (StreamReader reader = new StreamReader(templatePath))
				using (StreamWriter writer = new StreamWriter(projectFilePath))
				{
					while (!reader.EndOfStream) {
						string line = reader.ReadLine();

						line = line.Replace("${RootNamespace}", Settings.DefaultNamespace);
						line = line.Replace("${AssemblyName}", projectName);

						if (line.Contains("${SourceFile}")) {
							foreach (string fileName in FileNames) {
								string newLine = line.Replace("${SourceFile}", fileName);
								writer.WriteLine(newLine);
							}
						}
						else {
							writer.WriteLine(line);
						}
					}
				}

				return true;
			}
			catch {
				return false;
			}
		}
	}
}
