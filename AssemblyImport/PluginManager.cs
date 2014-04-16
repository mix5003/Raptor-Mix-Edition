using System;
using System.Collections.Generic;
using System.Text;
using System.Windows.Forms;
using NClass.Core;

namespace NClass.AssemblyImport
{
	public class PluginManager : IProjectPlugin
	{
		public void Launch(ProjectCore project)
		{
			if (project == null)
				return;

			if (project.Language.Name != "C#") {
				MessageBox.Show("The " + project.Language.Name + " language is not supported.",
					"Assembly Importer", MessageBoxButtons.OK, MessageBoxIcon.Information);
				return;
			}

			string fileName;
			using (OpenFileDialog dialog = new OpenFileDialog()) {
				dialog.Filter = "Assemblies (*.exe, *.dll)|*.exe;*.dll";
				if (dialog.ShowDialog() == DialogResult.Cancel)
					return;
				fileName = dialog.FileName;
			}

			ImportSettings settings = new ImportSettings();
			using (ImportSettingsForm settingsForm = new ImportSettingsForm(settings)) {
				if (settingsForm.ShowDialog() == DialogResult.OK && project.NewProject()) {
					NETImport importer = new NETImport(project, settings);
					importer.ImportAssembly(fileName);
				}
			}
		}

		public string Name
		{
			get { return "Assembly Importer"; }
		}

		public string Author
		{
			get { return "Malte Ried"; }
		}

		public string MenuText
		{
			get { return "&Import assembly..."; }
		}
	}
}
