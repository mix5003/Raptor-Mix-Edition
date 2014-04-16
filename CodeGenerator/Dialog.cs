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
using System.Windows.Forms;
using NClass.Core;
using NClass.CSharp;
using NClass.Translations;

namespace NClass.CodeGenerator
{
	public partial class Dialog : Form
	{
		ProjectCore project = null;

		public Dialog()
		{
			InitializeComponent();
		}

		private void UpdateTexts()
		{
			this.Text = Strings.GetString("code_generation");
			txtDestination.Text = Strings.GetString("destination");
			btnAddItem.Text = Strings.GetString("button_add_item");
			btnBrowse.Text = Strings.GetString("button_browse");
			btnGenerate.Text = Strings.GetString("button_generate");
			btnCancel.Text = Strings.GetString("button_cancel");
			toolImportList.Text = Strings.GetString("import_list");
			toolMoveUp.Text = Strings.GetString("move_up");
			toolMoveDown.Text = Strings.GetString("move_down");
			toolDelete.Text = Strings.GetString("delete");
			grpCodeStyle.Text = Strings.GetString("code_style");
			chkUseTabs.Text = Strings.GetString("use_tabs");
			lblDestination.Text = Strings.GetString("destination");
			lblIndentSize.Text = Strings.GetString("indent_size");
			lblProjectName.Text = Strings.GetString("project_name");

			txtDestination.Left = lblDestination.Right + 6;
			txtDestination.Width = btnBrowse.Left - txtDestination.Left - 6;
			txtProjectName.Left = lblProjectName.Right + 6;
			txtProjectName.Width = btnBrowse.Right - txtProjectName.Left;
		}
		
		private void UpdateValues()
		{
			if (project != null) {
				txtDestination.Text = project.ProjectDirectory;
				txtProjectName.Text = project.ProjectFileNameWithoutExtension;
				lstImportList.Items.Clear();
				foreach (string importString in Settings.ImportList[project.Language])
					lstImportList.Items.Add(importString);
			}
			chkUseTabs.Checked = Settings.UseTabsForIndents;
			updIndentSize.Value = Settings.IndentSize;
		}

		public void ShowDialog(ProjectCore project)
		{
			this.project = project;

			UpdateTexts();
			UpdateValues();
			ShowDialog();
		}

		private void btnBrowse_Click(object sender, EventArgs e)
		{
			using (FolderBrowserDialog dialog = new FolderBrowserDialog())
			{
				dialog.Description = Strings.GetString("generator_target_dir");
				dialog.SelectedPath = txtDestination.Text;
				if (dialog.ShowDialog() == DialogResult.OK)
					txtDestination.Text = dialog.SelectedPath;
			}
		}

		private void toolMoveUp_Click(object sender, EventArgs e)
		{
			int index = lstImportList.SelectedIndex;
			if (index > 0) {
				object temp = lstImportList.Items[index];
				lstImportList.Items[index] = lstImportList.Items[index - 1];
				lstImportList.Items[index - 1] = temp;
				lstImportList.SelectedIndex--;
			}
		}

		private void toolMoveDown_Click(object sender, EventArgs e)
		{
			int index = lstImportList.SelectedIndex;
			if (index < lstImportList.Items.Count - 1) {
				object temp = lstImportList.Items[index];
				lstImportList.Items[index] = lstImportList.Items[index + 1];
				lstImportList.Items[index + 1] = temp;
				lstImportList.SelectedIndex++;
			}
		}

		private void toolDelete_Click(object sender, EventArgs e)
		{
			if (lstImportList.SelectedItem != null) {
				int selectedIndex = lstImportList.SelectedIndex;
				lstImportList.Items.RemoveAt(selectedIndex);
				if (lstImportList.Items.Count > selectedIndex)
					lstImportList.SelectedIndex = selectedIndex;
				else
					lstImportList.SelectedIndex = lstImportList.Items.Count - 1;
			}
		}

		private void lstImportList_SelectedValueChanged(object sender, EventArgs e)
		{
			bool isSelected = (lstImportList.SelectedItem != null);

			toolMoveUp.Enabled = isSelected && (lstImportList.SelectedIndex > 0);
			toolMoveDown.Enabled = isSelected && 
				(lstImportList.SelectedIndex < lstImportList.Items.Count - 1);
			toolDelete.Enabled = isSelected;
		}

		private void lstImportList_Leave(object sender, EventArgs e)
		{
			lstImportList.SelectedItem = null;
		}

		private void txtNewImport_TextChanged(object sender, EventArgs e)
		{
			btnAddItem.Enabled = (txtNewImport.Text.Length > 0);
		}

		private void txtNewImport_KeyDown(object sender, KeyEventArgs e)
		{
			if (e.KeyCode == Keys.Enter && txtNewImport.Text.Length > 0) {
				lstImportList.Items.Add(txtNewImport.Text);
				txtNewImport.Text = string.Empty;
			}
		}

		private void btnAddItem_Click(object sender, EventArgs e)
		{
			lstImportList.Items.Add(txtNewImport.Text);
			txtNewImport.Text = string.Empty;
			txtNewImport.Focus();
		}

		private void txtProjectName_Validated(object sender, EventArgs e)
		{
			if (txtProjectName.Text.Length == 0)
				txtProjectName.Text = Strings.GetString("untitled");
		}

		private void chkUseTabs_CheckedChanged(object sender, EventArgs e)
		{
			bool useTabs = chkUseTabs.Checked;
			
			lblIndentSize.Enabled = !useTabs;
			updIndentSize.Enabled = !useTabs;
		}

		private void cmdGenerate_Click(object sender, EventArgs e)
		{
			if (project != null) {
				SaveModifiedSettings();

				try {
					Generator generator = new Generator(project);
					string destination = txtDestination.Text;
					string projectName = txtProjectName.Text;

					if (generator.Generate(destination, projectName)) {
						MessageBox.Show(Strings.GetString("code_generation_completed"),
							Strings.GetString("code_generation"), MessageBoxButtons.OK,
							MessageBoxIcon.Information);
					}
					else {
						MessageBox.Show(Strings.GetString("code_generation_failed"),
							Strings.GetString("error"), MessageBoxButtons.OK,
							MessageBoxIcon.Error);
					}
				}
				catch (Exception ex) {
					MessageBox.Show(ex.Message, Strings.GetString("unknown_error"),
						MessageBoxButtons.OK, MessageBoxIcon.Error);
				}
			}
		}

		private void SaveModifiedSettings()
		{
			Settings.DefaultNamespace = txtProjectName.Text;
			Settings.UseTabsForIndents = chkUseTabs.Checked;
			Settings.IndentSize = (int) updIndentSize.Value;

			List<string> importList = new List<string>(lstImportList.Items.Count);
			foreach (object import in lstImportList.Items)
				importList.Add(import.ToString());
			Settings.ImportList[project.Language] = importList;
		}
	}
}