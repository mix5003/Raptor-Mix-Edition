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
using System.Drawing;
using System.Windows.Forms;
using NClass.GUI.Diagram;
using NClass.Translations;

namespace NClass.GUI
{
	public sealed partial class OptionsDialog : Form
	{
		static string stylesDir;
		Style savedStyle = null;

		public event EventHandler Applied;
		public event EventHandler CurrentStyleChanged;

		static OptionsDialog()
		{
			try {
				stylesDir = Path.Combine(
					Directory.GetParent(Application.StartupPath).FullName, "styles");
			}
			catch {
				stylesDir = Application.StartupPath;
			}
		}

		public OptionsDialog()
		{
			InitializeComponent();
		}

		private void UpdateTexts()
		{
			this.Text = Strings.GetString("options");
			tabGeneral.Text = Strings.GetString("general");
			tabStyle.Text = Strings.GetString("style");
			grpGeneral.Text = Strings.GetString("general");
			grpDiagram.Text = Strings.GetString("diagram");
			lblLanguage.Text = Strings.GetString("language") + ":";
			chkLoadLastProject.Text = Strings.GetString("load_last_project");
			chkShowFullPath.Text = Strings.GetString("show_full_path");
			btnClearRecents.Text = Strings.GetString("clear_recent_list");
			chkUsePrecisionSnapping.Text = Strings.GetString("use_precision_snapping");
			lblShowChevron.Text = Strings.GetString("show_chevron");
			radAsNeeded.Text = Strings.GetString("as_mouse_passes_over");
			radAlways.Text = Strings.GetString("always");
			radNever.Text = Strings.GetString("never");
			btnLoad.Text = Strings.GetString("button_load");
			btnSave.Text = Strings.GetString("button_save");
			btnOK.Text = Strings.GetString("button_ok");
			btnCancel.Text = Strings.GetString("button_cancel");
			btnApply.Text = Strings.GetString("button_apply");

			cboLanguage.Left = lblLanguage.Left + lblLanguage.Width + 3;
		}

		private void FillLanguages()
		{
			cboLanguage.Items.Clear();
			foreach (string language in Strings.LocalizedLanguages)
				cboLanguage.Items.Add(language);
		}

		private void LoadStyles()
		{
			cboStyles.Items.Clear();
			foreach (Style style in Style.AvaiableStyles) {
				if (!style.IsCurrentStyle)
					cboStyles.Items.Add(style);
			}
		}

		private void LoadSettings()
		{
			// General settings
			cboLanguage.SelectedIndex = cboLanguage.Items.IndexOf(Settings.DisplayLanguage);
			chkLoadLastProject.Checked = Settings.LoadLastProject;
			chkShowFullPath.Checked = Settings.ShowFullFilePath;

			// Diagram settings
			chkUsePrecisionSnapping.Checked = Settings.Diagram.UsePrecisionSnapping;
			if (Settings.Diagram.ShowChevron == NClass.GUI.Diagram.Settings.ChevronMode.AsNeeded)
				radAsNeeded.Checked = true;
			else if (Settings.Diagram.ShowChevron == NClass.GUI.Diagram.Settings.ChevronMode.Always)
				radAlways.Checked = true;
			else
				radNever.Checked = true;

			// Style settings
			savedStyle = (Style) Style.CurrentStyle.Clone();
			stylePropertyGrid.SelectedObject = Style.CurrentStyle;
		}

		private void SaveSettings()
		{
			// General settings
			Settings.DisplayLanguage = cboLanguage.SelectedItem.ToString();
			Settings.LoadLastProject = chkLoadLastProject.Checked;
			Settings.ShowFullFilePath = chkShowFullPath.Checked;

			// Diagram settings
			Settings.Diagram.UsePrecisionSnapping = chkUsePrecisionSnapping.Checked;
			if (radAsNeeded.Checked)
				Settings.Diagram.ShowChevron = NClass.GUI.Diagram.Settings.ChevronMode.AsNeeded;
			else if (radAlways.Checked)
				Settings.Diagram.ShowChevron = NClass.GUI.Diagram.Settings.ChevronMode.Always;
			else
				Settings.Diagram.ShowChevron = NClass.GUI.Diagram.Settings.ChevronMode.Never;

			// Style settings
			Style.SaveCurrentStyle();
			if (savedStyle != null)
				savedStyle.Dispose();
			savedStyle = (Style) Style.CurrentStyle.Clone();
			stylePropertyGrid.SelectedObject = Style.CurrentStyle;

			Settings.SaveSettings();
		}

		protected override void OnLoad(EventArgs e)
		{
			base.OnLoad(e);

			UpdateTexts();
			FillLanguages();
			LoadSettings();
			LoadStyles();
			btnApply.Enabled = false;
		}

		protected override void OnClosed(EventArgs e)
		{
			base.OnClosed(e);
			if (this.DialogResult == DialogResult.Cancel)
				RestoreValidStyle();
		}

		private void RestoreValidStyle()
		{
			Style.CurrentStyle = savedStyle;
			if (CurrentStyleChanged != null)
				CurrentStyleChanged(this, EventArgs.Empty);
		}

		private void SettingsStateChanged(object sender, EventArgs e)
		{
			btnApply.Enabled = true;
		}

		private void stylePropertyGrid_PropertyValueChanged(object s,
			PropertyValueChangedEventArgs e)
		{
			SettingsStateChanged(s, e);
			OnCurrentStyleChanged();
			cboStyles.SelectedIndex = -1;
		}

		private void btnClearRecents_Click(object sender, EventArgs e)
		{
			Settings.ClearRecentsList();
		}

		private void cboStyles_SelectedIndexChanged(object sender, EventArgs e)
		{
			Style style = cboStyles.SelectedItem as Style;
			if (style != null)
				ChangeCurrentStyle(style);
		}

		private void ChangeCurrentStyle(Style style)
		{
			Style.CurrentStyle = style;
			stylePropertyGrid.SelectedObject = Style.CurrentStyle;
			btnApply.Enabled = true;
			OnCurrentStyleChanged();
		}

		private void btnLoad_Click(object sender, EventArgs e)
		{
			using (OpenFileDialog dialog = new OpenFileDialog()) {
				dialog.Filter = Strings.GetString("diagram_style") + " (*.dst)|*.dst";
				dialog.InitialDirectory = stylesDir;

				if (dialog.ShowDialog() == DialogResult.OK) {
					Style style = Style.Load(dialog.FileName);

					if (style == null) {
						MessageBox.Show(
							Strings.GetString("error_could_not_load_file"), Strings.GetString("load"),
							MessageBoxButtons.OK,MessageBoxIcon.Error);
					}
					else {
						cboStyles.Items.Add(style);
						cboStyles.SelectedItem = style;
					}
				}
			}
		}

		private void btnSave_Click(object sender, EventArgs e)
		{
			using (SaveFileDialog dialog = new SaveFileDialog()) {
				dialog.FileName = Style.CurrentStyle.Name;
				dialog.InitialDirectory = stylesDir;
				dialog.Filter = Strings.GetString("diagram_style") + " (*.dst)|*.dst";

				if (dialog.ShowDialog() == DialogResult.OK) {
					if (!Style.CurrentStyle.Save(dialog.FileName)) {
						MessageBox.Show(
							Strings.GetString("error_could_not_save_file"), Strings.GetString("save"),
							MessageBoxButtons.OK, MessageBoxIcon.Error);
					}
					else {
						LoadStyles();
					}
				}
			}
		}

		private void btnOK_Click(object sender, EventArgs e)
		{
			SaveSettings();
			OnApplied();
		}

		private void btnApply_Click(object sender, EventArgs e)
		{
			SaveSettings();
			UpdateTexts();
			OnApplied();
			btnApply.Enabled = false;
		}

		private void OnApplied()
		{
			if (Applied != null)
				Applied(this, EventArgs.Empty);
		}

		private void OnCurrentStyleChanged()
		{
			if (CurrentStyleChanged != null)
				CurrentStyleChanged(this, EventArgs.Empty);
		}
	}
}