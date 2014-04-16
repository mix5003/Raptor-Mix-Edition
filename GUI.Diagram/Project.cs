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
using System.Windows.Forms;
using NClass.Core;
using NClass.GUI.Diagram;
using NClass.Translations;

namespace NClass.GUI.Diagram
{
    [Serializable]
	public sealed class Project : ProjectCore
	{
		bool isDirty = false;
		bool isReadonly = false;

		/// <exception cref="ArgumentNullException">
		/// <paramref name="language"/> is null.
		/// </exception>
		public Project(Language language) : base(language)
		{
		}

		/// <exception cref="IOException">
		/// Could not load the project.
		/// </exception>
		/// <exception cref="InvalidDataException">
		/// The save file is corrupt and could not load.
		/// </exception>
		/// <exception cref="ArgumentException">
		/// <paramref name="fileName"/> is empty string.
		/// </exception>
		public Project(string fileName) : base(fileName)
		{
		}

		public bool IsReadonly
		{
			get
			{
				return isReadonly;
			}
			private set
			{
				if (isReadonly != value) {
					isReadonly = value;
					OnFileStateChanged(EventArgs.Empty);
				}
				else {
					isReadonly = value;
				}
			}
		}

		private DialogResult AskSaveModifying()
		{
			return MessageBox.Show(Strings.GetString("save_changes_confirmation"),
				Strings.GetString("confirmation"),
				MessageBoxButtons.YesNoCancel, MessageBoxIcon.Warning);
		}

		public bool CanClose()
		{
			if (IsDirty && !(IsUntitled && IsEmpty)) {
				switch (AskSaveModifying()) {
					case DialogResult.Yes:
						return Save();
					case DialogResult.No:
						return true;
					case DialogResult.Cancel:
					default:
						return false;
				}
			}
			else {
				return true;
			}
		}

		public override void ClearProject()
		{
			IsDirty = false;
			IsReadonly = false;
			base.ClearProject();
		}

		public override bool NewProject()
		{
			if (CanClose())
				return base.NewProject();
			else
				return false;
		}

		private bool GetReadonlyAttribute(string fileName)
		{
			try {
				FileInfo fileInfo = new FileInfo(fileName);
				return fileInfo.IsReadOnly;
			}
			catch {
				return false;
			}
		}

		public bool Load()
		{
			try {
				if (CanClose()) {
					using (OpenFileDialog dialog = new OpenFileDialog()) {
						dialog.Filter = string.Format(
							"NClass {0} (*.csd; *.jd)|*.csd;*.jd|C# {0} (*.csd)|" +
							"*.csd|Java {0} (*.jd)|*.jd", Strings.GetString("diagrams"));
						dialog.InitialDirectory = ProjectDirectory;
						dialog.ShowReadOnly = true;

						if (dialog.ShowDialog() == DialogResult.OK) {
							IsDirty = false;
							IsReadonly = dialog.ReadOnlyChecked;
							Load(dialog.FileName);
							return true;
						}
					}
				}
			}
			catch (Exception ex) {
				MessageBox.Show(Strings.GetString("error") + ": " + ex.Message,
					Strings.GetString("load"), MessageBoxButtons.OK, MessageBoxIcon.Error);
			}

			return false;
		}

		/// <exception cref="IOException">
		/// Could not load the project.
		/// </exception>
		/// <exception cref="InvalidDataException">
		/// The save file is corrupt and could not load.
		/// </exception>
		/// <exception cref="ArgumentException">
		/// <paramref name="fileName"/> is empty string.
		/// </exception>
		public override void Load(string fileName)
		{
			base.Load(fileName);
			IsDirty = false;
			IsReadonly = GetReadonlyAttribute(fileName);
		}

		public override bool Save()
		{
			if (IsUntitled || IsReadonly) {
				return SaveAs();
			}
			else {
				try {
					base.Save();
					return true;
				}
				catch (IOException ex) {
					MessageBox.Show(Strings.GetString("error") + ": " + ex.Message,
						Strings.GetString("save"), MessageBoxButtons.OK, MessageBoxIcon.Error);
					return SaveAs();
				}
			}
		}

		/// <exception cref="IOException">
		/// Could not save the project.
		/// </exception>
		/// <exception cref="ArgumentException">
		/// <paramref name="fileName"/> is empty string.
		/// </exception>
		public override void Save(string fileName)
		{
			base.Save(fileName);
			IsDirty = false;
			IsReadonly = false;
		}

		public bool SaveAs()
		{
			try {
				using (SaveFileDialog dialog = new SaveFileDialog()) {
					dialog.FileName = ProjectFileNameWithoutExtension;
					dialog.InitialDirectory = ProjectDirectory;
					dialog.Filter = Strings.GetString("diagram_language", Language.Name) +
						"|*" + Language.DefaultFileExtension;

					if (dialog.ShowDialog() == DialogResult.OK) {
						Save(dialog.FileName);
						return true;
					}
				}
			}
			catch (Exception ex) {
				MessageBox.Show(Strings.GetString("error") + ": " + ex.Message,
					Strings.GetString("save_as"), MessageBoxButtons.OK, MessageBoxIcon.Error);
			}

			return false;
		}

		public string GetTitleString(bool showFullFilePath)
		{
			string fileName;

			if (IsUntitled)
				fileName = Strings.GetString("untitled");
			else if (showFullFilePath)
				fileName = ProjectFile;
			else
				fileName = ProjectFileName;

			if (IsDirty) {
				if (IsReadonly)
					return string.Format("{0}* ({1})", fileName, Strings.GetString("readonly"));
				else
					return fileName + "*";
			}
			else {
				if (IsReadonly)
					return string.Format("{0} ({1})", fileName, Strings.GetString("readonly"));
				else
					return fileName;
			}
		}

		public bool IsDirty
		{
			get
			{
				return isDirty;
			}
			set //LATER: make it internal
			{
				if (isDirty != value) {
					isDirty = value;
					OnFileStateChanged(EventArgs.Empty);
				}
			}
		}

		protected override void OnContentChanged(EventArgs e)
		{
			base.OnContentChanged(e);
			IsDirty = true;
		}

		public override string ToString()
		{
			return GetTitleString(true);
		}
	}
}
