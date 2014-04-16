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
using System.Xml.Serialization;
using System.Collections.Generic;
using System.Windows.Forms;
using System.Text;
using NClass.Core;
using NClass.CSharp;
using NClass.Java;
using NClass.CodeGenerator;
using NClass.Translations;

namespace NClass.GUI
{
	public class Settings
	{
		[XmlInclude(typeof(CSharpLanguage)), XmlInclude(typeof(JavaLanguage))]
		public sealed class GeneralSettings
		{
			const int MaxRecentCount = 5;
			static GeneralSettings currentSettings = new GeneralSettings();

			bool loadLastProject = true;
			bool showFullFilePath = true;
			Language defaultLanguage = JavaLanguage.Instance;
			List<string> recentFiles = new List<string>(MaxRecentCount);

			internal GeneralSettings()
			{
			}

			public bool LoadLastProject
			{
				get { return loadLastProject; }
				set { loadLastProject = value; }
			}

			public string LastProject
			{
				get
				{
					if (RecentFileCount > 0)
						return GetRecentFile(0);
					else
						return null;
				}
			}

			public bool ShowFullFilePath
			{
				get { return showFullFilePath; }
				set { showFullFilePath = value; }
			}

			public Language DefaultLanguage
			{
				get
				{
					return defaultLanguage;
				}
				set
				{
					// To prevent making of other instances
					if (value.Name == "C#")
						defaultLanguage = CSharpLanguage.Instance;
					else if (value.Name == "Java")
						defaultLanguage = JavaLanguage.Instance;
				}
			}

			public string DisplayLanguage
			{
				get { return Strings.Language; }
				set { Strings.Language = value; }
			}

			[XmlArrayItem("File")]
			public List<string> RecentFiles
			{
				get
				{
					return recentFiles;
				}
				set
				{
					if (value != null)
						recentFiles = value;
				}
			}

			public void AddRecentFile(string recentFile)
			{
				if (!File.Exists(recentFile))
					return;

				int index = recentFiles.IndexOf(recentFile);

				if (index >= 0) {
					if (index > 0) {
						string temp = recentFiles[index];
						for (int i = index; i > 0; i--)
							recentFiles[i] = recentFiles[i - 1];
						recentFiles[0] = temp;
					}
				}
				else {
					if (recentFiles.Count < MaxRecentCount)
						recentFiles.Add("");
					for (int i = recentFiles.Count - 2; i >= 0; i--)
						recentFiles[i + 1] = recentFiles[i];
					recentFiles[0] = recentFile;
				}
			}

			/// <exception cref="ArgumentOutOfRangeException">
			/// <paramref name="index"/> is less than 0 or 
			/// is equal to or greater than <see cref="RecentFileCount"/>.
			/// </exception>
			public string GetRecentFile(int index)
			{
				return recentFiles[index];
			}

			internal void RemoveDeadRecents()
			{
				for (int i = 0; i < RecentFiles.Count; i++) {
					if (!File.Exists(RecentFiles[i]))
						RecentFiles.RemoveAt(i--);
				}
			}
		}

		GeneralSettings generalSettings = new GeneralSettings();
		Diagram.Settings.DiagramSettings diagramSettings =
			GUI.Diagram.Settings.CurrentSettings.Diagram;
		CodeGenerator.Settings.CodeGeneratorSettings generatorSettings =
			CodeGenerator.Settings.CurrentSettings.GeneratorSettings;

		public GeneralSettings General
		{
			get { return generalSettings; }
			set { generalSettings = value; }
		}

		[XmlElement("Diagram")]
		public Diagram.Settings.DiagramSettings DiagramSettings
		{
			get { return diagramSettings; }
			set { diagramSettings = value; }
		}

		[XmlElement("CodeGenerator")]
		public CodeGenerator.Settings.CodeGeneratorSettings GeneratorSettings
		{
			get { return generatorSettings; }
			set { generatorSettings = value; }
		}

		static readonly string FilePath = Path.Combine(Application.StartupPath, "settings.xml");
		static Settings currentSettings = new Settings();

		private Settings()
		{
		}

		public static Settings CurrentSettings
		{
			get { return currentSettings; }
		}
	
		public static Diagram.Settings.DiagramSettings Diagram
		{
			get { return currentSettings.diagramSettings; }
		}

		#region Static properties

		public static bool LoadLastProject
		{
			get { return currentSettings.General.LoadLastProject; }
			set { currentSettings.General.LoadLastProject = value; }
		}

		public static string LastProject
		{
			get { return currentSettings.General.LastProject; }
		}

		public static bool ShowFullFilePath
		{
			get { return currentSettings.General.ShowFullFilePath; }
			set { currentSettings.General.ShowFullFilePath = value; }
		}

		public static Language DefaultLanguage
		{
			get { return currentSettings.General.DefaultLanguage; }
			set { currentSettings.General.DefaultLanguage = value; }
		}

		public static string DisplayLanguage
		{
			get { return currentSettings.General.DisplayLanguage; }
			set { currentSettings.General.DisplayLanguage = value; }
		}

		public static int RecentFileCount
		{
			get { return currentSettings.General.RecentFiles.Count; }
		}

		#endregion

		#region Static methods

		public static void AddRecentFile(string recentFile)
		{
			currentSettings.General.AddRecentFile(recentFile);
		}

		/// <exception cref="ArgumentOutOfRangeException">
		/// <paramref name="index"/> is less than 0 or 
		/// is equal to or greater than <see cref="RecentFileCount"/>.
		/// </exception>
		public static string GetRecentFile(int index)
		{
			return currentSettings.General.GetRecentFile(index);
		}

		public static void ClearRecentsList()
		{
			currentSettings.General.RecentFiles.Clear();
		}

		public static bool LoadSettings()
		{
			return LoadSettings(FilePath);
		}

		public static bool LoadSettings(string filePath)
		{
			try {
				using (XmlTextReader reader = new XmlTextReader(filePath))
				{
					XmlSerializer serializer = new XmlSerializer(typeof(Settings));
					Settings settings = (Settings) serializer.Deserialize(reader);

					currentSettings = settings;
					currentSettings.General.RemoveDeadRecents();

					GUI.Diagram.Settings.CurrentSettings.Diagram =
						currentSettings.DiagramSettings;
					CodeGenerator.Settings.CurrentSettings.GeneratorSettings =
						currentSettings.GeneratorSettings;

					return true;
				}
			}
			catch {
				return false;
			}
		}

		public static bool SaveSettings()
		{
			return SaveSettings(FilePath);
		}

		public static bool SaveSettings(string filePath)
		{
			try {
				using (TextWriter writer = new StreamWriter(filePath))
				{
					XmlSerializer serializer = new XmlSerializer(typeof(Settings));
					XmlSerializerNamespaces namespaces = new XmlSerializerNamespaces();
					namespaces.Add("", "");

					serializer.Serialize(writer, currentSettings, namespaces);
					return true;
				}
			}
			catch {
				return false;
			}
		}

		#endregion
	}
}
