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
using NClass.Core;
using NClass.CSharp;
using NClass.Java;

namespace NClass.CodeGenerator
{
	public class Settings
	{
		[XmlInclude(typeof(CSharpLanguage))]
		public class CodeGeneratorSettings
		{
			Dictionary<Language, List<string>> importList;
			int indentSize = 4;
			bool useTabsForIndents = true;

			internal CodeGeneratorSettings()
			{
				importList = new Dictionary<Language, List<string>>();
				importList.Add(CSharpLanguage.Instance, new List<string>(
					new string[] {
						"System",
						"System.Xml",
						"System.Collections.Generic"
					}
				));
				importList.Add(JavaLanguage.Instance, new List<string>(
					new string[] {
						"java.io.*",
						"java.util.*"
					}
				));
			}

			[XmlArrayItem("Value")]
			public string[] CSharpImportList
			{
				get
				{
					return importList[CSharpLanguage.Instance].ToArray();
				}
				set
				{
					importList[CSharpLanguage.Instance].Clear();
					importList[CSharpLanguage.Instance].AddRange(value);
				}
			}

			[XmlArrayItem("Value")]
			public string[] JavaImportList
			{
				get
				{
					return importList[JavaLanguage.Instance].ToArray();
				}
				set
				{
					importList[JavaLanguage.Instance].Clear();
					importList[JavaLanguage.Instance].AddRange(value);
				}
			}

			[XmlIgnore]
			public Dictionary<Language, List<string>> ImportList
			{
				get { return importList; }
			}

			public int IndentSize
			{
				get
				{
					return indentSize;
				}
				set
				{
					if (value >= 0)
						indentSize = value;
				}
			}

			public bool UseTabsForIndents
			{
				get { return useTabsForIndents; }
				set { useTabsForIndents = value; }
			}
		}

		CodeGeneratorSettings generatorSettings = new CodeGeneratorSettings();

		[XmlElement("CodeGenerator")]
		public CodeGeneratorSettings GeneratorSettings
		{
			get { return generatorSettings; }
			set { generatorSettings = value; }
		}

		static string defaultNamespace = null;
		static Settings currentSettings = new Settings();
		static readonly string FilePath = Path.Combine(Application.StartupPath, "settings.xml");

		private Settings()
		{
		}

		public static Settings CurrentSettings
		{
			get { return currentSettings; }
		}
	
		#region Static properties

		public static Dictionary<Language, List<string>> ImportList
		{
			get { return currentSettings.GeneratorSettings.ImportList; }
		}

		public static string DefaultNamespace
		{
			get { return defaultNamespace; }
			set { defaultNamespace = value; }
		}

		public static int IndentSize
		{
			get { return currentSettings.GeneratorSettings.IndentSize; }
			set { currentSettings.GeneratorSettings.IndentSize = value; }
		}

		public static bool UseTabsForIndents
		{
			get { return currentSettings.GeneratorSettings.UseTabsForIndents; }
			set { currentSettings.GeneratorSettings.UseTabsForIndents = value; }
		}

		#endregion

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
	}
}
