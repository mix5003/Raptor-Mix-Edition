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
using System.Drawing;
using System.Windows.Forms;
using System.Xml.Serialization;
using NClass.Core;

namespace NClass.GUI.Diagram
{
	public class Settings
	{
		public enum ChevronMode
		{
			AsNeeded = 0,
			Always,
			Never
		}

		public sealed class DiagramSettings
		{
			int diagramWidth = 5000;
			int diagramHeight = 4000;
			bool usePrecisionSnapping = true;

			bool isPublicVisible = true;
			bool isProtintVisible = true;
			bool isInternalVisible = true;
			bool isProtectedVisible = true;
			bool isPrivateVisible = true;

			ChevronMode showChevron = ChevronMode.AsNeeded;
			bool showType = true;
			bool showParameters = true;
			bool showParameterNames = true;
			bool showInitialValue = true;

			internal DiagramSettings()
			{
			}

			public int DiagramWidth
			{
				get
				{
					return diagramWidth;
				}
				set
				{
					if (value == 0)
						diagramWidth = 5000;
					else
						diagramWidth = value;
				}
			}

			public int DiagramHeight
			{
				get
				{
					return diagramHeight;
				}
				set
				{
					if (value == 0)
						diagramHeight = 4000;
					else
						diagramHeight = value;
				}
			}

			public bool UsePrecisionSnapping
			{
				get { return usePrecisionSnapping; }
				set { usePrecisionSnapping = value; }
			}

			public ChevronMode ShowChevron
			{
				get { return showChevron; }
				set { showChevron = value; }
			}

			[Obsolete]
			public bool IsPublicVisible
			{
				get { return isPublicVisible; }
				set { isPublicVisible = value; }
			}

			[Obsolete]
			public bool IsProtintVisible
			{
				get { return isProtintVisible; }
				set { isProtintVisible = value; }
			}

			[Obsolete]
			public bool IsInternalVisible
			{
				get { return isInternalVisible; }
				set { isInternalVisible = value; }
			}

			[Obsolete]
			public bool IsProtectedVisible
			{
				get { return isProtectedVisible; }
				set { isProtectedVisible = value; }
			}

			[Obsolete]
			public bool IsPrivateVisible
			{
				get { return isPrivateVisible; }
				set { isPrivateVisible = value; }
			}

			public bool ShowType
			{
				get { return showType; }
				set { showType = value; }
			}

			public bool ShowParameters
			{
				get
				{
					return showParameters;
				}
				set
				{
					if (!value)
						showParameterNames = false;
					showParameters = value;
				}
			}

			public bool ShowParameterNames
			{
				get
				{
					return showParameterNames;
				}
				set
				{
					if (value)
						showParameters = true;
					showParameterNames = value;
				}
			}

			public bool ShowInitialValue
			{
				get { return showInitialValue; }
				set { showInitialValue = value; }
			}
		}

		DiagramSettings diagramSettings = new DiagramSettings();

		public DiagramSettings Diagram
		{
			get { return diagramSettings; }
			set { diagramSettings = value; }
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
	
		#region Static properties

		public static int DiagramWidth
		{
			get { return currentSettings.Diagram.DiagramWidth; }
			set { currentSettings.Diagram.DiagramWidth = value; }
		}

		public static int DiagramHeight
		{
			get { return currentSettings.Diagram.DiagramHeight; }
			set { currentSettings.Diagram.DiagramHeight = value; }
		}

		public static bool UsePrecisionSnapping
		{
			get { return currentSettings.Diagram.UsePrecisionSnapping; }
			set { currentSettings.Diagram.UsePrecisionSnapping = value; }
		}

		public static ChevronMode ShowChevron
		{
			get { return currentSettings.Diagram.ShowChevron; }
			set { currentSettings.Diagram.ShowChevron = value; }
		}

		public static bool IsPublicVisible
		{
			get { return currentSettings.Diagram.IsPublicVisible; }
			set { currentSettings.Diagram.IsPublicVisible = value; }
		}

		public static bool IsProtintVisible
		{
			get { return currentSettings.Diagram.IsProtintVisible; }
			set { currentSettings.Diagram.IsProtintVisible = value; }
		}

		public static bool IsInternalVisible
		{
			get { return currentSettings.Diagram.IsInternalVisible; }
			set { currentSettings.Diagram.IsInternalVisible = value; }
		}

		public static bool IsProtectedVisible
		{
			get { return currentSettings.Diagram.IsProtectedVisible; }
			set { currentSettings.Diagram.IsProtectedVisible = value; }
		}

		public static bool IsPrivateVisible
		{
			get { return currentSettings.Diagram.IsPrivateVisible; }
			set { currentSettings.Diagram.IsPrivateVisible = value; }
		}

		public static bool ShowType
		{
			get { return currentSettings.Diagram.ShowType; }
			set { currentSettings.Diagram.ShowType = value; }
		}

		public static bool ShowParameters
		{
			get { return currentSettings.Diagram.ShowParameters; }
			set { currentSettings.Diagram.ShowParameters = value; }
		}

		public static bool ShowParameterNames
		{
			get { return currentSettings.Diagram.ShowParameterNames; }
			set { currentSettings.Diagram.ShowParameterNames = value; }
		}

		public static bool ShowInitialValue
		{
			get { return currentSettings.Diagram.ShowInitialValue; }
			set { currentSettings.Diagram.ShowInitialValue = value; }
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
