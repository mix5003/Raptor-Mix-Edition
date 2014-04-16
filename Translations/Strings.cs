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
using System.Resources;
using System.Reflection;
using System.Globalization;
using System.Collections.Generic;

namespace NClass.Translations
{
	public static class Strings
	{
		const string DefaultLanguageString = "[Default]";

		static SortedList<string, CultureInfo> localizedCultures;
		static CultureInfo culture = CultureInfo.CurrentUICulture;
		static ResourceManager resourceManager;

		public static event EventHandler LanguageChanged;

		static Strings()
		{
			// Load localization resources
			Assembly assembly = Assembly.GetExecutingAssembly();
			string resourceDir = Path.GetDirectoryName(assembly.Location);
			resourceManager =  new ResourceManager(
				"NClass.Translations.Strings", Assembly.GetExecutingAssembly());

			// Search for localized cultures
			try {
				string[] directories = Directory.GetDirectories(
					resourceDir, "*", SearchOption.TopDirectoryOnly);
				localizedCultures = new SortedList<string, CultureInfo>(directories.Length + 2);

				foreach (string dirPath in directories) {
					DirectoryInfo directory = new DirectoryInfo(dirPath);
					if (directory.Name != "plugins") {
						CultureInfo culture;
						if (TryGetCulture(directory.Name, out culture))
							localizedCultures.Add(culture.EnglishName, culture);
					}
				}
			}
			catch {
				localizedCultures = new SortedList<string, CultureInfo>(2);
			}

			localizedCultures.Add(DefaultLanguageString, CultureInfo.CurrentUICulture);
			localizedCultures.Add("English", new CultureInfo("en"));
		}

		public static CultureInfo Culture
		{
			get
			{
				return culture;
			}
			set
			{
				if (value != null && culture != value) {
					culture = value;
					if (LanguageChanged != null)
						LanguageChanged(null, EventArgs.Empty);
				}
			}
		}

		public static IEnumerable<string> LocalizedLanguages
		{
			get { return localizedCultures.Keys; }
		}

		public static IEnumerable<CultureInfo> LocalizedCultures
		{
			get { return localizedCultures.Values; }
		}

		public static string Language
		{
			get
			{
				if (culture == CultureInfo.CurrentUICulture)
					return DefaultLanguageString;
				else
					return culture.EnglishName;
			}
			set
			{
				if (localizedCultures.ContainsKey(value))
					Culture = localizedCultures[value];
			}
		}

		private static bool TryGetCulture(string cultureString, out CultureInfo culture)
		{
			try {
				culture = new CultureInfo(cultureString);
				return true;
			}
			catch {
				culture = CultureInfo.CurrentUICulture;
				return false;
			}
		}

		public static string GetString(string name)
		{
			string text = resourceManager.GetString(name, culture);

			if (text == null)
				return "";
			else if (text.Contains("\\n"))
				return text.Replace("\\n", "\n");
			else
				return text;
		}

		public static string GetString(string name, object arg)
		{
			return string.Format(GetString(name), arg);
		}

		public static string GetString(string name, params object[] args)
		{
			return string.Format(GetString(name), args);
		}
	}
}
