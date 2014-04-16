using System;
using Microsoft.Win32;

namespace raptor
{
	/// <summary>
	/// Summary description for Registry_Settings.
	/// </summary>
	public class Registry_Settings
	{
		public Registry_Settings()
		{
			//
			// TODO: Add constructor logic here
			//
		}

		public static bool Ignore_Updates = false;

		public static void Write(string key, string val)
		{
			if (Ignore_Updates)
			{
				return;
			}

			try 
			{
				RegistryKey HKCU = Registry.CurrentUser;
				RegistryKey Software = HKCU.OpenSubKey("Software",true);
				RegistryKey Raptor = Software.CreateSubKey("Raptor");
				Raptor.SetValue(key,val);
			}
			catch
			{
			}
		}
		public static string Read(string key)
		{
			try 
			{
				RegistryKey HKCU = Registry.CurrentUser;
				RegistryKey Software = HKCU.OpenSubKey("Software");
				RegistryKey Raptor = Software.OpenSubKey("Raptor");
				return (string) Raptor.GetValue(key);
			}
			catch
			{
				return null;
			}
		}
		public static string Global_Read(string key)
		{
			try
			{
				RegistryKey HKLM = Registry.LocalMachine;
				RegistryKey Software = HKLM.OpenSubKey("Software");
				RegistryKey Raptor = Software.OpenSubKey("Raptor");
				return (string) Raptor.GetValue(key);
			}
			catch
			{
				return null;
			}
		}
	}
}
