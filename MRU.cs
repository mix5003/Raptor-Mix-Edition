using System;
using Microsoft.Win32;

namespace raptor
{
	/// <summary>
	/// Summary description for MRU.
	/// </summary>
	public class MRU
	{
		public MRU()
		{
		}
		public static void Update_MRU_Menus(Visual_Flow_Form form)
		{
			try 
			{
				RegistryKey HKCU = Registry.CurrentUser;
				RegistryKey Software = HKCU.OpenSubKey("Software");
				RegistryKey Raptor = Software.OpenSubKey("Raptor");
				string MRU1 = (string) Raptor.GetValue("MRU1");
				form.menuMRU1.Text = "&1 - " + MRU1;
				string MRU2 = (string) Raptor.GetValue("MRU2");
				form.menuMRU2.Text = "&2 - " + MRU2;
				string MRU3 = (string) Raptor.GetValue("MRU3");
				form.menuMRU3.Text = "&3 - " + MRU3;
				string MRU4 = (string) Raptor.GetValue("MRU4");
				form.menuMRU4.Text = "&4 - " + MRU4;
                string MRU5 = (string)Raptor.GetValue("MRU5");
                form.menuMRU5.Text = "&5 - " + MRU5;
                string MRU6 = (string)Raptor.GetValue("MRU6");
                form.menuMRU6.Text = "&6 - " + MRU6;
                string MRU7 = (string)Raptor.GetValue("MRU7");
                form.menuMRU7.Text = "&7 - " + MRU7;
                string MRU8 = (string)Raptor.GetValue("MRU8");
                form.menuMRU8.Text = "&8 - " + MRU8;
                string MRU9 = (string)Raptor.GetValue("MRU9");
                form.menuMRU9.Text = "&9 - " + MRU9;
            }
			catch
			{
			}
		}

		public static string Get_MRU_Registry(int i)
		{
			try 
			{
				RegistryKey HKCU = Registry.CurrentUser;
				RegistryKey Software = HKCU.OpenSubKey("Software");
				RegistryKey Raptor = Software.OpenSubKey("Raptor");
				switch (i) 
				{
					case 1:
						return (string) Raptor.GetValue("MRU1");
					case 2:
						return (string) Raptor.GetValue("MRU2");
					case 3:
						return (string) Raptor.GetValue("MRU3");
					case 4:
						return (string) Raptor.GetValue("MRU4");
                    case 5:
                        return (string)Raptor.GetValue("MRU5");
                    case 6:
                        return (string)Raptor.GetValue("MRU6");
                    case 7:
                        return (string)Raptor.GetValue("MRU7");
                    case 8:
                        return (string)Raptor.GetValue("MRU8");
                    case 9:
                        return (string)Raptor.GetValue("MRU9");
                    default:
						return null;
				}
			}
			catch 
			{
				return null;
			}
		}
		public static void Add_To_MRU_Registry(string name)
		{
            try
            {
                string[] MRU = new string[10];
                RegistryKey HKCU = Registry.CurrentUser;
                RegistryKey Software = HKCU.OpenSubKey("Software", true);
                RegistryKey Raptor = Software.CreateSubKey("Raptor");

                for (int i = 1; i < 10; i++)
                {
                    try
                    {
                        MRU[i] = (string)Raptor.GetValue("MRU" + i);
                    }
                    catch
                    {
                        MRU[i] = "";
                    }
                }
                for (int i = 1; i < 10; i++)
                {
                    if ((MRU[i] != null) && (MRU[i].ToLower().CompareTo(name.ToLower()) == 0))
                    {
                        string temp = MRU[i];
                        for (int j = i; j >= 2; j--)
                        {
                            MRU[j] = MRU[j - 1];
                            Raptor.SetValue("MRU" + j, MRU[j]);
                        }
                        MRU[1] = temp;
                        Raptor.SetValue("MRU1", temp);
                        return;
                    }
                }
                Raptor.SetValue("MRU1", name);
                for (int i = 2; i < 10; i++)
                {
                    if (MRU[i - 1] != null)
                    {
                        Raptor.SetValue("MRU" + i, MRU[i - 1]);
                    }
                }
            }
            catch
            {
            }
		}
	}
}
