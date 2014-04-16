using System;
using System.Windows.Forms;
using Microsoft.Win32;
using System.Net;
using System.Threading;

namespace raptor
{
	/// <summary>
	/// Summary description for Autoupdate.
	/// </summary>
	public class Autoupdate
	{
		public Autoupdate()
		{
			//
			// TODO: Add constructor logic here
			//
		}
		static bool result = false;
		static string path;
		static string setup_path;

		public static void Ask_The_Question()
		{
            try
            {
                if (System.IO.File.GetLastWriteTime(path) >
                    System.IO.File.GetLastWriteTime(
                    System.Windows.Forms.Application.ExecutablePath).AddMinutes(30.0))
                {

                    result = true;
                }
            }
            catch
            {
            }
		}
		public static ThreadStart question_delegate =
			new ThreadStart(Ask_The_Question);

		public static bool Autoupdate_Requested()
		{
			try 
			{
				RegistryKey HKCU = Registry.LocalMachine;
				RegistryKey Software = HKCU.OpenSubKey("Software");
				RegistryKey Raptor = Software.OpenSubKey("Raptor");
				bool at_usafa;
				path = (string) Raptor.GetValue("update_exe_path");
				setup_path = (string) Raptor.GetValue("update_setup_path");

				/*Console.WriteLine(System.Windows.Forms.Application.ExecutablePath);
				Console.WriteLine(System.IO.File.GetLastWriteTime(System.Windows.Forms.Application.ExecutablePath));
				Console.WriteLine(System.IO.File.GetLastWriteTime(path));*/
				string s = Dns.GetHostName();

				IPHostEntry h = Dns.GetHostEntry(s);
				at_usafa=false;
				foreach (IPAddress ip in h.AddressList)
				{
					byte[] bytes = ip.GetAddressBytes();

					if (bytes[0]==128 &&
						bytes[1]==236)
					{
						at_usafa=true;
					}
				}

				if (System.Environment.UserDomainName=="USAFA")
				{
					at_usafa=true;
				}
				if (!at_usafa)
				{
					return false;
				}
				// fire off a thread, which will have 4 seconds
				// to determine if there is a newer version
				Thread check_file = new Thread(question_delegate);
				check_file.Start();
				// wait the 4 seconds
				for (int i=0; i<8; i++)
				{
					Thread.Sleep(500);
					if (!check_file.IsAlive)
					{
						break;
					}
				}
				// kill the thread that is checking
				check_file.Abort();

				// if a newer version found, ask if they want it
				if (result)
				{
					bool answer = MessageBox.Show("An update to RAPTOR is available.  Download now?",
						"Update available",MessageBoxButtons.YesNo)==
						DialogResult.Yes;
					if (answer)
					{
						System.Diagnostics.Process.Start(setup_path);
					}
					return answer;
				}
				else
				{
					return false;
				}

			}
			catch (System.Exception exc)
			{
				Console.WriteLine(exc.Message);
				return false;
			}
			
		}
	}
}
