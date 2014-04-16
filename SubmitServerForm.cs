using System;
using System.Drawing;
using System.Collections;
using System.ComponentModel;
using System.Windows.Forms;
using System.Diagnostics;

namespace raptor
{
	/// <summary>
	/// Summary description for SubmitServerForm.
	/// </summary>
	public class SubmitServerForm : System.Windows.Forms.Form
	{
		private System.Windows.Forms.Label label1;
		private System.Windows.Forms.Label label2;
		private System.Windows.Forms.Label label3;
		private System.Windows.Forms.Button refreshButton;
		private System.Windows.Forms.TextBox textBoxServer;
		private System.Windows.Forms.TextBox textBoxPort;
		private System.Windows.Forms.ListBox assignmentsList;
		/// <summary>
		/// Required designer variable.
		/// </summary>
		private System.ComponentModel.Container components = null;
		private System.Windows.Forms.Button OKbutton;
		private System.Windows.Forms.Button cancelButton;
		
		private static void Connect_To_Server(string server, int port)
		{
            System.Net.IPAddress ipaddr;

            try
            {
                ipaddr = System.Net.IPAddress.Parse(server);
            }
            catch
            {
                System.Net.IPHostEntry iph =
                    System.Net.Dns.GetHostEntry(server);
                ipaddr = iph.AddressList[0];
            }
			raptor_files_pkg.network_redirect(
				ipaddr.ToString(),
				port,
                true);
		}
        public static Process process;
        private static void StopProcessRedirect()
        {
            if (process != null && process.HasExited == false)
            {
                process.Kill();
            }
            raptor_files_pkg.stop_process_redirect();
        }
        private static void Connect_To_Executable()
        {
            StopProcessRedirect();
            // Start a new process for the cmd
            process = new Process();
            process.StartInfo.UseShellExecute = false;
            process.StartInfo.RedirectStandardOutput = true;
            process.StartInfo.RedirectStandardError = true;
            process.StartInfo.RedirectStandardInput = true;
            process.StartInfo.CreateNoWindow = true;
            if (System.IO.Directory.Exists("r:\\java\\jre1.6.0_07\\bin"))
            {
                process.StartInfo.FileName = "r:\\java\\jre1.6.0_07\\bin\\java.exe";
            }
            else if (System.IO.Directory.Exists("x:\\programs\\java\\jre1.6.0_07\\bin"))
            {
                process.StartInfo.FileName = "x:\\programs\\java\\jre1.6.0_07\\bin\\java.exe";
            }
            else
            {
                process.StartInfo.FileName = "y:\\programs\\java\\jre1.6.0_07\\bin\\java.exe";
            }
            //process.StartInfo.FileName = "c:\\program files\\java\\jre1.6.0_07\\bin\\java.exe";
            //process.StartInfo.FileName = "x:\\programs\\raptor\\perl\\bin\\perl.exe";
            //process.StartInfo.FileName = "c:\\cygwin\\bin\\perl.exe";
            process.StartInfo.Arguments = "-jar AutoGraderServer-TE.jar";
            //process.StartInfo.Arguments = "autograder.pl -c -l demo_list.txt";
            //process.StartInfo.WorkingDirectory = "c:\\d\\temp";
            if (System.IO.Directory.Exists("b:\\raptor\\rage"))
            {
                process.StartInfo.WorkingDirectory = "b:\\raptor\\rage";
            }
            else if (System.IO.Directory.Exists("x:\\programs\\raptor\\rage"))
            {
                process.StartInfo.WorkingDirectory = "x:\\programs\\raptor\\rage";
            }
            else
            {
                process.StartInfo.WorkingDirectory = "y:\\programs\\raptor\\rage";
            }
            process.Start();
            raptor_files_pkg.process_redirect(process);
        }

        private void Refresh_List_Executable()
        {
            string s;
            process.StandardInput.WriteLine("DIRECTORY");
            while (process.StandardOutput.EndOfStream == false)
            {
                s = process.StandardOutput.ReadLine();
                if (s != "" && s != "EOF")
                {
                    this.assignmentsList.Items.Add(s);
                }
            }
            if (this.assignmentsList.Items.Count == 0)
            {
                this.assignmentsList.Items.Add("server unavailable");
            }
        }
		private void Refresh_List()
		{
            int i = 0;
			this.assignmentsList.Items.Clear();
            if (Component.BARTPE)
            {
                Connect_To_Executable();
                Refresh_List_Executable();
                return;
            }
			try 
			{
				try
				{
					Connect_To_Server(this.textBoxServer.Text,
						System.Int32.Parse(this.textBoxPort.Text));
				}
				catch
				{
					this.assignmentsList.Items.Add("Unable to connect");
					throw new System.Exception();
				}
				try
				{
					string x = "DIRECTORY\r\n";
					byte[] buffer = System.Text.Encoding.ASCII.GetBytes(x);
					raptor_files_pkg.current_socket.Send(buffer);
				}
				catch
				{
					this.assignmentsList.Items.Add("Server terminated abnormally");
					throw new System.Exception();
				}		
				try 
				{
					// loop until EOF at end of input
					while (true) 
					{
						string s = raptor_files_pkg.get_line();
						this.assignmentsList.Items.Add(s);
                        i++;
					}
				}
				catch (System.Exception e)
				{
					if (this.assignmentsList.Items.Count==0) 
					{
						this.assignmentsList.Items.Add("No assignments returned");
						throw new System.Exception();
					}
				}
				try
				{
					StopRedirection();
				}
				catch {}		
			}
			catch
			{
				try
				{
					StopRedirection();
				}
				catch {}		
			}
			if (this.assignmentsList.Items.Count==0) 
			{
				this.assignmentsList.Items.Add("server unavailable");
			}
		}

		public SubmitServerForm()
		{
			//
			// Required for Windows Form Designer support
			//
			InitializeComponent();
            if (Component.BARTPE)
            {
                this.textBoxServer.Text = "localhost";
                this.textBoxPort.Enabled = false;
            }
			string ts = Registry_Settings.Read("test_server");
			string tp = Registry_Settings.Read("test_port");
			if (ts!=null)
			{
				this.textBoxServer.Text = ts;
			}
			if (tp!=null)
			{
				this.textBoxPort.Text = tp;
			}
			this.Refresh_List();

		}

		/// <summary>
		/// Clean up any resources being used.
		/// </summary>
		protected override void Dispose( bool disposing )
		{
			if( disposing )
			{
				if(components != null)
				{
					components.Dispose();
				}
			}
			base.Dispose( disposing );
		}

		#region Windows Form Designer generated code
		/// <summary>
		/// Required method for Designer support - do not modify
		/// the contents of this method with the code editor.
		/// </summary>
		private void InitializeComponent()
		{
            this.textBoxServer = new System.Windows.Forms.TextBox();
            this.textBoxPort = new System.Windows.Forms.TextBox();
            this.label1 = new System.Windows.Forms.Label();
            this.label2 = new System.Windows.Forms.Label();
            this.label3 = new System.Windows.Forms.Label();
            this.cancelButton = new System.Windows.Forms.Button();
            this.refreshButton = new System.Windows.Forms.Button();
            this.assignmentsList = new System.Windows.Forms.ListBox();
            this.OKbutton = new System.Windows.Forms.Button();
            this.SuspendLayout();
            // 
            // textBoxServer
            // 
            this.textBoxServer.Location = new System.Drawing.Point(64, 16);
            this.textBoxServer.Name = "textBoxServer";
            this.textBoxServer.Size = new System.Drawing.Size(144, 20);
            this.textBoxServer.TabIndex = 2;
            this.textBoxServer.Text = "cssun1.usafa.edu";
            // 
            // textBoxPort
            // 
            this.textBoxPort.Location = new System.Drawing.Point(64, 40);
            this.textBoxPort.MaxLength = 6;
            this.textBoxPort.Name = "textBoxPort";
            this.textBoxPort.Size = new System.Drawing.Size(144, 20);
            this.textBoxPort.TabIndex = 3;
            this.textBoxPort.Text = "4242";
            // 
            // label1
            // 
            this.label1.Location = new System.Drawing.Point(16, 16);
            this.label1.Name = "label1";
            this.label1.Size = new System.Drawing.Size(40, 16);
            this.label1.TabIndex = 2;
            this.label1.Text = "Server";
            // 
            // label2
            // 
            this.label2.Location = new System.Drawing.Point(16, 40);
            this.label2.Name = "label2";
            this.label2.Size = new System.Drawing.Size(32, 16);
            this.label2.TabIndex = 3;
            this.label2.Text = "Port";
            // 
            // label3
            // 
            this.label3.Location = new System.Drawing.Point(24, 80);
            this.label3.Name = "label3";
            this.label3.Size = new System.Drawing.Size(136, 16);
            this.label3.TabIndex = 5;
            this.label3.Text = "Assignments Available";
            // 
            // cancelButton
            // 
            this.cancelButton.Location = new System.Drawing.Point(152, 264);
            this.cancelButton.Name = "cancelButton";
            this.cancelButton.Size = new System.Drawing.Size(88, 32);
            this.cancelButton.TabIndex = 4;
            this.cancelButton.Text = "Cancel";
            this.cancelButton.Click += new System.EventHandler(this.cancel_Click);
            // 
            // refreshButton
            // 
            this.refreshButton.Location = new System.Drawing.Point(216, 24);
            this.refreshButton.Name = "refreshButton";
            this.refreshButton.Size = new System.Drawing.Size(64, 32);
            this.refreshButton.TabIndex = 1;
            this.refreshButton.Text = "Check";
            this.refreshButton.Click += new System.EventHandler(this.refreshButton_Click);
            // 
            // assignmentsList
            // 
            this.assignmentsList.HorizontalScrollbar = true;
            this.assignmentsList.Location = new System.Drawing.Point(32, 104);
            this.assignmentsList.Name = "assignmentsList";
            this.assignmentsList.Size = new System.Drawing.Size(208, 134);
            this.assignmentsList.Sorted = true;
            this.assignmentsList.TabIndex = 6;
            // 
            // OKbutton
            // 
            this.OKbutton.Location = new System.Drawing.Point(56, 264);
            this.OKbutton.Name = "OKbutton";
            this.OKbutton.Size = new System.Drawing.Size(64, 32);
            this.OKbutton.TabIndex = 7;
            this.OKbutton.Text = "Ok";
            this.OKbutton.Click += new System.EventHandler(this.OKbutton_Click);
            // 
            // SubmitServerForm
            // 
            this.AutoScaleBaseSize = new System.Drawing.Size(5, 13);
            this.ClientSize = new System.Drawing.Size(292, 318);
            this.Controls.Add(this.OKbutton);
            this.Controls.Add(this.assignmentsList);
            this.Controls.Add(this.refreshButton);
            this.Controls.Add(this.cancelButton);
            this.Controls.Add(this.label3);
            this.Controls.Add(this.label2);
            this.Controls.Add(this.label1);
            this.Controls.Add(this.textBoxPort);
            this.Controls.Add(this.textBoxServer);
            this.Name = "SubmitServerForm";
            this.Text = "Server Status";
            this.ResumeLayout(false);
            this.PerformLayout();

		}
		#endregion
        public static string submit_filename;
        private static void submitNoThread(string filename)
        {
            submit_filename = filename;
            submitHelper();
        }
        public static void submit(string filename)
        {
            submit_filename = filename;
            try
            {
                if (Compile_Helpers.run_compiled_thread != null &&
                    Compile_Helpers.run_compiled_thread.ThreadState == System.Threading.ThreadState.Running)
                {
                    Compile_Helpers.run_compiled_thread.Abort();
                }
            }
            catch
            {
            }
            Runtime.consoleWriteln("Testing file: " + filename + ": please wait!");
            //Runtime.consoleWrite("PLEASE BE PATIENT!! The first test takes up to 2 minutes");
            Compile_Helpers.run_compiled_thread = new System.Threading.Thread(
                new System.Threading.ThreadStart(submitHelper));
            Compile_Helpers.run_compiled_thread.Start();
        }
		public static void submitHelper()
		{
            string filename = submit_filename;
			bool remember = Component.compiled_flowchart;
			string s;
			int num_input_files = 0;
			bool passed = true;

			// directory is a special case-- don't allow it
			if (filename.ToLower() == "directory")
			{
				MessageBox.Show("No tests found for: " + filename +
					"\n\nUse Save As to change filename.\n" +
					"View available tests using Select Server (Run menu).","Check filename",
					MessageBoxButtons.OK,MessageBoxIcon.Error);
				return;
			}
			// connect to server and test
			try 
			{
				try
				{
					string ts = Registry_Settings.Read("test_server");
					string tp = Registry_Settings.Read("test_port");

                    if (!Component.BARTPE)
                    {
                        Connect_To_Server(ts, System.Int32.Parse(tp));
                    }
                    else
                    {
                        Connect_To_Executable();
                    }
				}
				catch (System.Exception e)
				{
					throw new System.Exception();
				}
				try
				{
					//string x = this.assignmentsCombo.SelectedItem+"\r\n";
					string x = filename + "\r\n";
					byte[] buffer = System.Text.Encoding.ASCII.GetBytes(x);
                    if (!Component.BARTPE)
                    {
                        raptor_files_pkg.current_socket.Send(buffer);
                    }
                    else
                    {
                        process.StandardInput.WriteLine(filename);
                        process.StandardInput.Flush();
                    }
				}
				catch
				{
					MessageBox.Show("Server terminated abnormally.","Server failed",
						MessageBoxButtons.OK,MessageBoxIcon.Error);
					throw new System.Exception();
				}
				try
				{
                    if (Component.BARTPE && !process.StandardOutput.EndOfStream)
                    {
                        s = process.StandardOutput.ReadLine();
                        num_input_files = System.Int32.Parse(s);
                    }
                    else
                    {
                        s = raptor_files_pkg.get_line();
                        num_input_files = System.Int32.Parse(s);
                    }
                    if (num_input_files == 0)
                    {
                        throw new System.Exception();
                    }
				}
				catch
				{
					MessageBox.Show("No tests found for: " + filename +
						"\n\nUse Save As to change filename.\n" +
						"View available tests using Select Server (Run menu).","Check filename",
						MessageBoxButtons.OK,MessageBoxIcon.Error);
					throw new System.Exception();
				}
				Runtime.consoleClear();
				Component.run_compiled_flowchart = true;
                try
                {
                    Compile_Helpers.Compile_Flowchart(Runtime.parent.carlisle.TabPages);
                }
                catch (parse_tree.emit_code.illegal_code e)
                {
                    MessageBox.Show("Illegal flowchart: no graphics allowed when testing against server.");
                    throw;
                }
                catch (System.Exception e)
                {
                    MessageBox.Show(e.Message + '\n', "Compilation Error",
                        MessageBoxButtons.OK, MessageBoxIcon.Error);
                    throw;
                }
				for (int i=1; i<=num_input_files; i++)
				{
                    System.Threading.ThreadPriority priority = System.Threading.Thread.CurrentThread.Priority;
                    System.Threading.Thread.CurrentThread.Priority = System.Threading.ThreadPriority.BelowNormal;
                    
					try
					{
						Compile_Helpers.Run_Compiled_NoThread(false);
					}
					catch
					{
						MessageBox.Show("Program terminated abnormally","Program error",
							MessageBoxButtons.OK,MessageBoxIcon.Error);
						throw new System.Exception();
					}
                    System.Threading.Thread.CurrentThread.Priority = priority;
					// send the EOF
					try
					{
						string x = "\r\nEOF\r\n";
						byte[] buffer = System.Text.Encoding.ASCII.GetBytes(x);
                        if (!Component.BARTPE)
                        {
                            raptor_files_pkg.current_socket.Send(buffer);
                        }
                        else
                        {
                            process.StandardInput.WriteLine();
                            process.StandardInput.WriteLine("EOF");
                            process.StandardInput.Flush();
                        }
					}
					catch
					{
						MessageBox.Show("Server terminated abnormally","Network error",
							MessageBoxButtons.OK,MessageBoxIcon.Error);
						throw new System.Exception();
					}
					try 
					{
						// loop until EOF at end of input
						while (true) 
						{
							s = raptor_files_pkg.get_line();
						}
					}
					catch
					{
					}
					try
					{
						s = raptor_files_pkg.get_line();
						if (s=="INCORRECT")
						{
							passed = false;
						}
						Runtime.consoleMessage(filename + 
							": test case #" + i + ": " + s);
					}
					catch
					{
						Runtime.consoleMessage("no response");
						passed = false;
					}
				}
                StopRedirection();
				if (passed) 
				{
					Runtime.consoleMessage(filename + ": PASSED");
				}
				else
				{
					Runtime.consoleMessage(filename + ": FAILED");
				}
				Component.run_compiled_flowchart = false;
			}
			catch (System.Exception e)
			{
				try
				{
                    StopRedirection();
                }
				catch {}
				Component.run_compiled_flowchart = false;
			}


		}

        private static void StopRedirection()
        {
            if (!Component.BARTPE)
            {
                raptor_files_pkg.stop_network_redirect();
            }
            else
            {
                StopProcessRedirect();
            }
        }

		private void refreshButton_Click(object sender, System.EventArgs e)
		{
			this.Refresh_List();
		}

		private void OKbutton_Click(object sender, System.EventArgs e)
		{
			Registry_Settings.Write("test_server",this.textBoxServer.Text);
			try
			{
				int x = System.Int32.Parse(this.textBoxPort.Text);
				Registry_Settings.Write("test_port", this.textBoxPort.Text);
			}
			catch
			{
			}
			this.Close();
		}


		private void cancel_Click(object sender, System.EventArgs e)
		{
			this.Close();		
		}

		
	}
}
