using System;
using System.Drawing;
using System.Collections;
using System.ComponentModel;
using System.Windows.Forms;
using System.IO;
using System.Runtime.Serialization;
using System.Runtime.Serialization.Formatters.Binary;

namespace raptor
{
	/// <summary>
	/// Summary description for MasterConsole.
	/// </summary>
	public class MasterConsole : System.Windows.Forms.Form
	{
        private const int min_width = 100;
        private const int min_height = 50;
		private PensBrushes.family currentFamily = PensBrushes.family.times;
		private int currentFontSize = 8;
		private ArrayList commands = new ArrayList();
		private int current_command = 0;
		private bool last_had_new_line = true;
		private System.Windows.Forms.Panel panel1;
		private System.Windows.Forms.Button clear_button;
		private System.Windows.Forms.TextBox textBox2;
		private System.Windows.Forms.TextBox textBox1;
		private System.Windows.Forms.MainMenu mainMenu1;
		private System.Windows.Forms.MenuItem menuFile;
		private System.Windows.Forms.MenuItem menuFont;
		private System.Windows.Forms.MenuItem menuHelp;
		private System.Windows.Forms.MenuItem menuGeneralHelp;
		private System.Windows.Forms.MenuItem menuFont6;
		private System.Windows.Forms.MenuItem menuFont8;
		private System.Windows.Forms.MenuItem menuFont10;
		private System.Windows.Forms.MenuItem menuFont12;
		private System.Windows.Forms.MenuItem menuFont14;
		private System.Windows.Forms.MenuItem menuFont16;
		private System.Windows.Forms.MenuItem menuFont20;
		private System.Windows.Forms.MenuItem menuFont24;
		private System.Windows.Forms.MenuItem menuFont28;
		private System.Windows.Forms.MenuItem menuFont36;
		private System.Windows.Forms.MenuItem menuPrintConsole;
		private System.Windows.Forms.MenuItem current_font;
		private System.Windows.Forms.MenuItem menuItem1;
		private System.Windows.Forms.MenuItem menuItemCopy;
		private System.Windows.Forms.MenuItem menuItem2;
		private System.Windows.Forms.MenuItem menuCourier;
		private System.Windows.Forms.MenuItem menuTimes;
        private System.Windows.Forms.MenuItem menuArial;
        private IContainer components;
        private MenuItem menuItemSelectAll;
        private MenuItem menuItemShowLog;
        private bool am_standalone = false;

		public MasterConsole()
        {
            //
            // Required for Windows Form Designer support
            //
            InitializeComponent();
            this.current_font = this.menuFont8;
            string width_setting = Registry_Settings.Read("ConsoleWidth");
            System.Drawing.Rectangle my_rect = this.DesktopBounds;
            int my_width = -1, my_height = -1, my_x = -1, my_y = -1;
            if (width_setting != null)
            {
                my_width = Int32.Parse(width_setting);
            }
            string height_setting = Registry_Settings.Read("ConsoleHeight");
            if (height_setting != null)
            {
                my_height = Int32.Parse(height_setting);
            }
            else
            {
                my_height = this.DesktopBounds.Height;
            }
            string x_setting = Registry_Settings.Read("ConsoleX");
            if (x_setting != null)
            {
                my_x = Int32.Parse(x_setting);
            }
            string y_setting = Registry_Settings.Read("ConsoleY");
            if (y_setting != null)
            {
                my_y = Int32.Parse(y_setting);
            }
            if (my_y >= 0 && my_y < System.Windows.Forms.Screen.GetWorkingArea(this).Bottom-20)
            {
                my_rect.Y = my_y;
            }
            if (my_x >= 0 && my_x < System.Windows.Forms.Screen.GetWorkingArea(this).Right - 20)
            {
                my_rect.X = my_x;
            }
            if (my_width > min_width)
            {
                my_rect.Width = my_width;
            }
            if (my_height > min_height)
            {
                my_rect.Height = my_height;
            }
            // OK, I'm not sure what's up with these -20s, but they keep it from growing every time
            if (my_x >= 0 && my_y >= 0 && my_height > 0 && my_width > 0)
            {
                this.SetDesktopBounds(my_rect.X, my_rect.Y, my_rect.Width, my_rect.Height);
            }
            else
            {
                System.Drawing.Size sz = SystemInformation.PrimaryMonitorMaximizedWindowSize;
                this.Left = sz.Width - this.Width;
                this.Top = sz.Height - this.Height - 20;
            }
        }
        public void Set_Font_Size()
        {
            string console_font_setting = Registry_Settings.Read("ConsoleFontSize");
            if (console_font_setting != null)
            {
                if (console_font_setting == "6")
                {
                    this.menuFont6_Click(null, null);
                }
                else if (console_font_setting == "8")
                {
                    this.menuFont8_Click(null, null);
                }
                else if (console_font_setting == "10")
                {
                    this.menuFont10_Click(null, null);
                }
                else if (console_font_setting == "12")
                {
                    this.menuFont12_Click(null, null);
                }
                else if (console_font_setting == "14")
                {
                    this.menuFont14_Click(null, null);
                }
                else if (console_font_setting == "16")
                {
                    this.menuFont16_Click(null, null);
                }
                else if (console_font_setting == "20")
                {
                    this.menuFont20_Click(null, null);
                }
                else if (console_font_setting == "24")
                {
                    this.menuFont24_Click(null, null);
                }
                else if (console_font_setting == "28")
                {
                    this.menuFont28_Click(null, null);
                }
                else if (console_font_setting == "36")
                {
                    this.menuFont36_Click(null, null);
                }
            }
        }

        public MasterConsole(bool standalone)
        {
            this.am_standalone = standalone;
            InitializeComponent();
            this.current_font = this.menuFont8;
            this.Width = 400;
            this.Height = 200;
            this.Set_Font_Size();
            if (standalone)
            {
                this.textBox2.Enabled = false;
                this.menuHelp.Enabled = false;
            }
        }

		private void Extract_Process_Directory(DirectoryInfo di, StreamWriter output_stream, 
			bool do_all, bool recursive)
		{
            Visual_Flow_Form dummy = new Visual_Flow_Form(null);
            NClass.Core.ProjectCore.raptorUpdater = new UMLupdater(dummy);
            if (recursive)
			{
				foreach (DirectoryInfo sub_di in di.GetDirectories())
				{
					output_stream.WriteLine("Subfolder: " + sub_di.FullName);
					this.Extract_Process_Directory(sub_di,output_stream,do_all,recursive);
				}
			}
			Component.warned_about_error = true;
			Component.warned_about_newer_version = true;
            foreach (FileInfo fi in di.GetFiles())
			{
				try
				{
					Oval Start;
                    Mode file_mode = Mode.Intermediate;
					int incoming_serialization_version;
					bool incoming_USMA_mode = false;

                    TabControl tc = new TabControl();
                    dummy.carlisle = tc;
					if (fi.Extension.ToLower()==".rap")
					{
						Stream stream = File.Open(fi.FullName, FileMode.Open,
							FileAccess.Read);

                        TabControl.TabPageCollection tpc = new TabControl.TabPageCollection(tc);

						BinaryFormatter bformatter = new BinaryFormatter();
						// use di.FullName
						try
						{
							// starting with version 11, we put the version number first
							incoming_serialization_version = (int) bformatter.Deserialize(stream);
							// read in number of pages
							if (incoming_serialization_version >= 13)
							{
								incoming_USMA_mode = (bool)bformatter.Deserialize(stream);
							}
							int num_pages = (int) bformatter.Deserialize(stream);
                            for (int i = 0; i < num_pages; i++)
                            {
                                Subchart_Kinds incoming_kind;
                                string name = (string)bformatter.Deserialize(stream);
                                if (incoming_serialization_version >= 14)
                                {
                                    incoming_kind = (Subchart_Kinds)bformatter.Deserialize(stream);
                                }
                                else
                                {
                                    incoming_kind = Subchart_Kinds.Subchart;
                                }
                                if (i == 0 && incoming_kind != Subchart_Kinds.UML)
                                {
                                    file_mode = Mode.Intermediate;
                                    tpc.Add(new Subchart(dummy, "main"));
                                }
                                else if (i==0)
                                {
                                    file_mode = Mode.Expert;
                                    TabPage tp = new TabPage("UML");
                                    tp.Controls.Add(new UMLDiagram((UMLupdater)
                                        NClass.Core.ProjectCore.raptorUpdater));
                                    tpc.Add(tp);
                                }
                                if (i > 0)
                                {
                                    int param_count = 0;
                                    switch (incoming_kind)
                                    {
                                        case Subchart_Kinds.Function:
                                            param_count = (int)bformatter.Deserialize(stream);
                                            tpc.Add(new Procedure_Chart(dummy, name,
                                                param_count));
                                            break;
                                        case Subchart_Kinds.Procedure:
                                            if (incoming_serialization_version >= 15)
                                            {
                                                param_count = (int)bformatter.Deserialize(stream);
                                            }
                                            tpc.Add(new Procedure_Chart(dummy, name,
                                                param_count));
                                            break;
                                        case Subchart_Kinds.Subchart:
                                            tpc.Add(new Subchart(dummy, name));
                                            break;
                                    }
                                }
                            }
                            if (file_mode == Mode.Expert)
                            {
                                Control ctrl = tpc[0].Controls[0];
                                UMLDiagram umld = (ctrl as UMLDiagram);
                                NClass.Core.BinarySerializationHelper.diagram =
                                    umld.diagram;
                                umld.project.LoadBinary(
                                    bformatter, stream);
                            }
                            for (int i = ((file_mode==Mode.Expert) ? 1 : 0); i < num_pages; i++)
                            {
                                ((Subchart)tpc[i]).Start = (Oval)bformatter.Deserialize(stream);
                                if (incoming_serialization_version >= 17)
                                {
                                    byte[] ink = (byte[])bformatter.Deserialize(stream);
                                }
                            }
						}
						catch 
						{
							stream.Seek(0,SeekOrigin.Begin);
                            ((Subchart)tpc[0]).Start = (Oval)bformatter.Deserialize(stream);
                            incoming_serialization_version = ((Subchart)tpc[0]).Start.incoming_serialization_version;
						}
                        // load all of the subcharts based on what the UML Diagram created for tabs
                        if (file_mode == Mode.Expert)
                        {
                            for (int i = 2; i < dummy.carlisle.TabPages.Count; i++)
                            {
                                ClassTabPage ctp = dummy.carlisle.TabPages[i] as ClassTabPage;
                                for (int j = 0; j < ctp.tabControl1.TabPages.Count; j++)
                                {
                                    Subchart sc = ctp.tabControl1.TabPages[j] as Subchart;
                                    sc.Start = (Oval)bformatter.Deserialize(stream);
                                    byte[] ink = (byte[])bformatter.Deserialize(stream);
                                }
                            }
                        }
						if (incoming_serialization_version >= 4)
						{
							logging_info log = (logging_info)bformatter.Deserialize(stream);
							log.Record_Open(log.Last_Username());
							output_stream.Write(fi.Name + "," +
								log.Total_Minutes() + "," +
								log.Count_Saves() + "," +
								log.Last_Username());
							if (do_all)
							{
								output_stream.Write("," +
									log.Other_Authors());
							}
						}
						if (incoming_serialization_version >= 8)
						{
							bool compiled = (bool)bformatter.Deserialize(stream);
							System.Guid guid = (System.Guid)bformatter.Deserialize(stream);
							output_stream.Write("," + guid);
                            if (do_all)
                            {
                                Generate_Hash gh = new Generate_Hash(null);
                                Compile_Helpers.Do_Compilation(
                                    ((Subchart)tpc[0]).Start,
                                    gh, tpc);
                                output_stream.Write("," +
                                    gh.toString());
                            }
						}
						output_stream.WriteLine("");
                        stream.Close();
					}
				}
				catch (System.Exception exc)
				{
					output_stream.WriteLine(fi.Name + ",failed," + exc.Message);
				}
			}
            dummy.Close();
            NClass.Core.ProjectCore.raptorUpdater = new UMLupdater(Runtime.parent);
            Component.warned_about_newer_version = false;
			Component.warned_about_error = false;
		}
        private void compute_md5()
        {
            OpenFileDialog fileChooser = new OpenFileDialog();
            fileChooser.CheckFileExists = true;
            fileChooser.Title = "Select file to compute hash";
            fileChooser.Filter = "All files (*.*)|*.*";
            DialogResult result = fileChooser.ShowDialog();
            string dialog_fileName;

            if (result == DialogResult.Cancel)
            {
                return;
            }

            dialog_fileName = fileChooser.FileName;
            this.set_text("The hash is: " + 
                MD5Helper.ComputeHash(dialog_fileName)+'\n');
        }
		private void extract_times(bool do_all, bool recursive)
		{
            Runtime.parent.new_clicked(null, null);
			FolderBrowserDialog folderBrowser = new FolderBrowserDialog();
			DialogResult result = folderBrowser.ShowDialog();

			/*OpenFileDialog fileChooser = new OpenFileDialog();
			fileChooser.CheckFileExists = true;
			fileChooser.Title = "Select in desired folder";
			fileChooser.Filter = "Raptor files (*.rap)|*.rap|All files (*.*)|*.*";
			DialogResult result = fileChooser.ShowDialog();*/
			string dialog_fileName;

			if (result == DialogResult.Cancel)
			{
				return;
			}

			//dialog_fileName = fileChooser.FileName;
			dialog_fileName = folderBrowser.SelectedPath;

			if (dialog_fileName == "" || dialog_fileName == null)
			{
				MessageBox.Show("Invalid File Name", "Error",
					MessageBoxButtons.OK, MessageBoxIcon.Error);
				return;
			}

			SaveFileDialog resultfileChooser = new SaveFileDialog();
			resultfileChooser.CheckFileExists = false;
			resultfileChooser.Title = "Select result file";
			resultfileChooser.Filter = "CSV files (*.csv)|*.csv|All files (*.*)|*.*";
			result = resultfileChooser.ShowDialog();
			string resultdialog_fileName;

			if (result == DialogResult.Cancel)
			{
				return;
			}

			resultdialog_fileName = resultfileChooser.FileName;

			if (resultdialog_fileName == "" || resultdialog_fileName == null)
			{
				MessageBox.Show("Invalid File Name", "Error",
					MessageBoxButtons.OK, MessageBoxIcon.Error);
				return;
			}
			StreamWriter output_stream = File.CreateText(
				resultdialog_fileName);
			output_stream.WriteLine("Filename,Minutes,#Saves,Last author,Previous author,GUID,SHA-512 hash");
				
			//DirectoryInfo di = Directory.GetParent(dialog_fileName);
			DirectoryInfo di = new DirectoryInfo(dialog_fileName);

			this.Extract_Process_Directory(di,output_stream,do_all,recursive);
            Runtime.parent.carlisle.TabPages.Clear();
            Runtime.parent.carlisle.TabPages.Add(new Subchart(Runtime.parent, "main"));
            Runtime.parent.Create_Control_graphx();
            output_stream.Close();
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
        public delegate void clear_text_delegate_type(MasterConsole mc);
        public clear_text_delegate_type clear_text_Delegate = new clear_text_delegate_type(
            clear_text_delegate);

        // mcc: big changes 3/3/05 to allow multiple lines in set_text call
        public void clear_txt()
        {
            Object[] args = new Object[1];
            args[0] = this;
            this.Invoke(clear_text_Delegate, args);
        }
        public static void clear_text_delegate(MasterConsole mc)
		{
			mc.textBox1.Lines = new string[0];
			mc.last_had_new_line = true;
		}

		public void program_stopped(string message)
		{
			this.last_had_new_line = true;
			raptor_files_pkg.close_files();
			set_text("----" + message + "----\n");
		}
        public static void set_text_delegate(MasterConsole mc, string text) {
			bool has_new_line;
			string new_text;

            //MessageBox.Show("calling set_text with " + text);
			if (text.Length >= 1 && text[text.Length-1] != '\n')
			{
				has_new_line = false;
				new_text = text;
			}
			else
			{
				has_new_line = true;
				new_text = text.Substring(0,text.Length-1);
			}
			string[] split_text = new_text.Split('\n');
            /*for (int cnt = 0; cnt < split_text.Length; cnt++)
            {
                MessageBox.Show("Line " + cnt + ":" + split_text[cnt]);
            }*/
			// how many newlines are in the string being set
			int count_newlines = split_text.Length-1;

			int addlines = count_newlines;
			
			// we add 1 line for each \n in the string, plus one more if
			// either last had new line or the textbox is empty
			if (mc.last_had_new_line || mc.textBox1.Lines.Length==0)
			{
				addlines++;
			}

			int l = mc.textBox1.Lines.Length;
			string[] newmsg = new string[l+addlines];
			// copy all of the old lines in
			for (int i=0; i < l;i++)
			{
                //MessageBox.Show("copying: " + mc.textBox1.Lines[i] + ":" +
                //    mc.textBox1.Lines[i].Length);
                if (Component.MONO)
                {
                    newmsg[i] = mc.textBox1.Lines[i]+'\n';
                }
                else
                {
                    newmsg[i] = mc.textBox1.Lines[i];
                }
			}
			int j = 0;
			// if we are appending, do it here and increment j
			if (!mc.last_had_new_line && mc.textBox1.Lines.Length>0)
			{
                if (Component.MONO)
                {
                    newmsg[l - 1] = mc.textBox1.Lines[mc.textBox1.Lines.Length - 1] + split_text[0] + '\n';
                }
                else
                {
                    newmsg[l - 1] = mc.textBox1.Lines[mc.textBox1.Lines.Length - 1] + split_text[0];
                }
				j++;
			}
			for (; j<=count_newlines; j++)
			{
                if (Component.MONO)
                {
                    newmsg[l++] = split_text[j] + '\n';
                }
                else
                {
                    newmsg[l++] = split_text[j];
                }
			}

            /*for (int cnt = 0; cnt < newmsg.Length; cnt++)
            {
                MessageBox.Show("Set to: " + newmsg[cnt]);
            }*/
			mc.textBox1.Lines = newmsg;

			mc.last_had_new_line = has_new_line;
			mc.Focus();
			mc.textBox1.Focus();
			mc.textBox1.Select(mc.textBox1.Text.Length,0);
			mc.textBox1.ScrollToCaret();
            if (mc.WindowState == FormWindowState.Minimized)
            {
                mc.WindowState = FormWindowState.Normal;
            }
			mc.BringToFront();
        }
        public delegate void set_text_delegate_type(MasterConsole mc, string text);
        public set_text_delegate_type set_text_Delegate = new set_text_delegate_type(
            set_text_delegate);

		// mcc: big changes 3/3/05 to allow multiple lines in set_text call
		public void set_text(string text)
		{
            Object[] args = new Object[2];
            args[0] = this;
            args[1] = text;
            this.Invoke(set_text_Delegate,args);
		}

		#region Windows Form Designer generated code
		/// <summary>
		/// Required method for Designer support - do not modify
		/// the contents of this method with the code editor.
		/// </summary>
		private void InitializeComponent()
		{
            this.components = new System.ComponentModel.Container();
            System.ComponentModel.ComponentResourceManager resources = new System.ComponentModel.ComponentResourceManager(typeof(MasterConsole));
            this.panel1 = new System.Windows.Forms.Panel();
            this.clear_button = new System.Windows.Forms.Button();
            this.textBox2 = new System.Windows.Forms.TextBox();
            this.textBox1 = new System.Windows.Forms.TextBox();
            this.mainMenu1 = new System.Windows.Forms.MainMenu(this.components);
            this.menuFile = new System.Windows.Forms.MenuItem();
            this.menuPrintConsole = new System.Windows.Forms.MenuItem();
            this.menuItem2 = new System.Windows.Forms.MenuItem();
            this.menuArial = new System.Windows.Forms.MenuItem();
            this.menuCourier = new System.Windows.Forms.MenuItem();
            this.menuTimes = new System.Windows.Forms.MenuItem();
            this.menuFont = new System.Windows.Forms.MenuItem();
            this.menuFont6 = new System.Windows.Forms.MenuItem();
            this.menuFont8 = new System.Windows.Forms.MenuItem();
            this.menuFont10 = new System.Windows.Forms.MenuItem();
            this.menuFont12 = new System.Windows.Forms.MenuItem();
            this.menuFont14 = new System.Windows.Forms.MenuItem();
            this.menuFont16 = new System.Windows.Forms.MenuItem();
            this.menuFont20 = new System.Windows.Forms.MenuItem();
            this.menuFont24 = new System.Windows.Forms.MenuItem();
            this.menuFont28 = new System.Windows.Forms.MenuItem();
            this.menuFont36 = new System.Windows.Forms.MenuItem();
            this.menuItem1 = new System.Windows.Forms.MenuItem();
            this.menuItemCopy = new System.Windows.Forms.MenuItem();
            this.menuHelp = new System.Windows.Forms.MenuItem();
            this.menuGeneralHelp = new System.Windows.Forms.MenuItem();
            this.menuItemSelectAll = new System.Windows.Forms.MenuItem();
            this.menuItemShowLog = new System.Windows.Forms.MenuItem();
            this.panel1.SuspendLayout();
            this.SuspendLayout();
            // 
            // panel1
            // 
            this.panel1.Controls.Add(this.clear_button);
            this.panel1.Controls.Add(this.textBox2);
            this.panel1.Dock = System.Windows.Forms.DockStyle.Bottom;
            this.panel1.Location = new System.Drawing.Point(0, 214);
            this.panel1.Name = "panel1";
            this.panel1.Size = new System.Drawing.Size(360, 56);
            this.panel1.TabIndex = 1;
            // 
            // clear_button
            // 
            this.clear_button.Location = new System.Drawing.Point(272, 16);
            this.clear_button.Name = "clear_button";
            this.clear_button.Size = new System.Drawing.Size(72, 24);
            this.clear_button.TabIndex = 4;
            this.clear_button.Text = "Clear";
            this.clear_button.Click += new System.EventHandler(this.clear_button_Click);
            // 
            // textBox2
            // 
            this.textBox2.Location = new System.Drawing.Point(16, 18);
            this.textBox2.Name = "textBox2";
            this.textBox2.Size = new System.Drawing.Size(248, 20);
            this.textBox2.TabIndex = 3;
            this.textBox2.TextChanged += new System.EventHandler(this.textBox2_TextChanged);
            this.textBox2.KeyDown += new System.Windows.Forms.KeyEventHandler(this.textBox2_KeyDown);
            // 
            // textBox1
            // 
            this.textBox1.AcceptsReturn = true;
            this.textBox1.BackColor = System.Drawing.SystemColors.Window;
            this.textBox1.BorderStyle = System.Windows.Forms.BorderStyle.None;
            this.textBox1.Dock = System.Windows.Forms.DockStyle.Fill;
            this.textBox1.Font = new System.Drawing.Font("Times New Roman", 8.25F, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, ((byte)(0)));
            this.textBox1.Location = new System.Drawing.Point(0, 0);
            this.textBox1.Multiline = true;
            this.textBox1.Name = "textBox1";
            this.textBox1.ReadOnly = true;
            this.textBox1.ScrollBars = System.Windows.Forms.ScrollBars.Vertical;
            this.textBox1.Size = new System.Drawing.Size(360, 214);
            this.textBox1.TabIndex = 2;
            // 
            // mainMenu1
            // 
            this.mainMenu1.MenuItems.AddRange(new System.Windows.Forms.MenuItem[] {
            this.menuFile,
            this.menuItem2,
            this.menuFont,
            this.menuItem1,
            this.menuHelp});
            // 
            // menuFile
            // 
            this.menuFile.Index = 0;
            this.menuFile.MenuItems.AddRange(new System.Windows.Forms.MenuItem[] {
            this.menuPrintConsole});
            this.menuFile.Text = "&File";
            this.menuFile.Visible = false;
            // 
            // menuPrintConsole
            // 
            this.menuPrintConsole.Index = 0;
            this.menuPrintConsole.Text = "&Print Console";
            this.menuPrintConsole.Click += new System.EventHandler(this.menuPrintConsole_Click);
            // 
            // menuItem2
            // 
            this.menuItem2.Index = 1;
            this.menuItem2.MenuItems.AddRange(new System.Windows.Forms.MenuItem[] {
            this.menuArial,
            this.menuCourier,
            this.menuTimes});
            this.menuItem2.Text = "Fo&nt";
            // 
            // menuArial
            // 
            this.menuArial.Index = 0;
            this.menuArial.Text = "&Arial";
            this.menuArial.Click += new System.EventHandler(this.menuArial_Click);
            // 
            // menuCourier
            // 
            this.menuCourier.Index = 1;
            this.menuCourier.Text = "&Courier";
            this.menuCourier.Click += new System.EventHandler(this.menuCourier_Click);
            // 
            // menuTimes
            // 
            this.menuTimes.Checked = true;
            this.menuTimes.Index = 2;
            this.menuTimes.Text = "&Times New Roman";
            this.menuTimes.Click += new System.EventHandler(this.menuTimes_Click);
            // 
            // menuFont
            // 
            this.menuFont.Index = 2;
            this.menuFont.MenuItems.AddRange(new System.Windows.Forms.MenuItem[] {
            this.menuFont6,
            this.menuFont8,
            this.menuFont10,
            this.menuFont12,
            this.menuFont14,
            this.menuFont16,
            this.menuFont20,
            this.menuFont24,
            this.menuFont28,
            this.menuFont36});
            this.menuFont.Text = "Font &Size";
            // 
            // menuFont6
            // 
            this.menuFont6.Index = 0;
            this.menuFont6.Text = "6";
            this.menuFont6.Click += new System.EventHandler(this.menuFont6_Click);
            // 
            // menuFont8
            // 
            this.menuFont8.Checked = true;
            this.menuFont8.Index = 1;
            this.menuFont8.Text = "8";
            this.menuFont8.Click += new System.EventHandler(this.menuFont8_Click);
            // 
            // menuFont10
            // 
            this.menuFont10.Index = 2;
            this.menuFont10.Text = "10";
            this.menuFont10.Click += new System.EventHandler(this.menuFont10_Click);
            // 
            // menuFont12
            // 
            this.menuFont12.Index = 3;
            this.menuFont12.Text = "12";
            this.menuFont12.Click += new System.EventHandler(this.menuFont12_Click);
            // 
            // menuFont14
            // 
            this.menuFont14.Index = 4;
            this.menuFont14.Text = "14";
            this.menuFont14.Click += new System.EventHandler(this.menuFont14_Click);
            // 
            // menuFont16
            // 
            this.menuFont16.Index = 5;
            this.menuFont16.Text = "16";
            this.menuFont16.Click += new System.EventHandler(this.menuFont16_Click);
            // 
            // menuFont20
            // 
            this.menuFont20.Index = 6;
            this.menuFont20.Text = "20";
            this.menuFont20.Click += new System.EventHandler(this.menuFont20_Click);
            // 
            // menuFont24
            // 
            this.menuFont24.Index = 7;
            this.menuFont24.Text = "24";
            this.menuFont24.Click += new System.EventHandler(this.menuFont24_Click);
            // 
            // menuFont28
            // 
            this.menuFont28.Index = 8;
            this.menuFont28.Text = "28";
            this.menuFont28.Click += new System.EventHandler(this.menuFont28_Click);
            // 
            // menuFont36
            // 
            this.menuFont36.Index = 9;
            this.menuFont36.Text = "36";
            this.menuFont36.Click += new System.EventHandler(this.menuFont36_Click);
            // 
            // menuItem1
            // 
            this.menuItem1.Index = 3;
            this.menuItem1.MenuItems.AddRange(new System.Windows.Forms.MenuItem[] {
            this.menuItemCopy,
            this.menuItemSelectAll});
            this.menuItem1.Text = "&Edit";
            this.menuItem1.Popup += new System.EventHandler(this.menuItem1_Popup);
            // 
            // menuItemCopy
            // 
            this.menuItemCopy.Index = 0;
            this.menuItemCopy.Shortcut = System.Windows.Forms.Shortcut.CtrlC;
            this.menuItemCopy.Text = "&Copy";
            this.menuItemCopy.Click += new System.EventHandler(this.menuItem2_Click);
            // 
            // menuHelp
            // 
            this.menuHelp.Index = 4;
            this.menuHelp.MenuItems.AddRange(new System.Windows.Forms.MenuItem[] {
            this.menuGeneralHelp,
            this.menuItemShowLog});
            this.menuHelp.Text = "&Help";
            // 
            // menuGeneralHelp
            // 
            this.menuGeneralHelp.Index = 0;
            this.menuGeneralHelp.Text = "&General Help";
            this.menuGeneralHelp.Click += new System.EventHandler(this.menuGeneralHelp_Click);
            // 
            // menuItemSelectAll
            // 
            this.menuItemSelectAll.Index = 1;
            this.menuItemSelectAll.Text = "Select &all";
            this.menuItemSelectAll.Click += new System.EventHandler(this.menuItemSelectAll_Click);
            // 
            // menuItemShowLog
            // 
            this.menuItemShowLog.Index = 1;
            this.menuItemShowLog.Text = "&Show log";
            this.menuItemShowLog.Click += new System.EventHandler(this.menuItemShowLog_Click);
            // 
            // MasterConsole
            // 
            this.AutoScaleBaseSize = new System.Drawing.Size(5, 13);
            this.AutoScroll = true;
            this.ClientSize = new System.Drawing.Size(360, 270);
            this.Controls.Add(this.textBox1);
            this.Controls.Add(this.panel1);
            this.Icon = ((System.Drawing.Icon)(resources.GetObject("$this.Icon")));
            this.Location = new System.Drawing.Point(500, 100);
            this.Menu = this.mainMenu1;
            this.MinimumSize = new System.Drawing.Size(350, 140);
            this.Name = "MasterConsole";
            this.StartPosition = System.Windows.Forms.FormStartPosition.Manual;
            this.Text = "MasterConsole";
            this.Resize += new System.EventHandler(this.MasterConsole_Resize);
            this.Move += new System.EventHandler(this.MasterConsole_Resize);
            this.Closing += new System.ComponentModel.CancelEventHandler(this.MasterConsole_Closing);
            this.Load += new System.EventHandler(this.MasterConsole_Load);
            this.panel1.ResumeLayout(false);
            this.panel1.PerformLayout();
            this.ResumeLayout(false);
            this.PerformLayout();

		}
		#endregion

		private void textBox2_TextChanged(object sender, System.EventArgs e)
		{
		
		}

		private void clear_button_Click(object sender, System.EventArgs e)
		{
			this.textBox1.Clear();
		}

		private void textBox2_KeyDown(object sender, System.Windows.Forms.KeyEventArgs e)
		{
			interpreter.syntax_result result;
			bool b;

			try 
			{
				if(e.KeyCode.ToString() == "Up")
				{
					if (current_command > 0)
					{
						textBox2.Text = (string) 
							commands[--current_command];
					}
				}
				else if(e.KeyCode.ToString() == "Down")
				{
					if (current_command < commands.Count-1)
					{
						textBox2.Text = (string) 
							commands[++current_command];
					}
					else if (current_command == commands.Count - 1)
					{
						textBox2.Text = "";
						current_command++;
					}
				}
				else if(e.KeyCode==Keys.Enter || e.KeyCode==Keys.Return)
				{
                    if (textBox2.Text == "compute_md5")
                    {
                        this.compute_md5();
                    }
                    else if (textBox2.Text == "bartpe" &&
                        System.Environment.UserName.ToLower() == "martin.carlisle")
                    {
                        Component.BARTPE = true;
                        command_success();
                        return;
                    }
                    else if (textBox2.Text == "extract_times")
                    {
                        this.extract_times(false, false);
                        command_success();
                        return;
                    }
                    else if (textBox2.Text == "extract_times_all")
                    {
                        this.extract_times(true, false);
                        command_success();
                        return;
                    }
                    else if (textBox2.Text == "extract_times_recursive")
                    {
                        this.extract_times(true, true);
                        command_success();
                        return;
                    }
                    else if (textBox2.Text == "extract_times_recursive_all")
                    {
                        this.extract_times(true, true);
                        command_success();
                        return;
                    }
                    else if (textBox2.Text == "show_guids")
                    {
                        int count = Runtime.parent.carlisle.TabCount;
                        for (int i = 0; i < count; i++)
                        {
                            Runtime.consoleWriteln(
                                "GUIDs in " +
                                Runtime.parent.carlisle.TabPages[i].Text +
                                ": ");
                            ((Oval)((Subchart)(Runtime.parent.carlisle.TabPages[i])).Start).Show_Guids();
                        }
                        command_success();
                        return;
                    }
                    else if (textBox2.Text == "count_symbols")
                    {
                        Runtime.parent.menuCountSymbols_Click(null, null);
                        command_success();
                        return;
                    }
                    else if (textBox2.Text == "increase_scope")
                    {
                        Runtime.Increase_Scope("test");
                        command_success();
                        return;
                    }
                    else if (textBox2.Text == "show_full_log")
                    {
                        Runtime.parent.log.Display(Runtime.parent, true);
                        command_success();
                        return;
                    }
                    else if (textBox2.Text == "decrease_scope")
                    {
                        Runtime.Decrease_Scope();
                        command_success();
                        return;
                    }
                    else if (textBox2.Text.Contains("emergency_key"))
                    {
                        char[] separator = new char[1];
                        separator[0] = ',';
                        string[] files = textBox2.Text.Split(separator, 2);
                        Runtime.parent.AES_KeyHint(files[1]);
                        command_success();
                        return;
                    }
                    else if (textBox2.Text.Contains("emergency_decrypt"))
                    {
                        char[] separator = new char[1];
                        separator[0] = ',';
                        string[] files = textBox2.Text.Split(separator, 4);
                        Runtime.parent.AES_Decrypt(files[1], files[2], files[3]);
                        command_success();
                        return;
                    }
                    else if (textBox2.Text == "emergency")
                    {
                        EmergencyDialog em_dialog = new EmergencyDialog();
                        em_dialog.ShowDialog(this);
                        command_success();
                        return;
                    }
                    else if (textBox2.Text == "show_disassembly" &&
                        System.Environment.UserName.ToLower() == "martin.carlisle")
                    {
                        Component.compiled_flowchart = false;
                        textBox2.Text = "";
                        ((Subchart)Runtime.parent.carlisle.SelectedTab).flow_panel.Invalidate();
                        command_success();
                        return;
                    }

					Runtime.Clear_Updated();
					// you can't call a subchart from the MC
					result = interpreter_pkg.statement_syntax(textBox2.Text,false,null);
					if (result.valid) 
					{
						commands.Add(textBox2.Text);
						current_command = commands.Count;
						interpreter_pkg.run_assignment(result.tree,textBox2.Text);
						textBox2.Text = "";
						this.Activate();
					}
					else 
					{
						result = interpreter_pkg.conditional_syntax(textBox2.Text,null);
						if (result.valid) 
						{
							commands.Add(textBox2.Text);
							current_command = commands.Count;
							b = interpreter_pkg.run_boolean(result.tree,textBox2.Text);
							textBox2.Text = "";
							this.Activate();
							if (b) 
							{
								Runtime.consoleWriteln("TRUE");
							}
							else
							{
								Runtime.consoleWriteln("FALSE");
							}
						}
					}
				}
			}
			catch (System.Exception exc)
			{
				Runtime.consoleWriteln(exc.Message);
			}
		}

        private void command_success()
        {
            commands.Add(textBox2.Text);
            current_command = commands.Count;
            textBox2.Text = "";
            this.Activate();
        }

		private void MasterConsole_Load(object sender, System.EventArgs e)
		{
		
		}

		private void MasterConsole_Closing(object sender, System.ComponentModel.CancelEventArgs e)
		{
            if (!am_standalone)
            {
                e.Cancel = true;
                this.WindowState = FormWindowState.Minimized;
            }
            else
            {
                Application.Exit();
            }
		}

		private void menuGeneralHelp_Click(object sender, System.EventArgs e)
		{
            if (!Component.BARTPE && !Component.VM)
            {
                Help.ShowHelp(this, Directory.GetParent(
                    Application.ExecutablePath) + "\\raptor.chm");
            }
            else
            {
                MessageBox.Show("Help not installed properly");
            }
		}

		public void menuFont6_Click(object sender, System.EventArgs e)
		{
			this.current_font.Checked=false;
			this.menuFont6.Checked=true;
			currentFontSize = 6;
			this.textBox1.Font=PensBrushes.Get_Font(currentFamily,currentFontSize);
			this.current_font=this.menuFont6;
			Registry_Settings.Write("ConsoleFontSize","6");
		}
		public void menuFont8_Click(object sender, System.EventArgs e)
		{
			this.current_font.Checked=false;
			this.menuFont8.Checked=true;
			currentFontSize = 8;
			this.textBox1.Font=PensBrushes.Get_Font(currentFamily,currentFontSize);
			this.current_font=this.menuFont8;
			Registry_Settings.Write("ConsoleFontSize","8");
		}
		public void menuFont10_Click(object sender, System.EventArgs e)
		{
			this.current_font.Checked=false;
			this.menuFont10.Checked=true;
			currentFontSize = 10;
			this.textBox1.Font=PensBrushes.Get_Font(currentFamily,currentFontSize);
			this.current_font=this.menuFont10;
			Registry_Settings.Write("ConsoleFontSize","10");
		}
		public void menuFont12_Click(object sender, System.EventArgs e)
		{
			this.current_font.Checked=false;
			this.menuFont12.Checked=true;
			currentFontSize = 12;
			this.textBox1.Font=PensBrushes.Get_Font(currentFamily,currentFontSize);
			this.current_font=this.menuFont12;
			Registry_Settings.Write("ConsoleFontSize","12");
		}
		public void menuFont14_Click(object sender, System.EventArgs e)
		{
			this.current_font.Checked=false;
			this.menuFont14.Checked=true;
			currentFontSize = 14;
			this.textBox1.Font=PensBrushes.Get_Font(currentFamily,currentFontSize);
			this.current_font=this.menuFont14;
			Registry_Settings.Write("ConsoleFontSize","14");
		}
		public void menuFont16_Click(object sender, System.EventArgs e)
		{
			this.current_font.Checked=false;
			this.menuFont16.Checked=true;
			currentFontSize = 16;
			this.textBox1.Font=PensBrushes.Get_Font(currentFamily,currentFontSize);
			this.current_font=this.menuFont16;
			Registry_Settings.Write("ConsoleFontSize","16");
		}
		public void menuFont20_Click(object sender, System.EventArgs e)
		{
			this.current_font.Checked=false;
			this.menuFont20.Checked=true;
			currentFontSize = 20;
			this.textBox1.Font=PensBrushes.Get_Font(currentFamily,currentFontSize);
			this.current_font=this.menuFont20;
			Registry_Settings.Write("ConsoleFontSize","20");
		}
		public void menuFont24_Click(object sender, System.EventArgs e)
		{
			this.current_font.Checked=false;
			this.menuFont24.Checked=true;
			currentFontSize = 24;
			this.textBox1.Font=PensBrushes.Get_Font(currentFamily,currentFontSize);
			this.current_font=this.menuFont24;
			Registry_Settings.Write("ConsoleFontSize","24");
		}
		public void menuFont28_Click(object sender, System.EventArgs e)
		{
			this.current_font.Checked=false;
			this.menuFont28.Checked=true;
			currentFontSize = 28;
			this.textBox1.Font=PensBrushes.Get_Font(currentFamily,currentFontSize);
			this.current_font=this.menuFont28;
			Registry_Settings.Write("ConsoleFontSize","28");
		}
		public void menuFont36_Click(object sender, System.EventArgs e)
		{
			this.current_font.Checked=false;
			this.menuFont36.Checked=true;
			currentFontSize = 36;
			this.textBox1.Font=PensBrushes.Get_Font(currentFamily,currentFontSize);
			this.current_font=this.menuFont36;
			Registry_Settings.Write("ConsoleFontSize","36");
		}

		private void menuPrintConsole_Click(object sender, System.EventArgs e)
		{
		
		}

		private void menuItem2_Click(object sender, System.EventArgs e)
		{
			Clipboard.SetDataObject(this.textBox1.SelectedText);
		}

		private void menuItem1_Popup(object sender, System.EventArgs e)
		{
			if (this.textBox1.SelectedText!=null && this.textBox1.SelectedText!="")
			{
				this.menuItemCopy.Enabled=true;
			}
			else
			{
				this.menuItemCopy.Enabled=false;
			}
		}

		public void menuCourier_Click(object sender, System.EventArgs e)
		{
			this.menuArial.Checked = false;
			this.menuTimes.Checked = false;
			this.menuCourier.Checked = true;
			currentFamily = PensBrushes.family.courier;
			this.textBox1.Font=PensBrushes.Get_Font(currentFamily,currentFontSize);
			Registry_Settings.Write("ConsoleFontFamily","courier");
		}

		public void menuTimes_Click(object sender, System.EventArgs e)
		{
			this.menuArial.Checked = false;
			this.menuTimes.Checked = true;
			this.menuCourier.Checked = false;
			currentFamily = PensBrushes.family.times;
			this.textBox1.Font=PensBrushes.Get_Font(currentFamily,currentFontSize);
			Registry_Settings.Write("ConsoleFontFamily","times");
		}

		public void menuArial_Click(object sender, System.EventArgs e)
		{
			this.menuArial.Checked = true;
			this.menuTimes.Checked = false;
			this.menuCourier.Checked = false;
			currentFamily = PensBrushes.family.arial;
			this.textBox1.Font=PensBrushes.Get_Font(currentFamily,currentFontSize);
			Registry_Settings.Write("ConsoleFontFamily","arial");
		}

        public void MasterConsole_Resize(object sender, EventArgs e)
        {
            string s;
            int i;
            if (this.am_standalone || (this.WindowState == FormWindowState.Maximized))
            {
                return;
            }

            if (this.DesktopLocation.X >= 0)
            {
                Registry_Settings.Write("ConsoleX", this.DesktopLocation.X.ToString());
            }
            if (this.DesktopLocation.Y >= 0)
            {
                Registry_Settings.Write("ConsoleY", this.DesktopLocation.Y.ToString());
            }
            try
            {
                s = Registry_Settings.Read("ConsoleHeight");
                i = Int32.Parse(s);
            }
            catch { i = 0; }
            if (this.DesktopBounds.Width > min_width && Math.Abs(this.Width - i) > 20)
            {
                Registry_Settings.Write("ConsoleWidth", this.DesktopBounds.Width.ToString());
            }
            try
            {
                s = Registry_Settings.Read("ConsoleHeight");
                i = Int32.Parse(s);
            }
            catch { i = 0; }
            if (this.DesktopBounds.Height > min_height && Math.Abs(this.Height - i) > 20)
            {
                Registry_Settings.Write("ConsoleHeight", this.DesktopBounds.Height.ToString());
            }
        }

        private void menuItemSelectAll_Click(object sender, EventArgs e)
        {
            this.textBox1.SelectAll();
        }

        private void menuItemShowLog_Click(object sender, EventArgs e)
        {
            Runtime.parent.log.Display(Runtime.parent,false);
            this.BringToFront();
        }





	}

}
