using System;
using System.Drawing;
using System.Drawing.Imaging;
using System.Collections;
using System.Collections.Generic;
using System.ComponentModel;
using System.Windows.Forms;
using System.Data;
using System.Drawing.Printing;
using System.IO;
using System.Runtime.Serialization;
using System.Runtime.Serialization.Formatters.Binary;
using System.Threading;
using System.Timers;
using System.Runtime.InteropServices;
using System.Security.Cryptography;
using Microsoft.Win32;
using System.Diagnostics;
using System.Reflection;


namespace raptor
{

	/// <summary>
	/// Summary description for Visual_Flow_Form.
	/// </summary>
	public class Visual_Flow_Form : System.Windows.Forms.Form
	{
        private const int min_width = 570;
        private const int min_height = 370;
        private static bool starting_up = true;
		// did we get the "/run" flag?
		public static bool command_line_run = false;
		public static bool command_line_input_redirect = false;
		public static bool command_line_output_redirect = false;
		private string My_Title = "Raptor";
		public string tooltip_text = "";
		public float scale = 0.75f;
		public float print_scale = 0.75f;
		// used in Save,SaveAs
		private bool Save_Error = false;
		private System.DateTime last_autosave = System.DateTime.Now;
		private System.DateTime last_draw = System.DateTime.MinValue;
		public logging_info log = new logging_info();
		// remember my scroll location for when the
		// window is reactivated (mcc:5/16/03)

        // mcc: 03/31/08, number of symbols evaluated during run.
        private int symbol_count;

		// Constants to use for reference to Control Figures
		public const int assignment_fig = 0;
		public const int call_fig = 1;
		public const int input_fig = 2;
		public const int output_fig = 3;
		public const int if_control_fig = 4;
		public const int loop_fig = 5;
        public const int return_fig = 6;
		public int control_figure_selected = -1;

		public const int flow_height = 60, flow_width = 90;
		public const int control_height = 24, control_width = 36;
		// location of controls in left panel
		public const int control_X = 65;
		
		// Variables needed for printing
		private PageSettings pgSettings = new PageSettings();
		private PrinterSettings prtSettings = new PrinterSettings();
		private int num_vert_pages;
		private int num_hor_pages;
		private int vert_counter;
		private int hor_counter;
		private int current_page;
		// first_time is for each tab
		private bool first_time = true;
        private IEnumerator<Subchart> current_tab_enumerator;
		private int[] vert_page_breaks = new int[250];
		private int[] hor_page_breaks = new int[250];
		public int mouse_x, mouse_y;
        private TabPage tab_moving = null;
        private int tab_moving_index = 0;

		private Rectangle ASGN,CALL;
		private Parallelogram INPUT,OUTPUT;
		private IF_Control IFC;
		private Loop LP;
        private Oval_Return RETURN;
		public bool full_speed = false;
		private int x1, y1;
        internal NClass.GUI.Diagram.Project projectCore
        {
            get
            {
                return (this.carlisle.TabPages[0].Controls[0] as UMLDiagram).project;
            }
        }
        private UMLupdater _umlupdater;
        private UMLupdater umlupdater
        {
            get
            {
                return _umlupdater;
            }
        }

        public int mainIndex
        {
            get
            {
                if (Component.Current_Mode == Mode.Expert)
                    return 1;
                else
                    return 0;
            }
        }
		public Component currentObj = null;
		// check to see if we need to save before exit
		public bool modified = false;
        private System.Guid file_guid_back = System.Guid.NewGuid();
        public System.Guid file_guid
        {
            get
            {
                return file_guid_back;
            }
            set
            {
                file_guid_back = value;
                if (Component.BARTPE) {
                    this.MC.Text =  file_guid_back.ToString().Substring(0,8) + ": Console";
                }
            }
        }
	
		// variable needed for cut and copy;
		public Component clipboard;

		// name of file currently being edited
		string fileName;	

		private int Timer_Frequency = 1005-6*100;
		//private System.DateTime starting_time, waiting_time;
		private Thread InstanceCaller;
		private bool Resetting = false;

		// System form variables
		private System.Windows.Forms.ToolBar toolBar1;
		private System.Windows.Forms.Panel control_panel;
		private System.Windows.Forms.Splitter form_splitter;
		private System.Windows.Forms.Label label2;
		private System.Windows.Forms.MainMenu mainMenu1;
		private System.Windows.Forms.MenuItem menuItem8;
		private System.Windows.Forms.MenuItem menuItemPageSetup;
		private System.Windows.Forms.MenuItem menuItemPrintPreview;
		private System.Windows.Forms.MenuItem menuItemPrint;
		private System.Windows.Forms.MenuItem menuItem13;
		private System.Windows.Forms.MenuItem FileOpen;
		private System.Windows.Forms.MenuItem menuHelp;
		private System.Windows.Forms.MenuItem menuAbout;
		
		public MasterConsole MC;
		public Buffered flow_panel
		{
			get 
			{
                if (this.carlisle.SelectedTab is Subchart)
                {
                    return ((Subchart)this.carlisle.SelectedTab).flow_panel;
                }
                else if (this.carlisle.SelectedTab is ClassTabPage &&
                    (this.carlisle.SelectedTab as ClassTabPage).tabControl1.TabPages.Count > 0)
                {
                    return ((this.carlisle.SelectedTab as ClassTabPage).tabControl1.SelectedTab as Subchart).flow_panel;
                }
                else if (this.carlisle.TabPages.Count > 1 &&
                    this.carlisle.TabPages[1] is Subchart)
                {
                    return ((Subchart)this.carlisle.TabPages[1]).flow_panel;
                }
                else
                {
                    return null;
                } 
			}
		}
		public System.Windows.Forms.Panel panel1
		{
			get 
			{
                if (this.carlisle.SelectedTab is Subchart)
                {
                    return ((Subchart)this.carlisle.SelectedTab).panel1;
                }
                else if (this.carlisle.SelectedTab is ClassTabPage &&
                    (this.carlisle.SelectedTab as ClassTabPage).tabControl1.TabPages.Count > 0)
                {
                    return ((this.carlisle.SelectedTab as ClassTabPage).tabControl1.SelectedTab as Subchart).panel1;
                }
                else if (this.carlisle.TabPages.Count > 1 &&
                    this.carlisle.TabPages[1] is Subchart)
                {
                    return ((Subchart)this.carlisle.TabPages[1]).panel1;
                }
                else
                {
                    return null;
                }
			}
		}		
		public System.Drawing.Point scroll_location
		{
            get 
			{
                if (this.carlisle.SelectedTab is Subchart)
                {
                    return ((Subchart)this.carlisle.SelectedTab).scroll_location;
                }
                else if (this.carlisle.SelectedTab is ClassTabPage)
                {
                    if ((this.carlisle.SelectedTab as ClassTabPage).tabControl1.TabPages.Count > 0)
                    {
                        return ((Procedure_Chart)(this.carlisle.SelectedTab as ClassTabPage).
                            tabControl1.SelectedTab).scroll_location;
                    }
                    else
                    {
                        return ((Subchart)this.carlisle.TabPages[1]).scroll_location;
                    }
                }
                else
                {
                    return ((Subchart)this.carlisle.TabPages[1]).scroll_location;
                }
			}
			set
			{
                if (this.carlisle.SelectedTab is Subchart)
                {
                    ((Subchart)this.carlisle.SelectedTab).scroll_location = value;
                }
                else if (this.carlisle.SelectedTab is ClassTabPage && 
                    (this.carlisle.SelectedTab as ClassTabPage).tabControl1.TabPages.Count>0)
                {
                    ((Subchart)(this.carlisle.SelectedTab as ClassTabPage).tabControl1.SelectedTab)
                        .scroll_location = value;
                }
			}
		}
		public bool region_selected
		{
			get 
			{
                if (this.carlisle.SelectedTab is Subchart)
                {
                    return ((Subchart)this.carlisle.SelectedTab).region_selected;
                }
                else if (this.carlisle.SelectedTab is ClassTabPage)
                {
                    if ((this.carlisle.SelectedTab as ClassTabPage).tabControl1.TabPages.Count > 0)
                    {
                        return ((Procedure_Chart)(this.carlisle.SelectedTab as ClassTabPage).
                            tabControl1.SelectedTab).region_selected;
                    }
                    else
                    {
                        return false;
                    }
                }
                else
                {
                    return false;
                }
			}
			set
			{
                if (this.carlisle.SelectedTab is Subchart)
                {
                    ((Subchart)this.carlisle.SelectedTab).region_selected = value;
                }
                else if (this.carlisle.SelectedTab is ClassTabPage && 
                    (this.carlisle.SelectedTab as ClassTabPage).tabControl1.TabPages.Count>0)
                {
                    ((Subchart)(this.carlisle.SelectedTab as ClassTabPage).tabControl1.SelectedTab)
                        .region_selected = value;
                }
			}
		}
		public Component Breakpoint_Selection
		{
			get 
			{
                if (this.carlisle.SelectedTab is Subchart)
                {
                    return ((Subchart)this.carlisle.SelectedTab).Breakpoint_Selection;
                }
                else if (this.carlisle.SelectedTab is ClassTabPage)
                {
                    if ((this.carlisle.SelectedTab as ClassTabPage).tabControl1.TabPages.Count > 0)
                    {
                        return ((Procedure_Chart)(this.carlisle.SelectedTab as ClassTabPage).
                            tabControl1.SelectedTab).Breakpoint_Selection;
                    }
                    else
                    {
                        return null;
                    }
                }
                else
                {
                    return null;
                }
			}
			set
			{
                if (this.carlisle.SelectedTab is Subchart)
                {
                    ((Subchart)this.carlisle.SelectedTab).Breakpoint_Selection = value;
                }
                else if (this.carlisle.SelectedTab is ClassTabPage &&
                    (this.carlisle.SelectedTab as ClassTabPage).tabControl1.TabPages.Count > 0)
                {
                    ((Subchart)(this.carlisle.SelectedTab as ClassTabPage).tabControl1.SelectedTab)
                        .Breakpoint_Selection = value;
                }
			}
		}		
		
		public CommentBox selectedComment
		{
			get 
			{
                if (this.carlisle.SelectedTab is Subchart)
                {
                    return ((Subchart)this.carlisle.SelectedTab).selectedComment;
                }
                else if (this.carlisle.SelectedTab is ClassTabPage &&
                    (this.carlisle.SelectedTab as ClassTabPage).tabControl1.TabPages.Count > 0)
                {
                    return ((Subchart)(this.carlisle.SelectedTab as ClassTabPage).tabControl1.SelectedTab)
                        .selectedComment;
                }
                else
                {
                    return null;
                }
			}
			set
			{
                if (this.carlisle.SelectedTab is Subchart)
                {
                    ((Subchart)this.carlisle.SelectedTab).selectedComment = value;
                }
                else if (this.carlisle.SelectedTab is ClassTabPage &&
                    (this.carlisle.SelectedTab as ClassTabPage).tabControl1.TabPages.Count > 0)
                {
                    ((Subchart)(this.carlisle.SelectedTab as ClassTabPage).tabControl1.SelectedTab)
                        .selectedComment = value;
                }
			}
		}		
		public Component Current_Selection 
		{
			set 
			{
                if (this.carlisle.SelectedTab is Subchart)
                {
                    ((Subchart)this.carlisle.SelectedTab).Current_Selection = value;
                }
                else if (this.carlisle.SelectedTab is ClassTabPage &&
    (this.carlisle.SelectedTab as ClassTabPage).tabControl1.TabPages.Count > 0)
                {
                    ((Subchart)(this.carlisle.SelectedTab as ClassTabPage).tabControl1.SelectedTab)
                        .Current_Selection = value;
                }

			}
			get
			{
                if (this.carlisle.SelectedTab is Subchart)
                {
                    return ((Subchart)this.carlisle.SelectedTab).Current_Selection;
                }
                else if (this.carlisle.SelectedTab is ClassTabPage &&
                    (this.carlisle.SelectedTab as ClassTabPage).tabControl1.TabPages.Count > 0)
                {
                    return ((Subchart)(this.carlisle.SelectedTab as ClassTabPage).tabControl1.SelectedTab)
                        .Current_Selection;
                }
                else
                {
                    return null;
                }
			}
		}		
		private bool the_runningState = false;
		public bool runningState	//get or set the label of the object
		{
			set 
			{
				if (value) 
				{
					this.menuEdit.Enabled = false;
					this.menuItem13.Enabled = false;
					this.FileOpen.Enabled = false;
					this.menuItem8.Enabled = false;
					this.menuSaveAs.Enabled = false;
					this.menuItemPageSetup.Enabled = false;
					this.menuItemPrintPreview.Enabled = false;
					this.menuItemPrint.Enabled = false;
				}
				else
				{
					this.menuEdit.Enabled = true;
					this.menuItem13.Enabled = true;
					this.FileOpen.Enabled = true;
					this.menuItem8.Enabled = true;
					this.menuSaveAs.Enabled = true;
                    if (!Component.BARTPE)
                    {
                        this.menuItemPageSetup.Enabled = true;
                        this.menuItemPrintPreview.Enabled = true;
                        this.menuItemPrint.Enabled = true;
                    }
				}
				the_runningState = value; 
			}
			get {return the_runningState;}
		}

		public bool continuous_Run = false;
		internal System.Windows.Forms.TabPage running_tab;

		private System.Windows.Forms.MenuItem step;
		public System.Windows.Forms.ToolTip toolTip1;
		private System.ComponentModel.IContainer components;
		private System.Windows.Forms.MenuItem menuSaveAs;
		private System.Windows.Forms.MenuItem menuReset;
		private System.Windows.Forms.MenuItem menuResetExecute;
		private System.Windows.Forms.ImageList imageList1;
		private System.Windows.Forms.ToolBarButton newButton;
		private System.Windows.Forms.ToolBarButton openButton;
		private System.Windows.Forms.ToolBarButton saveButton;
		private System.Windows.Forms.ToolBarButton cutButton;
		private System.Windows.Forms.ToolBarButton copyButton;
		private System.Windows.Forms.ToolBarButton pasteButton;
		private System.Windows.Forms.ToolBarButton printButton;
		public System.Windows.Forms.ToolBarButton undoButton;
		public System.Windows.Forms.ToolBarButton redoButton;
		private System.Windows.Forms.ToolBarButton playButton;
		private System.Windows.Forms.ToolBarButton pauseButton;
		private System.Windows.Forms.ToolBarButton stopButton;
		private System.Windows.Forms.ToolBarButton stepButton;
		private System.Windows.Forms.MenuItem menuExecute;
		private System.Windows.Forms.MenuItem menuPause;
		private System.Windows.Forms.TrackBar trackBar1;
		private System.Windows.Forms.TreeView watchBox;
		private System.Windows.Forms.ComboBox comboBox1;
		private System.Windows.Forms.MenuItem menuItem14;
		private System.Windows.Forms.MenuItem menuFile;
		public System.Windows.Forms.MenuItem menuMRU1;
		public System.Windows.Forms.MenuItem menuMRU2;
		public System.Windows.Forms.MenuItem menuMRU3;
		public System.Windows.Forms.MenuItem menuMRU4;
		private System.Windows.Forms.MenuItem menuItem1;
		private System.Windows.Forms.MenuItem menuItem25;
		private System.Windows.Forms.MenuItem menuScale;
		private System.Windows.Forms.MenuItem menuRun;
		private System.Windows.Forms.MenuItem menuAllText;
		private System.Windows.Forms.MenuItem menuTruncated;
		private System.Windows.Forms.MenuItem menuNoText;
		private System.Drawing.Printing.PrintDocument printDoc;
		private System.Windows.Forms.MenuItem generalHelpMenu;
		private System.Windows.Forms.MenuItem menuScale125;
		private System.Windows.Forms.MenuItem menuScale100;
		private System.Windows.Forms.MenuItem menuScale80;
		private System.Windows.Forms.MenuItem menuScale60;
		private System.Windows.Forms.MenuItem menuScale40;
		public System.Windows.Forms.ContextMenu contextMenu1;
		private System.Windows.Forms.MenuItem menuItem21;
		private System.Windows.Forms.MenuItem menuItem22;
		private System.Windows.Forms.MenuItem menuItem23;
		public System.Windows.Forms.MenuItem menuItemUndo;
		public System.Windows.Forms.MenuItem menuItemRedo;
		private System.Windows.Forms.MenuItem menuItemComment;
		private System.Windows.Forms.MenuItem menuItemCut;
		private System.Windows.Forms.MenuItem menuItemCopy;
		private System.Windows.Forms.MenuItem menuItemPaste;
		private System.Windows.Forms.MenuItem menuItemDelete;
		private System.Windows.Forms.MenuItem menuEdit;
		private System.Windows.Forms.MenuItem contextMenuEdit;
		private System.Windows.Forms.MenuItem contextMenuComment;
		private System.Windows.Forms.MenuItem contextMenuCut;
		private System.Windows.Forms.MenuItem contextMenuCopy;
		private System.Windows.Forms.MenuItem contextMenuDelete;
		private System.Windows.Forms.MenuItem menuItem2;
		private System.Windows.Forms.MenuItem menuViewComments;
		private System.Windows.Forms.MenuItem menuView;
		private System.Windows.Forms.MenuItem menuItem3;
		private System.Windows.Forms.MenuItem menuViewVariables;
		private System.Windows.Forms.MenuItem menuItem4;
		private System.Windows.Forms.MenuItem menuShowLog;
		private System.Windows.Forms.MenuItem menuWindow;
		private System.Windows.Forms.MenuItem menuTileVertical;
		private System.Windows.Forms.MenuItem menuTileHorizontal;
		private System.Windows.Forms.MenuItem menuItemEditSelection;
		public System.Windows.Forms.ContextMenu contextMenuInsert;
		private System.Windows.Forms.MenuItem menuItemLoop;
		private System.Windows.Forms.MenuItem menuItemIf;
		public System.Windows.Forms.MenuItem contextMenu2Paste;
		private System.Windows.Forms.MenuItem menuItem5;
		private System.Windows.Forms.MenuItem menuItemCompile;
		private System.Windows.Forms.MenuItem menuItemAssignment;
		private System.Windows.Forms.MenuItem menuItemInput;
		private System.Windows.Forms.MenuItem menuItemCall;
		private System.Windows.Forms.MenuItem menuItemOutput;
		private System.Windows.Forms.MenuItem menuBreakpoint;
		public System.Windows.Forms.ContextMenu contextMenu2;
		private System.Windows.Forms.MenuItem menuItem7;
		private System.Windows.Forms.MenuItem menuClearBreakpoints;
		private System.Windows.Forms.MenuItem menuBreakpoint2;
		private System.Windows.Forms.MenuItem menuPrintClipboard;
		private System.Windows.Forms.MenuItem menuScale150;
		private System.Windows.Forms.MenuItem menuScale175;
		private System.Windows.Forms.MenuItem menuScale200;
		private System.Windows.Forms.MenuItem menuScale300;

        private System.Timers.Timer myTimer;
        private System.Timers.Timer loadTimer;
        private string load_filename;
        // for printing
		private float leftMargin;
		private float rightMargin;
		private float topMargin;
		private float bottomMargin;
		private float pageheight;
		private float pagewidth;
		private int drawing_height;
		private int drawing_width;
		// for moving comments
		private System.Windows.Forms.MenuItem menuItem6;
		private System.Windows.Forms.MenuItem menuSelectAll;
		private System.Windows.Forms.MenuItem menuExpandAll;
		private System.Windows.Forms.MenuItem menuCollapseAll;
		private System.Windows.Forms.MenuItem menuItem17;
		public System.Windows.Forms.TabControl carlisle;
		private System.Windows.Forms.TabPage tabPage1;
		private System.Windows.Forms.ContextMenu tabContextMenu;
		private System.Windows.Forms.MenuItem menuAddSubchart;
		private System.Windows.Forms.MenuItem menuDeleteSubchart;
		private System.Windows.Forms.MenuItem menuRenameSubchart;
		private System.Windows.Forms.MenuItem menuItemPrintScale;
		private System.Windows.Forms.MenuItem printScale60;
		private System.Windows.Forms.MenuItem printScale40;
		private System.Windows.Forms.MenuItem printScale80;
		private System.Windows.Forms.MenuItem printScale100;
		private System.Windows.Forms.MenuItem printScale125;
		private System.Windows.Forms.MenuItem printScale150;
		private System.Windows.Forms.MenuItem printScale200;
		private System.Windows.Forms.MenuItem printScale300;
		private System.Windows.Forms.MenuItem printScale175;
		private System.Windows.Forms.MenuItem menuItemRunCompiled;
		private System.Windows.Forms.MenuItem menuItem15;
		private System.Windows.Forms.MenuItem menuRunServer;
		private System.Windows.Forms.MenuItem menuItemSelectServer;
        private MenuItem menuGraphOnTop;
        private MenuItem menuProgramCompleteDialog;
        private MenuItem DefaultWindowSize;
        private MenuItem menuMode;
        private MenuItem menuNovice;
        private MenuItem menuIntermediate;
        private MenuItem menuAddFunction;
        private MenuItem menuAddProcedure;
        public MenuItem menuItemGenerate;
        private MenuItem menuGenerateStandalone;
        private MenuItem menuItemInk;
        public MenuItem menuItemInkOff;
        private MenuItem menuItem20;
        public MenuItem menuItemInkBlack;
        public MenuItem menuItemInkBlue;
        public MenuItem menuItemInkRed;
        private MenuItem menuItem19;
        public MenuItem menuItemInkErase;
        public MenuItem menuItemInkGreen;
        private MenuItem menuItem10;
        public MenuItem menuItemInkSelect;
        public MenuItem menuMRU5;
        public MenuItem menuMRU6;
        public MenuItem menuMRU7;
        public MenuItem menuMRU8;
        public MenuItem menuMRU9;
        private MenuItem menuViewHardDrive;
        private ToolBarButton testServerButton;
        private ToolBarButton InkButton1;
        private MenuItem menuObjectiveMode;
        private MenuItem menuItemReturn;
		private System.Windows.Forms.MenuItem menuCountSymbols;
        public Subchart mainSubchart()
        {
            if (Component.Current_Mode==Mode.Expert)
            {
                if (this.carlisle.TabPages.Count > 1)
                {
                    return (Subchart)this.carlisle.TabPages[1];
                }
                else
                {
                    return (Subchart)this.carlisle.TabPages[0];
                }
            }
            else {
                return (Subchart) this.carlisle.TabPages[0];
            }
        }
        public Visual_Flow_Form(object dummy)
        {
            InitializeComponent();
        }
		public Visual_Flow_Form(bool silent)
		{
            try
            {
                Component.BARTPE = false;
                RegistryKey HKLM = Registry.LocalMachine;
                RegistryKey System_Key = HKLM.OpenSubKey("System");
                RegistryKey CurrentControlSet = System_Key.OpenSubKey("CurrentControlSet");
                RegistryKey Control_Key = CurrentControlSet.OpenSubKey("Control");
                RegistryKey PEBuilder = Control_Key.OpenSubKey("PE Builder");
                String startOptions = (String) Control_Key.GetValue("SystemStartOptions");
                if (PEBuilder != null || startOptions.ToLower().Contains("minint"))
                {
                    if (!VerifyTestingEnvironment.VerifyEnvironment())
                    {
                        //System.Windows.Forms.MessageBox.Show("failed verify environment");
                        //return;
                    }
                    Component.BARTPE = true;
                }
            }
            catch
            {
            }

            try
            {
                // the following code breaks on Vista 64 bit
                /*if (COM.Tools.VMDetect.VMDetect.IsInsideVMWare ||
                    COM.Tools.VMDetect.VMDetect.IsInsideVPC)
                {
                }
                else
                {
                    Component.VM = false;
                }*/
                Component.VM = false;
            }
            catch
            {
            }
            try
            {
                PensBrushes.initialize();
            }
            catch (System.Exception e)
            {
                System.Windows.Forms.MessageBox.Show(e.Message);
            }
			InitializeComponent();
            this._umlupdater = new UMLupdater(this);

            
            // must determine USMA mode before creating subcharts
			string usma_mode=Registry_Settings.Global_Read("USMA_mode");
			if (usma_mode != null)
			{
				Component.USMA_mode = true;
                Component.reverse_loop_logic = true;
			}
			else
			{
                try
                {
                    if (System.Environment.UserDomainName.ToLower() == "usmaedu")
                    {
                        Component.USMA_mode = true;
                        Component.reverse_loop_logic = true;
                    }
                    else
                    {
                        Component.USMA_mode = false;
                        Component.reverse_loop_logic = false;
                    }
                }
                catch
                {
                    Component.USMA_mode = false;
                    Component.reverse_loop_logic = false;
                }
			}

            string reverse_loops = Registry_Settings.Global_Read("reverse_loop_logic");
            if (reverse_loops != null)
            {
                Component.reverse_loop_logic = true;
            }
            string mode_setting = Registry_Settings.Read("UserMode");
            if (mode_setting != null)
            {
                if (mode_setting == "Novice")
                {
                    this.menuNovice_Click(null, null);
                }
                else if (mode_setting == "Intermediate")
                {
                    this.menuIntermediate_Click(null, null);
                }
                else if (mode_setting == "Expert")
                {
                    this.menuObjectiveMode_Click(null, null);
                }
            }
            if (Component.Current_Mode == Mode.Expert)
            {


            }

            tabPage1 = new Subchart(this, "main");
            carlisle.TabPages.Add(tabPage1);
            carlisle.SelectedTab = tabPage1;
			Create_Control_graphx(); // create the control figures
			Create_Flow_graphx();	// create the initial start/end figures


			ada_interpreter_pkg.adainit();

			// hopefully get the DLL loaded right
			interpreter.syntax_result s =
				interpreter_pkg.statement_syntax("x:=5",false,null);

			MC = new MasterConsole();
            file_guid = System.Guid.NewGuid();
            if (!silent)
            {
                MC.Show();
            }
            Runtime.Init(this, this.MC, this.watchBox);
            Runtime.Clear_Variables();
            //beforeLoop = false;
			//afterLoop = false;
			//inLoop = false;
			//MC.set_text("THIS IS A TEST");
			//MC.set_text("THIS IS A 2ND TEST");
            this.log.Record_Open();

            // read stored text view option
			string view_text_setting=Registry_Settings.Read("TextView");
			if (view_text_setting != null)
			{
				if (view_text_setting=="NoText")
				{
					this.menuNoText_Click(null,null);
				}
				else if (view_text_setting=="AllText")
				{
					this.menuAllText_Click(null,null);
				}
				else
				{
					// default is truncated
				}
			}

			this.mainSubchart().Start.scale = this.scale;
			this.mainSubchart().Start.Scale(this.scale);
			flow_panel.Invalidate();

            // read what the user last did for scale and speed
			string console_font_family=Registry_Settings.Read("ConsoleFontFamily");
			if (console_font_family!=null)
			{
				if (console_font_family=="times")
				{
					this.MC.menuTimes_Click(null,null);
				}
				else if (console_font_family=="arial")
				{
					this.MC.menuArial_Click(null,null);
				}
				else if (console_font_family=="courier")
				{
					this.MC.menuCourier_Click(null,null);
				}
			}
            this.MC.Set_Font_Size();			
			string scale_setting=Registry_Settings.Read("Scale");
			if (scale_setting!=null)
			{
                try
                {
                    this.comboBox1.SelectedIndex = Int32.Parse(scale_setting) + 4;
                }
                catch
                {
                    this.comboBox1.SelectedIndex = 6;
                }
			}
			else
			{
				this.comboBox1.SelectedIndex = 6;
			}
            string print_scale_setting = Registry_Settings.Read("PrintScale");
			if (print_scale_setting != null)
			{
                try
                {
                    switch (Int32.Parse(print_scale_setting) + 4)
                    {
                        case 0:
                            this.PrintScale_300(null, null);
                            break;
                        case 1:
                            this.PrintScale_200(null, null);
                            break;
                        case 2:
                            this.PrintScale_175(null, null);
                            break;
                        case 3:
                            this.PrintScale_150(null, null);
                            break;
                        case 4:
                            this.PrintScale_100(null, null);
                            break;
                        case 5:
                            this.PrintScale_80(null, null);
                            break;
                        case 6:
                            this.PrintScale_60(null, null);
                            break;
                        case 7:
                            this.PrintScale_40(null, null);
                            break;
                        case 8:
                            this.PrintScale_20(null, null);
                            break;
                        case 9:
                            break;
                    }
                }
                catch
                {
                    this.PrintScale_40(null, null);
                }
			}
			else
			{
				this.PrintScale_40(null,null);
			}
			string speed_setting=Registry_Settings.Read("Speed");
			if (speed_setting!=null)
			{
                try
                {
                    this.trackBar1.Value = Int32.Parse(speed_setting);
                }
                catch
                {
                }
				this.trackBar1_Scroll(null,null);
			}
			string comment_setting=Registry_Settings.Read("ViewComments");
			if (comment_setting!=null)
			{
                try
                {
                    if (this.menuViewComments.Checked != bool.Parse(comment_setting))
                    {
                        this.menuViewComments_Click(null, null);
                    }
                }
                catch
                {
                }
			}
			string variable_setting=Registry_Settings.Read("ViewVariables");
			if (variable_setting!=null)
			{
                try
                {
                    if (this.menuViewVariables.Checked != bool.Parse(variable_setting))
                    {
                        this.menuViewVariables_Click(null, null);
                    }
                }
                catch
                {
                }
			}
            string graph_on_top_setting = Registry_Settings.Read("RAPTORGraphOnTop");
            if (graph_on_top_setting != null)
            {
                try
                {
                    // set it opposite of what we want
                    this.menuGraphOnTop.Checked = bool.Parse(graph_on_top_setting);
                    // click it to what we want so all the right things happen
                    //this.menuGraphOnTop_Click(null, null);
                    dotnetgraphlibrary.dotnetgraph.start_topmost = bool.Parse(graph_on_top_setting);
                }
                catch
                {
                }
            }
            string program_complete_dialog = Registry_Settings.Read("ProgramCompleteDialog");
            if (program_complete_dialog != null)
            {
                try
                {
                    this.menuProgramCompleteDialog.Checked = bool.Parse(program_complete_dialog);
                }
                catch
                {
                }
            }
            string width_setting = Registry_Settings.Read("FormWidth");
            System.Drawing.Rectangle my_rect = this.DesktopBounds;
            int my_width=-1, my_height=-1, my_x=-1, my_y = -1;
            if (width_setting != null)
            {
                try
                {
                    my_width = Int32.Parse(width_setting);
                }
                catch
                {
                }
            }
            string height_setting = Registry_Settings.Read("FormHeight");
            if (height_setting != null)
            {
                try
                {
                    my_height = Int32.Parse(height_setting);
                }
                catch
                {
                    my_height = this.DesktopBounds.Height;
                }
            }
            else
            {
                my_height = this.DesktopBounds.Height;
            }
            string x_setting = Registry_Settings.Read("FormX");
            if (x_setting != null)
            {
                try
                {
                    my_x = Int32.Parse(x_setting);
                }
                catch
                {
                }
            }
            string y_setting = Registry_Settings.Read("FormY");
            if (y_setting != null)
            {
                try
                {
                    my_y = Int32.Parse(y_setting);
                }
                catch
                {
                }
            }
            if (my_y >= 0 && my_y < System.Windows.Forms.Screen.GetWorkingArea(this).Bottom-20)
            {
                my_rect.Y = my_y;
            }
            if (my_x >= 0 && my_x < System.Windows.Forms.Screen.GetWorkingArea(this).Right-20)
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
            this.SetDesktopBounds(my_rect.X, my_rect.Y, my_rect.Width, my_rect.Height);


            Plugins.Load_Plugins("");
            Generators.Load_Generators(this);

            if (Component.USMA_mode)
			{
                this.menuIntermediate_Click(null, null);
				this.menuItemAssignment.Text = "Insert &Process";
				this.menuItemCall.Text = "Insert &Flow transfer";
				this.menuItemLoop.Text = "Insert I&teration";
			}
            this.Invalidate();

            // detect BARTPE and initialize dotnetgraph appropriately
            if (Component.MONO)
            {
                this.menuItemRunCompiled.Enabled = false;
                this.testServerButton.Enabled = false;
                this.menuItemSelectServer.Enabled = false;
                this.menuRunServer.Enabled = false;
                this.InkButton1.Enabled = false;
                this.InkButton1.Visible = false;
                this.menuPrintClipboard.Enabled = false;
                this.menuItemInk.Enabled = false;
            }
            if (Component.BARTPE && !Component.VM)
            {
                    this.menuItemGenerate.Enabled = false;
                    this.menuItemInk.Enabled = false;
                    this.InkButton1.Enabled = false;
                    this.InkButton1.Visible = false;
                    //dotnetgraphlibrary.dotnetgraph.BARTPE_Initialize(this.watchBox);
                    this.menuItemCompile.Enabled = false;
                    this.menuItemPageSetup.Enabled = false;
                    this.menuItemPrint.Enabled = false;
                    this.menuItemPrintPreview.Enabled = false;
                    this.menuItemPrintScale.Enabled = false;
                    this.menuPrintClipboard.Enabled = false;
                    //this.menuItemSelectServer.Enabled = false;
                    //this.menuRunServer.Enabled = false;
                    this.menuViewHardDrive.Visible = true;
            }

            starting_up = false;
            this.carlisle.SelectedTab = this.carlisle.TabPages[0];
        }

		/// <summary>
		/// Clean up any resources being used.
		/// </summary>
		protected override void Dispose( bool disposing )
		{
			if( disposing )
			{
				if (components != null) 
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
            this.components = new System.ComponentModel.Container();
            System.ComponentModel.ComponentResourceManager resources = new System.ComponentModel.ComponentResourceManager(typeof(Visual_Flow_Form));
            this.toolBar1 = new System.Windows.Forms.ToolBar();
            this.newButton = new System.Windows.Forms.ToolBarButton();
            this.openButton = new System.Windows.Forms.ToolBarButton();
            this.saveButton = new System.Windows.Forms.ToolBarButton();
            this.cutButton = new System.Windows.Forms.ToolBarButton();
            this.copyButton = new System.Windows.Forms.ToolBarButton();
            this.pasteButton = new System.Windows.Forms.ToolBarButton();
            this.printButton = new System.Windows.Forms.ToolBarButton();
            this.undoButton = new System.Windows.Forms.ToolBarButton();
            this.redoButton = new System.Windows.Forms.ToolBarButton();
            this.playButton = new System.Windows.Forms.ToolBarButton();
            this.pauseButton = new System.Windows.Forms.ToolBarButton();
            this.stopButton = new System.Windows.Forms.ToolBarButton();
            this.stepButton = new System.Windows.Forms.ToolBarButton();
            this.testServerButton = new System.Windows.Forms.ToolBarButton();
            this.InkButton1 = new System.Windows.Forms.ToolBarButton();
            this.contextMenu1 = new System.Windows.Forms.ContextMenu();
            this.contextMenuEdit = new System.Windows.Forms.MenuItem();
            this.contextMenuComment = new System.Windows.Forms.MenuItem();
            this.menuBreakpoint = new System.Windows.Forms.MenuItem();
            this.contextMenuCut = new System.Windows.Forms.MenuItem();
            this.contextMenuCopy = new System.Windows.Forms.MenuItem();
            this.contextMenuDelete = new System.Windows.Forms.MenuItem();
            this.imageList1 = new System.Windows.Forms.ImageList(this.components);
            this.form_splitter = new System.Windows.Forms.Splitter();
            this.control_panel = new System.Windows.Forms.Panel();
            this.watchBox = new System.Windows.Forms.TreeView();
            this.label2 = new System.Windows.Forms.Label();
            this.mainMenu1 = new System.Windows.Forms.MainMenu(this.components);
            this.menuFile = new System.Windows.Forms.MenuItem();
            this.menuItem13 = new System.Windows.Forms.MenuItem();
            this.FileOpen = new System.Windows.Forms.MenuItem();
            this.menuItem8 = new System.Windows.Forms.MenuItem();
            this.menuSaveAs = new System.Windows.Forms.MenuItem();
            this.menuViewHardDrive = new System.Windows.Forms.MenuItem();
            this.menuItem5 = new System.Windows.Forms.MenuItem();
            this.menuItemCompile = new System.Windows.Forms.MenuItem();
            this.menuItem23 = new System.Windows.Forms.MenuItem();
            this.menuItemPageSetup = new System.Windows.Forms.MenuItem();
            this.menuItemPrintScale = new System.Windows.Forms.MenuItem();
            this.printScale40 = new System.Windows.Forms.MenuItem();
            this.printScale60 = new System.Windows.Forms.MenuItem();
            this.printScale80 = new System.Windows.Forms.MenuItem();
            this.printScale100 = new System.Windows.Forms.MenuItem();
            this.printScale125 = new System.Windows.Forms.MenuItem();
            this.printScale150 = new System.Windows.Forms.MenuItem();
            this.printScale175 = new System.Windows.Forms.MenuItem();
            this.printScale200 = new System.Windows.Forms.MenuItem();
            this.printScale300 = new System.Windows.Forms.MenuItem();
            this.menuItemPrintPreview = new System.Windows.Forms.MenuItem();
            this.menuItemPrint = new System.Windows.Forms.MenuItem();
            this.menuPrintClipboard = new System.Windows.Forms.MenuItem();
            this.menuItem1 = new System.Windows.Forms.MenuItem();
            this.menuMRU1 = new System.Windows.Forms.MenuItem();
            this.menuMRU2 = new System.Windows.Forms.MenuItem();
            this.menuMRU3 = new System.Windows.Forms.MenuItem();
            this.menuMRU4 = new System.Windows.Forms.MenuItem();
            this.menuMRU5 = new System.Windows.Forms.MenuItem();
            this.menuMRU6 = new System.Windows.Forms.MenuItem();
            this.menuMRU7 = new System.Windows.Forms.MenuItem();
            this.menuMRU8 = new System.Windows.Forms.MenuItem();
            this.menuMRU9 = new System.Windows.Forms.MenuItem();
            this.menuItem25 = new System.Windows.Forms.MenuItem();
            this.menuItem14 = new System.Windows.Forms.MenuItem();
            this.menuEdit = new System.Windows.Forms.MenuItem();
            this.menuItemUndo = new System.Windows.Forms.MenuItem();
            this.menuItemRedo = new System.Windows.Forms.MenuItem();
            this.menuItem21 = new System.Windows.Forms.MenuItem();
            this.menuItemComment = new System.Windows.Forms.MenuItem();
            this.menuItemEditSelection = new System.Windows.Forms.MenuItem();
            this.menuItem22 = new System.Windows.Forms.MenuItem();
            this.menuItemCut = new System.Windows.Forms.MenuItem();
            this.menuItemCopy = new System.Windows.Forms.MenuItem();
            this.menuItemPaste = new System.Windows.Forms.MenuItem();
            this.menuItemDelete = new System.Windows.Forms.MenuItem();
            this.menuItem6 = new System.Windows.Forms.MenuItem();
            this.menuSelectAll = new System.Windows.Forms.MenuItem();
            this.menuScale = new System.Windows.Forms.MenuItem();
            this.menuScale300 = new System.Windows.Forms.MenuItem();
            this.menuScale200 = new System.Windows.Forms.MenuItem();
            this.menuScale175 = new System.Windows.Forms.MenuItem();
            this.menuScale150 = new System.Windows.Forms.MenuItem();
            this.menuScale125 = new System.Windows.Forms.MenuItem();
            this.menuScale100 = new System.Windows.Forms.MenuItem();
            this.menuScale80 = new System.Windows.Forms.MenuItem();
            this.menuScale60 = new System.Windows.Forms.MenuItem();
            this.menuScale40 = new System.Windows.Forms.MenuItem();
            this.menuView = new System.Windows.Forms.MenuItem();
            this.menuAllText = new System.Windows.Forms.MenuItem();
            this.menuTruncated = new System.Windows.Forms.MenuItem();
            this.menuNoText = new System.Windows.Forms.MenuItem();
            this.menuItem2 = new System.Windows.Forms.MenuItem();
            this.menuViewComments = new System.Windows.Forms.MenuItem();
            this.menuItem3 = new System.Windows.Forms.MenuItem();
            this.menuViewVariables = new System.Windows.Forms.MenuItem();
            this.menuItem17 = new System.Windows.Forms.MenuItem();
            this.menuExpandAll = new System.Windows.Forms.MenuItem();
            this.menuCollapseAll = new System.Windows.Forms.MenuItem();
            this.menuRun = new System.Windows.Forms.MenuItem();
            this.step = new System.Windows.Forms.MenuItem();
            this.menuExecute = new System.Windows.Forms.MenuItem();
            this.menuReset = new System.Windows.Forms.MenuItem();
            this.menuResetExecute = new System.Windows.Forms.MenuItem();
            this.menuItemRunCompiled = new System.Windows.Forms.MenuItem();
            this.menuPause = new System.Windows.Forms.MenuItem();
            this.menuItem7 = new System.Windows.Forms.MenuItem();
            this.menuItemSelectServer = new System.Windows.Forms.MenuItem();
            this.menuRunServer = new System.Windows.Forms.MenuItem();
            this.menuItem15 = new System.Windows.Forms.MenuItem();
            this.menuClearBreakpoints = new System.Windows.Forms.MenuItem();
            this.menuMode = new System.Windows.Forms.MenuItem();
            this.menuNovice = new System.Windows.Forms.MenuItem();
            this.menuIntermediate = new System.Windows.Forms.MenuItem();
            this.menuObjectiveMode = new System.Windows.Forms.MenuItem();
            this.menuItemInk = new System.Windows.Forms.MenuItem();
            this.menuItemInkOff = new System.Windows.Forms.MenuItem();
            this.menuItem20 = new System.Windows.Forms.MenuItem();
            this.menuItemInkBlack = new System.Windows.Forms.MenuItem();
            this.menuItemInkBlue = new System.Windows.Forms.MenuItem();
            this.menuItemInkGreen = new System.Windows.Forms.MenuItem();
            this.menuItemInkRed = new System.Windows.Forms.MenuItem();
            this.menuItem19 = new System.Windows.Forms.MenuItem();
            this.menuItemInkErase = new System.Windows.Forms.MenuItem();
            this.menuItem10 = new System.Windows.Forms.MenuItem();
            this.menuItemInkSelect = new System.Windows.Forms.MenuItem();
            this.menuWindow = new System.Windows.Forms.MenuItem();
            this.menuProgramCompleteDialog = new System.Windows.Forms.MenuItem();
            this.menuGraphOnTop = new System.Windows.Forms.MenuItem();
            this.menuTileVertical = new System.Windows.Forms.MenuItem();
            this.menuTileHorizontal = new System.Windows.Forms.MenuItem();
            this.DefaultWindowSize = new System.Windows.Forms.MenuItem();
            this.menuItemGenerate = new System.Windows.Forms.MenuItem();
            this.menuGenerateStandalone = new System.Windows.Forms.MenuItem();
            this.menuHelp = new System.Windows.Forms.MenuItem();
            this.menuAbout = new System.Windows.Forms.MenuItem();
            this.generalHelpMenu = new System.Windows.Forms.MenuItem();
            this.menuItem4 = new System.Windows.Forms.MenuItem();
            this.menuShowLog = new System.Windows.Forms.MenuItem();
            this.menuCountSymbols = new System.Windows.Forms.MenuItem();
            this.toolTip1 = new System.Windows.Forms.ToolTip(this.components);
            this.trackBar1 = new System.Windows.Forms.TrackBar();
            this.comboBox1 = new System.Windows.Forms.ComboBox();
            this.printDoc = new System.Drawing.Printing.PrintDocument();
            this.contextMenuInsert = new System.Windows.Forms.ContextMenu();
            this.contextMenu2Paste = new System.Windows.Forms.MenuItem();
            this.menuItemAssignment = new System.Windows.Forms.MenuItem();
            this.menuItemCall = new System.Windows.Forms.MenuItem();
            this.menuItemInput = new System.Windows.Forms.MenuItem();
            this.menuItemOutput = new System.Windows.Forms.MenuItem();
            this.menuItemIf = new System.Windows.Forms.MenuItem();
            this.menuItemLoop = new System.Windows.Forms.MenuItem();
            this.menuItemReturn = new System.Windows.Forms.MenuItem();
            this.contextMenu2 = new System.Windows.Forms.ContextMenu();
            this.menuBreakpoint2 = new System.Windows.Forms.MenuItem();
            this.carlisle = new System.Windows.Forms.TabControl();
            this.tabContextMenu = new System.Windows.Forms.ContextMenu();
            this.menuAddSubchart = new System.Windows.Forms.MenuItem();
            this.menuAddFunction = new System.Windows.Forms.MenuItem();
            this.menuAddProcedure = new System.Windows.Forms.MenuItem();
            this.menuDeleteSubchart = new System.Windows.Forms.MenuItem();
            this.menuRenameSubchart = new System.Windows.Forms.MenuItem();
            this.control_panel.SuspendLayout();
            ((System.ComponentModel.ISupportInitialize)(this.trackBar1)).BeginInit();
            this.SuspendLayout();
            // 
            // toolBar1
            // 
            this.toolBar1.Buttons.AddRange(new System.Windows.Forms.ToolBarButton[] {
            this.newButton,
            this.openButton,
            this.saveButton,
            this.cutButton,
            this.copyButton,
            this.pasteButton,
            this.printButton,
            this.undoButton,
            this.redoButton,
            this.playButton,
            this.pauseButton,
            this.stopButton,
            this.stepButton,
            this.testServerButton,
            this.InkButton1});
            this.toolBar1.ButtonSize = new System.Drawing.Size(25, 25);
            this.toolBar1.DropDownArrows = true;
            this.toolBar1.ImageList = this.imageList1;
            this.toolBar1.Location = new System.Drawing.Point(0, 0);
            this.toolBar1.Name = "toolBar1";
            this.toolBar1.ShowToolTips = true;
            this.toolBar1.Size = new System.Drawing.Size(714, 32);
            this.toolBar1.TabIndex = 0;
            this.toolBar1.ButtonClick += new System.Windows.Forms.ToolBarButtonClickEventHandler(this.toolBar1_ButtonClick);
            this.toolBar1.KeyDown += new System.Windows.Forms.KeyEventHandler(this.Visual_Flow_Form_KeyDown);
            // 
            // newButton
            // 
            this.newButton.ImageIndex = 2;
            this.newButton.Name = "newButton";
            this.newButton.ToolTipText = "New";
            // 
            // openButton
            // 
            this.openButton.ImageIndex = 3;
            this.openButton.Name = "openButton";
            this.openButton.ToolTipText = "Open";
            // 
            // saveButton
            // 
            this.saveButton.ImageIndex = 6;
            this.saveButton.Name = "saveButton";
            this.saveButton.ToolTipText = "Save";
            // 
            // cutButton
            // 
            this.cutButton.ImageIndex = 1;
            this.cutButton.Name = "cutButton";
            this.cutButton.ToolTipText = "Cut";
            // 
            // copyButton
            // 
            this.copyButton.ImageIndex = 0;
            this.copyButton.Name = "copyButton";
            this.copyButton.ToolTipText = "Copy";
            // 
            // pasteButton
            // 
            this.pasteButton.ImageIndex = 4;
            this.pasteButton.Name = "pasteButton";
            this.pasteButton.ToolTipText = "Paste";
            // 
            // printButton
            // 
            this.printButton.ImageIndex = 16;
            this.printButton.Name = "printButton";
            this.printButton.ToolTipText = "Print";
            // 
            // undoButton
            // 
            this.undoButton.ImageIndex = 14;
            this.undoButton.Name = "undoButton";
            this.undoButton.ToolTipText = "Undo";
            // 
            // redoButton
            // 
            this.redoButton.ImageIndex = 15;
            this.redoButton.Name = "redoButton";
            this.redoButton.ToolTipText = "Redo";
            // 
            // playButton
            // 
            this.playButton.ImageIndex = 11;
            this.playButton.Name = "playButton";
            this.playButton.ToolTipText = "Execute to Completion";
            // 
            // pauseButton
            // 
            this.pauseButton.ImageIndex = 12;
            this.pauseButton.Name = "pauseButton";
            this.pauseButton.ToolTipText = "Pause";
            // 
            // stopButton
            // 
            this.stopButton.ImageIndex = 13;
            this.stopButton.Name = "stopButton";
            this.stopButton.ToolTipText = "Stop/Reset";
            // 
            // stepButton
            // 
            this.stepButton.ImageIndex = 10;
            this.stepButton.Name = "stepButton";
            this.stepButton.ToolTipText = "Step to Next Shape";
            // 
            // testServerButton
            // 
            this.testServerButton.ImageIndex = 17;
            this.testServerButton.Name = "testServerButton";
            this.testServerButton.ToolTipText = "Test against server";
            // 
            // InkButton1
            // 
            this.InkButton1.DropDownMenu = this.contextMenu1;
            this.InkButton1.ImageIndex = 18;
            this.InkButton1.Name = "InkButton1";
            this.InkButton1.Style = System.Windows.Forms.ToolBarButtonStyle.ToggleButton;
            this.InkButton1.ToolTipText = "Toggle ink";
            // 
            // contextMenu1
            // 
            this.contextMenu1.MenuItems.AddRange(new System.Windows.Forms.MenuItem[] {
            this.contextMenuEdit,
            this.contextMenuComment,
            this.menuBreakpoint,
            this.contextMenuCut,
            this.contextMenuCopy,
            this.contextMenuDelete});
            this.contextMenu1.Popup += new System.EventHandler(this.contextMenu1_Popup);
            // 
            // contextMenuEdit
            // 
            this.contextMenuEdit.Index = 0;
            this.contextMenuEdit.Text = "&Edit";
            this.contextMenuEdit.Click += new System.EventHandler(this.contextMenuEdit_Click);
            // 
            // contextMenuComment
            // 
            this.contextMenuComment.Index = 1;
            this.contextMenuComment.Text = "Co&mment";
            this.contextMenuComment.Click += new System.EventHandler(this.contextMenuComment_Click);
            // 
            // menuBreakpoint
            // 
            this.menuBreakpoint.Index = 2;
            this.menuBreakpoint.Text = "Toggle &Breakpoint";
            this.menuBreakpoint.Click += new System.EventHandler(this.menuBreakpoint_Click);
            // 
            // contextMenuCut
            // 
            this.contextMenuCut.Index = 3;
            this.contextMenuCut.Text = "Cu&t";
            this.contextMenuCut.Click += new System.EventHandler(this.Cut_Click);
            // 
            // contextMenuCopy
            // 
            this.contextMenuCopy.Index = 4;
            this.contextMenuCopy.Text = "&Copy";
            this.contextMenuCopy.Click += new System.EventHandler(this.Copy_Click);
            // 
            // contextMenuDelete
            // 
            this.contextMenuDelete.Index = 5;
            this.contextMenuDelete.Text = "&Delete";
            this.contextMenuDelete.Click += new System.EventHandler(this.delete_Click);
            // 
            // imageList1
            // 
            this.imageList1.ImageStream = ((System.Windows.Forms.ImageListStreamer)(resources.GetObject("imageList1.ImageStream")));
            this.imageList1.TransparentColor = System.Drawing.Color.Transparent;
            this.imageList1.Images.SetKeyName(0, "");
            this.imageList1.Images.SetKeyName(1, "");
            this.imageList1.Images.SetKeyName(2, "");
            this.imageList1.Images.SetKeyName(3, "");
            this.imageList1.Images.SetKeyName(4, "");
            this.imageList1.Images.SetKeyName(5, "");
            this.imageList1.Images.SetKeyName(6, "");
            this.imageList1.Images.SetKeyName(7, "");
            this.imageList1.Images.SetKeyName(8, "");
            this.imageList1.Images.SetKeyName(9, "");
            this.imageList1.Images.SetKeyName(10, "");
            this.imageList1.Images.SetKeyName(11, "");
            this.imageList1.Images.SetKeyName(12, "");
            this.imageList1.Images.SetKeyName(13, "");
            this.imageList1.Images.SetKeyName(14, "");
            this.imageList1.Images.SetKeyName(15, "");
            this.imageList1.Images.SetKeyName(16, "");
            this.imageList1.Images.SetKeyName(17, "test_server.bmp");
            this.imageList1.Images.SetKeyName(18, "pen.bmp");
            // 
            // form_splitter
            // 
            this.form_splitter.BackColor = System.Drawing.SystemColors.ScrollBar;
            this.form_splitter.Location = new System.Drawing.Point(133, 32);
            this.form_splitter.MinSize = 100;
            this.form_splitter.Name = "form_splitter";
            this.form_splitter.Size = new System.Drawing.Size(3, 480);
            this.form_splitter.TabIndex = 2;
            this.form_splitter.TabStop = false;
            // 
            // control_panel
            // 
            this.control_panel.BackColor = System.Drawing.SystemColors.Control;
            this.control_panel.BorderStyle = System.Windows.Forms.BorderStyle.Fixed3D;
            this.control_panel.Controls.Add(this.watchBox);
            this.control_panel.Controls.Add(this.label2);
            this.control_panel.Dock = System.Windows.Forms.DockStyle.Left;
            this.control_panel.Location = new System.Drawing.Point(0, 32);
            this.control_panel.Name = "control_panel";
            this.control_panel.Size = new System.Drawing.Size(133, 480);
            this.control_panel.TabIndex = 0;
            this.control_panel.Paint += new System.Windows.Forms.PaintEventHandler(this.control_panel_Paint);
            this.control_panel.MouseDown += new System.Windows.Forms.MouseEventHandler(this.select_control_Shape);
            // 
            // watchBox
            // 
            this.watchBox.Dock = System.Windows.Forms.DockStyle.Fill;
            this.watchBox.Indent = 15;
            this.watchBox.Location = new System.Drawing.Point(0, 234);
            this.watchBox.Name = "watchBox";
            this.watchBox.Size = new System.Drawing.Size(129, 242);
            this.watchBox.TabIndex = 3;
            this.toolTip1.SetToolTip(this.watchBox, "Watch Window");
            this.watchBox.KeyDown += new System.Windows.Forms.KeyEventHandler(this.Visual_Flow_Form_KeyDown);
            // 
            // label2
            // 
            this.label2.AllowDrop = true;
            this.label2.BackColor = System.Drawing.Color.Transparent;
            this.label2.Dock = System.Windows.Forms.DockStyle.Top;
            this.label2.Font = new System.Drawing.Font("Times New Roman", 9.75F, ((System.Drawing.FontStyle)((System.Drawing.FontStyle.Bold | System.Drawing.FontStyle.Underline))), System.Drawing.GraphicsUnit.Point, ((byte)(0)));
            this.label2.Location = new System.Drawing.Point(0, 0);
            this.label2.Name = "label2";
            this.label2.Size = new System.Drawing.Size(129, 234);
            this.label2.TabIndex = 1;
            this.label2.Text = "           Symbols";
            this.label2.DragOver += new System.Windows.Forms.DragEventHandler(this.label2_DragOver);
            this.label2.MouseDown += new System.Windows.Forms.MouseEventHandler(this.select_control_Shape);
            // 
            // mainMenu1
            // 
            this.mainMenu1.MenuItems.AddRange(new System.Windows.Forms.MenuItem[] {
            this.menuFile,
            this.menuEdit,
            this.menuScale,
            this.menuView,
            this.menuRun,
            this.menuMode,
            this.menuItemInk,
            this.menuWindow,
            this.menuItemGenerate,
            this.menuHelp});
            // 
            // menuFile
            // 
            this.menuFile.Index = 0;
            this.menuFile.MenuItems.AddRange(new System.Windows.Forms.MenuItem[] {
            this.menuItem13,
            this.FileOpen,
            this.menuItem8,
            this.menuSaveAs,
            this.menuViewHardDrive,
            this.menuItem5,
            this.menuItemCompile,
            this.menuItem23,
            this.menuItemPageSetup,
            this.menuItemPrintScale,
            this.menuItemPrintPreview,
            this.menuItemPrint,
            this.menuPrintClipboard,
            this.menuItem1,
            this.menuMRU1,
            this.menuMRU2,
            this.menuMRU3,
            this.menuMRU4,
            this.menuMRU5,
            this.menuMRU6,
            this.menuMRU7,
            this.menuMRU8,
            this.menuMRU9,
            this.menuItem25,
            this.menuItem14});
            this.menuFile.Text = "&File";
            this.menuFile.Popup += new System.EventHandler(this.menuFile_Click);
            // 
            // menuItem13
            // 
            this.menuItem13.Index = 0;
            this.menuItem13.Shortcut = System.Windows.Forms.Shortcut.CtrlN;
            this.menuItem13.Text = "&New";
            this.menuItem13.Click += new System.EventHandler(this.new_clicked);
            // 
            // FileOpen
            // 
            this.FileOpen.Index = 1;
            this.FileOpen.Shortcut = System.Windows.Forms.Shortcut.CtrlO;
            this.FileOpen.Text = "&Open";
            this.FileOpen.Click += new System.EventHandler(this.FileOpen_Click);
            // 
            // menuItem8
            // 
            this.menuItem8.Index = 2;
            this.menuItem8.Shortcut = System.Windows.Forms.Shortcut.CtrlS;
            this.menuItem8.Text = "&Save";
            this.menuItem8.Click += new System.EventHandler(this.FileSave_Click);
            // 
            // menuSaveAs
            // 
            this.menuSaveAs.Index = 3;
            this.menuSaveAs.Text = "Save &As";
            this.menuSaveAs.Click += new System.EventHandler(this.SaveAs_Click);
            // 
            // menuViewHardDrive
            // 
            this.menuViewHardDrive.Index = 4;
            this.menuViewHardDrive.Text = "&View Hard Drive";
            this.menuViewHardDrive.Visible = false;
            this.menuViewHardDrive.Click += new System.EventHandler(this.menuViewHardDrive_Click);
            // 
            // menuItem5
            // 
            this.menuItem5.Index = 5;
            this.menuItem5.Text = "-";
            // 
            // menuItemCompile
            // 
            this.menuItemCompile.Index = 6;
            this.menuItemCompile.Text = "&Compile";
            this.menuItemCompile.Click += new System.EventHandler(this.menuItemCompile_Click);
            // 
            // menuItem23
            // 
            this.menuItem23.Index = 7;
            this.menuItem23.Text = "-";
            // 
            // menuItemPageSetup
            // 
            this.menuItemPageSetup.Index = 8;
            this.menuItemPageSetup.Text = "Page Setup";
            this.menuItemPageSetup.Click += new System.EventHandler(this.filePageSetupMenuItem_Click);
            // 
            // menuItemPrintScale
            // 
            this.menuItemPrintScale.Index = 9;
            this.menuItemPrintScale.MenuItems.AddRange(new System.Windows.Forms.MenuItem[] {
            this.printScale40,
            this.printScale60,
            this.printScale80,
            this.printScale100,
            this.printScale125,
            this.printScale150,
            this.printScale175,
            this.printScale200,
            this.printScale300});
            this.menuItemPrintScale.Text = "Print Scale";
            // 
            // printScale40
            // 
            this.printScale40.Index = 0;
            this.printScale40.Text = "40%";
            this.printScale40.Click += new System.EventHandler(this.PrintScale_20);
            // 
            // printScale60
            // 
            this.printScale60.Checked = true;
            this.printScale60.Index = 1;
            this.printScale60.Text = "60%";
            this.printScale60.Click += new System.EventHandler(this.PrintScale_40);
            // 
            // printScale80
            // 
            this.printScale80.Index = 2;
            this.printScale80.Text = "80%";
            this.printScale80.Click += new System.EventHandler(this.PrintScale_60);
            // 
            // printScale100
            // 
            this.printScale100.Index = 3;
            this.printScale100.Text = "100%";
            this.printScale100.Click += new System.EventHandler(this.PrintScale_80);
            // 
            // printScale125
            // 
            this.printScale125.Index = 4;
            this.printScale125.Text = "125%";
            this.printScale125.Click += new System.EventHandler(this.PrintScale_100);
            // 
            // printScale150
            // 
            this.printScale150.Index = 5;
            this.printScale150.Text = "150%";
            this.printScale150.Click += new System.EventHandler(this.PrintScale_150);
            // 
            // printScale175
            // 
            this.printScale175.Index = 6;
            this.printScale175.Text = "175%";
            this.printScale175.Click += new System.EventHandler(this.PrintScale_175);
            // 
            // printScale200
            // 
            this.printScale200.Index = 7;
            this.printScale200.Text = "200%";
            this.printScale200.Click += new System.EventHandler(this.PrintScale_200);
            // 
            // printScale300
            // 
            this.printScale300.Index = 8;
            this.printScale300.Text = "300%";
            this.printScale300.Click += new System.EventHandler(this.PrintScale_300);
            // 
            // menuItemPrintPreview
            // 
            this.menuItemPrintPreview.Index = 10;
            this.menuItemPrintPreview.Text = "Print Preview";
            this.menuItemPrintPreview.Click += new System.EventHandler(this.filePrintPreviewMenuItem_Click);
            // 
            // menuItemPrint
            // 
            this.menuItemPrint.Index = 11;
            this.menuItemPrint.Shortcut = System.Windows.Forms.Shortcut.CtrlP;
            this.menuItemPrint.Text = "Print";
            this.menuItemPrint.Click += new System.EventHandler(this.filePrintMenuItem_Click);
            // 
            // menuPrintClipboard
            // 
            this.menuPrintClipboard.Index = 12;
            this.menuPrintClipboard.Text = "Print to Clipboard";
            this.menuPrintClipboard.Click += new System.EventHandler(this.menuPrintClipboard_Click);
            // 
            // menuItem1
            // 
            this.menuItem1.Index = 13;
            this.menuItem1.Text = "-";
            // 
            // menuMRU1
            // 
            this.menuMRU1.Index = 14;
            this.menuMRU1.Text = "&1";
            this.menuMRU1.Click += new System.EventHandler(this.menuMRU1_Click);
            // 
            // menuMRU2
            // 
            this.menuMRU2.Index = 15;
            this.menuMRU2.Text = "&2";
            this.menuMRU2.Click += new System.EventHandler(this.menuMRU2_Click);
            // 
            // menuMRU3
            // 
            this.menuMRU3.Index = 16;
            this.menuMRU3.Text = "&3";
            this.menuMRU3.Click += new System.EventHandler(this.menuMRU3_Click);
            // 
            // menuMRU4
            // 
            this.menuMRU4.Index = 17;
            this.menuMRU4.Text = "&4";
            this.menuMRU4.Click += new System.EventHandler(this.menuMRU4_Click);
            // 
            // menuMRU5
            // 
            this.menuMRU5.Index = 18;
            this.menuMRU5.Text = "&5";
            this.menuMRU5.Click += new System.EventHandler(this.menuMRU5_Click);
            // 
            // menuMRU6
            // 
            this.menuMRU6.Index = 19;
            this.menuMRU6.Text = "&6";
            this.menuMRU6.Click += new System.EventHandler(this.menuMRU6_Click);
            // 
            // menuMRU7
            // 
            this.menuMRU7.Index = 20;
            this.menuMRU7.Text = "&7";
            this.menuMRU7.Click += new System.EventHandler(this.menuMRU7_Click);
            // 
            // menuMRU8
            // 
            this.menuMRU8.Index = 21;
            this.menuMRU8.Text = "&8";
            this.menuMRU8.Click += new System.EventHandler(this.menuMRU8_Click);
            // 
            // menuMRU9
            // 
            this.menuMRU9.Index = 22;
            this.menuMRU9.Text = "&9";
            this.menuMRU9.Click += new System.EventHandler(this.menuMRU9_Click);
            // 
            // menuItem25
            // 
            this.menuItem25.Index = 23;
            this.menuItem25.Text = "-";
            // 
            // menuItem14
            // 
            this.menuItem14.Index = 24;
            this.menuItem14.Text = "E&xit";
            this.menuItem14.Click += new System.EventHandler(this.exit_Click);
            // 
            // menuEdit
            // 
            this.menuEdit.Index = 1;
            this.menuEdit.MenuItems.AddRange(new System.Windows.Forms.MenuItem[] {
            this.menuItemUndo,
            this.menuItemRedo,
            this.menuItem21,
            this.menuItemComment,
            this.menuItemEditSelection,
            this.menuItem22,
            this.menuItemCut,
            this.menuItemCopy,
            this.menuItemPaste,
            this.menuItemDelete,
            this.menuItem6,
            this.menuSelectAll});
            this.menuEdit.Text = "&Edit";
            this.menuEdit.Popup += new System.EventHandler(this.menuEdit_Click);
            // 
            // menuItemUndo
            // 
            this.menuItemUndo.Index = 0;
            this.menuItemUndo.Shortcut = System.Windows.Forms.Shortcut.CtrlZ;
            this.menuItemUndo.Text = "&Undo";
            this.menuItemUndo.Click += new System.EventHandler(this.Undo_Click);
            // 
            // menuItemRedo
            // 
            this.menuItemRedo.Enabled = false;
            this.menuItemRedo.Index = 1;
            this.menuItemRedo.Shortcut = System.Windows.Forms.Shortcut.CtrlY;
            this.menuItemRedo.Text = "&Redo";
            this.menuItemRedo.Click += new System.EventHandler(this.Redo_Click);
            // 
            // menuItem21
            // 
            this.menuItem21.Index = 2;
            this.menuItem21.Text = "-";
            // 
            // menuItemComment
            // 
            this.menuItemComment.Index = 3;
            this.menuItemComment.Text = "Co&mment";
            this.menuItemComment.Click += new System.EventHandler(this.contextMenuComment_Click);
            // 
            // menuItemEditSelection
            // 
            this.menuItemEditSelection.Index = 4;
            this.menuItemEditSelection.Shortcut = System.Windows.Forms.Shortcut.F2;
            this.menuItemEditSelection.Text = "&Edit selection";
            this.menuItemEditSelection.Click += new System.EventHandler(this.menuItemEditSelection_Click);
            // 
            // menuItem22
            // 
            this.menuItem22.Index = 5;
            this.menuItem22.Text = "-";
            // 
            // menuItemCut
            // 
            this.menuItemCut.Index = 6;
            this.menuItemCut.Shortcut = System.Windows.Forms.Shortcut.CtrlX;
            this.menuItemCut.Text = "Cu&t";
            this.menuItemCut.Click += new System.EventHandler(this.Cut_Click);
            // 
            // menuItemCopy
            // 
            this.menuItemCopy.Index = 7;
            this.menuItemCopy.Shortcut = System.Windows.Forms.Shortcut.CtrlC;
            this.menuItemCopy.Text = "&Copy";
            this.menuItemCopy.Click += new System.EventHandler(this.Copy_Click);
            // 
            // menuItemPaste
            // 
            this.menuItemPaste.Index = 8;
            this.menuItemPaste.Shortcut = System.Windows.Forms.Shortcut.CtrlV;
            this.menuItemPaste.Text = "&Paste";
            this.menuItemPaste.Click += new System.EventHandler(this.paste_Click);
            // 
            // menuItemDelete
            // 
            this.menuItemDelete.Index = 9;
            this.menuItemDelete.Shortcut = System.Windows.Forms.Shortcut.Del;
            this.menuItemDelete.Text = "&Delete";
            this.menuItemDelete.Click += new System.EventHandler(this.delete_Click);
            // 
            // menuItem6
            // 
            this.menuItem6.Index = 10;
            this.menuItem6.Text = "-";
            // 
            // menuSelectAll
            // 
            this.menuSelectAll.Index = 11;
            this.menuSelectAll.Shortcut = System.Windows.Forms.Shortcut.CtrlA;
            this.menuSelectAll.Text = "Select &All";
            this.menuSelectAll.Click += new System.EventHandler(this.menuSelectAll_Click);
            // 
            // menuScale
            // 
            this.menuScale.Index = 2;
            this.menuScale.MenuItems.AddRange(new System.Windows.Forms.MenuItem[] {
            this.menuScale300,
            this.menuScale200,
            this.menuScale175,
            this.menuScale150,
            this.menuScale125,
            this.menuScale100,
            this.menuScale80,
            this.menuScale60,
            this.menuScale40});
            this.menuScale.Text = "&Scale";
            // 
            // menuScale300
            // 
            this.menuScale300.Index = 0;
            this.menuScale300.Text = "300%";
            this.menuScale300.Click += new System.EventHandler(this.menuScale300_Click);
            // 
            // menuScale200
            // 
            this.menuScale200.Index = 1;
            this.menuScale200.Text = "200%";
            this.menuScale200.Click += new System.EventHandler(this.menuScale200_Click);
            // 
            // menuScale175
            // 
            this.menuScale175.Index = 2;
            this.menuScale175.Text = "175%";
            this.menuScale175.Click += new System.EventHandler(this.menuScale175_Click);
            // 
            // menuScale150
            // 
            this.menuScale150.Index = 3;
            this.menuScale150.Text = "150%";
            this.menuScale150.Click += new System.EventHandler(this.menuScale150_Click);
            // 
            // menuScale125
            // 
            this.menuScale125.Index = 4;
            this.menuScale125.Text = "125%";
            this.menuScale125.Click += new System.EventHandler(this.Scale_100);
            // 
            // menuScale100
            // 
            this.menuScale100.Checked = true;
            this.menuScale100.Index = 5;
            this.menuScale100.Text = "100%";
            this.menuScale100.Click += new System.EventHandler(this.Scale_80);
            // 
            // menuScale80
            // 
            this.menuScale80.Index = 6;
            this.menuScale80.Text = "80%";
            this.menuScale80.Click += new System.EventHandler(this.Scale_60);
            // 
            // menuScale60
            // 
            this.menuScale60.Index = 7;
            this.menuScale60.Text = "60%";
            this.menuScale60.Click += new System.EventHandler(this.Scale_40);
            // 
            // menuScale40
            // 
            this.menuScale40.Index = 8;
            this.menuScale40.Text = "40%";
            this.menuScale40.Click += new System.EventHandler(this.Scale_20);
            // 
            // menuView
            // 
            this.menuView.Index = 3;
            this.menuView.MenuItems.AddRange(new System.Windows.Forms.MenuItem[] {
            this.menuAllText,
            this.menuTruncated,
            this.menuNoText,
            this.menuItem2,
            this.menuViewComments,
            this.menuItem3,
            this.menuViewVariables,
            this.menuItem17,
            this.menuExpandAll,
            this.menuCollapseAll});
            this.menuView.Text = "&View";
            // 
            // menuAllText
            // 
            this.menuAllText.Checked = true;
            this.menuAllText.Index = 0;
            this.menuAllText.Text = "&All text";
            this.menuAllText.Click += new System.EventHandler(this.menuAllText_Click);
            // 
            // menuTruncated
            // 
            this.menuTruncated.Index = 1;
            this.menuTruncated.Text = "&Truncated";
            this.menuTruncated.Click += new System.EventHandler(this.menuTruncated_Click);
            // 
            // menuNoText
            // 
            this.menuNoText.Index = 2;
            this.menuNoText.Text = "&No text";
            this.menuNoText.Click += new System.EventHandler(this.menuNoText_Click);
            // 
            // menuItem2
            // 
            this.menuItem2.Index = 3;
            this.menuItem2.Text = "-";
            // 
            // menuViewComments
            // 
            this.menuViewComments.Checked = true;
            this.menuViewComments.Index = 4;
            this.menuViewComments.Text = "&Comments";
            this.menuViewComments.Click += new System.EventHandler(this.menuViewComments_Click);
            // 
            // menuItem3
            // 
            this.menuItem3.Index = 5;
            this.menuItem3.Text = "-";
            // 
            // menuViewVariables
            // 
            this.menuViewVariables.Checked = true;
            this.menuViewVariables.Index = 6;
            this.menuViewVariables.Text = "&Variables";
            this.menuViewVariables.Click += new System.EventHandler(this.menuViewVariables_Click);
            // 
            // menuItem17
            // 
            this.menuItem17.Index = 7;
            this.menuItem17.Text = "-";
            // 
            // menuExpandAll
            // 
            this.menuExpandAll.Index = 8;
            this.menuExpandAll.Text = "E&xpand all";
            this.menuExpandAll.Click += new System.EventHandler(this.menuExpandAll_Click);
            // 
            // menuCollapseAll
            // 
            this.menuCollapseAll.Index = 9;
            this.menuCollapseAll.Text = "C&ollapse all";
            this.menuCollapseAll.Click += new System.EventHandler(this.menuCollapseAll_Click);
            // 
            // menuRun
            // 
            this.menuRun.Index = 4;
            this.menuRun.MenuItems.AddRange(new System.Windows.Forms.MenuItem[] {
            this.step,
            this.menuExecute,
            this.menuReset,
            this.menuResetExecute,
            this.menuItemRunCompiled,
            this.menuPause,
            this.menuItem7,
            this.menuItemSelectServer,
            this.menuRunServer,
            this.menuItem15,
            this.menuClearBreakpoints});
            this.menuRun.Text = "&Run";
            this.menuRun.Popup += new System.EventHandler(this.menuRun_Popup);
            // 
            // step
            // 
            this.step.Index = 0;
            this.step.Shortcut = System.Windows.Forms.Shortcut.F10;
            this.step.Text = "&Step";
            this.step.Click += new System.EventHandler(this.menuStep_Click);
            // 
            // menuExecute
            // 
            this.menuExecute.Index = 1;
            this.menuExecute.Text = "E&xecute to Completion";
            this.menuExecute.Click += new System.EventHandler(this.menuExecute_Click);
            // 
            // menuReset
            // 
            this.menuReset.Index = 2;
            this.menuReset.Text = "&Reset";
            this.menuReset.Click += new System.EventHandler(this.menuReset_Click);
            // 
            // menuResetExecute
            // 
            this.menuResetExecute.Index = 3;
            this.menuResetExecute.Shortcut = System.Windows.Forms.Shortcut.F5;
            this.menuResetExecute.Text = "Reset/Execute";
            this.menuResetExecute.Click += new System.EventHandler(this.menuResetExecute_Click);
            // 
            // menuItemRunCompiled
            // 
            this.menuItemRunCompiled.Index = 4;
            this.menuItemRunCompiled.Shortcut = System.Windows.Forms.Shortcut.CtrlF5;
            this.menuItemRunCompiled.Text = "R&un Compiled";
            this.menuItemRunCompiled.Click += new System.EventHandler(this.menuItemRunCompiled_Click);
            // 
            // menuPause
            // 
            this.menuPause.Index = 5;
            this.menuPause.Text = "&Pause";
            this.menuPause.Click += new System.EventHandler(this.menuPause_Click);
            // 
            // menuItem7
            // 
            this.menuItem7.Index = 6;
            this.menuItem7.Text = "-";
            // 
            // menuItemSelectServer
            // 
            this.menuItemSelectServer.Index = 7;
            this.menuItemSelectServer.Text = "Se&lect server";
            this.menuItemSelectServer.Click += new System.EventHandler(this.ConfigServer_Click);
            // 
            // menuRunServer
            // 
            this.menuRunServer.Index = 8;
            this.menuRunServer.Shortcut = System.Windows.Forms.Shortcut.F2;
            this.menuRunServer.Text = "Test against ser&ver";
            this.menuRunServer.Click += new System.EventHandler(this.menuRunServer_Click);
            // 
            // menuItem15
            // 
            this.menuItem15.Index = 9;
            this.menuItem15.Text = "-";
            // 
            // menuClearBreakpoints
            // 
            this.menuClearBreakpoints.Index = 10;
            this.menuClearBreakpoints.Text = "&Clear all Breakpoints";
            this.menuClearBreakpoints.Click += new System.EventHandler(this.menuClearBreakpoints_Click);
            // 
            // menuMode
            // 
            this.menuMode.Index = 5;
            this.menuMode.MenuItems.AddRange(new System.Windows.Forms.MenuItem[] {
            this.menuNovice,
            this.menuIntermediate,
            this.menuObjectiveMode});
            this.menuMode.Text = "&Mode";
            // 
            // menuNovice
            // 
            this.menuNovice.Checked = true;
            this.menuNovice.Index = 0;
            this.menuNovice.Text = "&Novice";
            this.menuNovice.Click += new System.EventHandler(this.menuNovice_Click);
            // 
            // menuIntermediate
            // 
            this.menuIntermediate.Index = 1;
            this.menuIntermediate.Text = "&Intermediate";
            this.menuIntermediate.Click += new System.EventHandler(this.menuIntermediate_Click);
            // 
            // menuObjectiveMode
            // 
            this.menuObjectiveMode.Index = 2;
            this.menuObjectiveMode.Text = "&Object-oriented";
            this.menuObjectiveMode.Click += new System.EventHandler(this.menuObjectiveMode_Click);
            // 
            // menuItemInk
            // 
            this.menuItemInk.Index = 6;
            this.menuItemInk.MenuItems.AddRange(new System.Windows.Forms.MenuItem[] {
            this.menuItemInkOff,
            this.menuItem20,
            this.menuItemInkBlack,
            this.menuItemInkBlue,
            this.menuItemInkGreen,
            this.menuItemInkRed,
            this.menuItem19,
            this.menuItemInkErase,
            this.menuItem10,
            this.menuItemInkSelect});
            this.menuItemInk.Text = "&Ink";
            // 
            // menuItemInkOff
            // 
            this.menuItemInkOff.Checked = true;
            this.menuItemInkOff.Index = 0;
            this.menuItemInkOff.Text = "&Off";
            this.menuItemInkOff.Click += new System.EventHandler(this.menuItemInkOff_Click);
            // 
            // menuItem20
            // 
            this.menuItem20.Index = 1;
            this.menuItem20.Text = "-";
            // 
            // menuItemInkBlack
            // 
            this.menuItemInkBlack.Index = 2;
            this.menuItemInkBlack.Text = "&Black";
            this.menuItemInkBlack.Click += new System.EventHandler(this.menuItemInkBlack_Click);
            // 
            // menuItemInkBlue
            // 
            this.menuItemInkBlue.Index = 3;
            this.menuItemInkBlue.Text = "Bl&ue";
            this.menuItemInkBlue.Click += new System.EventHandler(this.menuItemInkBlue_Click);
            // 
            // menuItemInkGreen
            // 
            this.menuItemInkGreen.Index = 4;
            this.menuItemInkGreen.Text = "&Green";
            this.menuItemInkGreen.Click += new System.EventHandler(this.menuItemInkGreen_Click);
            // 
            // menuItemInkRed
            // 
            this.menuItemInkRed.Index = 5;
            this.menuItemInkRed.Text = "&Red";
            this.menuItemInkRed.Click += new System.EventHandler(this.menuItemInkRed_Click);
            // 
            // menuItem19
            // 
            this.menuItem19.Index = 6;
            this.menuItem19.Text = "-";
            // 
            // menuItemInkErase
            // 
            this.menuItemInkErase.Index = 7;
            this.menuItemInkErase.Text = "&Eraser";
            this.menuItemInkErase.Click += new System.EventHandler(this.menuItemInkErase_Click);
            // 
            // menuItem10
            // 
            this.menuItem10.Index = 8;
            this.menuItem10.Text = "-";
            // 
            // menuItemInkSelect
            // 
            this.menuItemInkSelect.Index = 9;
            this.menuItemInkSelect.Text = "&Select";
            this.menuItemInkSelect.Click += new System.EventHandler(this.menuItemInkSelect_Click);
            // 
            // menuWindow
            // 
            this.menuWindow.Index = 7;
            this.menuWindow.MenuItems.AddRange(new System.Windows.Forms.MenuItem[] {
            this.menuProgramCompleteDialog,
            this.menuGraphOnTop,
            this.menuTileVertical,
            this.menuTileHorizontal,
            this.DefaultWindowSize});
            this.menuWindow.Text = "&Window";
            // 
            // menuProgramCompleteDialog
            // 
            this.menuProgramCompleteDialog.Index = 0;
            this.menuProgramCompleteDialog.Text = "Display \"Flowchart &Complete\" dialog";
            this.menuProgramCompleteDialog.Click += new System.EventHandler(this.menuProgramCompleteDialog_Click);
            // 
            // menuGraphOnTop
            // 
            this.menuGraphOnTop.Checked = true;
            this.menuGraphOnTop.Index = 1;
            this.menuGraphOnTop.Text = "Keep RAPTORGraph on &Top";
            this.menuGraphOnTop.Click += new System.EventHandler(this.menuGraphOnTop_Click);
            // 
            // menuTileVertical
            // 
            this.menuTileVertical.Index = 2;
            this.menuTileVertical.Text = "Tile &Vertical";
            this.menuTileVertical.Click += new System.EventHandler(this.menuTileVertical_Click);
            // 
            // menuTileHorizontal
            // 
            this.menuTileHorizontal.Index = 3;
            this.menuTileHorizontal.Text = "Tile &Horizontal";
            this.menuTileHorizontal.Click += new System.EventHandler(this.menuTileHorizontal_Click);
            // 
            // DefaultWindowSize
            // 
            this.DefaultWindowSize.Index = 4;
            this.DefaultWindowSize.Text = "&Default sizes";
            this.DefaultWindowSize.Click += new System.EventHandler(this.DefaultWindowSize_Click);
            // 
            // menuItemGenerate
            // 
            this.menuItemGenerate.Index = 8;
            this.menuItemGenerate.MenuItems.AddRange(new System.Windows.Forms.MenuItem[] {
            this.menuGenerateStandalone});
            this.menuItemGenerate.Text = "&Generate";
            this.menuItemGenerate.Popup += new System.EventHandler(this.menuItemGenerate_Popup);
            // 
            // menuGenerateStandalone
            // 
            this.menuGenerateStandalone.Index = 0;
            this.menuGenerateStandalone.Text = "&Standalone";
            this.menuGenerateStandalone.Click += new System.EventHandler(this.menuGenerateStandalone_Click);
            // 
            // menuHelp
            // 
            this.menuHelp.Index = 9;
            this.menuHelp.MenuItems.AddRange(new System.Windows.Forms.MenuItem[] {
            this.menuAbout,
            this.generalHelpMenu,
            this.menuItem4,
            this.menuShowLog,
            this.menuCountSymbols});
            this.menuHelp.Text = "&Help";
            // 
            // menuAbout
            // 
            this.menuAbout.Index = 0;
            this.menuAbout.Text = "&About";
            this.menuAbout.Click += new System.EventHandler(this.menuAbout_Click);
            // 
            // generalHelpMenu
            // 
            this.generalHelpMenu.Index = 1;
            this.generalHelpMenu.Shortcut = System.Windows.Forms.Shortcut.F1;
            this.generalHelpMenu.Text = "&General Help";
            this.generalHelpMenu.Click += new System.EventHandler(this.listOfFunctionsMenu_Click);
            // 
            // menuItem4
            // 
            this.menuItem4.Index = 2;
            this.menuItem4.Text = "-";
            // 
            // menuShowLog
            // 
            this.menuShowLog.Index = 3;
            this.menuShowLog.Text = "&Show log";
            this.menuShowLog.Click += new System.EventHandler(this.menuShowLog_Click);
            // 
            // menuCountSymbols
            // 
            this.menuCountSymbols.Index = 4;
            this.menuCountSymbols.Text = "&Count symbols";
            this.menuCountSymbols.Click += new System.EventHandler(this.menuCountSymbols_Click);
            // 
            // trackBar1
            // 
            this.trackBar1.AutoSize = false;
            this.trackBar1.BackColor = System.Drawing.Color.FromArgb(((int)(((byte)(237)))), ((int)(((byte)(232)))), ((int)(((byte)(217)))));
            this.trackBar1.Location = new System.Drawing.Point(426, 2);
            this.trackBar1.Name = "trackBar1";
            this.trackBar1.Size = new System.Drawing.Size(112, 24);
            this.trackBar1.TabIndex = 4;
            this.trackBar1.TickStyle = System.Windows.Forms.TickStyle.None;
            this.toolTip1.SetToolTip(this.trackBar1, "Play Speed");
            this.trackBar1.Value = 6;
            this.trackBar1.Scroll += new System.EventHandler(this.trackBar1_Scroll);
            this.trackBar1.KeyDown += new System.Windows.Forms.KeyEventHandler(this.Visual_Flow_Form_KeyDown);
            // 
            // comboBox1
            // 
            this.comboBox1.DropDownStyle = System.Windows.Forms.ComboBoxStyle.DropDownList;
            this.comboBox1.Items.AddRange(new object[] {
            "300%",
            "200%",
            "175%",
            "150%",
            "125%",
            "100%",
            "80%",
            "60%",
            "40%"});
            this.comboBox1.Location = new System.Drawing.Point(546, 2);
            this.comboBox1.Name = "comboBox1";
            this.comboBox1.Size = new System.Drawing.Size(64, 24);
            this.comboBox1.TabIndex = 5;
            this.toolTip1.SetToolTip(this.comboBox1, "Scale");
            this.comboBox1.SelectedIndexChanged += new System.EventHandler(this.comboBox1_SelectedIndexChanged);
            this.comboBox1.KeyDown += new System.Windows.Forms.KeyEventHandler(this.Visual_Flow_Form_KeyDown);
            // 
            // printDoc
            // 
            this.printDoc.PrintPage += new System.Drawing.Printing.PrintPageEventHandler(this.printDoc_PrintPage);
            this.printDoc.BeginPrint += new System.Drawing.Printing.PrintEventHandler(this.printDoc_BeginPrint);
            // 
            // contextMenuInsert
            // 
            this.contextMenuInsert.MenuItems.AddRange(new System.Windows.Forms.MenuItem[] {
            this.contextMenu2Paste,
            this.menuItemAssignment,
            this.menuItemCall,
            this.menuItemInput,
            this.menuItemOutput,
            this.menuItemIf,
            this.menuItemLoop,
            this.menuItemReturn});
            this.contextMenuInsert.Popup += new System.EventHandler(this.contextMenuInsert_Popup);
            // 
            // contextMenu2Paste
            // 
            this.contextMenu2Paste.Index = 0;
            this.contextMenu2Paste.Text = "Paste";
            this.contextMenu2Paste.Click += new System.EventHandler(this.paste_Click);
            // 
            // menuItemAssignment
            // 
            this.menuItemAssignment.Index = 1;
            this.menuItemAssignment.Text = "Insert &Assignment";
            this.menuItemAssignment.Click += new System.EventHandler(this.menuItemAssignment_Click);
            // 
            // menuItemCall
            // 
            this.menuItemCall.Index = 2;
            this.menuItemCall.Text = "Insert &Call";
            this.menuItemCall.Click += new System.EventHandler(this.menuItemCall_Click);
            // 
            // menuItemInput
            // 
            this.menuItemInput.Index = 3;
            this.menuItemInput.Text = "Insert &Input";
            this.menuItemInput.Click += new System.EventHandler(this.menuItemParallelogram_Click);
            // 
            // menuItemOutput
            // 
            this.menuItemOutput.Index = 4;
            this.menuItemOutput.Text = "Insert &Output";
            this.menuItemOutput.Click += new System.EventHandler(this.menuItemOutput_Click);
            // 
            // menuItemIf
            // 
            this.menuItemIf.Index = 5;
            this.menuItemIf.Text = "Insert &Selection";
            this.menuItemIf.Click += new System.EventHandler(this.menuItemIf_Click);
            // 
            // menuItemLoop
            // 
            this.menuItemLoop.Index = 6;
            this.menuItemLoop.Text = "Insert &Loop";
            this.menuItemLoop.Click += new System.EventHandler(this.menuItemLoop_Click);
            // 
            // menuItemReturn
            // 
            this.menuItemReturn.Index = 7;
            this.menuItemReturn.Text = "Insert &Return";
            this.menuItemReturn.Click += new System.EventHandler(this.menuItemReturn_Click);
            // 
            // contextMenu2
            // 
            this.contextMenu2.MenuItems.AddRange(new System.Windows.Forms.MenuItem[] {
            this.menuBreakpoint2});
            // 
            // menuBreakpoint2
            // 
            this.menuBreakpoint2.Index = 0;
            this.menuBreakpoint2.Text = "Toggle &Breakpoint";
            this.menuBreakpoint2.Click += new System.EventHandler(this.menuBreakpoint_Click);
            // 
            // carlisle
            // 
            this.carlisle.Dock = System.Windows.Forms.DockStyle.Fill;
            this.carlisle.Location = new System.Drawing.Point(136, 32);
            this.carlisle.Name = "carlisle";
            this.carlisle.SelectedIndex = 0;
            this.carlisle.Size = new System.Drawing.Size(578, 480);
            this.carlisle.TabIndex = 6;
            this.carlisle.MouseMove += new System.Windows.Forms.MouseEventHandler(this.tabControl1_MouseMove);
            this.carlisle.MouseDown += new System.Windows.Forms.MouseEventHandler(this.tabControl1_MouseDown);
            this.carlisle.TabIndexChanged += new System.EventHandler(this.tabControl1_TabIndexChanged);
            this.carlisle.MouseUp += new System.Windows.Forms.MouseEventHandler(this.tabControl1_MouseUp);
            // 
            // tabContextMenu
            // 
            this.tabContextMenu.MenuItems.AddRange(new System.Windows.Forms.MenuItem[] {
            this.menuAddSubchart,
            this.menuAddFunction,
            this.menuAddProcedure,
            this.menuDeleteSubchart,
            this.menuRenameSubchart});
            this.tabContextMenu.Popup += new System.EventHandler(this.tabContextMenu_Popup);
            // 
            // menuAddSubchart
            // 
            this.menuAddSubchart.Index = 0;
            this.menuAddSubchart.Text = "&Add subchart";
            this.menuAddSubchart.Click += new System.EventHandler(this.menuAddSubchart_Click);
            // 
            // menuAddFunction
            // 
            this.menuAddFunction.Index = 1;
            this.menuAddFunction.Text = "Add &function";
            this.menuAddFunction.Visible = false;
            // 
            // menuAddProcedure
            // 
            this.menuAddProcedure.Index = 2;
            this.menuAddProcedure.Text = "Add &procedure";
            this.menuAddProcedure.Visible = false;
            this.menuAddProcedure.Click += new System.EventHandler(this.menuAddProcedure_Click);
            // 
            // menuDeleteSubchart
            // 
            this.menuDeleteSubchart.Index = 3;
            this.menuDeleteSubchart.Text = "&Delete";
            this.menuDeleteSubchart.Click += new System.EventHandler(this.menuDeleteSubchart_Click);
            // 
            // menuRenameSubchart
            // 
            this.menuRenameSubchart.Index = 4;
            this.menuRenameSubchart.Text = "&Rename";
            this.menuRenameSubchart.Click += new System.EventHandler(this.menuRenameSubchart_Click);
            // 
            // Visual_Flow_Form
            // 
            this.AutoScaleBaseSize = new System.Drawing.Size(6, 15);
            this.BackColor = System.Drawing.SystemColors.Window;
            this.ClientSize = new System.Drawing.Size(714, 512);
            this.Controls.Add(this.carlisle);
            this.Controls.Add(this.comboBox1);
            this.Controls.Add(this.trackBar1);
            this.Controls.Add(this.form_splitter);
            this.Controls.Add(this.control_panel);
            this.Controls.Add(this.toolBar1);
            this.Font = new System.Drawing.Font("Microsoft Sans Serif", 9.75F, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, ((byte)(0)));
            this.Icon = ((System.Drawing.Icon)(resources.GetObject("$this.Icon")));
            this.Menu = this.mainMenu1;
            this.Name = "Visual_Flow_Form";
            this.StartPosition = System.Windows.Forms.FormStartPosition.Manual;
            this.Text = "Raptor";
            this.Deactivate += new System.EventHandler(this.Visual_Flow_Form_Deactivate);
            this.Activated += new System.EventHandler(this.Visual_Flow_Form_Activated);
            this.Closing += new System.ComponentModel.CancelEventHandler(this.Visual_Flow_Form_Closing);
            this.Move += new System.EventHandler(this.Visual_Flow_Form_Move);
            this.Resize += new System.EventHandler(this.Visual_Flow_Form_Resize);
            this.KeyDown += new System.Windows.Forms.KeyEventHandler(this.Visual_Flow_Form_KeyDown);
            this.control_panel.ResumeLayout(false);
            ((System.ComponentModel.ISupportInitialize)(this.trackBar1)).EndInit();
            this.ResumeLayout(false);
            this.PerformLayout();

		}
		#endregion

		/// <summary>
		/// The main entry point for the application.
		/// </summary>
		[STAThread]
		static void Main(string[] args) 
		{
            // example:  raptor nameofraptor.rap /run inputfile.txt outputfile.txt
            if (!Autoupdate.Autoupdate_Requested())
			{

                Visual_Flow_Form form;
                if (args != null && args.Length >= 1)
                {
                    if (args.Length >= 2 && args[1].ToLower() == "/runsilent")
                    {
                        form = new Visual_Flow_Form(false);
                        commandLineRun(args, true, form);
                        return;
                    }
                    else if (args.Length >= 2 && args[1] == "/run")
                    {
                        form = new Visual_Flow_Form(args.Length>=4);
                        commandLineRun(args, false, form);
                        return;
                    }
                    else
                    {
                        form = new Visual_Flow_Form(false);
                        form.loadTimer = new System.Timers.Timer(500.0);
                        form.loadTimer.Elapsed += new System.Timers.ElapsedEventHandler(form.loader);
                        form.load_filename = args[0];
                        form.loadTimer.Start();

                        //form.Load_File(args[0]);
                    }
                }
                else
                {
                    form = new Visual_Flow_Form(false);
                }
				Application.Run(form);
			}
		}

        private static void commandLineRun(string[] args, bool silent, Visual_Flow_Form form)
        {
            bool file_failed = false;
            bool remember = Component.compiled_flowchart;
            command_line_run = true;
            try
            {
                form.Load_File(args[0]);
                if (Component.compiled_flowchart)
                {
                    throw new System.Exception("can't run compiled file from commandline");
                }
                Compile_Helpers.Compile_Flowchart(form.carlisle.TabPages);
            }
            catch (System.Exception exc)
            {
                file_failed = true;
            }
            form.full_speed = true;
            form.trackBar1.Value = form.trackBar1.Maximum;
            form.trackBar1_Scroll(null, null);
            form.menuReset_Click(null, null);
            Component.compiled_flowchart = true;
            // check for input file
            try
            {
                if (silent)
                {
                    ada_runtime_pkg.redirect_standard_input();
                    command_line_input_redirect = true;
                }
                else if (args.Length >= 3)
                {
                    numbers.value v1 = numbers_pkg.make_string_value(args[2]);
                    ada_runtime_pkg.redirect_input(v1);
                    command_line_input_redirect = true;
                }
            }
            catch
            {
                MessageBox.Show("Failed reading: " + args[2]);
                Application.Exit();
            }
            // check for output file
            try
            {
                if (silent)
                {
                    ada_runtime_pkg.redirect_standard_output();
                    command_line_output_redirect = true;
                }
                else if (args.Length >= 4)
                {
                    numbers.value v1 = numbers_pkg.make_string_value(args[3]);
                    ada_runtime_pkg.redirect_output(v1);
                    command_line_output_redirect = true;
                }
            }
            catch
            {
                MessageBox.Show("Failed creating: " + args[2]);
                Application.Exit();
            }
            try
            {
                if (!file_failed)
                {
                    Compile_Helpers.Run_Compiled_NoThread(true);
                    Component.compiled_flowchart = remember;
                }
                else
                {
                    raptor.Runtime.consoleWriteln("Was unable to either load or compile file");
                }
            }
            catch (System.Exception exc)
            {
                raptor.Runtime.consoleWriteln("Exception!: " + exc.Message);
            }
            form.Close();
            if (command_line_output_redirect)
            {
                raptor_files_pkg.stop_redirect_output();
            }
            Application.Exit();
            return;
        }



		// private void control_panel_Paint(object sender, System.Windows.Forms.PaintEventArgs e)
		// This method is used to draw the control figures in the left panel
		// It is also used to select the control figure to insert in the flow
		// panel.  When a control figure is selected, it will be colored red.
		private void control_panel_Paint(object sender, System.Windows.Forms.PaintEventArgs e)
		{
			ASGN.selected = false;
			CALL.selected = false;
			INPUT.selected = false;
			OUTPUT.selected = false;
			IFC.selected = false;
			LP.selected = false;
            RETURN.selected = false;

			if (control_figure_selected == assignment_fig)
			{
				ASGN.selected = true;
			}
			else if (control_figure_selected == call_fig)
			{
				CALL.selected = true;
			}
			else if (control_figure_selected == input_fig)
			{
				INPUT.selected = true;
			}
			else if (control_figure_selected == output_fig)
			{
				OUTPUT.selected = true;
			}
			else if (control_figure_selected == if_control_fig)
			{
				IFC.selected = true;
			}
			else if (control_figure_selected == loop_fig)
			{
				LP.selected = true;
			}
            else if (control_figure_selected == return_fig)
            {
                RETURN.selected = true;
            }
			ASGN.draw(e.Graphics,control_X,25);
			ASGN.X = control_X;
			ASGN.Y = 25;
            if (!Component.USMA_mode)
            {
                e.Graphics.DrawString("Assignment",
                    PensBrushes.times10,
                    PensBrushes.blackbrush,
                    new System.Drawing.Point(control_X - ASGN.W / 2 - 15, 50));
            }
            else
            {
                e.Graphics.DrawString("Process",
                    PensBrushes.times10,
                    PensBrushes.blackbrush,
                    new System.Drawing.Point(control_X - ASGN.W / 2 - 5, 50));
            } 
            CALL.Y = 68;
            if (Component.Current_Mode == Mode.Expert)
            {
                CALL.X = control_X - 3 * ASGN.W / 4;
                RETURN.X = control_X + 3 * ASGN.W / 4+3;
                RETURN.Y = CALL.Y;
                RETURN.draw(e.Graphics, RETURN.X, RETURN.Y);
                e.Graphics.DrawString("Return",
                    PensBrushes.times10,
                    PensBrushes.blackbrush,
                    new System.Drawing.Point(control_X + ASGN.W / 2 - 8, 93));
            }
            else
            {
                CALL.X = control_X;
            }
            CALL.draw(e.Graphics, CALL.X, 68);

			if (!Component.USMA_mode)
			{
				e.Graphics.DrawString("Call",
					PensBrushes.times10,
					PensBrushes.blackbrush,
					new System.Drawing.Point(CALL.X-10,93));
			}
			else
			{
				e.Graphics.DrawString("Flow transfer",
					PensBrushes.times10,
					PensBrushes.blackbrush,
					new System.Drawing.Point(CALL.X-ASGN.W/2-20,93));
			}
			INPUT.draw(e.Graphics,control_X-3*ASGN.W/4,111);
			INPUT.X = control_X-3*ASGN.W/4;
			INPUT.Y = 111;
			e.Graphics.DrawString("Input",
				PensBrushes.times10,
				PensBrushes.blackbrush,
				new System.Drawing.Point(control_X-INPUT.W-8,136));
			OUTPUT.draw(e.Graphics,control_X+3*ASGN.W/4+3,111);
			OUTPUT.X = control_X+3*ASGN.W/4+3;
			OUTPUT.Y = 111;
			e.Graphics.DrawString("Output",
				PensBrushes.times10,
				PensBrushes.blackbrush,
				new System.Drawing.Point(control_X+ASGN.W/2-8,136));
			IFC.draw(e.Graphics,control_X,153);
			IFC.X = control_X;
			IFC.Y = 153;
			e.Graphics.DrawString("Selection",
				PensBrushes.times10,
				PensBrushes.blackbrush,
				new System.Drawing.Point(control_X-ASGN.W/2-8,168));
			LP.draw(e.Graphics,control_X,183);
			LP.X = control_X;
			LP.Y = 183;
			if (!Component.USMA_mode)
			{
				e.Graphics.DrawString("Loop",
					PensBrushes.times10,
					PensBrushes.blackbrush,
					new System.Drawing.Point(control_X-ASGN.W/2+3,218));
			}
			else
			{
				e.Graphics.DrawString("Iteration",
					PensBrushes.times10,
					PensBrushes.blackbrush,
					new System.Drawing.Point(control_X-ASGN.W/2-8,218));
			}
		}

		// public void Create_Control_graphx()
		// This method is used to create the control figures
		// that will be used as choices in the left panel. 
		public void Create_Control_graphx()
		{
			ASGN = new Rectangle(control_height, control_width, 
				"Rectangle", Rectangle.Kind_Of.Assignment);
			CALL = new Rectangle(control_height, control_width, 
				"Rectangle", Rectangle.Kind_Of.Call);
			INPUT = new Parallelogram(control_height, control_width, 
				"Parallelogram", true);
			OUTPUT = new Parallelogram(control_height, control_width, 
				"Parallelogram", false);
            RETURN = new Oval_Return(control_height, control_width,
                "Return");
			IFC = new IF_Control(control_height-15, control_width-15, "IF_Control");
			LP = new Loop(control_height-15, control_width-15, "Loop");
		}

		// private void select_control_Shape(object sender, System.Windows.Forms.MouseEventArgs e)
		// This method is used to select the desired control figure
		// in the left panel.
		private void select_control_Shape(object sender, System.Windows.Forms.MouseEventArgs e)
		{
			mouse_x = e.X;
			mouse_y = e.Y;

			if (e.Button == MouseButtons.Left) 
			{
				ASGN.selected = false;
				CALL.selected = false;
				INPUT.selected = false;
				OUTPUT.selected = false;
				IFC.selected = false;
				LP.selected = false;

				if (ASGN.In_Footprint(mouse_x, mouse_y))
				{
					control_figure_selected = assignment_fig;
					ASGN.selected = true;
					control_panel.Invalidate();
                    this.DoDragDrop("raptor_ASGN", DragDropEffects.Copy | DragDropEffects.Link);
				}
				else if (CALL.In_Footprint(mouse_x, mouse_y))
				{
					control_figure_selected = call_fig;
					CALL.selected = true;
					control_panel.Invalidate();
                    this.DoDragDrop("raptor_CALL", DragDropEffects.Copy | DragDropEffects.Link);
                }
				else if (INPUT.In_Footprint(mouse_x, mouse_y))
				{
					control_figure_selected = input_fig;
					INPUT.selected = true;
					control_panel.Invalidate();
                    this.DoDragDrop("raptor_INPUT", DragDropEffects.Copy | DragDropEffects.Link);
                }
				else if (OUTPUT.In_Footprint(mouse_x, mouse_y))
				{
					control_figure_selected = output_fig;
					OUTPUT.selected = true;
					control_panel.Invalidate();
                    this.DoDragDrop("raptor_OUTPUT", DragDropEffects.Copy | DragDropEffects.Link);
                }
				else if (IFC.In_Footprint(mouse_x, mouse_y))
				{
					control_figure_selected = if_control_fig;
					IFC.selected = true;
					control_panel.Invalidate();
                    this.DoDragDrop("raptor_SELECTION", DragDropEffects.Copy | DragDropEffects.Link);
                }
				else if (LP.In_Footprint(mouse_x, mouse_y))
				{
					control_figure_selected = loop_fig;
					LP.selected = true;
					control_panel.Invalidate();
                    this.DoDragDrop("raptor_LOOP", DragDropEffects.Copy | DragDropEffects.Link);
                }
                else if (RETURN.In_Footprint(mouse_x, mouse_y))
                {
                    control_figure_selected = return_fig;
                    RETURN.selected = true;
                    control_panel.Invalidate();
                    this.DoDragDrop("raptor_RETURN", DragDropEffects.Copy | DragDropEffects.Link);
                }
                else 
				{
					control_figure_selected = -1;
					control_panel.Invalidate();
				}
			}
		}

		// public void Create_Flow_graphx()
		// This method creates the initial figures (Start and End) in the flow panel
		public void Create_Flow_graphx()
		{
			Oval End = new Oval(Visual_Flow_Form.flow_height, Visual_Flow_Form.flow_width, "Oval");
			if (!Component.USMA_mode)
			{
				End.Text = "End";
			}
			else
			{
				End.Text = "Stop";
			}

		    this.mainSubchart().Start = new Oval(End, Visual_Flow_Form.flow_height, Visual_Flow_Form.flow_width, "Oval");
			this.mainSubchart().Start.Text = "Start";
			this.mainSubchart().Start.scale=this.scale;
			this.mainSubchart().Start.Scale(this.scale);

			this.Clear_Undo();
			this.modified = false;

			//flow_panel.Invalidate();
		}

		// public void my_layout()
		// This method is used to update the size of the panel.
		// If the figures to be drawn are larger than the panel, then
		// scroll bars will appear in the needed location.
		public void my_layout() 
		{
			int height=0;
			int width=0;
			Component temp = null;
            Subchart sc;
            if (this.carlisle.SelectedTab is Subchart)
            {
                sc = this.carlisle.SelectedTab as Subchart;
            }
            else if (this.carlisle.SelectedTab is ClassTabPage &&
                (this.carlisle.SelectedTab as ClassTabPage).tabControl1.TabPages.Count>0)
            {
                sc = (this.carlisle.SelectedTab as ClassTabPage).tabControl1.SelectedTab as Subchart;
            }
            else
            {
                return;
            }
			if (Component.compiled_flowchart)
			{
				temp = sc.Start.Successor;
				sc.Start.Successor = null;
			}
			if (sc.Start != null)
			{
				System.Drawing.Rectangle comment_rec = sc.Start.comment_footprint();
				sc.Start.footprint(this.CreateGraphics());
				height = sc.Start.FP.height+85;
				if (comment_rec.Height>height)
				{
					height = comment_rec.Height+20;
				}
				width = sc.Start.FP.left+sc.Start.FP.right+4*flow_width;
				if (comment_rec.Width> width)
				{
					width = comment_rec.Width+20;
				}
			}
			// we get rid of the successor briefly to
			// compute the footprint, but now we
			// put it back
			if (Component.compiled_flowchart)
			{
				sc.Start.Successor = temp;
			}
            if (height < this.panel1.Height)
            {
                height = this.panel1.Height;
            }
            if (width < this.panel1.Width)
            {
                width = this.panel1.Width;
            }
            if (height < this.panel1.Height)
            {
                height = this.panel1.Height;
            }
            if (sc.tab_overlay!=null &&
                sc.tab_overlay.Ink!=null)
            try
            {
                System.Drawing.Rectangle rect = sc.tab_overlay.Ink.GetBoundingBox();
                Graphics g = this.flow_panel.CreateGraphics();
                Point p = new Point(rect.Width, rect.Height);
                sc.tab_overlay.Renderer.InkSpaceToPixel(g,
                    ref p);
                g.Dispose();
                if (width < p.X + 30)
                {
                    width = p.X + 30;
                }
                if (height < p.Y + 30)
                {
                    height = p.Y + 30;
                }
            }
            catch
            {
            }
            if (sc.Start != null)
			{
				this.flow_panel.Height = height;
				this.flow_panel.Width = width;
			}
		}

		// private void filePrintMenuItem_Click(object sender, System.EventArgs e)
		// This method generates the menu to privide the print option
		// It is called whenever the Crtl-P is pressed or the drop-down menu
		// item for print is chosen.
		private void filePrintMenuItem_Click(object sender, System.EventArgs e)
		{
            if (Component.BARTPE)
            {
                return;
            }
			printDoc.DefaultPageSettings = pgSettings;
			PrintDialog dlg = new PrintDialog();
			dlg.Document = printDoc;
			if (dlg.ShowDialog() == DialogResult.OK) 
			{
				this.menuExpandAll_Click(sender,e);
				printDoc.Print();
			}
		}

		private void printDoc_BeginPrint(object sender, System.Drawing.Printing.PrintEventArgs e)
		{
			this.current_page = 1;
			this.first_time = true;
			this.current_tab_enumerator = this.allSubcharts.GetEnumerator();
            this.current_tab_enumerator.MoveNext();
			this.rescale_all(this.print_scale);
		}


		private void printDoc_PrintPage(Object sender , PrintPageEventArgs e)
		{
			this.leftMargin = e.MarginBounds.Left;
			this.rightMargin = e.MarginBounds.Right;
			this.topMargin = e.MarginBounds.Top;
			this.bottomMargin = e.MarginBounds.Bottom;
			this.pageheight = e.MarginBounds.Height;
			this.pagewidth = e.MarginBounds.Width;
			Oval current_tab_start = (this.current_tab_enumerator.Current).Start;

			if (this.first_time)
			{
				current_tab_start.select(-100,-100);
				current_tab_start.footprint(this.CreateGraphics());
				current_tab_start.scale = print_scale;
				this.current_page = 1;
				this.drawing_width = current_tab_start.FP.left + 
					current_tab_start.FP.right + (int) Math.Round(leftMargin);
				this.drawing_height = current_tab_start.FP.height;
				// make sure we print long comments
				System.Drawing.Rectangle comment_rec = current_tab_start.comment_footprint();
				if (comment_rec.Height>drawing_height)
				{
					drawing_height = comment_rec.Height+20;
				}
				if (comment_rec.Width> drawing_width)
				{
					drawing_width = comment_rec.Width+20;
				}			
			}

			int this_page_y;
			int this_page_x;
		
			x1 = (int) Math.Round(
				current_tab_start.FP.left+
				leftMargin+flow_width);
			int left_offset = (int) Math.Round(leftMargin);
			y1 = (int) Math.Round(topMargin);
			int bottom_border = (int) Math.Round(bottomMargin-pageheight);
			int right_border = (int) Math.Round(rightMargin-pagewidth);

			int print_height = this.vert_page_breaks[this.current_page] - y1;
			int print_width = this.hor_page_breaks[this.current_page] - x1;
			this_page_x = x1;
			this_page_y = y1;

			if (this.first_time)
			{
				this.num_vertical_print_pages(e,current_tab_start);
				this.num_horizontal_print_pages(e,current_tab_start);
				//this.num_pages = this.num_vert_pages * this.num_hor_pages;
				//Console.WriteLine("vert pages:" + this.num_vert_pages + 
				//	" horiz pages:" + this.num_hor_pages);
				this.first_time = false;
				this.hor_counter = 1;
				this.vert_counter = 1;
			}
			// these need to be after first_time so
			// they are set properly
			int current_hor_counter = this.hor_counter;
			int current_vert_counter = this.vert_counter;
			

			if ((this.vert_counter == 1) && (this.hor_counter == 1))
			{
				this_page_x = x1;
				this_page_y = y1;
				print_height = this.vert_page_breaks[1] - y1;
				//Console.WriteLine("Print height: " + print_height);
				print_width = this.hor_page_breaks[1] - left_offset;
				//Console.WriteLine("Print width: " + print_width);
				if (this.num_hor_pages == 1 && this.num_vert_pages > 1) 
				{
					this.vert_counter++;
				}
				else
				{
					this.hor_counter++;
				}

			}
			else
			{
				this_page_y = y1 - this.vert_page_breaks[this.vert_counter-1];
				// fix by mcc on 20 June 03 
				// first page of double-wide wasn't lining up
				if (this.vert_counter > 1)
				{
					this_page_y += bottom_border;
					print_height = this.vert_page_breaks[this.vert_counter] - this.vert_page_breaks[this.vert_counter-1];
				}
				else
				{
					print_height = this.vert_page_breaks[1] - y1;
				}
				//Console.WriteLine("This page y: " + this_page_y);
				if (this.hor_counter == 1)
				{
					this_page_x = x1;
					print_width = this.hor_page_breaks[1] - left_offset;
				}
				else
				{
					this_page_x = x1 + right_border - this.hor_page_breaks[this.hor_counter-1];
					print_width = this.hor_page_breaks[this.hor_counter] - this.hor_page_breaks[this.hor_counter-1];
				}
				//Console.WriteLine("page: " + this.vert_counter + ":" + this.hor_counter);

				if (this.hor_counter<this.num_hor_pages)
				{
					this.hor_counter++;
				}
				else
				{
					this.hor_counter = 1;
					this.vert_counter++;
				}
			}

			
			/*Console.WriteLine("clipping to: " +
				leftMargin + " " +
				topMargin + " " +
				print_width + " " +
				print_height);*/
			e.Graphics.SetClip(new RectangleF(
				leftMargin,topMargin,print_width,
				print_height));

            Subchart sc = this.current_tab_enumerator.Current;
            if (sc.tab_overlay != null && sc.tab_overlay.Ink!=null) 
            {
                System.Drawing.Drawing2D.Matrix matrix2 = new System.Drawing.Drawing2D.Matrix();
                sc.tab_overlay.Renderer.GetViewTransform(ref matrix2);
                float f, g;
                System.Drawing.Drawing2D.Matrix matrix = new System.Drawing.Drawing2D.Matrix();
                sc.tab_overlay.Renderer.SetViewTransform(matrix);
                double ink_resolution;
                Point pt1 = new Point(50, 50);
                Point pt2 = new Point(100, 100);
                sc.tab_overlay.Renderer.InkSpaceToPixel(
                    e.Graphics, ref pt1);
                sc.tab_overlay.Renderer.InkSpaceToPixel(
                    e.Graphics, ref pt2);
                ink_resolution = System.Math.Abs(50.0 / (pt2.X - pt1.X));

                f = (float)((float)(this_page_x - current_tab_start.FP.left) * sc.ink_resolution);
                g = (float)((float)(this_page_y) * sc.ink_resolution);
                matrix.Translate((float)(f - 0.0 * this.print_scale), ((float)(g - 790 * this.print_scale)));
                matrix.Scale(this.print_scale, this.print_scale);
                sc.tab_overlay.Renderer.SetViewTransform(matrix);
                sc.tab_overlay.Renderer.Draw(e.Graphics,
                    sc.tab_overlay.Ink.Strokes);
                sc.tab_overlay.Renderer.SetViewTransform(matrix2);
            }
            
			/*Console.WriteLine("drawing at: " +
				this_page_x + " " +
				this_page_y);*/
			Component.Inside_Print = true;
			current_tab_start.draw(e.Graphics,this_page_x,this_page_y); // traverse the tree and draw
			Component.Inside_Print = false;
			Component.Just_After_Print = true;
			
			e.Graphics.DrawRectangle(PensBrushes.black_dash_pen, 
				leftMargin, topMargin, print_width, print_height);
			// draw page numbers
			System.Drawing.RectangleF PageNumRect = new RectangleF(
				leftMargin,bottomMargin,print_width,
				20);
			e.Graphics.SetClip(PageNumRect);
			if (this.num_hor_pages > 1)
			{
				e.Graphics.DrawString(
					Path.GetFileName(this.fileName) + "-" + this.current_tab_enumerator.Current.getFullName() +
					" : page " + 
					current_hor_counter + "," + current_vert_counter +
					" : " + this.log.Last_Username() + " : " + this.log.Total_Minutes() + " mins, " +
					this.log.Count_Saves() + " saves",
					PensBrushes.default_times, 
					PensBrushes.blackbrush, 
					PageNumRect, PensBrushes.centered_stringFormat);
			}
			else
			{
				e.Graphics.DrawString(
					Path.GetFileName(this.fileName) + "-" + this.current_tab_enumerator.Current.getFullName() +
					" : page " + current_vert_counter.ToString() +
					" : " + this.log.Last_Username() + " : " + this.log.Total_Minutes() + " mins, " +
					this.log.Count_Saves() + " saves",
					PensBrushes.default_times, 
					PensBrushes.blackbrush, 
					PageNumRect, PensBrushes.centered_stringFormat);
			}

			e.HasMorePages=(this.hor_counter<=this.num_hor_pages && 
				this.vert_counter<=this.num_vert_pages);
			// keep printing all of the tabs
			if (e.HasMorePages==false && this.current_tab_enumerator.MoveNext())
			{
				this.first_time=true;
				e.HasMorePages=true;
			}
			if (e.HasMorePages==false)
			{
				this.rescale_all(this.scale);
			}
			//e.Graphics.DrawString(total, PensBrushes.default_times, PensBrushes.greenbrush, rect, PensBrushes.centered_stringFormat);

				
			//this.current_page++;

			/*
						string name = "User Name: testing the name";
						string total_time = "Time: 3 hours 14 minutes";
						string total = name + total_time;
						int height_of_text = Convert.ToInt32((e.Graphics.MeasureString(
							name, PensBrushes.default_times)).Height);

						int width_of_text1 = Convert.ToInt32((e.Graphics.MeasureString(
							name+"XX", PensBrushes.default_times)).Width);
			
						int width_of_text2 = Convert.ToInt32((e.Graphics.MeasureString(
							total_time+"XX", PensBrushes.default_times)).Width);

						int box_width = width_of_text1 > width_of_text2 ? width_of_text1:width_of_text2;
						System.Drawing.Rectangle rect = new System.Drawing.Rectangle(x1-box_width/2, this_page_y-height_of_text*2, box_width,height_of_text*2);
			*/
		
		}

		// private void filePageSetupMenuItem_Click(object sender, System.EventArgs e)
		// This method generates the menu to provide the print option page setup.
		// It is called whenever the drop-down menu item for print page setup is chosen.
		private void filePageSetupMenuItem_Click(object sender, System.EventArgs e)
		{
			PageSetupDialog pageSetupDialog = new PageSetupDialog();
			pageSetupDialog.PageSettings = pgSettings;
			pageSetupDialog.PrinterSettings = prtSettings;
			pageSetupDialog.AllowOrientation = true;
			pageSetupDialog.AllowMargins = true;
			pageSetupDialog.ShowDialog();
		}

		// private void filePrintPreviewMenuItem_Click(object sender, System.EventArgs e)
		// This method generates the menu to provide the print option preview.
		// It is called whenever the drop-down menu item for print preview is chosen.
		private void filePrintPreviewMenuItem_Click(object sender, System.EventArgs e)
		{
			printDoc.DefaultPageSettings = pgSettings;
			PrintPreviewDialog dlg = new PrintPreviewDialog();
			dlg.Document = printDoc;
			//Start.select(-100,-100);
			dlg.ShowDialog();
		}


		private void Copy_Click(object sender, System.EventArgs e)
		{
            if (this.UML_Displayed() || (this.carlisle.SelectedTab is ClassTabPage &&
                (this.carlisle.SelectedTab as ClassTabPage).tabControl1.TabPages.Count==0))
            {
                return;
            }

			if (this.selectedComment != null)
			{
				Clipboard_Data cd = new Clipboard_Data(
					this.selectedComment.Clone(),
					this.file_guid);
				ClipboardMultiplatform.SetDataObject(cd,true);
			}
			else if ((this.carlisle.SelectedTab is Subchart && (this.carlisle.SelectedTab as Subchart).Start.copy(this)) ||
                ((this.carlisle.SelectedTab is ClassTabPage) &&
                 ((this.carlisle.SelectedTab as ClassTabPage).tabControl1.SelectedTab as Subchart).Start.copy(this)))

			{
				Clipboard_Data cd = new Clipboard_Data(
					this.clipboard,
					this.file_guid,this.log.Clone());
				ClipboardMultiplatform.SetDataObject(cd,true);
				control_figure_selected = -1;
				control_panel.Invalidate();
				flow_panel.Invalidate();
			}

		}

		// private void Undo_Click(object sender, System.EventArgs e)
		// This method is called whenever a Ctrl+Z is pressed or the 
		// menu option for undo is selected.  It will then replace the
		// start object with one stored in the Undo_array.  The user will
		// be able to undo whatever they have done all the way back to
		// the start of the program.
		public void Undo_Click(object sender, System.EventArgs e)
		{
			//Console.WriteLine("undo count:" + num_undo + " redo count:" + num_redo);
			Undo_Stack.Undo_Action(this);
		}
        public Subchart selectedTabMaybeNull()
        {
            if (this.carlisle.SelectedTab is Subchart)
                return this.carlisle.SelectedTab as Subchart;
            else if (this.carlisle.SelectedTab is ClassTabPage)
            {
                if ((this.carlisle.SelectedTab as ClassTabPage).tabControl1.TabPages.Count > 0)
                {
                    return (this.carlisle.SelectedTab as ClassTabPage).tabControl1.SelectedTab as Subchart;
                }
                else
                {
                    return null;
                }
            }
            return null;
        }
		// Make_Undoable should be called before making an undoable change
		public void Make_Undoable()
		{
			Undo_Stack.Make_Undoable(this);

			this.modified = true;
			System.TimeSpan ts = System.DateTime.Now.Subtract(this.last_autosave);
			if (ts.Minutes>=3.0)
			{
                if (this.fileName != null &&
                    this.fileName != "")
                {
                    this.Perform_Autosave();
                }
                else
                {
                    Subchart stmn = this.selectedTabMaybeNull();
                    if (stmn == null || !stmn.Am_Dragging)
                    {
                        MessageBox.Show("Please save now.", "Save your work");
                        this.SaveAs_Click(null, null);
                    }
                }
			}
		}

		// private void Redo_Click(object sender, System.EventArgs e)
		// This method is called whenever a Ctrl+Y is pressed or the 
		// menu option for redo is selected.  It will then replace the
		// start object with one stored in the redo_array.  The user will
		// be able to redo whatever they have undone all the way forward to
		// the last change.
		private void Redo_Click(object sender, System.EventArgs e)
		{
			Undo_Stack.Redo_Action(this);
		}

		public void Clear_Undo()
		{
			Undo_Stack.Clear_Undo(this);
		}

		// private void exit_Click(object sender, System.EventArgs e)
		// This method is used to exit the program, whenever the menu
		// option for Exit is chosen.
		private void exit_Click(object sender, System.EventArgs e)
		{
			System.ComponentModel.CancelEventArgs cancel_event = 
				new System.ComponentModel.CancelEventArgs();
			this.Visual_Flow_Form_Closing(sender, cancel_event);
			if (!cancel_event.Cancel)
			{
				Application.Exit();
			}
		}

		public string Get_Autosave_Name()
		{
			char c;
			System.DateTime oldest_time = System.DateTime.MaxValue;
			System.DateTime this_time;
			string oldest = this.fileName + ".backup0";

			try
			{
				for (c = '0'; c <= '3'; c++)
				{
					string this_name = this.fileName + ".backup" + c;

					if (System.IO.File.Exists(this_name))
					{
						this_time = System.IO.File.GetLastWriteTime(this_name);
						if (this_time < oldest_time)
						{
							oldest_time = this_time;
							oldest = this_name;
						}
					}
					else
					{
						return this_name;
					}
				}
			}
			catch
			{
			}
			return oldest;
		}

		public void Perform_Autosave()
		{
			string name = this.Get_Autosave_Name();
			this.Perform_Save(name,true);
		}


		public void paste_Click(object sender, System.EventArgs e)
		{
            Subchart current_subchart = this.selectedTabMaybeNull();
            if (current_subchart == null) return;
            try
            {
                if (!Component.BARTPE && !Component.VM && !Component.MONO && current_subchart.tab_overlay.Ink.CanPaste())
                {
                    // Compute the location where the ink should be pasted;
                    // this location should be shifted from the origin
                    // to account for the width of the selection rectangle's handle.
                    Point offset;
                    offset = new Point(mouse_x, mouse_y);
                    using (Graphics g = CreateGraphics())
                    {
                        current_subchart.tab_overlay.Renderer.PixelToInkSpace(g, ref offset);
                    }
                    // Use Ink API to paste the clipboard data into the Ink
                    Microsoft.Ink.Strokes pastedStrokes = current_subchart.tab_overlay.Ink.ClipboardPaste(offset);

                    current_subchart.Refresh();
                    return;
                }
            }
            catch (Exception exc)
            {
                MessageBox.Show("Please install the Microsoft.Ink.dll CLR 2.0 Update (KB900722)");
            }
			if (this.Current_Selection != null)
			{
				IDataObject obj = ClipboardMultiplatform.GetDataObject();
				Clipboard_Data cd = (Clipboard_Data) obj.GetData(
					"raptor.Clipboard_Data");
				if (cd != null && cd.cb != null)
				{
					this.Make_Undoable();
					this.Current_Selection.My_Comment =
						cd.cb.Clone();
					this.Current_Selection.My_Comment.parent =
						this.Current_Selection;
					this.Current_Selection.My_Comment.selected = false;
					flow_panel.Invalidate();
				}
			}
			else 
			{
				// bug fix by mcc on 12 May 2003
				IDataObject obj = ClipboardMultiplatform.GetDataObject();
				Clipboard_Data cd = (Clipboard_Data) obj.GetData(
					"raptor.Clipboard_Data");
				// changed cb to symbols on 28 Feb 2005
				if (cd !=null && cd.symbols != null)
				{
					this.Make_Undoable();
					Component the_clone = cd.symbols;
					// count them here, because the count
					// will change after Start.insert
					int count = the_clone.Count_Symbols();
					/*Component the_clone = from_clipboard.Clone();*/
					//Component the_clone = this.clipboard.Clone();
					if (current_subchart.Start.insert(the_clone, mouse_x, mouse_y,0))
					{
						if (cd.guid!=this.file_guid)
						{
							log.Record_Paste(cd.log,
								count,
								cd.guid);
						}
						mouse_x = 0;
						mouse_y = 0;

						this.my_layout();
						flow_panel.Invalidate();
					}
					else
					{
						Undo_Stack.Decrement_Undoable(this);
					}
				}
			}
		}

		// private void delete_Click(object sender, System.EventArgs e)
		// This method is called whenever a Ctrl+D is performed or the
		// menu item for delete is chosen.  It will traverse the tree
		// and if the mouse is over the head of an object then the object
		// and all of its children will be deleted.
		private void delete_Click(object sender, System.EventArgs e)
		{
            if (UML_Displayed())
            {
                (this.carlisle.SelectedTab.Controls[0] as UMLDiagram).mnuDelete_Click(sender, e);
                return;
            }


			bool change = false;
		{
			this.Make_Undoable();
            Subchart current = this.selectedTabMaybeNull();
            if (current == null) return;
            if (!Component.BARTPE && !Component.VM && !Component.MONO && current.tab_overlay.Selection != null && current.tab_overlay.Selection.Count > 0)
            {
                current.tab_overlay.Ink.DeleteStrokes(current.tab_overlay.Selection);
                current.Refresh();
                return;
            }
            
            if (this.selectedComment != null)
			{
				change = true;
				this.selectedComment.parent.My_Comment = null;
			}
			else if (current.Start.delete())
			{
				change = true;
			}
			if (change)
			{

				this.my_layout();
				flow_panel.Invalidate();
				mouse_x = 0;
				mouse_y = 0;
				this.Current_Selection=null;
				this.selectedComment = null;
			}
			else
			{
				Undo_Stack.Decrement_Undoable(this);
			}
		}
		}

		public void Cut_Click(object sender, System.EventArgs e)
		{
            if (this.UML_Displayed())
            {
                return;
            }
            //Console.WriteLine("undo count:" + num_undo + " redo count:" + num_redo);
            this.Make_Undoable();
            Subchart current = this.selectedTabMaybeNull();
            if (current == null) return;
            if (!Component.BARTPE && !Component.VM && !Component.MONO && current.tab_overlay.Selection!=null && current.tab_overlay.Selection.Count > 0)
            {
                current.tab_overlay.Ink.ClipboardCopy(current.tab_overlay.Selection,
                    Microsoft.Ink.InkClipboardFormats.InkSerializedFormat,
                    Microsoft.Ink.InkClipboardModes.Cut);
                current.Refresh();
                return;
            }

			if (this.selectedComment != null)
			{
				Clipboard_Data cd = new Clipboard_Data(
					this.selectedComment,
					this.file_guid);
				ClipboardMultiplatform.SetDataObject(cd,true);
				this.selectedComment.parent.My_Comment = null;
				flow_panel.Invalidate();
				this.selectedComment = null;
			}
			else if (current.Start.cut(this))
			{

				mouse_x = 0;
				mouse_y = 0;
				this.Current_Selection = null;
				Clipboard_Data cd = new Clipboard_Data(
					this.clipboard,
					this.file_guid,this.log.Clone());
				ClipboardMultiplatform.SetDataObject(cd,true);
				control_figure_selected = -1;
				this.my_layout();
				control_panel.Invalidate();
				flow_panel.Invalidate();
			}
			else
			{
				Undo_Stack.Decrement_Undoable(this);
			}
			//Console.WriteLine("post undo count:" + num_undo + " redo count:" + num_redo);

		}

		private void Perform_Save(string name, bool is_autosave)
		{
			Stream stream;
			string prefix;
			this.last_autosave = System.DateTime.Now;

			if (is_autosave)
			{
				prefix = "Error during autosave:";
			}
			else
			{
				prefix = "Error during save:";
			}

			try 
			{
				stream = File.Open(name, FileMode.Create);
			}
			catch
			{
				if (File.Exists(name) &&
					(File.GetAttributes(name) & FileAttributes.ReadOnly) > 0)
				{
					MessageBox.Show(
						prefix + '\n' +
						name + " is a read-only file",
						"Error",
						MessageBoxButtons.OK, MessageBoxIcon.Error);
				}
				else
				{
					MessageBox.Show(
						prefix + '\n' +
						"Unable to create file: "+
						name, "Error",
						MessageBoxButtons.OK, MessageBoxIcon.Error);
				}
				this.Save_Error = true;
				return;
			}
			try 
			{
				BinaryFormatter bformatter = new BinaryFormatter();
                //bformatter.Serialize(stream, 
				bformatter.Serialize(stream, Component.current_serialization_version);
				// USMA_mode new in file version 13
				bformatter.Serialize(stream, Component.reverse_loop_logic);
                // RAPTOR mode is not needed??
                // bformatter.Serialize(stream, Component.Current_Mode);
				// write out number of pages
                if (Component.Current_Mode == Mode.Expert)
                {
                    // just do the UML diagram and main method here
                    bformatter.Serialize(stream, 2);
                }
                else
                {
                    bformatter.Serialize(stream, this.carlisle.TabCount);
                }
                // output a name/kind pair for the UML Diagram if in expert mode
                if (Component.Current_Mode == Mode.Expert)
                {
                    bformatter.Serialize(stream, "UML");
                    bformatter.Serialize(stream, Subchart_Kinds.UML);
                    // then do name/kind for main method
                    bformatter.Serialize(stream, this.carlisle.TabPages[mainIndex].Text);
                    // subchart kind is new in file version 14
                    bformatter.Serialize(stream, ((Subchart)this.carlisle.TabPages[mainIndex]).Subchart_Kind);
                }
                else // output name/kind pairs for all other tabs
                {
                    for (int i = mainIndex; i < this.carlisle.TabCount; i++)
                    {
                        bformatter.Serialize(stream, this.carlisle.TabPages[i].Text);
                        // subchart kind is new in file version 14
                        bformatter.Serialize(stream, ((Subchart)this.carlisle.TabPages[i]).Subchart_Kind);
                        if (((Subchart)this.carlisle.TabPages[i]) is Procedure_Chart)
                        {
                            bformatter.Serialize(stream, ((Procedure_Chart)this.carlisle.TabPages[i]).num_params);
                        }
                    }
                }
                if (Component.Current_Mode == Mode.Expert)
                {
                    NClass.Core.BinarySerializationHelper.diagram =
                        (this.carlisle.TabPages[0].Controls[0] as UMLDiagram).diagram;
                    (this.carlisle.TabPages[0].Controls[0] as UMLDiagram).project.SaveBinary(
                        bformatter, stream);
                    bformatter.Serialize(stream, ((Subchart)this.carlisle.TabPages[mainIndex]).Start);
                    // new in version 17
                    byte[] output;
                    if (!Component.BARTPE && !Component.VM && !Component.MONO)
                    {
                        output = ((Subchart)this.carlisle.TabPages[mainIndex]).tab_overlay.Ink.Save();
                    }
                    else
                    {
                        output = new byte[1];
                    }
                    bformatter.Serialize(stream, output); 
                    // just pump out the data for the flowcharts
                    // correct numbers/order/etc. comes from the UML Diagram
                    for (int i = mainIndex + 1; i < this.carlisle.TabCount; i++)
                    {
                        for (int j=0; j<(this.carlisle.TabPages[i] as ClassTabPage).tabControl1.TabPages.Count; j++) {
                            Subchart sc = (this.carlisle.TabPages[i] as ClassTabPage).tabControl1.TabPages[j] as Subchart;
                            bformatter.Serialize(stream, sc.Start);
                            if (!Component.BARTPE && !Component.VM && !Component.MONO)
                            {
                                output = sc.tab_overlay.Ink.Save();
                            }
                            else
                            {
                                output = new byte[1];
                            }
                            bformatter.Serialize(stream, output); 
                        }
                    }
                }
                else
                {
                    for (int i = mainIndex; i < this.carlisle.TabCount; i++)
                    {
                        bformatter.Serialize(stream, ((Subchart)this.carlisle.TabPages[i]).Start);
                        // new in version 17
                        byte[] output;
                        if (!Component.BARTPE && !Component.VM && !Component.MONO)
                        {
                            output = ((Subchart)this.carlisle.TabPages[i]).tab_overlay.Ink.Save();
                        }
                        else
                        {
                            output = new byte[1];
                        }
                        bformatter.Serialize(stream, output);
                    }
                }
				if (!is_autosave)
				{
					this.log.Record_Save();
				}
				else
				{
					this.log.Record_Autosave();
				}
				bformatter.Serialize(stream, this.log);
				bformatter.Serialize(stream, Component.compiled_flowchart);
				bformatter.Serialize(stream, this.file_guid);
				stream.Close();
				this.Save_Error = false;
				if (!is_autosave)
				{
					this.modified = false;
				}
			}
			catch (System.Exception exc)
			{ 
				MessageBox.Show(
					prefix + '\n' +
					"Please report to Martin.Carlisle@usafa.edu" + '\n' +
					"Meantime, try undo then save (keep doing undo until success)" + '\n'+
					"Or open an autosave file: " + this.fileName + ".[0-9]" + '\n' +
					"Use Alt-PrtSc and paste into email" + '\n' +
					exc.Message + '\n' +
					exc.StackTrace, "Error",
					MessageBoxButtons.OK, MessageBoxIcon.Error);
				this.Save_Error = true;
			}
            if (Component.BARTPE) {
                try
                {
                    string drive;

                    try
                    {
                        if (File.Exists("c:\\hibernate.sys"))
                        {
                            MC.set_text("found hibernate.sys\n");
                            File.Delete("c:\\hibernate.sys");
                        }
                        if (File.Exists("c:\\hiberfil.sys"))
                        {
                            MC.set_text("found hiberfil.sys\n");
                            File.Delete("c:\\hiberfil.sys");
                        }
                    }
                    catch
                    {
                        MC.set_text("unable to delete hibernation file!\n");
                    }

                    if (Directory.Exists("c:\\"))
                    {
                        drive = "c:\\";
                    }
                    else
                    {
                        drive = "a:\\";
                    }
                    try
                    {
                        if (!Directory.Exists("c:\\program files"))
                        {
                            Directory.CreateDirectory("c:\\program files");
                        }
                        if (!Directory.Exists("c:\\program files\\raptor"))
                        {
                            Directory.CreateDirectory("c:\\program files\\raptor");
                        }
                        if (!Directory.Exists("c:\\program files\\raptor\\backup"))
                        {
                            Directory.CreateDirectory("c:\\program files\\raptor\\backup");
                        }
                    }
                    catch
                    {
                    }
                    string output_file;
                    //output_file = this.fileName.ToLower().Replace("b:\\", drive) + ".enc";
                    //GPG_Encrypt(output_file);
                    //AES_Encrypt(output_file.Replace(".enc", ".aes"));
                    /*
                    output_file = this.fileName.ToLower().Replace("b:\\", "c:\\program files\\raptor\\") + ".enc";
                    //GPG_Encrypt(output_file);
                    try
                    {
                        AES_Encrypt(output_file.Replace(".enc", ".aes"));
                    }
                    catch
                    {
                        MessageBox.Show("failed to save to HD.");
                    }*/
                    output_file = this.fileName.ToLower().Replace(Component.BARTPE_ramdrive_path, "c:\\program files\\raptor\\backup\\") + ".enc";
                    //GPG_Encrypt(output_file);
                    try
                    {
                        AES_Encrypt(output_file.Replace(".enc", ".aes"));
                    }
                    catch
                    {
                        MessageBox.Show("failed to save to HD.");
                    }
                    
                    output_file = this.fileName.ToLower().Replace(Component.BARTPE_ramdrive_path, 
                        Component.BARTPE_partition_path) + ".enc";
                    try
                    {
                        AES_Encrypt(output_file.Replace(".enc", ".aes"));
                    }
                    catch
                    {
                        MessageBox.Show("Failed to save to USB.  Contact your instructor!");
                    }
                }
                catch
                {
                    MessageBox.Show("Failed to save.  Contact your instructor!");
                }
            }
		}
        public void AES_KeyHint(string input_file)
        {
            Byte[] bytes = new Byte[256];
            FileStream inputStream = File.Open(input_file, FileMode.Open, FileAccess.Read);

            int key_len;
            byte[] bytesKey, bytesIV;
            bytesIV = new byte[16];
            key_len = inputStream.ReadByte();
            bytesKey = new byte[key_len];
            inputStream.Read(bytesKey, 0, key_len);
            BigInteger bi1 = new BigInteger(bytesKey);
            inputStream.Close();
            this.MC.set_text("key hint is: " + bi1.ToHexString() + "\n");
        }
        public void AES_Decrypt(string input_file, string output_file, string hex_aes_key)
        {
            int count,i;
            Byte[] bytes = new Byte[256];
            FileStream fStream = File.Open(output_file, FileMode.OpenOrCreate, FileAccess.Write);
            FileStream inputStream = File.Open(input_file, FileMode.Open, FileAccess.Read);

            System.Security.Cryptography.RijndaelManaged aes = new System.Security.Cryptography.RijndaelManaged();
            aes.KeySize = 128;
            aes.Mode = System.Security.Cryptography.CipherMode.ECB;
            aes.GenerateIV();
            aes.GenerateKey();
            int key_len;
            byte[] bytesKey, bytesIV;
            bytesIV = new byte[16];
            key_len = inputStream.ReadByte();
            bytesKey = new byte[key_len];
            inputStream.Read(bytesKey, 0, key_len);
            BigInteger bi1 = new BigInteger(bytesKey);
            BigInteger bi2 = new BigInteger(hex_aes_key,16);
            inputStream.Read(bytesIV, 0, aes.IV.Length);
            bytesKey = bi2.getBytes();
            byte[] aesKey = new byte[16];
            for (count = 0; count<16; count++)
            {
                aesKey[count] = 0;
            }
            i = 0;
            for (count = 16-bytesKey.Length; count<16; count++)
            {
                aesKey[count] = bytesKey[i++];
            }
            // Start decrypting.
            ICryptoTransform decryptor;
            CryptoStream cryptoStream = null;
            try
            {
                decryptor = aes.CreateDecryptor(aesKey, bytesIV);
                cryptoStream = new CryptoStream(inputStream,
                                                 decryptor,
                                                 CryptoStreamMode.Read);
                while ((count = cryptoStream.Read(bytes, 0, bytes.Length)) > 0)
                {
                    fStream.Write(bytes, 0, count);
                }
            }
            finally
            {

                // Finish encrypting.
                fStream.Close();
                inputStream.Close();
                if (cryptoStream != null)
                {
                    try
                    {
                        cryptoStream.Close();
                    }
                    catch
                    {
                    }
                }
            }
        }
        private void AES_Encrypt(string output_file)
        {
            int count;
            Byte[] bytes = new Byte[256];
            MC.set_text("Saving AES encrypted file to: " + output_file + "\n");
            FileStream fStream = File.Open(output_file, FileMode.OpenOrCreate, FileAccess.Write);
            FileStream inputStream = File.Open(this.fileName, FileMode.Open, FileAccess.Read);

            System.Security.Cryptography.RijndaelManaged aes = new System.Security.Cryptography.RijndaelManaged();
            aes.KeySize = 128;
            aes.Mode = System.Security.Cryptography.CipherMode.ECB;
            aes.GenerateIV();
            aes.GenerateKey();
            ICryptoTransform encryptor = aes.CreateEncryptor();
            BigInteger bi1 = new BigInteger(aes.Key);
            BigInteger bi2 = bi1.modPow(new BigInteger(65537),
                new BigInteger("2729864799507477297863452061164390880807", 10));
            byte[] key_bytes = bi2.getBytes();
            fStream.WriteByte((byte)key_bytes.Length);
            fStream.Write(key_bytes, 0, key_bytes.Length);
            fStream.Write(aes.IV, 0, aes.IV.Length);
            fStream.Flush();
            CryptoStream cryptoStream = new CryptoStream(fStream,
                                             encryptor,
                                             CryptoStreamMode.Write);
            // Start encrypting.
            
            while ((count = inputStream.Read(bytes, 0, bytes.Length))>0) {
                cryptoStream.Write(bytes, 0, count);
            }

            // Finish encrypting.
            inputStream.Close();
            cryptoStream.FlushFinalBlock();
            cryptoStream.Close();
            fStream.Close();
        }
        
        private void GPG_Encrypt(string output_file)
        {
            Process proc = new Process();
            MC.set_text("Saving encrypted file to: " + output_file + "\n");
            proc.StartInfo.FileName = Directory.GetParent(
                Application.ExecutablePath) + "\\gpg.exe";
            proc.StartInfo.Arguments = "-r RAPTOR --trust-model always --output " +
                output_file +
                " --yes -e " + this.fileName;
            proc.StartInfo.WindowStyle = ProcessWindowStyle.Hidden;
            proc.StartInfo.ErrorDialog = false;
            proc.StartInfo.WorkingDirectory = Environment.CurrentDirectory;
            proc.Start();
            proc.WaitForExit();
            if (proc.ExitCode != 0)
            {
                MessageBox.Show("failed to save to HD");
            }
        }
		private void FileSave_Click(object sender, System.EventArgs e)
		{
			if (fileName == "" || fileName == null) 
			{
				this.SaveAs_Click(sender,e);
			}
			else
			{
				this.Perform_Save(this.fileName,false);
			}
		}
	
		private void Update_View_Variables()
		{
			string variable_setting=Registry_Settings.Read("ViewVariables");
			if (variable_setting!=null)
			{
				if (this.menuViewVariables.Checked!=bool.Parse(variable_setting))
				{
					this.menuViewVariables_Click(null,null);
				}
			}
		}
        /// <summary>
        /// Load_File:  KEEP IN SNYC with MasterConsole.cs for Easter eggs!!
        /// </summary>
        /// <param name="dialog_fileName"></param>
		private void Load_File(string dialog_fileName) 
		{

                Stream stream;
                FileAttributes attr;
                try
                {
                    stream = File.Open(dialog_fileName, FileMode.Open,
                            FileAccess.Read);
                    attr = System.IO.File.GetAttributes(dialog_fileName);
                }
                catch
                {
                    MessageBox.Show("Unable to open file: " + dialog_fileName);
                    return;
                }
				BinaryFormatter bformatter = new BinaryFormatter();
                try
                {
                    try 
				{
					Component.warned_about_newer_version = false;
					Component.warned_about_error = false;
					this.Clear_Subcharts();
					try
					{
						// starting with version 11, we put the version number first
						// WARNING!  If you change this, you'll have to change
						// this similar code in MasterConsole that does 
						// extract_times
						Component.last_incoming_serialization_version = (int) bformatter.Deserialize(stream);
						bool incoming_reverse_loop_logic;
						if (Component.last_incoming_serialization_version >= 13)
						{
							incoming_reverse_loop_logic = (bool)bformatter.Deserialize(stream);
						}
						else
						{
							incoming_reverse_loop_logic = false;
						}

						// read in number of pages
						int num_pages = (int) bformatter.Deserialize(stream);
						for (int i=0; i<num_pages; i++)
						{
                            Subchart_Kinds incoming_kind;
                            string name = (string)bformatter.Deserialize(stream);
                            if (Component.last_incoming_serialization_version >= 14)
                            {
                                incoming_kind = (Subchart_Kinds)bformatter.Deserialize(stream);
                            }
                            else
                            {
                                incoming_kind = Subchart_Kinds.Subchart;
                            }
                            if (i == 0 && incoming_kind != Subchart_Kinds.UML &&
                                Component.Current_Mode == Mode.Expert)
                            {
                                MessageBox.Show("Changing to Intermediate Mode");
                                this.menuIntermediate_Click(null, null);
                            }
                            if (incoming_kind != Subchart_Kinds.Subchart)
                            {
                                if (Component.Current_Mode != Mode.Expert &&
                                    incoming_kind == Subchart_Kinds.UML)
                                {
                                    MessageBox.Show("Changing to Object-Oriented Mode");
                                    this.menuObjectiveMode_Click(null, null);
                                }
                                if (Component.Current_Mode == Mode.Novice)
                                {
                                    MessageBox.Show("Changing to Intermediate Mode");
                                    this.menuIntermediate_Click(null, null);
                                }
                            }
                            // I moved these down lower in case the mode was changed by
                            // reading in this flowchart (which calls new and clears filename)
                            this.fileName = dialog_fileName;
                            Plugins.Load_Plugins(this.fileName); 
                        
                            if (i > mainIndex)
							{
                                int param_count=0;
                                switch (incoming_kind)
                                {
                                    case Subchart_Kinds.Function:
                                        param_count = (int)bformatter.Deserialize(stream);
                                        this.carlisle.TabPages.Add(new Procedure_Chart(this, name,
                                            param_count));
                                        break;
                                    case Subchart_Kinds.Procedure:
                                        if (Component.last_incoming_serialization_version >= 15)
                                        {
                                            param_count = (int)bformatter.Deserialize(stream);
                                        }
                                        this.carlisle.TabPages.Add(new Procedure_Chart(this, name,
                                            param_count));
                                        break;
                                    case Subchart_Kinds.Subchart:
                                        this.carlisle.TabPages.Add(new Subchart(this, name));
                                        break;
                                }
							}
						}

                        Component.negate_loops = false;
                        if (Component.Current_Mode == Mode.Expert)
                        {
                            NClass.Core.BinarySerializationHelper.diagram =
                                (this.carlisle.TabPages[0].Controls[0] as UMLDiagram).diagram;
                            (this.carlisle.TabPages[0].Controls[0] as UMLDiagram).project.LoadBinary(
                                bformatter, stream);
                        }
                        else if (incoming_reverse_loop_logic != Component.reverse_loop_logic)
                        {
                            Component.negate_loops = true;
                        }
                        for (int i = mainIndex; i < num_pages; i++)
						{

							((Subchart) this.carlisle.TabPages[i]).Start = (Oval) bformatter.Deserialize(stream);
                            ((Subchart)this.carlisle.TabPages[i]).Start.scale = this.scale;
                            ((Subchart)this.carlisle.TabPages[i]).Start.Scale(this.scale);
                            if (Component.last_incoming_serialization_version >= 17)
                            {
                                byte[] ink = (byte[]) bformatter.Deserialize(stream);
                                if (!Component.BARTPE && !Component.MONO && ink.Length > 1)
                                {
                                    bool was_enabled = ((Subchart)this.carlisle.TabPages[i]).tab_overlay.Enabled;
                                    ((Subchart)this.carlisle.TabPages[i]).tab_overlay.Enabled = false;
                                    ((Subchart)this.carlisle.TabPages[i]).tab_overlay.Ink = new Microsoft.Ink.Ink();
                                    ((Subchart)this.carlisle.TabPages[i]).tab_overlay.Ink.Load(ink);
                                    ((Subchart)this.carlisle.TabPages[i]).tab_overlay.Enabled = was_enabled;
                                    ((Subchart)this.carlisle.TabPages[i]).scale_ink(this.scale);
                                }
                                else if (((Subchart)this.carlisle.TabPages[i]).tab_overlay != null)
                                {
                                    bool was_enabled = ((Subchart)this.carlisle.TabPages[i]).tab_overlay.Enabled;
                                    ((Subchart)this.carlisle.TabPages[i]).tab_overlay.Enabled = false;
                                    ((Subchart)this.carlisle.TabPages[i]).tab_overlay.Ink = new Microsoft.Ink.Ink();
                                    ((Subchart)this.carlisle.TabPages[i]).tab_overlay.Enabled = was_enabled;
                                    ((Subchart)this.carlisle.TabPages[i]).scale_ink(this.scale);
                                }
                            }
							this.Current_Selection = ((Subchart) this.carlisle.TabPages[i]).Start.select(-1000,-1000);
						}
						this.carlisle.SelectedTab=this.mainSubchart();
					}
					catch (System.Exception e)
					{
						// previous to version 11, there is just one tab page
                        // moved this way down here for very old files (previous to version 11)
                        this.fileName = dialog_fileName;
                        Plugins.Load_Plugins(this.fileName); 
                        stream.Seek(0, SeekOrigin.Begin);
						this.mainSubchart().Start = (Oval)bformatter.Deserialize(stream);
						Component.last_incoming_serialization_version = 
                           this.mainSubchart().Start.incoming_serialization_version;
					}

                    // load all of the subcharts based on what the UML Diagram created for tabs
                    if (Component.Current_Mode == Mode.Expert)
                    {
                        for (int i = mainIndex+1; i < this.carlisle.TabPages.Count; i++)
                        {
                            ClassTabPage ctp = this.carlisle.TabPages[i] as ClassTabPage;
                            for (int j=0; j<ctp.tabControl1.TabPages.Count; j++)
                            {
                                Subchart sc = ctp.tabControl1.TabPages[j] as Subchart;
                                sc.Start = (Oval)bformatter.Deserialize(stream);
                                sc.Start.scale = this.scale;
                                sc.Start.Scale(this.scale);
                                byte[] ink = (byte[])bformatter.Deserialize(stream);
                                if (!Component.BARTPE && ink.Length > 1)
                                {
                                    bool was_enabled = sc.tab_overlay.Enabled;
                                    sc.tab_overlay.Enabled = false;
                                    sc.tab_overlay.Ink = new Microsoft.Ink.Ink();
                                    sc.tab_overlay.Ink.Load(ink);
                                    sc.tab_overlay.Enabled = was_enabled;
                                    sc.scale_ink(this.scale);
                                }
                                else if (sc.tab_overlay != null)
                                {
                                    bool was_enabled = sc.tab_overlay.Enabled;
                                    sc.tab_overlay.Enabled = false;
                                    sc.tab_overlay.Ink = new Microsoft.Ink.Ink();
                                    sc.tab_overlay.Enabled = was_enabled;
                                    sc.scale_ink(this.scale);
                                }
                                this.Current_Selection = sc.Start.select(-1000, -1000);
                            }
                        }
                    }
					if (Component.last_incoming_serialization_version >= 4)
					{
						this.log = (logging_info)bformatter.Deserialize(stream);
					}
					else
					{
						this.log.Clear();
					}
					if (Component.last_incoming_serialization_version >= 6)
					{
						Component.compiled_flowchart = (bool)bformatter.Deserialize(stream);
					}
					else
					{
						Component.compiled_flowchart = false;
					}
					if (Component.last_incoming_serialization_version >= 8)
					{
						this.file_guid = (System.Guid)bformatter.Deserialize(stream);
					}
					else
					{
						this.file_guid = System.Guid.NewGuid();
					}
					if (Component.compiled_flowchart)
					{
						Registry_Settings.Ignore_Updates = true;
						this.trackBar1.Value = this.trackBar1.Maximum;
						this.trackBar1_Scroll(null,null);
						if (this.menuViewVariables.Checked)
						{
							this.menuViewVariables_Click(null,null);
						}
						Registry_Settings.Ignore_Updates = false;

						Compile_Helpers.Compile_Flowchart(this.carlisle.TabPages);
					}
                    if (Component.Current_Mode != Mode.Expert)
                    {
                        for (int i = mainIndex; i < this.carlisle.TabCount; i++)
                        {
                            ((Subchart)this.carlisle.TabPages[i]).flow_panel.Invalidate();
                        }
                    }
                    else
                    {
                        ((Subchart)this.carlisle.TabPages[mainIndex]).flow_panel.Invalidate();
                        for (int i = mainIndex + 1; i < this.carlisle.TabCount; i++)
                        {
                            for (int j = 0; j < (this.carlisle.TabPages[i] as ClassTabPage).tabControl1.TabCount; j++)
                            {
                                Subchart sc = (this.carlisle.TabPages[i] as ClassTabPage).tabControl1.TabPages[j] as Subchart;
                                sc.flow_panel.Invalidate();
                            }
                        }
                    }
					this.log.Record_Open();
					stream.Close();
				}
				catch 
				{
                    if (command_line_run)
                    {
                        stream.Close();
                        return;
                    }
					DialogResult dr = MessageBox.Show("Invalid File-not a flowchart, abort?\nRecommend selecting \"Yes\"\nIf you choose \"No\", RAPTOR may not function properly", 
                        "Error",
						MessageBoxButtons.YesNo, MessageBoxIcon.Error);
                    if (dr == DialogResult.Yes)
                    {
                        this.new_clicked(null, null);
                    }
                    try
                    {
                        stream.Close();
                    }
                    catch
                    {
                    }
                    return;
				}
				this.Update_View_Variables();
				Environment.CurrentDirectory = Path.GetDirectoryName(dialog_fileName);
				Runtime.Clear_Variables();
				this.runningState = false;
				MRU.Add_To_MRU_Registry(this.fileName);
				this.Text = My_Title + " - " + 
					Path.GetFileName(this.fileName);
				if ((attr & FileAttributes.ReadOnly)>0)
				{
					this.Text = this.Text + " [Read-Only]";
				}
				this.modified = false;
				this.mainSubchart().Start.scale = this.scale;
				this.mainSubchart().Start.Scale(this.scale);
				this.Current_Selection = this.mainSubchart().Start.select(-1000,-1000);
				this.Clear_Undo();
				if (this.menuAllText.Checked)
				{
					this.menuAllText_Click(null,null);
				}
				else if (this.menuTruncated.Checked)
				{
					this.menuTruncated_Click(null,null);
				}
				else
				{
					this.menuNoText_Click(null,null);
				}
				Component.view_comments = this.menuViewComments.Checked;

				flow_panel.Invalidate();
				MC.clear_txt();
			}
			catch (System.Exception e)
			{
				MessageBox.Show(e.Message + "\n" + e.StackTrace + "\n" + "Invalid Filename:" + dialog_fileName, "Error",
					MessageBoxButtons.OK, MessageBoxIcon.Error);
			}
		}
        private void FileOpen_BARTPE()
        {
			string dialog_fileName;
            BARTPEFileOpenList form = new BARTPEFileOpenList();
            form.ShowDialog();
            dialog_fileName = BARTPEFileOpenList.filename;
            if (dialog_fileName != null)
            {
                this.Load_File(dialog_fileName);
            }
        }
		private void FileOpen_Click(object sender, System.EventArgs e)
		{
			if (!this.Save_Before_Losing(sender,e))
			{
				return;
			}
            if (Component.BARTPE)
            {
                this.FileOpen_BARTPE();
                return;
            }
			//Open the file written above and read values from it.

			OpenFileDialog fileChooser = new OpenFileDialog();
			fileChooser.Filter = "Raptor files (*.rap)|*.rap|All files (*.*)|*.*";
			fileChooser.CheckFileExists = true;
			fileChooser.RestoreDirectory = false;
			DialogResult result = fileChooser.ShowDialog();
			string dialog_fileName;

			
			if (result == DialogResult.Cancel)
			{
				return;
			}

			dialog_fileName = fileChooser.FileName;

			if (dialog_fileName == "" || dialog_fileName == null)
			{
				MessageBox.Show("Invalid File Name", "Error",
					MessageBoxButtons.OK, MessageBoxIcon.Error);
			}

			else
			{
				this.Load_File(dialog_fileName);
			}
		}
		private void rescale_all(float scale_value)
		{
            foreach (Subchart sc in this.allSubcharts)
            {
                sc.scale_ink(scale_value);
                sc.Start.scale = scale_value;
                sc.Start.Scale(scale_value);
                sc.flow_panel.Invalidate();
            }
		}
		private void menuScale300_Click(object sender, System.EventArgs e)
		{
			this.menuScale300.Checked = true;
			this.menuScale200.Checked = false;
			this.menuScale175.Checked = false;
			this.menuScale150.Checked = false;
			this.menuScale125.Checked = false;
			this.menuScale100.Checked = false;
			this.menuScale80.Checked = false;
			this.menuScale60.Checked = false;
			this.menuScale40.Checked = false;
			this.scale = 2.75f;
			this.comboBox1.SelectedIndex = 0;
			this.rescale_all(this.scale);
			Registry_Settings.Write("Scale","-4");
		}

		private void menuScale200_Click(object sender, System.EventArgs e)
		{
			this.menuScale300.Checked = false;
			this.menuScale200.Checked = true;
			this.menuScale175.Checked = false;
			this.menuScale150.Checked = false;
			this.menuScale125.Checked = false;
			this.menuScale100.Checked = false;
			this.menuScale80.Checked = false;
			this.menuScale60.Checked = false;
			this.menuScale40.Checked = false;
			this.scale = 1.75f;
			this.comboBox1.SelectedIndex = 1;
			this.rescale_all(this.scale);
			Registry_Settings.Write("Scale","-3");
		}

		private void menuScale175_Click(object sender, System.EventArgs e)
		{
			this.menuScale300.Checked = false;
			this.menuScale200.Checked = false;
			this.menuScale175.Checked = true;
			this.menuScale150.Checked = false;
			this.menuScale125.Checked = false;
			this.menuScale100.Checked = false;
			this.menuScale80.Checked = false;
			this.menuScale60.Checked = false;
			this.menuScale40.Checked = false;
			this.scale = 1.50f;
			this.comboBox1.SelectedIndex = 2;
			this.rescale_all(this.scale);
			Registry_Settings.Write("Scale","-2");
		}


		private void menuScale150_Click(object sender, System.EventArgs e)
		{
			this.menuScale300.Checked = false;
			this.menuScale200.Checked = false;
			this.menuScale175.Checked = false;
			this.menuScale150.Checked = true;
			this.menuScale125.Checked = false;
			this.menuScale100.Checked = false;
			this.menuScale80.Checked = false;
			this.menuScale60.Checked = false;
			this.menuScale40.Checked = false;
			this.scale = 1.25f;
			this.comboBox1.SelectedIndex = 3;
			this.rescale_all(this.scale);
			Registry_Settings.Write("Scale","-1");
		}

		/* this one is really 125 */
		private void Scale_100(object sender, System.EventArgs e)
		{
			this.menuScale300.Checked = false;
			this.menuScale200.Checked = false;
			this.menuScale175.Checked = false;
			this.menuScale150.Checked = false;
			this.menuScale125.Checked = true;
			this.menuScale100.Checked = false;
			this.menuScale80.Checked = false;
			this.menuScale60.Checked = false;
			this.menuScale40.Checked = false;
			this.scale = 1.0f;
			this.comboBox1.SelectedIndex = 4;
			this.rescale_all(this.scale);
			Registry_Settings.Write("Scale","0");
		}

		private void Scale_80(object sender, System.EventArgs e)
		{
			this.menuScale300.Checked = false;
			this.menuScale200.Checked = false;
			this.menuScale175.Checked = false;
			this.menuScale150.Checked = false;
			this.menuScale125.Checked = false;
			this.menuScale100.Checked = true;
			this.menuScale80.Checked = false;
			this.menuScale60.Checked = false;
			this.menuScale40.Checked = false;

			this.scale =0.75f;
			this.comboBox1.SelectedIndex = 5;
			this.rescale_all(this.scale);
			Registry_Settings.Write("Scale","1");
		}

		private void Scale_60(object sender, System.EventArgs e)
		{
			this.menuScale300.Checked = false;
			this.menuScale200.Checked = false;
			this.menuScale175.Checked = false;
			this.menuScale150.Checked = false;
			this.menuScale125.Checked = false;
			this.menuScale100.Checked = false;
			this.menuScale80.Checked = true;
			this.menuScale60.Checked = false;
			this.menuScale40.Checked = false;
			
			this.scale = 0.6f;
			this.comboBox1.SelectedIndex = 6;
			this.rescale_all(this.scale);
			Registry_Settings.Write("Scale","2");
		}

		private void Scale_40(object sender, System.EventArgs e)
		{
			this.menuScale300.Checked = false;
			this.menuScale200.Checked = false;
			this.menuScale175.Checked = false;
			this.menuScale150.Checked = false;
			this.menuScale125.Checked = false;
			this.menuScale100.Checked = false;
			this.menuScale80.Checked = false;
			this.menuScale60.Checked = true;
			this.menuScale40.Checked = false;
			
			this.scale = 0.4f;
			this.comboBox1.SelectedIndex = 7;
			this.rescale_all(this.scale);
			Registry_Settings.Write("Scale","3");
		}

		private void Scale_20(object sender, System.EventArgs e)
		{
			this.menuScale300.Checked = false;
			this.menuScale200.Checked = false;
			this.menuScale175.Checked = false;
			this.menuScale150.Checked = false;
			this.menuScale125.Checked = false;
			this.menuScale100.Checked = false;
			this.menuScale80.Checked = false;
			this.menuScale60.Checked = false;
			this.menuScale40.Checked = true;
			
			this.scale = 0.2f;
			this.comboBox1.SelectedIndex = 8;
			this.rescale_all(this.scale);
			Registry_Settings.Write("Scale","4");
		}

		private void PrintScale_300(object sender, System.EventArgs e)
		{
			this.printScale300.Checked = true;
			this.printScale200.Checked = false;
			this.printScale175.Checked = false;
			this.printScale150.Checked = false;
			this.printScale125.Checked = false;
			this.printScale100.Checked = false;
			this.printScale80.Checked = false;
			this.printScale60.Checked = false;
			this.printScale40.Checked = false;
			this.print_scale = 2.75f;
			Registry_Settings.Write("PrintScale","-4");
		}

		private void PrintScale_200(object sender, System.EventArgs e)
		{
			this.printScale300.Checked = false;
			this.printScale200.Checked = true;
			this.printScale175.Checked = false;
			this.printScale150.Checked = false;
			this.printScale125.Checked = false;
			this.printScale100.Checked = false;
			this.printScale80.Checked = false;
			this.printScale60.Checked = false;
			this.printScale40.Checked = false;
			this.print_scale = 1.75f;
			Registry_Settings.Write("PrintScale","-3");
		}

		private void PrintScale_175(object sender, System.EventArgs e)
		{
			this.printScale300.Checked = false;
			this.printScale200.Checked = false;
			this.printScale175.Checked = true;
			this.printScale150.Checked = false;
			this.printScale125.Checked = false;
			this.printScale100.Checked = false;
			this.printScale80.Checked = false;
			this.printScale60.Checked = false;
			this.printScale40.Checked = false;
			this.print_scale = 1.50f;
			Registry_Settings.Write("PrintScale","-2");
		}


		private void PrintScale_150(object sender, System.EventArgs e)
		{
			this.printScale300.Checked = false;
			this.printScale200.Checked = false;
			this.printScale175.Checked = false;
			this.printScale150.Checked = true;
			this.printScale125.Checked = false;
			this.printScale100.Checked = false;
			this.printScale80.Checked = false;
			this.printScale60.Checked = false;
			this.printScale40.Checked = false;
			this.print_scale = 1.25f;
			Registry_Settings.Write("PrintScale","-1");
		}

		/* this one is really 125 */
		private void PrintScale_100(object sender, System.EventArgs e)
		{
			this.printScale300.Checked = false;
			this.printScale200.Checked = false;
			this.printScale175.Checked = false;
			this.printScale150.Checked = false;
			this.printScale125.Checked = true;
			this.printScale100.Checked = false;
			this.printScale80.Checked = false;
			this.printScale60.Checked = false;
			this.printScale40.Checked = false;
			this.print_scale = 1.0f;
			Registry_Settings.Write("PrintScale","0");
		}

		private void PrintScale_80(object sender, System.EventArgs e)
		{
			this.printScale300.Checked = false;
			this.printScale200.Checked = false;
			this.printScale175.Checked = false;
			this.printScale150.Checked = false;
			this.printScale125.Checked = false;
			this.printScale100.Checked = true;
			this.printScale80.Checked = false;
			this.printScale60.Checked = false;
			this.printScale40.Checked = false;

			this.print_scale =0.75f;
			Registry_Settings.Write("PrintScale","1");
		}

		private void PrintScale_60(object sender, System.EventArgs e)
		{
			this.printScale300.Checked = false;
			this.printScale200.Checked = false;
			this.printScale175.Checked = false;
			this.printScale150.Checked = false;
			this.printScale125.Checked = false;
			this.printScale100.Checked = false;
			this.printScale80.Checked = true;
			this.printScale60.Checked = false;
			this.printScale40.Checked = false;
			
			this.print_scale = 0.6f;
			Registry_Settings.Write("PrintScale","2");
		}

		private void PrintScale_40(object sender, System.EventArgs e)
		{
			this.printScale300.Checked = false;
			this.printScale200.Checked = false;
			this.printScale175.Checked = false;
			this.printScale150.Checked = false;
			this.printScale125.Checked = false;
			this.printScale100.Checked = false;
			this.printScale80.Checked = false;
			this.printScale60.Checked = true;
			this.printScale40.Checked = false;
			
			this.print_scale = 0.4f;
			Registry_Settings.Write("PrintScale","3");
		}

		private void PrintScale_20(object sender, System.EventArgs e)
		{
			this.printScale300.Checked = false;
			this.printScale200.Checked = false;
			this.printScale175.Checked = false;
			this.printScale150.Checked = false;
			this.printScale125.Checked = false;
			this.printScale100.Checked = false;
			this.printScale80.Checked = false;
			this.printScale60.Checked = false;
			this.printScale40.Checked = true;
			
			this.print_scale = 0.2f;
			Registry_Settings.Write("PrintScale","4");
		}
		// return false if we shouldn't continue
		private bool Save_Before_Losing(object sender, System.EventArgs e)
		{
			DialogResult dr = DialogResult.No;
			if (this.modified) 
			{
				string msg = "Choosing this option will delete the current flow chart!" + '\n' + "Do you want to save first?";
				dr = MessageBox.Show(msg,
					"Open New Chart",MessageBoxButtons.YesNoCancel,MessageBoxIcon.Warning);
			}

			if (dr == DialogResult.Cancel) 
			{
				return false;
			}
			if (dr == DialogResult.Yes) 
			{
				this.FileSave_Click(sender,e);
				
				while (this.Save_Error) 
				{
					dr = MessageBox.Show("Save failed-- try again?",
						"Open New Chart",
						MessageBoxButtons.YesNoCancel,MessageBoxIcon.Warning);
					if (dr==DialogResult.Yes) 
					{
						this.SaveAs_Click(sender,e);
					}
					else if (dr==DialogResult.Cancel) 
					{
						return false;
					}
					else
					{
						this.Save_Error=false;
					}
				}
			}
			return true;
		}

		private void Clear_Subcharts()
		{
            int min;
            if (Component.Current_Mode == Mode.Expert)
            {
                min = 1;
            }
            else
            {
                min = 0;
            }
			for (int i=this.carlisle.TabCount-1; i>min; i--)
			{
				this.carlisle.TabPages.RemoveAt(i);
			}
            if (this.carlisle.TabPages.Count > 0)
            {
                this.carlisle.SelectedTab = this.mainSubchart();
            }
		}

		public void new_clicked(object sender, System.EventArgs e)
		{
			if (!this.Save_Before_Losing(sender,e))
			{
				return;
			}
            if (Component.Current_Mode == Mode.Expert)
            {
                (this.carlisle.TabPages[0].Controls[0] as UMLDiagram).project.ClearProject();
            }
			Clear_Subcharts();
			Create_Flow_graphx();
			Runtime.Clear_Variables();
			Component.compiled_flowchart = false;
			this.runningState = false;
			this.mainSubchart().Start.scale = this.scale;
			this.mainSubchart().Start.Scale(this.scale);
			flow_panel.Invalidate();

			Undo_Stack.Clear_Undo(this);
			this.Text = My_Title + "- Untitled";
            if (!Component.BARTPE && !Component.VM && !Component.MONO)
            {
                this.mainSubchart().tab_overlay.Ink.DeleteStrokes();
                this.mainSubchart().scale_ink(this.scale);
            }
			this.modified = false;
			this.fileName = null;
			Plugins.Load_Plugins("");
			this.log.Clear();
			this.log.Record_Open();
			this.file_guid = System.Guid.NewGuid();
			MC.clear_txt();
			this.Update_View_Variables();
		}

		private void menuAbout_Click(object sender, System.EventArgs e)
		{
			HelpForm Helper  = new HelpForm();
			Helper.ShowDialog();
		}
        public static Oval Start_Delegate(Visual_Flow_Form f)
        {
            return f.mainSubchart().Start;
        }
        public delegate Oval Start_Delegate_Type(Visual_Flow_Form f);
        public static Start_Delegate_Type Start_delegate = new Start_Delegate_Type(Start_Delegate);

        public static void Set_Current_Tab_Delegate(Visual_Flow_Form f, TabPage tb)
        {
            if (Component.Current_Mode != Mode.Expert)
            {
                f.carlisle.SelectedTab = tb;
            }
            else
            {
                if (f.carlisle.TabPages.Contains(tb))
                {
                    f.carlisle.SelectedTab = tb;
                }
                else
                {
                    for (int i = f.mainIndex + 1; i < f.carlisle.TabCount; i++)
                    {
                        if ((f.carlisle.TabPages[i] as ClassTabPage).tabControl1.TabPages.Contains(tb))
                        {
                            (f.carlisle.TabPages[i] as ClassTabPage).tabControl1.SelectedTab = tb;
                            f.carlisle.SelectedTab = f.carlisle.TabPages[i];
                            return;
                        }
                    }
                }
            }
        }
        public delegate void Set_Current_Tab_Delegate_Type(Visual_Flow_Form f, TabPage tb);
        public static Set_Current_Tab_Delegate_Type Set_Current_Tab_delegate =
            new Set_Current_Tab_Delegate_Type(Set_Current_Tab_Delegate);

        public static void Set_Running_Delegate(Visual_Flow_Form f, bool v)
        {
            f.runningState = v;
        }

        public delegate void Set_Running_Delegate_Type(Visual_Flow_Form f, bool v);
        public static Set_Running_Delegate_Type Set_Running_delegate =
            new Set_Running_Delegate_Type(Set_Running_Delegate);

        public static void Set_TopMost_Delegate(Visual_Flow_Form f, bool v)
        {
            f.TopMost = v;
        }
        public delegate void Set_TopMost_Delegate_Type(Visual_Flow_Form f, bool v);
        public static Set_TopMost_Delegate_Type Set_TopMost_delegate =
            new Set_TopMost_Delegate_Type(Set_TopMost_Delegate);


        public static void MessageBox_Delegate(Visual_Flow_Form f,
			String text, String caption, MessageBoxIcon icon)
		{
			f.TopMost = true;
			System.Threading.Thread.Sleep(50);
			MessageBox.Show(f,text,caption,MessageBoxButtons.OK,
				icon);
			f.TopMost = false;
			f.MC.BringToFront();
		}

		public delegate void MessageBox_Delegate_Type(Visual_Flow_Form f,
			String text, String caption, MessageBoxIcon icon);
		public static MessageBox_Delegate_Type MessageBox_delegate=
			new MessageBox_Delegate_Type(MessageBox_Delegate);

		public static void updateScreen(Visual_Flow_Form f)
		{
			if (f.currentObj!=null)
			{
				// how do I scroll to this point?
				System.Drawing.Point pt =
					f.panel1.AutoScrollPosition;
				// ok, so I don't get it, but pt.Y seems
				// to be negative??
				if (pt.Y<0)
				{
					pt.Y = -pt.Y;
				}
				int x = f.currentObj.X;
				int y = f.currentObj.Y;
				// really, for a loop we're not interested
				// in the circle, but the diamond
				if ((f.currentObj.Name=="Loop") &&
					(((Loop) f.currentObj).light_head == false))
				{
					y = ((Loop) f.currentObj).diamond_top;
				}
				if ((pt.Y < y) &&
					(y < pt.Y+f.panel1.Height-20))
				{
					y=pt.Y;
				}
				else
				{
					y=y-f.panel1.Height/2;
				}
				if ((pt.X < x) &&
					(x < pt.X+f.panel1.Width-20))
				{
					x=pt.X;
				}
				else
				{
					x=x-f.panel1.Width/2;
				}
				f.panel1.AutoScrollPosition =
					new System.Drawing.Point(
					x,
					y);
				f.scroll_location = f.panel1.AutoScrollPosition;
			}
			f.flow_panel.Invalidate();
		}
        public delegate void updateScreen_Delegate_Type(Visual_Flow_Form f);
        public static updateScreen_Delegate_Type updateScreen_delegate =
            new updateScreen_Delegate_Type(updateScreen);

        public static void invalidateScreen(Visual_Flow_Form f)
        {
            f.flow_panel.Invalidate();
        }
        public delegate void invalidateScreen_Delegate_Type(Visual_Flow_Form f);
        public static invalidateScreen_Delegate_Type invalidateScreen_delegate =
            new invalidateScreen_Delegate_Type(invalidateScreen);
        private int firstSubchart()
        {
            return ((Component.Current_Mode == Mode.Expert) ? 1 : 0);
        }
        private void Clear_Selections()
		{
            foreach (Subchart s in this.allSubcharts)
            {
				s.Start.select(-1000,-1000);
			}
		}
        public IEnumerable<ClassTabPage> allClasses
        {
            get
            {
                if (Component.Current_Mode == Mode.Expert)
                {
                    for (int i = mainIndex + 1; i < this.carlisle.TabPages.Count; i++)
                    {
                        yield return this.carlisle.TabPages[i] as ClassTabPage;
                    }
                }
            }
        }
                // Iterate from bottom to top.
        public IEnumerable<Subchart> allSubcharts
        {
            get
            {
                return Compile_Helpers.allSubcharts(this.carlisle.TabPages);
            }
        }
        private void Clear_Expression_Counts()
        {
            foreach (Subchart s in this.allSubcharts)
            {
                s.Start.reset_number_method_expressions_run();
            }
        }
		private bool Has_Code()
		{
			bool answer=true;

			foreach (Subchart s in this.allSubcharts)
			{
				if (!s.Start.has_code())
				{
					s.Start.mark_error();
					s.flow_panel.Invalidate();
					answer=false;
				}
			}
			return answer;
		}
		public Subchart Find_Tab(string s)
		{
            string t;
            int paren_loc = s.IndexOf('(');
            if (paren_loc > 0)
            {
                t = s.Substring(0, paren_loc).Trim();
            }
            else
            {
                t = s.Trim();
            }
			for (int i=0; i<this.carlisle.TabCount; i++)
			{
				if (this.carlisle.TabPages[i].Text.ToLower()==t.ToLower())
				{
                    if (this.carlisle.TabPages[i] is Subchart)
                    {
                        return (Subchart)this.carlisle.TabPages[i];
                    }
				}
			}
			throw new System.Exception("can't find procedure or subchart: " + s);
		}
        public ClassTabPage currentClass()
        {
            return findClass(this.running_tab);
        }
        public void Possible_Tab_Update(TabPage tb)
        {
            if (!this.full_speed || !this.continuous_Run)
            {
                Set_Current_Tab_Using_Delegate(tb);
            }
        }
		private void menuStep()
		{
			try 
			{
				if (Component.compiled_flowchart)
				{
					if (runningState == false)
					{
						try 
						{
							Compile_Helpers.Run_Compiled(false);
						}
						catch (System.Exception e)
						{
							System.Windows.Forms.MessageBox.Show("Flowchart terminated abnormally\n" +
								e.ToString());
						}
					}
					return;
				}
				if (runningState == false)
                {
                    Object[] delegate_args = new Object[2];
                    // set current obj to null so Start gets highlighted as "about to run"
                    this.currentObj = null;
                    this.symbol_count = 0;
                    Runtime.Clear_Variables();
                    delegate_args[0] = this;
                    delegate_args[1] = true;
                    this.Invoke(Set_Running_delegate, delegate_args);
                    //runningState = true;
                    this.Clear_Selections();
                    this.Clear_Expression_Counts();
                    if (!this.Has_Code())
                    {
                        string msg = "Before you can execute this flow chart, you need to add code to the the symbols with the red error message";
                        Object[] args = new Object[4];
                        args[0] = this;
                        args[1] = msg;
                        args[2] = "Error";
                        args[3] = MessageBoxIcon.Warning;
                        this.Invoke(MessageBox_delegate,
                            args);
                        delegate_args[0] = this;
                        delegate_args[1] = false;
                        this.Invoke(Set_Running_delegate, delegate_args);
                        this.continuous_Run = false;
                        if (this.myTimer != null)
                        {
                            this.myTimer.Stop();
                        }
                        return;
                    }
                    Runtime.createStaticVariables();
                    // start on the main tab
                    this.running_tab = this.mainSubchart();
                    {
                        this.Set_Current_Tab_Using_Delegate(this.mainSubchart());
                    }

                }
				Runtime.Clear_Updated();
				control_figure_selected = -1;
				control_panel.Invalidate();

				Component prevObj = currentObj;

				do
				{
                    this.symbol_count++;
					// currentObj is null means that we need to highlight the start symbol
					if (this.currentObj==null)
					{
                        Object[] delegate3_args = new Object[1];
                        //Object[] delegate_answer;
                        delegate3_args[0] = this;
						this.currentObj = (Oval) this.Invoke(Start_delegate,delegate3_args);
                        //this.currentObj = (Oval)delegate_answer[0];
						this.currentObj.running = true;
					}
					else
					{
						Component previous_Obj = this.currentObj;
                        if ((this.currentObj is Rectangle) &&
                            (this.currentObj.parse_tree is parse_tree.method_proc_call))
                        {
                            System.Windows.Forms.TabPage tb;
                            CallStack.Push(this.currentObj, (Subchart)this.running_tab);
                            //tb = this.Find_Tab(this.currentObj.Text);
                            //this.running_tab = tb;
                            Step_Helpers.Set_State_Entering();
                            interpreter_pkg.run_assignment(this.currentObj.parse_tree,
                                this.currentObj.Text);
                            this.Possible_Tab_Update(this.running_tab);
                            this.currentObj = ((Subchart)this.running_tab).Start;
                            this.currentObj.running = true;
                        }
						else if (this.currentObj.Name=="Rectangle" && 
							this.Is_Subchart_Call(this.currentObj.Text))
						{
							System.Windows.Forms.TabPage tb;
							CallStack.Push(this.currentObj,(Subchart) this.running_tab);
							tb = this.Find_Tab(this.currentObj.Text);
							this.running_tab = tb;
                            if (this.running_tab is Procedure_Chart)
                            {
                                Step_Helpers.Set_State_Entering();
                                interpreter_pkg.run_assignment(this.currentObj.parse_tree,
                                    this.currentObj.Text);
                            }
                            this.Possible_Tab_Update(tb);
							this.currentObj=((Subchart) tb).Start;
							this.currentObj.running=true;
						}
						else
						{
							this.currentObj=Step_Helpers.Step_Once(
								this.currentObj,this);
							previous_Obj.running=false;
							if (this.currentObj == null && CallStack.Count()==0) 
							{
                                Object[] delegate4_args = new Object[2];
                                delegate4_args[0] = this;
                                delegate4_args[1] = false;
                                this.Invoke(Set_Running_delegate, delegate4_args);
                                //this.runningState = false;
                                if (this.menuProgramCompleteDialog.Checked)
                                {
                                    delegate4_args[0] = this;
                                    delegate4_args[1] = true;
                                    this.Invoke(Set_TopMost_delegate, delegate4_args);
                                    System.Threading.Thread.Sleep(10);
                                    Object[] args = new Object[4];
                                    args[0] = this;
                                    args[1] = "Flowchart complete.";
                                    args[2] = "End of algorithm";
                                    args[3] = MessageBoxIcon.None;
                                    this.Invoke(MessageBox_delegate,
                                        args);
                                    delegate4_args[0] = this;
                                    delegate4_args[1] = false;
                                    this.Invoke(Set_TopMost_delegate, delegate4_args);
                                }
                                this.continuous_Run = false;
								this.MC.program_stopped("Run complete.  " + (this.symbol_count-1) +
                                    " symbols evaluated.");
								if (command_line_run)
								{
									this.exit_Click(null,null);
								}
							} 
							else if (this.currentObj == null)
							{
								// this means that we got past the end of a subchart
								CallStack.StackFrame sf = CallStack.Top();
								CallStack.Pop();
                                Step_Helpers.call_rect = sf.obj;
                                Subchart sc = this.running_tab as Subchart;
                                Step_Helpers.end_oval = sc.Start.find_end();
                                Step_Helpers.next_chart = sf.code;
                                // we don't really want to evaluate the stuff to the left of the "." in method calls
                                // so we remember the context on the callstack.  This is set in Step_Helpers during
                                // the Invoke_Method.  We reset the context here, skip most of the evaluation that is
                                // done in the execute from within step once (which resets the context to null when it is finished).
                                Runtime.setContext(sf.context);
                                Step_Helpers.Set_State_Leaving();
                                // for recursion, we'll make really sure this object
                                // knows a scope change happened
                                if (!(sf.obj is Rectangle) ||
                                    (sf.obj as Rectangle).kind!=Rectangle.Kind_Of.Call)
                                {
                                    sf.obj.need_to_decrease_scope = true;
                                }
                                this.currentObj = Step_Helpers.Step_Once(sf.obj,this);
                                this.currentObj.running = true;
								this.running_tab = sf.code;
                                this.Possible_Tab_Update(this.running_tab);
							}
							else 
							{
								this.currentObj.running = true;
							}
						}
						// moved here on 11/10/04 so that
						// breakpoint is checked in all cases
						if (this.continuous_Run &&
							this.currentObj!=null &&
							this.currentObj.break_now())
						{
                            // mcc: 03/01/06-- bahn reports not changing tabs on breakpoint
                            Set_Current_Tab_Using_Delegate(this.running_tab);
                            // this.tabControl1.SelectedTab = this.running_tab;
                            this.continuous_Run = false;
						}
					}
				} while (this.continuous_Run && this.full_speed);
				// don't update the screen if we are running at the
				// maximum speed
				if (!this.full_speed || !this.continuous_Run) 
				{
                    Object[] delegate_args5 = new Object[1];
                    delegate_args5[0] = this;
					this.Invoke(updateScreen_delegate,delegate_args5);
				}
				if (this.continuous_Run)
				{
					myTimer.Start();
				}
			}
			catch (System.Exception exc)
			{
				if (!this.Resetting)
				{
                    Object[] delegate_args6 = new Object[1];
                    delegate_args6[0] = this;
                    this.Invoke(updateScreen_delegate,delegate_args6);
                    if (this.running_tab != null)
                    {
                        Set_Current_Tab_Using_Delegate(this.running_tab);
                    }
                    this.Invoke(Set_TopMost_delegate, new Object[] { this, true });
                    // was exc.Message
                    if (exc.InnerException != null)
                    {
                        Runtime.consoleWriteln(exc.InnerException.Message);
                        this.Invoke(MessageBox_delegate,
                            new Object[] { this, exc.InnerException.Message, "Error during run", MessageBoxIcon.Error });
                    }
                    else
                    {
                        Runtime.consoleWriteln(exc.Message);
                        this.Invoke(MessageBox_delegate,
                            new Object[] { this, exc.Message, "Error during run", MessageBoxIcon.Error });
                    }
                    this.Invoke(Set_TopMost_delegate, new Object[] { this, false });
                    if (command_line_run)
					{
						this.exit_Click(null,null);
					}
					this.MC.program_stopped("Error, run halted");
				}
				if (this.continuous_Run ==true) 
				{
					myTimer.Stop();
				}
				this.continuous_Run = false;
                this.Invoke(Set_Running_delegate, new Object[] { this, false });
                if (currentObj != null)
				{
					currentObj.running = false;
					currentObj.selected = true;
				}
                currentObj = null; 
				CallStack.Clear_Call_Stack();
                this.Invoke(invalidateScreen_delegate, new Object[] { this });
			}

		}

        private void Set_Current_Tab_Using_Delegate(System.Windows.Forms.TabPage tb)
        {
            Object[] delegate_select_args = new Object[2];
            delegate_select_args[0] = this;
            delegate_select_args[1] = tb;
            this.Invoke(Set_Current_Tab_delegate, delegate_select_args);
        }


		private void menuStep_Click(object sender, System.EventArgs e)
		{
			if (InstanceCaller != null && InstanceCaller.IsAlive)
			{
				return;
			}
			if (!this.continuous_Run) 
			{
				InstanceCaller = new Thread(new ThreadStart(this.menuStep));
                //InstanceCaller.SetApartmentState(ApartmentState.STA);
                InstanceCaller.SetApartmentState(ApartmentState.MTA);
                InstanceCaller.Priority = ThreadPriority.BelowNormal;
                // Start the thread.
				InstanceCaller.Start();
			}
		}
			
		private void menuExecute_Click(object sender, System.EventArgs e)
		{
            if (this.fileName != null)
            {
                try
                {
                    System.Environment.CurrentDirectory = Directory.GetParent(this.fileName).FullName;
                }
                catch { }
            }
			if (Component.compiled_flowchart)
			{
				if (runningState == false)
				{
					try
					{
						Compile_Helpers.Run_Compiled(false);
					}
					catch (System.Exception exc)
					{
						System.Windows.Forms.MessageBox.Show("Flowchart terminated abnormally\n" +
							exc.ToString());
					}
				}
				return;
			}
			continuous_Run = true;
			if ((this.currentObj!=null) && (this.full_speed))
			{
				this.currentObj.running=false;
				this.flow_panel.Invalidate();
			}
			if (this.myTimer == null) 
			{
				myTimer = new System.Timers.Timer(this.Timer_Frequency);
				myTimer.Elapsed+= new System.Timers.ElapsedEventHandler(this.stepper);
			}
			//this.starting_time = System.DateTime.Now;
			//this.waiting_time = this.starting_time.AddMilliseconds(500.0);
			//Console.WriteLine("now is:" + this.starting_time + ":" + this.starting_time.Millisecond);
			myTimer.Start();
		}

        public static void Load_Delegate(Visual_Flow_Form f)
        {
            f.Load_File(f.load_filename);
        }
        public delegate void Load_Delegate_Type(Visual_Flow_Form f);
        public static Load_Delegate_Type Load_delegate = new Load_Delegate_Type(Load_Delegate);

        private void loader(object sender, System.Timers.ElapsedEventArgs e)
        {
            this.loadTimer.Stop();
            Object[] delegate_args = new Object[1];
            //Object[] delegate_answer;
            delegate_args[0] = this;
            this.Invoke(Load_delegate, delegate_args);
                        
            //this.Load_File(this.load_filename);
        }

		private void stepper(object sender, System.Timers.ElapsedEventArgs e)
		{
			//if (e.SignalTime < this.waiting_time)
			if (InstanceCaller != null && InstanceCaller.IsAlive)
			{
				return;
			}
			this.myTimer.Stop();
			//Console.WriteLine(e.SignalTime+ ":" + e.SignalTime.Millisecond);
			try 
			{
				InstanceCaller = new Thread(new ThreadStart(this.menuStep));
                //InstanceCaller.SetApartmentState(ApartmentState.STA);
                InstanceCaller.SetApartmentState(ApartmentState.MTA);
                InstanceCaller.Priority = ThreadPriority.BelowNormal;
				// Start the thread.
				InstanceCaller.Start();
			}
			catch (System.Exception exc)
			{
				Console.WriteLine(exc.Message);
			}
		}


        private ClassTabPage findClass(System.Windows.Forms.TabPage tp)
        {
            System.Windows.Forms.Control ctrl;
            ctrl = tp.Parent;
            while ((ctrl!=null) && (!(ctrl is System.Windows.Forms.TabPage)))
            {
                ctrl = ctrl.Parent;
            }
            return ctrl as ClassTabPage;
        }

		private void menuPause_Click(object sender, System.EventArgs e)
		{
			if (Component.compiled_flowchart)
			{
				return;
			}
			//Console.WriteLine("pausing");
			if (this.continuous_Run && this.full_speed) 
			{
				if (this.currentObj!=null)
				{
					this.currentObj.running=true;
				}
				if (this.running_tab!=null)
				{
                    if (this.running_tab.Parent == this.carlisle)
                    {
                        this.carlisle.SelectedTab = this.running_tab;
                    }
                    else
                    {
                        (this.running_tab.Parent as TabControl).SelectedTab = this.running_tab;
                        this.carlisle.SelectedTab = findClass(this.running_tab);
                    }
				}
				this.flow_panel.Invalidate();
			}
			this.continuous_Run = false;
			if (this.myTimer != null)
			{
				this.myTimer.Stop();
			}
		}

		private void SaveAs_Click(object sender, System.EventArgs e)
		{
			// Open a file and serialize the object into it in binary format.

            string dialog_fileName;
            string oldName = this.fileName;
            if (!Component.BARTPE)
            {
                SaveFileDialog fileChooser = new SaveFileDialog();
                fileChooser.CheckFileExists = false;
                fileChooser.Filter = "Raptor files (*.rap)|*.rap|All files (*.*)|*.*";
                fileChooser.DefaultExt = ".rap";
                fileChooser.RestoreDirectory = false;
                DialogResult result = fileChooser.ShowDialog();


                if (result == DialogResult.Cancel)
                {
                    return;
                }

                dialog_fileName = fileChooser.FileName;
            }
            else
            {
                BARTPEFileSaveList form = new BARTPEFileSaveList();
                form.ShowDialog();
                dialog_fileName = BARTPEFileSaveList.filename;
                if (dialog_fileName == null)
                {
                    return;
                }
            }
			if (dialog_fileName == "" || dialog_fileName == null)
			{
				MessageBox.Show("Invalid File Name", "Error",
					MessageBoxButtons.OK, MessageBoxIcon.Error);
				return;
			}

			else
			{
				this.fileName = dialog_fileName;
				this.FileSave_Click(sender,e);
				if (this.Save_Error)
				{
					this.fileName = oldName;
					return;
				}
				this.Text = My_Title + " - " + 
					Path.GetFileName(this.fileName);
				this.modified = false;
				Plugins.Load_Plugins(this.fileName);
				MRU.Add_To_MRU_Registry(this.fileName);
			}
		}
		private void menuStop_Click(object sender, System.EventArgs e)
		{

            if (this.runningState || (Compile_Helpers.run_compiled_thread != null &&
                Compile_Helpers.run_compiled_thread.ThreadState == System.Threading.ThreadState.Running))
            {
                this.MC.program_stopped("Run halted");
            }
            else
            {
                this.MC.program_stopped("Reset");
            }
            if (Compile_Helpers.run_compiled_thread != null &&
                Compile_Helpers.run_compiled_thread.ThreadState == System.Threading.ThreadState.Running)
            {
                try
                {
                    Compile_Helpers.run_compiled_thread.Abort();
                }
                catch 
                {
                }
            } 
            this.Resetting = true;
			this.runningState = false;
			if (this.currentObj!=null) 
			{
				this.currentObj.running = false;
			}
			this.currentObj = null;
			this.continuous_Run = false;
			if (this.myTimer != null) 
			{
				this.myTimer.Stop();
			}
			PromptForm.Kill();
			if (this.InstanceCaller != null && this.InstanceCaller.IsAlive)
			{
                try {
				   this.InstanceCaller.Abort();
				   this.InstanceCaller.Join(new System.TimeSpan(0,0,0,1,0));
                }
                catch 
                {
                }
				//this.InstanceCaller.Join();
			}

			this.Resetting = false;
		}

		private void menuReset_Click(object sender, System.EventArgs e)
		{
			this.Resetting = true;
            Component.run_compiled_flowchart = false;
			this.menuStop_Click(sender,e);
			Runtime.Clear_Variables();
			CallStack.Clear_Call_Stack();
			this.flow_panel.Invalidate();
			this.Resetting = false;
		}

		private void menuResetExecute_Click(object sender, System.EventArgs e)
		{
			this.menuReset_Click(sender,e);
			this.menuExecute_Click(sender,e);
		}

		private void toolBar1_ButtonClick(object sender, System.Windows.Forms.ToolBarButtonClickEventArgs e)
		{
			if ( e.Button == newButton )
			{
				this.new_clicked(sender,e);
			} 
			
			else if ( e.Button == openButton )
			{
				this.FileOpen_Click(sender,e);
			}

			else if ( e.Button == saveButton )
			{
				this.FileSave_Click(sender,e);
			}

			else if ( e.Button == cutButton )
			{
				this.Cut_Click(sender,e);
			}

			else if ( e.Button == copyButton )
			{
				this.Copy_Click(sender,e);
			}

			else if ( e.Button == pasteButton )
			{
				this.paste_Click(sender,e);
			}

			else if ( e.Button == printButton )
			{
				this.filePrintMenuItem_Click(sender,e);
			}

			else if ( e.Button == undoButton )
			{
				this.Undo_Click(sender,e);
			}

			else if ( e.Button == redoButton )
			{
				this.Redo_Click(sender,e);
			}

			else if ( e.Button == stepButton )
			{
				this.menuStep_Click(sender,e);
			}

			else if ( e.Button == stopButton )
			{
				if (this.runningState)
				{
					this.menuPause_Click(sender,e);
					this.menuStop_Click(sender,e);
				}
				else
				{
					this.menuReset_Click(sender,e);
				}
			}
			else if ( e.Button == pauseButton )
			{
				this.menuPause_Click(sender,e);
			}

			else if ( e.Button == playButton )
			{
				this.menuExecute_Click(sender,e);
			}

            else if (e.Button == testServerButton)
            {
                this.menuRunServer_Click(sender, e);
            }
            else if (e.Button == InkButton1)
            {
                if (InkButton1.Pushed)
                {
                    if (Last_Ink_Color == Color.Green)
                    {
                        this.menuItemInkGreen_Click(sender, e);
                    }
                    else if (Last_Ink_Color == Color.Black)
                    {
                        this.menuItemInkBlack_Click(sender, e);
                    }
                    else if (Last_Ink_Color == Color.Red)
                    {
                        this.menuItemInkRed_Click(sender, e);
                    }
                    else
                    {
                        this.menuItemInkBlue_Click(sender, e);
                    }
                }
                else
                {
                    this.menuItemInkOff_Click(sender,e);
                }
            }
			
			//MessageBox.Show( "Button1 Clicked ");
		}

		private void trackBar1_Scroll(object sender, System.EventArgs e)
		{
			this.Timer_Frequency = 1005-this.trackBar1.Value*100;
			//Console.WriteLine(this.Timer_Frequency);
			if (this.myTimer != null) 
			{
				this.myTimer.Interval = this.Timer_Frequency;
			}
			if (this.trackBar1.Value==this.trackBar1.Maximum && !Component.BARTPE)
			{
				this.full_speed=true;
				if (this.continuous_Run==true)
				{
					this.currentObj.running=false;
					this.flow_panel.Invalidate();
				}
			}
			else
			{
				this.full_speed=false;
				if (this.continuous_Run==true)
				{
					this.currentObj.running=true;
					this.flow_panel.Invalidate();
				}
			}
			Registry_Settings.Write("Speed",
				this.trackBar1.Value.ToString());
		}

		private void comboBox1_SelectedIndexChanged(object sender, System.EventArgs e)
		{
			switch(this.comboBox1.SelectedIndex) 
			{
				case 0:
					this.menuScale300_Click(sender,e);
					break;
				case 1:
					this.menuScale200_Click(sender,e);
					break;
				case 2:
					this.menuScale175_Click(sender,e);
					break;
				case 3:
					this.menuScale150_Click(sender,e);
					break;
				case 4:
					this.Scale_100(sender,e);
					break;
				case 5:
					this.Scale_80(sender,e);
					break;
				case 6:
					this.Scale_60(sender,e);
					break;
				case 7:
					this.Scale_40(sender,e);
					break;
				case 8:
					this.Scale_20(sender,e);
					break;
			}
		}

		private void menuFile_Click(object sender, System.EventArgs e)
        {
            this.menuItemCompile.Enabled = (Component.Current_Mode != Mode.Expert);
			MRU.Update_MRU_Menus(this);
		}

		public void Load_MRU(string name)
		{
			if (!this.Save_Before_Losing(null,null))
			{
				return;
			}
			if ((name != null) && (name.CompareTo("")!=0))
			{
				this.Load_File(name);
			}
			MRU.Add_To_MRU_Registry(name);
		}

		private void menuMRU1_Click(object sender, System.EventArgs e)
		{
			string name = MRU.Get_MRU_Registry(1);
			this.Load_MRU(name);
		}

		private void menuMRU2_Click(object sender, System.EventArgs e)
		{
			string name = MRU.Get_MRU_Registry(2);
			this.Load_MRU(name);
		}

		private void menuMRU3_Click(object sender, System.EventArgs e)
		{
			string name = MRU.Get_MRU_Registry(3);
			this.Load_MRU(name);
		}

		private void menuMRU4_Click(object sender, System.EventArgs e)
		{
			string name = MRU.Get_MRU_Registry(4);
			this.Load_MRU(name);
		}
        private void menuMRU5_Click(object sender, EventArgs e)
        {
            string name = MRU.Get_MRU_Registry(5);
            this.Load_MRU(name);
        }

        private void menuMRU6_Click(object sender, EventArgs e)
        {
            string name = MRU.Get_MRU_Registry(6);
            this.Load_MRU(name);
        }

        private void menuMRU7_Click(object sender, EventArgs e)
        {
            string name = MRU.Get_MRU_Registry(7);
            this.Load_MRU(name);
        }

        private void menuMRU8_Click(object sender, EventArgs e)
        {
            string name = MRU.Get_MRU_Registry(8);
            this.Load_MRU(name);
        }

        private void menuMRU9_Click(object sender, EventArgs e)
        {
            string name = MRU.Get_MRU_Registry(9);
            this.Load_MRU(name);
        }
		private void Visual_Flow_Form_Closing(object sender, System.ComponentModel.CancelEventArgs e)
		{
			if (this.modified)
			{
				DialogResult dr = MessageBox.Show(
					"Flowchart was modified-- save these changes?",
					"Save changes?",MessageBoxButtons.YesNoCancel,MessageBoxIcon.Warning);
				if (dr==DialogResult.Yes)
				{
					this.FileSave_Click(sender,null);
					e.Cancel=this.Save_Error;
				}
				else if (dr==DialogResult.Cancel)
				{
					e.Cancel=true;
				}
				else
				{
					e.Cancel=PromptForm.Close_All();
				}
			}
			else
			{
				e.Cancel=PromptForm.Close_All();
			}
			if (e.Cancel==false)
			{
				// closes RaptorGraph, files, etc.
                if (this.InstanceCaller != null && this.InstanceCaller.IsAlive)
                {
                    this.InstanceCaller.Abort();
                }
				Runtime.Clear_Variables();
                dotnetgraphlibrary.dotnetgraph.Shutdown_Dotnetgraph();
			}
		}
		public void Show_Text_On_Error()
		{
			if (Component.text_visible)
			{
				return;
			}
			Component.text_visible = true;
			if (Component.full_text)
			{
				this.menuAllText.Checked = true;
				this.menuTruncated.Checked = false;
				this.menuNoText.Checked = false;
			}
			else
			{
				this.menuAllText.Checked = false;
				this.menuTruncated.Checked = true;
				this.menuNoText.Checked = false;
			}
			this.flow_panel.Invalidate();
		}

		private void menuAllText_Click(object sender, System.EventArgs e)
		{
			Component.full_text = true;
			Component.text_visible = true;
			this.menuAllText.Checked = true;
			this.menuTruncated.Checked = false;
			this.menuNoText.Checked = false;
			this.flow_panel.Invalidate();
			Registry_Settings.Write("TextView","AllText");
		}

		private void menuTruncated_Click(object sender, System.EventArgs e)
		{
			Component.full_text = false;
			Component.text_visible = true;
			this.menuAllText.Checked = false;
			this.menuTruncated.Checked = true;
			this.menuNoText.Checked = false;
			this.flow_panel.Invalidate();
			Registry_Settings.Write("TextView","Truncated");
		}

		private void menuNoText_Click(object sender, System.EventArgs e)
		{
			Component.full_text = false;
			Component.text_visible = false;
			this.menuAllText.Checked = false;
			this.menuTruncated.Checked = false;
			this.menuNoText.Checked = true;
			this.flow_panel.Invalidate();
			Registry_Settings.Write("TextView","NoText");
		}

		private void listOfFunctionsMenu_Click(object sender, System.EventArgs e)
		{
                Help.ShowHelp(this, Directory.GetParent(
                    Application.ExecutablePath) + "\\raptor.chm");
		}

		private void contextMenu1_Popup(object sender, System.EventArgs e)
		{
			bool non_oval_selected = this.Current_Selection != null &&
				this.Current_Selection.Name != "Oval";
			//this.contextMenuPaste.Enabled=(
			//	(this.clipboard != null && this.Current_Selection==null) ||
			//	(this.comment_clipboard != null && this.Current_Selection != null));
			this.contextMenuComment.Enabled=(this.Current_Selection!=null);
			if (non_oval_selected || this.selectedComment != null || this.region_selected)
			{
				this.contextMenuCopy.Enabled=true;
				this.contextMenuCut.Enabled=true;
				this.contextMenuDelete.Enabled=true;
				this.contextMenuEdit.Enabled=true;
			}
			else
			{
				this.contextMenuCopy.Enabled=false;
				this.contextMenuCut.Enabled=false;
				this.contextMenuDelete.Enabled=false;
				this.contextMenuEdit.Enabled=false;
			}
		}
        private bool UML_Displayed()
        {
            return (Component.Current_Mode==Mode.Expert && this.carlisle.SelectedIndex == 0);
        }

		private void menuEdit_Click(object sender, System.EventArgs e)
		{
            if (UML_Displayed())
            {
                this.menuItemCopy.Enabled = false;
                this.menuItemDelete.Enabled = true;
                this.menuItemCut.Enabled = false;
                this.menuItemPaste.Enabled = false;
                this.menuItemComment.Enabled = false;
                this.menuItemEditSelection.Enabled = false;
                return;
            }
			bool non_oval_selected = this.Current_Selection != null &&
				this.Current_Selection.Name != "Oval";
			IDataObject obj = ClipboardMultiplatform.GetDataObject();
            Component.warned_about_error = true;
            Clipboard_Data cd=null;
            if (obj != null)
            {
                cd = (Clipboard_Data)obj.GetData(
                    "raptor.Clipboard_Data");
            }

			this.menuItemPaste.Enabled=(
				(cd != null && cd.kind == Clipboard_Data.kinds.symbols &&
				 this.Current_Selection==null) ||
				(cd != null && cd.kind == Clipboard_Data.kinds.comment && 
				 this.Current_Selection!=null));
			this.menuItemComment.Enabled=(this.Current_Selection!=null);
            Subchart current = this.selectedTabMaybeNull(); 
            try
            {

                if (!Component.BARTPE && !Component.VM && !Component.MONO && current!=null && current.tab_overlay.Ink.CanPaste())
                {
                    this.menuItemPaste.Enabled = true;
                }
            }
            catch
            {
                MessageBox.Show("Please install the Microsoft.Ink.dll CLR 2.0 Update (KB900722)");
            }
            if (non_oval_selected || this.selectedComment != null || this.region_selected)
			{
				this.menuItemCopy.Enabled=true;
				this.menuItemDelete.Enabled=true;
				this.menuItemCut.Enabled=true;
			}
			else
			{
				this.menuItemCopy.Enabled=false;
				this.menuItemDelete.Enabled=false;
				this.menuItemCut.Enabled=false;
			}
            // special ink enabling
            if (!Component.BARTPE && !Component.VM && !Component.MONO && current!=null && current.tab_overlay.Selection != null &&
                current.tab_overlay.Selection.Count > 0)
            {
                this.menuItemCopy.Enabled = true;
                this.menuItemDelete.Enabled = true;
                this.menuItemCut.Enabled = true;
            }
			this.menuItemEditSelection.Enabled=
				(this.Current_Selection!=null);
            
		}

		private void contextMenuComment_Click(object sender, System.EventArgs e)
		{
			this.Current_Selection.addComment(this);
			this.my_layout();
			this.flow_panel.Invalidate();
		}

		private void menuViewComments_Click(object sender, System.EventArgs e)
		{
			this.menuViewComments.Checked = !this.menuViewComments.Checked;
			Component.view_comments = this.menuViewComments.Checked;
			this.flow_panel.Invalidate();
			Registry_Settings.Write("ViewComments",
				this.menuViewComments.Checked.ToString());
		}


		private void menuViewVariables_Click(object sender, System.EventArgs e)
		{
			this.menuViewVariables.Checked = !this.menuViewVariables.Checked &&
				!Component.compiled_flowchart;
			if (!this.menuViewVariables.Checked || Component.compiled_flowchart)
			{
				this.watchBox.Hide();
			}
			else
			{
				this.watchBox.Show();
			}
			if (!Component.compiled_flowchart)
			{
				Registry_Settings.Write("ViewVariables",
					this.menuViewVariables.Checked.ToString());
			}
		}

		private void menuShowLog_Click(object sender, System.EventArgs e)
		{
            this.log.Display(this, false);
            this.MC.BringToFront();
        }

		private void menuTileVertical_Click(object sender, System.EventArgs e)
		{
			this.WindowState=FormWindowState.Normal;
			this.MC.WindowState=FormWindowState.Normal;

			System.Drawing.Size sz = SystemInformation.PrimaryMonitorMaximizedWindowSize;
			this.Left = 0;
			this.Top = 0;
			this.Height = sz.Height-20;
			this.Width = sz.Width-this.MC.MinimumSize.Width;
			this.MC.Left = this.Width;
			this.MC.Top = 0;
			this.MC.Width = this.MC.MinimumSize.Width;
			this.MC.Height = sz.Height-20;
            this.MC.MasterConsole_Resize(sender, e);
            this.Visual_Flow_Form_Resize(sender, e);
		}

		private void menuTileHorizontal_Click(object sender, System.EventArgs e)
		{
			this.WindowState=FormWindowState.Normal;
			this.MC.WindowState=FormWindowState.Normal;

			System.Drawing.Size sz = SystemInformation.PrimaryMonitorMaximizedWindowSize;
			this.Left = 0;
			this.Top = 0;
			this.Width = sz.Width;
			this.Height = sz.Height - 20 - 150;
			this.MC.Left = 0;
			this.MC.Top = this.Height;
			this.MC.Height = 150;
			this.MC.Width = sz.Width;
            this.MC.MasterConsole_Resize(sender, e);
            this.Visual_Flow_Form_Resize(sender, e);
        }

		private void menuItemEditSelection_Click(object sender, System.EventArgs e)
		{
            Subchart current = this.selectedTabMaybeNull();
			if (current!=null && current.Current_Selection != null)
			{
				current.Start.setText(
                    current.Current_Selection.X+1,current.Current_Selection.Y+1,this);
			}
		}

		private void Change_Selection_To(Component c)
		{
            Subchart current = this.selectedTabMaybeNull();
            if (current == null) return;
            current.Current_Selection.selected=false;
			current.Current_Selection=c;
			current.Start.select(current.Current_Selection.X+1,
				current.Current_Selection.Y+
				current.Current_Selection.H/2);
			this.flow_panel.Invalidate();
		}

		public void Visual_Flow_Form_KeyDown(object sender, System.Windows.Forms.KeyEventArgs e)
		{
			if (e.KeyCode==System.Windows.Forms.Keys.PageUp)
			{
				System.Drawing.Point pt =
					this.panel1.AutoScrollPosition;
				// ok, so I don't get it, but pt.Y seems
				// to be negative??
				if (pt.Y<0)
				{
					pt.Y = -pt.Y;
				}
				pt.Y = pt.Y - 450;
				if (pt.Y < 0)
				{
					pt.Y = 0;
				}
				this.panel1.AutoScrollPosition = pt;
			}
			else if (e.KeyCode==System.Windows.Forms.Keys.PageDown)
			{
				System.Drawing.Point pt =
					this.panel1.AutoScrollPosition;
				// ok, so I don't get it, but pt.Y seems
				// to be negative??
				if (pt.Y<0)
				{
					pt.Y = -pt.Y;
				}
				pt.Y = pt.Y + 450;

				this.panel1.AutoScrollPosition = pt;
			}

			if (this.Current_Selection!=null)
			{
				// on down, if a loop, try first the before child
				// then the after child, then the successor
				// on down, otherwise, go to successor
				// on down, if no successor and before_child, go
				// to after_child or successor
				// on down, if no successor and after_child, go
				// to parent's successor
				if (e.KeyCode==System.Windows.Forms.Keys.Down)
				{
					if (this.Current_Selection.Name!="Loop")
					{
						if (this.Current_Selection.Successor!=null)
						{
							this.Change_Selection_To(
								this.Current_Selection.Successor);
						}
						else if (this.Current_Selection.is_afterChild)
						{
							this.Change_Selection_To(
								this.Current_Selection.parent.Successor);
						}
						else if (this.Current_Selection.is_beforeChild)
						{
							if (((Loop) this.Current_Selection.parent).after_Child != null)
							{
								this.Change_Selection_To(
									((Loop) this.Current_Selection.parent).after_Child);
							}
							else
							{
								this.Change_Selection_To(
									this.Current_Selection.parent.Successor);
							}
						}
					}
					else
					{
						if (((Loop) this.Current_Selection).before_Child != null)
						{
							this.Change_Selection_To(
								((Loop) this.Current_Selection).before_Child);
						}
						else if (((Loop) this.Current_Selection).after_Child != null)
						{
							this.Change_Selection_To(
								((Loop) this.Current_Selection).after_Child);
						}
						else
						{
							this.Change_Selection_To(
								this.Current_Selection.Successor);
						}
					}
				}
				else if (e.KeyCode==System.Windows.Forms.Keys.Up && this.selectedTabMaybeNull()!=null)
				{
					// up goes to predecessor, which for loop first
					// after child may be last of loop before child
					Component predecessor =
						this.selectedTabMaybeNull().Start.Find_Predecessor(
						this.Current_Selection);
					if (predecessor!=null)
					{
						this.Change_Selection_To(
							predecessor);
					}
				}
                else if (e.KeyCode == System.Windows.Forms.Keys.Left && this.selectedTabMaybeNull() != null)
				{
					// left goes to left child of if, or successor
					// of loop
					if (this.Current_Selection.Name=="IF_Control" &&
						((IF_Control) this.Current_Selection).left_Child!=null)
					{
						this.Change_Selection_To(
							((IF_Control) this.Current_Selection).left_Child);
					}
					else if (this.Current_Selection.Name=="Loop")
					{
						this.Change_Selection_To(
							this.Current_Selection.Successor);
					}
				}
                else if (e.KeyCode == System.Windows.Forms.Keys.Right && this.selectedTabMaybeNull() != null)
				{
					// right goes to right child of if
					if (this.Current_Selection.Name=="IF_Control" &&
						((IF_Control) this.Current_Selection).right_Child!=null)
					{
						this.Change_Selection_To(
							((IF_Control) this.Current_Selection).right_Child);
					}
				}
                else if ((e.KeyCode == System.Windows.Forms.Keys.Enter ||
                        e.KeyCode == System.Windows.Forms.Keys.Return) &&
                    this.selectedTabMaybeNull()!=null)
                {
                    this.selectedTabMaybeNull().Start.setText(this.Current_Selection.X + 5, this.Current_Selection.Y + 5, this);
                }
			}
			else // current selection == null
			{
				if (e.KeyCode==System.Windows.Forms.Keys.Down && 
                    this.selectedTabMaybeNull()!=null)
				{
                    Subchart current = this.selectedTabMaybeNull();
                    this.Current_Selection = current.Start;
                    this.Change_Selection_To(current.Start);
				}
			}
		}

		
		public void num_vertical_print_pages(PrintPageEventArgs e, Oval current_tab_start)
		{
			
			int current_page_num = 1;
			// mcc: bug found 9/12/03 where last page isn't printed if it is less
			// than topMargin tall
			int more_to_test = drawing_height;
			int y_test;

			this.vert_page_breaks[0] = 0;

			//Console.WriteLine("FP height: " + Start.FP.height);
			//Console.WriteLine("page height: " + pageheight);

			e.Graphics.SetClip(new RectangleF(
				0,0,0,
				0));

			current_tab_start.draw(e.Graphics,x1,y1);
			while (more_to_test > 0)
			{
				int y_test_start, y_test_stop;
				if (current_page_num == 1)
				{
					y_test = y1 + (int)pageheight;
					y_test_stop = y1 + ((int) pageheight)*2/3;
				}
				else
				{
					y_test = this.vert_page_breaks[current_page_num-1] + (int)pageheight;
					y_test_stop = this.vert_page_breaks[current_page_num-1] + ((int) pageheight)*2/3;
				}
				y_test_start = y_test;

				System.Drawing.Rectangle  rec = new System.Drawing.Rectangle(1, y_test,drawing_width,1);

				bool found_break = false;

				while (current_tab_start.SelectRegion(rec) && 
					(y_test > y_test_stop))
				{
					y_test =  y_test-1;
					rec.Y = y_test;
					found_break = true;
					//Console.WriteLine(y_test);
				}

				if (y_test==y_test_stop)
				{
					y_test = y_test_start;
				}

				if (found_break)
				{
					this.vert_page_breaks[current_page_num] = y_test-(current_tab_start.CL/2);
					//Console.WriteLine("Found break for page " +
					//   current_page_num + 
					//   " at: " + this.vert_page_breaks[current_page_num]);
				}
				else
				{
					this.vert_page_breaks[current_page_num] = y_test;
				}

				// bug fixed on 24 Sep 2003 (mcc)
				more_to_test = (y1 + drawing_height) - y_test;
				
				if (more_to_test > 0)
				{
					current_page_num++;
				}
			}
			this.num_vert_pages = current_page_num;
			//Console.WriteLine("Page Breaks: " + current_page_num);
		}

		public void num_horizontal_print_pages(PrintPageEventArgs e, Oval current_tab_start)
		{
			int current_page_num = 1;
			int x_test;
			int xleft = (int) Math.Round(leftMargin);

			int more_to_test = drawing_width;
			this.hor_page_breaks[0] = 0;
			e.Graphics.SetClip(new RectangleF(
				0,0,0,
				0));

			current_tab_start.draw(e.Graphics,x1,y1);
			while (more_to_test > 0)
			{
				int x_test_start, x_test_stop;

				// x_test will test for horizontal page breaks
				// starting at pagewidth and going back to 2/3 of the
				// width
				if (current_page_num == 1)
				{
					x_test = xleft + (int)pagewidth;
					x_test_stop = xleft + ((int) pagewidth)*2/3;
				}
				else
				{
					x_test = this.hor_page_breaks[current_page_num-1] + (int)pagewidth;
					x_test_stop = this.hor_page_breaks[current_page_num-1] + 
						((int) pagewidth)*2/3;
				}
				x_test_start = x_test;
	
				System.Drawing.Rectangle  rec = new System.Drawing.Rectangle(x_test,1,1,drawing_height);

				bool found_break = false;
				// try to find a break point that is within a third of the page
				// before just giving up and going for the full page
				while (current_tab_start.SelectRegion(rec) && 
					(x_test > x_test_stop))
				{
					x_test =  x_test-1;
					rec.X = x_test;
					found_break = true;
					//Console.WriteLine(x_test);
				}

				// if we gave up, go back to the start
				if (x_test==x_test_stop)
				{
					x_test = x_test_start;
				}

				if (found_break)
				{
					this.hor_page_breaks[current_page_num] = x_test-(current_tab_start.CL/2);
					//Console.WriteLine("Found horiz break for page " +
					//	current_page_num +
					//	" at: " + this.hor_page_breaks[current_page_num]);
				}
				else
				{
					this.hor_page_breaks[current_page_num] = x_test;
				}
				more_to_test = drawing_width - x_test;

				if (more_to_test > 0)
				{
					current_page_num++;
				}
			}
			this.num_hor_pages = current_page_num;
			//Console.WriteLine("Page Breaks: " + current_page_num);
		}


		public void menuItemAssignment_Click(object sender, System.EventArgs e)
		{
            Subchart sc = this.selectedTabMaybeNull();
            if (sc == null) return;
            Oval Start = sc.Start;
			this.Make_Undoable();
			if (Start.insert(new Rectangle(flow_height, flow_width,
				"Rectangle",
				Rectangle.Kind_Of.Assignment), mouse_x, mouse_y, 0))
			{
				this.Current_Selection = Start.select(-1000,-1000);
				Start.Scale(this.scale);
				this.my_layout();
				this.flow_panel.Invalidate();
			}
			else
			{
				Undo_Stack.Decrement_Undoable(this);
			}
		}

		public void menuItemCall_Click(object sender, System.EventArgs e)
		{
            Subchart sc = this.selectedTabMaybeNull();
            if (sc == null) return;
            Oval Start = sc.Start;
            this.Make_Undoable();
			if (Start.insert(new Rectangle(flow_height, flow_width, 
				"Rectangle", 
				Rectangle.Kind_Of.Call), mouse_x, mouse_y, 0))
			{
				this.Current_Selection = Start.select(-1000,-1000);
				Start.Scale(this.scale);
				this.my_layout();
				this.flow_panel.Invalidate();
			}
			else
			{
				Undo_Stack.Decrement_Undoable(this);
			}
		}

		public void menuItemParallelogram_Click(object sender, System.EventArgs e)
		{
            Subchart sc = this.selectedTabMaybeNull();
            if (sc == null) return;
            Oval Start = sc.Start;
            this.Make_Undoable();
			if (Start.insert(new Parallelogram(flow_height, flow_width, "Parallelogram", true), mouse_x, mouse_y, 0))
			{
				this.Current_Selection = Start.select(-1000,-1000);
				Start.Scale(this.scale);
				this.my_layout();
				this.flow_panel.Invalidate();
			}
			else
			{
				Undo_Stack.Decrement_Undoable(this);
			}
		}
		public void menuItemOutput_Click(object sender, System.EventArgs e)
		{
            Subchart sc = this.selectedTabMaybeNull();
            if (sc == null) return;
            Oval Start = sc.Start;
            this.Make_Undoable();
			if (Start.insert(new Parallelogram(flow_height, flow_width, "Parallelogram", false), mouse_x, mouse_y, 0))
			{
				this.Current_Selection = Start.select(-1000,-1000);
				Start.Scale(this.scale);
				this.my_layout();
				this.flow_panel.Invalidate();
			}
			else
			{
				Undo_Stack.Decrement_Undoable(this);
			}
		}

		public void menuItemIf_Click(object sender, System.EventArgs e)
        {
            Subchart sc = this.selectedTabMaybeNull();
            if (sc == null) return;
            Oval Start = sc.Start;
            this.Make_Undoable();
			if (Start.insert(new IF_Control(flow_height, flow_width, "IF_Control"), mouse_x, mouse_y, 0))
			{
				this.Current_Selection = Start.select(-1000,-1000);
				Start.Scale(this.scale);
				this.my_layout();
				this.flow_panel.Invalidate();
			}
			else
			{
				Undo_Stack.Decrement_Undoable(this);
			}
		}

		public void menuItemLoop_Click(object sender, System.EventArgs e)
        {
            Subchart sc = this.selectedTabMaybeNull();
            if (sc == null) return;
            Oval Start = sc.Start;
            this.Make_Undoable();
			if (Start.insert(new Loop(flow_height, flow_width, "Loop"), mouse_x, mouse_y, 0))
			{
				this.Current_Selection = Start.select(-1000,-1000);
				Start.Scale(this.scale);
				this.my_layout();
				this.flow_panel.Invalidate();
			}
			else
			{
				Undo_Stack.Decrement_Undoable(this);
			}
		}
        public void menuItemReturn_Click(object sender, EventArgs e)
        {
            Subchart sc = this.selectedTabMaybeNull();
            if (sc == null) return;
            Oval Start = sc.Start;
            this.Make_Undoable();
            if (Start.insert(new Oval_Return(flow_height, flow_width, "Return"), mouse_x, mouse_y, 0))
            {
                this.Current_Selection = Start.select(-1000, -1000);
                Start.Scale(this.scale);
                this.my_layout();
                this.flow_panel.Invalidate();
            }
            else
            {
                Undo_Stack.Decrement_Undoable(this);
            }
        }
		private void menuItemCompile_Click(object sender, System.EventArgs e)
		{
			this.mainSubchart().Start.selected = false;

			if (this.fileName==null||this.fileName=="")
			{
				MessageBox.Show("Must save before compiling",
					"Can't compile");
				return;
			}
			if (Component.compiled_flowchart)
			{
				MessageBox.Show("Already compiled",
					"Can't compile");
				return;
			}
			string compiled_name;

			if (this.fileName.Length>4)
			{
				compiled_name = this.fileName.Substring(0,
					this.fileName.Length-4) + " (compiled).rap";
			}
			else
			{
				compiled_name = this.fileName + " (compiled).rap";
			}

			DialogResult dr = MessageBox.Show(
				"Are you sure you want to compile?\n" +
				"You can not undo this operation.\n\n" +
				"This will save your current flowchart as:\n" +
				"   " + this.fileName + "\n" +
				"and compile to:\n" +
				"   " + compiled_name,
				"Compile?",
				MessageBoxButtons.YesNo,MessageBoxIcon.Warning);
		

			if (dr == DialogResult.No) 
			{
				return;
			}

			try
			{
				this.FileSave_Click(sender,e);
				this.fileName = compiled_name;
				Compile_Helpers.Compile_Flowchart(this.carlisle.TabPages);
				Component.compiled_flowchart = true;
				this.FileSave_Click(sender,e);

				this.Text = My_Title + " - " + 
					Path.GetFileName(this.fileName);
				this.modified = false;
				Registry_Settings.Ignore_Updates = true;
				this.trackBar1.Value=this.trackBar1.Maximum;
				this.trackBar1_Scroll(sender,e);
				this.menuViewVariables_Click(sender,e);
				Registry_Settings.Ignore_Updates = false;
				Undo_Stack.Clear_Undo(this);
				this.my_layout();
				this.flow_panel.Invalidate();
			}
			catch (System.Exception exc)
			{
				MessageBox.Show(exc.Message,"Can't compile");
			}
		}

		private void Visual_Flow_Form_Deactivate(object sender, System.EventArgs e)
		{
            Subchart sc = this.selectedTabMaybeNull();
            if (this.panel1 != null && sc!=null)
            {
                this.scroll_location = sc.AutoScrollPosition;
            }
		}

		private void Visual_Flow_Form_Activated(object sender, System.EventArgs e)
		{
            Subchart sc = this.selectedTabMaybeNull();
            if (sc!=null)
            {
                sc.Activated(sender, e);
            }
		}


		private void menuBreakpoint_Click(object sender, System.EventArgs e)
		{
			if (this.Breakpoint_Selection!=null &&
				this.Breakpoint_Selection.Name!="Oval")
			{
				this.Breakpoint_Selection.Toggle_Breakpoint(
					mouse_x,mouse_y);
				this.flow_panel.Invalidate();
			}
		}

		private void menuClearBreakpoints_Click(object sender, System.EventArgs e)
		{
            Oval Start;
            if (this.carlisle.SelectedTab is Subchart)
            {
                Start = (this.carlisle.SelectedTab as Subchart).Start;
            }
            else if (this.carlisle.SelectedTab is ClassTabPage)
            {
                if ((this.carlisle.SelectedTab as ClassTabPage).tabControl1.TabPages.Count > 0)
                {
                    Start = ((this.carlisle.SelectedTab as ClassTabPage).tabControl1.SelectedTab as Procedure_Chart).Start;
                }
                else
                {
                    return;
                }

            }
            else
            {
                return;
            }
            Start.Clear_Breakpoints();
            this.flow_panel.Invalidate();
		}
		public class Win32 
		{
			[DllImport("user32.dll")]
			public static extern bool OpenClipboard(IntPtr hWndNewOwner);
			[DllImport("user32.dll")]
			public static extern bool EmptyClipboard();
			[DllImport("user32.dll")]
			public static extern IntPtr SetClipboardData(uint uFormat, IntPtr hMem);
			[DllImport("user32.dll")]
			public static extern bool CloseClipboard();
			[DllImport("gdi32.dll")]
			public static extern IntPtr CopyEnhMetaFile(IntPtr hemfSrc, IntPtr hNULL);
			[DllImport("gdi32.dll")]
			public static extern bool DeleteEnhMetaFile(IntPtr hemf);
		}

		private void menuPrintClipboard_Click(object sender, System.EventArgs e)
		{
            Oval Start;
            if (this.carlisle.SelectedTab is Subchart)
            {
                Start = (this.carlisle.SelectedTab as Subchart).Start;
            }
            else if (this.carlisle.SelectedTab is ClassTabPage)
            {
                if ((this.carlisle.SelectedTab as ClassTabPage).tabControl1.TabPages.Count > 0)
                {
                    Start = ((this.carlisle.SelectedTab as ClassTabPage).tabControl1.SelectedTab as Procedure_Chart).Start;
                }
                else
                {
                    return;
                }

            }
            else
            {
                return;
            }
			Graphics g1,g_this;
			//Bitmap b1;

			if (Start != null)
			{
				g_this = this.CreateGraphics();
				Start.footprint(g_this); // calculate all sizes and needed x,y points
				x1 = Start.FP.left+flow_width;
				y1 = (int) Math.Round(scale*30);
				this.my_layout();
				
				//Metafile m1 = new Metafile(
				//  "bob.emf",
				//  this.CreateGraphics().GetHdc());
				Metafile m1 = new Metafile(
					g_this.GetHdc(),
					EmfType.EmfPlusDual);
				/*b1 = new Bitmap(
					flow_panel.Width,
					flow_panel.Height,
					g1);*/
				//g1 = Graphics.FromImage(b1);
				g1 = Graphics.FromImage(m1);

				g1.Clear(System.Drawing.Color.White);
				Start.draw(g1,x1,y1); // traverse the tree and draw
                Subchart sc = this.selectedTabMaybeNull();
                if (sc == null) return;
                if (sc.tab_overlay != null && sc.tab_overlay.Ink!=null)
                {
                    sc.tab_overlay.Renderer.Draw(g1,
                        sc.tab_overlay.Ink.Strokes);
                }

				// this dispose is needed to close the metafile
				g1.Dispose();
				//Clipboard.SetDataObject(b1,true);
				MessageBox.Show("Please open MS Word, or other " +
					"application you intend to paste into.\n" +
					"Otherwise it may not paste correctly.",
					"Paste to clipboard");
				
				// This was found at:
				// http://www.dotnet247.com/247reference/msgs/23/118514.aspx
				IntPtr hWnd=this.Handle;
				bool bResult = false;
				IntPtr hEMF, hEMF2;
				hEMF = m1.GetHenhmetafile(); // invalidates m1
				hEMF2 = Win32.CopyEnhMetaFile( hEMF, new IntPtr(0) );
				if( ! hEMF2.Equals( new IntPtr(0) ) )
				{
					if( Win32.OpenClipboard( hWnd ) )
					{
						if( Win32.EmptyClipboard() )
						{
							IntPtr hRes = Win32.SetClipboardData( 
								14 /*CF_ENHMETAFILE*/, hEMF2 );
							bResult = hRes.Equals( hEMF2 );
							Win32.CloseClipboard();
						}
					}
				}

			}		
		}


		public void menuCountSymbols_Click(object sender, System.EventArgs e)
		{
			int total = 0;
			int count = this.carlisle.TabCount;
            for (int i = this.mainIndex; i < count; i++)
			{
				int num;
				num = Runtime.Count_Symbols((Oval) ((Subchart) (this.carlisle.TabPages[i])).Start);
				Runtime.consoleWriteln(
					"The number of symbols in " +
					this.carlisle.TabPages[i].Text +
					" is: " +
					num);
				total += num;
			}
			if (count > 1)
			{
				Runtime.consoleWriteln(
					"The total number of symbols is: " + total);
			}
            this.MC.BringToFront();
        }

		private void menuSelectAll_Click(object sender, System.EventArgs e)
		{
            if (UML_Displayed())
            {
                (this.carlisle.SelectedTab.Controls[0] as UMLDiagram).mnuSelectAll_Click(sender, e);
            }
            else
            {
                Oval Start;
                if (this.carlisle.SelectedTab is Subchart)
                {
                    Start = (this.carlisle.SelectedTab as Subchart).Start;
                }
                else if (this.carlisle.SelectedTab is ClassTabPage)
                {
                    if ((this.carlisle.SelectedTab as ClassTabPage).tabControl1.TabPages.Count > 0)
                    {
                        Start = ((this.carlisle.SelectedTab as ClassTabPage).tabControl1.SelectedTab as Procedure_Chart).Start;
                    }
                    else
                    {
                        return;
                    }

                }
                else
                {
                    return;
                }
                Start.selectAll();
                this.flow_panel.Invalidate();
            }
		}

		private void menuExpandAll_Click(object sender, System.EventArgs e)
		{
            Oval Start;
            if (this.carlisle.SelectedTab is Subchart)
            {
                Start = (this.carlisle.SelectedTab as Subchart).Start;
            }
            else if (this.carlisle.SelectedTab is ClassTabPage)
            {
                if ((this.carlisle.SelectedTab as ClassTabPage).tabControl1.TabPages.Count > 0)
                {
                    Start = ((this.carlisle.SelectedTab as ClassTabPage).tabControl1.SelectedTab as Procedure_Chart).Start;
                }
                else
                {
                    return;
                }

            }
            else
            {
                return;
            }
			this.Make_Undoable();
			Start.change_compressed(false);
			this.flow_panel.Invalidate();
		}

		private void menuCollapseAll_Click(object sender, System.EventArgs e)
		{
            Oval Start;
            if (this.carlisle.SelectedTab is Subchart)
            {
                Start = (this.carlisle.SelectedTab as Subchart).Start;
            }
            else if (this.carlisle.SelectedTab is ClassTabPage)
            {
                if ((this.carlisle.SelectedTab as ClassTabPage).tabControl1.TabPages.Count > 0)
                {
                    Start = ((this.carlisle.SelectedTab as ClassTabPage).tabControl1.SelectedTab as Procedure_Chart).Start;
                }
                else
                {
                    return;
                }

            }
            else
            {
                return;
            }
			this.Make_Undoable();
			Start.change_compressed(true);
			this.flow_panel.Invalidate();
		}

		private void tabControl1_TabIndexChanged(object sender, System.EventArgs e)
		{
            Subchart sc = this.selectedTabMaybeNull();

            if (sc!=null)
            {
                sc.Activated(sender, e);
            }
		}
        private void tabControl1_MouseUp(object sender, System.Windows.Forms.MouseEventArgs e)
        {
            this.tab_moving = null;
            this.tab_moving_index = 0;
        }
        private void tabControl1_MouseMove(object sender, System.Windows.Forms.MouseEventArgs e)
        {
            if (this.tab_moving != null)
            {
                int index=0;
                for (int i = 0; i < this.carlisle.TabCount; i++)
                {
                    System.Drawing.Rectangle rect = this.carlisle.GetTabRect(i);
                    if (rect.IntersectsWith(new System.Drawing.Rectangle(e.X, e.Y, 1, 1)))
                    {
                        index = i;
                    }
                }
                if (index > 0 && index != this.tab_moving_index)
                {
                    this.carlisle.TabPages.Remove(this.tab_moving);
                    this.carlisle.TabPages.Insert(index, tab_moving);
                    this.tab_moving_index = index;
                    this.carlisle.SelectedIndex = index;
                    this.carlisle.Refresh();
                }
            }
        }
		private void tabControl1_MouseDown(object sender, System.Windows.Forms.MouseEventArgs e)
		{
			if (e.Button.ToString()=="Right")
			{
                if (Component.Current_Mode == Mode.Expert)
                {
                    return;
                }
				for (int i=0; i<this.carlisle.TabCount; i++)
				{
					System.Drawing.Rectangle rect = this.carlisle.GetTabRect(i);
					if (rect.IntersectsWith(new System.Drawing.Rectangle(e.X,e.Y,1,1)))
					{
						this.carlisle.SelectedIndex=i;
					}
				}
				if (this.carlisle.SelectedTab.Text=="main")
				{
					this.menuDeleteSubchart.Enabled=false;
					this.menuRenameSubchart.Enabled=false;
				}
				else
				{
					this.menuDeleteSubchart.Enabled=true;
					this.menuRenameSubchart.Enabled=true;
                    if (this.carlisle.SelectedTab is Procedure_Chart)
                    {
                        this.menuRenameSubchart.Text = "&Modify procedure";
                        this.menuDeleteSubchart.Text = "&Delete procedure";
                    }
                    else
                    {
                        this.menuRenameSubchart.Text = "&Rename subchart";
                        this.menuDeleteSubchart.Text = "&Delete subchart";
                    }
					for (int i=0; i<this.carlisle.TabCount; i++)
					{
						if (((Subchart) this.carlisle.TabPages[i]).Start.Called_Tab(this.carlisle.SelectedTab.Text))
						{
							this.menuDeleteSubchart.Enabled=false;
							break;
						}
					}
				}
				this.tabContextMenu.Show(this.carlisle,new Point(e.X,e.Y));
			}
            else if (e.Button == MouseButtons.Left)
            {
                for (int i = ((Component.Current_Mode==Mode.Expert) ? 2 : 1);
                    i < this.carlisle.TabCount; i++)
                {
                    System.Drawing.Rectangle rect = this.carlisle.GetTabRect(i);
                    if (rect.IntersectsWith(new System.Drawing.Rectangle(e.X, e.Y, 1, 1)))
                    {
                        this.tab_moving = this.carlisle.TabPages[i];
                        this.tab_moving_index = i;
                    }
                }
            }
		}

		private void menuAddSubchart_Click(object sender, System.EventArgs e)
		{
			Subchart sc;
			string name = Subchart_name.RunDialog("",this);
            if (name != null && name != "")
            {
                sc = new Subchart(this, name);
                this.carlisle.TabPages.Add(sc);
                this.carlisle.SelectedTab = sc;
                Undo_Stack.Make_Add_Tab_Undoable(this, sc);
            }
		}

		private void menuDeleteSubchart_Click(object sender, System.EventArgs e)
		{
			if (this.carlisle.SelectedTab!=this.mainSubchart())
			{
				Undo_Stack.Make_Delete_Tab_Undoable(this,(Subchart) this.carlisle.SelectedTab);
				this.carlisle.TabPages.Remove(this.carlisle.SelectedTab);
			}
		}

		public void Rename_Tab(string old_name, string name)
		{
			if (name.ToLower()!=old_name.ToLower())
			{
				for (int i=0; i<this.carlisle.TabCount; i++)
				{
					((Subchart) this.carlisle.TabPages[i]).Start.Rename_Tab(old_name,name);
				}
			}
		}

		public void menuRenameSubchart_Click(object sender, System.EventArgs e)
		{
            string old_name = this.carlisle.SelectedTab.Text;
            if (!(this.carlisle.SelectedTab is Procedure_Chart))
            {
                string name = Subchart_name.RunDialog(this.carlisle.SelectedTab.Text, this);
                if (name != null && name != "" && old_name != name)
                {
                    this.carlisle.SelectedTab.Text = name;
                    this.Rename_Tab(old_name, name);
                    Undo_Stack.Make_Rename_Tab_Undoable(this, (Subchart)this.carlisle.SelectedTab,
                        old_name, name);
                }
            }
            else
            {
                string name = ((Procedure_Chart) this.carlisle.SelectedTab).RunDialog(old_name, this);
                if (name != null && name != "" && old_name != name)
                {
                    this.carlisle.SelectedTab.Text = name;
                    this.Rename_Tab(old_name, name);
                    Undo_Stack.Make_Rename_Tab_Undoable(this, (Subchart)this.carlisle.SelectedTab,
                        old_name, name);
                }
            }
		}

		public bool Is_Subchart_Name(string s)
		{
			for (int i=0; i<this.carlisle.TabCount; i++)
			{
				if ((!(this.carlisle.TabPages[i] is ClassTabPage)) &&
                    (this.carlisle.TabPages[i].Text.ToLower()==s.Trim().ToLower()))
				{
					return true;
				}
			}
			return false;
		}
        public bool Is_Subchart_Call(string s)
        {
            int paren_loc = s.IndexOf("(");
            if (paren_loc > 0)
            {
                return Is_Subchart_Name(s.Substring(0, paren_loc));
            }
            else
            {
                return Is_Subchart_Name(s);
            }
        }
		private void menuItemRunCompiled_Click(object sender, System.EventArgs e)
		{
			// don't want a security hole of uncompiling flowcharts
			if (Component.compiled_flowchart)
			{
				this.menuResetExecute_Click(sender,e);
				return;
			}
			this.FileSave_Click(sender,e);
			this.menuReset_Click(sender,e);
			Component.run_compiled_flowchart = true;
            try
            {
                Compile_Helpers.Compile_Flowchart(this.carlisle.TabPages);
                try
                {
                    Compile_Helpers.Run_Compiled(false);
                }
                catch (System.Exception exc)
                {
                    System.Windows.Forms.MessageBox.Show("Flowchart terminated abnormally\n" +
                        exc.ToString());
                }
            }
            catch (System.Exception exc)
            {
                System.Windows.Forms.MessageBox.Show(exc.Message + "\n", "Compilation error", 
                    MessageBoxButtons.OK, MessageBoxIcon.Error);
            }


			Component.run_compiled_flowchart = false;
			this.rescale_all(this.scale);
		
		}

		private void ConfigServer_Click(object sender, System.EventArgs e)
		{
			SubmitServerForm form;
			form = new SubmitServerForm();
			form.ShowDialog();
		}

		private void menuRunServer_Click(object sender, System.EventArgs e)
		{
			if (fileName == "" || fileName == null) 
			{
				this.SaveAs_Click(sender,e);
			}
			if (fileName == "" || fileName == null) 
			{
				return;
			}
			SubmitServerForm.submit(Path.GetFileNameWithoutExtension(this.fileName));
        }

        private void contextMenuEdit_Click(object sender, EventArgs e)
        {
            this.menuItemEditSelection_Click(sender, e);
        }


        private void label2_DragOver(object sender, DragEventArgs e)
        {
            if (e.Data.GetDataPresent(DataFormats.Text))
            {
                string s = (string)e.Data.GetData(DataFormats.Text);
                if (s == "raptor_ASGN" || s == "raptor_CALL" ||
                    s == "raptor_INPUT" || s == "raptor_OUTPUT" ||
                    s == "raptor_SELECTION" || s == "raptor_LOOP" || 
                    s == "raptor_RETURN")
                {
                    e.Effect = DragDropEffects.Copy;
                }
                else
                {
                    e.Effect = DragDropEffects.None;
                }
            }
            else
            {
                e.Effect = DragDropEffects.None;
            }
        }

        private void menuGraphOnTop_Click(object sender, EventArgs e)
        {
            this.menuGraphOnTop.Checked = !this.menuGraphOnTop.Checked;
            dotnetgraphlibrary.dotnetgraph.start_topmost = this.menuGraphOnTop.Checked;
            if (this.menuGraphOnTop.Checked)
            {
                dotnetgraphlibrary.dotnetgraph.MakeTopMost();
            }
            else
            {
                dotnetgraphlibrary.dotnetgraph.MakeNonTopMost();
            }
            Registry_Settings.Write("RAPTORGraphOnTop", this.menuGraphOnTop.Checked.ToString());

        }

        private void menuProgramCompleteDialog_Click(object sender, EventArgs e)
        {
            this.menuProgramCompleteDialog.Checked = !this.menuProgramCompleteDialog.Checked;
            Registry_Settings.Write("ProgramCompleteDialog", this.menuProgramCompleteDialog.Checked.ToString());
        }

        private void Visual_Flow_Form_Move(object sender, EventArgs e)
        {
            this.Visual_Flow_Form_Resize(sender, e);
        }

        private void Visual_Flow_Form_Resize(object sender, EventArgs e)
        {
            string s;
            int i;
            if (!starting_up && (this.WindowState != FormWindowState.Maximized))
            {
                if (this.DesktopLocation.X >= 0)
                {
                    Registry_Settings.Write("FormX", this.DesktopLocation.X.ToString());
                }
                if (this.DesktopLocation.Y >= 0)
                {
                    Registry_Settings.Write("FormY", this.DesktopLocation.Y.ToString());
                }
                try
                {
                    s = Registry_Settings.Read("FormWidth");
                    i = Int32.Parse(s);
                }
                catch { i=0; }
                if (this.DesktopBounds.Width > min_width && Math.Abs(this.Width-i)>20)
                {
                    Registry_Settings.Write("FormWidth", this.Width.ToString());
                }
                try
                {
                    s = Registry_Settings.Read("FormHeight");
                    i = Int32.Parse(s);
                }
                catch { i = 0; }
                if (this.DesktopBounds.Height > min_height && Math.Abs(this.Height - i) > 20)
                {
                    Registry_Settings.Write("FormHeight", this.Height.ToString());
                }
            }

        }

        private void DefaultWindowSize_Click(object sender, EventArgs e)
        {
            System.Drawing.Size sz = SystemInformation.PrimaryMonitorMaximizedWindowSize;
            this.MC.Width = 368;
            this.MC.Height = 344;
            this.MC.Left = sz.Width - this.MC.Width;
            this.MC.Top = sz.Height - this.MC.Height - 20;
            this.SetDesktopBounds(100, 100, 722, 566);
        }
        private void menuObjectiveMode_Click(object sender, EventArgs e)
        {
            if (Component.Current_Mode != Mode.Expert)
            {
                if (!this.Save_Before_Losing(sender, e))
                {
                    return;
                }
                this.modified = false;
                if (this.carlisle.TabPages.Count > 0)
                {
                    this.new_clicked(sender, e);
                }
                TabPage tp = new TabPage("UML");
                if (carlisle.TabPages.Count == 0)
                {
                    carlisle.TabPages.Add(tp);
                }
                else
                {
                    carlisle.TabPages.Insert(0, tp);
                }
                tp.Controls.Add(
                    new UMLDiagram(this.umlupdater));
                control_panel.Invalidate();
            }
            Registry_Settings.Write("UserMode", "Expert");
            Component.Current_Mode = Mode.Expert;
            this.menuIntermediate.Checked = false;
            this.menuNovice.Checked = false;
            this.menuObjectiveMode.Checked = true;
        }
        private void menuNovice_Click(object sender, EventArgs e)
        {
            if (Component.Current_Mode == Mode.Expert)
            {
                if (!this.Save_Before_Losing(sender, e))
                {
                    return;
                }
                this.modified = false;
                this.new_clicked(sender, e);
                this.carlisle.TabPages.RemoveAt(0);
                control_panel.Invalidate();
            } 
            for (int i = 0; i < this.carlisle.TabCount; i++)
            {
                if (this.carlisle.TabPages[i] is Procedure_Chart)
                {
                    MessageBox.Show("Can't switch to novice mode, " +
                        this.carlisle.TabPages[i].Text + " is a procedure");
                    return;
                }
            }
            Registry_Settings.Write("UserMode", "Novice");
            Component.Current_Mode = Mode.Novice;
            this.menuIntermediate.Checked = false;
            this.menuNovice.Checked = true;
            this.menuObjectiveMode.Checked = false;
        }

        private void menuIntermediate_Click(object sender, EventArgs e)
        {
            if (Component.Current_Mode == Mode.Expert)
            {
                if (!this.Save_Before_Losing(sender, e))
                {
                    return;
                }
                this.modified = false;
                this.new_clicked(sender, e);
                this.carlisle.TabPages.RemoveAt(0);
                control_panel.Invalidate();
            } 
            Registry_Settings.Write("UserMode", "Intermediate");
            Component.Current_Mode = Mode.Intermediate;
            this.menuIntermediate.Checked = true;
            this.menuNovice.Checked = false;
            this.menuObjectiveMode.Checked = false;
        }

        private void tabContextMenu_Popup(object sender, EventArgs e)
        {
            if (Component.Current_Mode == Mode.Intermediate)
            {
                //this.menuAddFunction.Visible = true;
                this.menuAddProcedure.Visible = true;
            }
            else if (Component.Current_Mode == Mode.Novice)
            {
                this.menuAddProcedure.Visible = false;
                this.menuAddFunction.Visible = false;
            }
        }

        private void menuAddProcedure_Click(object sender, EventArgs e)
        {
            Procedure_Chart sc;
            string[] s = new string[0];
            bool[] is_input = null;
            bool[] is_output = null;
            string name = Procedure_name.RunDialog("", ref s, ref is_input, ref is_output, this);
            if (name != null && name != "")
            {
                sc = new Procedure_Chart(this, name, s, is_input, is_output);
                this.carlisle.TabPages.Add(sc);
                this.carlisle.SelectedTab = sc;
                Undo_Stack.Make_Add_Tab_Undoable(this, sc);
            }

        }

        private void menuGenerateStandalone_Click(object sender, EventArgs e)
        {
			this.mainSubchart().Start.selected = false;

			if (this.fileName==null||this.fileName=="")
			{
				MessageBox.Show("Must save before generating standalone",
					"Can't generate EXE");
				return;
			}
            try
            {
                string folder = System.IO.Path.GetDirectoryName(this.fileName) +
                    System.IO.Path.DirectorySeparatorChar +
                    System.IO.Path.GetFileNameWithoutExtension(this.fileName) +
                    "_exe";
                Compile_Helpers.Compile_Flowchart_To(
                    this.mainSubchart().Start,
                    folder,
                    System.IO.Path.GetFileNameWithoutExtension(this.fileName) +
                    "_rap.exe");
                MessageBox.Show("Result in folder: " + folder, "Generation complete");
            }
            catch (System.Exception exc)
            {
                MessageBox.Show("Generation failed: " + exc.Message);
            }
        }

        public void handle_click(object sender, EventArgs e)
        {
            MenuItem s = sender as MenuItem;
            if (this.fileName == null || this.fileName == "")
            {
                MessageBox.Show("Must save before generating code",
                    "Can't generate code");
                return;
            } 
            if (Component.compiled_flowchart)
            {
                MessageBox.Show("Cannot generate code from compiled flowchart", "Unable to generate");
                return;
            }
            else
            {
                try
                {
                    generate_interface.typ gi = Generators.Create_From_Menu(s.Text, this.fileName);
                    Compile_Helpers.Do_Compilation(this.mainSubchart().Start, gi, Runtime.parent.carlisle.TabPages);
                }
                catch (System.Exception exc)
                {
                    String message;
                    if (exc.InnerException != null)
                    {
                        message = exc.InnerException.Message;
                    }
                    else
                    {
                        message = exc.Message;
                    }
                    System.Windows.Forms.MessageBox.Show(message + "\n", "Compilation error",
                        MessageBoxButtons.OK, MessageBoxIcon.Error);
                }
            }
        }

        private Color Last_Ink_Color = Color.Green;

        private void Ink_Enable(Color c)
        {
                Last_Ink_Color = c;
                for (int i = 0; i < this.carlisle.TabCount; i++)
                {
                    ((Subchart)this.carlisle.TabPages[i]).tab_overlay.Enabled = true;
                    ((Subchart)this.carlisle.TabPages[i]).tab_overlay.EditingMode =
                        Microsoft.Ink.InkOverlayEditingMode.Ink;
                    ((Subchart)this.carlisle.TabPages[i]).tab_overlay.DefaultDrawingAttributes.Color =
                        c;
                }
        }

        private void Ink_Erase()
        {
            for (int i = 0; i < this.carlisle.TabCount; i++)
            {
                ((Subchart)this.carlisle.TabPages[i]).tab_overlay.Enabled = true;
                ((Subchart)this.carlisle.TabPages[i]).tab_overlay.EditingMode =
                    Microsoft.Ink.InkOverlayEditingMode.Delete;
            }
        }

        private void Ink_Disable()
        {
            for (int i = 0; i < this.carlisle.TabCount; i++)
            {
                ((Subchart)this.carlisle.TabPages[i]).tab_overlay.Enabled = false;
                ((Subchart)this.carlisle.TabPages[i]).tab_overlay.EditingMode =
                    Microsoft.Ink.InkOverlayEditingMode.Select;
            }
        }

        private void Ink_Select()
        {
            for (int i = 0; i < this.carlisle.TabCount; i++)
            {
                ((Subchart)this.carlisle.TabPages[i]).tab_overlay.Enabled = true;
                ((Subchart)this.carlisle.TabPages[i]).tab_overlay.EditingMode =
                    Microsoft.Ink.InkOverlayEditingMode.Select;
            }
        }
        private void menuItemInkBlack_Click(object sender, EventArgs e)
        {
            this.Make_Undoable();
            this.InkButton1.Pushed = true;
            this.menuItemInkBlue.Checked = false;
            this.menuItemInkErase.Checked = false;
            this.menuItemInkRed.Checked = false;
            this.menuItemInkBlack.Checked = true;
            this.menuItemInkOff.Checked = false;
            this.menuItemInkGreen.Checked = false;
            this.menuItemInkSelect.Checked = false;
            Ink_Enable(Color.Black);
        }

        private void menuItemInkOff_Click(object sender, EventArgs e)
        {
            this.InkButton1.Pushed = false;
            this.menuItemInkBlue.Checked = false;
            this.menuItemInkErase.Checked = false;
            this.menuItemInkRed.Checked = false;
            this.menuItemInkBlack.Checked = false;
            this.menuItemInkOff.Checked = true;
            this.menuItemInkGreen.Checked = false;
            this.menuItemInkSelect.Checked = false;
            Ink_Disable();
        }

        private void menuItemInkBlue_Click(object sender, EventArgs e)
        {
            this.Make_Undoable();
            this.InkButton1.Pushed = true;
            this.menuItemInkBlue.Checked = true;
            this.menuItemInkErase.Checked = false;
            this.menuItemInkRed.Checked = false;
            this.menuItemInkBlack.Checked = false;
            this.menuItemInkOff.Checked = false;
            this.menuItemInkGreen.Checked = false;
            this.menuItemInkSelect.Checked = false;
            Ink_Enable(Color.Blue);
        }

        private void menuItemInkRed_Click(object sender, EventArgs e)
        {
            this.Make_Undoable();
            this.InkButton1.Pushed = true;
            this.menuItemInkBlue.Checked = false;
            this.menuItemInkErase.Checked = false;
            this.menuItemInkRed.Checked = true;
            this.menuItemInkBlack.Checked = false;
            this.menuItemInkOff.Checked = false;
            this.menuItemInkGreen.Checked = false;
            this.menuItemInkSelect.Checked = false;
            Ink_Enable(Color.Red);
        }

        private void menuItemInkErase_Click(object sender, EventArgs e)
        {
            this.Make_Undoable();
            this.InkButton1.Pushed = true;
            this.menuItemInkBlue.Checked = false;
            this.menuItemInkErase.Checked = true;
            this.menuItemInkRed.Checked = false;
            this.menuItemInkBlack.Checked = false;
            this.menuItemInkGreen.Checked = false;
            this.menuItemInkOff.Checked = false;
            this.menuItemInkSelect.Checked = false;
            Ink_Erase();
        }

        private void menuItemInkGreen_Click(object sender, EventArgs e)
        {
            this.Make_Undoable();
            this.InkButton1.Pushed = true;
            this.menuItemInkBlue.Checked = false;
            this.menuItemInkErase.Checked = false;
            this.menuItemInkRed.Checked = false;
            this.menuItemInkGreen.Checked = true;
            this.menuItemInkBlack.Checked = false;
            this.menuItemInkOff.Checked = false;
            this.menuItemInkSelect.Checked = false;
            Ink_Enable(Color.Green);
        }

        private void menuItemInkSelect_Click(object sender, EventArgs e)
        {
            this.Make_Undoable();
            this.InkButton1.Pushed = true;
            this.menuItemInkBlue.Checked = false;
            this.menuItemInkErase.Checked = false;
            this.menuItemInkRed.Checked = false;
            this.menuItemInkGreen.Checked = false;
            this.menuItemInkBlack.Checked = false;
            this.menuItemInkOff.Checked = false;
            this.menuItemInkSelect.Checked = true;
            Ink_Select();
        }

        private void menuViewHardDrive_Click(object sender, EventArgs e)
        {
            BARTPEFileOpenList form = new BARTPEFileOpenList();
            form.Text = "Saved on Hard Drive";
            form.View_HD();
            form.ShowDialog();
        }

        private void contextMenuInsert_Popup(object sender, EventArgs e)
        {
            this.menuItemReturn.Visible = (Component.Current_Mode == Mode.Expert);
        }

        private void menuItemGenerate_Popup(object sender, EventArgs e)
        {
            foreach (MenuItem mi in this.menuItemGenerate.MenuItems)
            {
                if (mi == this.menuGenerateStandalone)
                {
                    mi.Enabled = (Component.Current_Mode!=Mode.Expert) &&
                        !Component.MONO;
                }
                else if (Component.Current_Mode == Mode.Expert)
                {
                    mi.Visible = Generators.Handles_OO(mi.Text);
                }
                else
                {
                    mi.Visible = Generators.Handles_Imperative(mi.Text);
                }
            }
        }

        private void menuRun_Popup(object sender, EventArgs e)
        {
            this.menuItemRunCompiled.Enabled = (Component.Current_Mode != Mode.Expert);
        }









		// To do list:
		// 1. Undo
		// 2. fix load/save for subchart calls
	}
}


