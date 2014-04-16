using System;
using NClass.Translations;

namespace NClass.GUI
{
	sealed partial class MainForm
	{
		/// <summary>
		/// Required designer variable.
		/// </summary>
		private System.ComponentModel.IContainer components = null;

		/// <summary>
		/// Clean up any resources being used.
		/// </summary>
		/// <param name="disposing">true if managed resources should be disposed; otherwise, false.</param>
		protected override void Dispose(bool disposing)
		{
			Strings.LanguageChanged -= new EventHandler(Strings_LanguageChanged);

			if (disposing && (components != null)) {
				components.Dispose();
			}
			base.Dispose(disposing);
		}

		#region Windows Form Designer generated code

		/// <summary>
		/// Required method for Designer support - do not modify
		/// the contents of this method with the code editor.
		/// </summary>
		private void InitializeComponent()
		{
            System.ComponentModel.ComponentResourceManager resources = new System.ComponentModel.ComponentResourceManager(typeof(MainForm));
            this.toolStripContainer = new System.Windows.Forms.ToolStripContainer();
            this.statusStrip = new System.Windows.Forms.StatusStrip();
            this.lblStatus = new System.Windows.Forms.ToolStripStatusLabel();
            this.lblLanguage = new System.Windows.Forms.ToolStripStatusLabel();
            this.menuStrip = new System.Windows.Forms.MenuStrip();
            this.mnuFile = new System.Windows.Forms.ToolStripMenuItem();
            this.mnuNew = new System.Windows.Forms.ToolStripMenuItem();
            this.mnuNewCSharpDiagram = new System.Windows.Forms.ToolStripMenuItem();
            this.mnuNewJavaDiagram = new System.Windows.Forms.ToolStripMenuItem();
            this.mnuOpen = new System.Windows.Forms.ToolStripMenuItem();
            this.mnuOpenFile = new System.Windows.Forms.ToolStripMenuItem();
            this.sepOpenFile = new System.Windows.Forms.ToolStripSeparator();
            this.mnuRecentFile1 = new System.Windows.Forms.ToolStripMenuItem();
            this.mnuRecentFile2 = new System.Windows.Forms.ToolStripMenuItem();
            this.mnuRecentFile3 = new System.Windows.Forms.ToolStripMenuItem();
            this.mnuRecentFile4 = new System.Windows.Forms.ToolStripMenuItem();
            this.mnuRecentFile5 = new System.Windows.Forms.ToolStripMenuItem();
            this.mnuSave = new System.Windows.Forms.ToolStripMenuItem();
            this.mnuSaveAs = new System.Windows.Forms.ToolStripMenuItem();
            this.mnuPrint = new System.Windows.Forms.ToolStripMenuItem();
            this.mnuSepPrint = new System.Windows.Forms.ToolStripSeparator();
            this.mnuImport = new System.Windows.Forms.ToolStripMenuItem();
            this.mnuExport = new System.Windows.Forms.ToolStripMenuItem();
            this.mnuGenerateCode = new System.Windows.Forms.ToolStripMenuItem();
            this.mnuSepExport = new System.Windows.Forms.ToolStripSeparator();
            this.mnuExit = new System.Windows.Forms.ToolStripMenuItem();
            this.mnuEdit = new System.Windows.Forms.ToolStripMenuItem();
            this.mnuUndo = new System.Windows.Forms.ToolStripMenuItem();
            this.mnuRedo = new System.Windows.Forms.ToolStripMenuItem();
            this.mnuSepReso = new System.Windows.Forms.ToolStripSeparator();
            this.mnuCut = new System.Windows.Forms.ToolStripMenuItem();
            this.mnuPaste = new System.Windows.Forms.ToolStripMenuItem();
            this.mnuDelete = new System.Windows.Forms.ToolStripMenuItem();
            this.mnuSepDelete = new System.Windows.Forms.ToolStripSeparator();
            this.mnuSelectAll = new System.Windows.Forms.ToolStripMenuItem();
            this.mnuDiagram = new System.Windows.Forms.ToolStripMenuItem();
            this.mnuAddNewElement = new System.Windows.Forms.ToolStripMenuItem();
            this.mnuNewClass = new System.Windows.Forms.ToolStripMenuItem();
            this.mnuNewStructure = new System.Windows.Forms.ToolStripMenuItem();
            this.mnuNewInterface = new System.Windows.Forms.ToolStripMenuItem();
            this.mnuNewEnum = new System.Windows.Forms.ToolStripMenuItem();
            this.mnuNewDelegate = new System.Windows.Forms.ToolStripMenuItem();
            this.mnuNewComment = new System.Windows.Forms.ToolStripMenuItem();
            this.mnuSepElement = new System.Windows.Forms.ToolStripSeparator();
            this.mnuNewAssociation = new System.Windows.Forms.ToolStripMenuItem();
            this.mnuNewComposition = new System.Windows.Forms.ToolStripMenuItem();
            this.mnuNewAggregation = new System.Windows.Forms.ToolStripMenuItem();
            this.mnuNewGeneralization = new System.Windows.Forms.ToolStripMenuItem();
            this.mnuNewRealization = new System.Windows.Forms.ToolStripMenuItem();
            this.mnuNewDependency = new System.Windows.Forms.ToolStripMenuItem();
            this.mnuNewNesting = new System.Windows.Forms.ToolStripMenuItem();
            this.mnuNewCommentRelation = new System.Windows.Forms.ToolStripMenuItem();
            this.mnuMembersFormat = new System.Windows.Forms.ToolStripMenuItem();
            this.mnuShowType = new System.Windows.Forms.ToolStripMenuItem();
            this.mnuShowParameters = new System.Windows.Forms.ToolStripMenuItem();
            this.mnuShowParameterNames = new System.Windows.Forms.ToolStripMenuItem();
            this.mnuShowInitialValue = new System.Windows.Forms.ToolStripMenuItem();
            this.mnuSepFormat = new System.Windows.Forms.ToolStripSeparator();
            this.mnuAutoZoom = new System.Windows.Forms.ToolStripMenuItem();
            this.mnuDiagramSize = new System.Windows.Forms.ToolStripMenuItem();
            this.mnuSaveAsImage = new System.Windows.Forms.ToolStripMenuItem();
            this.mnuSepAutoZoom = new System.Windows.Forms.ToolStripSeparator();
            this.mnuOptions = new System.Windows.Forms.ToolStripMenuItem();
            this.mnuFormat = new System.Windows.Forms.ToolStripMenuItem();
            this.mnuAlign = new System.Windows.Forms.ToolStripMenuItem();
            this.mnuAlignTop = new System.Windows.Forms.ToolStripMenuItem();
            this.mnuAlignLeft = new System.Windows.Forms.ToolStripMenuItem();
            this.mnuAlignBottom = new System.Windows.Forms.ToolStripMenuItem();
            this.mnuAlignRight = new System.Windows.Forms.ToolStripMenuItem();
            this.sepAlign = new System.Windows.Forms.ToolStripSeparator();
            this.mnuAlignHorizontal = new System.Windows.Forms.ToolStripMenuItem();
            this.mnuAlignVertical = new System.Windows.Forms.ToolStripMenuItem();
            this.mnuMakeSameSize = new System.Windows.Forms.ToolStripMenuItem();
            this.mnuSameWidth = new System.Windows.Forms.ToolStripMenuItem();
            this.mnuSameHeight = new System.Windows.Forms.ToolStripMenuItem();
            this.mnuSameSize = new System.Windows.Forms.ToolStripMenuItem();
            this.sepFormat = new System.Windows.Forms.ToolStripSeparator();
            this.mnuAutoWidth = new System.Windows.Forms.ToolStripMenuItem();
            this.mnuAutoHeight = new System.Windows.Forms.ToolStripMenuItem();
            this.mnuAutoLayout = new System.Windows.Forms.ToolStripMenuItem();
            this.sepAutoLayout = new System.Windows.Forms.ToolStripSeparator();
            this.mnuCollapseAll = new System.Windows.Forms.ToolStripMenuItem();
            this.mnuExpandAll = new System.Windows.Forms.ToolStripMenuItem();
            this.mnuPlugins = new System.Windows.Forms.ToolStripMenuItem();
            this.mnuHelp = new System.Windows.Forms.ToolStripMenuItem();
            this.mnuContents = new System.Windows.Forms.ToolStripMenuItem();
            this.mnuCheckForUpdates = new System.Windows.Forms.ToolStripMenuItem();
            this.mnuSepUpdates = new System.Windows.Forms.ToolStripSeparator();
            this.mnuAbout = new System.Windows.Forms.ToolStripMenuItem();
            this.standardToolStrip = new System.Windows.Forms.ToolStrip();
            this.toolNew = new System.Windows.Forms.ToolStripSplitButton();
            this.toolNewCSharpDiagram = new System.Windows.Forms.ToolStripMenuItem();
            this.toolNewJavaDiagram = new System.Windows.Forms.ToolStripMenuItem();
            this.toolOpen = new System.Windows.Forms.ToolStripSplitButton();
            this.toolRecentFile1 = new System.Windows.Forms.ToolStripMenuItem();
            this.toolRecentFile2 = new System.Windows.Forms.ToolStripMenuItem();
            this.toolRecentFile3 = new System.Windows.Forms.ToolStripMenuItem();
            this.toolRecentFile4 = new System.Windows.Forms.ToolStripMenuItem();
            this.toolRecentFile5 = new System.Windows.Forms.ToolStripMenuItem();
            this.toolSave = new System.Windows.Forms.ToolStripButton();
            this.toolPrint = new System.Windows.Forms.ToolStripButton();
            this.toolSepSave = new System.Windows.Forms.ToolStripSeparator();
            this.toolZoomValue = new System.Windows.Forms.ToolStripLabel();
            this.toolZoomOut = new System.Windows.Forms.ToolStripButton();
            this.toolZoom = new NClass.GUI.ZoomingToolStrip();
            this.toolZoomIn = new System.Windows.Forms.ToolStripButton();
            this.toolAutoZoom = new System.Windows.Forms.ToolStripButton();
            this.typeDetailsToolStrip = new System.Windows.Forms.ToolStrip();
            this.lblName = new System.Windows.Forms.ToolStripLabel();
            this.txtName = new System.Windows.Forms.ToolStripTextBox();
            this.lblAccess = new System.Windows.Forms.ToolStripLabel();
            this.cboAccess = new System.Windows.Forms.ToolStripComboBox();
            this.lblModifier = new System.Windows.Forms.ToolStripLabel();
            this.cboModifier = new System.Windows.Forms.ToolStripComboBox();
            this.lblReturnType = new System.Windows.Forms.ToolStripLabel();
            this.txtReturnType = new System.Windows.Forms.ToolStripTextBox();
            this.elementsToolStrip = new System.Windows.Forms.ToolStrip();
            this.toolNewClass = new System.Windows.Forms.ToolStripButton();
            this.toolNewStruct = new System.Windows.Forms.ToolStripButton();
            this.toolNewInterface = new System.Windows.Forms.ToolStripButton();
            this.toolNewEnum = new System.Windows.Forms.ToolStripButton();
            this.toolNewDelegate = new System.Windows.Forms.ToolStripButton();
            this.toolNewComment = new System.Windows.Forms.ToolStripButton();
            this.toolSepEntities = new System.Windows.Forms.ToolStripSeparator();
            this.toolNewAssociation = new System.Windows.Forms.ToolStripButton();
            this.toolNewComposition = new System.Windows.Forms.ToolStripButton();
            this.toolNewAggregation = new System.Windows.Forms.ToolStripButton();
            this.toolNewGeneralization = new System.Windows.Forms.ToolStripButton();
            this.toolNewRealization = new System.Windows.Forms.ToolStripButton();
            this.toolNewDependency = new System.Windows.Forms.ToolStripButton();
            this.toolNewNesting = new System.Windows.Forms.ToolStripButton();
            this.toolNewCommentRelation = new System.Windows.Forms.ToolStripButton();
            this.toolSepRelations = new System.Windows.Forms.ToolStripSeparator();
            this.toolDelete = new System.Windows.Forms.ToolStripButton();
            this.toolStripContainer.BottomToolStripPanel.SuspendLayout();
            this.toolStripContainer.TopToolStripPanel.SuspendLayout();
            this.toolStripContainer.SuspendLayout();
            this.statusStrip.SuspendLayout();
            this.menuStrip.SuspendLayout();
            this.standardToolStrip.SuspendLayout();
            this.typeDetailsToolStrip.SuspendLayout();
            this.elementsToolStrip.SuspendLayout();
            this.SuspendLayout();
            // 
            // toolStripContainer
            // 
            // 
            // toolStripContainer.BottomToolStripPanel
            // 
            this.toolStripContainer.BottomToolStripPanel.Controls.Add(this.statusStrip);
            // 
            // toolStripContainer.ContentPanel
            // 
            this.toolStripContainer.ContentPanel.BackColor = System.Drawing.Color.FromArgb(((int)(((byte)(209)))), ((int)(((byte)(221)))), ((int)(((byte)(226)))));
            this.toolStripContainer.ContentPanel.ForeColor = System.Drawing.SystemColors.ControlText;
            this.toolStripContainer.ContentPanel.Padding = new System.Windows.Forms.Padding(1);
            this.toolStripContainer.ContentPanel.Size = new System.Drawing.Size(762, 395);
            this.toolStripContainer.Dock = System.Windows.Forms.DockStyle.Fill;
            this.toolStripContainer.Location = new System.Drawing.Point(0, 0);
            this.toolStripContainer.Name = "toolStripContainer";
            this.toolStripContainer.Size = new System.Drawing.Size(762, 516);
            this.toolStripContainer.TabIndex = 0;
            this.toolStripContainer.Text = "toolStripContainer1";
            // 
            // toolStripContainer.TopToolStripPanel
            // 
            this.toolStripContainer.TopToolStripPanel.Controls.Add(this.menuStrip);
            this.toolStripContainer.TopToolStripPanel.Controls.Add(this.standardToolStrip);
            this.toolStripContainer.TopToolStripPanel.Controls.Add(this.typeDetailsToolStrip);
            this.toolStripContainer.TopToolStripPanel.Controls.Add(this.elementsToolStrip);
            // 
            // statusStrip
            // 
            this.statusStrip.Dock = System.Windows.Forms.DockStyle.None;
            this.statusStrip.GripStyle = System.Windows.Forms.ToolStripGripStyle.Visible;
            this.statusStrip.Items.AddRange(new System.Windows.Forms.ToolStripItem[] {
            this.lblStatus,
            this.lblLanguage});
            this.statusStrip.Location = new System.Drawing.Point(0, 0);
            this.statusStrip.Name = "statusStrip";
            this.statusStrip.RenderMode = System.Windows.Forms.ToolStripRenderMode.ManagerRenderMode;
            this.statusStrip.Size = new System.Drawing.Size(762, 22);
            this.statusStrip.TabIndex = 0;
            // 
            // lblStatus
            // 
            this.lblStatus.Name = "lblStatus";
            this.lblStatus.Size = new System.Drawing.Size(693, 17);
            this.lblStatus.Spring = true;
            this.lblStatus.Text = "Status";
            this.lblStatus.TextAlign = System.Drawing.ContentAlignment.MiddleLeft;
            // 
            // lblLanguage
            // 
            this.lblLanguage.Enabled = false;
            this.lblLanguage.Name = "lblLanguage";
            this.lblLanguage.Size = new System.Drawing.Size(54, 17);
            this.lblLanguage.Text = "Language";
            this.lblLanguage.TextAlign = System.Drawing.ContentAlignment.MiddleRight;
            // 
            // menuStrip
            // 
            this.menuStrip.Dock = System.Windows.Forms.DockStyle.None;
            this.menuStrip.Items.AddRange(new System.Windows.Forms.ToolStripItem[] {
            this.mnuFile,
            this.mnuEdit,
            this.mnuDiagram,
            this.mnuFormat,
            this.mnuPlugins,
            this.mnuHelp});
            this.menuStrip.Location = new System.Drawing.Point(0, 0);
            this.menuStrip.Name = "menuStrip";
            this.menuStrip.Size = new System.Drawing.Size(762, 24);
            this.menuStrip.TabIndex = 0;
            this.menuStrip.Text = "menuStrip1";
            // 
            // mnuFile
            // 
            this.mnuFile.DropDownItems.AddRange(new System.Windows.Forms.ToolStripItem[] {
            this.mnuNew,
            this.mnuOpen,
            this.mnuSave,
            this.mnuSaveAs,
            this.mnuPrint,
            this.mnuSepPrint,
            this.mnuImport,
            this.mnuExport,
            this.mnuSepExport,
            this.mnuExit});
            this.mnuFile.Name = "mnuFile";
            this.mnuFile.Size = new System.Drawing.Size(35, 20);
            this.mnuFile.Text = "&File";
            // 
            // mnuNew
            // 
            this.mnuNew.DropDownItems.AddRange(new System.Windows.Forms.ToolStripItem[] {
            this.mnuNewCSharpDiagram,
            this.mnuNewJavaDiagram});
            this.mnuNew.Image = global::NClass.GUI.Properties.Resources.NewDocument;
            this.mnuNew.Name = "mnuNew";
            this.mnuNew.Size = new System.Drawing.Size(157, 22);
            this.mnuNew.Text = "&New";
            // 
            // mnuNewCSharpDiagram
            // 
            this.mnuNewCSharpDiagram.Name = "mnuNewCSharpDiagram";
            this.mnuNewCSharpDiagram.Size = new System.Drawing.Size(149, 22);
            this.mnuNewCSharpDiagram.Text = "&C# diagram";
            this.mnuNewCSharpDiagram.Click += new System.EventHandler(this.mnuNewCSharpDiagram_Click);
            // 
            // mnuNewJavaDiagram
            // 
            this.mnuNewJavaDiagram.Name = "mnuNewJavaDiagram";
            this.mnuNewJavaDiagram.Size = new System.Drawing.Size(149, 22);
            this.mnuNewJavaDiagram.Text = "&Java diagram";
            this.mnuNewJavaDiagram.Click += new System.EventHandler(this.mnuNewJavaDiagram_Click);
            // 
            // mnuOpen
            // 
            this.mnuOpen.DropDownItems.AddRange(new System.Windows.Forms.ToolStripItem[] {
            this.mnuOpenFile,
            this.sepOpenFile,
            this.mnuRecentFile1,
            this.mnuRecentFile2,
            this.mnuRecentFile3,
            this.mnuRecentFile4,
            this.mnuRecentFile5});
            this.mnuOpen.Image = global::NClass.GUI.Properties.Resources.Open;
            this.mnuOpen.Name = "mnuOpen";
            this.mnuOpen.Size = new System.Drawing.Size(157, 22);
            this.mnuOpen.Text = "&Open";
            this.mnuOpen.DropDownOpening += new System.EventHandler(this.mnuOpen_DropDownOpening);
            // 
            // mnuOpenFile
            // 
            this.mnuOpenFile.Name = "mnuOpenFile";
            this.mnuOpenFile.ShortcutKeys = ((System.Windows.Forms.Keys)((System.Windows.Forms.Keys.Control | System.Windows.Forms.Keys.O)));
            this.mnuOpenFile.Size = new System.Drawing.Size(177, 22);
            this.mnuOpenFile.Text = "&New File...";
            this.mnuOpenFile.Click += new System.EventHandler(this.mnuOpenFile_Click);
            // 
            // sepOpenFile
            // 
            this.sepOpenFile.Name = "sepOpenFile";
            this.sepOpenFile.Size = new System.Drawing.Size(174, 6);
            // 
            // mnuRecentFile1
            // 
            this.mnuRecentFile1.Name = "mnuRecentFile1";
            this.mnuRecentFile1.Size = new System.Drawing.Size(177, 22);
            this.mnuRecentFile1.Tag = 0;
            this.mnuRecentFile1.Text = "Recent file 1";
            this.mnuRecentFile1.Click += new System.EventHandler(this.OpenRecentFile_Click);
            // 
            // mnuRecentFile2
            // 
            this.mnuRecentFile2.Name = "mnuRecentFile2";
            this.mnuRecentFile2.Size = new System.Drawing.Size(177, 22);
            this.mnuRecentFile2.Tag = 1;
            this.mnuRecentFile2.Text = "Recent file 2";
            this.mnuRecentFile2.Click += new System.EventHandler(this.OpenRecentFile_Click);
            // 
            // mnuRecentFile3
            // 
            this.mnuRecentFile3.Name = "mnuRecentFile3";
            this.mnuRecentFile3.Size = new System.Drawing.Size(177, 22);
            this.mnuRecentFile3.Tag = 2;
            this.mnuRecentFile3.Text = "Recent file 3";
            this.mnuRecentFile3.Click += new System.EventHandler(this.OpenRecentFile_Click);
            // 
            // mnuRecentFile4
            // 
            this.mnuRecentFile4.Name = "mnuRecentFile4";
            this.mnuRecentFile4.Size = new System.Drawing.Size(177, 22);
            this.mnuRecentFile4.Tag = 3;
            this.mnuRecentFile4.Text = "Recent file 4";
            this.mnuRecentFile4.Click += new System.EventHandler(this.OpenRecentFile_Click);
            // 
            // mnuRecentFile5
            // 
            this.mnuRecentFile5.Name = "mnuRecentFile5";
            this.mnuRecentFile5.Size = new System.Drawing.Size(177, 22);
            this.mnuRecentFile5.Tag = 4;
            this.mnuRecentFile5.Text = "Recent file 5";
            this.mnuRecentFile5.Click += new System.EventHandler(this.OpenRecentFile_Click);
            // 
            // mnuSave
            // 
            this.mnuSave.Image = global::NClass.GUI.Properties.Resources.Save;
            this.mnuSave.Name = "mnuSave";
            this.mnuSave.ShortcutKeys = ((System.Windows.Forms.Keys)((System.Windows.Forms.Keys.Control | System.Windows.Forms.Keys.S)));
            this.mnuSave.Size = new System.Drawing.Size(157, 22);
            this.mnuSave.Text = "&Save";
            this.mnuSave.Click += new System.EventHandler(this.mnuSave_Click);
            // 
            // mnuSaveAs
            // 
            this.mnuSaveAs.Name = "mnuSaveAs";
            this.mnuSaveAs.Size = new System.Drawing.Size(157, 22);
            this.mnuSaveAs.Text = "Save &As...";
            this.mnuSaveAs.Click += new System.EventHandler(this.mnuSaveAs_Click);
            // 
            // mnuPrint
            // 
            this.mnuPrint.Image = global::NClass.GUI.Properties.Resources.Print;
            this.mnuPrint.Name = "mnuPrint";
            this.mnuPrint.ShortcutKeys = ((System.Windows.Forms.Keys)((System.Windows.Forms.Keys.Control | System.Windows.Forms.Keys.P)));
            this.mnuPrint.Size = new System.Drawing.Size(157, 22);
            this.mnuPrint.Text = "&Print...";
            this.mnuPrint.Click += new System.EventHandler(this.mnuPrint_Click);
            // 
            // mnuSepPrint
            // 
            this.mnuSepPrint.Name = "mnuSepPrint";
            this.mnuSepPrint.Size = new System.Drawing.Size(154, 6);
            // 
            // mnuImport
            // 
            this.mnuImport.Name = "mnuImport";
            this.mnuImport.Size = new System.Drawing.Size(157, 22);
            this.mnuImport.Text = "&Import";
            this.mnuImport.Visible = false;
            // 
            // mnuExport
            // 
            this.mnuExport.DropDownItems.AddRange(new System.Windows.Forms.ToolStripItem[] {
            this.mnuGenerateCode});
            this.mnuExport.Name = "mnuExport";
            this.mnuExport.Size = new System.Drawing.Size(157, 22);
            this.mnuExport.Text = "&Export";
            this.mnuExport.DropDownOpening += new System.EventHandler(this.mnuExport_DropDownOpening);
            // 
            // mnuGenerateCode
            // 
            this.mnuGenerateCode.Image = global::NClass.GUI.Properties.Resources.CodeGenerator;
            this.mnuGenerateCode.Name = "mnuGenerateCode";
            this.mnuGenerateCode.Size = new System.Drawing.Size(170, 22);
            this.mnuGenerateCode.Text = "&Generate Code...";
            this.mnuGenerateCode.Click += new System.EventHandler(this.mnuGenerateCode_Click);
            // 
            // mnuSepExport
            // 
            this.mnuSepExport.Name = "mnuSepExport";
            this.mnuSepExport.Size = new System.Drawing.Size(154, 6);
            // 
            // mnuExit
            // 
            this.mnuExit.Name = "mnuExit";
            this.mnuExit.Size = new System.Drawing.Size(157, 22);
            this.mnuExit.Text = "E&xit";
            this.mnuExit.Click += new System.EventHandler(this.mnuExit_Click);
            // 
            // mnuEdit
            // 
            this.mnuEdit.DropDownItems.AddRange(new System.Windows.Forms.ToolStripItem[] {
            this.mnuUndo,
            this.mnuRedo,
            this.mnuSepReso,
            this.mnuCut,
            this.mnuPaste,
            this.mnuDelete,
            this.mnuSepDelete,
            this.mnuSelectAll});
            this.mnuEdit.Name = "mnuEdit";
            this.mnuEdit.Size = new System.Drawing.Size(37, 20);
            this.mnuEdit.Text = "&Edit";
            this.mnuEdit.DropDownOpening += new System.EventHandler(this.mnuEdit_DropDownOpening);
            this.mnuEdit.DropDownClosed += new System.EventHandler(this.mnuEdit_DropDownClosed);
            // 
            // mnuUndo
            // 
            this.mnuUndo.Image = global::NClass.GUI.Properties.Resources.Undo;
            this.mnuUndo.Name = "mnuUndo";
            this.mnuUndo.ShortcutKeys = ((System.Windows.Forms.Keys)((System.Windows.Forms.Keys.Control | System.Windows.Forms.Keys.Z)));
            this.mnuUndo.Size = new System.Drawing.Size(167, 22);
            this.mnuUndo.Text = "&Undo";
            this.mnuUndo.Visible = false;
            this.mnuUndo.Click += new System.EventHandler(this.mnuUndo_Click);
            // 
            // mnuRedo
            // 
            this.mnuRedo.Image = global::NClass.GUI.Properties.Resources.Redo;
            this.mnuRedo.Name = "mnuRedo";
            this.mnuRedo.ShortcutKeys = ((System.Windows.Forms.Keys)((System.Windows.Forms.Keys.Control | System.Windows.Forms.Keys.Y)));
            this.mnuRedo.Size = new System.Drawing.Size(167, 22);
            this.mnuRedo.Text = "&Redo";
            this.mnuRedo.Visible = false;
            this.mnuRedo.Click += new System.EventHandler(this.mnuRedo_Click);
            // 
            // mnuSepReso
            // 
            this.mnuSepReso.Name = "mnuSepReso";
            this.mnuSepReso.Size = new System.Drawing.Size(164, 6);
            this.mnuSepReso.Visible = false;
            // 
            // mnuCut
            // 
            this.mnuCut.Image = global::NClass.GUI.Properties.Resources.Cut;
            this.mnuCut.Name = "mnuCut";
            this.mnuCut.ShortcutKeys = ((System.Windows.Forms.Keys)((System.Windows.Forms.Keys.Control | System.Windows.Forms.Keys.X)));
            this.mnuCut.Size = new System.Drawing.Size(167, 22);
            this.mnuCut.Text = "Cu&t";
            this.mnuCut.Click += new System.EventHandler(this.mnuCut_Click);
            // 
            // mnuPaste
            // 
            this.mnuPaste.Image = global::NClass.GUI.Properties.Resources.Paste;
            this.mnuPaste.Name = "mnuPaste";
            this.mnuPaste.ShortcutKeys = ((System.Windows.Forms.Keys)((System.Windows.Forms.Keys.Control | System.Windows.Forms.Keys.V)));
            this.mnuPaste.Size = new System.Drawing.Size(167, 22);
            this.mnuPaste.Text = "&Paste";
            this.mnuPaste.Click += new System.EventHandler(this.mnuPaste_Click);
            // 
            // mnuDelete
            // 
            this.mnuDelete.Image = global::NClass.GUI.Properties.Resources.Delete;
            this.mnuDelete.Name = "mnuDelete";
            this.mnuDelete.ShortcutKeyDisplayString = "Delete";
            this.mnuDelete.Size = new System.Drawing.Size(167, 22);
            this.mnuDelete.Text = "&Delete";
            this.mnuDelete.Click += new System.EventHandler(this.mnuDelete_Click);
            // 
            // mnuSepDelete
            // 
            this.mnuSepDelete.Name = "mnuSepDelete";
            this.mnuSepDelete.Size = new System.Drawing.Size(164, 6);
            // 
            // mnuSelectAll
            // 
            this.mnuSelectAll.Name = "mnuSelectAll";
            this.mnuSelectAll.ShortcutKeyDisplayString = "";
            this.mnuSelectAll.ShortcutKeys = ((System.Windows.Forms.Keys)((System.Windows.Forms.Keys.Control | System.Windows.Forms.Keys.A)));
            this.mnuSelectAll.Size = new System.Drawing.Size(167, 22);
            this.mnuSelectAll.Text = "Select &All";
            this.mnuSelectAll.Click += new System.EventHandler(this.mnuSelectAll_Click);
            // 
            // mnuDiagram
            // 
            this.mnuDiagram.DropDownItems.AddRange(new System.Windows.Forms.ToolStripItem[] {
            this.mnuAddNewElement,
            this.mnuMembersFormat,
            this.mnuSepFormat,
            this.mnuAutoZoom,
            this.mnuDiagramSize,
            this.mnuSaveAsImage,
            this.mnuSepAutoZoom,
            this.mnuOptions});
            this.mnuDiagram.Name = "mnuDiagram";
            this.mnuDiagram.Size = new System.Drawing.Size(58, 20);
            this.mnuDiagram.Text = "&Diagram";
            this.mnuDiagram.DropDownOpening += new System.EventHandler(this.mnuDiagram_DropDownOpening);
            // 
            // mnuAddNewElement
            // 
            this.mnuAddNewElement.DropDownItems.AddRange(new System.Windows.Forms.ToolStripItem[] {
            this.mnuNewClass,
            this.mnuNewStructure,
            this.mnuNewInterface,
            this.mnuNewEnum,
            this.mnuNewDelegate,
            this.mnuNewComment,
            this.mnuSepElement,
            this.mnuNewAssociation,
            this.mnuNewComposition,
            this.mnuNewAggregation,
            this.mnuNewGeneralization,
            this.mnuNewRealization,
            this.mnuNewDependency,
            this.mnuNewNesting,
            this.mnuNewCommentRelation});
            this.mnuAddNewElement.Image = global::NClass.GUI.Properties.Resources.NewEntity;
            this.mnuAddNewElement.Name = "mnuAddNewElement";
            this.mnuAddNewElement.Size = new System.Drawing.Size(169, 22);
            this.mnuAddNewElement.Text = "&Add New";
            // 
            // mnuNewClass
            // 
            this.mnuNewClass.Image = global::NClass.GUI.Properties.Resources.Class;
            this.mnuNewClass.Name = "mnuNewClass";
            this.mnuNewClass.Size = new System.Drawing.Size(172, 22);
            this.mnuNewClass.Text = "&Class";
            this.mnuNewClass.Click += new System.EventHandler(this.mnuNewClass_Click);
            // 
            // mnuNewStructure
            // 
            this.mnuNewStructure.Image = global::NClass.GUI.Properties.Resources.Struct;
            this.mnuNewStructure.Name = "mnuNewStructure";
            this.mnuNewStructure.Size = new System.Drawing.Size(172, 22);
            this.mnuNewStructure.Text = "&Structure";
            this.mnuNewStructure.Click += new System.EventHandler(this.mnuNewStructure_Click);
            // 
            // mnuNewInterface
            // 
            this.mnuNewInterface.Image = global::NClass.GUI.Properties.Resources.Interface;
            this.mnuNewInterface.Name = "mnuNewInterface";
            this.mnuNewInterface.Size = new System.Drawing.Size(172, 22);
            this.mnuNewInterface.Text = "&Interface";
            this.mnuNewInterface.Click += new System.EventHandler(this.mnuNewInterface_Click);
            // 
            // mnuNewEnum
            // 
            this.mnuNewEnum.Image = global::NClass.GUI.Properties.Resources.Enum;
            this.mnuNewEnum.Name = "mnuNewEnum";
            this.mnuNewEnum.Size = new System.Drawing.Size(172, 22);
            this.mnuNewEnum.Text = "&Enum";
            this.mnuNewEnum.Click += new System.EventHandler(this.mnuNewEnum_Click);
            // 
            // mnuNewDelegate
            // 
            this.mnuNewDelegate.Image = global::NClass.GUI.Properties.Resources.Delegate;
            this.mnuNewDelegate.Name = "mnuNewDelegate";
            this.mnuNewDelegate.Size = new System.Drawing.Size(172, 22);
            this.mnuNewDelegate.Text = "&Delegate";
            this.mnuNewDelegate.Click += new System.EventHandler(this.mnuNewDelegate_Click);
            // 
            // mnuNewComment
            // 
            this.mnuNewComment.Image = global::NClass.GUI.Properties.Resources.Note;
            this.mnuNewComment.Name = "mnuNewComment";
            this.mnuNewComment.Size = new System.Drawing.Size(172, 22);
            this.mnuNewComment.Text = "Commen&t";
            this.mnuNewComment.Click += new System.EventHandler(this.mnuNewComment_Click);
            // 
            // mnuSepElement
            // 
            this.mnuSepElement.Name = "mnuSepElement";
            this.mnuSepElement.Size = new System.Drawing.Size(169, 6);
            // 
            // mnuNewAssociation
            // 
            this.mnuNewAssociation.Image = global::NClass.GUI.Properties.Resources.Association;
            this.mnuNewAssociation.Name = "mnuNewAssociation";
            this.mnuNewAssociation.Size = new System.Drawing.Size(172, 22);
            this.mnuNewAssociation.Text = "&Association";
            this.mnuNewAssociation.Click += new System.EventHandler(this.mnuNewAssociation_Click);
            // 
            // mnuNewComposition
            // 
            this.mnuNewComposition.Image = global::NClass.GUI.Properties.Resources.Composition;
            this.mnuNewComposition.Name = "mnuNewComposition";
            this.mnuNewComposition.Size = new System.Drawing.Size(172, 22);
            this.mnuNewComposition.Text = "C&omposition";
            this.mnuNewComposition.Click += new System.EventHandler(this.mnuNewComposition_Click);
            // 
            // mnuNewAggregation
            // 
            this.mnuNewAggregation.Image = global::NClass.GUI.Properties.Resources.Aggregation;
            this.mnuNewAggregation.Name = "mnuNewAggregation";
            this.mnuNewAggregation.Size = new System.Drawing.Size(172, 22);
            this.mnuNewAggregation.Text = "A&ggregation";
            this.mnuNewAggregation.Click += new System.EventHandler(this.mnuNewAggregation_Click);
            // 
            // mnuNewGeneralization
            // 
            this.mnuNewGeneralization.Image = global::NClass.GUI.Properties.Resources.Generalization;
            this.mnuNewGeneralization.Name = "mnuNewGeneralization";
            this.mnuNewGeneralization.Size = new System.Drawing.Size(172, 22);
            this.mnuNewGeneralization.Text = "Genera&lization";
            this.mnuNewGeneralization.Click += new System.EventHandler(this.mnuNewGeneralization_Click);
            // 
            // mnuNewRealization
            // 
            this.mnuNewRealization.Image = global::NClass.GUI.Properties.Resources.Realization;
            this.mnuNewRealization.Name = "mnuNewRealization";
            this.mnuNewRealization.Size = new System.Drawing.Size(172, 22);
            this.mnuNewRealization.Text = "&Realization";
            this.mnuNewRealization.Click += new System.EventHandler(this.mnuNewRealization_Click);
            // 
            // mnuNewDependency
            // 
            this.mnuNewDependency.Image = global::NClass.GUI.Properties.Resources.Dependency;
            this.mnuNewDependency.Name = "mnuNewDependency";
            this.mnuNewDependency.Size = new System.Drawing.Size(172, 22);
            this.mnuNewDependency.Text = "&Dependency";
            this.mnuNewDependency.Click += new System.EventHandler(this.mnuNewDependency_Click);
            // 
            // mnuNewNesting
            // 
            this.mnuNewNesting.Image = global::NClass.GUI.Properties.Resources.Nesting;
            this.mnuNewNesting.Name = "mnuNewNesting";
            this.mnuNewNesting.Size = new System.Drawing.Size(172, 22);
            this.mnuNewNesting.Text = "&Nesting";
            this.mnuNewNesting.Click += new System.EventHandler(this.mnuNewNesting_Click);
            // 
            // mnuNewCommentRelation
            // 
            this.mnuNewCommentRelation.Image = global::NClass.GUI.Properties.Resources.Comment;
            this.mnuNewCommentRelation.Name = "mnuNewCommentRelation";
            this.mnuNewCommentRelation.Size = new System.Drawing.Size(172, 22);
            this.mnuNewCommentRelation.Text = "Co&mment Relation";
            this.mnuNewCommentRelation.Click += new System.EventHandler(this.mnuNewCommentRelation_Click);
            // 
            // mnuMembersFormat
            // 
            this.mnuMembersFormat.DropDownItems.AddRange(new System.Windows.Forms.ToolStripItem[] {
            this.mnuShowType,
            this.mnuShowParameters,
            this.mnuShowParameterNames,
            this.mnuShowInitialValue});
            this.mnuMembersFormat.Image = global::NClass.GUI.Properties.Resources.Format;
            this.mnuMembersFormat.Name = "mnuMembersFormat";
            this.mnuMembersFormat.Size = new System.Drawing.Size(169, 22);
            this.mnuMembersFormat.Text = "&Member\'s Format";
            this.mnuMembersFormat.DropDownOpening += new System.EventHandler(this.mnuMembersFormat_DropDownOpening);
            // 
            // mnuShowType
            // 
            this.mnuShowType.CheckOnClick = true;
            this.mnuShowType.Name = "mnuShowType";
            this.mnuShowType.Size = new System.Drawing.Size(170, 22);
            this.mnuShowType.Text = "&Type";
            this.mnuShowType.CheckedChanged += new System.EventHandler(this.mnuShowType_Click);
            // 
            // mnuShowParameters
            // 
            this.mnuShowParameters.CheckOnClick = true;
            this.mnuShowParameters.Name = "mnuShowParameters";
            this.mnuShowParameters.Size = new System.Drawing.Size(170, 22);
            this.mnuShowParameters.Text = "&Parameters";
            this.mnuShowParameters.CheckedChanged += new System.EventHandler(this.mnuShowParameters_Click);
            // 
            // mnuShowParameterNames
            // 
            this.mnuShowParameterNames.CheckOnClick = true;
            this.mnuShowParameterNames.Name = "mnuShowParameterNames";
            this.mnuShowParameterNames.Size = new System.Drawing.Size(170, 22);
            this.mnuShowParameterNames.Text = "Parameter &Names";
            this.mnuShowParameterNames.CheckedChanged += new System.EventHandler(this.mnuShowParameterNames_Click);
            // 
            // mnuShowInitialValue
            // 
            this.mnuShowInitialValue.CheckOnClick = true;
            this.mnuShowInitialValue.Name = "mnuShowInitialValue";
            this.mnuShowInitialValue.Size = new System.Drawing.Size(170, 22);
            this.mnuShowInitialValue.Text = "&Initial Value";
            this.mnuShowInitialValue.CheckedChanged += new System.EventHandler(this.mnuShowInitialValue_Click);
            // 
            // mnuSepFormat
            // 
            this.mnuSepFormat.Name = "mnuSepFormat";
            this.mnuSepFormat.Size = new System.Drawing.Size(166, 6);
            // 
            // mnuAutoZoom
            // 
            this.mnuAutoZoom.Image = global::NClass.GUI.Properties.Resources.AutoZoom;
            this.mnuAutoZoom.Name = "mnuAutoZoom";
            this.mnuAutoZoom.Size = new System.Drawing.Size(169, 22);
            this.mnuAutoZoom.Text = "Auto &Zoom";
            this.mnuAutoZoom.Click += new System.EventHandler(this.mnuAutoZoom_Click);
            // 
            // mnuDiagramSize
            // 
            this.mnuDiagramSize.Image = global::NClass.GUI.Properties.Resources.DiagramSize;
            this.mnuDiagramSize.Name = "mnuDiagramSize";
            this.mnuDiagramSize.Size = new System.Drawing.Size(169, 22);
            this.mnuDiagramSize.Text = "&Diagram Size...";
            this.mnuDiagramSize.Click += new System.EventHandler(this.mnuDiagramSize_Click);
            // 
            // mnuSaveAsImage
            // 
            this.mnuSaveAsImage.Image = global::NClass.GUI.Properties.Resources.Image;
            this.mnuSaveAsImage.Name = "mnuSaveAsImage";
            this.mnuSaveAsImage.Size = new System.Drawing.Size(169, 22);
            this.mnuSaveAsImage.Text = "&Save As Image...";
            this.mnuSaveAsImage.Click += new System.EventHandler(this.mnuSaveAsImage_Click);
            // 
            // mnuSepAutoZoom
            // 
            this.mnuSepAutoZoom.Name = "mnuSepAutoZoom";
            this.mnuSepAutoZoom.Size = new System.Drawing.Size(166, 6);
            // 
            // mnuOptions
            // 
            this.mnuOptions.Image = global::NClass.GUI.Properties.Resources.Options;
            this.mnuOptions.Name = "mnuOptions";
            this.mnuOptions.Size = new System.Drawing.Size(169, 22);
            this.mnuOptions.Text = "&Options...";
            this.mnuOptions.Click += new System.EventHandler(this.mnuOptions_Click);
            // 
            // mnuFormat
            // 
            this.mnuFormat.DropDownItems.AddRange(new System.Windows.Forms.ToolStripItem[] {
            this.mnuAlign,
            this.mnuMakeSameSize,
            this.sepFormat,
            this.mnuAutoWidth,
            this.mnuAutoHeight,
            this.mnuAutoLayout,
            this.sepAutoLayout,
            this.mnuCollapseAll,
            this.mnuExpandAll});
            this.mnuFormat.Name = "mnuFormat";
            this.mnuFormat.Size = new System.Drawing.Size(53, 20);
            this.mnuFormat.Text = "F&ormat";
            this.mnuFormat.DropDownOpening += new System.EventHandler(this.mnuFormat_DropDownOpening);
            // 
            // mnuAlign
            // 
            this.mnuAlign.DropDownItems.AddRange(new System.Windows.Forms.ToolStripItem[] {
            this.mnuAlignTop,
            this.mnuAlignLeft,
            this.mnuAlignBottom,
            this.mnuAlignRight,
            this.sepAlign,
            this.mnuAlignHorizontal,
            this.mnuAlignVertical});
            this.mnuAlign.Name = "mnuAlign";
            this.mnuAlign.Size = new System.Drawing.Size(161, 22);
            this.mnuAlign.Text = "&Align";
            this.mnuAlign.DropDownOpening += new System.EventHandler(this.mnuAlign_DropDownOpening);
            // 
            // mnuAlignTop
            // 
            this.mnuAlignTop.Image = global::NClass.GUI.Properties.Resources.AlignTop;
            this.mnuAlignTop.Name = "mnuAlignTop";
            this.mnuAlignTop.Size = new System.Drawing.Size(195, 22);
            this.mnuAlignTop.Text = "Align &Top";
            this.mnuAlignTop.Click += new System.EventHandler(this.mnuAlignTop_Click);
            // 
            // mnuAlignLeft
            // 
            this.mnuAlignLeft.Image = global::NClass.GUI.Properties.Resources.AlignLeft;
            this.mnuAlignLeft.Name = "mnuAlignLeft";
            this.mnuAlignLeft.Size = new System.Drawing.Size(195, 22);
            this.mnuAlignLeft.Text = "Align &Left";
            this.mnuAlignLeft.Click += new System.EventHandler(this.mnuAlignLeft_Click);
            // 
            // mnuAlignBottom
            // 
            this.mnuAlignBottom.Image = global::NClass.GUI.Properties.Resources.AlignBottom;
            this.mnuAlignBottom.Name = "mnuAlignBottom";
            this.mnuAlignBottom.Size = new System.Drawing.Size(195, 22);
            this.mnuAlignBottom.Text = "Align &Bottom";
            this.mnuAlignBottom.Click += new System.EventHandler(this.mnuAlignBottom_Click);
            // 
            // mnuAlignRight
            // 
            this.mnuAlignRight.Image = global::NClass.GUI.Properties.Resources.AlignRight;
            this.mnuAlignRight.Name = "mnuAlignRight";
            this.mnuAlignRight.Size = new System.Drawing.Size(195, 22);
            this.mnuAlignRight.Text = "Align &Right";
            this.mnuAlignRight.Click += new System.EventHandler(this.mnuAlignRight_Click);
            // 
            // sepAlign
            // 
            this.sepAlign.Name = "sepAlign";
            this.sepAlign.Size = new System.Drawing.Size(192, 6);
            // 
            // mnuAlignHorizontal
            // 
            this.mnuAlignHorizontal.Image = global::NClass.GUI.Properties.Resources.AlignHorizontal;
            this.mnuAlignHorizontal.Name = "mnuAlignHorizontal";
            this.mnuAlignHorizontal.Size = new System.Drawing.Size(195, 22);
            this.mnuAlignHorizontal.Text = "Align &Horizontal Center";
            this.mnuAlignHorizontal.Click += new System.EventHandler(this.mnuAlignHorizontal_Click);
            // 
            // mnuAlignVertical
            // 
            this.mnuAlignVertical.Image = global::NClass.GUI.Properties.Resources.AlignVertical;
            this.mnuAlignVertical.Name = "mnuAlignVertical";
            this.mnuAlignVertical.Size = new System.Drawing.Size(195, 22);
            this.mnuAlignVertical.Text = "Align &Vertical Center";
            this.mnuAlignVertical.Click += new System.EventHandler(this.mnuAlignVertical_Click);
            // 
            // mnuMakeSameSize
            // 
            this.mnuMakeSameSize.DropDownItems.AddRange(new System.Windows.Forms.ToolStripItem[] {
            this.mnuSameWidth,
            this.mnuSameHeight,
            this.mnuSameSize});
            this.mnuMakeSameSize.Name = "mnuMakeSameSize";
            this.mnuMakeSameSize.Size = new System.Drawing.Size(161, 22);
            this.mnuMakeSameSize.Text = "&Make Same Size";
            this.mnuMakeSameSize.DropDownOpening += new System.EventHandler(this.mnuMakeSameSize_DropDownOpening);
            // 
            // mnuSameWidth
            // 
            this.mnuSameWidth.Name = "mnuSameWidth";
            this.mnuSameWidth.Size = new System.Drawing.Size(197, 22);
            this.mnuSameWidth.Text = "Same &Width";
            this.mnuSameWidth.Click += new System.EventHandler(this.mnuSameWidth_Click);
            // 
            // mnuSameHeight
            // 
            this.mnuSameHeight.Name = "mnuSameHeight";
            this.mnuSameHeight.Size = new System.Drawing.Size(197, 22);
            this.mnuSameHeight.Text = "Same &Height";
            this.mnuSameHeight.Click += new System.EventHandler(this.mnuSameHeight_Click);
            // 
            // mnuSameSize
            // 
            this.mnuSameSize.Name = "mnuSameSize";
            this.mnuSameSize.Size = new System.Drawing.Size(197, 22);
            this.mnuSameSize.Text = "&Same Width and Height";
            this.mnuSameSize.Click += new System.EventHandler(this.mnuSameSize_Click);
            // 
            // sepFormat
            // 
            this.sepFormat.Name = "sepFormat";
            this.sepFormat.Size = new System.Drawing.Size(158, 6);
            // 
            // mnuAutoWidth
            // 
            this.mnuAutoWidth.Name = "mnuAutoWidth";
            this.mnuAutoWidth.Size = new System.Drawing.Size(161, 22);
            this.mnuAutoWidth.Text = "Auto &Width";
            this.mnuAutoWidth.Click += new System.EventHandler(this.mnuAutoWidth_Click);
            // 
            // mnuAutoHeight
            // 
            this.mnuAutoHeight.Name = "mnuAutoHeight";
            this.mnuAutoHeight.Size = new System.Drawing.Size(161, 22);
            this.mnuAutoHeight.Text = "Auto &Height";
            this.mnuAutoHeight.Click += new System.EventHandler(this.mnuAutoHeight_Click);
            // 
            // mnuAutoLayout
            // 
            this.mnuAutoLayout.Image = global::NClass.GUI.Properties.Resources.AutoLayout;
            this.mnuAutoLayout.Name = "mnuAutoLayout";
            this.mnuAutoLayout.Size = new System.Drawing.Size(161, 22);
            this.mnuAutoLayout.Text = "Auto &Layout";
            this.mnuAutoLayout.Visible = false;
            // 
            // sepAutoLayout
            // 
            this.sepAutoLayout.Name = "sepAutoLayout";
            this.sepAutoLayout.Size = new System.Drawing.Size(158, 6);
            // 
            // mnuCollapseAll
            // 
            this.mnuCollapseAll.Image = global::NClass.GUI.Properties.Resources.CollapseAll;
            this.mnuCollapseAll.Name = "mnuCollapseAll";
            this.mnuCollapseAll.Size = new System.Drawing.Size(161, 22);
            this.mnuCollapseAll.Text = "&Collapse All";
            this.mnuCollapseAll.Click += new System.EventHandler(this.mnuCollapseAll_Click);
            // 
            // mnuExpandAll
            // 
            this.mnuExpandAll.Image = global::NClass.GUI.Properties.Resources.ExpandAll;
            this.mnuExpandAll.Name = "mnuExpandAll";
            this.mnuExpandAll.Size = new System.Drawing.Size(161, 22);
            this.mnuExpandAll.Text = "&Expand All";
            this.mnuExpandAll.Click += new System.EventHandler(this.mnuExpandAll_Click);
            // 
            // mnuPlugins
            // 
            this.mnuPlugins.Name = "mnuPlugins";
            this.mnuPlugins.Size = new System.Drawing.Size(52, 20);
            this.mnuPlugins.Text = "&Plugins";
            this.mnuPlugins.Visible = false;
            // 
            // mnuHelp
            // 
            this.mnuHelp.DropDownItems.AddRange(new System.Windows.Forms.ToolStripItem[] {
            this.mnuContents,
            this.mnuCheckForUpdates,
            this.mnuSepUpdates,
            this.mnuAbout});
            this.mnuHelp.Name = "mnuHelp";
            this.mnuHelp.Size = new System.Drawing.Size(40, 20);
            this.mnuHelp.Text = "&Help";
            // 
            // mnuContents
            // 
            this.mnuContents.Image = global::NClass.GUI.Properties.Resources.Help;
            this.mnuContents.Name = "mnuContents";
            this.mnuContents.ShortcutKeys = System.Windows.Forms.Keys.F1;
            this.mnuContents.Size = new System.Drawing.Size(174, 22);
            this.mnuContents.Text = "&Contents";
            this.mnuContents.Visible = false;
            this.mnuContents.Click += new System.EventHandler(this.mnuContents_Click);
            // 
            // mnuCheckForUpdates
            // 
            this.mnuCheckForUpdates.Image = global::NClass.GUI.Properties.Resources.SearchWeb;
            this.mnuCheckForUpdates.Name = "mnuCheckForUpdates";
            this.mnuCheckForUpdates.Size = new System.Drawing.Size(174, 22);
            this.mnuCheckForUpdates.Text = "Check for &Updates";
            this.mnuCheckForUpdates.Click += new System.EventHandler(this.mnuCheckForUpdates_Click);
            // 
            // mnuSepUpdates
            // 
            this.mnuSepUpdates.Name = "mnuSepUpdates";
            this.mnuSepUpdates.Size = new System.Drawing.Size(171, 6);
            // 
            // mnuAbout
            // 
            this.mnuAbout.Name = "mnuAbout";
            this.mnuAbout.Size = new System.Drawing.Size(174, 22);
            this.mnuAbout.Text = "&About NClass...";
            this.mnuAbout.Click += new System.EventHandler(this.mnuAbout_Click);
            // 
            // standardToolStrip
            // 
            this.standardToolStrip.Dock = System.Windows.Forms.DockStyle.None;
            this.standardToolStrip.Items.AddRange(new System.Windows.Forms.ToolStripItem[] {
            this.toolNew,
            this.toolOpen,
            this.toolSave,
            this.toolPrint,
            this.toolSepSave,
            this.toolZoomValue,
            this.toolZoomOut,
            this.toolZoom,
            this.toolZoomIn,
            this.toolAutoZoom});
            this.standardToolStrip.Location = new System.Drawing.Point(3, 24);
            this.standardToolStrip.Name = "standardToolStrip";
            this.standardToolStrip.Size = new System.Drawing.Size(333, 25);
            this.standardToolStrip.TabIndex = 1;
            // 
            // toolNew
            // 
            this.toolNew.DisplayStyle = System.Windows.Forms.ToolStripItemDisplayStyle.Image;
            this.toolNew.DropDownItems.AddRange(new System.Windows.Forms.ToolStripItem[] {
            this.toolNewCSharpDiagram,
            this.toolNewJavaDiagram});
            this.toolNew.Image = global::NClass.GUI.Properties.Resources.NewDocument;
            this.toolNew.ImageTransparentColor = System.Drawing.Color.Magenta;
            this.toolNew.Name = "toolNew";
            this.toolNew.Size = new System.Drawing.Size(32, 22);
            this.toolNew.ButtonClick += new System.EventHandler(this.toolNew_ButtonClick);
            // 
            // toolNewCSharpDiagram
            // 
            this.toolNewCSharpDiagram.Name = "toolNewCSharpDiagram";
            this.toolNewCSharpDiagram.Size = new System.Drawing.Size(149, 22);
            this.toolNewCSharpDiagram.Text = "C# diagram";
            this.toolNewCSharpDiagram.Click += new System.EventHandler(this.mnuNewCSharpDiagram_Click);
            // 
            // toolNewJavaDiagram
            // 
            this.toolNewJavaDiagram.Name = "toolNewJavaDiagram";
            this.toolNewJavaDiagram.Size = new System.Drawing.Size(149, 22);
            this.toolNewJavaDiagram.Text = "Java diagram";
            this.toolNewJavaDiagram.Click += new System.EventHandler(this.mnuNewJavaDiagram_Click);
            // 
            // toolOpen
            // 
            this.toolOpen.DisplayStyle = System.Windows.Forms.ToolStripItemDisplayStyle.Image;
            this.toolOpen.DropDownItems.AddRange(new System.Windows.Forms.ToolStripItem[] {
            this.toolRecentFile1,
            this.toolRecentFile2,
            this.toolRecentFile3,
            this.toolRecentFile4,
            this.toolRecentFile5});
            this.toolOpen.Image = global::NClass.GUI.Properties.Resources.Open;
            this.toolOpen.ImageTransparentColor = System.Drawing.Color.Magenta;
            this.toolOpen.Name = "toolOpen";
            this.toolOpen.Size = new System.Drawing.Size(32, 22);
            this.toolOpen.ButtonClick += new System.EventHandler(this.mnuOpenFile_Click);
            this.toolOpen.DropDownOpening += new System.EventHandler(this.toolOpen_DropDownOpening);
            // 
            // toolRecentFile1
            // 
            this.toolRecentFile1.Name = "toolRecentFile1";
            this.toolRecentFile1.Size = new System.Drawing.Size(145, 22);
            this.toolRecentFile1.Tag = 0;
            this.toolRecentFile1.Text = "Recent file 1";
            this.toolRecentFile1.Click += new System.EventHandler(this.OpenRecentFile_Click);
            // 
            // toolRecentFile2
            // 
            this.toolRecentFile2.Name = "toolRecentFile2";
            this.toolRecentFile2.Size = new System.Drawing.Size(145, 22);
            this.toolRecentFile2.Tag = 1;
            this.toolRecentFile2.Text = "Recent file 2";
            this.toolRecentFile2.Click += new System.EventHandler(this.OpenRecentFile_Click);
            // 
            // toolRecentFile3
            // 
            this.toolRecentFile3.Name = "toolRecentFile3";
            this.toolRecentFile3.Size = new System.Drawing.Size(145, 22);
            this.toolRecentFile3.Tag = 2;
            this.toolRecentFile3.Text = "Recent file 3";
            this.toolRecentFile3.Click += new System.EventHandler(this.OpenRecentFile_Click);
            // 
            // toolRecentFile4
            // 
            this.toolRecentFile4.Name = "toolRecentFile4";
            this.toolRecentFile4.Size = new System.Drawing.Size(145, 22);
            this.toolRecentFile4.Tag = 3;
            this.toolRecentFile4.Text = "Recent file 4";
            this.toolRecentFile4.Click += new System.EventHandler(this.OpenRecentFile_Click);
            // 
            // toolRecentFile5
            // 
            this.toolRecentFile5.Name = "toolRecentFile5";
            this.toolRecentFile5.Size = new System.Drawing.Size(145, 22);
            this.toolRecentFile5.Tag = 4;
            this.toolRecentFile5.Text = "Recent file 5";
            this.toolRecentFile5.Click += new System.EventHandler(this.OpenRecentFile_Click);
            // 
            // toolSave
            // 
            this.toolSave.DisplayStyle = System.Windows.Forms.ToolStripItemDisplayStyle.Image;
            this.toolSave.Image = global::NClass.GUI.Properties.Resources.Save;
            this.toolSave.ImageTransparentColor = System.Drawing.Color.Magenta;
            this.toolSave.Name = "toolSave";
            this.toolSave.Size = new System.Drawing.Size(23, 22);
            this.toolSave.Click += new System.EventHandler(this.mnuSave_Click);
            // 
            // toolPrint
            // 
            this.toolPrint.DisplayStyle = System.Windows.Forms.ToolStripItemDisplayStyle.Image;
            this.toolPrint.Image = global::NClass.GUI.Properties.Resources.Print;
            this.toolPrint.ImageTransparentColor = System.Drawing.Color.Magenta;
            this.toolPrint.Name = "toolPrint";
            this.toolPrint.Size = new System.Drawing.Size(23, 22);
            this.toolPrint.Text = "toolStripButton1";
            this.toolPrint.Click += new System.EventHandler(this.mnuPrint_Click);
            // 
            // toolSepSave
            // 
            this.toolSepSave.Name = "toolSepSave";
            this.toolSepSave.Size = new System.Drawing.Size(6, 25);
            // 
            // toolZoomValue
            // 
            this.toolZoomValue.AutoSize = false;
            this.toolZoomValue.Name = "toolZoomValue";
            this.toolZoomValue.Size = new System.Drawing.Size(36, 22);
            this.toolZoomValue.Text = "100%";
            this.toolZoomValue.TextAlign = System.Drawing.ContentAlignment.MiddleRight;
            // 
            // toolZoomOut
            // 
            this.toolZoomOut.DisplayStyle = System.Windows.Forms.ToolStripItemDisplayStyle.Image;
            this.toolZoomOut.Image = global::NClass.GUI.Properties.Resources.ZoomOut;
            this.toolZoomOut.ImageTransparentColor = System.Drawing.Color.Magenta;
            this.toolZoomOut.Name = "toolZoomOut";
            this.toolZoomOut.Size = new System.Drawing.Size(23, 22);
            this.toolZoomOut.Click += new System.EventHandler(this.toolZoomOut_Click);
            // 
            // toolZoom
            // 
            this.toolZoom.Name = "toolZoom";
            this.toolZoom.Size = new System.Drawing.Size(100, 22);
            this.toolZoom.ZoomValue = 1F;
            this.toolZoom.ZoomValueChanged += new System.EventHandler(this.toolZoom_ZoomValueChanged);
            // 
            // toolZoomIn
            // 
            this.toolZoomIn.DisplayStyle = System.Windows.Forms.ToolStripItemDisplayStyle.Image;
            this.toolZoomIn.Image = global::NClass.GUI.Properties.Resources.ZoomIn;
            this.toolZoomIn.ImageTransparentColor = System.Drawing.Color.Magenta;
            this.toolZoomIn.Name = "toolZoomIn";
            this.toolZoomIn.Size = new System.Drawing.Size(23, 22);
            this.toolZoomIn.Click += new System.EventHandler(this.toolZoomIn_Click);
            // 
            // toolAutoZoom
            // 
            this.toolAutoZoom.DisplayStyle = System.Windows.Forms.ToolStripItemDisplayStyle.Image;
            this.toolAutoZoom.Image = global::NClass.GUI.Properties.Resources.AutoZoom;
            this.toolAutoZoom.ImageTransparentColor = System.Drawing.Color.Magenta;
            this.toolAutoZoom.Name = "toolAutoZoom";
            this.toolAutoZoom.Size = new System.Drawing.Size(23, 22);
            this.toolAutoZoom.Click += new System.EventHandler(this.mnuAutoZoom_Click);
            // 
            // typeDetailsToolStrip
            // 
            this.typeDetailsToolStrip.Dock = System.Windows.Forms.DockStyle.None;
            this.typeDetailsToolStrip.Items.AddRange(new System.Windows.Forms.ToolStripItem[] {
            this.lblName,
            this.txtName,
            this.lblAccess,
            this.cboAccess,
            this.lblModifier,
            this.cboModifier,
            this.lblReturnType,
            this.txtReturnType});
            this.typeDetailsToolStrip.Location = new System.Drawing.Point(3, 49);
            this.typeDetailsToolStrip.Name = "typeDetailsToolStrip";
            this.typeDetailsToolStrip.Size = new System.Drawing.Size(591, 25);
            this.typeDetailsToolStrip.TabIndex = 5;
            // 
            // lblName
            // 
            this.lblName.Name = "lblName";
            this.lblName.Size = new System.Drawing.Size(38, 22);
            this.lblName.Text = "Name:";
            // 
            // txtName
            // 
            this.txtName.Enabled = false;
            this.txtName.Name = "txtName";
            this.txtName.Size = new System.Drawing.Size(200, 25);
            this.txtName.Validated += new System.EventHandler(this.txtName_Validated);
            this.txtName.KeyPress += new System.Windows.Forms.KeyPressEventHandler(this.txtName_KeyPress);
            this.txtName.TextChanged += new System.EventHandler(this.txtName_TextChanged);
            // 
            // lblAccess
            // 
            this.lblAccess.Name = "lblAccess";
            this.lblAccess.Size = new System.Drawing.Size(44, 22);
            this.lblAccess.Text = "Access:";
            // 
            // cboAccess
            // 
            this.cboAccess.DropDownStyle = System.Windows.Forms.ComboBoxStyle.DropDownList;
            this.cboAccess.Enabled = false;
            this.cboAccess.Name = "cboAccess";
            this.cboAccess.Size = new System.Drawing.Size(121, 25);
            this.cboAccess.SelectedIndexChanged += new System.EventHandler(this.cboAccess_SelectedIndexChanged);
            // 
            // lblModifier
            // 
            this.lblModifier.Name = "lblModifier";
            this.lblModifier.Size = new System.Drawing.Size(49, 22);
            this.lblModifier.Text = "Modifier:";
            // 
            // cboModifier
            // 
            this.cboModifier.DropDownStyle = System.Windows.Forms.ComboBoxStyle.DropDownList;
            this.cboModifier.Enabled = false;
            this.cboModifier.Name = "cboModifier";
            this.cboModifier.Size = new System.Drawing.Size(121, 25);
            this.cboModifier.SelectedIndexChanged += new System.EventHandler(this.cboModifier_SelectedIndexChanged);
            // 
            // lblReturnType
            // 
            this.lblReturnType.Name = "lblReturnType";
            this.lblReturnType.Size = new System.Drawing.Size(69, 22);
            this.lblReturnType.Text = "Return type:";
            this.lblReturnType.Visible = false;
            // 
            // txtReturnType
            // 
            this.txtReturnType.Name = "txtReturnType";
            this.txtReturnType.Size = new System.Drawing.Size(100, 25);
            this.txtReturnType.Visible = false;
            this.txtReturnType.Validated += new System.EventHandler(this.txtReturnType_Validated);
            this.txtReturnType.TextChanged += new System.EventHandler(this.txtReturnType_TextChanged);
            // 
            // elementsToolStrip
            // 
            this.elementsToolStrip.Dock = System.Windows.Forms.DockStyle.None;
            this.elementsToolStrip.Items.AddRange(new System.Windows.Forms.ToolStripItem[] {
            this.toolNewClass,
            this.toolNewStruct,
            this.toolNewInterface,
            this.toolNewEnum,
            this.toolNewDelegate,
            this.toolNewComment,
            this.toolSepEntities,
            this.toolNewAssociation,
            this.toolNewComposition,
            this.toolNewAggregation,
            this.toolNewGeneralization,
            this.toolNewRealization,
            this.toolNewDependency,
            this.toolNewNesting,
            this.toolNewCommentRelation,
            this.toolSepRelations,
            this.toolDelete});
            this.elementsToolStrip.Location = new System.Drawing.Point(3, 74);
            this.elementsToolStrip.Name = "elementsToolStrip";
            this.elementsToolStrip.Size = new System.Drawing.Size(369, 25);
            this.elementsToolStrip.TabIndex = 5;
            // 
            // toolNewClass
            // 
            this.toolNewClass.DisplayStyle = System.Windows.Forms.ToolStripItemDisplayStyle.Image;
            this.toolNewClass.Image = global::NClass.GUI.Properties.Resources.Class;
            this.toolNewClass.ImageTransparentColor = System.Drawing.Color.Magenta;
            this.toolNewClass.Name = "toolNewClass";
            this.toolNewClass.Size = new System.Drawing.Size(23, 22);
            this.toolNewClass.Click += new System.EventHandler(this.mnuNewClass_Click);
            // 
            // toolNewStruct
            // 
            this.toolNewStruct.DisplayStyle = System.Windows.Forms.ToolStripItemDisplayStyle.Image;
            this.toolNewStruct.Image = global::NClass.GUI.Properties.Resources.Struct;
            this.toolNewStruct.ImageTransparentColor = System.Drawing.Color.Magenta;
            this.toolNewStruct.Name = "toolNewStruct";
            this.toolNewStruct.Size = new System.Drawing.Size(23, 22);
            this.toolNewStruct.Click += new System.EventHandler(this.mnuNewStructure_Click);
            // 
            // toolNewInterface
            // 
            this.toolNewInterface.DisplayStyle = System.Windows.Forms.ToolStripItemDisplayStyle.Image;
            this.toolNewInterface.Image = global::NClass.GUI.Properties.Resources.Interface;
            this.toolNewInterface.ImageTransparentColor = System.Drawing.Color.Magenta;
            this.toolNewInterface.Name = "toolNewInterface";
            this.toolNewInterface.Size = new System.Drawing.Size(23, 22);
            this.toolNewInterface.Click += new System.EventHandler(this.mnuNewInterface_Click);
            // 
            // toolNewEnum
            // 
            this.toolNewEnum.DisplayStyle = System.Windows.Forms.ToolStripItemDisplayStyle.Image;
            this.toolNewEnum.Image = global::NClass.GUI.Properties.Resources.Enum;
            this.toolNewEnum.ImageTransparentColor = System.Drawing.Color.Magenta;
            this.toolNewEnum.Name = "toolNewEnum";
            this.toolNewEnum.Size = new System.Drawing.Size(23, 22);
            this.toolNewEnum.Click += new System.EventHandler(this.mnuNewEnum_Click);
            // 
            // toolNewDelegate
            // 
            this.toolNewDelegate.DisplayStyle = System.Windows.Forms.ToolStripItemDisplayStyle.Image;
            this.toolNewDelegate.Image = global::NClass.GUI.Properties.Resources.Delegate;
            this.toolNewDelegate.ImageTransparentColor = System.Drawing.Color.Magenta;
            this.toolNewDelegate.Name = "toolNewDelegate";
            this.toolNewDelegate.Size = new System.Drawing.Size(23, 22);
            this.toolNewDelegate.Click += new System.EventHandler(this.mnuNewDelegate_Click);
            // 
            // toolNewComment
            // 
            this.toolNewComment.DisplayStyle = System.Windows.Forms.ToolStripItemDisplayStyle.Image;
            this.toolNewComment.Image = global::NClass.GUI.Properties.Resources.Note;
            this.toolNewComment.ImageTransparentColor = System.Drawing.Color.Magenta;
            this.toolNewComment.Name = "toolNewComment";
            this.toolNewComment.Size = new System.Drawing.Size(23, 22);
            this.toolNewComment.Click += new System.EventHandler(this.mnuNewComment_Click);
            // 
            // toolSepEntities
            // 
            this.toolSepEntities.Name = "toolSepEntities";
            this.toolSepEntities.Size = new System.Drawing.Size(6, 25);
            // 
            // toolNewAssociation
            // 
            this.toolNewAssociation.DisplayStyle = System.Windows.Forms.ToolStripItemDisplayStyle.Image;
            this.toolNewAssociation.Image = global::NClass.GUI.Properties.Resources.Association;
            this.toolNewAssociation.ImageTransparentColor = System.Drawing.Color.Magenta;
            this.toolNewAssociation.Name = "toolNewAssociation";
            this.toolNewAssociation.Size = new System.Drawing.Size(23, 22);
            this.toolNewAssociation.Click += new System.EventHandler(this.mnuNewAssociation_Click);
            // 
            // toolNewComposition
            // 
            this.toolNewComposition.DisplayStyle = System.Windows.Forms.ToolStripItemDisplayStyle.Image;
            this.toolNewComposition.Image = global::NClass.GUI.Properties.Resources.Composition;
            this.toolNewComposition.ImageTransparentColor = System.Drawing.Color.Magenta;
            this.toolNewComposition.Name = "toolNewComposition";
            this.toolNewComposition.Size = new System.Drawing.Size(23, 22);
            this.toolNewComposition.Click += new System.EventHandler(this.mnuNewComposition_Click);
            // 
            // toolNewAggregation
            // 
            this.toolNewAggregation.DisplayStyle = System.Windows.Forms.ToolStripItemDisplayStyle.Image;
            this.toolNewAggregation.Image = global::NClass.GUI.Properties.Resources.Aggregation;
            this.toolNewAggregation.ImageTransparentColor = System.Drawing.Color.Magenta;
            this.toolNewAggregation.Name = "toolNewAggregation";
            this.toolNewAggregation.Size = new System.Drawing.Size(23, 22);
            this.toolNewAggregation.Click += new System.EventHandler(this.mnuNewAggregation_Click);
            // 
            // toolNewGeneralization
            // 
            this.toolNewGeneralization.DisplayStyle = System.Windows.Forms.ToolStripItemDisplayStyle.Image;
            this.toolNewGeneralization.Image = global::NClass.GUI.Properties.Resources.Generalization;
            this.toolNewGeneralization.ImageTransparentColor = System.Drawing.Color.Magenta;
            this.toolNewGeneralization.Name = "toolNewGeneralization";
            this.toolNewGeneralization.Size = new System.Drawing.Size(23, 22);
            this.toolNewGeneralization.Click += new System.EventHandler(this.mnuNewGeneralization_Click);
            // 
            // toolNewRealization
            // 
            this.toolNewRealization.DisplayStyle = System.Windows.Forms.ToolStripItemDisplayStyle.Image;
            this.toolNewRealization.Image = global::NClass.GUI.Properties.Resources.Realization;
            this.toolNewRealization.ImageTransparentColor = System.Drawing.Color.Magenta;
            this.toolNewRealization.Name = "toolNewRealization";
            this.toolNewRealization.Size = new System.Drawing.Size(23, 22);
            this.toolNewRealization.Click += new System.EventHandler(this.mnuNewRealization_Click);
            // 
            // toolNewDependency
            // 
            this.toolNewDependency.DisplayStyle = System.Windows.Forms.ToolStripItemDisplayStyle.Image;
            this.toolNewDependency.Image = global::NClass.GUI.Properties.Resources.Dependency;
            this.toolNewDependency.ImageTransparentColor = System.Drawing.Color.Magenta;
            this.toolNewDependency.Name = "toolNewDependency";
            this.toolNewDependency.Size = new System.Drawing.Size(23, 22);
            this.toolNewDependency.Click += new System.EventHandler(this.mnuNewDependency_Click);
            // 
            // toolNewNesting
            // 
            this.toolNewNesting.DisplayStyle = System.Windows.Forms.ToolStripItemDisplayStyle.Image;
            this.toolNewNesting.Image = global::NClass.GUI.Properties.Resources.Nesting;
            this.toolNewNesting.ImageTransparentColor = System.Drawing.Color.Magenta;
            this.toolNewNesting.Name = "toolNewNesting";
            this.toolNewNesting.Size = new System.Drawing.Size(23, 22);
            this.toolNewNesting.Click += new System.EventHandler(this.mnuNewNesting_Click);
            // 
            // toolNewCommentRelation
            // 
            this.toolNewCommentRelation.DisplayStyle = System.Windows.Forms.ToolStripItemDisplayStyle.Image;
            this.toolNewCommentRelation.Image = global::NClass.GUI.Properties.Resources.Comment;
            this.toolNewCommentRelation.ImageTransparentColor = System.Drawing.Color.Magenta;
            this.toolNewCommentRelation.Name = "toolNewCommentRelation";
            this.toolNewCommentRelation.Size = new System.Drawing.Size(23, 22);
            this.toolNewCommentRelation.Click += new System.EventHandler(this.mnuNewCommentRelation_Click);
            // 
            // toolSepRelations
            // 
            this.toolSepRelations.Name = "toolSepRelations";
            this.toolSepRelations.Size = new System.Drawing.Size(6, 25);
            // 
            // toolDelete
            // 
            this.toolDelete.DisplayStyle = System.Windows.Forms.ToolStripItemDisplayStyle.Image;
            this.toolDelete.Enabled = false;
            this.toolDelete.Image = global::NClass.GUI.Properties.Resources.Delete;
            this.toolDelete.ImageTransparentColor = System.Drawing.Color.Magenta;
            this.toolDelete.Name = "toolDelete";
            this.toolDelete.Size = new System.Drawing.Size(23, 22);
            this.toolDelete.Click += new System.EventHandler(this.mnuDelete_Click);
            // 
            // MainForm
            // 
            this.AllowDrop = true;
            this.AutoScaleDimensions = new System.Drawing.SizeF(6F, 13F);
            this.AutoScaleMode = System.Windows.Forms.AutoScaleMode.Font;
            this.ClientSize = new System.Drawing.Size(762, 516);
            this.Controls.Add(this.toolStripContainer);
            this.Icon = ((System.Drawing.Icon)(resources.GetObject("$this.Icon")));
            this.MainMenuStrip = this.menuStrip;
            this.MinimumSize = new System.Drawing.Size(400, 200);
            this.Name = "MainForm";
            this.StartPosition = System.Windows.Forms.FormStartPosition.CenterScreen;
            this.Text = "NClass";
            this.toolStripContainer.BottomToolStripPanel.ResumeLayout(false);
            this.toolStripContainer.BottomToolStripPanel.PerformLayout();
            this.toolStripContainer.TopToolStripPanel.ResumeLayout(false);
            this.toolStripContainer.TopToolStripPanel.PerformLayout();
            this.toolStripContainer.ResumeLayout(false);
            this.toolStripContainer.PerformLayout();
            this.statusStrip.ResumeLayout(false);
            this.statusStrip.PerformLayout();
            this.menuStrip.ResumeLayout(false);
            this.menuStrip.PerformLayout();
            this.standardToolStrip.ResumeLayout(false);
            this.standardToolStrip.PerformLayout();
            this.typeDetailsToolStrip.ResumeLayout(false);
            this.typeDetailsToolStrip.PerformLayout();
            this.elementsToolStrip.ResumeLayout(false);
            this.elementsToolStrip.PerformLayout();
            this.ResumeLayout(false);

		}

		#endregion

		private System.Windows.Forms.ToolStripContainer toolStripContainer;
		private System.Windows.Forms.StatusStrip statusStrip;
		private System.Windows.Forms.MenuStrip menuStrip;
		private System.Windows.Forms.ToolStrip standardToolStrip;
		private System.Windows.Forms.ToolStripMenuItem mnuFile;
		private System.Windows.Forms.ToolStripMenuItem mnuNew;
		private System.Windows.Forms.ToolStripMenuItem mnuNewCSharpDiagram;
		private System.Windows.Forms.ToolStripMenuItem mnuNewJavaDiagram;
		private System.Windows.Forms.ToolStripMenuItem mnuOpen;
		private System.Windows.Forms.ToolStripMenuItem mnuOpenFile;
		private System.Windows.Forms.ToolStripSeparator sepOpenFile;
		private System.Windows.Forms.ToolStripMenuItem mnuSave;
		private System.Windows.Forms.ToolStripMenuItem mnuSaveAs;
		private System.Windows.Forms.ToolStripSeparator mnuSepPrint;
		private System.Windows.Forms.ToolStripMenuItem mnuPrint;
		private System.Windows.Forms.ToolStripSeparator mnuSepExport;
		private System.Windows.Forms.ToolStripMenuItem mnuExit;
		private System.Windows.Forms.ToolStripMenuItem mnuDiagram;
		private System.Windows.Forms.ToolStripMenuItem mnuAddNewElement;
		private System.Windows.Forms.ToolStripMenuItem mnuNewClass;
		private System.Windows.Forms.ToolStripMenuItem mnuNewStructure;
		private System.Windows.Forms.ToolStripMenuItem mnuNewInterface;
		private System.Windows.Forms.ToolStripMenuItem mnuNewEnum;
		private System.Windows.Forms.ToolStripMenuItem mnuNewDelegate;
		private System.Windows.Forms.ToolStripMenuItem mnuNewComment;
		private System.Windows.Forms.ToolStripMenuItem mnuMembersFormat;
		private System.Windows.Forms.ToolStripMenuItem mnuShowType;
		private System.Windows.Forms.ToolStripMenuItem mnuShowParameters;
		private System.Windows.Forms.ToolStripMenuItem mnuShowParameterNames;
		private System.Windows.Forms.ToolStripMenuItem mnuShowInitialValue;
		private System.Windows.Forms.ToolStripSeparator mnuSepFormat;
		private System.Windows.Forms.ToolStripMenuItem mnuSaveAsImage;
		private System.Windows.Forms.ToolStripMenuItem mnuAutoZoom;
		private System.Windows.Forms.ToolStripSeparator mnuSepAutoZoom;
		private System.Windows.Forms.ToolStripMenuItem mnuOptions;
		private System.Windows.Forms.ToolStripMenuItem mnuHelp;
		private System.Windows.Forms.ToolStripMenuItem mnuContents;
		private System.Windows.Forms.ToolStripSeparator mnuSepUpdates;
		private System.Windows.Forms.ToolStripMenuItem mnuAbout;
		private System.Windows.Forms.ToolStripMenuItem mnuRecentFile1;
		private System.Windows.Forms.ToolStripSplitButton toolNew;
		private System.Windows.Forms.ToolStripSplitButton toolOpen;
		private System.Windows.Forms.ToolStripButton toolSave;
		private System.Windows.Forms.ToolStripSeparator toolSepSave;
		private System.Windows.Forms.ToolStripMenuItem mnuRecentFile2;
		private System.Windows.Forms.ToolStripMenuItem mnuRecentFile3;
		private System.Windows.Forms.ToolStripMenuItem mnuRecentFile4;
		private System.Windows.Forms.ToolStripMenuItem mnuRecentFile5;
		private System.Windows.Forms.ToolStripMenuItem toolNewCSharpDiagram;
		private System.Windows.Forms.ToolStripMenuItem toolNewJavaDiagram;
		private System.Windows.Forms.ToolStripMenuItem toolRecentFile1;
		private System.Windows.Forms.ToolStripMenuItem toolRecentFile2;
		private System.Windows.Forms.ToolStripMenuItem toolRecentFile3;
		private System.Windows.Forms.ToolStripMenuItem toolRecentFile4;
		private System.Windows.Forms.ToolStripMenuItem toolRecentFile5;
		private System.Windows.Forms.ToolStripButton toolZoomIn;
		private System.Windows.Forms.ToolStripButton toolZoomOut;
		private System.Windows.Forms.ToolStripButton toolAutoZoom;
		private System.Windows.Forms.ToolStripSeparator mnuSepElement;
		private System.Windows.Forms.ToolStripStatusLabel lblLanguage;
		private System.Windows.Forms.ToolStrip typeDetailsToolStrip;
		private System.Windows.Forms.ToolStripLabel lblName;
		private System.Windows.Forms.ToolStripTextBox txtName;
		private System.Windows.Forms.ToolStripLabel lblAccess;
		private System.Windows.Forms.ToolStripComboBox cboAccess;
		private System.Windows.Forms.ToolStripLabel lblModifier;
		private System.Windows.Forms.ToolStripComboBox cboModifier;
		private System.Windows.Forms.ToolStrip elementsToolStrip;
		private System.Windows.Forms.ToolStripButton toolNewClass;
		private System.Windows.Forms.ToolStripButton toolNewStruct;
		private System.Windows.Forms.ToolStripButton toolNewInterface;
		private System.Windows.Forms.ToolStripButton toolNewEnum;
		private System.Windows.Forms.ToolStripButton toolNewDelegate;
		private System.Windows.Forms.ToolStripButton toolNewComment;
		private System.Windows.Forms.ToolStripButton toolDelete;
		private System.Windows.Forms.ToolStripStatusLabel lblStatus;
		private System.Windows.Forms.ToolStripSeparator toolSepEntities;
		private System.Windows.Forms.ToolStripButton toolNewAssociation;
		private System.Windows.Forms.ToolStripButton toolNewComposition;
		private System.Windows.Forms.ToolStripButton toolNewAggregation;
		private System.Windows.Forms.ToolStripButton toolNewGeneralization;
		private System.Windows.Forms.ToolStripButton toolNewRealization;
		private System.Windows.Forms.ToolStripButton toolNewDependency;
		private System.Windows.Forms.ToolStripButton toolNewNesting;
		private System.Windows.Forms.ToolStripButton toolNewCommentRelation;
		private System.Windows.Forms.ToolStripSeparator toolSepRelations;
		private System.Windows.Forms.ToolStripMenuItem mnuNewAssociation;
		private System.Windows.Forms.ToolStripMenuItem mnuNewComposition;
		private System.Windows.Forms.ToolStripMenuItem mnuNewAggregation;
		private System.Windows.Forms.ToolStripMenuItem mnuNewGeneralization;
		private System.Windows.Forms.ToolStripMenuItem mnuNewRealization;
		private System.Windows.Forms.ToolStripMenuItem mnuNewDependency;
		private System.Windows.Forms.ToolStripMenuItem mnuNewNesting;
		private System.Windows.Forms.ToolStripMenuItem mnuNewCommentRelation;
		private System.Windows.Forms.ToolStripButton toolPrint;
		private System.Windows.Forms.ToolStripMenuItem mnuEdit;
		private System.Windows.Forms.ToolStripMenuItem mnuUndo;
		private System.Windows.Forms.ToolStripMenuItem mnuRedo;
		private System.Windows.Forms.ToolStripSeparator mnuSepReso;
		private System.Windows.Forms.ToolStripMenuItem mnuCut;
		private System.Windows.Forms.ToolStripMenuItem mnuPaste;
		private System.Windows.Forms.ToolStripMenuItem mnuDelete;
		private System.Windows.Forms.ToolStripSeparator mnuSepDelete;
		private System.Windows.Forms.ToolStripMenuItem mnuSelectAll;
		private System.Windows.Forms.ToolStripMenuItem mnuCheckForUpdates;
		private System.Windows.Forms.ToolStripMenuItem mnuPlugins;
		private System.Windows.Forms.ToolStripMenuItem mnuImport;
		private System.Windows.Forms.ToolStripMenuItem mnuExport;
		private System.Windows.Forms.ToolStripMenuItem mnuGenerateCode;
		private System.Windows.Forms.ToolStripLabel lblReturnType;
		private System.Windows.Forms.ToolStripTextBox txtReturnType;
		private ZoomingToolStrip toolZoom;
		private System.Windows.Forms.ToolStripLabel toolZoomValue;
		private System.Windows.Forms.ToolStripMenuItem mnuDiagramSize;
		private System.Windows.Forms.ToolStripMenuItem mnuFormat;
		private System.Windows.Forms.ToolStripMenuItem mnuAlign;
		private System.Windows.Forms.ToolStripMenuItem mnuAlignTop;
		private System.Windows.Forms.ToolStripMenuItem mnuAlignLeft;
		private System.Windows.Forms.ToolStripMenuItem mnuAlignBottom;
		private System.Windows.Forms.ToolStripMenuItem mnuAlignRight;
		private System.Windows.Forms.ToolStripSeparator sepAlign;
		private System.Windows.Forms.ToolStripMenuItem mnuAlignHorizontal;
		private System.Windows.Forms.ToolStripMenuItem mnuAlignVertical;
		private System.Windows.Forms.ToolStripMenuItem mnuMakeSameSize;
		private System.Windows.Forms.ToolStripMenuItem mnuSameWidth;
		private System.Windows.Forms.ToolStripMenuItem mnuSameHeight;
		private System.Windows.Forms.ToolStripMenuItem mnuSameSize;
		private System.Windows.Forms.ToolStripMenuItem mnuAutoWidth;
		private System.Windows.Forms.ToolStripMenuItem mnuAutoHeight;
		private System.Windows.Forms.ToolStripMenuItem mnuAutoLayout;
		private System.Windows.Forms.ToolStripSeparator sepFormat;
		private System.Windows.Forms.ToolStripSeparator sepAutoLayout;
		private System.Windows.Forms.ToolStripMenuItem mnuCollapseAll;
		private System.Windows.Forms.ToolStripMenuItem mnuExpandAll;
	}
}