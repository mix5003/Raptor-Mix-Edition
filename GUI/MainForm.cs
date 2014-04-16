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
using System.Drawing;
using System.Drawing.Imaging;
using System.Drawing.Printing;
using System.Collections.Generic;
using System.Reflection;
using System.ComponentModel;
using System.Windows.Forms;
using NClass.Core;
using NClass.CSharp;
using NClass.Java;
using NClass.CodeGenerator;
using NClass.GUI.Diagram;
using NClass.Translations;

using WindowSettings = NClass.GUI.Properties.Settings;

namespace NClass.GUI
{
	public sealed partial class MainForm : Form
	{
		Project project;
		DiagramControl diagram;
		OptionsDialog optionsDialog;
		List<IProjectPlugin> projectPlugins = new List<IProjectPlugin>();
		List<IDiagramPlugin> diagramPlugins = new List<IDiagramPlugin>();

		static readonly Dictionary<Language, AccessModifier[]> accessOrders;
		static readonly Dictionary<Language, ClassModifier[]> modifierOrders;

		static MainForm()
		{
			accessOrders = new Dictionary<Language, AccessModifier[]>(Language.LanguageCount);
			modifierOrders = new Dictionary<Language, ClassModifier[]>(Language.LanguageCount);

			accessOrders.Add(CSharpLanguage.Instance, new AccessModifier[] {
				AccessModifier.Public,
				AccessModifier.ProtectedInternal,
				AccessModifier.Internal,
				AccessModifier.Protected,
				AccessModifier.Private,
				AccessModifier.Default
			});
			accessOrders.Add(JavaLanguage.Instance, new AccessModifier[] {
				AccessModifier.Public,
				AccessModifier.Protected,
				AccessModifier.Private,
				AccessModifier.Default
			});

			modifierOrders.Add(CSharpLanguage.Instance, new ClassModifier[] {
				ClassModifier.None,
				ClassModifier.Abstract,
				ClassModifier.Sealed,
				ClassModifier.Static
			});
			modifierOrders.Add(JavaLanguage.Instance, new ClassModifier[] {
				ClassModifier.None,
				ClassModifier.Abstract,
				ClassModifier.Sealed,
				ClassModifier.Static
			});
		}

		public MainForm()
		{
			Init();

			if (Settings.LoadLastProject && !string.IsNullOrEmpty(Settings.LastProject))
				LoadProject(Settings.LastProject);
			else
				project.NewProject();
		}

		public MainForm(string fileName)
		{
			Init();
			LoadProject(fileName);
		}

		private void Init()
		{
			InitializeComponent();
			LoadWindowSettings();
			if (MonoHelper.IsRunningOnMono)
				this.AllowDrop = false;

			project = new Project(Settings.DefaultLanguage);
			diagram = new DiagramControl(project);
			diagram.Dock = DockStyle.Fill;
			diagram.ZoomChanged += new EventHandler(diagram_ZoomChanged);
			toolStripContainer.ContentPanel.Controls.Add(diagram);

			optionsDialog = new OptionsDialog();
			lblStatus.Text = Strings.GetString("ready");

			project.FileStateChanged += new EventHandler(project_FileStateChanged);
			project.LanguageChanged += delegate { UpdateLanguageChanges(); };
			project.RelationAdded += new RelationEventHandler(project_RelationAdded);
			diagram.SelectionChanged += new System.EventHandler(this.diagram_SelectionChanged);
			optionsDialog.Applied += new EventHandler(optionsDialog_Apply);
			optionsDialog.CurrentStyleChanged += new EventHandler(optionsDialog_CurrentStyleChanged);
			Strings.LanguageChanged += new EventHandler(Strings_LanguageChanged);

			LoadPlugins();
			UpdateTexts();
			UpdateLanguageChanges();
#if !DEBUG
			AppDomain.CurrentDomain.UnhandledException += new UnhandledExceptionEventHandler(AppDomain_UnhandledException);
#endif
		}

		private void AppDomain_UnhandledException(object sender, UnhandledExceptionEventArgs e)
		{
			if (e.IsTerminating) {
				string fileName = "backup" + DateTime.Now.ToString("yyyyMMddHHmmss") +
					project.Language.DefaultFileExtension;
				string filePath = Path.Combine(Application.StartupPath, fileName);

				project.Save(filePath);
				Exception ex = (Exception) e.ExceptionObject;
				CreateCrashLog(ex);

				MessageBox.Show(
					Strings.GetString("program_terminates"), Strings.GetString("critical_error"),
					MessageBoxButtons.OK, MessageBoxIcon.Error);
				
				System.Diagnostics.Process.GetCurrentProcess().Kill();
			}
		}

		private void CreateCrashLog(Exception exception)
		{
			StreamWriter writer = null;

			try {
				string filePath = Path.Combine(Application.StartupPath, "crash.log");
				writer = new StreamWriter(filePath);

				writer.WriteLine(Strings.GetString("send_log_file", Program.MailAddress));
				writer.WriteLine();
				writer.WriteLine("Version: {0}", Program.GetVersionString());
				writer.WriteLine("Mono: {0}", MonoHelper.IsRunningOnMono ? "yes" : "no");
				
				writer.WriteLine();
				writer.WriteLine(exception.Message);
				Exception innerException = exception.InnerException;
				while (innerException != null) {
					writer.WriteLine(innerException.Message);
					innerException = innerException.InnerException;
				}

				writer.WriteLine();
				writer.WriteLine(exception.StackTrace);
			}
			catch {
				// Do nothing
			}
			finally {
				if (writer != null)
					writer.Close();
			}
		}

		private void UpdateTexts()
		{
			// File menu
			mnuFile.Text = Strings.GetString("menu_file");
			mnuNew.Text = Strings.GetString("menu_add_new");
			mnuNewCSharpDiagram.Text = Strings.GetString("menu_csharp_diagram");
			mnuNewJavaDiagram.Text = Strings.GetString("menu_java_diagram");
			mnuOpen.Text = Strings.GetString("menu_open");
			mnuOpenFile.Text = Strings.GetString("menu_new_file");
			mnuSave.Text = Strings.GetString("menu_save");
			mnuSaveAs.Text = Strings.GetString("menu_save_as");
			mnuImport.Text = Strings.GetString("menu_import");
			mnuExport.Text = Strings.GetString("menu_export");
			mnuGenerateCode.Text = Strings.GetString("menu_generate_code");
			mnuPrint.Text = Strings.GetString("menu_print");
			mnuExit.Text = Strings.GetString("menu_exit");

			// Edit menu
			mnuEdit.Text = Strings.GetString("menu_edit");
			mnuUndo.Text = Strings.GetString("menu_undo");
			mnuRedo.Text = Strings.GetString("menu_redo");
			mnuCut.Text = Strings.GetString("menu_cut");
			mnuPaste.Text = Strings.GetString("menu_paste");
			mnuDelete.Text = Strings.GetString("menu_delete");
			mnuSelectAll.Text = Strings.GetString("menu_select_all");

			// Diagram menu
			mnuDiagram.Text = Strings.GetString("menu_diagram");
			mnuAddNewElement.Text = Strings.GetString("menu_new");
			mnuNewClass.Text = Strings.GetString("menu_class");
			mnuNewStructure.Text = Strings.GetString("menu_struct");
			mnuNewInterface.Text = Strings.GetString("menu_interface");
			mnuNewEnum.Text = Strings.GetString("menu_enum");
			mnuNewDelegate.Text = Strings.GetString("menu_delegate");
			mnuNewComment.Text = Strings.GetString("menu_comment");
			mnuNewAssociation.Text = Strings.GetString("menu_association");
			mnuNewComposition.Text = Strings.GetString("menu_composition");
			mnuNewAggregation.Text = Strings.GetString("menu_aggregation");
			mnuNewGeneralization.Text = Strings.GetString("menu_generalization");
			mnuNewRealization.Text = Strings.GetString("menu_realization");
			mnuNewDependency.Text = Strings.GetString("menu_dependency");
			mnuNewNesting.Text = Strings.GetString("menu_nesting");
			mnuNewCommentRelation.Text = Strings.GetString("menu_comment_relation");
			mnuMembersFormat.Text = Strings.GetString("menu_members_format");
			mnuShowType.Text = Strings.GetString("menu_type");
			mnuShowParameters.Text = Strings.GetString("menu_parameters");
			mnuShowParameterNames.Text = Strings.GetString("menu_parameter_names");
			mnuShowInitialValue.Text = Strings.GetString("menu_initial_value");			
			mnuAutoZoom.Text = Strings.GetString("menu_auto_zoom");
			mnuDiagramSize.Text = Strings.GetString("menu_diagram_size");
			mnuSaveAsImage.Text = Strings.GetString("menu_save_as_image");
			mnuOptions.Text = Strings.GetString("menu_options");

			// Format menu
			mnuFormat.Text = Strings.GetString("menu_format");
			mnuAlign.Text = Strings.GetString("menu_align");
			mnuAlignTop.Text = Strings.GetString("menu_align_top");
			mnuAlignLeft.Text = Strings.GetString("menu_align_left");
			mnuAlignBottom.Text = Strings.GetString("menu_align_bottom");
			mnuAlignRight.Text = Strings.GetString("menu_align_right");
			mnuAlignHorizontal.Text = Strings.GetString("menu_align_horizontal");
			mnuAlignVertical.Text = Strings.GetString("menu_align_vertical");
			mnuMakeSameSize.Text = Strings.GetString("menu_make_same_size");
			mnuSameWidth.Text = Strings.GetString("menu_same_width");
			mnuSameHeight.Text = Strings.GetString("menu_same_height");
			mnuSameSize.Text = Strings.GetString("menu_same_size");
			mnuAutoWidth.Text = Strings.GetString("menu_auto_width");
			mnuAutoHeight.Text = Strings.GetString("menu_auto_height");
			mnuCollapseAll.Text = Strings.GetString("menu_collapse_all");
			mnuExpandAll.Text = Strings.GetString("menu_expand_all");

			// Plugins menu
			mnuPlugins.Text = Strings.GetString("menu_plugins");
			foreach (ToolStripItem menu in mnuPlugins.DropDownItems) {
				IPlugin plugin = (IPlugin) menu.Tag;
				menu.ToolTipText = Strings.GetString("plugin_tooltip", plugin.Name, plugin.Author);
			}

			// Help menu
			mnuHelp.Text = Strings.GetString("menu_help");
			mnuContents.Text = Strings.GetString("menu_contents");
			mnuCheckForUpdates.Text = Strings.GetString("menu_check_for_updates");
			mnuAbout.Text = Strings.GetString("menu_about");

			// Toolbar
			toolNewCSharpDiagram.Text = Strings.GetString("menu_csharp_diagram");
			toolNewJavaDiagram.Text = Strings.GetString("menu_java_diagram");
			toolSave.Text = Strings.GetString("save");
			toolPrint.Text = Strings.GetString("print");
			toolZoomIn.Text = Strings.GetString("zoom_in");
			toolZoomOut.Text = Strings.GetString("zoom_out");
			toolAutoZoom.Text = Strings.GetString("auto_zoom");
			toolNewClass.Text = Strings.GetString("add_new_class");
			toolNewStruct.Text = Strings.GetString("add_new_struct");
			toolNewInterface.Text = Strings.GetString("add_new_interface");
			toolNewEnum.Text = Strings.GetString("add_new_enum");
			toolNewDelegate.Text = Strings.GetString("add_new_delegate");
			toolNewComment.Text = Strings.GetString("add_new_comment");
			toolNewAssociation.Text = Strings.GetString("add_new_association");
			toolNewComposition.Text = Strings.GetString("add_new_composition");
			toolNewAggregation.Text = Strings.GetString("add_new_aggregation");
			toolNewGeneralization.Text = Strings.GetString("add_new_generalization");
			toolNewRealization.Text = Strings.GetString("add_new_realization");
			toolNewDependency.Text = Strings.GetString("add_new_dependency");
			toolNewNesting.Text = Strings.GetString("add_new_nesting");
			toolNewCommentRelation.Text = Strings.GetString("add_new_comment_relation");
			toolDelete.Text = Strings.GetString("delete_selected_items");
			lblName.Text = Strings.GetString("name:");
			lblAccess.Text = Strings.GetString("access:");
			lblModifier.Text = Strings.GetString("modifier:");
		}

		private void LoadPlugins()
		{
			try {
				string pluginsPath = Path.Combine(Application.StartupPath, "plugins");
				if (!Directory.Exists(pluginsPath))
					return;

				DirectoryInfo directory = new DirectoryInfo(pluginsPath);

				foreach (FileInfo file in directory.GetFiles("*.dll")) {
					Assembly assembly = Assembly.LoadFile(file.FullName);
					LoadPlugin(assembly);
				}
			}
			catch (Exception ex) {
				MessageBox.Show(Strings.GetString("error_could_not_load_plugins", ex.Message));
			}

			if (projectPlugins.Count > 0 || diagramPlugins.Count > 0) {
				mnuPlugins.Visible = true;

				foreach (IProjectPlugin plugin in projectPlugins) {
					ToolStripMenuItem menu = new ToolStripMenuItem();
					menu.Text = plugin.MenuText;
					menu.Tag = plugin;
					menu.Click += new EventHandler(delegate {
						try {
							IProjectPlugin taggedPlugin = menu.Tag as IProjectPlugin;
							if (taggedPlugin != null)
								taggedPlugin.Launch(project);
						}
						catch (Exception ex) {
							MessageBox.Show(ex.Message, Strings.GetString("unknown_error"),
								MessageBoxButtons.OK, MessageBoxIcon.Error);
						}
					});
					mnuPlugins.DropDownItems.Add(menu);
				}
				foreach (IDiagramPlugin plugin in diagramPlugins) {
					ToolStripMenuItem menu = new ToolStripMenuItem();
					menu.Text = plugin.MenuText;
					menu.Tag = plugin;
					menu.Click += new EventHandler(delegate {
						try {
							IDiagramPlugin taggedPlugin = menu.Tag as IDiagramPlugin;
							if (taggedPlugin != null)
								taggedPlugin.Launch(diagram);
						}
						catch (Exception ex) {
							MessageBox.Show(ex.Message, Strings.GetString("unknown_error"),
								MessageBoxButtons.OK, MessageBoxIcon.Error);
						}
					});
					mnuPlugins.DropDownItems.Add(menu);
				}
			}
		}

		private void LoadCorePlugin(IProjectPlugin plugin)
		{
			try {
				plugin.Launch(project);
			}
			catch (Exception ex) {
				MessageBox.Show(ex.Message, Strings.GetString("unknown_error"),
					MessageBoxButtons.OK, MessageBoxIcon.Error);
			}
		}

		private void LoadPlugin(Assembly assembly)
		{
			try {
				foreach (Type type in assembly.GetTypes()) {
					if (type.GetInterface("IProjectPlugin") != null) {
						IProjectPlugin plugin = (IProjectPlugin) Activator.CreateInstance(type);
						projectPlugins.Add(plugin);
					}
					else if (type.GetInterface("IDiagramPlugin") != null) {
						IDiagramPlugin plugin = 
							(IDiagramPlugin) Activator.CreateInstance(type);
						diagramPlugins.Add(plugin);
					}
				}
			}
			catch (Exception ex) {
				MessageBox.Show(Strings.GetString("error_could_not_load_plugins", ex.Message));
			}
		}

		private void LoadProject(string fileName)
		{
			try {
				this.Cursor = Cursors.WaitCursor;
				this.SuspendLayout();
				project.Load(fileName);
			}
			catch (Exception ex) {
				MessageBox.Show(Strings.GetString("error") + ": " + ex.Message,
					Strings.GetString("load"), MessageBoxButtons.OK, MessageBoxIcon.Error);
			}
			finally {
				this.ResumeLayout();
				this.Cursor = Cursors.Default;
			}
		}

		private Control PanelFromPosition(DockStyle dockStyle)
		{
			switch (dockStyle) {
				case DockStyle.Left:
					return toolStripContainer.LeftToolStripPanel;

				case DockStyle.Right:
					return toolStripContainer.RightToolStripPanel;

				case DockStyle.Bottom:
					return toolStripContainer.BottomToolStripPanel;

				case DockStyle.Top:
				default:
					return toolStripContainer.TopToolStripPanel;
			}
		}

		private DockStyle PositionFromPanel(Control control)
		{
			if (control == toolStripContainer.LeftToolStripPanel)
				return DockStyle.Left;

			if (control == toolStripContainer.RightToolStripPanel)
				return DockStyle.Right;

			if (control == toolStripContainer.BottomToolStripPanel)
				return DockStyle.Bottom;

			return DockStyle.Top;
		}

		private void LoadWindowSettings()
		{
			if (MonoHelper.IsRunningOnMono)
				return;

			Location = WindowSettings.Default.WindowPosition;
			Size = WindowSettings.Default.WindowSize;
			if (WindowSettings.Default.IsWindowMaximized)
				WindowState = FormWindowState.Maximized;

			standardToolStrip.Parent =
				PanelFromPosition(WindowSettings.Default.StandardToolBarPosition);
			elementsToolStrip.Parent =
				PanelFromPosition(WindowSettings.Default.ElementsToolBarPosition);
			typeDetailsToolStrip.Parent =
				PanelFromPosition(WindowSettings.Default.TypeDetailsToolBarPosition);
			
			standardToolStrip.Location = WindowSettings.Default.StandardToolBarLocation;
			elementsToolStrip.Location = WindowSettings.Default.ElementsToolBarLocation;
			typeDetailsToolStrip.Location = WindowSettings.Default.TypeDetailsToolBarLocation;
		}

		private void SaveWindowSettings()
		{
			if (MonoHelper.IsRunningOnMono)
				return;

			if (WindowState == FormWindowState.Maximized) {
				WindowSettings.Default.IsWindowMaximized = true;
			}
			else {
				WindowSettings.Default.IsWindowMaximized = false;
				if (WindowState == FormWindowState.Normal)
					WindowSettings.Default.WindowSize = Size;
				if (WindowState == FormWindowState.Normal)
					WindowSettings.Default.WindowPosition = Location;
			}

			WindowSettings.Default.StandardToolBarPosition = PositionFromPanel(
				standardToolStrip.Parent);
			WindowSettings.Default.ElementsToolBarPosition = PositionFromPanel(
				elementsToolStrip.Parent);
			WindowSettings.Default.TypeDetailsToolBarPosition = PositionFromPanel(
				typeDetailsToolStrip.Parent);

			WindowSettings.Default.StandardToolBarLocation = standardToolStrip.Location;
			WindowSettings.Default.ElementsToolBarLocation = elementsToolStrip.Location;
			WindowSettings.Default.TypeDetailsToolBarLocation = typeDetailsToolStrip.Location;

			WindowSettings.Default.Save();
		}

		protected override void OnClosing(System.ComponentModel.CancelEventArgs e)
		{
			base.OnClosing(e);

			if (!project.CanClose())
				e.Cancel = true;
			else
				SaveWindowSettings();
		}

		protected override void OnDragEnter(DragEventArgs e)
		{
			base.OnDragEnter(e);

			if (e.Data.GetDataPresent(DataFormats.FileDrop))
				e.Effect = DragDropEffects.Copy;
			else
				e.Effect = DragDropEffects.None;
		}

		protected override void OnDragDrop(DragEventArgs e)
		{
			base.OnDragDrop(e);

			if (e.Data.GetDataPresent(DataFormats.FileDrop)) {
				string[] files = (string[]) e.Data.GetData(DataFormats.FileDrop);
				if (files.Length > 0)
					LoadProject(files[0]);
			}
		}

		private void UpdateHeader()
		{
			this.Text = "NClass - " + project.GetTitleString(Settings.ShowFullFilePath);
		}

		private void UpdateStatusBar()
		{
			if (!diagram.HasSelectedElement)
				lblStatus.Text = Strings.GetString("ready");
			else if (diagram.SingleSelection)
				lblStatus.Text = diagram.FirstSelectedElement.ToString();
			else
				lblStatus.Text = Strings.GetString("items_selected", diagram.SelectedElementCount);
		}

		private void UpdateWindow()
		{
			UpdateHeader();
			diagram.RefreshDiagram();
		}

		private void UpdateModifiersToolBar()
		{
			int selectedItemCount = diagram.SelectedShapeCount;
			DiagramElement selectedElement = diagram.FirstSelectedElement;

			if (selectedItemCount == 1 && selectedElement is TypeShape) {
				TypeBase type = ((TypeShape) selectedElement).TypeBase;

				txtName.Text = type.Name;
				txtName.Enabled = true;
				SetAccessLabel(type.AccessModifier);
				cboAccess.Enabled = true;

				if (type is ClassType) {
					lblModifier.Text = Strings.GetString("modifier:");
					SetModifierLabel(((ClassType) type).Modifier);
					cboModifier.Enabled = true;
					cboModifier.Visible = true;
					lblModifier.Visible = true;
					lblReturnType.Visible = false;
					txtReturnType.Visible = false;
				}
				else if (type is DelegateType) {
					lblReturnType.Text = Strings.GetString("return_type:");
					txtReturnType.Text = ((DelegateType) type).ReturnType;
					txtReturnType.Enabled = true;
					txtReturnType.Visible = true;
					lblReturnType.Visible = true;
					cboModifier.Visible = false;
					lblModifier.Visible = false;
				}
				else {
					lblModifier.Text = Strings.GetString("modifier:");
					cboModifier.Text = null;
					cboModifier.Enabled = false;
					cboModifier.Visible = true;
					lblModifier.Visible = true;
					txtReturnType.Visible = false;
					lblReturnType.Visible = false;
				}
			}
			else {
				txtName.Text = null;
				txtName.Enabled = false;
				cboAccess.Text = null;
				cboAccess.Enabled = false;
				cboModifier.Text = null;
				cboModifier.Enabled = false;
				cboModifier.Visible = true;
				lblModifier.Visible = true;
				txtReturnType.Visible = false;
				lblReturnType.Visible = false;
			}
		}

		private void SetAccessLabel(AccessModifier access)
		{
			AccessModifier[] accessOrder = accessOrders[project.Language];

			for (int i = 0; i < accessOrder.Length; i++) {
				if (accessOrder[i] == access) {
					cboAccess.SelectedIndex = i;
					return;
				}
			}

			cboAccess.SelectedIndex = 0;
		}

		private void SetModifierLabel(ClassModifier modifier)
		{
			ClassModifier[] modifierOrder = modifierOrders[project.Language];

			for (int i = 0; i < modifierOrder.Length; i++) {
				if (modifierOrder[i] == modifier) {
					cboModifier.SelectedIndex = i;
					return;
				}
			}

			cboModifier.SelectedIndex = 0;
		}

		private void UpdateLanguageChanges()
		{
			Settings.DefaultLanguage = project.Language;

			mnuNewInterface.Visible = project.Language.SupportsInterfaces;
			toolNewInterface.Visible = project.Language.SupportsInterfaces;
			mnuNewStructure.Visible = project.Language.SupportsStructures;
			toolNewStruct.Visible = project.Language.SupportsStructures;
			mnuNewEnum.Visible = project.Language.SupportsEnums;
			toolNewEnum.Visible = project.Language.SupportsEnums;
			mnuNewDelegate.Visible = project.Language.SupportsDelegates;
			toolNewDelegate.Visible = project.Language.SupportsDelegates;

			UpdateComboBoxes(project.Language);
			lblLanguage.Text = Strings.GetString("language") + ": " + project.Language.Name;
		}

		private void UpdateComboBoxes(Language language)
		{
			cboAccess.Items.Clear();
			cboModifier.Items.Clear();

			if (language == CSharp.CSharpLanguage.Instance) {
				cboAccess.Items.Add("Public");
				cboAccess.Items.Add("Protected Internal");
				cboAccess.Items.Add("Internal");
				cboAccess.Items.Add("Protected");
				cboAccess.Items.Add("Private");
				cboAccess.Items.Add("Default");

				cboModifier.Items.Add("None");
				cboModifier.Items.Add("Abstract");
				cboModifier.Items.Add("Sealed");
				cboModifier.Items.Add("Static");
			}
			else {
				cboAccess.Items.Add("Public");
				cboAccess.Items.Add("Protected");
				cboAccess.Items.Add("Private");
				cboAccess.Items.Add("Default");

				cboModifier.Items.Add("None");
				cboModifier.Items.Add("Abstract");
				cboModifier.Items.Add("Sealed");
				cboModifier.Items.Add("Static");
			}
		}

		private void project_FileStateChanged(object sender, EventArgs e)
		{
			UpdateHeader();
			if (!project.IsUntitled)
				Settings.AddRecentFile(project.ProjectFile);
		}

		private void project_RelationAdded(object sender, RelationEventArgs e)
		{
			toolNewAssociation.Checked = false;
			toolNewComposition.Checked = false;
			toolNewAggregation.Checked = false;
			toolNewGeneralization.Checked = false;
			toolNewRealization.Checked = false;
			toolNewDependency.Checked = false;
			toolNewNesting.Checked = false;
			toolNewCommentRelation.Checked = false;
		}

		private void diagram_SelectionChanged(object sender, EventArgs e)
		{
			UpdateStatusBar();
			UpdateModifiersToolBar();
			toolDelete.Enabled = diagram.HasSelectedElement;
		}

		private void Strings_LanguageChanged(object sender, EventArgs e)
		{
			UpdateTexts();
			UpdateStatusBar();
		}

		private void OpenRecentFile_Click(object sender, EventArgs e)
		{
			if (sender is ToolStripItem && ((ToolStripItem) sender).Tag is int) {
				int index = (int) ((ToolStripItem) sender).Tag;
				if (index >= 0 && index < Settings.RecentFileCount)
					LoadProject(Settings.GetRecentFile(index));
			}
		}

		private void optionsDialog_Apply(object sender, EventArgs e)
		{
			UpdateWindow();
		}

		private void optionsDialog_CurrentStyleChanged(object sender, EventArgs e)
		{
			diagram.RefreshDiagram();
		}

		private void diagram_ZoomChanged(object sender, EventArgs e)
		{
			toolZoom.ZoomValue = diagram.Zoom;
			toolZoomValue.Text = diagram.ZoomPercentage + "%";
		}

		#region File menu eventhandlers

		private void mnuNewCSharpDiagram_Click(object sender, EventArgs e)
		{
			project.NewProject(CSharpLanguage.Instance);
			UpdateComboBoxes(project.Language);
		}

		private void mnuNewJavaDiagram_Click(object sender, EventArgs e)
		{
			project.NewProject(JavaLanguage.Instance);
			UpdateComboBoxes(project.Language);
		}

		private void mnuOpenFile_Click(object sender, EventArgs e)
		{
			project.Load();
		}

		private void mnuOpen_DropDownOpening(object sender, EventArgs e)
		{
			foreach (ToolStripItem item in mnuOpen.DropDownItems) {
				if (item.Tag is int) {
					int index = (int) item.Tag;

					if (index < Settings.RecentFileCount) {
						item.Text = Settings.GetRecentFile(index);
						item.Visible = true;
					}
					else {
						item.Visible = false;
					}
				}
			}

			sepOpenFile.Visible = (Settings.RecentFileCount > 0);
		}

		private void mnuSave_Click(object sender, EventArgs e)
		{
			project.Save();
		}

		private void mnuSaveAs_Click(object sender, EventArgs e)
		{
			project.SaveAs();
		}

		private void mnuExport_DropDownOpening(object sender, EventArgs e)
		{
			mnuGenerateCode.Enabled = !project.IsEmpty;
		}

		private void mnuGenerateCode_Click(object sender, EventArgs e)
		{
			using (CodeGenerator.Dialog dialog = new CodeGenerator.Dialog())
			{
				try {
					dialog.ShowDialog(project);
				}
				catch (Exception ex) {
					MessageBox.Show(ex.Message, Strings.GetString("unknown_error"),
						MessageBoxButtons.OK, MessageBoxIcon.Error);
				}
			}
		}

		private void mnuPrint_Click(object sender, EventArgs e)
		{
			diagram.Print();
		}

		private void mnuExit_Click(object sender, EventArgs e)
		{
			this.Close();
		}

		#endregion

		#region Edit menu eventhandlers

		private void mnuEdit_DropDownOpening(object sender, EventArgs e)
		{
			//TODO: mnuEdit_DropDownOpening
			/*
			mnuUndo.Enabled = diagram.CanUndo;
			mnuRedo.Enabled = diagram.CanRedo;
			mnuCut.Enabled = (diagram.SelectedItemCount > 0);
			mnuPaste.Enabled = diagram.CanPaste;
			*/
			mnuCut.Enabled = false;
			mnuPaste.Enabled = false;
			mnuDelete.Enabled = diagram.HasSelectedElement;
			mnuSelectAll.Enabled = !diagram.IsEmpty;
		}

		private void mnuEdit_DropDownClosed(object sender, EventArgs e)
		{
			mnuUndo.Enabled = true;
			mnuRedo.Enabled = true;
			mnuCut.Enabled = true;
			mnuPaste.Enabled = true;
			mnuDelete.Enabled = true;
			mnuSelectAll.Enabled = true;
		}

		private void mnuUndo_Click(object sender, EventArgs e)
		{
			//UNDONE: mnuUndo_Click
		}

		private void mnuRedo_Click(object sender, EventArgs e)
		{
			//UNDONE: mnuRedo_Click
		}

		private void mnuCut_Click(object sender, EventArgs e)
		{
			//UNDONE: mnuCut_Click
		}

		private void mnuPaste_Click(object sender, EventArgs e)
		{
			//UNDONE: mnuPaste_Click
		}

		private void mnuDelete_Click(object sender, EventArgs e)
		{
			diagram.DeleteSelectedElements();
		}

		private void mnuSelectAll_Click(object sender, EventArgs e)
		{
			diagram.SelectAll();
		}

		#endregion

		#region Diagram menu eventhandlers

		private void mnuDiagram_DropDownOpening(object sender, EventArgs e)
		{
			mnuSaveAsImage.Enabled = !diagram.IsEmpty;
		}

		private void mnuNewClass_Click(object sender, EventArgs e)
		{
			project.AddClass();
		}

		private void mnuNewStructure_Click(object sender, EventArgs e)
		{
			if (project.Language.SupportsStructures)
				project.AddStructure();
		}

		private void mnuNewInterface_Click(object sender, EventArgs e)
		{
			if (project.Language.SupportsInterfaces)
				project.AddInterface();
		}

		private void mnuNewEnum_Click(object sender, EventArgs e)
		{
			if (project.Language.SupportsEnums)
				project.AddEnum();
		}

		private void mnuNewDelegate_Click(object sender, EventArgs e)
		{
			if (project.Language.SupportsDelegates)
				project.AddDelegate();
		}

		private void mnuNewComment_Click(object sender, EventArgs e)
		{
			project.AddComment();
		}

		private void mnuNewAssociation_Click(object sender, EventArgs e)
		{
			diagram.CreateNewAssociation();
			toolNewAssociation.Checked = true;
		}

		private void mnuNewComposition_Click(object sender, EventArgs e)
		{
			diagram.CreateNewComposition();
			toolNewComposition.Checked = true;
		}

		private void mnuNewAggregation_Click(object sender, EventArgs e)
		{
			diagram.CreateNewAggregation();
			toolNewAggregation.Checked = true;
		}

		private void mnuNewGeneralization_Click(object sender, EventArgs e)
		{
			diagram.CreateNewGeneralization();
			toolNewGeneralization.Checked = true;
		}

		private void mnuNewRealization_Click(object sender, EventArgs e)
		{
			diagram.CreateNewRealization();
			toolNewRealization.Checked = true;
		}

		private void mnuNewDependency_Click(object sender, EventArgs e)
		{
			diagram.CreateNewDependency();
			toolNewDependency.Checked = true;
		}

		private void mnuNewNesting_Click(object sender, EventArgs e)
		{
			diagram.CreateNewNesting();
			toolNewNesting.Checked = true;
		}

		private void mnuNewCommentRelation_Click(object sender, EventArgs e)
		{
			diagram.CreateNewCommentRelationsip();
			toolNewCommentRelation.Checked = true;
		}

		private void mnuMembersFormat_DropDownOpening(object sender, EventArgs e)
		{
			mnuShowType.Checked = Settings.Diagram.ShowType;
			mnuShowParameters.Checked = Settings.Diagram.ShowParameters;
			mnuShowParameterNames.Checked = Settings.Diagram.ShowParameterNames;
			mnuShowInitialValue.Checked = Settings.Diagram.ShowInitialValue;
		}

		private void mnuShowType_Click(object sender, EventArgs e)
		{
			Settings.Diagram.ShowType = ((ToolStripMenuItem) sender).Checked;
			diagram.RefreshDiagram();
		}

		private void mnuShowParameters_Click(object sender, EventArgs e)
		{
			Settings.Diagram.ShowParameters = ((ToolStripMenuItem) sender).Checked;
			diagram.RefreshDiagram();
		}

		private void mnuShowParameterNames_Click(object sender, EventArgs e)
		{
			Settings.Diagram.ShowParameterNames = ((ToolStripMenuItem) sender).Checked;
			diagram.RefreshDiagram();
		}

		private void mnuShowInitialValue_Click(object sender, EventArgs e)
		{
			Settings.Diagram.ShowInitialValue = ((ToolStripMenuItem) sender).Checked;
			diagram.RefreshDiagram();
		}

		private void mnuAutoZoom_Click(object sender, EventArgs e)
		{
			diagram.AutoZoom();
		}

		private void mnuDiagramSize_Click(object sender, EventArgs e)
		{
			using (DiagramSizeDialog dialog = new DiagramSizeDialog(
				diagram.DiagramSize, diagram.GetMinimumDiagramSize()))
			{
				if (dialog.ShowDialog() == DialogResult.OK)
					diagram.DiagramSize = dialog.DiagramSize;
			}
		}

		private void mnuSaveAsImage_Click(object sender, EventArgs e)
		{
			diagram.SaveAsImage();
		}

		private void mnuOptions_Click(object sender, EventArgs e)
		{
			optionsDialog.ShowDialog();
		}

		#endregion

		#region Format menu eventhandlers

		private void mnuFormat_DropDownOpening(object sender, EventArgs e)
		{
			bool multiselection = (diagram.SelectedShapeCount >= 2);
			mnuAlign.Enabled = multiselection;
			mnuMakeSameSize.Enabled = multiselection;
			mnuAutoWidth.Enabled = diagram.HasSelectedShape;
			mnuAutoHeight.Enabled = diagram.HasSelectedShape;
		}

		private void mnuAlign_DropDownOpening(object sender, EventArgs e)
		{
			bool canAlign = (diagram.SelectedShapeCount >= 2);

			mnuAlignLeft.Enabled = canAlign;
			mnuAlignRight.Enabled = canAlign;
			mnuAlignTop.Enabled = canAlign;
			mnuAlignBottom.Enabled = canAlign;
			mnuAlignHorizontal.Enabled = canAlign;
			mnuAlignVertical.Enabled = canAlign;
		}

		private void mnuAlignTop_Click(object sender, EventArgs e)
		{
			diagram.AlignTop();
		}

		private void mnuAlignLeft_Click(object sender, EventArgs e)
		{
			diagram.AlignLeft();
		}

		private void mnuAlignBottom_Click(object sender, EventArgs e)
		{
			diagram.AlignBottom();
		}

		private void mnuAlignRight_Click(object sender, EventArgs e)
		{
			diagram.AlignRight();
		}

		private void mnuAlignHorizontal_Click(object sender, EventArgs e)
		{
			diagram.AlignHorizontal();
		}

		private void mnuAlignVertical_Click(object sender, EventArgs e)
		{
			diagram.AlignVertical();
		}

		private void mnuMakeSameSize_DropDownOpening(object sender, EventArgs e)
		{
			bool canResize = (diagram.SelectedShapeCount >= 2);

			mnuSameWidth.Enabled = canResize;
			mnuSameHeight.Enabled = canResize;
			mnuSameSize.Enabled = canResize;
		}

		private void mnuSameWidth_Click(object sender, EventArgs e)
		{
			diagram.AdjustToSameWidth();
		}

		private void mnuSameHeight_Click(object sender, EventArgs e)
		{
			diagram.AdjustToSameHeight();
		}

		private void mnuSameSize_Click(object sender, EventArgs e)
		{
			diagram.AdjustToSameSize();
		}

		private void mnuAutoWidth_Click(object sender, EventArgs e)
		{
			diagram.AutoWidthOfSelectedShapes();
		}

		private void mnuAutoHeight_Click(object sender, EventArgs e)
		{
			diagram.AutoHeightOfSelectedShapes();
		}

		private void mnuCollapseAll_Click(object sender, EventArgs e)
		{
			diagram.CollapseAll();
		}

		private void mnuExpandAll_Click(object sender, EventArgs e)
		{
			diagram.ExpandAll();
		}

		#endregion

		#region Help menu eventhandlers

		private void mnuContents_Click(object sender, EventArgs e)
		{
			MessageBox.Show(Strings.GetString("not_implemented"), "NClass",
				MessageBoxButtons.OK, MessageBoxIcon.Information);
		}

		private void mnuCheckForUpdates_Click(object sender, EventArgs e)
		{
			UpdatesChecker.CheckForUpdates();
		}

		private void mnuAbout_Click(object sender, EventArgs e)
		{
			using (AboutDialog dialog = new AboutDialog())
				dialog.ShowDialog();
		}
		
		#endregion

		#region Standard toolbar eventhandlers

		private void toolNew_ButtonClick(object sender, EventArgs e)
		{
			project.NewProject();
		}

		private void toolOpen_DropDownOpening(object sender, EventArgs e)
		{
			foreach (ToolStripItem item in toolOpen.DropDownItems) {
				if (item.Tag is int) {
					int index = (int) item.Tag;

					if (index < Settings.RecentFileCount) {
						item.Text = Settings.GetRecentFile(index);
						item.Visible = true;
					}
					else {
						item.Visible = false;
					}
				}
			}
		}

		private void toolZoomIn_Click(object sender, EventArgs e)
		{
			diagram.ChangeZoom(true);
		}

		private void toolZoomOut_Click(object sender, EventArgs e)
		{
			diagram.ChangeZoom(false);
		}

		private void toolZoom_ZoomValueChanged(object sender, EventArgs e)
		{
			diagram.ChangeZoom(toolZoom.ZoomValue);
		}

		#endregion

		#region Details toolbar eventhandlers

		private void txtName_KeyPress(object sender, KeyPressEventArgs e)
		{
			if (e.KeyChar == '<') {
				int cursorIndex = txtName.SelectionStart;

				if (cursorIndex == txtName.Text.Length) {
					txtName.Text = txtName.Text.Insert(cursorIndex, "<T>");
					txtName.SelectionStart = cursorIndex + 1;
					txtName.SelectionLength = 1;
					e.Handled = true;
				}
			}
		}

		private void txtName_TextChanged(object sender, EventArgs e)
		{
			if (diagram.SingleSelection && diagram.FirstSelectedElement is TypeShape) {
				TypeBase type = ((TypeShape) diagram.FirstSelectedElement).TypeBase;

				if (type.Name != txtName.Text) {
					try {
						type.Name = txtName.Text;
						txtName.ForeColor = SystemColors.WindowText;
						UpdateStatusBar();
					}
					catch (BadSyntaxException) {
						txtName.ForeColor = Color.Red;
					}
				}
				else {
					txtName.ForeColor = SystemColors.WindowText;
				}
			}
		}

		private void txtName_Validated(object sender, EventArgs e)
		{
			txtName.ForeColor = SystemColors.WindowText;
		}

		private void cboAccess_SelectedIndexChanged(object sender, EventArgs e)
		{
			DiagramElement selectedElement = diagram.FirstSelectedElement;

			if (diagram.SingleSelection && selectedElement is TypeShape) {
				TypeBase type = ((TypeShape) selectedElement).TypeBase;

				int index = cboAccess.SelectedIndex;
				type.AccessModifier = accessOrders[type.Language][index];
			}
		}

		private void cboModifier_SelectedIndexChanged(object sender, EventArgs e)
		{
			DiagramElement selectedElement = diagram.FirstSelectedElement;

			if (diagram.SingleSelection && selectedElement is ClassShape) {
				ClassType type = ((ClassShape) selectedElement).ClassType;

				int index = cboModifier.SelectedIndex;
				type.Modifier = modifierOrders[type.Language][index];
			}
		}

		private void txtReturnType_TextChanged(object sender, EventArgs e)
		{
			DiagramElement selectedElement = diagram.FirstSelectedElement;

			if (diagram.SingleSelection && selectedElement is DelegateShape) {
				DelegateType _delegate = ((DelegateShape) selectedElement).DelegateType;

				if (_delegate.ReturnType != txtReturnType.Text) {
					try {
						_delegate.ReturnType = txtReturnType.Text;
						txtReturnType.ForeColor = SystemColors.WindowText;
					}
					catch (BadSyntaxException) {
						txtReturnType.ForeColor = Color.Red;
					}
				}
				else {
					txtReturnType.ForeColor = SystemColors.WindowText;
				}
			}
		}

		private void txtReturnType_Validated(object sender, EventArgs e)
		{
			txtReturnType.ForeColor = SystemColors.WindowText;
		}

		#endregion
	}
}