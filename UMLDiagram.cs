// contains code from NClass - Free class diagram editor
// Copyright (C) 2006-2007 Balazs Tihanyi
// modified by Martin C. Carlisle, US Air Force Academy
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
using System.Collections.Generic;
using System.ComponentModel;
using System.Drawing;
using System.Data;
using System.Text;
using System.Windows.Forms;
using NClass.Core;
using NClass.CSharp;
using NClass.Java;
using NClass.CodeGenerator;
using NClass.GUI.Diagram;
using NClass.Translations;
//using NClass.GUI;
using NClass;

namespace raptor
{
    public partial class UMLDiagram : UserControl
    {
        public Project project;
        public DiagramControl diagram;
        NClass.GUI.OptionsDialog optionsDialog;
        List<IProjectPlugin> projectPlugins = new List<IProjectPlugin>();
        List<IDiagramPlugin> diagramPlugins = new List<IDiagramPlugin>();

        static readonly Dictionary<Language, AccessModifier[]> accessOrders;
        static readonly Dictionary<Language, ClassModifier[]> modifierOrders;


        public UMLDiagram(UMLupdater umlupdater)
        {
            InitializeComponent();
            this.Dock = DockStyle.Fill;
            project = new Project(NClass.GUI.Settings.DefaultLanguage);
            ProjectCore.raptorUpdater = umlupdater as RAPTORUpdater;
            diagram = new DiagramControl(project);
            diagram.Dock = DockStyle.Fill;
            diagram.ZoomChanged += new EventHandler(diagram_ZoomChanged);
            toolStripContainer.ContentPanel.Controls.Add(diagram);

            optionsDialog = new NClass.GUI.OptionsDialog();
            //lblStatus.Text = Strings.GetString("ready");

            project.FileStateChanged += new EventHandler(project_FileStateChanged);
            project.LanguageChanged += delegate { UpdateLanguageChanges(); };
            project.RelationAdded += new RelationEventHandler(project_RelationAdded);
            diagram.SelectionChanged += new System.EventHandler(this.diagram_SelectionChanged);
            optionsDialog.Applied += new EventHandler(optionsDialog_Apply);
            optionsDialog.CurrentStyleChanged += new EventHandler(optionsDialog_CurrentStyleChanged);
            Strings.LanguageChanged += new EventHandler(Strings_LanguageChanged);

            //LoadPlugins();
            UpdateTexts();
            UpdateLanguageChanges();
        }
        static UMLDiagram() {
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

        private void toolStripContainer1_TopToolStripPanel_Click(object sender, EventArgs e)
        {

        }


        private void UpdateTexts()
        {

            // Toolbar
            toolZoomIn.Text = Strings.GetString("zoom_in");
            toolZoomOut.Text = Strings.GetString("zoom_out");
            toolAutoZoom.Text = Strings.GetString("auto_zoom");
            toolNewClass.Text = Strings.GetString("add_new_class");
            toolNewStruct.Text = Strings.GetString("add_new_struct");
            toolNewInterface.Text = Strings.GetString("add_new_interface");
            toolNewEnum.Text = Strings.GetString("add_new_enum");
            //toolNewDelegate.Text = Strings.GetString("add_new_delegate");
            toolNewComment.Text = Strings.GetString("add_new_comment");
            toolNewAssociation.Text = Strings.GetString("add_new_association");
            toolNewComposition.Text = Strings.GetString("add_new_composition");
            toolNewAggregation.Text = Strings.GetString("add_new_aggregation");
            toolNewGeneralization.Text = Strings.GetString("add_new_generalization");
            toolNewRealization.Text = Strings.GetString("add_new_realization");
            toolNewDependency.Text = Strings.GetString("add_new_dependency");
            toolNewNesting.Text = Strings.GetString("add_new_nesting");
            //toolNewCommentRelation.Text = Strings.GetString("add_new_comment_relation");
            toolDelete.Text = Strings.GetString("delete_selected_items");
            lblName.Text = Strings.GetString("name:");
            lblAccess.Text = Strings.GetString("access:");
            lblModifier.Text = Strings.GetString("modifier:");
        }


        private void LoadProject(string fileName)
        {
            try
            {
                this.Cursor = Cursors.WaitCursor;
                this.SuspendLayout();
                project.Load(fileName);
            }
            catch (Exception ex)
            {
                MessageBox.Show(Strings.GetString("error") + ": " + ex.Message,
                    Strings.GetString("load"), MessageBoxButtons.OK, MessageBoxIcon.Error);
            }
            finally
            {
                this.ResumeLayout();
                this.Cursor = Cursors.Default;
            }
        }

        private Control PanelFromPosition(DockStyle dockStyle)
        {
            switch (dockStyle)
            {
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

            if (e.Data.GetDataPresent(DataFormats.FileDrop))
            {
                string[] files = (string[])e.Data.GetData(DataFormats.FileDrop);
                if (files.Length > 0)
                    LoadProject(files[0]);
            }
        }



        private void UpdateWindow()
        {
            diagram.RefreshDiagram();
        }

        private void UpdateModifiersToolBar()
        {
            int selectedItemCount = diagram.SelectedShapeCount;
            DiagramElement selectedElement = diagram.FirstSelectedElement;

            if (selectedItemCount == 1 && selectedElement is TypeShape)
            {
                TypeBase type = ((TypeShape)selectedElement).TypeBase;

                txtName.Text = type.Name;
                txtName.Enabled = true;
                SetAccessLabel(type.AccessModifier);
                cboAccess.Enabled = true;

                if (type is ClassType)
                {
                    lblModifier.Text = Strings.GetString("modifier:");
                    SetModifierLabel(((ClassType)type).Modifier);
                    cboModifier.Enabled = true;
                    cboModifier.Visible = true;
                    lblModifier.Visible = true;
                    //lblReturnType.Visible = false;
                    //txtReturnType.Visible = false;
                }
                else if (type is DelegateType)
                {
                    //lblReturnType.Text = Strings.GetString("return_type:");
                    //txtReturnType.Text = ((DelegateType)type).ReturnType;
                    //txtReturnType.Enabled = true;
                    //txtReturnType.Visible = true;
                    //lblReturnType.Visible = true;
                    cboModifier.Visible = false;
                    lblModifier.Visible = false;
                }
                else
                {
                    lblModifier.Text = Strings.GetString("modifier:");
                    cboModifier.Text = null;
                    cboModifier.Enabled = false;
                    cboModifier.Visible = true;
                    lblModifier.Visible = true;
                    //txtReturnType.Visible = false;
                    //lblReturnType.Visible = false;
                }
            }
            else
            {
                txtName.Text = null;
                txtName.Enabled = false;
                cboAccess.Text = null;
                cboAccess.Enabled = false;
                cboModifier.Text = null;
                cboModifier.Enabled = false;
                cboModifier.Visible = true;
                lblModifier.Visible = true;
                //txtReturnType.Visible = false;
                //lblReturnType.Visible = false;
            }
        }

        private void SetAccessLabel(AccessModifier access)
        {
            AccessModifier[] accessOrder = accessOrders[project.Language];

            for (int i = 0; i < accessOrder.Length; i++)
            {
                if (accessOrder[i] == access)
                {
                    cboAccess.SelectedIndex = i;
                    return;
                }
            }

            cboAccess.SelectedIndex = 0;
        }

        private void SetModifierLabel(ClassModifier modifier)
        {
            ClassModifier[] modifierOrder = modifierOrders[project.Language];

            for (int i = 0; i < modifierOrder.Length; i++)
            {
                if (modifierOrder[i] == modifier)
                {
                    cboModifier.SelectedIndex = i;
                    return;
                }
            }

            cboModifier.SelectedIndex = 0;
        }

        private void UpdateLanguageChanges()
        {
            NClass.GUI.Settings.DefaultLanguage = project.Language;

            //mnuNewInterface.Visible = project.Language.SupportsInterfaces;
            toolNewInterface.Visible = project.Language.SupportsInterfaces;
            //mnuNewStructure.Visible = project.Language.SupportsStructures;
            toolNewStruct.Visible = project.Language.SupportsStructures;
            //mnuNewEnum.Visible = project.Language.SupportsEnums;
            toolNewEnum.Visible = project.Language.SupportsEnums;
            //mnuNewDelegate.Visible = project.Language.SupportsDelegates;
            //toolNewDelegate.Visible = project.Language.SupportsDelegates;

            UpdateComboBoxes(project.Language);
            //lblLanguage.Text = Strings.GetString("language") + ": " + project.Language.Name;
        }

        private void UpdateComboBoxes(Language language)
        {
            cboAccess.Items.Clear();
            cboModifier.Items.Clear();

            if (language == NClass.CSharp.CSharpLanguage.Instance)
            {
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
            else
            {
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
            //UpdateHeader();
            if (!project.IsUntitled)
                NClass.GUI.Settings.AddRecentFile(project.ProjectFile);
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
            //toolNewCommentRelation.Checked = false;
        }

        private void diagram_SelectionChanged(object sender, EventArgs e)
        {
            //UpdateStatusBar();
            UpdateModifiersToolBar();
            toolDelete.Enabled = diagram.HasSelectedElement;
        }

        private void Strings_LanguageChanged(object sender, EventArgs e)
        {
            UpdateTexts();
            //UpdateStatusBar();
        }

        private void OpenRecentFile_Click(object sender, EventArgs e)
        {
            if (sender is ToolStripItem && ((ToolStripItem)sender).Tag is int)
            {
                int index = (int)((ToolStripItem)sender).Tag;
                if (index >= 0 && index < NClass.GUI.Settings.RecentFileCount)
                    LoadProject(NClass.GUI.Settings.GetRecentFile(index));
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
            toolZoomValue.Text = diagram.ZoomPercentage.ToString() + "%";
            toolZoom.ZoomValue = diagram.Zoom;
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
            //toolNewCommentRelation.Checked = true;
        }

        /*private void mnuMembersFormat_DropDownOpening(object sender, EventArgs e)
        {
            mnuShowType.Checked = NClass.GUI.Settings.Diagram.ShowType;
            mnuShowParameters.Checked = NClass.GUI.Settings.Diagram.ShowParameters;
            mnuShowParameterNames.Checked = NClass.GUI.Settings.Diagram.ShowParameterNames;
            mnuShowInitialValue.Checked = NClass.GUI.Settings.Diagram.ShowInitialValue;
        }

        private void mnuShowType_Click(object sender, EventArgs e)
        {
            NClass.GUI.Settings.Diagram.ShowType = ((ToolStripMenuItem)sender).Checked;
            diagram.RefreshDiagram();
        }

        private void mnuShowParameters_Click(object sender, EventArgs e)
        {
            Settings.Diagram.ShowParameters = ((ToolStripMenuItem)sender).Checked;
            diagram.RefreshDiagram();
        }

        private void mnuShowParameterNames_Click(object sender, EventArgs e)
        {
            Settings.Diagram.ShowParameterNames = ((ToolStripMenuItem)sender).Checked;
            diagram.RefreshDiagram();
        }

        private void mnuShowInitialValue_Click(object sender, EventArgs e)
        {
            Settings.Diagram.ShowInitialValue = ((ToolStripMenuItem)sender).Checked;
            diagram.RefreshDiagram();
        }*/

        private void mnuAutoZoom_Click(object sender, EventArgs e)
        {
            diagram.AutoZoom();
        }

        private void mnuDiagramSize_Click(object sender, EventArgs e)
        {
            using (NClass.GUI.DiagramSizeDialog dialog = new 
                NClass.GUI.DiagramSizeDialog(
                diagram.DiagramSize, diagram.GetMinimumDiagramSize()))
            {
                if (dialog.ShowDialog() == DialogResult.OK)
                    diagram.DiagramSize = dialog.DiagramSize;
            }
        }

        private void txtName_KeyPress(object sender, KeyPressEventArgs e)
        {
            if (e.KeyChar == '<')
            {
                int cursorIndex = txtName.SelectionStart;

                if (cursorIndex == txtName.Text.Length)
                {
                    txtName.Text = txtName.Text.Insert(cursorIndex, "<T>");
                    txtName.SelectionStart = cursorIndex + 1;
                    txtName.SelectionLength = 1;
                    e.Handled = true;
                }
            }
        }

        private void txtName_TextChanged(object sender, EventArgs e)
        {
            if (diagram.SingleSelection && diagram.FirstSelectedElement is TypeShape)
            {
                TypeBase type = ((TypeShape)diagram.FirstSelectedElement).TypeBase;

                if (type.Name != txtName.Text)
                {
                    try
                    {
                        type.Name = txtName.Text;
                        txtName.ForeColor = SystemColors.WindowText;
                        if (type is ClassType)
                        {
                            ClassType ct = type as ClassType;
                            ProjectCore.raptorUpdater.renameClass(ct.raptorTab, type.Name);
                            foreach (Operation o in ct.Operations)
                            {
                                if (o is Constructor)
                                {
                                    ProjectCore.raptorUpdater.renameMethod(ct, (o as Constructor).raptorTab,
                                        type.Name);
                                }
                            }
                        }
                        //UpdateStatusBar();
                    }
                    catch (BadSyntaxException)
                    {
                        txtName.ForeColor = Color.Red;
                    }
                }
                else
                {
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

            if (diagram.SingleSelection && selectedElement is TypeShape)
            {
                TypeBase type = ((TypeShape)selectedElement).TypeBase;

                int index = cboAccess.SelectedIndex;
                type.AccessModifier = accessOrders[type.Language][index];
            }
        }

        private void cboModifier_SelectedIndexChanged(object sender, EventArgs e)
        {
            DiagramElement selectedElement = diagram.FirstSelectedElement;

            if (diagram.SingleSelection && selectedElement is ClassShape)
            {
                ClassType type = ((ClassShape)selectedElement).ClassType;

                int index = cboModifier.SelectedIndex;
                type.Modifier = modifierOrders[type.Language][index];
            }
        }

        private void txtReturnType_TextChanged(object sender, EventArgs e)
        {
            DiagramElement selectedElement = diagram.FirstSelectedElement;

            /*if (diagram.SingleSelection && selectedElement is DelegateShape)
            {
                DelegateType _delegate = ((DelegateShape)selectedElement).DelegateType;

                if (_delegate.ReturnType != txtReturnType.Text)
                {
                    try
                    {
                        _delegate.ReturnType = txtReturnType.Text;
                        txtReturnType.ForeColor = SystemColors.WindowText;
                    }
                    catch (BadSyntaxException)
                    {
                        txtReturnType.ForeColor = Color.Red;
                    }
                }
                else
                {
                    txtReturnType.ForeColor = SystemColors.WindowText;
                }
            }*/
        }

        private void txtReturnType_Validated(object sender, EventArgs e)
        {
            //txtReturnType.ForeColor = SystemColors.WindowText;
        }
        public void mnuDelete_Click(object sender, EventArgs e)
        {
            diagram.DeleteSelectedElements();
        }

        public void mnuSelectAll_Click(object sender, EventArgs e)
        {
            diagram.SelectAll();
        }
    }
}
