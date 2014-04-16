using System;
using NClass.Translations;

namespace NClass.GUI.Diagram
{
	partial class DiagramControl
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

		#region Component Designer generated code

		/// <summary> 
		/// Required method for Designer support - do not modify 
		/// the contents of this method with the code editor.
		/// </summary>
		private void InitializeComponent()
		{
			this.components = new System.ComponentModel.Container();
			this.saveAsImageDialog = new System.Windows.Forms.SaveFileDialog();
			this.defaultContextMenu = new System.Windows.Forms.ContextMenuStrip(this.components);
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
			this.mnuZoom = new System.Windows.Forms.ToolStripMenuItem();
			this.mnuZoomIn = new System.Windows.Forms.ToolStripMenuItem();
			this.mnuZoomOut = new System.Windows.Forms.ToolStripMenuItem();
			this.mnuAutoZoom = new System.Windows.Forms.ToolStripMenuItem();
			this.mnuMembersFormat = new System.Windows.Forms.ToolStripMenuItem();
			this.mnuShowType = new System.Windows.Forms.ToolStripMenuItem();
			this.mnuShowParameters = new System.Windows.Forms.ToolStripMenuItem();
			this.mnuShowParameterNames = new System.Windows.Forms.ToolStripMenuItem();
			this.mnuShowInitialValue = new System.Windows.Forms.ToolStripMenuItem();
			this.toolStripSeparator1 = new System.Windows.Forms.ToolStripSeparator();
			this.mnuPaste = new System.Windows.Forms.ToolStripMenuItem();
			this.mnuSaveAsImage = new System.Windows.Forms.ToolStripMenuItem();
			this.mnuSelectAll = new System.Windows.Forms.ToolStripMenuItem();
			this.dynamicContextMenu = new System.Windows.Forms.ContextMenuStrip(this.components);
			this.defaultContextMenu.SuspendLayout();
			this.SuspendLayout();
			// 
			// saveAsImageDialog
			// 
			this.saveAsImageDialog.DefaultExt = "png";
			this.saveAsImageDialog.Filter = "BMP (*.bmp)|*.bmp|GIF (*.gif)|*.gif|JPEG (*.jpg;*.jpeg)|*.jpg;*.jpeg|PNG (*.png)|" +
				"*.png|Transparent PNG (*.png)|*.png|Enhanced Metafile (*.emf)|*.emf";
			this.saveAsImageDialog.FilterIndex = 4;
			// 
			// defaultContextMenu
			// 
			this.defaultContextMenu.Items.AddRange(new System.Windows.Forms.ToolStripItem[] {
            this.mnuAddNewElement,
            this.mnuZoom,
            this.mnuMembersFormat,
            this.toolStripSeparator1,
            this.mnuPaste,
            this.mnuSaveAsImage,
            this.mnuSelectAll});
			this.defaultContextMenu.Name = "mnuZoom";
			this.defaultContextMenu.Size = new System.Drawing.Size(170, 142);
			this.defaultContextMenu.Opening += new System.ComponentModel.CancelEventHandler(this.defaultContextMenu_Opening);
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
			this.mnuAddNewElement.Image = global::NClass.GUI.Diagram.Properties.Resources.NewEntity;
			this.mnuAddNewElement.Name = "mnuAddNewElement";
			this.mnuAddNewElement.Size = new System.Drawing.Size(169, 22);
			this.mnuAddNewElement.Text = "&Add New";
			// 
			// mnuNewClass
			// 
			this.mnuNewClass.Image = global::NClass.GUI.Diagram.Properties.Resources.Class;
			this.mnuNewClass.Name = "mnuNewClass";
			this.mnuNewClass.Size = new System.Drawing.Size(172, 22);
			this.mnuNewClass.Text = "&Class";
			this.mnuNewClass.Click += new System.EventHandler(this.mnuNewClass_Click);
			// 
			// mnuNewStructure
			// 
			this.mnuNewStructure.Image = global::NClass.GUI.Diagram.Properties.Resources.Struct;
			this.mnuNewStructure.Name = "mnuNewStructure";
			this.mnuNewStructure.Size = new System.Drawing.Size(172, 22);
			this.mnuNewStructure.Text = "&Structure";
			this.mnuNewStructure.Click += new System.EventHandler(this.mnuNewStructure_Click);
			// 
			// mnuNewInterface
			// 
			this.mnuNewInterface.Image = global::NClass.GUI.Diagram.Properties.Resources.Interface32;
			this.mnuNewInterface.Name = "mnuNewInterface";
			this.mnuNewInterface.Size = new System.Drawing.Size(172, 22);
			this.mnuNewInterface.Text = "&Interface";
			this.mnuNewInterface.Click += new System.EventHandler(this.mnuNewInterface_Click);
			// 
			// mnuNewEnum
			// 
			this.mnuNewEnum.Image = global::NClass.GUI.Diagram.Properties.Resources.Enum;
			this.mnuNewEnum.Name = "mnuNewEnum";
			this.mnuNewEnum.Size = new System.Drawing.Size(172, 22);
			this.mnuNewEnum.Text = "&Enum";
			this.mnuNewEnum.Click += new System.EventHandler(this.mnuNewEnum_Click);
			// 
			// mnuNewDelegate
			// 
			this.mnuNewDelegate.Image = global::NClass.GUI.Diagram.Properties.Resources.Delegate;
			this.mnuNewDelegate.Name = "mnuNewDelegate";
			this.mnuNewDelegate.Size = new System.Drawing.Size(172, 22);
			this.mnuNewDelegate.Text = "&Delegate";
			this.mnuNewDelegate.Click += new System.EventHandler(this.mnuNewDelegate_Click);
			// 
			// mnuNewComment
			// 
			this.mnuNewComment.Image = global::NClass.GUI.Diagram.Properties.Resources.Note;
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
			this.mnuNewAssociation.Image = global::NClass.GUI.Diagram.Properties.Resources.Association;
			this.mnuNewAssociation.Name = "mnuNewAssociation";
			this.mnuNewAssociation.Size = new System.Drawing.Size(172, 22);
			this.mnuNewAssociation.Text = "&Association";
			this.mnuNewAssociation.Click += new System.EventHandler(this.mnuNewAssociation_Click);
			// 
			// mnuNewComposition
			// 
			this.mnuNewComposition.Image = global::NClass.GUI.Diagram.Properties.Resources.Composition;
			this.mnuNewComposition.Name = "mnuNewComposition";
			this.mnuNewComposition.Size = new System.Drawing.Size(172, 22);
			this.mnuNewComposition.Text = "C&omposition";
			this.mnuNewComposition.Click += new System.EventHandler(this.mnuNewComposition_Click);
			// 
			// mnuNewAggregation
			// 
			this.mnuNewAggregation.Image = global::NClass.GUI.Diagram.Properties.Resources.Aggregation;
			this.mnuNewAggregation.Name = "mnuNewAggregation";
			this.mnuNewAggregation.Size = new System.Drawing.Size(172, 22);
			this.mnuNewAggregation.Text = "A&ggregation";
			this.mnuNewAggregation.Click += new System.EventHandler(this.mnuNewAggregation_Click);
			// 
			// mnuNewGeneralization
			// 
			this.mnuNewGeneralization.Image = global::NClass.GUI.Diagram.Properties.Resources.Generalization;
			this.mnuNewGeneralization.Name = "mnuNewGeneralization";
			this.mnuNewGeneralization.Size = new System.Drawing.Size(172, 22);
			this.mnuNewGeneralization.Text = "Genera&lization";
			this.mnuNewGeneralization.Click += new System.EventHandler(this.mnuNewGeneralization_Click);
			// 
			// mnuNewRealization
			// 
			this.mnuNewRealization.Image = global::NClass.GUI.Diagram.Properties.Resources.Realization;
			this.mnuNewRealization.Name = "mnuNewRealization";
			this.mnuNewRealization.Size = new System.Drawing.Size(172, 22);
			this.mnuNewRealization.Text = "&Realization";
			this.mnuNewRealization.Click += new System.EventHandler(this.mnuNewRealization_Click);
			// 
			// mnuNewDependency
			// 
			this.mnuNewDependency.Image = global::NClass.GUI.Diagram.Properties.Resources.Dependency;
			this.mnuNewDependency.Name = "mnuNewDependency";
			this.mnuNewDependency.Size = new System.Drawing.Size(172, 22);
			this.mnuNewDependency.Text = "&Dependency";
			this.mnuNewDependency.Click += new System.EventHandler(this.mnuNewDependency_Click);
			// 
			// mnuNewNesting
			// 
			this.mnuNewNesting.Image = global::NClass.GUI.Diagram.Properties.Resources.Nesting;
			this.mnuNewNesting.Name = "mnuNewNesting";
			this.mnuNewNesting.Size = new System.Drawing.Size(172, 22);
			this.mnuNewNesting.Text = "&Nesting";
			this.mnuNewNesting.Click += new System.EventHandler(this.mnuNewNesting_Click);
			// 
			// mnuNewCommentRelation
			// 
			this.mnuNewCommentRelation.Image = global::NClass.GUI.Diagram.Properties.Resources.Comment;
			this.mnuNewCommentRelation.Name = "mnuNewCommentRelation";
			this.mnuNewCommentRelation.Size = new System.Drawing.Size(172, 22);
			this.mnuNewCommentRelation.Text = "Co&mment Relation";
			this.mnuNewCommentRelation.Click += new System.EventHandler(this.mnuNewCommentRelation_Click);
			// 
			// mnuZoom
			// 
			this.mnuZoom.DropDownItems.AddRange(new System.Windows.Forms.ToolStripItem[] {
            this.mnuZoomIn,
            this.mnuZoomOut,
            this.mnuAutoZoom});
			this.mnuZoom.Image = global::NClass.GUI.Diagram.Properties.Resources.Zoom;
			this.mnuZoom.Name = "mnuZoom";
			this.mnuZoom.Size = new System.Drawing.Size(169, 22);
			this.mnuZoom.Text = "&Zoom";
			// 
			// mnuZoomIn
			// 
			this.mnuZoomIn.Image = global::NClass.GUI.Diagram.Properties.Resources.ZoomIn;
			this.mnuZoomIn.Name = "mnuZoomIn";
			this.mnuZoomIn.Size = new System.Drawing.Size(137, 22);
			this.mnuZoomIn.Text = "&Zoom In";
			this.mnuZoomIn.Click += new System.EventHandler(this.mnuZoomIn_Click);
			// 
			// mnuZoomOut
			// 
			this.mnuZoomOut.Image = global::NClass.GUI.Diagram.Properties.Resources.ZoomOut;
			this.mnuZoomOut.Name = "mnuZoomOut";
			this.mnuZoomOut.Size = new System.Drawing.Size(137, 22);
			this.mnuZoomOut.Text = "Zoom &Out";
			this.mnuZoomOut.Click += new System.EventHandler(this.mnuZoomOut_Click);
			// 
			// mnuAutoZoom
			// 
			this.mnuAutoZoom.Image = global::NClass.GUI.Diagram.Properties.Resources.AutoZoom;
			this.mnuAutoZoom.Name = "mnuAutoZoom";
			this.mnuAutoZoom.Size = new System.Drawing.Size(137, 22);
			this.mnuAutoZoom.Text = "Auto &Zoom";
			this.mnuAutoZoom.Click += new System.EventHandler(this.mnuAutoZoom_Click);
			// 
			// mnuMembersFormat
			// 
			this.mnuMembersFormat.DropDownItems.AddRange(new System.Windows.Forms.ToolStripItem[] {
            this.mnuShowType,
            this.mnuShowParameters,
            this.mnuShowParameterNames,
            this.mnuShowInitialValue});
			this.mnuMembersFormat.Name = "mnuMembersFormat";
			this.mnuMembersFormat.Size = new System.Drawing.Size(169, 22);
			this.mnuMembersFormat.Text = "&Member\'s Format";
			// 
			// mnuShowType
			// 
			this.mnuShowType.CheckOnClick = true;
			this.mnuShowType.Name = "mnuShowType";
			this.mnuShowType.Size = new System.Drawing.Size(170, 22);
			this.mnuShowType.Text = "&Type";
			this.mnuShowType.Click += new System.EventHandler(this.mnuShowType_Click);
			// 
			// mnuShowParameters
			// 
			this.mnuShowParameters.CheckOnClick = true;
			this.mnuShowParameters.Name = "mnuShowParameters";
			this.mnuShowParameters.Size = new System.Drawing.Size(170, 22);
			this.mnuShowParameters.Text = "&Parameters";
			this.mnuShowParameters.Click += new System.EventHandler(this.mnuShowParameters_Click);
			// 
			// mnuShowParameterNames
			// 
			this.mnuShowParameterNames.CheckOnClick = true;
			this.mnuShowParameterNames.Name = "mnuShowParameterNames";
			this.mnuShowParameterNames.Size = new System.Drawing.Size(170, 22);
			this.mnuShowParameterNames.Text = "Parameter &Names";
			this.mnuShowParameterNames.Click += new System.EventHandler(this.mnuShowParameterNames_Click);
			// 
			// mnuShowInitialValue
			// 
			this.mnuShowInitialValue.CheckOnClick = true;
			this.mnuShowInitialValue.Name = "mnuShowInitialValue";
			this.mnuShowInitialValue.Size = new System.Drawing.Size(170, 22);
			this.mnuShowInitialValue.Text = "&Initial Value";
			this.mnuShowInitialValue.Click += new System.EventHandler(this.mnuShowInitialValue_Click);
			// 
			// toolStripSeparator1
			// 
			this.toolStripSeparator1.Name = "toolStripSeparator1";
			this.toolStripSeparator1.Size = new System.Drawing.Size(166, 6);
			// 
			// mnuPaste
			// 
			this.mnuPaste.Image = global::NClass.GUI.Diagram.Properties.Resources.Paste;
			this.mnuPaste.Name = "mnuPaste";
			this.mnuPaste.Size = new System.Drawing.Size(169, 22);
			this.mnuPaste.Text = "&Paste";
			this.mnuPaste.Click += new System.EventHandler(this.mnuPaste_Click);
			// 
			// mnuSaveAsImage
			// 
			this.mnuSaveAsImage.Image = global::NClass.GUI.Diagram.Properties.Resources.Image;
			this.mnuSaveAsImage.Name = "mnuSaveAsImage";
			this.mnuSaveAsImage.Size = new System.Drawing.Size(169, 22);
			this.mnuSaveAsImage.Text = "&Save As Image...";
			this.mnuSaveAsImage.Click += new System.EventHandler(this.mnuSaveAsImage_Click);
			// 
			// mnuSelectAll
			// 
			this.mnuSelectAll.Name = "mnuSelectAll";
			this.mnuSelectAll.ShortcutKeyDisplayString = "";
			this.mnuSelectAll.Size = new System.Drawing.Size(169, 22);
			this.mnuSelectAll.Text = "Select &All";
			this.mnuSelectAll.Click += new System.EventHandler(this.mnuSelectAll_Click);
			// 
			// dynamicContextMenu
			// 
			this.dynamicContextMenu.Name = "dynamicContextMenu";
			this.dynamicContextMenu.Size = new System.Drawing.Size(61, 4);
			this.dynamicContextMenu.Opening += new System.ComponentModel.CancelEventHandler(this.dynamicContextMenu_Opening);
			// 
			// DiagramControl
			// 
			this.AutoScaleDimensions = new System.Drawing.SizeF(6F, 13F);
			this.AutoScaleMode = System.Windows.Forms.AutoScaleMode.Font;
			this.Name = "DiagramControl";
			this.defaultContextMenu.ResumeLayout(false);
			this.ResumeLayout(false);

		}

		#endregion

		private System.Windows.Forms.SaveFileDialog saveAsImageDialog;
		private System.Windows.Forms.ContextMenuStrip defaultContextMenu;
		private System.Windows.Forms.ToolStripMenuItem mnuAddNewElement;
		private System.Windows.Forms.ToolStripMenuItem mnuNewClass;
		private System.Windows.Forms.ToolStripMenuItem mnuNewStructure;
		private System.Windows.Forms.ToolStripMenuItem mnuNewInterface;
		private System.Windows.Forms.ToolStripMenuItem mnuNewEnum;
		private System.Windows.Forms.ToolStripMenuItem mnuNewDelegate;
		private System.Windows.Forms.ToolStripMenuItem mnuNewComment;
		private System.Windows.Forms.ToolStripSeparator mnuSepElement;
		private System.Windows.Forms.ToolStripMenuItem mnuNewAssociation;
		private System.Windows.Forms.ToolStripMenuItem mnuNewComposition;
		private System.Windows.Forms.ToolStripMenuItem mnuNewAggregation;
		private System.Windows.Forms.ToolStripMenuItem mnuNewGeneralization;
		private System.Windows.Forms.ToolStripMenuItem mnuNewRealization;
		private System.Windows.Forms.ToolStripMenuItem mnuNewDependency;
		private System.Windows.Forms.ToolStripMenuItem mnuNewNesting;
		private System.Windows.Forms.ToolStripMenuItem mnuNewCommentRelation;
		private System.Windows.Forms.ToolStripMenuItem mnuZoom;
		private System.Windows.Forms.ToolStripMenuItem mnuMembersFormat;
		private System.Windows.Forms.ToolStripMenuItem mnuShowType;
		private System.Windows.Forms.ToolStripMenuItem mnuShowParameters;
		private System.Windows.Forms.ToolStripMenuItem mnuShowParameterNames;
		private System.Windows.Forms.ToolStripMenuItem mnuShowInitialValue;
		private System.Windows.Forms.ToolStripSeparator toolStripSeparator1;
		private System.Windows.Forms.ToolStripMenuItem mnuPaste;
		private System.Windows.Forms.ToolStripMenuItem mnuSaveAsImage;
		private System.Windows.Forms.ToolStripMenuItem mnuSelectAll;
		private System.Windows.Forms.ToolStripMenuItem mnuZoomIn;
		private System.Windows.Forms.ToolStripMenuItem mnuZoomOut;
		private System.Windows.Forms.ToolStripMenuItem mnuAutoZoom;
		private System.Windows.Forms.ContextMenuStrip dynamicContextMenu;
	}
}
