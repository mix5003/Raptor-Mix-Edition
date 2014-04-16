namespace raptor
{
    partial class UMLDiagram
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
            if (disposing && (components != null))
            {
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
            this.toolStripContainer = new System.Windows.Forms.ToolStripContainer();
            this.standardToolStrip = new System.Windows.Forms.ToolStrip();
            this.lblName = new System.Windows.Forms.ToolStripLabel();
            this.txtName = new System.Windows.Forms.ToolStripTextBox();
            this.lblAccess = new System.Windows.Forms.ToolStripLabel();
            this.cboAccess = new System.Windows.Forms.ToolStripComboBox();
            this.lblModifier = new System.Windows.Forms.ToolStripLabel();
            this.cboModifier = new System.Windows.Forms.ToolStripComboBox();
            this.elementsToolStrip = new System.Windows.Forms.ToolStrip();
            this.toolNewClass = new System.Windows.Forms.ToolStripButton();
            this.toolNewStruct = new System.Windows.Forms.ToolStripButton();
            this.toolNewInterface = new System.Windows.Forms.ToolStripButton();
            this.toolNewEnum = new System.Windows.Forms.ToolStripButton();
            this.toolNewComment = new System.Windows.Forms.ToolStripButton();
            this.toolSepEntities = new System.Windows.Forms.ToolStripSeparator();
            this.toolNewGeneralization = new System.Windows.Forms.ToolStripButton();
            this.toolNewRealization = new System.Windows.Forms.ToolStripButton();
            this.toolNewNesting = new System.Windows.Forms.ToolStripButton();
            this.toolStripSeparator1 = new System.Windows.Forms.ToolStripSeparator();
            this.toolNewAssociation = new System.Windows.Forms.ToolStripButton();
            this.toolNewComposition = new System.Windows.Forms.ToolStripButton();
            this.toolNewAggregation = new System.Windows.Forms.ToolStripButton();
            this.toolNewDependency = new System.Windows.Forms.ToolStripButton();
            this.toolSepRelations = new System.Windows.Forms.ToolStripSeparator();
            this.toolDelete = new System.Windows.Forms.ToolStripButton();
            this.zoomToolStrip = new System.Windows.Forms.ToolStrip();
            this.toolZoomValue = new System.Windows.Forms.ToolStripLabel();
            this.toolZoomOut = new System.Windows.Forms.ToolStripButton();
            this.toolZoom = new NClass.GUI.ZoomingToolStrip();
            this.toolZoomIn = new System.Windows.Forms.ToolStripButton();
            this.toolAutoZoom = new System.Windows.Forms.ToolStripButton();
            this.toolStripContainer.TopToolStripPanel.SuspendLayout();
            this.toolStripContainer.SuspendLayout();
            this.standardToolStrip.SuspendLayout();
            this.elementsToolStrip.SuspendLayout();
            this.zoomToolStrip.SuspendLayout();
            this.SuspendLayout();
            // 
            // toolStripContainer
            // 
            // 
            // toolStripContainer.ContentPanel
            // 
            this.toolStripContainer.ContentPanel.Size = new System.Drawing.Size(544, 241);
            this.toolStripContainer.Dock = System.Windows.Forms.DockStyle.Fill;
            this.toolStripContainer.Location = new System.Drawing.Point(0, 0);
            this.toolStripContainer.Name = "toolStripContainer";
            this.toolStripContainer.Size = new System.Drawing.Size(544, 316);
            this.toolStripContainer.TabIndex = 0;
            this.toolStripContainer.Text = "toolStripContainer1";
            // 
            // toolStripContainer.TopToolStripPanel
            // 
            this.toolStripContainer.TopToolStripPanel.Controls.Add(this.standardToolStrip);
            this.toolStripContainer.TopToolStripPanel.Controls.Add(this.elementsToolStrip);
            this.toolStripContainer.TopToolStripPanel.Controls.Add(this.zoomToolStrip);
            this.toolStripContainer.TopToolStripPanel.Click += new System.EventHandler(this.toolStripContainer1_TopToolStripPanel_Click);
            // 
            // standardToolStrip
            // 
            this.standardToolStrip.Dock = System.Windows.Forms.DockStyle.None;
            this.standardToolStrip.Items.AddRange(new System.Windows.Forms.ToolStripItem[] {
            this.lblName,
            this.txtName,
            this.lblAccess,
            this.cboAccess,
            this.lblModifier,
            this.cboModifier});
            this.standardToolStrip.Location = new System.Drawing.Point(3, 0);
            this.standardToolStrip.Name = "standardToolStrip";
            this.standardToolStrip.Size = new System.Drawing.Size(460, 25);
            this.standardToolStrip.TabIndex = 7;
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
            this.txtName.Size = new System.Drawing.Size(120, 25);
            this.txtName.Validated += new System.EventHandler(this.txtName_Validated);
            this.txtName.KeyPress += new System.Windows.Forms.KeyPressEventHandler(this.txtName_KeyPress);
            this.txtName.TextChanged += new System.EventHandler(this.txtName_TextChanged);
            // 
            // lblAccess
            // 
            this.lblAccess.Name = "lblAccess";
            this.lblAccess.Size = new System.Drawing.Size(40, 22);
            this.lblAccess.Text = "Access";
            // 
            // cboAccess
            // 
            this.cboAccess.Enabled = false;
            this.cboAccess.Name = "cboAccess";
            this.cboAccess.Size = new System.Drawing.Size(81, 25);
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
            this.cboModifier.Enabled = false;
            this.cboModifier.Name = "cboModifier";
            this.cboModifier.Size = new System.Drawing.Size(81, 25);
            this.cboModifier.SelectedIndexChanged += new System.EventHandler(this.cboModifier_SelectedIndexChanged);
            // 
            // elementsToolStrip
            // 
            this.elementsToolStrip.Dock = System.Windows.Forms.DockStyle.None;
            this.elementsToolStrip.Items.AddRange(new System.Windows.Forms.ToolStripItem[] {
            this.toolNewClass,
            this.toolNewStruct,
            this.toolNewInterface,
            this.toolNewEnum,
            this.toolNewComment,
            this.toolSepEntities,
            this.toolNewGeneralization,
            this.toolNewRealization,
            this.toolNewNesting,
            this.toolStripSeparator1,
            this.toolNewAssociation,
            this.toolNewComposition,
            this.toolNewAggregation,
            this.toolNewDependency,
            this.toolSepRelations,
            this.toolDelete});
            this.elementsToolStrip.Location = new System.Drawing.Point(3, 25);
            this.elementsToolStrip.Name = "elementsToolStrip";
            this.elementsToolStrip.Size = new System.Drawing.Size(329, 25);
            this.elementsToolStrip.TabIndex = 6;
            // 
            // toolNewClass
            // 
            this.toolNewClass.DisplayStyle = System.Windows.Forms.ToolStripItemDisplayStyle.Image;
            this.toolNewClass.Image = global::raptor.Properties.Resources.class1;
            this.toolNewClass.ImageTransparentColor = System.Drawing.Color.Magenta;
            this.toolNewClass.Name = "toolNewClass";
            this.toolNewClass.Size = new System.Drawing.Size(23, 22);
            this.toolNewClass.Click += new System.EventHandler(this.mnuNewClass_Click);
            // 
            // toolNewStruct
            // 
            this.toolNewStruct.DisplayStyle = System.Windows.Forms.ToolStripItemDisplayStyle.Image;
            this.toolNewStruct.Image = global::raptor.Properties.Resources.struct1;
            this.toolNewStruct.ImageTransparentColor = System.Drawing.Color.Magenta;
            this.toolNewStruct.Name = "toolNewStruct";
            this.toolNewStruct.Size = new System.Drawing.Size(23, 22);
            this.toolNewStruct.Click += new System.EventHandler(this.mnuNewStructure_Click);
            // 
            // toolNewInterface
            // 
            this.toolNewInterface.DisplayStyle = System.Windows.Forms.ToolStripItemDisplayStyle.Image;
            this.toolNewInterface.Image = global::raptor.Properties.Resources.interface1;
            this.toolNewInterface.ImageTransparentColor = System.Drawing.Color.Magenta;
            this.toolNewInterface.Name = "toolNewInterface";
            this.toolNewInterface.Size = new System.Drawing.Size(23, 22);
            this.toolNewInterface.Click += new System.EventHandler(this.mnuNewInterface_Click);
            // 
            // toolNewEnum
            // 
            this.toolNewEnum.DisplayStyle = System.Windows.Forms.ToolStripItemDisplayStyle.Image;
            this.toolNewEnum.Image = global::raptor.Properties.Resources.enum1;
            this.toolNewEnum.ImageTransparentColor = System.Drawing.Color.Magenta;
            this.toolNewEnum.Name = "toolNewEnum";
            this.toolNewEnum.Size = new System.Drawing.Size(23, 22);
            this.toolNewEnum.Click += new System.EventHandler(this.mnuNewEnum_Click);
            // 
            // toolNewComment
            // 
            this.toolNewComment.DisplayStyle = System.Windows.Forms.ToolStripItemDisplayStyle.Image;
            this.toolNewComment.Image = global::raptor.Properties.Resources.comment1;
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
            // toolNewGeneralization
            // 
            this.toolNewGeneralization.DisplayStyle = System.Windows.Forms.ToolStripItemDisplayStyle.Image;
            this.toolNewGeneralization.Image = global::raptor.Properties.Resources.generalization1;
            this.toolNewGeneralization.ImageTransparentColor = System.Drawing.Color.Magenta;
            this.toolNewGeneralization.Name = "toolNewGeneralization";
            this.toolNewGeneralization.Size = new System.Drawing.Size(23, 22);
            this.toolNewGeneralization.Click += new System.EventHandler(this.mnuNewGeneralization_Click);
            // 
            // toolNewRealization
            // 
            this.toolNewRealization.DisplayStyle = System.Windows.Forms.ToolStripItemDisplayStyle.Image;
            this.toolNewRealization.Image = global::raptor.Properties.Resources.realization1;
            this.toolNewRealization.ImageTransparentColor = System.Drawing.Color.Magenta;
            this.toolNewRealization.Name = "toolNewRealization";
            this.toolNewRealization.Size = new System.Drawing.Size(23, 22);
            this.toolNewRealization.Click += new System.EventHandler(this.mnuNewRealization_Click);
            // 
            // toolNewNesting
            // 
            this.toolNewNesting.DisplayStyle = System.Windows.Forms.ToolStripItemDisplayStyle.Image;
            this.toolNewNesting.Image = global::raptor.Properties.Resources.nesting1;
            this.toolNewNesting.ImageTransparentColor = System.Drawing.Color.Magenta;
            this.toolNewNesting.Name = "toolNewNesting";
            this.toolNewNesting.Size = new System.Drawing.Size(23, 22);
            this.toolNewNesting.Click += new System.EventHandler(this.mnuNewNesting_Click);
            // 
            // toolStripSeparator1
            // 
            this.toolStripSeparator1.Name = "toolStripSeparator1";
            this.toolStripSeparator1.Size = new System.Drawing.Size(6, 25);
            // 
            // toolNewAssociation
            // 
            this.toolNewAssociation.DisplayStyle = System.Windows.Forms.ToolStripItemDisplayStyle.Image;
            this.toolNewAssociation.Image = global::raptor.Properties.Resources.association1;
            this.toolNewAssociation.ImageTransparentColor = System.Drawing.Color.Magenta;
            this.toolNewAssociation.Name = "toolNewAssociation";
            this.toolNewAssociation.Size = new System.Drawing.Size(23, 22);
            this.toolNewAssociation.Click += new System.EventHandler(this.mnuNewAssociation_Click);
            // 
            // toolNewComposition
            // 
            this.toolNewComposition.DisplayStyle = System.Windows.Forms.ToolStripItemDisplayStyle.Image;
            this.toolNewComposition.Image = global::raptor.Properties.Resources.composition1;
            this.toolNewComposition.ImageTransparentColor = System.Drawing.Color.Magenta;
            this.toolNewComposition.Name = "toolNewComposition";
            this.toolNewComposition.Size = new System.Drawing.Size(23, 22);
            // 
            // toolNewAggregation
            // 
            this.toolNewAggregation.DisplayStyle = System.Windows.Forms.ToolStripItemDisplayStyle.Image;
            this.toolNewAggregation.Image = global::raptor.Properties.Resources.aggregation1;
            this.toolNewAggregation.ImageTransparentColor = System.Drawing.Color.Magenta;
            this.toolNewAggregation.Name = "toolNewAggregation";
            this.toolNewAggregation.Size = new System.Drawing.Size(23, 22);
            this.toolNewAggregation.Click += new System.EventHandler(this.mnuNewAggregation_Click);
            // 
            // toolNewDependency
            // 
            this.toolNewDependency.DisplayStyle = System.Windows.Forms.ToolStripItemDisplayStyle.Image;
            this.toolNewDependency.Image = global::raptor.Properties.Resources.dependency1;
            this.toolNewDependency.ImageTransparentColor = System.Drawing.Color.Magenta;
            this.toolNewDependency.Name = "toolNewDependency";
            this.toolNewDependency.Size = new System.Drawing.Size(23, 22);
            this.toolNewDependency.Click += new System.EventHandler(this.mnuNewDependency_Click);
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
            this.toolDelete.Image = global::raptor.Properties.Resources.delete1;
            this.toolDelete.ImageTransparentColor = System.Drawing.Color.Magenta;
            this.toolDelete.Name = "toolDelete";
            this.toolDelete.Size = new System.Drawing.Size(23, 22);
            this.toolDelete.Click += new System.EventHandler(this.mnuDelete_Click);
            // 
            // zoomToolStrip
            // 
            this.zoomToolStrip.Dock = System.Windows.Forms.DockStyle.None;
            this.zoomToolStrip.Items.AddRange(new System.Windows.Forms.ToolStripItem[] {
            this.toolZoomValue,
            this.toolZoomOut,
            this.toolZoom,
            this.toolZoomIn,
            this.toolAutoZoom});
            this.zoomToolStrip.Location = new System.Drawing.Point(3, 50);
            this.zoomToolStrip.Name = "zoomToolStrip";
            this.zoomToolStrip.Size = new System.Drawing.Size(217, 25);
            this.zoomToolStrip.TabIndex = 8;
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
            this.toolZoomOut.Image = global::raptor.Properties.Resources.ZoomOut;
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
            this.toolZoomIn.Image = global::raptor.Properties.Resources.ZoomIn;
            this.toolZoomIn.ImageTransparentColor = System.Drawing.Color.Magenta;
            this.toolZoomIn.Name = "toolZoomIn";
            this.toolZoomIn.Size = new System.Drawing.Size(23, 22);
            this.toolZoomIn.Click += new System.EventHandler(this.toolZoomIn_Click);
            // 
            // toolAutoZoom
            // 
            this.toolAutoZoom.DisplayStyle = System.Windows.Forms.ToolStripItemDisplayStyle.Image;
            this.toolAutoZoom.Image = global::raptor.Properties.Resources.AutoZoom;
            this.toolAutoZoom.ImageTransparentColor = System.Drawing.Color.Magenta;
            this.toolAutoZoom.Name = "toolAutoZoom";
            this.toolAutoZoom.Size = new System.Drawing.Size(23, 22);
            this.toolAutoZoom.Click += new System.EventHandler(this.mnuAutoZoom_Click);
            // 
            // UMLDiagram
            // 
            this.AutoScaleDimensions = new System.Drawing.SizeF(6F, 13F);
            this.AutoScaleMode = System.Windows.Forms.AutoScaleMode.Font;
            this.Controls.Add(this.toolStripContainer);
            this.Name = "UMLDiagram";
            this.Size = new System.Drawing.Size(544, 316);
            this.toolStripContainer.TopToolStripPanel.ResumeLayout(false);
            this.toolStripContainer.TopToolStripPanel.PerformLayout();
            this.toolStripContainer.ResumeLayout(false);
            this.toolStripContainer.PerformLayout();
            this.standardToolStrip.ResumeLayout(false);
            this.standardToolStrip.PerformLayout();
            this.elementsToolStrip.ResumeLayout(false);
            this.elementsToolStrip.PerformLayout();
            this.zoomToolStrip.ResumeLayout(false);
            this.zoomToolStrip.PerformLayout();
            this.ResumeLayout(false);

        }

        #endregion

        private System.Windows.Forms.ToolStripContainer toolStripContainer;
        private System.Windows.Forms.ToolStrip elementsToolStrip;
        private System.Windows.Forms.ToolStripButton toolNewClass;
        private System.Windows.Forms.ToolStripButton toolNewStruct;
        private System.Windows.Forms.ToolStripButton toolNewInterface;
        private System.Windows.Forms.ToolStripButton toolNewEnum;
        private System.Windows.Forms.ToolStripButton toolNewComment;
        private System.Windows.Forms.ToolStripSeparator toolSepEntities;
        private System.Windows.Forms.ToolStripButton toolNewAssociation;
        private System.Windows.Forms.ToolStripButton toolNewComposition;
        private System.Windows.Forms.ToolStripButton toolNewAggregation;
        private System.Windows.Forms.ToolStripButton toolNewGeneralization;
        private System.Windows.Forms.ToolStripButton toolNewRealization;
        private System.Windows.Forms.ToolStripButton toolNewDependency;
        private System.Windows.Forms.ToolStripButton toolNewNesting;
        private System.Windows.Forms.ToolStripSeparator toolSepRelations;
        private System.Windows.Forms.ToolStripButton toolDelete;
        private System.Windows.Forms.ToolStrip standardToolStrip;
        private System.Windows.Forms.ToolStripLabel lblName;
        private System.Windows.Forms.ToolStripTextBox txtName;
        private System.Windows.Forms.ToolStripLabel lblAccess;
        private System.Windows.Forms.ToolStripComboBox cboAccess;
        private System.Windows.Forms.ToolStrip zoomToolStrip;
        private System.Windows.Forms.ToolStripLabel toolZoomValue;
        private System.Windows.Forms.ToolStripButton toolZoomOut;
        private System.Windows.Forms.ToolStripButton toolZoomIn;
        private System.Windows.Forms.ToolStripButton toolAutoZoom;
        private NClass.GUI.ZoomingToolStrip toolZoom;
        private System.Windows.Forms.ToolStripComboBox cboModifier;
        private System.Windows.Forms.ToolStripLabel lblModifier;
        private System.Windows.Forms.ToolStripSeparator toolStripSeparator1;
    }
}
