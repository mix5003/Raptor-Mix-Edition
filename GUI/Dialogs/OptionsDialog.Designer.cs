namespace NClass.GUI
{
	sealed partial class OptionsDialog
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
			this.btnCancel = new System.Windows.Forms.Button();
			this.btnOK = new System.Windows.Forms.Button();
			this.grpDiagram = new System.Windows.Forms.GroupBox();
			this.radNever = new System.Windows.Forms.RadioButton();
			this.radAlways = new System.Windows.Forms.RadioButton();
			this.radAsNeeded = new System.Windows.Forms.RadioButton();
			this.lblShowChevron = new System.Windows.Forms.Label();
			this.chkUsePrecisionSnapping = new System.Windows.Forms.CheckBox();
			this.tabOptions = new System.Windows.Forms.TabControl();
			this.tabGeneral = new System.Windows.Forms.TabPage();
			this.grpGeneral = new System.Windows.Forms.GroupBox();
			this.lblLanguage = new System.Windows.Forms.Label();
			this.chkShowFullPath = new System.Windows.Forms.CheckBox();
			this.chkLoadLastProject = new System.Windows.Forms.CheckBox();
			this.btnClearRecents = new System.Windows.Forms.Button();
			this.cboLanguage = new System.Windows.Forms.ComboBox();
			this.tabStyle = new System.Windows.Forms.TabPage();
			this.cboStyles = new System.Windows.Forms.ComboBox();
			this.btnSave = new System.Windows.Forms.Button();
			this.btnLoad = new System.Windows.Forms.Button();
			this.stylePropertyGrid = new System.Windows.Forms.PropertyGrid();
			this.btnApply = new System.Windows.Forms.Button();
			this.grpDiagram.SuspendLayout();
			this.tabOptions.SuspendLayout();
			this.tabGeneral.SuspendLayout();
			this.grpGeneral.SuspendLayout();
			this.tabStyle.SuspendLayout();
			this.SuspendLayout();
			// 
			// btnCancel
			// 
			this.btnCancel.Anchor = ((System.Windows.Forms.AnchorStyles) ((System.Windows.Forms.AnchorStyles.Bottom | System.Windows.Forms.AnchorStyles.Right)));
			this.btnCancel.DialogResult = System.Windows.Forms.DialogResult.Cancel;
			this.btnCancel.Location = new System.Drawing.Point(208, 471);
			this.btnCancel.Name = "btnCancel";
			this.btnCancel.Size = new System.Drawing.Size(75, 23);
			this.btnCancel.TabIndex = 2;
			this.btnCancel.Text = "Cancel";
			this.btnCancel.UseVisualStyleBackColor = true;
			// 
			// btnOK
			// 
			this.btnOK.Anchor = ((System.Windows.Forms.AnchorStyles) ((System.Windows.Forms.AnchorStyles.Bottom | System.Windows.Forms.AnchorStyles.Right)));
			this.btnOK.DialogResult = System.Windows.Forms.DialogResult.OK;
			this.btnOK.Location = new System.Drawing.Point(127, 471);
			this.btnOK.Name = "btnOK";
			this.btnOK.Size = new System.Drawing.Size(75, 23);
			this.btnOK.TabIndex = 1;
			this.btnOK.Text = "OK";
			this.btnOK.UseVisualStyleBackColor = true;
			this.btnOK.Click += new System.EventHandler(this.btnOK_Click);
			// 
			// grpDiagram
			// 
			this.grpDiagram.Anchor = ((System.Windows.Forms.AnchorStyles) ((((System.Windows.Forms.AnchorStyles.Top | System.Windows.Forms.AnchorStyles.Bottom)
						| System.Windows.Forms.AnchorStyles.Left)
						| System.Windows.Forms.AnchorStyles.Right)));
			this.grpDiagram.Controls.Add(this.radNever);
			this.grpDiagram.Controls.Add(this.radAlways);
			this.grpDiagram.Controls.Add(this.radAsNeeded);
			this.grpDiagram.Controls.Add(this.lblShowChevron);
			this.grpDiagram.Controls.Add(this.chkUsePrecisionSnapping);
			this.grpDiagram.Location = new System.Drawing.Point(6, 139);
			this.grpDiagram.Name = "grpDiagram";
			this.grpDiagram.Size = new System.Drawing.Size(333, 282);
			this.grpDiagram.TabIndex = 3;
			this.grpDiagram.TabStop = false;
			this.grpDiagram.Text = "Diagram";
			// 
			// radNever
			// 
			this.radNever.AutoSize = true;
			this.radNever.Location = new System.Drawing.Point(28, 118);
			this.radNever.Name = "radNever";
			this.radNever.Size = new System.Drawing.Size(54, 17);
			this.radNever.TabIndex = 15;
			this.radNever.TabStop = true;
			this.radNever.Text = "Never";
			this.radNever.UseVisualStyleBackColor = true;
			this.radNever.CheckedChanged += new System.EventHandler(this.SettingsStateChanged);
			// 
			// radAlways
			// 
			this.radAlways.AutoSize = true;
			this.radAlways.Location = new System.Drawing.Point(28, 95);
			this.radAlways.Name = "radAlways";
			this.radAlways.Size = new System.Drawing.Size(58, 17);
			this.radAlways.TabIndex = 14;
			this.radAlways.TabStop = true;
			this.radAlways.Text = "Always";
			this.radAlways.UseVisualStyleBackColor = true;
			this.radAlways.CheckedChanged += new System.EventHandler(this.SettingsStateChanged);
			// 
			// radAsNeeded
			// 
			this.radAsNeeded.AutoSize = true;
			this.radAsNeeded.Location = new System.Drawing.Point(28, 72);
			this.radAsNeeded.Name = "radAsNeeded";
			this.radAsNeeded.Size = new System.Drawing.Size(131, 17);
			this.radAsNeeded.TabIndex = 13;
			this.radAsNeeded.TabStop = true;
			this.radAsNeeded.Text = "As mouse passes over";
			this.radAsNeeded.UseVisualStyleBackColor = true;
			this.radAsNeeded.CheckedChanged += new System.EventHandler(this.SettingsStateChanged);
			// 
			// lblShowChevron
			// 
			this.lblShowChevron.AutoSize = true;
			this.lblShowChevron.Location = new System.Drawing.Point(6, 51);
			this.lblShowChevron.Name = "lblShowChevron";
			this.lblShowChevron.Size = new System.Drawing.Size(112, 13);
			this.lblShowChevron.TabIndex = 12;
			this.lblShowChevron.Text = "Show chevron button:";
			// 
			// chkUsePrecisionSnapping
			// 
			this.chkUsePrecisionSnapping.AutoSize = true;
			this.chkUsePrecisionSnapping.Location = new System.Drawing.Point(9, 20);
			this.chkUsePrecisionSnapping.Name = "chkUsePrecisionSnapping";
			this.chkUsePrecisionSnapping.Size = new System.Drawing.Size(136, 17);
			this.chkUsePrecisionSnapping.TabIndex = 0;
			this.chkUsePrecisionSnapping.Text = "Use precision snapping";
			this.chkUsePrecisionSnapping.UseVisualStyleBackColor = true;
			this.chkUsePrecisionSnapping.CheckedChanged += new System.EventHandler(this.SettingsStateChanged);
			// 
			// tabOptions
			// 
			this.tabOptions.Anchor = ((System.Windows.Forms.AnchorStyles) ((((System.Windows.Forms.AnchorStyles.Top | System.Windows.Forms.AnchorStyles.Bottom)
						| System.Windows.Forms.AnchorStyles.Left)
						| System.Windows.Forms.AnchorStyles.Right)));
			this.tabOptions.Controls.Add(this.tabGeneral);
			this.tabOptions.Controls.Add(this.tabStyle);
			this.tabOptions.Location = new System.Drawing.Point(12, 12);
			this.tabOptions.Multiline = true;
			this.tabOptions.Name = "tabOptions";
			this.tabOptions.SelectedIndex = 0;
			this.tabOptions.Size = new System.Drawing.Size(352, 453);
			this.tabOptions.TabIndex = 5;
			// 
			// tabGeneral
			// 
			this.tabGeneral.Controls.Add(this.grpGeneral);
			this.tabGeneral.Controls.Add(this.grpDiagram);
			this.tabGeneral.Location = new System.Drawing.Point(4, 22);
			this.tabGeneral.Name = "tabGeneral";
			this.tabGeneral.Padding = new System.Windows.Forms.Padding(3);
			this.tabGeneral.Size = new System.Drawing.Size(344, 427);
			this.tabGeneral.TabIndex = 0;
			this.tabGeneral.Text = "General";
			this.tabGeneral.UseVisualStyleBackColor = true;
			// 
			// grpGeneral
			// 
			this.grpGeneral.Anchor = ((System.Windows.Forms.AnchorStyles) (((System.Windows.Forms.AnchorStyles.Top | System.Windows.Forms.AnchorStyles.Left)
						| System.Windows.Forms.AnchorStyles.Right)));
			this.grpGeneral.Controls.Add(this.lblLanguage);
			this.grpGeneral.Controls.Add(this.chkShowFullPath);
			this.grpGeneral.Controls.Add(this.chkLoadLastProject);
			this.grpGeneral.Controls.Add(this.btnClearRecents);
			this.grpGeneral.Controls.Add(this.cboLanguage);
			this.grpGeneral.Location = new System.Drawing.Point(6, 9);
			this.grpGeneral.Name = "grpGeneral";
			this.grpGeneral.Size = new System.Drawing.Size(333, 124);
			this.grpGeneral.TabIndex = 1;
			this.grpGeneral.TabStop = false;
			this.grpGeneral.Text = "General";
			// 
			// lblLanguage
			// 
			this.lblLanguage.AutoSize = true;
			this.lblLanguage.Location = new System.Drawing.Point(6, 20);
			this.lblLanguage.Name = "lblLanguage";
			this.lblLanguage.Size = new System.Drawing.Size(58, 13);
			this.lblLanguage.TabIndex = 0;
			this.lblLanguage.Text = "Language:";
			// 
			// chkShowFullPath
			// 
			this.chkShowFullPath.AutoSize = true;
			this.chkShowFullPath.Location = new System.Drawing.Point(9, 67);
			this.chkShowFullPath.Name = "chkShowFullPath";
			this.chkShowFullPath.Size = new System.Drawing.Size(174, 17);
			this.chkShowFullPath.TabIndex = 3;
			this.chkShowFullPath.Text = "Show full file path in the header";
			this.chkShowFullPath.UseVisualStyleBackColor = true;
			this.chkShowFullPath.CheckedChanged += new System.EventHandler(this.SettingsStateChanged);
			// 
			// chkLoadLastProject
			// 
			this.chkLoadLastProject.AutoSize = true;
			this.chkLoadLastProject.Location = new System.Drawing.Point(9, 44);
			this.chkLoadLastProject.Name = "chkLoadLastProject";
			this.chkLoadLastProject.Size = new System.Drawing.Size(143, 17);
			this.chkLoadLastProject.TabIndex = 2;
			this.chkLoadLastProject.Text = "Load last opened project";
			this.chkLoadLastProject.UseVisualStyleBackColor = true;
			this.chkLoadLastProject.CheckedChanged += new System.EventHandler(this.SettingsStateChanged);
			// 
			// btnClearRecents
			// 
			this.btnClearRecents.AutoSize = true;
			this.btnClearRecents.Location = new System.Drawing.Point(9, 90);
			this.btnClearRecents.Name = "btnClearRecents";
			this.btnClearRecents.Size = new System.Drawing.Size(110, 23);
			this.btnClearRecents.TabIndex = 4;
			this.btnClearRecents.Text = "Clear recent files list";
			this.btnClearRecents.UseVisualStyleBackColor = true;
			// 
			// cboLanguage
			// 
			this.cboLanguage.DropDownStyle = System.Windows.Forms.ComboBoxStyle.DropDownList;
			this.cboLanguage.FormattingEnabled = true;
			this.cboLanguage.Location = new System.Drawing.Point(67, 17);
			this.cboLanguage.Name = "cboLanguage";
			this.cboLanguage.Size = new System.Drawing.Size(121, 21);
			this.cboLanguage.TabIndex = 1;
			this.cboLanguage.SelectedIndexChanged += new System.EventHandler(this.SettingsStateChanged);
			// 
			// tabStyle
			// 
			this.tabStyle.Controls.Add(this.cboStyles);
			this.tabStyle.Controls.Add(this.btnSave);
			this.tabStyle.Controls.Add(this.btnLoad);
			this.tabStyle.Controls.Add(this.stylePropertyGrid);
			this.tabStyle.Location = new System.Drawing.Point(4, 22);
			this.tabStyle.Name = "tabStyle";
			this.tabStyle.Padding = new System.Windows.Forms.Padding(3);
			this.tabStyle.Size = new System.Drawing.Size(344, 427);
			this.tabStyle.TabIndex = 1;
			this.tabStyle.Text = "Style";
			this.tabStyle.UseVisualStyleBackColor = true;
			// 
			// cboStyles
			// 
			this.cboStyles.Anchor = ((System.Windows.Forms.AnchorStyles) (((System.Windows.Forms.AnchorStyles.Bottom | System.Windows.Forms.AnchorStyles.Left)
						| System.Windows.Forms.AnchorStyles.Right)));
			this.cboStyles.DropDownStyle = System.Windows.Forms.ComboBoxStyle.DropDownList;
			this.cboStyles.FormattingEnabled = true;
			this.cboStyles.Location = new System.Drawing.Point(6, 371);
			this.cboStyles.Name = "cboStyles";
			this.cboStyles.Size = new System.Drawing.Size(332, 21);
			this.cboStyles.TabIndex = 5;
			this.cboStyles.SelectedIndexChanged += new System.EventHandler(this.cboStyles_SelectedIndexChanged);
			// 
			// btnSave
			// 
			this.btnSave.Anchor = ((System.Windows.Forms.AnchorStyles) ((System.Windows.Forms.AnchorStyles.Bottom | System.Windows.Forms.AnchorStyles.Right)));
			this.btnSave.Location = new System.Drawing.Point(263, 398);
			this.btnSave.Name = "btnSave";
			this.btnSave.Size = new System.Drawing.Size(75, 23);
			this.btnSave.TabIndex = 4;
			this.btnSave.Text = "Save";
			this.btnSave.UseVisualStyleBackColor = true;
			this.btnSave.Click += new System.EventHandler(this.btnSave_Click);
			// 
			// btnLoad
			// 
			this.btnLoad.Anchor = ((System.Windows.Forms.AnchorStyles) ((System.Windows.Forms.AnchorStyles.Bottom | System.Windows.Forms.AnchorStyles.Right)));
			this.btnLoad.Location = new System.Drawing.Point(182, 398);
			this.btnLoad.Name = "btnLoad";
			this.btnLoad.Size = new System.Drawing.Size(75, 23);
			this.btnLoad.TabIndex = 3;
			this.btnLoad.Text = "Load";
			this.btnLoad.UseVisualStyleBackColor = true;
			this.btnLoad.Click += new System.EventHandler(this.btnLoad_Click);
			// 
			// stylePropertyGrid
			// 
			this.stylePropertyGrid.Anchor = ((System.Windows.Forms.AnchorStyles) ((((System.Windows.Forms.AnchorStyles.Top | System.Windows.Forms.AnchorStyles.Bottom)
						| System.Windows.Forms.AnchorStyles.Left)
						| System.Windows.Forms.AnchorStyles.Right)));
			this.stylePropertyGrid.Location = new System.Drawing.Point(6, 6);
			this.stylePropertyGrid.Name = "stylePropertyGrid";
			this.stylePropertyGrid.Size = new System.Drawing.Size(332, 359);
			this.stylePropertyGrid.TabIndex = 0;
			this.stylePropertyGrid.ToolbarVisible = false;
			this.stylePropertyGrid.PropertyValueChanged += new System.Windows.Forms.PropertyValueChangedEventHandler(this.stylePropertyGrid_PropertyValueChanged);
			// 
			// btnApply
			// 
			this.btnApply.Anchor = ((System.Windows.Forms.AnchorStyles) ((System.Windows.Forms.AnchorStyles.Bottom | System.Windows.Forms.AnchorStyles.Right)));
			this.btnApply.Enabled = false;
			this.btnApply.Location = new System.Drawing.Point(289, 471);
			this.btnApply.Name = "btnApply";
			this.btnApply.Size = new System.Drawing.Size(75, 23);
			this.btnApply.TabIndex = 6;
			this.btnApply.Text = "Apply";
			this.btnApply.UseVisualStyleBackColor = true;
			this.btnApply.Click += new System.EventHandler(this.btnApply_Click);
			// 
			// OptionsDialog
			// 
			this.AcceptButton = this.btnOK;
			this.AutoScaleDimensions = new System.Drawing.SizeF(6F, 13F);
			this.AutoScaleMode = System.Windows.Forms.AutoScaleMode.Font;
			this.CancelButton = this.btnCancel;
			this.ClientSize = new System.Drawing.Size(376, 506);
			this.Controls.Add(this.btnApply);
			this.Controls.Add(this.tabOptions);
			this.Controls.Add(this.btnCancel);
			this.Controls.Add(this.btnOK);
			this.MaximizeBox = false;
			this.MinimizeBox = false;
			this.Name = "OptionsDialog";
			this.ShowIcon = false;
			this.ShowInTaskbar = false;
			this.SizeGripStyle = System.Windows.Forms.SizeGripStyle.Show;
			this.StartPosition = System.Windows.Forms.FormStartPosition.CenterParent;
			this.Text = "Options";
			this.grpDiagram.ResumeLayout(false);
			this.grpDiagram.PerformLayout();
			this.tabOptions.ResumeLayout(false);
			this.tabGeneral.ResumeLayout(false);
			this.grpGeneral.ResumeLayout(false);
			this.grpGeneral.PerformLayout();
			this.tabStyle.ResumeLayout(false);
			this.ResumeLayout(false);

		}

		#endregion

		private System.Windows.Forms.Button btnCancel;
		private System.Windows.Forms.Button btnOK;
		private System.Windows.Forms.GroupBox grpDiagram;
		private System.Windows.Forms.TabControl tabOptions;
		private System.Windows.Forms.TabPage tabGeneral;
		private System.Windows.Forms.GroupBox grpGeneral;
		private System.Windows.Forms.Label lblLanguage;
		private System.Windows.Forms.CheckBox chkShowFullPath;
		private System.Windows.Forms.CheckBox chkLoadLastProject;
		private System.Windows.Forms.Button btnClearRecents;
		private System.Windows.Forms.ComboBox cboLanguage;
		private System.Windows.Forms.TabPage tabStyle;
		private System.Windows.Forms.PropertyGrid stylePropertyGrid;
		private System.Windows.Forms.Button btnApply;
		private System.Windows.Forms.Button btnSave;
		private System.Windows.Forms.Button btnLoad;
		private System.Windows.Forms.CheckBox chkUsePrecisionSnapping;
		private System.Windows.Forms.ComboBox cboStyles;
		private System.Windows.Forms.Label lblShowChevron;
		private System.Windows.Forms.RadioButton radAsNeeded;
		private System.Windows.Forms.RadioButton radNever;
		private System.Windows.Forms.RadioButton radAlways;



	}
}