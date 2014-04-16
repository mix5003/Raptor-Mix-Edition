namespace NClass.GUI
{
	partial class DiagramSizeDialog
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
			this.btnOK = new System.Windows.Forms.Button();
			this.btnCancel = new System.Windows.Forms.Button();
			this.lblDescription = new System.Windows.Forms.Label();
			this.lblWidth = new System.Windows.Forms.Label();
			this.lblHeight = new System.Windows.Forms.Label();
			this.numWidth = new System.Windows.Forms.NumericUpDown();
			this.numHeight = new System.Windows.Forms.NumericUpDown();
			((System.ComponentModel.ISupportInitialize) (this.numWidth)).BeginInit();
			((System.ComponentModel.ISupportInitialize) (this.numHeight)).BeginInit();
			this.SuspendLayout();
			// 
			// btnOK
			// 
			this.btnOK.Anchor = ((System.Windows.Forms.AnchorStyles) ((System.Windows.Forms.AnchorStyles.Bottom | System.Windows.Forms.AnchorStyles.Right)));
			this.btnOK.DialogResult = System.Windows.Forms.DialogResult.OK;
			this.btnOK.Location = new System.Drawing.Point(153, 96);
			this.btnOK.Name = "btnOK";
			this.btnOK.Size = new System.Drawing.Size(75, 23);
			this.btnOK.TabIndex = 0;
			this.btnOK.Text = "OK";
			this.btnOK.UseVisualStyleBackColor = true;
			// 
			// btnCancel
			// 
			this.btnCancel.Anchor = ((System.Windows.Forms.AnchorStyles) ((System.Windows.Forms.AnchorStyles.Bottom | System.Windows.Forms.AnchorStyles.Right)));
			this.btnCancel.DialogResult = System.Windows.Forms.DialogResult.Cancel;
			this.btnCancel.Location = new System.Drawing.Point(234, 96);
			this.btnCancel.Name = "btnCancel";
			this.btnCancel.Size = new System.Drawing.Size(75, 23);
			this.btnCancel.TabIndex = 1;
			this.btnCancel.Text = "Cancel";
			this.btnCancel.UseVisualStyleBackColor = true;
			// 
			// lblDescription
			// 
			this.lblDescription.AutoSize = true;
			this.lblDescription.Location = new System.Drawing.Point(14, 9);
			this.lblDescription.Name = "lblDescription";
			this.lblDescription.Size = new System.Drawing.Size(277, 13);
			this.lblDescription.TabIndex = 2;
			this.lblDescription.Text = "Please adjust the maximal size of the diagram workspace.";
			// 
			// lblWidth
			// 
			this.lblWidth.AutoSize = true;
			this.lblWidth.Location = new System.Drawing.Point(14, 37);
			this.lblWidth.Name = "lblWidth";
			this.lblWidth.Size = new System.Drawing.Size(77, 13);
			this.lblWidth.TabIndex = 3;
			this.lblWidth.Text = "Diagram width:";
			// 
			// lblHeight
			// 
			this.lblHeight.AutoSize = true;
			this.lblHeight.Location = new System.Drawing.Point(14, 63);
			this.lblHeight.Name = "lblHeight";
			this.lblHeight.Size = new System.Drawing.Size(81, 13);
			this.lblHeight.TabIndex = 4;
			this.lblHeight.Text = "Diagram height:";
			// 
			// numWidth
			// 
			this.numWidth.Increment = new decimal(new int[] {
            100,
            0,
            0,
            0});
			this.numWidth.Location = new System.Drawing.Point(101, 35);
			this.numWidth.Maximum = new decimal(new int[] {
            100000,
            0,
            0,
            0});
			this.numWidth.Minimum = new decimal(new int[] {
            1000,
            0,
            0,
            0});
			this.numWidth.Name = "numWidth";
			this.numWidth.Size = new System.Drawing.Size(90, 20);
			this.numWidth.TabIndex = 5;
			this.numWidth.Value = new decimal(new int[] {
            1000,
            0,
            0,
            0});
			// 
			// numHeight
			// 
			this.numHeight.Increment = new decimal(new int[] {
            100,
            0,
            0,
            0});
			this.numHeight.Location = new System.Drawing.Point(101, 61);
			this.numHeight.Maximum = new decimal(new int[] {
            100000,
            0,
            0,
            0});
			this.numHeight.Minimum = new decimal(new int[] {
            1000,
            0,
            0,
            0});
			this.numHeight.Name = "numHeight";
			this.numHeight.Size = new System.Drawing.Size(90, 20);
			this.numHeight.TabIndex = 6;
			this.numHeight.Value = new decimal(new int[] {
            1000,
            0,
            0,
            0});
			// 
			// DiagramSizeDialog
			// 
			this.AcceptButton = this.btnOK;
			this.AutoScaleDimensions = new System.Drawing.SizeF(6F, 13F);
			this.AutoScaleMode = System.Windows.Forms.AutoScaleMode.Font;
			this.CancelButton = this.btnCancel;
			this.ClientSize = new System.Drawing.Size(321, 131);
			this.Controls.Add(this.numHeight);
			this.Controls.Add(this.numWidth);
			this.Controls.Add(this.lblHeight);
			this.Controls.Add(this.lblWidth);
			this.Controls.Add(this.lblDescription);
			this.Controls.Add(this.btnCancel);
			this.Controls.Add(this.btnOK);
			this.FormBorderStyle = System.Windows.Forms.FormBorderStyle.FixedDialog;
			this.MaximizeBox = false;
			this.MinimizeBox = false;
			this.Name = "DiagramSizeDialog";
			this.ShowInTaskbar = false;
			this.StartPosition = System.Windows.Forms.FormStartPosition.CenterParent;
			this.Text = "Diagram Size";
			((System.ComponentModel.ISupportInitialize) (this.numWidth)).EndInit();
			((System.ComponentModel.ISupportInitialize) (this.numHeight)).EndInit();
			this.ResumeLayout(false);
			this.PerformLayout();

		}

		#endregion

		private System.Windows.Forms.Button btnOK;
		private System.Windows.Forms.Button btnCancel;
		private System.Windows.Forms.Label lblDescription;
		private System.Windows.Forms.Label lblWidth;
		private System.Windows.Forms.Label lblHeight;
		private System.Windows.Forms.NumericUpDown numWidth;
		private System.Windows.Forms.NumericUpDown numHeight;


	}
}