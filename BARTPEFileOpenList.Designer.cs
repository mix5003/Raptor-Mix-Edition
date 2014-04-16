namespace raptor
{
    partial class BARTPEFileOpenList
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

        #region Windows Form Designer generated code

        /// <summary>
        /// Required method for Designer support - do not modify
        /// the contents of this method with the code editor.
        /// </summary>
        private void InitializeComponent()
        {
            this.label1 = new System.Windows.Forms.Label();
            this.filesListBox1 = new FilesBrowser.FilesListBox();
            this.checkBox1 = new System.Windows.Forms.CheckBox();
            this.SuspendLayout();
            // 
            // label1
            // 
            this.label1.AutoSize = true;
            this.label1.Dock = System.Windows.Forms.DockStyle.Bottom;
            this.label1.Location = new System.Drawing.Point(0, 253);
            this.label1.Name = "label1";
            this.label1.Size = new System.Drawing.Size(121, 13);
            this.label1.TabIndex = 1;
            this.label1.Text = "Double click file to open";
            // 
            // filesListBox1
            // 
            this.filesListBox1.Dock = System.Windows.Forms.DockStyle.Fill;
            this.filesListBox1.DrawMode = System.Windows.Forms.DrawMode.OwnerDrawFixed;
            this.filesListBox1.Extension = ".rap";
            this.filesListBox1.FileIconSize = FilesBrowser.IconSize.Small;
            this.filesListBox1.FormattingEnabled = true;
            this.filesListBox1.ItemHeight = 16;
            this.filesListBox1.Location = new System.Drawing.Point(0, 0);
            this.filesListBox1.Name = "filesListBox1";
            this.filesListBox1.Size = new System.Drawing.Size(292, 244);
            this.filesListBox1.TabIndex = 2;
            this.filesListBox1.FileSelected += new FilesBrowser.FileSelectedEventHandler(this.filesListBox1_FileSelected);
            // 
            // checkBox1
            // 
            this.checkBox1.AutoSize = true;
            this.checkBox1.Location = new System.Drawing.Point(168, 251);
            this.checkBox1.Name = "checkBox1";
            this.checkBox1.Size = new System.Drawing.Size(113, 17);
            this.checkBox1.TabIndex = 3;
            this.checkBox1.Text = "Show backup files";
            this.checkBox1.UseVisualStyleBackColor = true;
            this.checkBox1.CheckedChanged += new System.EventHandler(this.checkBox1_CheckedChanged);
            // 
            // BARTPEFileOpenList
            // 
            this.AutoScaleDimensions = new System.Drawing.SizeF(6F, 13F);
            this.AutoScaleMode = System.Windows.Forms.AutoScaleMode.Font;
            this.ClientSize = new System.Drawing.Size(292, 266);
            this.Controls.Add(this.checkBox1);
            this.Controls.Add(this.filesListBox1);
            this.Controls.Add(this.label1);
            this.Name = "BARTPEFileOpenList";
            this.Text = "Open File";
            this.ResumeLayout(false);
            this.PerformLayout();

        }

        #endregion

        private System.Windows.Forms.Label label1;
        private FilesBrowser.FilesListBox filesListBox1;
        private System.Windows.Forms.CheckBox checkBox1;

    }
}
