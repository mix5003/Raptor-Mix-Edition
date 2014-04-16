namespace dotnetgraphlibrary
{
    partial class dotnetgraph
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
            this.SuspendLayout();
            // 
            // dotnetgraph
            // 
            this.AutoScaleDimensions = new System.Drawing.SizeF(6F, 13F);
            this.AutoScaleMode = System.Windows.Forms.AutoScaleMode.Font;
            this.ClientSize = new System.Drawing.Size(571, 431);
            this.DoubleBuffered = true;
            this.MaximizeBox = false;
            this.Name = "dotnetgraph";
            this.Text = "Form1";
            this.MouseUp += new System.Windows.Forms.MouseEventHandler(this.dotnetgraph_MouseUp);
            this.Paint += new System.Windows.Forms.PaintEventHandler(this.Form1_Paint);
            this.FormClosed += new System.Windows.Forms.FormClosedEventHandler(this.Form1_FormClosed);
            this.MouseDown += new System.Windows.Forms.MouseEventHandler(this.dotnetgraph_MouseDown);
            this.KeyPress += new System.Windows.Forms.KeyPressEventHandler(this.dotnetgraph_KeyPress);
            this.KeyUp += new System.Windows.Forms.KeyEventHandler(this.dotnetgraph_KeyUp);
            this.MouseMove += new System.Windows.Forms.MouseEventHandler(this.dotnetgraph_MouseMove);
            this.KeyDown += new System.Windows.Forms.KeyEventHandler(this.dotnetgraph_KeyDown);
            this.ResumeLayout(false);

        }

        #endregion
    }
}

