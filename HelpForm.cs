using System;
using System.Drawing;
using System.Collections;
using System.ComponentModel;
using System.Windows.Forms;

namespace raptor
{
	/// <summary>
	/// Summary description for HelpForm.
	/// </summary>
	public class HelpForm : System.Windows.Forms.Form
	{
        // Version information for an assembly consists of the following four values:
        //
        //      Major Version
        //      Minor Version 
        //      Build Number
        //      Revision
        // MAKE SURE TO build release and then RUN UPDATEVERSION.BAT to recompile interpreter.dll
        // WHEN YOU CHANGE THIS!! (or build debug, then toada.bat, then recompiler interpreter.dll)
            // then to create new installer, build release (again), then build installer
        public const string Version_String = "4.0.5.0002";
        public const string Date = "16 August 2012";
		private System.Windows.Forms.Label label1;
		private System.Windows.Forms.Label label2;
		private System.Windows.Forms.Label label3;
		private System.Windows.Forms.PictureBox pictureBox1;
		private System.Windows.Forms.Button button_OK;
		private System.Windows.Forms.PictureBox pictureBox2;
        private Label label4;
        private Label label5;
        private LinkLabel linkLabel1;
		/// <summary>
		/// Required designer variable.
		/// </summary>
		private System.ComponentModel.Container components = null;

		public HelpForm()
		{
			//
			// Required for Windows Form Designer support
			//
			InitializeComponent();
			string who = "Terry A. Wilson\n" + 
				"Martin C. Carlisle\n" + 
				"Jeffrey W. Humphries\n" +
				"Jason A. Moore\n";
			if (Component.USMA_mode) 
			{
				who = who + "\nUSMA edition\n";
			}
			who = who + "\n\nVersion " + Version_String + "\n" + Date;
            if (Component.BARTPE)
            {
                this.linkLabel1.Visible = false;
            }
			this.label3.Text = who;

		}

		/// <summary>
		/// Clean up any resources being used.
		/// </summary>
		protected override void Dispose( bool disposing )
		{
			if( disposing )
			{
				if(components != null)
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
            System.ComponentModel.ComponentResourceManager resources = new System.ComponentModel.ComponentResourceManager(typeof(HelpForm));
            this.label1 = new System.Windows.Forms.Label();
            this.label2 = new System.Windows.Forms.Label();
            this.label3 = new System.Windows.Forms.Label();
            this.pictureBox1 = new System.Windows.Forms.PictureBox();
            this.button_OK = new System.Windows.Forms.Button();
            this.pictureBox2 = new System.Windows.Forms.PictureBox();
            this.label4 = new System.Windows.Forms.Label();
            this.label5 = new System.Windows.Forms.Label();
            this.linkLabel1 = new System.Windows.Forms.LinkLabel();
            ((System.ComponentModel.ISupportInitialize)(this.pictureBox1)).BeginInit();
            ((System.ComponentModel.ISupportInitialize)(this.pictureBox2)).BeginInit();
            this.SuspendLayout();
            // 
            // label1
            // 
            this.label1.BackColor = System.Drawing.SystemColors.Window;
            this.label1.Font = new System.Drawing.Font("Microsoft Sans Serif", 14.25F, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, ((byte)(0)));
            this.label1.Location = new System.Drawing.Point(28, 16);
            this.label1.Name = "label1";
            this.label1.Size = new System.Drawing.Size(240, 32);
            this.label1.TabIndex = 0;
            this.label1.Text = "RAPTOR";
            this.label1.TextAlign = System.Drawing.ContentAlignment.MiddleCenter;
            // 
            // label2
            // 
            this.label2.BackColor = System.Drawing.SystemColors.Window;
            this.label2.Font = new System.Drawing.Font("Microsoft Sans Serif", 9F, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, ((byte)(0)));
            this.label2.Location = new System.Drawing.Point(24, 48);
            this.label2.Name = "label2";
            this.label2.Size = new System.Drawing.Size(248, 32);
            this.label2.TabIndex = 2;
            this.label2.Text = "Rapid Algorithmic Prototyping Tool for Ordered Reasoning";
            this.label2.TextAlign = System.Drawing.ContentAlignment.MiddleCenter;
            // 
            // label3
            // 
            this.label3.BackColor = System.Drawing.SystemColors.Window;
            this.label3.Font = new System.Drawing.Font("Microsoft Sans Serif", 9F, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, ((byte)(0)));
            this.label3.Location = new System.Drawing.Point(24, 96);
            this.label3.Name = "label3";
            this.label3.Size = new System.Drawing.Size(248, 113);
            this.label3.TabIndex = 3;
            // 
            // pictureBox1
            // 
            this.pictureBox1.BackColor = System.Drawing.SystemColors.Window;
            this.pictureBox1.Image = ((System.Drawing.Image)(resources.GetObject("pictureBox1.Image")));
            this.pictureBox1.Location = new System.Drawing.Point(280, 137);
            this.pictureBox1.Name = "pictureBox1";
            this.pictureBox1.Size = new System.Drawing.Size(176, 72);
            this.pictureBox1.SizeMode = System.Windows.Forms.PictureBoxSizeMode.StretchImage;
            this.pictureBox1.TabIndex = 4;
            this.pictureBox1.TabStop = false;
            // 
            // button_OK
            // 
            this.button_OK.Location = new System.Drawing.Point(328, 267);
            this.button_OK.Name = "button_OK";
            this.button_OK.Size = new System.Drawing.Size(88, 32);
            this.button_OK.TabIndex = 5;
            this.button_OK.Text = "OK";
            this.button_OK.Click += new System.EventHandler(this.button_OK_Click);
            // 
            // pictureBox2
            // 
            this.pictureBox2.Image = ((System.Drawing.Image)(resources.GetObject("pictureBox2.Image")));
            this.pictureBox2.Location = new System.Drawing.Point(320, 24);
            this.pictureBox2.Name = "pictureBox2";
            this.pictureBox2.Size = new System.Drawing.Size(96, 48);
            this.pictureBox2.TabIndex = 6;
            this.pictureBox2.TabStop = false;
            // 
            // label4
            // 
            this.label4.AutoSize = true;
            this.label4.Location = new System.Drawing.Point(32, 267);
            this.label4.Name = "label4";
            this.label4.Size = new System.Drawing.Size(250, 13);
            this.label4.TabIndex = 7;
            this.label4.Text = "OO version includes NClass code by Balazs Tihanyi";
            // 
            // label5
            // 
            this.label5.AutoSize = true;
            this.label5.Location = new System.Drawing.Point(32, 283);
            this.label5.Name = "label5";
            this.label5.Size = new System.Drawing.Size(167, 13);
            this.label5.TabIndex = 8;
            this.label5.Text = "See http://nclass.sourceforge.net";
            // 
            // linkLabel1
            // 
            this.linkLabel1.AutoSize = true;
            this.linkLabel1.Location = new System.Drawing.Point(23, 230);
            this.linkLabel1.Name = "linkLabel1";
            this.linkLabel1.Size = new System.Drawing.Size(151, 13);
            this.linkLabel1.TabIndex = 9;
            this.linkLabel1.TabStop = true;
            this.linkLabel1.Text = "http://raptor.martincarlisle.com";
            this.linkLabel1.LinkClicked += new System.Windows.Forms.LinkLabelLinkClickedEventHandler(this.linkLabel1_LinkClicked);
            // 
            // HelpForm
            // 
            this.AutoScaleBaseSize = new System.Drawing.Size(5, 13);
            this.BackColor = System.Drawing.Color.White;
            this.ClientSize = new System.Drawing.Size(468, 310);
            this.Controls.Add(this.linkLabel1);
            this.Controls.Add(this.label5);
            this.Controls.Add(this.label4);
            this.Controls.Add(this.pictureBox2);
            this.Controls.Add(this.button_OK);
            this.Controls.Add(this.pictureBox1);
            this.Controls.Add(this.label3);
            this.Controls.Add(this.label2);
            this.Controls.Add(this.label1);
            this.Icon = ((System.Drawing.Icon)(resources.GetObject("$this.Icon")));
            this.Name = "HelpForm";
            this.Text = "About Raptor";
            ((System.ComponentModel.ISupportInitialize)(this.pictureBox1)).EndInit();
            ((System.ComponentModel.ISupportInitialize)(this.pictureBox2)).EndInit();
            this.ResumeLayout(false);
            this.PerformLayout();

		}
		#endregion

		private void functionsLink_LinkClicked(object sender, System.Windows.Forms.LinkLabelLinkClickedEventArgs e)
		{
			System.Diagnostics.Process.Start("list of functions.htm");
		}


		private void button_OK_Click(object sender, System.EventArgs e)
		{
			this.Close();
		}

        private void linkLabel1_LinkClicked(object sender, LinkLabelLinkClickedEventArgs e)
        {
            System.Diagnostics.Process.Start(this.linkLabel1.Text);
        }




	}
}
