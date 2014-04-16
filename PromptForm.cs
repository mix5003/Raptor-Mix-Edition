using System;
using System.Drawing;
using System.Collections;
using System.ComponentModel;
using System.Windows.Forms;

namespace raptor
{
	/// <summary>
	/// Summary description for PromptForm.
	/// </summary>
	public class PromptForm : System.Windows.Forms.Form
	{
		private System.Windows.Forms.Label promptLabel;
		private System.Windows.Forms.TextBox inputBox;
		private System.Windows.Forms.Button OKbutton;
		/// <summary>
		/// Required designer variable.
		/// </summary>
		private System.ComponentModel.Container components = null;
		private string result;
		public static PromptForm current;

		public PromptForm(string str, System.Windows.Forms.Form p)
		{
			//
			// Required for Windows Form Designer support
			//
			InitializeComponent();
			if (str.Length>0) 
			{
				promptLabel.Text = str;
			}
			else
			{
				promptLabel.Text = "Please enter a number.";
			}
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
			System.Resources.ResourceManager resources = new System.Resources.ResourceManager(typeof(PromptForm));
			this.promptLabel = new System.Windows.Forms.Label();
			this.inputBox = new System.Windows.Forms.TextBox();
			this.OKbutton = new System.Windows.Forms.Button();
			this.SuspendLayout();
			// 
			// promptLabel
			// 
			this.promptLabel.Location = new System.Drawing.Point(20, 16);
			this.promptLabel.Name = "promptLabel";
			this.promptLabel.Size = new System.Drawing.Size(236, 40);
			this.promptLabel.TabIndex = 0;
			this.promptLabel.TextAlign = System.Drawing.ContentAlignment.MiddleLeft;
			this.promptLabel.Paint += new System.Windows.Forms.PaintEventHandler(this.promptLabel_Paint);
			// 
			// inputBox
			// 
			this.inputBox.Location = new System.Drawing.Point(16, 64);
			this.inputBox.Name = "inputBox";
			this.inputBox.Size = new System.Drawing.Size(240, 20);
			this.inputBox.TabIndex = 1;
			this.inputBox.Text = "";
			this.inputBox.WordWrap = false;
			this.inputBox.KeyDown += new System.Windows.Forms.KeyEventHandler(this.inputBox_KeyDown);
			// 
			// OKbutton
			// 
			this.OKbutton.Location = new System.Drawing.Point(112, 96);
			this.OKbutton.Name = "OKbutton";
			this.OKbutton.Size = new System.Drawing.Size(56, 24);
			this.OKbutton.TabIndex = 2;
			this.OKbutton.Text = "OK";
			this.OKbutton.Click += new System.EventHandler(this.OKbutton_Click);
			// 
			// PromptForm
			// 
			this.AutoScaleBaseSize = new System.Drawing.Size(5, 13);
			this.ClientSize = new System.Drawing.Size(292, 134);
			this.ControlBox = false;
			this.Controls.Add(this.OKbutton);
			this.Controls.Add(this.inputBox);
			this.Controls.Add(this.promptLabel);
			this.Icon = ((System.Drawing.Icon)(resources.GetObject("$this.Icon")));
			this.MaximizeBox = false;
			this.MinimizeBox = false;
			this.Name = "PromptForm";
			this.Text = "Input";
			this.TopMost = true;
			this.Load += new System.EventHandler(this.PromptForm_Load);
			this.ResumeLayout(true);

		}
		#endregion
	
	

		private void OKbutton_Click(object sender, System.EventArgs e)
		{
            // keep Kill up to date with this
			result = inputBox.Text;
			current = null;
			this.Close();
		}

		public string Go() 
		{
			current = this;
            //causes tasking exception in .NET 2.0
			//this.ShowDialog(Runtime.parent);
            this.ShowDialog();
			// ok, so this code is a bit bizarre, but
			// resolves a bug (.NET?) where if the
			// parent window is minimized the dialog
			// was returning null immediately
			if (result==null)
			{
				this.ShowDialog();
			}
			return result;
		}

		private void inputBox_KeyDown(object sender, System.Windows.Forms.KeyEventArgs e)
		{
			if (e.KeyCode==Keys.Enter ||  e.KeyCode==Keys.Return)
			{
				this.OKbutton_Click(sender,e);
			}
		}

		private void PromptForm_Load(object sender, System.EventArgs e)
		{
			this.inputBox.Focus();
		}

		public static bool Close_All()
		{
			return (current != null);
		}

        public static void Kill_Delegate(PromptForm f)
        {
            f.result = "0";
            f.Close();
        }
        public delegate void Kill_Delegate_Type(PromptForm f);
        public static Kill_Delegate_Type Kill_delegate = new Kill_Delegate_Type(Kill_Delegate);
		public static void Kill()
		{
			if (current!=null)
			{
                // keep me up to date with OKbutton_Click
                // don't call that b/c of tasking.
                Object[] delegate3_args = new Object[1];
                //Object[] delegate_answer;
                delegate3_args[0] = current;
                current.Invoke(Kill_delegate, delegate3_args);
                current = null;
            }
		}

		private void promptLabel_Paint(object sender, System.Windows.Forms.PaintEventArgs e)
		{
			this.inputBox.Focus();
		}

	}

}
