using System;
using System.Drawing;
using System.Collections;
using System.ComponentModel;
using System.Windows.Forms;

namespace raptor
{
	/// <summary>
	/// Summary description for Subchart_name.
	/// </summary>
	public class Subchart_name : System.Windows.Forms.Form
	{
		private System.Windows.Forms.TextBox textBox1;
		private System.Windows.Forms.Button button1;
		private System.Windows.Forms.Label label1;
		private System.Windows.Forms.Label label2;
		private System.Windows.Forms.Label label3;
		private static string result;
		private string init_name;
		private System.Windows.Forms.Label label4;
		private Visual_Flow_Form form;
        private Button buttonCancel;

		/// <summary>
		/// Required designer variable.
		/// </summary>
		private System.ComponentModel.Container components = null;

		public Subchart_name()
		{
			//
			// Required for Windows Form Designer support
			//
			InitializeComponent();

			//
			// TODO: Add any constructor code after InitializeComponent call
			//
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

		public static string RunDialog(string s, Visual_Flow_Form the_form) 
		{
			Subchart_name dialog = new Subchart_name();
			dialog.textBox1.Text=s;
			dialog.init_name=s.ToLower();
			dialog.form = the_form;
			dialog.ShowDialog();
			return result;
		}

		#region Windows Form Designer generated code
		/// <summary>
		/// Required method for Designer support - do not modify
		/// the contents of this method with the code editor.
		/// </summary>
		private void InitializeComponent()
		{
            this.textBox1 = new System.Windows.Forms.TextBox();
            this.button1 = new System.Windows.Forms.Button();
            this.label1 = new System.Windows.Forms.Label();
            this.label2 = new System.Windows.Forms.Label();
            this.label3 = new System.Windows.Forms.Label();
            this.label4 = new System.Windows.Forms.Label();
            this.buttonCancel = new System.Windows.Forms.Button();
            this.SuspendLayout();
            // 
            // textBox1
            // 
            this.textBox1.AcceptsReturn = true;
            this.textBox1.Location = new System.Drawing.Point(22, 176);
            this.textBox1.Name = "textBox1";
            this.textBox1.Size = new System.Drawing.Size(248, 20);
            this.textBox1.TabIndex = 0;
            this.textBox1.Text = "textBox1";
            this.textBox1.KeyUp += new System.Windows.Forms.KeyEventHandler(this.Control_Text_KeyUp);
            this.textBox1.KeyDown += new System.Windows.Forms.KeyEventHandler(this.Check_key);
            // 
            // button1
            // 
            this.button1.Location = new System.Drawing.Point(42, 211);
            this.button1.Name = "button1";
            this.button1.Size = new System.Drawing.Size(75, 23);
            this.button1.TabIndex = 1;
            this.button1.Text = "Ok";
            this.button1.Click += new System.EventHandler(this.button1_Click);
            // 
            // label1
            // 
            this.label1.Font = new System.Drawing.Font("Microsoft Sans Serif", 11.25F, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, ((byte)(0)));
            this.label1.Location = new System.Drawing.Point(40, 16);
            this.label1.Name = "label1";
            this.label1.Size = new System.Drawing.Size(216, 24);
            this.label1.TabIndex = 2;
            this.label1.Text = "Please enter name of subchart";
            // 
            // label2
            // 
            this.label2.Location = new System.Drawing.Point(36, 48);
            this.label2.Name = "label2";
            this.label2.Size = new System.Drawing.Size(224, 40);
            this.label2.TabIndex = 3;
            this.label2.Text = "Name must begin with letter, and contain only letters, numbers and underscores.";
            // 
            // label3
            // 
            this.label3.Location = new System.Drawing.Point(36, 87);
            this.label3.Name = "label3";
            this.label3.Size = new System.Drawing.Size(224, 57);
            this.label3.TabIndex = 4;
            this.label3.Text = "Examples:";
            // 
            // label4
            // 
            this.label4.Location = new System.Drawing.Point(32, 152);
            this.label4.Name = "label4";
            this.label4.Size = new System.Drawing.Size(224, 16);
            this.label4.TabIndex = 5;
            // 
            // buttonCancel
            // 
            this.buttonCancel.Location = new System.Drawing.Point(180, 211);
            this.buttonCancel.Name = "buttonCancel";
            this.buttonCancel.Size = new System.Drawing.Size(75, 23);
            this.buttonCancel.TabIndex = 6;
            this.buttonCancel.Text = "Cancel";
            this.buttonCancel.Click += new System.EventHandler(this.button2_Click);
            // 
            // Subchart_name
            // 
            this.AutoScaleBaseSize = new System.Drawing.Size(5, 13);
            this.ClientSize = new System.Drawing.Size(296, 246);
            this.ControlBox = false;
            this.Controls.Add(this.buttonCancel);
            this.Controls.Add(this.label4);
            this.Controls.Add(this.label3);
            this.Controls.Add(this.label2);
            this.Controls.Add(this.label1);
            this.Controls.Add(this.button1);
            this.Controls.Add(this.textBox1);
            this.MaximizeBox = false;
            this.MinimizeBox = false;
            this.Name = "Subchart_name";
            this.Text = "Name Subchart";
            this.Load += new System.EventHandler(this.Subchart_name_Load);
            this.ResumeLayout(false);
            this.PerformLayout();

		}
		#endregion

		private void Subchart_name_Load(object sender, System.EventArgs e)
		{
			this.label3.Text="Examples:\n   Draw_Boxes\n   Find_Smallest";
		}

		private bool All_Legal(string s)
		{
			for (int i=0; i<s.Length; i++)
			{
				if (!Char.IsLetterOrDigit(s,i) && s[i]!='_')
				{
					return false;
				}
			}
			return true;
		}

		private void button1_Click(object sender, System.EventArgs e)
		{
			string temp_result = this.textBox1.Text.Trim();
            result = "";
			if (temp_result.Length==0)
			{
				this.label4.Text="Can't have blank name";
			}
			else if (!Char.IsLetter(temp_result,0))
			{
				this.label4.Text="Name must begin with letter";
			}
			else if (!All_Legal(temp_result))
			{
				this.label4.Text="Use only letter, number, or underscore";
			}
			else if (!token_helpers_pkg.verify_id(temp_result))
			{
				this.label4.Text=temp_result + " is a reserved word";
			}
			else if (form.Is_Subchart_Name(temp_result) && temp_result.ToLower()!=init_name)
			{
				this.label4.Text=temp_result + " is already used";
			}
			else
			{
                result = temp_result;
				this.Close();
			}
		}
		private void Control_Text_KeyUp(object sender, System.Windows.Forms.KeyEventArgs e)
		{
            if (e.KeyCode==Keys.Enter || e.KeyCode==Keys.Return)
			{
				e.Handled=true;
			}
		}
		private void Check_key(object sender, System.Windows.Forms.KeyEventArgs e)
		{
			if(e.KeyCode==Keys.Enter || e.KeyCode==Keys.Return)
			{
				button1_Click(sender, e);
				e.Handled = true;
			}
		}

        private void button2_Click(object sender, EventArgs e)
        {
            result = "";
            this.Close();
        }



	}
}
