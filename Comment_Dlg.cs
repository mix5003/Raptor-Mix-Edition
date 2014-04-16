using System;
using System.Drawing;
using System.Collections;
using System.ComponentModel;
using System.Windows.Forms;

namespace raptor
{
	/// <summary>
	/// Summary description for Assignment_Dlg.
	/// </summary>
	public class Comment_Dlg : System.Windows.Forms.Form
	{
		private System.Windows.Forms.Label label1;
		private System.Windows.Forms.RichTextBox textBox1;
		private System.Windows.Forms.Button Done_button;
		private CommentBox CB;
		private Visual_Flow_Form the_form;

		/// <summary>
		/// Required designer variable.
		/// </summary>
		private System.ComponentModel.Container components = null;

		public Comment_Dlg(CommentBox CBpointer, Visual_Flow_Form form)
		{
			//
			// Required for Windows Form Designer support
			//
			CB = CBpointer;
			the_form = form;
			InitializeComponent();
			this.label1.Text = "Enter the desired line(s) of comments.";
			if (CB.Text_Array!=null)
			{
				this.textBox1.Lines=CB.Text_Array;
				this.textBox1.Select(this.textBox1.Text.Length,0);
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
			System.Resources.ResourceManager resources = new System.Resources.ResourceManager(typeof(Comment_Dlg));
			this.label1 = new System.Windows.Forms.Label();
			this.textBox1 = new System.Windows.Forms.RichTextBox();
			this.Done_button = new System.Windows.Forms.Button();
			this.SuspendLayout();
			// 
			// label1
			// 
			this.label1.Location = new System.Drawing.Point(39, 16);
			this.label1.Name = "label1";
			this.label1.Size = new System.Drawing.Size(280, 40);
			this.label1.TabIndex = 3;
			// 
			// textBox1
			// 
			this.textBox1.Location = new System.Drawing.Point(15, 72);
			this.textBox1.Name = "textBox1";
			this.textBox1.Size = new System.Drawing.Size(328, 96);
			this.textBox1.TabIndex = 1;
			this.textBox1.Text = "";
			// 
			// Done_button
			// 
			this.Done_button.Location = new System.Drawing.Point(135, 184);
			this.Done_button.Name = "Done_button";
			this.Done_button.Size = new System.Drawing.Size(88, 24);
			this.Done_button.TabIndex = 6;
			this.Done_button.Text = "Done";
			this.Done_button.Click += new System.EventHandler(this.done_button_Click);
			// 
			// Comment_Dlg
			// 
			this.AutoScaleBaseSize = new System.Drawing.Size(5, 13);
			this.ClientSize = new System.Drawing.Size(360, 230);
			this.Controls.Add(this.Done_button);
			this.Controls.Add(this.textBox1);
			this.Controls.Add(this.label1);
			this.Icon = ((System.Drawing.Icon)(resources.GetObject("$this.Icon")));
			this.Name = "Comment_Dlg";
			this.Text = "Enter Comment";
			this.TopMost = true;
			this.KeyDown += new System.Windows.Forms.KeyEventHandler(this.Check_key);
			this.Closed += new System.EventHandler(this.Comment_Dlg_Closed);
			this.ResumeLayout(false);

		}
		#endregion

		private void done_button_Click(object sender, System.EventArgs e)
		{	
			the_form.Make_Undoable();
			CB.Text_Array = this.textBox1.Lines;
			if (CB.Text_Array==null || CB.Text_Array.Length==0)
			{
				CB.parent.My_Comment=null;
				CB.text_change = false;
			}
			else
			{
				CB.text_change = true;
			}
			this.Close();
		}

		protected override void OnPaint(PaintEventArgs e)
		{
			this.textBox1.Focus();		
		}
		


		private void Check_key(object sender, System.Windows.Forms.KeyEventArgs e)
		{
			
			if(e.KeyCode==Keys.Enter || e.KeyCode==Keys.Return)
			{
				done_button_Click(sender, e);
			}
		}

		private void Comment_Dlg_Closed(object sender, System.EventArgs e)
		{
			if (CB.Text_Array==null)
			{
				CB.parent.My_Comment=null;
				CB.text_change = false;
			}
		}

	}
}
