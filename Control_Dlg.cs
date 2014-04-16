using System;
using System.Drawing;
using System.Collections;
using System.ComponentModel;
using System.Windows.Forms;
using System.IO;

namespace raptor
{
	/// <summary>
	/// Summary description for Control_Dlg.
	/// </summary>
	public class Control_Dlg : System.Windows.Forms.Form
	{
		private interpreter.suggestion_result suggestion_result;
		private interpreter.syntax_result result;
		private Component Cmp;
		private bool error = false;
		public System.Windows.Forms.TextBox Control_Text;
		private System.Windows.Forms.Button done_button;
		private System.Windows.Forms.Label label1;
		private System.Windows.Forms.Label label2;
		private System.Drawing.Graphics labelGraphics;
		private string error_msg;

		private System.Windows.Forms.Splitter splitter1;
		private System.Windows.Forms.RichTextBox textBox1;
		private string current_suggestion;

		/// <summary>
		/// Required designer variable.
		/// </summary>
		private System.ComponentModel.Container components = null;
		private System.Windows.Forms.MainMenu mainMenu1;
		private System.Windows.Forms.MenuItem menuHelp;
		private System.Windows.Forms.MenuItem menuGeneralHelp;
		private Visual_Flow_Form the_form;
		private string examples = "Examples:" + '\n' + 
			"   Count = X+2" + '\n' + 
			"   Count != 5" + '\n' + 
			"   Score_Array[4] < 10" + '\n' +  
			"   Middle <= Y and Y <= Top";

		public Control_Dlg(Component parent_Cmp, 
			Visual_Flow_Form form,
			bool is_loop)
		{
			Cmp = parent_Cmp;
			the_form = form;
			//
			// Required for Windows Form Designer support
			//
			InitializeComponent();
            Dialog_Helpers.Init();

			if (is_loop)
			{
                if (!Component.reverse_loop_logic)
                {
                    this.label1.Text = "Enter loop exit condition." +
                        '\n' + '\n' + this.examples;
                }
                else
                {
                    this.label1.Text = "Enter loop condition." +
                        '\n' + '\n' + this.examples;
                }
				this.Text = "Enter Loop Condition";
			}
			else
			{
				this.label1.Text = "Enter selection condition." +
					'\n' + '\n' + this.examples;
				this.Text = "Enter Selection Condition";
			}
			this.labelGraphics = label2.CreateGraphics();
			if (Cmp.Text != null)
			{
				this.Control_Text.Text = Cmp.Text;
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
			System.Resources.ResourceManager resources = new System.Resources.ResourceManager(typeof(Control_Dlg));
			this.Control_Text = new System.Windows.Forms.TextBox();
			this.done_button = new System.Windows.Forms.Button();
			this.label1 = new System.Windows.Forms.Label();
			this.label2 = new System.Windows.Forms.Label();
			this.splitter1 = new System.Windows.Forms.Splitter();
			this.textBox1 = new System.Windows.Forms.RichTextBox();
			this.mainMenu1 = new System.Windows.Forms.MainMenu();
			this.menuHelp = new System.Windows.Forms.MenuItem();
			this.menuGeneralHelp = new System.Windows.Forms.MenuItem();
			this.SuspendLayout();
			// 
			// Control_Text
			// 
			this.Control_Text.AcceptsReturn = true;
			this.Control_Text.AcceptsTab = true;
			this.Control_Text.Location = new System.Drawing.Point(48, 96);
			this.Control_Text.Multiline = true;
			this.Control_Text.Name = "Control_Text";
			this.Control_Text.ScrollBars = System.Windows.Forms.ScrollBars.Vertical;
			this.Control_Text.Size = new System.Drawing.Size(200, 72);
			this.Control_Text.TabIndex = 1;
			this.Control_Text.Text = "";
			this.Control_Text.KeyDown += new System.Windows.Forms.KeyEventHandler(this.Check_key);
			this.Control_Text.KeyPress += new System.Windows.Forms.KeyPressEventHandler(this.Control_Text_KeyPress);
			this.Control_Text.TextChanged += new System.EventHandler(this.Check_Hint);
			this.Control_Text.KeyUp += new System.Windows.Forms.KeyEventHandler(this.Control_Text_KeyUp);
			// 
			// done_button
			// 
			this.done_button.Location = new System.Drawing.Point(104, 368);
			this.done_button.Name = "done_button";
			this.done_button.Size = new System.Drawing.Size(88, 24);
			this.done_button.TabIndex = 2;
			this.done_button.Text = "Done";
			this.done_button.Click += new System.EventHandler(this.done_button_Click);
			// 
			// label1
			// 
			this.label1.Location = new System.Drawing.Point(51, 8);
			this.label1.Name = "label1";
			this.label1.Size = new System.Drawing.Size(221, 88);
			this.label1.TabIndex = 3;
			// 
			// label2
			// 
			this.label2.Location = new System.Drawing.Point(16, 184);
			this.label2.Name = "label2";
			this.label2.Size = new System.Drawing.Size(264, 48);
			this.label2.TabIndex = 4;
			// 
			// splitter1
			// 
			this.splitter1.Location = new System.Drawing.Point(0, 0);
			this.splitter1.Name = "splitter1";
			this.splitter1.Size = new System.Drawing.Size(3, 401);
			this.splitter1.TabIndex = 5;
			this.splitter1.TabStop = false;
			// 
			// textBox1
			// 
			this.textBox1.Location = new System.Drawing.Point(8, 248);
			this.textBox1.Name = "textBox1";
			this.textBox1.ReadOnly = true;
			this.textBox1.Size = new System.Drawing.Size(272, 112);
			this.textBox1.TabIndex = 6;
			this.textBox1.TabStop = false;
			this.textBox1.Text = "";
			this.textBox1.Visible = false;
			this.textBox1.MouseDown += new System.Windows.Forms.MouseEventHandler(this.textBox1_MouseDown);
			// 
			// mainMenu1
			// 
			this.mainMenu1.MenuItems.AddRange(new System.Windows.Forms.MenuItem[] {
																					  this.menuHelp});
			// 
			// menuHelp
			// 
			this.menuHelp.Index = 0;
			this.menuHelp.MenuItems.AddRange(new System.Windows.Forms.MenuItem[] {
																					 this.menuGeneralHelp});
			this.menuHelp.Text = "&Help";
			// 
			// menuGeneralHelp
			// 
			this.menuGeneralHelp.Index = 0;
			this.menuGeneralHelp.Shortcut = System.Windows.Forms.Shortcut.F1;
			this.menuGeneralHelp.Text = "&General Help";
			this.menuGeneralHelp.Click += new System.EventHandler(this.menuGeneralHelp_Click);
			// 
			// Control_Dlg
			// 
			this.AutoScaleBaseSize = new System.Drawing.Size(5, 13);
			this.ClientSize = new System.Drawing.Size(287, 401);
			this.Controls.Add(this.textBox1);
			this.Controls.Add(this.splitter1);
			this.Controls.Add(this.label2);
			this.Controls.Add(this.label1);
			this.Controls.Add(this.done_button);
			this.Controls.Add(this.Control_Text);
			this.Icon = ((System.Drawing.Icon)(resources.GetObject("$this.Icon")));
			this.Menu = this.mainMenu1;
			this.Name = "Control_Dlg";
			this.Text = "Control_Dlg";
			this.TopMost = true;
			this.Resize += new System.EventHandler(this.Control_Dlg_Resize);
			this.ResumeLayout(false);

		}
		#endregion

		private void done_button_Click(object sender, System.EventArgs e)
		{

			if (this.Control_Text.Text.CompareTo(Cmp.Text)==0)
			{
				// nothing changed
				this.Close();
				return;
			}
			result = interpreter_pkg.conditional_syntax(this.Control_Text.Text,Cmp);
			if (result.valid)
			{
				the_form.Make_Undoable();
				Cmp.Text = this.Control_Text.Text;
				Cmp.parse_tree = result.tree;
				Cmp.changed();
				this.error = false;
				this.Close();
			}

			else
			{
				this.error = true;
				this.error_msg = result.message;
				this.Invalidate();
			}
		}

		protected override void OnPaint(PaintEventArgs e)
		{
			int location;

			if (this.result!=null)
			{
				location = this.result.location;
			}
			else
			{
				location = 0;
			}
			this.labelGraphics = this.label2.CreateGraphics();
			Dialog_Helpers.Paint_Helper(
				this.labelGraphics,
				this.Control_Text.Text,
				this.label1,
				this.error_msg,
				location,
				this.error);
			this.Control_Text.Focus();
		}

		private bool Complete_Suggestion()
		{
			return Dialog_Helpers.Complete_Suggestion(
				this.Control_Text,
                interpreter_pkg.expr_dialog,
				this.current_suggestion,
				ref this.suggestion_result);
		}

		private void Check_key(object sender, System.Windows.Forms.KeyEventArgs e)
		{
			
			if(e.KeyCode==Keys.Enter || e.KeyCode==Keys.Return)
			{
				e.Handled = this.Complete_Suggestion();
				if (!e.Handled)
				{
					this.Control_Text.Select(this.Control_Text.TextLength,0);
					done_button_Click(sender, e);
					e.Handled = true;
				}
				
			}
			else if (e.KeyCode.ToString() == "Down")
			{
				e.Handled = true;
				Dialog_Helpers.suggestions_downarrow(
					this.textBox1,
					ref this.current_suggestion);
			}
			else if (e.KeyCode.ToString() == "Up")
			{
				e.Handled = true;
				Dialog_Helpers.suggestions_uparrow(
					this.textBox1,
					ref this.current_suggestion);
			}
		}

		private void Check_Hint(object sender, System.EventArgs e)
		{
			Dialog_Helpers.Check_Hint(
				this.Control_Text,
				this.textBox1,
                interpreter_pkg.expr_dialog,
				ref this.current_suggestion,
				ref this.suggestion_result,
				ref this.error,
				this.Font);
			this.Invalidate();
		}
		private void Control_Text_KeyPress(object sender, System.Windows.Forms.KeyPressEventArgs e)
		{
			const char ESC = ((char) 27);

			if (e.KeyChar=='\t')
			{
				e.Handled = this.Complete_Suggestion();
				if (!e.Handled)
				{
					this.done_button.Focus();
				}
				e.Handled=true;
			}
			else if (e.KeyChar==ESC)
			{
				this.Close();
			}
		}
		private void textBox1_MouseDown(object sender, System.Windows.Forms.MouseEventArgs e)
		{
			bool Handled;
			Dialog_Helpers.suggestions_mousedown(
				this.textBox1,
				ref this.current_suggestion,
				e);
			if (e.Clicks==2)
			{
				Handled = this.Complete_Suggestion();
			}
			this.Control_Text.Focus();
		}

		private void menuGeneralHelp_Click(object sender, System.EventArgs e)
		{
            if (!Component.BARTPE && !Component.VM)
            {
                Help.ShowHelp(this, Directory.GetParent(
                    Application.ExecutablePath) + "\\raptor.chm");
            }
            else
            {
                MessageBox.Show("Help not installed properly");
            }
		}

		private void Control_Text_KeyUp(object sender, System.Windows.Forms.KeyEventArgs e)
		{
			if (e.KeyCode==Keys.Enter || e.KeyCode==Keys.Return)
			{
				e.Handled=true;
			}
		}

		private void Control_Dlg_Resize(object sender, System.EventArgs e)
		{
			label2.Width = this.Width-31;
			this.Control_Text.Width = this.Width-95;
			this.Control_Text.Invalidate();
			this.label2.Invalidate();
		}

	}

}
