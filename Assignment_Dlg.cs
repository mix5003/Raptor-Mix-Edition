using System;
using System.Drawing;
using System.Collections;
using System.ComponentModel;
using System.Windows.Forms;
using System.IO;

namespace raptor
{
	/// <summary>
	/// Summary description for Assignment_Dlg.
	/// </summary>
	public class Assignment_Dlg : System.Windows.Forms.Form
	{
		private interpreter.suggestion_result suggestion_result;
		private interpreter.syntax_result result;
		private Rectangle Rec;
		private bool error = false;
		private bool is_shifted = false;

		private System.Windows.Forms.Label label1;
		private System.Drawing.Graphics labelGraphics;
		private Visual_Flow_Form the_form;

		private String error_msg;
		private String current_suggestion = "";
		private System.Windows.Forms.MainMenu mainMenu1;
		private System.Windows.Forms.MenuItem menuHelp;
		private System.Windows.Forms.MenuItem menuGeneralHelp;
		private System.Windows.Forms.Label label4;
		private System.Windows.Forms.Label label3;
		private System.Windows.Forms.TextBox lhsTextBox;
		private System.Windows.Forms.RichTextBox suggestionTextBox;
		private System.Windows.Forms.Label label2;
		private System.Windows.Forms.Button done_button;
        public System.Windows.Forms.TextBox assignment_Text;
        private IContainer components;

		public Assignment_Dlg(Rectangle parent_Rec, Visual_Flow_Form form)
		{
			int index;
			Rec = parent_Rec;
			the_form = form;
			//
			// Required for Windows Form Designer support
			//
			InitializeComponent();
            Dialog_Helpers.Init();
			this.label1.Text = "Enter an assignment." + 
				'\n' + '\n' + "Examples:" + '\n' + "   Set Coins to 5" +
				'\n' +  "   Set Count to Count + 1" +
				'\n' + "   Set Board[3,3] to 0";
			this.labelGraphics = label2.CreateGraphics();
			if (Rec.Text != null)
			{
				index = Rec.Text.IndexOf(":=");
				if (index>0)
				{
					this.lhsTextBox.Text = Rec.Text.Substring(0,index);
					this.assignment_Text.Text =
						Rec.Text.Substring(index+2,
						Rec.Text.Length-(index+2));
				}
				else
				{
					this.lhsTextBox.Text = Rec.Text;
				}
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
            this.components = new System.ComponentModel.Container();
            System.ComponentModel.ComponentResourceManager resources = new System.ComponentModel.ComponentResourceManager(typeof(Assignment_Dlg));
            this.label1 = new System.Windows.Forms.Label();
            this.mainMenu1 = new System.Windows.Forms.MainMenu(this.components);
            this.menuHelp = new System.Windows.Forms.MenuItem();
            this.menuGeneralHelp = new System.Windows.Forms.MenuItem();
            this.label4 = new System.Windows.Forms.Label();
            this.label3 = new System.Windows.Forms.Label();
            this.lhsTextBox = new System.Windows.Forms.TextBox();
            this.suggestionTextBox = new System.Windows.Forms.RichTextBox();
            this.label2 = new System.Windows.Forms.Label();
            this.done_button = new System.Windows.Forms.Button();
            this.assignment_Text = new System.Windows.Forms.TextBox();
            this.SuspendLayout();
            // 
            // label1
            // 
            this.label1.Location = new System.Drawing.Point(51, 16);
            this.label1.Name = "label1";
            this.label1.Size = new System.Drawing.Size(221, 80);
            this.label1.TabIndex = 3;
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
            // label4
            // 
            this.label4.Location = new System.Drawing.Point(16, 152);
            this.label4.Name = "label4";
            this.label4.Size = new System.Drawing.Size(16, 16);
            this.label4.TabIndex = 15;
            this.label4.Text = "to";
            this.label4.TextAlign = System.Drawing.ContentAlignment.MiddleLeft;
            // 
            // label3
            // 
            this.label3.Location = new System.Drawing.Point(16, 112);
            this.label3.Name = "label3";
            this.label3.Size = new System.Drawing.Size(32, 24);
            this.label3.TabIndex = 14;
            this.label3.Text = "Set";
            this.label3.TextAlign = System.Drawing.ContentAlignment.MiddleLeft;
            // 
            // lhsTextBox
            // 
            this.lhsTextBox.Location = new System.Drawing.Point(48, 112);
            this.lhsTextBox.Name = "lhsTextBox";
            this.lhsTextBox.Size = new System.Drawing.Size(200, 20);
            this.lhsTextBox.TabIndex = 9;
            this.lhsTextBox.KeyPress += new System.Windows.Forms.KeyPressEventHandler(this.textBox2_KeyPress);
            this.lhsTextBox.TextChanged += new System.EventHandler(this.Check_Hint_Lhs);
            this.lhsTextBox.KeyDown += new System.Windows.Forms.KeyEventHandler(this.textBox2_KeyDown);
            // 
            // suggestionTextBox
            // 
            this.suggestionTextBox.Location = new System.Drawing.Point(8, 304);
            this.suggestionTextBox.Name = "suggestionTextBox";
            this.suggestionTextBox.ReadOnly = true;
            this.suggestionTextBox.Size = new System.Drawing.Size(272, 96);
            this.suggestionTextBox.TabIndex = 12;
            this.suggestionTextBox.TabStop = false;
            this.suggestionTextBox.Text = "";
            this.suggestionTextBox.Visible = false;
            this.suggestionTextBox.MouseDown += new System.Windows.Forms.MouseEventHandler(this.textBox1_MouseDown);
            // 
            // label2
            // 
            this.label2.Location = new System.Drawing.Point(16, 248);
            this.label2.Name = "label2";
            this.label2.Size = new System.Drawing.Size(264, 48);
            this.label2.TabIndex = 11;
            // 
            // done_button
            // 
            this.done_button.Location = new System.Drawing.Point(104, 408);
            this.done_button.Name = "done_button";
            this.done_button.Size = new System.Drawing.Size(88, 24);
            this.done_button.TabIndex = 16;
            this.done_button.Text = "Done";
            this.done_button.Click += new System.EventHandler(this.done_button_Click);
            // 
            // assignment_Text
            // 
            this.assignment_Text.AcceptsReturn = true;
            this.assignment_Text.AcceptsTab = true;
            this.assignment_Text.Location = new System.Drawing.Point(48, 152);
            this.assignment_Text.Multiline = true;
            this.assignment_Text.Name = "assignment_Text";
            this.assignment_Text.ScrollBars = System.Windows.Forms.ScrollBars.Vertical;
            this.assignment_Text.Size = new System.Drawing.Size(200, 72);
            this.assignment_Text.TabIndex = 13;
            this.assignment_Text.KeyPress += new System.Windows.Forms.KeyPressEventHandler(this.assignment_Text_KeyPress);
            this.assignment_Text.TextChanged += new System.EventHandler(this.Check_Hint);
            this.assignment_Text.KeyDown += new System.Windows.Forms.KeyEventHandler(this.Check_key);
            // 
            // Assignment_Dlg
            // 
            this.AutoScaleBaseSize = new System.Drawing.Size(5, 13);
            this.ClientSize = new System.Drawing.Size(296, 449);
            this.Controls.Add(this.label4);
            this.Controls.Add(this.label3);
            this.Controls.Add(this.lhsTextBox);
            this.Controls.Add(this.assignment_Text);
            this.Controls.Add(this.suggestionTextBox);
            this.Controls.Add(this.label2);
            this.Controls.Add(this.done_button);
            this.Controls.Add(this.label1);
            this.Icon = ((System.Drawing.Icon)(resources.GetObject("$this.Icon")));
            this.Menu = this.mainMenu1;
            this.Name = "Assignment_Dlg";
            this.Text = "Enter Statement";
            this.TopMost = true;
            this.Resize += new System.EventHandler(this.Assignment_Dlg_Resize);
            this.KeyDown += new System.Windows.Forms.KeyEventHandler(this.Check_key);
            this.ResumeLayout(false);
            this.PerformLayout();

		}
		#endregion

		private void done_button_Click(object sender, System.EventArgs e)
		{
			if (Rec.Text!=null && Rec.Text!="" && 
				Rec.Text.CompareTo(this.lhsTextBox.Text+":="+
				this.assignment_Text.Text)==0)
			{
				// nothing changed
				this.Close();
				return;
			}
			result = interpreter_pkg.assignment_syntax(this.lhsTextBox.Text,
				this.assignment_Text.Text,Rec);
			if (result.valid)
			{
				the_form.Make_Undoable();
				Rec.Text = this.lhsTextBox.Text + ":=" +
					this.assignment_Text.Text;
				Rec.parse_tree = result.tree;
				Rec.changed();
				this.error = false;
				((Visual_Flow_Form) the_form).flow_panel.Invalidate();
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
				this.lhsTextBox.Text + ' ' + Component.assignmentSymbol + this.assignment_Text.Text,
				this.label1,
				this.error_msg,
				location,
				this.error);
		}

		private bool Complete_Suggestion()
		{
			return Dialog_Helpers.Complete_Suggestion(
				this.assignment_Text,
                interpreter_pkg.expr_dialog,
				this.current_suggestion,
				ref this.suggestion_result);
		}
        private bool Complete_Suggestion_Lhs()
        {
            return Dialog_Helpers.Complete_Suggestion(
                this.lhsTextBox,
                interpreter_pkg.lhs_dialog,
                this.current_suggestion,
                ref this.suggestion_result);
        }
		private void Check_key(object sender, System.Windows.Forms.KeyEventArgs e)
		{
			if (e.Shift)
			{
				is_shifted = true;
			}
			else
			{
				is_shifted = false;
			}
			if(e.KeyCode==Keys.Enter || e.KeyCode==Keys.Return)
			{
				e.Handled = this.Complete_Suggestion();
				if (!e.Handled)
				{
					e.Handled = true;
					this.assignment_Text.Select(this.assignment_Text.TextLength,0);
					done_button_Click(sender, e);
				}
				
			}
			else if (e.KeyCode.ToString() == "Down")
			{
				e.Handled = true;
				Dialog_Helpers.suggestions_downarrow(
					this.suggestionTextBox,
					ref this.current_suggestion);
			}
			else if (e.KeyCode.ToString() == "Up")
			{
				e.Handled = true;
				Dialog_Helpers.suggestions_uparrow(
					this.suggestionTextBox,
					ref this.current_suggestion);
			}
		}

		private void Check_Hint(object sender, System.EventArgs e)
		{
			Dialog_Helpers.Check_Hint(
				this.assignment_Text,
				this.suggestionTextBox,
                interpreter_pkg.expr_dialog,
				ref this.current_suggestion,
				ref this.suggestion_result,
				ref this.error,
				this.Font);
			this.Invalidate();
		}
        private void Check_Hint_Lhs(object sender, System.EventArgs e)
        {
            Dialog_Helpers.Check_Hint(
                this.lhsTextBox,
                this.suggestionTextBox,
                interpreter_pkg.lhs_dialog,
                ref this.current_suggestion,
                ref this.suggestion_result,
                ref this.error,
                this.Font);
            this.Invalidate();
        }
		private void assignment_Text_KeyPress(object sender, System.Windows.Forms.KeyPressEventArgs e)
		{
			const char ESC = ((char) 27);

			if (e.KeyChar=='\t')
			{
				e.Handled = this.Complete_Suggestion();
				if (!e.Handled)
				{
					if (is_shifted)
					{
						this.lhsTextBox.Focus();
					}
					else
					{
						this.done_button.Focus();
					}
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
				this.suggestionTextBox,
				ref this.current_suggestion,
				e);
			if (e.Clicks==2)
			{
				Handled = this.Complete_Suggestion();
			}
			this.assignment_Text.Focus();
		}

		private void menuGeneralHelp_Click(object sender, System.EventArgs e)
		{
            if ((!Component.BARTPE) && !Component.VM)
            {
                Help.ShowHelp(this, Directory.GetParent(
                    Application.ExecutablePath) + "\\raptor.chm");
            }
            else
            {
                MessageBox.Show("Help not installed properly");
            }

		}

		private void textBox2_KeyDown(object sender, System.Windows.Forms.KeyEventArgs e)
		{
            if (e.KeyCode == Keys.Enter || e.KeyCode == Keys.Return)
            {
                e.Handled = this.Complete_Suggestion_Lhs();
                e.SuppressKeyPress = e.Handled;
                if (!e.Handled)
                {
                    done_button_Click(sender, e);
                }
            }
            else if (e.KeyCode.ToString() == "Down")
            {
                e.Handled = true;
                e.SuppressKeyPress = e.Handled;
                Dialog_Helpers.suggestions_downarrow(
                    this.suggestionTextBox,
                    ref this.current_suggestion);
            }
            else if (e.KeyCode.ToString() == "Up")
            {
                e.Handled = true;
                e.SuppressKeyPress = e.Handled;
                Dialog_Helpers.suggestions_uparrow(
                    this.suggestionTextBox,
                    ref this.current_suggestion);
            }
		}

		private void textBox2_KeyPress(object sender, System.Windows.Forms.KeyPressEventArgs e)
		{
			const char ESC = ((char) 27);

			if (e.KeyChar=='\t')
			{
				e.Handled = this.Complete_Suggestion();
				if (!e.Handled)
				{
					if (is_shifted)
					{
						this.done_button.Focus();
					}
					else
					{
						this.assignment_Text.Focus();
					}
				}
				e.Handled=true;
			}
            else if (e.KeyChar == ESC)
			{
				this.Close();
			}
		
		}

		private void Assignment_Dlg_Resize(object sender, System.EventArgs e)
		{
			label2.Width = this.Width-40;
			this.lhsTextBox.Width = this.Width-104;
			this.assignment_Text.Width = this.Width-104;
			this.label2.Invalidate();
			this.lhsTextBox.Invalidate();
			this.assignment_Text.Invalidate();
		}

	}
}
