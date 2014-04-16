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
	public class Call_Dialog : System.Windows.Forms.Form
	{
		private interpreter.suggestion_result suggestion_result;
		private interpreter.syntax_result result;
		private Rectangle Rec;
		private bool error = false;
		public System.Windows.Forms.TextBox assignment_Text;
		private System.Windows.Forms.Button done_button;
		private System.Windows.Forms.Label label1;
		private System.Windows.Forms.Label label2;
		private System.Drawing.Graphics labelGraphics;
		private Visual_Flow_Form the_form;

		private String error_msg;
		private System.Windows.Forms.RichTextBox textBox1;
		private String current_suggestion = "";
		private System.Windows.Forms.MainMenu mainMenu1;
		private System.Windows.Forms.MenuItem menuHelp;
		private System.Windows.Forms.MenuItem menuGeneralHelp;

		/// <summary>
		/// Required designer variable.
		/// </summary>
		private System.ComponentModel.Container components = null;

		public Call_Dialog(Rectangle parent_Rec, Visual_Flow_Form form)
		{
			Rec = parent_Rec;
			the_form = form;
			//
			// Required for Windows Form Designer support
			//
			InitializeComponent();
            Dialog_Helpers.Init();

			this.label1.Text = "Enter a procedure call." + 
				'\n' + '\n' + "Examples:" + '\n' + "   Wait_For_Mouse_Button(Left_Button)" +
				'\n' + "   Open_Graph_Window(300,300)";
			this.labelGraphics = label2.CreateGraphics();
			if (Rec.Text != null)
			{
				this.assignment_Text.Text = Rec.Text;
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
			System.Resources.ResourceManager resources = new System.Resources.ResourceManager(typeof(Call_Dialog));
			this.assignment_Text = new System.Windows.Forms.TextBox();
			this.done_button = new System.Windows.Forms.Button();
			this.label1 = new System.Windows.Forms.Label();
			this.label2 = new System.Windows.Forms.Label();
			this.textBox1 = new System.Windows.Forms.RichTextBox();
			this.mainMenu1 = new System.Windows.Forms.MainMenu();
			this.menuHelp = new System.Windows.Forms.MenuItem();
			this.menuGeneralHelp = new System.Windows.Forms.MenuItem();
			this.SuspendLayout();
			// 
			// assignment_Text
			// 
			this.assignment_Text.AcceptsReturn = true;
			this.assignment_Text.AcceptsTab = true;
			this.assignment_Text.Location = new System.Drawing.Point(48, 96);
			this.assignment_Text.Multiline = true;
			this.assignment_Text.Name = "assignment_Text";
			this.assignment_Text.ScrollBars = System.Windows.Forms.ScrollBars.Vertical;
			this.assignment_Text.Size = new System.Drawing.Size(200, 72);
			this.assignment_Text.TabIndex = 1;
			this.assignment_Text.Text = "";
			this.assignment_Text.KeyDown += new System.Windows.Forms.KeyEventHandler(this.Check_key);
			this.assignment_Text.KeyPress += new System.Windows.Forms.KeyPressEventHandler(this.assignment_Text_KeyPress);
			this.assignment_Text.TextChanged += new System.EventHandler(this.Check_Hint);
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
			this.label1.Location = new System.Drawing.Point(51, 16);
			this.label1.Name = "label1";
			this.label1.Size = new System.Drawing.Size(221, 80);
			this.label1.TabIndex = 3;
			// 
			// label2
			// 
			this.label2.Location = new System.Drawing.Point(16, 200);
			this.label2.Name = "label2";
			this.label2.Size = new System.Drawing.Size(264, 48);
			this.label2.TabIndex = 4;
			// 
			// textBox1
			// 
			this.textBox1.Location = new System.Drawing.Point(8, 264);
			this.textBox1.Name = "textBox1";
			this.textBox1.ReadOnly = true;
			this.textBox1.Size = new System.Drawing.Size(272, 96);
			this.textBox1.TabIndex = 5;
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
			// Call_Dialog
			// 
			this.AutoScaleBaseSize = new System.Drawing.Size(5, 13);
			this.ClientSize = new System.Drawing.Size(288, 401);
			this.Controls.Add(this.textBox1);
			this.Controls.Add(this.label2);
			this.Controls.Add(this.label1);
			this.Controls.Add(this.done_button);
			this.Controls.Add(this.assignment_Text);
			this.Icon = ((System.Drawing.Icon)(resources.GetObject("$this.Icon")));
			this.Menu = this.mainMenu1;
			this.Name = "Call_Dialog";
			this.Text = "Enter Call";
			this.TopMost = true;
			this.KeyDown += new System.Windows.Forms.KeyEventHandler(this.Check_key);
			this.Resize += new System.EventHandler(this.Call_Dialog_Resize);
			this.ResumeLayout(false);

		}
		#endregion

        private bool CreateNewTab(string s)
        {
            for (int i = 0; i < s.Length; i++)
            {
                if (!(Char.IsLetterOrDigit(s[i]) || s[i] == '_'))
                {
                    return false;
                }
            }
            if (!Char.IsLetter(s,0) ||!token_helpers_pkg.verify_id(s))
            {
                return false;
            }
            DialogResult dialog_result = MessageBox.Show("Do you wish to create a new tab named "
                + s, "Create new tab?", MessageBoxButtons.YesNo);
            if (dialog_result == DialogResult.Yes)
            {
                Subchart sc = new Subchart(the_form, s);
                Undo_Stack.Make_Add_Tab_Undoable(the_form, sc);
                the_form.Make_Undoable();
                the_form.carlisle.TabPages.Add(sc);
                result = interpreter_pkg.call_syntax(this.assignment_Text.Text,Rec);
                the_form.carlisle.SelectedTab = sc;
                Rec.Text = this.assignment_Text.Text;
                Rec.parse_tree = result.tree;
                this.error = false;
                Rec.changed();
                ((Visual_Flow_Form)the_form).flow_panel.Invalidate();
                this.Close();
                return true;
            }
            else
            {
                return false;
            }
        }

        private void done_button_Click(object sender, System.EventArgs e)
		{
            if (this.assignment_Text.Text == "")
            {
                // must have entered something
                return;
            }
			if (Rec.Text!=null && Rec.Text!="" && this.assignment_Text.Text.CompareTo(Rec.Text)==0)
			{
				// nothing changed
				this.Close();
				return;
			}
			result = interpreter_pkg.call_syntax(this.assignment_Text.Text,Rec);
            if (!result.valid && this.assignment_Text.Text.Length>=2)
            {
                if (CreateNewTab(this.assignment_Text.Text))
                {
                    return;
                }
            }

            if (result.valid)
			{
				the_form.Make_Undoable();
				Rec.Text = this.assignment_Text.Text;
				Rec.parse_tree = result.tree;
				this.error = false;
				Rec.changed();
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
				this.assignment_Text.Text,
				this.label1,
				this.error_msg,
				location,
				this.error);
			this.assignment_Text.Focus();
		}

		private bool Complete_Suggestion()
		{
			return Dialog_Helpers.Complete_Suggestion(
				this.assignment_Text,
                interpreter_pkg.call_dialog,
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
					e.Handled = true;
					done_button_Click(sender, e);
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
				this.assignment_Text,
				this.textBox1,
                interpreter_pkg.call_dialog,
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
			this.assignment_Text.Focus();
		}

		private void menuGeneralHelp_Click(object sender, System.EventArgs e)
		{
            if (!Component.BARTPE && !Component.VM)
            {
                Help.ShowHelp(this, Directory.GetParent(
                    Application.ExecutablePath) + "\\raptor.chm");
            }
            else{
                MessageBox.Show("Help not installed properly");
            }

		}

		private void Call_Dialog_Resize(object sender, System.EventArgs e)
		{
			label2.Width = this.Width-32;
			this.assignment_Text.Width = this.Width-96;
			this.label2.Invalidate();
			this.assignment_Text.Invalidate();
		}
	}
}
