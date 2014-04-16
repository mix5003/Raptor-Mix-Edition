using System;
using System.Drawing;
using System.Collections;
using System.ComponentModel;
using System.Windows.Forms;

namespace raptor
{
	/// <summary>
	/// Summary description for Input_Output_Dlg.
	/// </summary>
	public class Return_Dlg : System.Windows.Forms.Form
	{
        private interpreter.suggestion_result suggestion_result;
        private string current_suggestion="";

		private interpreter.syntax_result result;
		private bool error = false;
        Oval_Return RETURN;
		private System.Windows.Forms.TextBox textBox1;
		private System.Windows.Forms.Label label3;
		private System.Windows.Forms.Button done_button;
		private System.Drawing.Graphics labelGraphics;
		private System.Drawing.StringFormat stringFormat;

		private String error_msg;
		private System.Windows.Forms.Label label4;
		/// <summary>
		/// Required designer variable.
		/// </summary>
        private System.ComponentModel.Container components = null;
		private System.Windows.Forms.Label label2;
        private RichTextBox suggestionTextBox;
        private Visual_Flow_Form the_form;
		public Return_Dlg(Oval_Return Parent_Oval, Visual_Flow_Form form)
		{
			RETURN = Parent_Oval;
			the_form = form;
			//
			// Required for Windows Form Designer support
			//
			InitializeComponent();
            Dialog_Helpers.Init();
            if ((RETURN.Text != null) && (RETURN.Text.CompareTo("") != 0))
            {
                this.textBox1.Text = RETURN.Text;
            }
			this.labelGraphics = label4.CreateGraphics();
			stringFormat = new System.Drawing.StringFormat();

			// Center the block of text (top to bottom) in the rectangle.
			stringFormat.LineAlignment = System.Drawing.StringAlignment.Center;
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
            this.textBox1 = new System.Windows.Forms.TextBox();
            this.label3 = new System.Windows.Forms.Label();
            this.done_button = new System.Windows.Forms.Button();
            this.label4 = new System.Windows.Forms.Label();
            this.label2 = new System.Windows.Forms.Label();
            this.suggestionTextBox = new System.Windows.Forms.RichTextBox();
            this.SuspendLayout();
            // 
            // textBox1
            // 
            this.textBox1.Location = new System.Drawing.Point(48, 102);
            this.textBox1.Multiline = true;
            this.textBox1.Name = "textBox1";
            this.textBox1.ScrollBars = System.Windows.Forms.ScrollBars.Vertical;
            this.textBox1.Size = new System.Drawing.Size(208, 72);
            this.textBox1.TabIndex = 4;
            this.textBox1.TextChanged += new System.EventHandler(this.textBox1_TextChanged);
            this.textBox1.KeyDown += new System.Windows.Forms.KeyEventHandler(this.Check_key);
            // 
            // label3
            // 
            this.label3.Location = new System.Drawing.Point(48, 6);
            this.label3.Name = "label3";
            this.label3.Size = new System.Drawing.Size(208, 16);
            this.label3.TabIndex = 12;
            this.label3.Text = "Enter variable to return here";
            this.label3.TextAlign = System.Drawing.ContentAlignment.MiddleCenter;
            // 
            // done_button
            // 
            this.done_button.Location = new System.Drawing.Point(120, 381);
            this.done_button.Name = "done_button";
            this.done_button.Size = new System.Drawing.Size(64, 24);
            this.done_button.TabIndex = 6;
            this.done_button.Text = "Done";
            this.done_button.Click += new System.EventHandler(this.done_button_Click);
            // 
            // label4
            // 
            this.label4.Location = new System.Drawing.Point(8, 190);
            this.label4.Name = "label4";
            this.label4.Size = new System.Drawing.Size(264, 48);
            this.label4.TabIndex = 8;
            // 
            // label2
            // 
            this.label2.Location = new System.Drawing.Point(40, 30);
            this.label2.Name = "label2";
            this.label2.Size = new System.Drawing.Size(216, 64);
            this.label2.TabIndex = 11;
            // 
            // suggestionTextBox
            // 
            this.suggestionTextBox.BackColor = System.Drawing.SystemColors.Control;
            this.suggestionTextBox.Location = new System.Drawing.Point(8, 243);
            this.suggestionTextBox.Name = "suggestionTextBox";
            this.suggestionTextBox.Size = new System.Drawing.Size(272, 96);
            this.suggestionTextBox.TabIndex = 13;
            this.suggestionTextBox.Text = "";
            // 
            // Return_Dlg
            // 
            this.AutoScaleBaseSize = new System.Drawing.Size(5, 13);
            this.ClientSize = new System.Drawing.Size(288, 422);
            this.Controls.Add(this.suggestionTextBox);
            this.Controls.Add(this.label2);
            this.Controls.Add(this.label4);
            this.Controls.Add(this.done_button);
            this.Controls.Add(this.label3);
            this.Controls.Add(this.textBox1);
            this.Name = "Return_Dlg";
            this.Text = "Enter Return";
            this.Resize += new System.EventHandler(this.Output_Dlg_Resize);
            this.ResumeLayout(false);
            this.PerformLayout();

		}
		#endregion

		private void radioButton2_CheckedChanged(object sender, System.EventArgs e)
		{	
			this.label2.Text = "Examples:" + '\n' + 
				"   Coins" + '\n' +
				"   " + '"' + "Number of Coins: " + '"' + "+Coins" + '\n' +  
				"   Board[3,3]";
            this.textBox1_TextChanged(sender, e);
		}

		private void radioButton3_CheckedChanged(object sender, System.EventArgs e)
		{
			this.label2.Text = "Examples:" + '\n' + 
				"   Welcome to tic-tac-toe" + '\n' +  
				"   The total is" + '\n' +
				"   The word is \"blue\"";
            this.textBox1_TextChanged(sender, e);
        }

		private void done_button_Click(object sender, System.EventArgs e)
		{
            if (this.textBox1.Text.Contains("("))
            {
                result = new interpreter.syntax_result();
                result.valid = false;
                result.location = this.textBox1.Text.IndexOf("(")+1;
                result.message = "can not call function in RETURN";
            }
            else
            {
                result = interpreter_pkg.output_syntax(this.textBox1.Text,
                    false, RETURN);
            }

			if (result.valid)
			{
				the_form.Make_Undoable();
    			RETURN.Text = this.textBox1.Text;

				RETURN.parse_tree = result.tree;

				RETURN.changed();
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
			this.labelGraphics = this.label4.CreateGraphics();
			Dialog_Helpers.Paint_Helper(this.labelGraphics,
				this.textBox1.Text,
				this.label2,
				this.error_msg,
				location,
				this.error);
			this.textBox1.Focus();
		}
        private bool Complete_Suggestion()
        {
            return Dialog_Helpers.Complete_Suggestion(
                this.textBox1,
                interpreter_pkg.expr_dialog,
                this.current_suggestion,
                ref this.suggestion_result);
        }
		private void Check_key(object sender, System.Windows.Forms.KeyEventArgs e)
		{
            if (e.KeyCode == Keys.Enter || e.KeyCode == Keys.Return)
            {
                e.Handled = this.Complete_Suggestion();

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
		private void Output_Dlg_Resize(object sender, System.EventArgs e)
		{
			label4.Width = this.Width-32;
			this.textBox1.Width = this.Width-88;
			this.label4.Invalidate();
			this.textBox1.Invalidate();
		}

        private void textBox1_TextChanged(object sender, System.EventArgs e)
        {
            if (textBox1.Lines.Length > 1)
            {
                textBox1.Text = textBox1.Lines[0] + textBox1.Lines[1];
                textBox1.Select(textBox1.Text.Length, 0);
            }

                Dialog_Helpers.Check_Hint(
                    this.textBox1,
                    this.suggestionTextBox,
                    interpreter_pkg.expr_dialog,
                    ref this.current_suggestion,
                    ref this.suggestion_result,
                    ref this.error,
                    this.Font);

            this.Invalidate();
        }





	}
}
