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
	public class Output_Dlg : System.Windows.Forms.Form
	{
        private interpreter.suggestion_result suggestion_result;
        private string current_suggestion="";

		private interpreter.syntax_result result;
		private bool error = false;
        Parallelogram PAR;
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
        private System.Windows.Forms.CheckBox new_line;
		private System.Windows.Forms.Label label2;
        private RichTextBox suggestionTextBox;
        private Visual_Flow_Form the_form;
		public Output_Dlg(Parallelogram Parent_Parallelogram, Visual_Flow_Form form)
		{
			PAR = Parent_Parallelogram;
			the_form = form;
			//
			// Required for Windows Form Designer support
			//
			InitializeComponent();
            Dialog_Helpers.Init();
            this.label2.Text = "Examples:" + '\n' +
                "   " + '"' + "exact text" + '"' + '\n'+
                "   Coins" + '\n' +
                "   " + '"' + "Number of Coins: " + '"' + "+Coins" + '\n' +
                "   Board[3,3]";
            this.textBox1.Text = PAR.Text;
			this.new_line.Checked = PAR.new_line;
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
            System.ComponentModel.ComponentResourceManager resources = new System.ComponentModel.ComponentResourceManager(typeof(Output_Dlg));
            this.textBox1 = new System.Windows.Forms.TextBox();
            this.label3 = new System.Windows.Forms.Label();
            this.done_button = new System.Windows.Forms.Button();
            this.label4 = new System.Windows.Forms.Label();
            this.new_line = new System.Windows.Forms.CheckBox();
            this.label2 = new System.Windows.Forms.Label();
            this.suggestionTextBox = new System.Windows.Forms.RichTextBox();
            this.SuspendLayout();
            // 
            // textBox1
            // 
            this.textBox1.Location = new System.Drawing.Point(48, 106);
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
            this.label3.Location = new System.Drawing.Point(48, 10);
            this.label3.Name = "label3";
            this.label3.Size = new System.Drawing.Size(208, 16);
            this.label3.TabIndex = 12;
            this.label3.Text = "Enter Output Here";
            this.label3.TextAlign = System.Drawing.ContentAlignment.MiddleCenter;
            // 
            // done_button
            // 
            this.done_button.Location = new System.Drawing.Point(120, 385);
            this.done_button.Name = "done_button";
            this.done_button.Size = new System.Drawing.Size(64, 24);
            this.done_button.TabIndex = 6;
            this.done_button.Text = "Done";
            this.done_button.Click += new System.EventHandler(this.done_button_Click);
            // 
            // label4
            // 
            this.label4.Location = new System.Drawing.Point(8, 194);
            this.label4.Name = "label4";
            this.label4.Size = new System.Drawing.Size(264, 48);
            this.label4.TabIndex = 8;
            // 
            // new_line
            // 
            this.new_line.Location = new System.Drawing.Point(100, 353);
            this.new_line.Name = "new_line";
            this.new_line.Size = new System.Drawing.Size(116, 24);
            this.new_line.TabIndex = 5;
            this.new_line.Text = "&End current line";
            // 
            // label2
            // 
            this.label2.Location = new System.Drawing.Point(40, 34);
            this.label2.Name = "label2";
            this.label2.Size = new System.Drawing.Size(216, 64);
            this.label2.TabIndex = 11;
            // 
            // suggestionTextBox
            // 
            this.suggestionTextBox.BackColor = System.Drawing.SystemColors.Control;
            this.suggestionTextBox.Location = new System.Drawing.Point(8, 247);
            this.suggestionTextBox.Name = "suggestionTextBox";
            this.suggestionTextBox.Size = new System.Drawing.Size(272, 96);
            this.suggestionTextBox.TabIndex = 13;
            this.suggestionTextBox.Text = "";
            // 
            // Output_Dlg
            // 
            this.AutoScaleBaseSize = new System.Drawing.Size(5, 13);
            this.ClientSize = new System.Drawing.Size(288, 435);
            this.Controls.Add(this.suggestionTextBox);
            this.Controls.Add(this.label2);
            this.Controls.Add(this.new_line);
            this.Controls.Add(this.label4);
            this.Controls.Add(this.done_button);
            this.Controls.Add(this.label3);
            this.Controls.Add(this.textBox1);
            this.Icon = ((System.Drawing.Icon)(resources.GetObject("$this.Icon")));
            this.Name = "Output_Dlg";
            this.Text = "Enter Output";
            this.Resize += new System.EventHandler(this.Output_Dlg_Resize);
            this.ResumeLayout(false);
            this.PerformLayout();

		}
		#endregion




		private void done_button_Click(object sender, System.EventArgs e)
		{
				result = interpreter_pkg.output_syntax(this.textBox1.Text,
					this.new_line.Checked,PAR);

				PAR.is_input = false;
			if (result.valid)
			{
				the_form.Make_Undoable();
					PAR.Text = this.textBox1.Text;
				PAR.parse_tree = result.tree;
				PAR.new_line = this.new_line.Checked;
				PAR.changed();
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
