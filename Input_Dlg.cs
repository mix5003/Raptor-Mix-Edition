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
	public class Input_Dlg : System.Windows.Forms.Form
	{
        private interpreter.suggestion_result suggestion_result_expr;
        private string current_suggestion_expr = "";
        private interpreter.suggestion_result suggestion_result_var;
        private string current_suggestion_var = "";
        
        private interpreter.syntax_result result;
		private interpreter.syntax_result prompt_result;
		private bool error = false;
		Parallelogram PAR;
		private System.Windows.Forms.Label examplesLabel;
		private System.Windows.Forms.TextBox variableTextBox;
		private System.Windows.Forms.Label label2;
		private System.Windows.Forms.TextBox exprTextBox;
		private System.Windows.Forms.Label label3;
		private System.Windows.Forms.Button done_button;
		private System.Drawing.Graphics labelGraphics;
		private System.Drawing.StringFormat stringFormat;

		private String error_msg;
		private System.Windows.Forms.Label errorLabel;
		/// <summary>
		/// Required designer variable.
		/// </summary>
        private System.ComponentModel.Container components = null;
        private RichTextBox suggestionTextBox;
        private Visual_Flow_Form the_form;
		public Input_Dlg(Parallelogram Parent_Parallelogram, Visual_Flow_Form form)
		{
			PAR = Parent_Parallelogram;
			the_form = form;
			//
			// Required for Windows Form Designer support
			//
			InitializeComponent();
            Dialog_Helpers.Init();

			if ((PAR.Text!=null) && (PAR.Text.CompareTo("")!=0))
			{

				this.exprTextBox.Text = PAR.prompt;
				this.variableTextBox.Text = PAR.Text;
			}

			this.examplesLabel.Text = "Examples:" + '\n' + "   Coins" + 
				'\n' + "   Board[3,3]";
			this.labelGraphics = errorLabel.CreateGraphics();
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
            System.ComponentModel.ComponentResourceManager resources = new System.ComponentModel.ComponentResourceManager(typeof(Input_Dlg));
            this.examplesLabel = new System.Windows.Forms.Label();
            this.variableTextBox = new System.Windows.Forms.TextBox();
            this.label2 = new System.Windows.Forms.Label();
            this.exprTextBox = new System.Windows.Forms.TextBox();
            this.label3 = new System.Windows.Forms.Label();
            this.done_button = new System.Windows.Forms.Button();
            this.errorLabel = new System.Windows.Forms.Label();
            this.suggestionTextBox = new System.Windows.Forms.RichTextBox();
            this.SuspendLayout();
            // 
            // examplesLabel
            // 
            this.examplesLabel.Location = new System.Drawing.Point(40, 145);
            this.examplesLabel.Name = "examplesLabel";
            this.examplesLabel.Size = new System.Drawing.Size(216, 40);
            this.examplesLabel.TabIndex = 0;
            // 
            // variableTextBox
            // 
            this.variableTextBox.Location = new System.Drawing.Point(48, 193);
            this.variableTextBox.Name = "variableTextBox";
            this.variableTextBox.Size = new System.Drawing.Size(208, 20);
            this.variableTextBox.TabIndex = 5;
            this.variableTextBox.WordWrap = false;
            this.variableTextBox.TextChanged += new System.EventHandler(this.variableTextBox_TextChanged);
            this.variableTextBox.KeyDown += new System.Windows.Forms.KeyEventHandler(this.Check_key);
            this.variableTextBox.Enter += new System.EventHandler(this.variableTextBox_TextChanged);
            // 
            // label2
            // 
            this.label2.Location = new System.Drawing.Point(48, 8);
            this.label2.Name = "label2";
            this.label2.Size = new System.Drawing.Size(208, 16);
            this.label2.TabIndex = 4;
            this.label2.Text = "Enter Prompt Here";
            this.label2.TextAlign = System.Drawing.ContentAlignment.MiddleCenter;
            // 
            // exprTextBox
            // 
            this.exprTextBox.Location = new System.Drawing.Point(48, 33);
            this.exprTextBox.Multiline = true;
            this.exprTextBox.Name = "exprTextBox";
            this.exprTextBox.ScrollBars = System.Windows.Forms.ScrollBars.Vertical;
            this.exprTextBox.Size = new System.Drawing.Size(208, 72);
            this.exprTextBox.TabIndex = 3;
            this.exprTextBox.TextChanged += new System.EventHandler(this.textBox2_TextChanged);
            this.exprTextBox.KeyDown += new System.Windows.Forms.KeyEventHandler(this.Check_key_expr);
            this.exprTextBox.Enter += new System.EventHandler(this.textBox2_TextChanged);
            // 
            // label3
            // 
            this.label3.Location = new System.Drawing.Point(48, 121);
            this.label3.Name = "label3";
            this.label3.Size = new System.Drawing.Size(208, 16);
            this.label3.TabIndex = 6;
            this.label3.Text = "Enter Variable Here";
            this.label3.TextAlign = System.Drawing.ContentAlignment.MiddleCenter;
            // 
            // done_button
            // 
            this.done_button.Location = new System.Drawing.Point(120, 415);
            this.done_button.Name = "done_button";
            this.done_button.Size = new System.Drawing.Size(64, 24);
            this.done_button.TabIndex = 7;
            this.done_button.Text = "Done";
            this.done_button.Click += new System.EventHandler(this.done_button_Click);
            // 
            // errorLabel
            // 
            this.errorLabel.BackColor = System.Drawing.SystemColors.Control;
            this.errorLabel.Location = new System.Drawing.Point(16, 233);
            this.errorLabel.Name = "errorLabel";
            this.errorLabel.Size = new System.Drawing.Size(264, 48);
            this.errorLabel.TabIndex = 8;
            // 
            // suggestionTextBox
            // 
            this.suggestionTextBox.BackColor = System.Drawing.SystemColors.Control;
            this.suggestionTextBox.Location = new System.Drawing.Point(8, 297);
            this.suggestionTextBox.Name = "suggestionTextBox";
            this.suggestionTextBox.Size = new System.Drawing.Size(272, 96);
            this.suggestionTextBox.TabIndex = 11;
            this.suggestionTextBox.Text = "";
            // 
            // Input_Dlg
            // 
            this.AutoScaleBaseSize = new System.Drawing.Size(5, 13);
            this.ClientSize = new System.Drawing.Size(299, 448);
            this.Controls.Add(this.suggestionTextBox);
            this.Controls.Add(this.errorLabel);
            this.Controls.Add(this.done_button);
            this.Controls.Add(this.label3);
            this.Controls.Add(this.exprTextBox);
            this.Controls.Add(this.variableTextBox);
            this.Controls.Add(this.label2);
            this.Controls.Add(this.examplesLabel);
            this.Icon = ((System.Drawing.Icon)(resources.GetObject("$this.Icon")));
            this.Name = "Input_Dlg";
            this.Text = "Enter Input";
            this.TopMost = true;
            this.Resize += new System.EventHandler(this.Input_Dlg_Resize);
            this.ResumeLayout(false);
            this.PerformLayout();

		}
		#endregion

		private void done_button_Click(object sender, System.EventArgs e)
		{
			result = interpreter_pkg.input_syntax(this.variableTextBox.Text,PAR);
			
   		    prompt_result = interpreter_pkg.output_syntax(this.exprTextBox.Text,false,PAR);
			
		    PAR.is_input = true;			    
				
			if (result.valid && prompt_result.valid)
			{
				the_form.Make_Undoable();
				PAR.prompt = this.exprTextBox.Text;
				PAR.Text = this.variableTextBox.Text;
				PAR.parse_tree = result.tree;
				PAR.prompt_tree = prompt_result.tree;
				PAR.input_is_expression = true;
				PAR.changed();
				this.error = false;
				this.Close();
			}

			else
			{
				this.error = true;
				if (!result.valid)
				{
					this.error_msg = result.message;
				}
				else
				{
					this.error_msg = prompt_result.message;
				}
				this.Invalidate();
			}
						
		}

		protected override void OnPaint(PaintEventArgs e)
		{
			int location;
			string error_text;

			Console.WriteLine(this.error);
			if (this.result!=null && !this.result.valid)
			{
				location = this.result.location;
				error_text = this.variableTextBox.Text;
			}
			else
			{
				if (this.prompt_result!=null && !this.prompt_result.valid)
				{
					location = this.prompt_result.location;
					error_text = this.exprTextBox.Text;
				}
				else
				{
					location = 0;
					error_text = "";
				}
			}
			this.labelGraphics = this.errorLabel.CreateGraphics();
			Dialog_Helpers.Paint_Helper(this.labelGraphics,
				error_text,
				this.examplesLabel,
				this.error_msg,
				location,
				this.error);
		}
        private bool Complete_Suggestion_Expr()
        {
            return Dialog_Helpers.Complete_Suggestion(
                this.exprTextBox,
                interpreter_pkg.expr_dialog,
                this.current_suggestion_expr,
                ref this.suggestion_result_expr);
        }
        private bool Complete_Suggestion_Var()
        {
            return Dialog_Helpers.Complete_Suggestion(
                this.variableTextBox,
                interpreter_pkg.lhs_dialog,
                this.current_suggestion_var,
                ref this.suggestion_result_var);
        }
		private void Check_key(object sender, System.Windows.Forms.KeyEventArgs e)
		{
            if (e.KeyCode == Keys.Enter || e.KeyCode == Keys.Return)
            {
                e.Handled = this.Complete_Suggestion_Var();
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
                    ref this.current_suggestion_var);
            }
            else if (e.KeyCode.ToString() == "Up")
            {
                e.Handled = true;
                e.SuppressKeyPress = e.Handled;
                Dialog_Helpers.suggestions_uparrow(
                    this.suggestionTextBox,
                    ref this.current_suggestion_var);
            }
        }
        private void Check_key_expr(object sender, KeyEventArgs e)
        {
            if (e.KeyCode == Keys.Enter || e.KeyCode == Keys.Return)
            {
                e.Handled = this.Complete_Suggestion_Expr();
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
                    ref this.current_suggestion_expr);
            }
            else if (e.KeyCode.ToString() == "Up")
            {
                e.Handled = true;
                e.SuppressKeyPress = e.Handled;
                Dialog_Helpers.suggestions_uparrow(
                    this.suggestionTextBox,
                    ref this.current_suggestion_expr);
            }
        }

		private void Input_Dlg_Resize(object sender, System.EventArgs e)
		{
			errorLabel.Width = this.Width-40;
			this.exprTextBox.Width = this.Width-96;
			this.variableTextBox.Width = this.Width-96;
			this.exprTextBox.Invalidate();
			this.variableTextBox.Invalidate();
			this.errorLabel.Invalidate();
		}

		private void textBox2_TextChanged(object sender, System.EventArgs e)
		{
			if (exprTextBox.Lines.Length > 1) 
			{
				exprTextBox.Text = exprTextBox.Lines[0]+exprTextBox.Lines[1];
				exprTextBox.Select(exprTextBox.Text.Length,0);
			}
                Dialog_Helpers.Check_Hint(
                    this.exprTextBox,
                    this.suggestionTextBox,
                    interpreter_pkg.expr_dialog,
                    ref this.current_suggestion_expr,
                    ref this.suggestion_result_expr,
                    ref this.error,
                    this.Font);
            this.Invalidate();
		}

        private void variableTextBox_TextChanged(object sender, EventArgs e)
        {
            Dialog_Helpers.Check_Hint(
                this.variableTextBox,
                this.suggestionTextBox,
                interpreter_pkg.lhs_dialog,
                ref this.current_suggestion_var,
                ref this.suggestion_result_var,
                ref this.error,
                this.Font);
            this.Invalidate();
        }




	}
}
