using System;
using System.Windows.Forms;
using System.Runtime.Serialization;
using System.Runtime.Serialization.Formatters.Binary;
using System.Drawing;

namespace raptor
{
	/// <summary>
	/// Summary description for Parallelogram.
	/// </summary>
	
	[Serializable]
	public class Parallelogram : Component
	{
		public string prompt = "";
		public bool is_input;
		public bool new_line=true;
		public bool input_is_expression=false;
		public parse_tree.parseable prompt_tree;
		private interpreter.syntax_result prompt_result;

		public Parallelogram(int height, int width, String str_name, bool input)
			: base(height, width, str_name)
		{
			this.init();
			this.is_input = input;
		}

		public Parallelogram(Component Successor, int height, int width, String str_name, bool input)
			: base(Successor, height, width, str_name)
		{
			this.is_input = input;
			this.init();
		}

		public Parallelogram(SerializationInfo info, StreamingContext ctxt)
			: base(info,ctxt)
		{
			prompt = (string)info.GetValue("_prompt", typeof(string));
			is_input = (bool)info.GetValue("_is_input", typeof(bool));
			// is prompt an expression or just plain text?
			if (this.incoming_serialization_version >= 9 && this.is_input)
			{
				this.input_is_expression = info.GetBoolean("_input_expression");
			}
			else
			{
				this.input_is_expression = false;
			}

			if (this.incoming_serialization_version >= 5)
			{
				this.new_line = info.GetBoolean("_new_line");
			}
			else
			{
				this.new_line = true;
			}
			if (is_input)
			{
				result = interpreter_pkg.input_syntax(this.Text,this);
				if (this.input_is_expression)
				{
					prompt_result = interpreter_pkg.output_syntax(this.prompt,false,this);
				}
			}
			else
			{
				result = interpreter_pkg.output_syntax(this.Text,this.new_line,this);
			}

			if (this.input_is_expression)
			{
				if (prompt_result.valid)
				{
					this.prompt_tree = prompt_result.tree;
				}
				else
				{
					this.prompt = "";
				}
			}

			if (result.valid)
			{
				this.parse_tree = result.tree;
			}
			else
			{
				if (!Component.warned_about_error && this.Text!="")
				{
					MessageBox.Show("Unknown error: \n" +
						this.Text + "\n" +
						"is not recognized.");
					Component.warned_about_error = true;
				}
				this.Text = "";
			}
		}
				
		
		//Serialization function.
		public override void GetObjectData(SerializationInfo info, StreamingContext ctxt)
		{
			//You can use any custom name for your name-value pair. But make sure you
			// read the values with the same name. For ex:- If you write EmpId as "EmployeeId"
			info.AddValue("_prompt", prompt);
			info.AddValue("_is_input", is_input);
			info.AddValue("_new_line", new_line);
			info.AddValue("_input_expression", input_is_expression);
			base.GetObjectData(info,ctxt);
		}


		public override void draw(System.Drawing.Graphics gr, int x, int y)
		{
			bool draw_text;
			int box_width;

			if ((this.scale <= .4) || (this.head_heightOrig < 10))
			{
				draw_text = false;
			}
			else
			{
				draw_text = Component.text_visible;
			}
			if (draw_text) 
			{
				base.draw(gr,x,y);
			}

			int delta = W/8;
			X = x;
			Y = y;

			height_of_text = Convert.ToInt32((gr.MeasureString(
				"Yes", PensBrushes.default_times)).Height);

			width_of_text = Convert.ToInt32((gr.MeasureString(
				this.getDrawText()+" X", PensBrushes.default_times)).Width);

			gr.SmoothingMode=System.Drawing.Drawing2D.SmoothingMode.HighQuality;

			System.Drawing.Pen pen;
			if (this.selected)
			{
				pen = PensBrushes.red_pen;
			}
			else if (this.running)
			{
				pen = PensBrushes.chartreuse_pen;
			}
			else
			{
				pen = PensBrushes.blue_pen;
			}
			if (this.drawing_text_width > W)
			{
				box_width = this.drawing_text_width+3*delta/2;
			}
			else
			{
				box_width = W;
			}
			gr.DrawLine(pen,x-box_width/2+delta,y,
				x+box_width/2,y); // draw top line
			gr.DrawLine(pen,x+box_width/2,y,
				x+box_width/2-delta,y+H); // draw right line
			gr.DrawLine(pen,x-box_width/2,y+H,
				x+box_width/2-delta,y+H); // draw bottom line
			gr.DrawLine(pen,x-box_width/2,y+H,
				x-box_width/2+delta,y); // draw bottom line
			if (this.has_breakpoint)
			{
				StopSign.Draw(gr,x-box_width/2-W/6-2,y, W/6);
			}

			if (this.is_input)
			{
				gr.DrawLine(pen,x-box_width/2+3*W/32-W/4,y+H/4,
					x-box_width/2+3*W/32,y+H/4); // input line
				gr.DrawLine(pen,x-box_width/2+3*W/32,y+H/4,
					x-box_width/2+3*W/32-W/8,y+H/4-H/8); // up arrow
				gr.DrawLine(pen,x-box_width/2+3*W/32,y+H/4,
					x-box_width/2+3*W/32-W/8,y+H/4+H/8); // down arrow
/*				gr.DrawLine(pen,x-box_width/2-W/32,y+H/4,
					x-box_width/2-W/32+W/3,y+H/4); // input line
				gr.DrawLine(pen,x-box_width/2-W/32+W/3,y+H/4,
					x-box_width/2-W/32+W/3-W/8,y+H/4-H/8); // up arrow
				gr.DrawLine(pen,x-box_width/2-W/32+W/3,y+H/4,
					x-box_width/2-W/32+W/3-W/8,y+H/4+H/8); // down arrow
					*/
			}
			else
			{
				gr.DrawLine(pen,x+box_width/2-3*W/32,y+H-H/4,
					x+box_width/2-3*W/32+W/4,y+H-H/4); // output line
				gr.DrawLine(pen,x+box_width/2-3*W/32+W/4,y+H-H/4,
					x+box_width/2-3*W/32+W/4-W/8,y+H-H/4-H/8); // up arrow
				gr.DrawLine(pen,x+box_width/2-3*W/32+W/4,y+H-H/4,
					x+box_width/2-3*W/32+W/4-W/8,y+H-H/4+H/8); // down arrow
/*				gr.DrawLine(pen,x+box_width/2-3*W/16,y+H-H/4,
					x+box_width/2-3*W/16+W/3,y+H-H/4); // output line
				gr.DrawLine(pen,x+box_width/2-3*W/16+W/3,y+H-H/4,
					x+box_width/2-3*W/16+W/3-W/8,y+H-H/4-H/8); // up arrow
				gr.DrawLine(pen,x+box_width/2-3*W/16+W/3,y+H-H/4,
					x+box_width/2-3*W/16+W/3-W/8,y+H-H/4+H/8); // down arrow
					*/
			}


			if(draw_text)
			{
				if (Component.full_text)
				{
					// we get rect from footprint
					if (drawing_text_width>0)
					{
						rect = new System.Drawing.Rectangle(x-drawing_text_width/2, Y+(H*1)/32,
							drawing_text_width,this.height_of_text*3);
					}
					else
					{
						rect = new System.Drawing.Rectangle(x-this.width_of_text/2, Y+(H*6)/16,
							this.width_of_text,this.height_of_text);
					}
				}
				else
				{
					rect = new System.Drawing.Rectangle(x-(W*7)/16, Y+(H*6)/16, W-W/8,this.height_of_text);
				}

				if (this.Text == "Error")
				{
					gr.DrawString(this.Text, PensBrushes.default_times, PensBrushes.redbrush, rect, PensBrushes.centered_stringFormat);
				}
				else
				{
					gr.DrawString(this.getDrawText(), 
						PensBrushes.default_times, 
						PensBrushes.blackbrush, rect, 
						PensBrushes.centered_stringFormat);
				}
			}

			if (Successor != null)
			{
				if (this.selected)
				{
					pen=PensBrushes.red_pen;
				}
				else
				{
					pen=PensBrushes.blue_pen;
				}
				gr.DrawLine(pen,x,y+H,x,y+H+CL); // draw connector line to successor
				gr.DrawLine(pen,x,y+H+CL,x-CL/4,y+H+CL - CL/4); // draw left side of arrow
				gr.DrawLine(pen,x,y+H+CL,x+CL/4,y+H+CL - CL/4); // draw right side of arrow

				Successor.scale = this.scale;
				Successor.draw(gr,x,y+H+CL);
			}
		}

		public override void wide_footprint(System.Drawing.Graphics gr)
		{
			int height_of_text, width_of_text=2*W;
			SizeF sz;

			height_of_text = Convert.ToInt32((gr.MeasureString(
				"Yes", PensBrushes.default_times)).Height);

			// loop starting at 2*W until you get on 3 lines.
			while (true) 
			{
				sz = gr.MeasureString(
					this.getDrawText()+"XX", PensBrushes.default_times, 
					width_of_text);
				if (sz.Height<height_of_text*7/2)
				{
					break;
				}
				width_of_text = width_of_text + W/2;
			}

			if (sz.Height > height_of_text*3/2) 
			{
				FP.left = (width_of_text-W)/2+W/2;
				FP.right = (width_of_text-W)/2+W/2;
				drawing_text_width = width_of_text;
			}
			else if ((int) sz.Width > W)
			{
				width_of_text = W;
				while (width_of_text < (int) sz.Width)
				{
					width_of_text += W/2;
				}
				FP.left = (width_of_text-W)/2+W/2;
				FP.right = (width_of_text-W)/2+W/2;
				drawing_text_width = width_of_text;
			}
			else
			{
				drawing_text_width = 0;
			}
		}

		//Scale the object
		public override void Scale(float new_scale)
		{

			H = (int) Math.Round(this.scale*this.head_heightOrig);
			W = (int) Math.Round(this.scale*this.head_widthOrig);
			

			if (this.Successor != null)
			{
				this.Successor.scale = this.scale;
				this.Successor.Scale(new_scale);
			}
			base.Scale(new_scale);

		}




		
		// Get the text from a pop-up dialog and then set it?
		public override string getText(int x, int y)
		{
			if (contains(x,y))
			{
				return this.Text;
			}
			else
			{
				if (this.Successor != null)
				{
					return this.Successor.getText(x,y);
				}
				else
				{
					return "";
				}
			}
		}

		// Get the text from a pop-up dialog and then set it?
		public override bool setText(int x, int y, Visual_Flow_Form form)
		{
			bool textset = false;
			if (contains(x,y))
			{
				if (this.is_input)
				{
					Input_Dlg IOD = new Input_Dlg(this,form);
					IOD.ShowDialog();
				}
				else
				{
					Output_Dlg IOD = new Output_Dlg(this,form);
					IOD.ShowDialog();
				}
				textset = true;
				return textset;
			}
	
			if (this.Successor != null)
			{
				return(this.Successor.setText(x,y,form));
			}
			
			return textset;
		}



		
		// Check to see if I or my successor or chidren have empty parse trees.
		public override bool has_code()
		{
			bool im_ok = true;
			bool my_successor_ok = true;

			if (this.Successor != null)
			{
				my_successor_ok = this.Successor.has_code();
			}
			if (this.parse_tree == null)
			{
				im_ok = false;
			}

			return (im_ok && my_successor_ok);
		}

		// Mark error if I or my successor or chidren have empty parse trees.
		public override void mark_error()
		{
			if (this.Successor != null)
			{
				this.Successor.mark_error();
			}
			if (this.parse_tree == null)
			{
				this.Text = "Error";
				Runtime.parent.Show_Text_On_Error();
			}
		}

		public override string getDrawText()
		{
			string result;
			string get_string;
			string put_string;

			if (Component.USMA_mode) 
			{
				get_string = "INPUT " + this.Text;
				put_string = "OUTPUT " + this.Text;
			}
			else
			{
				get_string = "GET " + this.Text;
				put_string = "PUT " + this.Text;
			}

			if (this.is_input && this.Text != "")
			{
				if (Component.full_text)
				{
					if (!this.input_is_expression)
					{
						result = '"' + this.prompt + '"' + '\n' +
							get_string;
					}
					else
					{
						result = this.prompt + '\n' +
							get_string;
					}
				}
				else
				{
					result = get_string;
				}
			}
			else if (!this.is_input && this.Text != "")
			{
				if (!this.new_line)
				{
					result = put_string;
				}
				else
				{
					result = put_string + '\u00B6';
				}
			}
			else
			{
				result = "";
			}
			if (this.is_input || (this.Text!="" && this.Text[0]=='"'))
			{
				return result;
			}
			else
			{
				return Component.unbreakString(result);
			}
		}
		public override void compile_pass1(generate_interface.typ gen)
		{
			if (this.parse_tree!=null & this.is_input && this.input_is_expression)
			{
                // maintain with parse_tree.adb
                if (!Compile_Helpers.Start_New_Declaration("raptor_prompt_variable_zzyz"))
                {
                    gen.Declare_String_Variable("raptor_prompt_variable_zzyz");
                }
			}
			base.compile_pass1(gen);
		}

		public override void Emit_Code(generate_interface.typ gen)
		{	
			if (this.parse_tree!=null)
			{
				if (this.is_input)
				{
					if (!this.input_is_expression)
					{
						parse_tree_pkg.set_prompt(this.prompt);
					}
					else
					{
						parse_tree_pkg.set_prompt(null);
                        // maintain with parse_tree.adb
                        gen.Variable_Assignment_Start("raptor_prompt_variable_zzyz");
                        interpreter_pkg.emit_code(
							((parse_tree.expr_output) this.prompt_tree).expr,
							this.prompt,gen);
                        gen.Variable_Assignment_PastRHS();
					}
				}
				interpreter_pkg.emit_code(this.parse_tree,this.Text,gen);
			}
			if (this.Successor!=null)
			{
				this.Successor.Emit_Code(gen);
			}
		}
        public override void collect_variable_names(System.Collections.Generic.IList<string> l,
            System.Collections.Generic.IDictionary<string, string> types)
        {
            if (this.is_input && this.parse_tree != null)
            {
                string name = interpreter_pkg.get_name_input((parse_tree.input)this.parse_tree, this.Text);
                l.Add(name);
            }
            if (this.Successor != null)
            {
                this.Successor.collect_variable_names(l,types);
            }
        }

	}
}
