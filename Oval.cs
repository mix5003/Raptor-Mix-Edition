using System;
using System.Windows.Forms;
using System.Runtime.Serialization;
using System.Runtime.Serialization.Formatters.Binary;

namespace raptor
{
	/// <summary>
	/// Summary description for Oval.
	/// </summary>
	
	[Serializable]
	public class Oval : Component
	{
		public Oval(int height, int width, String str_name)
			: base(height, width, str_name)
		{
			this.init();
		}

		public Oval(Component Successor, int height, int width, String str_name)
			: base(Successor, height, width, str_name)
		{
			this.init();
		}
		public Oval(SerializationInfo info, StreamingContext ctxt)
			: base(info,ctxt)
		{
		}

		public override void draw(System.Drawing.Graphics gr, int x, int y)
		{
			bool draw_text;
            int box_width;

			X = x;
			Y = y;

			if ((this.scale <= .4) || (this.head_heightOrig < 10))
			{
				draw_text = false;
			}
			else
			{
				draw_text = Component.text_visible;
			}

			height_of_text = Convert.ToInt32((gr.MeasureString(
				"Yes", PensBrushes.default_times)).Height);

			width_of_text = Convert.ToInt32((gr.MeasureString(
				this.Text+"XX", PensBrushes.default_times)).Width);
            if (this.drawing_text_width > W && !Component.compiled_flowchart)
            {
                box_width = this.drawing_text_width;
            }
            else
            {
                box_width = W;
            }
			gr.SmoothingMode=System.Drawing.Drawing2D.SmoothingMode.HighQuality;
            System.Drawing.Pen pen_color;
            if (this.selected)
            {
                pen_color = PensBrushes.red_pen;
            }
            else if (this.running)
            {
                pen_color = PensBrushes.chartreuse_pen;
            }
            else
            {
                pen_color = PensBrushes.blue_pen;
            }


			gr.DrawEllipse(pen_color, X-box_width/2, Y, box_width, H);
			
			if (draw_text)
			{
                if (Component.full_text && ! Component.compiled_flowchart)
                {
                    // we get rect from footprint
                    if (drawing_text_width > W)
                    {
                        rect = new System.Drawing.Rectangle(x - drawing_text_width / 2, Y + (H * 1) / 32, drawing_text_width, this.height_of_text * 3);
                    }
                    else
                    {
                        rect = new System.Drawing.Rectangle(x - this.width_of_text / 2, Y + (H * 6) / 16, this.width_of_text, this.height_of_text);
                    }
                }
                else
                {
                    rect = new System.Drawing.Rectangle(x - W / 2, Y + (H * 6) / 16, W, this.height_of_text);
                }

				if (Component.compiled_flowchart)
				{
					gr.DrawString("compiled", PensBrushes.default_times, 
						PensBrushes.blackbrush, rect, 
						PensBrushes.centered_stringFormat);
				}
				else
				{
						gr.DrawString(this.getDrawText(), PensBrushes.default_times, 
							PensBrushes.blackbrush, rect, 
							PensBrushes.centered_stringFormat);
				}
			}

			if (!Component.compiled_flowchart && Successor != null)
			{
				System.Drawing.Pen pen;
				if (this.selected)
				{
					pen=PensBrushes.red_pen;
				}
				else
				{
					pen=PensBrushes.blue_pen;
				}
                drawConnector(gr, pen);
				
				this.Successor.scale = this.scale;
				this.Successor.Scale(this.scale);
				this.Successor.draw(gr,X,Y+H+CL);
			}

			if (draw_text) 
			{
				base.draw(gr,x,y);
			}
		}

        protected virtual void drawConnector(System.Drawing.Graphics gr, System.Drawing.Pen pen)
        {
            gr.DrawLine(pen, X, Y + H, X, Y + H + CL); // draw connector line to successor
            gr.DrawLine(pen, X, Y + H + CL, X - CL / 4, Y + H + CL - CL / 4); // draw left side of arrow
            gr.DrawLine(pen, X, Y + H + CL, X + CL / 4, Y + H + CL - CL / 4); // draw right side of arrow
        }

		// can't select the ovals along with anything else
		public override bool SelectRegion(System.Drawing.Rectangle rec)
		{
			this.selected=false;
			if (this.Successor!=null)
			{
				return this.Successor.SelectRegion(rec);
			}
			return false;
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

			// I only need to do this once, since these are static variables
			if (this.scale >= 2.75f)
			{
				PensBrushes.default_arial = PensBrushes.arial24;
				PensBrushes.default_times = PensBrushes.times30;
			}
			else if (this.scale >= 1.75f)
			{
				PensBrushes.default_arial = PensBrushes.arial16;
				PensBrushes.default_times = PensBrushes.times18;
			}
			else if (this.scale >= 1.5f)
			{
				PensBrushes.default_arial = PensBrushes.arial14;
				PensBrushes.default_times = PensBrushes.times16;
			}
			else if (this.scale >= 1.25f)
			{
				PensBrushes.default_arial = PensBrushes.arial12;
				PensBrushes.default_times = PensBrushes.times14;
			}
			else if (this.scale >= 1.0f)
			{
				PensBrushes.default_arial = PensBrushes.arial10;
				PensBrushes.default_times = PensBrushes.times12;
			}
			else if (this.scale >= 0.8f)
			{
				PensBrushes.default_arial = PensBrushes.arial8;
				PensBrushes.default_times = PensBrushes.times10;
			}
			else if (this.scale >= 0.6f)
			{
				PensBrushes.default_arial = PensBrushes.arial6;
				PensBrushes.default_times = PensBrushes.times8;
			}
			else if (this.scale >= 0.4f)
			{
				PensBrushes.default_arial = PensBrushes.arial4;
				PensBrushes.default_times = PensBrushes.times6;
			}
			base.Scale(new_scale);

		}





		public override bool editable_selected()
		{
			return false;
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

			return (im_ok && my_successor_ok);
		}

		// Mark error if I or my successor or chidren have empty parse trees.
		public override void mark_error()
		{
			if (this.Successor != null)
			{
				this.Successor.mark_error();
			}
		}

		// don't allow Start/End to be selected on a select all
		public override void selectAll()
		{
			this.selected = false;
			if (this.Successor != null)
			{
				this.Successor.selectAll();
			}
		}

	}
}
