using System;
using System.Drawing.Drawing2D;


namespace raptor
{
	/// <summary>
	/// Summary description for StopSign.
	/// </summary>
	public class StopSign
	{
		public StopSign()
		{
			//
			// TODO: Add constructor logic here
			//
		}

		public static GraphicsPath Make_Path(
			int x, int y, int size)
		{
			GraphicsPath result = new GraphicsPath();
			result.StartFigure();
			result.AddLine(x,y+size/3,x+size/3,y);
			result.AddLine(x+size/3,y,x+2*size/3,y);
			result.AddLine(x+2*size/3,y,x+size,y+size/3);
			result.AddLine(x+size,y+size/3,x+size,y+2*size/3);
			result.AddLine(x+size,y+2*size/3,x+2*size/3,y+size);
			result.AddLine(x+2*size/3,y+size,x+size/3,y+size);
			result.AddLine(x+size/3,y+size,x,y+2*size/3);
			result.AddLine(x,y+2*size/3,x,y+size/3);
			return result;
		}

		public static void Draw(System.Drawing.Graphics gr,
			int x, int y, int size)
		{
			GraphicsPath gp = Make_Path(x,y,size);
			gr.FillPath(PensBrushes.redbrush,gp);
			gr.DrawPath(PensBrushes.black_pen,gp);
		}
	}
}
