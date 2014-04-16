using System;

namespace raptor
{
	/// <summary>
	/// Summary description for Buffered.
	/// </summary>
	public class Buffered : System.Windows.Forms.Panel
	{
		public Buffered()
			: base ()
		{
			this.SetStyle(System.Windows.Forms.ControlStyles.DoubleBuffer, true);
			this.SetStyle(System.Windows.Forms.ControlStyles.AllPaintingInWmPaint, true);
			this.SetStyle(System.Windows.Forms.ControlStyles.UserPaint, true);
		}
	}
}
