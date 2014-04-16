using System;
using System.Drawing;

namespace raptor
{
	/// <summary>
	/// Summary description for PensBrushes.
	/// </summary>
	/// 
	[Serializable]
	public class PensBrushes
	{
		public enum family {times, arial, courier};

		public static System.Drawing.Pen red_pen;
		public static System.Drawing.Pen black_pen;
		public static System.Drawing.Pen black_dash_pen;
		public static System.Drawing.Pen blue_pen;
		public static System.Drawing.Pen blue_dash_pen;
		public static System.Drawing.Pen green_pen;
		public static System.Drawing.Pen chartreuse_pen;

		public static System.Drawing.FontStyle Boldstyle;
		public static System.Drawing.FontStyle Regstyle;
		
		public static System.Drawing.FontFamily times;
		public static System.Drawing.FontFamily arial;
		public static System.Drawing.FontFamily courier;
		
		public static System.Drawing.Font times36;
		public static System.Drawing.Font times30;
		public static System.Drawing.Font times28;
		public static System.Drawing.Font times24;
		public static System.Drawing.Font times20;
		public static System.Drawing.Font times18;
		public static System.Drawing.Font times16;
		public static System.Drawing.Font times14;
		public static System.Drawing.Font times12;
		public static System.Drawing.Font times10;
		public static System.Drawing.Font times8;
		public static System.Drawing.Font times6;
		public static System.Drawing.Font times4;

		public static System.Drawing.Font courier36;
		public static System.Drawing.Font courier30;
		public static System.Drawing.Font courier28;
		public static System.Drawing.Font courier24;
		public static System.Drawing.Font courier20;
		public static System.Drawing.Font courier18;
		public static System.Drawing.Font courier16;
		public static System.Drawing.Font courier14;
		public static System.Drawing.Font courier12;
		public static System.Drawing.Font courier10;
		public static System.Drawing.Font courier8;
		public static System.Drawing.Font courier6;
		public static System.Drawing.Font courier4;

		public static System.Drawing.Font default_times;
		public static System.Drawing.Font default_courier;
		public static System.Drawing.Font default_arial;

		public static System.Drawing.Font arial36;
		public static System.Drawing.Font arial30;
		public static System.Drawing.Font arial28;
		public static System.Drawing.Font arial24;
		public static System.Drawing.Font arial20;
		public static System.Drawing.Font arial18;
		public static System.Drawing.Font arial16;
		public static System.Drawing.Font arial14;
		public static System.Drawing.Font arial12;
		public static System.Drawing.Font arial10;
		public static System.Drawing.Font arial8;
		public static System.Drawing.Font arial6;
		public static System.Drawing.Font arial4;
		
		public static System.Drawing.SolidBrush blackbrush;
		public static System.Drawing.SolidBrush redbrush;
		public static System.Drawing.SolidBrush greenbrush;
		
		public static System.Drawing.StringFormat centered_stringFormat;
		public static System.Drawing.StringFormat left_stringFormat;
		
		public PensBrushes()
		{
		}

		public static System.Drawing.Font Get_Font(family f, int size) 
		{
			switch (f) 
			{
				case family.arial:
					switch (size) 
					{
						case 4:
							return arial4;
						case 6:
							return arial6;
						case 8:
							return arial8;
						case 10:
							return arial10;
						case 12:
							return arial12;
						case 14:
							return arial14;
						case 16:
							return arial16;
						case 18:
							return arial18;
						case 20:
							return arial20;
						case 24:
							return arial24;
						case 28:
							return arial28;
						case 30:
							return arial30;
						case 36:
							return arial36;
						default:
							throw new System.Exception("no size:" + size + " in family: " + f);
					}
				case family.times:
					switch (size) 
					{
						case 4:
							return times4;
						case 6:
							return times6;
						case 8:
							return times8;
						case 10:
							return times10;
						case 12:
							return times12;
						case 14:
							return times14;
						case 16:
							return times16;
						case 18:
							return times18;
						case 20:
							return times20;
						case 24:
							return times24;
						case 28:
							return times28;
						case 30:
							return times30;
						case 36:
							return times36;
						default:
							throw new System.Exception("no size:" + size + " in family: " + f);
					}	
				case family.courier:
					switch (size) 
					{
						case 4:
							return courier4;
						case 6:
							return courier6;
						case 8:
							return courier8;
						case 10:
							return courier10;
						case 12:
							return courier12;
						case 14:
							return courier14;
						case 16:
							return courier16;
						case 18:
							return courier18;
						case 20:
							return courier20;
						case 24:
							return courier24;
						case 28:
							return courier28;
						case 30:
							return courier30;
						case 36:
							return courier36;
						default:
							throw new System.Exception("no size:" + size + " in family: " + f);
					}
				default:
					throw new System.Exception("no such family");
			}
		}

		public static void initialize()
		{
		 red_pen = new System.Drawing.Pen(Color.Red);
		 black_pen = new System.Drawing.Pen(Color.Black);
		 black_dash_pen = new System.Drawing.Pen(Color.Black);
		 blue_pen = new System.Drawing.Pen(Color.Blue);
		 blue_dash_pen = new System.Drawing.Pen(Color.Blue);
		 green_pen = new System.Drawing.Pen(Color.Green);
		 chartreuse_pen = new System.Drawing.Pen(Color.Chartreuse,4.0f);

		 Boldstyle = System.Drawing.FontStyle.Bold;
		 Regstyle = System.Drawing.FontStyle.Regular;

         try
         {
             times = new System.Drawing.FontFamily("Times New Roman");
             if (times == null) throw new System.Exception("times not found");
         }
         catch
         {
             times = System.Drawing.FontFamily.GenericSerif;
         }
         try
         {
             arial = new System.Drawing.FontFamily("Arial");
             if (arial == null) throw new System.Exception("arial not found");
         }
         catch { 
             arial = System.Drawing.FontFamily.GenericSansSerif;
         }
         try
         {
             courier = new System.Drawing.FontFamily("Courier New");
             if (courier == null) throw new System.Exception("courier not found");
         }
         catch
         {
             courier = System.Drawing.FontFamily.GenericMonospace;
         } 

		 times36 = new System.Drawing.Font(times,36,Regstyle);
		 times30 = new System.Drawing.Font(times,30,Regstyle);
		 times28 = new System.Drawing.Font(times,28,Regstyle);
		 times24 = new System.Drawing.Font(times,24,Regstyle);
		 times20 = new System.Drawing.Font(times,20,Regstyle);
		 times18 = new System.Drawing.Font(times,18,Regstyle);
		 times16 = new System.Drawing.Font(times,16,Regstyle);
		 times14 = new System.Drawing.Font(times,14,Regstyle);
		 times12 = new System.Drawing.Font(times,12,Regstyle);
		 times10 = new System.Drawing.Font(times,10,Regstyle);
		 times8 = new System.Drawing.Font(times,8,Regstyle);
		 times6 = new System.Drawing.Font(times,6,Regstyle);
		 times4 = new System.Drawing.Font(times,4,Regstyle);

		 courier36 = new System.Drawing.Font(courier,36,Regstyle);
		 courier30 = new System.Drawing.Font(courier,30,Regstyle);
		 courier28 = new System.Drawing.Font(courier,28,Regstyle);
		 courier24 = new System.Drawing.Font(courier,24,Regstyle);
		 courier20 = new System.Drawing.Font(courier,20,Regstyle);
		 courier18 = new System.Drawing.Font(courier,18,Regstyle);
		 courier16 = new System.Drawing.Font(courier,16,Regstyle);
		 courier14 = new System.Drawing.Font(courier,14,Regstyle);
		 courier12 = new System.Drawing.Font(courier,12,Regstyle);
		 courier10 = new System.Drawing.Font(courier,10,Regstyle);
		 courier8 = new System.Drawing.Font(courier,8,Regstyle);
		 courier6 = new System.Drawing.Font(courier,6,Regstyle);
		 courier4 = new System.Drawing.Font(courier,4,Regstyle);

		 default_times = times10;
		 default_courier = courier10;
		 default_arial = arial8;

		 arial36 = new System.Drawing.Font(arial,36,Regstyle);
		 arial30 = new System.Drawing.Font(arial,30,Regstyle);
		 arial28 = new System.Drawing.Font(arial,28,Regstyle);
		 arial24 = new System.Drawing.Font(arial,24,Regstyle);
		 arial20 = new System.Drawing.Font(arial,20,Regstyle);
		 arial18 = new System.Drawing.Font(arial,18,Regstyle);
		 arial16 = new System.Drawing.Font(arial,16,Regstyle);
		 arial14 = new System.Drawing.Font(arial,14,Regstyle);
		 arial12 = new System.Drawing.Font(arial,12,Regstyle);
		 arial10 = new System.Drawing.Font(arial,10,Regstyle);
		 arial8 = new System.Drawing.Font(arial,8,Regstyle);
		 arial6 = new System.Drawing.Font(arial,6,Regstyle);
		 arial4 = new System.Drawing.Font(arial,4,Regstyle);
		
		 blackbrush = new System.Drawing.SolidBrush(System.Drawing.Color.Black);
		 redbrush = new System.Drawing.SolidBrush(System.Drawing.Color.Red);
		 greenbrush = new System.Drawing.SolidBrush(System.Drawing.Color.Green);
		
		 centered_stringFormat = new System.Drawing.StringFormat();
		 left_stringFormat = new System.Drawing.StringFormat();
			// Center each line of text.
			centered_stringFormat.Alignment = System.Drawing.StringAlignment.Center;

			// Center the block of text (top to bottom) in the rectangle.
			centered_stringFormat.LineAlignment = System.Drawing.StringAlignment.Center;

			// Left justify each line of text.
			left_stringFormat.Alignment = System.Drawing.StringAlignment.Near;

			// Center the block of text (top to bottom) in the rectangle.
			left_stringFormat.LineAlignment = System.Drawing.StringAlignment.Center;
			black_dash_pen.DashStyle = System.Drawing.Drawing2D.DashStyle.DashDot;
			blue_dash_pen.DashStyle = System.Drawing.Drawing2D.DashStyle.DashDotDot;

			float[] x = new float[6];
			x[0] = 1;
			x[1] = 3;
			x[2] = 1;
			x[3] = 3;
			x[4] = 1;
			x[5] = 3;
			blue_dash_pen.DashPattern = x;
		}

	}
}
