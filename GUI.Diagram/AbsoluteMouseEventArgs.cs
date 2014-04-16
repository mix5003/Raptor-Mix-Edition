// NClass - Free class diagram editor
// Copyright (C) 2006-2007 Balazs Tihanyi
// 
// This program is free software; you can redistribute it and/or modify it under 
// the terms of the GNU General Public License as published by the Free Software 
// Foundation; either version 3 of the License, or (at your option) any later version.
// 
// This program is distributed in the hope that it will be useful, but WITHOUT 
// ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS 
// FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License along with 
// this program; if not, write to the Free Software Foundation, Inc., 
// 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA

using System;
using System.Drawing;
using System.Windows.Forms;

namespace NClass.GUI.Diagram
{
	public delegate void AbsoluteMouseEventHandler(object sender, AbsoluteMouseEventArgs e);

	public class AbsoluteMouseEventArgs
	{
		float x;
		float y;
		MouseButtons button;

		public AbsoluteMouseEventArgs(MouseButtons button, float x, float y)
		{
			this.button = button;
			this.x = x;
			this.y = y;
		}

		public AbsoluteMouseEventArgs(MouseButtons button, PointF location)
		{
			this.button = button;
			this.x = location.X;
			this.y = location.Y;
		}

		public AbsoluteMouseEventArgs(MouseEventArgs e, SizeF offset, float zoom)
		{
			if (zoom == 0)
				zoom = 1;

			PointF zoomedRelative = new PointF(e.X / zoom, e.Y / zoom);
			PointF absolutePosition = (zoomedRelative + offset);

			this.button = e.Button;
			this.x = absolutePosition.X;
			this.y = absolutePosition.Y;
		}

		public MouseButtons Button
		{
			get { return button; }
		}

		public float X
		{
			get { return x; }
		}

		public float Y
		{
			get { return y; }
		}

		public PointF Location
		{
			get { return new PointF(x, y); }
		}
	}
}
