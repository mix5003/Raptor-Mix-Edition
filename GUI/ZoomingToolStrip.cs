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
using System.Drawing.Drawing2D;
using System.ComponentModel;
using System.Windows.Forms;
using System.Windows.Forms.Design;
using System.Windows.Forms.VisualStyles;
using NClass.GUI.Diagram;

namespace NClass.GUI
{
	[ToolStripItemDesignerAvailability(
		ToolStripItemDesignerAvailability.ToolStrip |
		ToolStripItemDesignerAvailability.StatusStrip)]
	[DefaultEvent("ZoomValueChanged")]
	[DefaultProperty("ZoomValue")]
	public class ZoomingToolStrip : ToolStripItem
	{
		const float MaxValue = DiagramControl.MaxZoom;
		const float MinValue = DiagramControl.MinZoom;
		const float DefaultValue = 1.0F;

		const int MinWidth = 100;
		const int PrecisionSize = 4;
		const int SliderWidth = 12;
		const int SliderHeight = 18;
		static readonly Pen linePen = SystemPens.ControlDarkDark;

		float value = DefaultValue;

		public event EventHandler ZoomValueChanged;

		public float ZoomValue
		{
			get
			{
				return value;
			}
			set
			{
				if (value > MaxValue) value = MaxValue;
				if (value < MinValue) value = MinValue;

				if (this.value != value) {
					this.value = value;
					OnZoomValueChanged(EventArgs.Empty);
				}
				Invalidate();
			}
		}

		protected override Size DefaultSize
		{
			get
			{
				return new Size(MinWidth, base.DefaultSize.Height);
			}
		}

		public override Size GetPreferredSize(Size constrainingSize)
		{
			return new Size(
				Math.Max(MinWidth, Size.Width), Size.Height);
		}

		private void MoveSlider(int location, bool snapToCenter)
		{
			int center = Width / 2;

			if (MonoHelper.IsRunningOnMono)
				location -= this.Bounds.Left;

			if (snapToCenter && Math.Abs(location - center) <= PrecisionSize)
				location = center;

			if (location < center) {
				int left = SliderWidth / 2;
				float scale = (DefaultValue - MinValue) / (center - left);
				ZoomValue = (location - left) * scale + MinValue;
			}
			else { // location >= center
				int right = Width - SliderWidth / 2;
				float scale = (MaxValue - DefaultValue) / (right - center);
				ZoomValue = (location - center) * scale + DefaultValue;
			}
		}

		protected virtual void OnZoomValueChanged(EventArgs e)
		{
			if (ZoomValueChanged != null)
				ZoomValueChanged(this, e);
		}

		protected override void OnMouseDown(MouseEventArgs e)
		{
			base.OnMouseDown(e);

			if (e.Button == MouseButtons.Left) {
				bool snapToCenter = (Control.ModifierKeys == Keys.None);
				MoveSlider(e.X, snapToCenter);
			}
		}

		protected override void OnMouseMove(MouseEventArgs e)
		{
			base.OnMouseDown(e);

			if (e.Button == MouseButtons.Left) {
				bool snapToCenter = (Control.ModifierKeys == Keys.None);					
				MoveSlider(e.X, snapToCenter);
			}
		}

		protected override void OnPaint(PaintEventArgs e)
		{
			base.OnPaint(e);
			Graphics g = e.Graphics;

			Point center = new Point(Width / 2, Height / 2);
			int left = SliderWidth / 2;
			int right = Width - SliderWidth / 2;
			int top = center.Y - 3;
			int bottom = center.Y + 3;

			g.DrawLine(linePen, left, center.Y, right, center.Y); // Draw horizotal line
			g.DrawLine(linePen, center.X, top, center.X, bottom); // Draw white line

			DrawSlider(g, TrackBarThumbState.Normal);
		}

		private void DrawSlider(Graphics g, TrackBarThumbState state)
		{
			int sliderLocation;

			if (ZoomValue < DefaultValue) {
				int regionWidth = Width / 2 - SliderWidth / 2;
				float scale = (float) regionWidth / (DefaultValue - MinValue);
				sliderLocation = (int) Math.Round(scale * (ZoomValue - MinValue)) + SliderWidth / 2;
			}
			else {
				int regionWidth = Width / 2 - SliderWidth / 2;
				float scale = (float) regionWidth / (MaxValue - DefaultValue);
				sliderLocation = (int) Math.Round(scale * (ZoomValue - DefaultValue)) + Width / 2;
			}

			int top = Height / 2 - SliderHeight / 2;
			int left = sliderLocation - SliderWidth / 2;
			Rectangle bounds = new Rectangle(left, top, SliderWidth, SliderHeight);

			if (TrackBarRenderer.IsSupported && !MonoHelper.IsRunningOnMono) {
				TrackBarRenderer.DrawBottomPointingThumb(g, bounds, state);
			}
			else {
				bounds.X += 1;
				bounds.Width -= 3;
				g.FillRectangle(SystemBrushes.ControlLightLight, bounds);
				g.DrawRectangle(linePen, bounds);
			}
		}
	}
}
