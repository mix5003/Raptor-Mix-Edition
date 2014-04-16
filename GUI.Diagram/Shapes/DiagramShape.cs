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
using System.Xml;
using System.Drawing;
using System.Drawing.Drawing2D;
using System.Windows.Forms;
using NClass.Core;
using System.Collections.Generic;

namespace NClass.GUI.Diagram
{
    [Serializable]
	public abstract class DiagramShape : DiagramElement, IClipboardItem
	{
		protected enum ResizeMode
		{
			None = 0,
			Right = 1,
			Bottom = 2
		}

		public const int SelectionMargin = 12;
		static readonly Pen selectionSquarePen = new Pen(Color.Black);
		protected static readonly float[] borderDashPattern = new float[] { 3, 3 };
		protected static readonly SolidBrush shadowBrush = new SolidBrush(Color.Gray);
		protected static readonly Size defaultMinSize = new Size(50, 50);

		internal static Graphics graphics = null;

		Point location;
		Size size;
		ResizeMode resizeMode = ResizeMode.None;
		Rectangle oldBorderRectangle = Rectangle.Empty;
		Rectangle oldDrawingArea = Rectangle.Empty;
		Size minimumSize = defaultMinSize;
		bool firstDraw = true;
		bool mouseLeaved = true;

		public event MoveEventHandler Move;
		public event ResizeEventHandler Resize;

		/// <exception cref="ArgumentNullException">
		/// <paramref name="entity"/> is null.
		/// </exception>
		protected DiagramShape(IEntity entity)
		{
			if (entity == null)
				throw new ArgumentNullException("entity");

			location = Point.Empty;
			size = DefaultSize;

			entity.Serializing += delegate(object sender, SerializeEventArgsBinary e) {
				OnSerializing(e);
			};
			entity.Deserializing += delegate(object sender, SerializeEventArgsBinary e) {
				OnDeserializing(e);
			};
		}

		protected virtual Size DefaultSize
		{
			get { return new Size(150, 200); }
		}

		protected virtual Size MinimumSize
		{
			get
			{
				return minimumSize;
			}
			set
			{
				minimumSize = value;
				if (minimumSize.Width > this.Width)
					this.Width = value.Width;
				if (minimumSize.Height > this.Height)
					this.Height = value.Height;
			}
		}

		//LATER: make it protected internal
		public abstract IEntity Entity
		{
			get;
		}

		protected internal override bool IsDirty
		{
			get
			{
				return (base.IsDirty || oldBorderRectangle != BorderRectangle);
			}
			protected set
			{
				if (!value)
					oldBorderRectangle = BorderRectangle;
				base.IsDirty = value;
			}
		}

		public Point Location
		{
			get
			{
				return location;
			}
			set
			{
				if (location != value) {
					Size offset = new Size(value.X - X, value.Y - Y);
					mouseDownLocation += offset;
					location = value;
					OnMove(new MoveEventArgs(offset));
				}
			}
		}

		public int X
		{
			get
			{
				return location.X;
			}
			set
			{
				if (location.X != value) {
					Size offset = new Size(value - X, 0);
					mouseDownLocation.X += offset.Width;
					location.X = value;
					OnMove(new MoveEventArgs(offset));
				}
			}
		}

		public int Y
		{
			get
			{
				return location.Y;
			}
			set
			{
				if (location.Y != value) {
					Size offset = new Size(0, value - Y);
					mouseDownLocation.Y += offset.Height;
					location.Y = value;
					OnMove(new MoveEventArgs(offset));
				}
			}
		}

		public virtual Size Size
		{
			get
			{
				return size;
			}
			set
			{
				if (value.Width < MinimumSize.Width)
					value.Width = MinimumSize.Width;
				if (value.Height < MinimumSize.Height)
					value.Height = MinimumSize.Height;

				if (size != value) {
					Size change = new Size(value.Width - Width, value.Height - Height);
					if (IsResizing) //TODO: ez kell?
						mouseDownLocation += change;
					size = value;
					OnResize(new ResizeEventArgs(change));
				}
			}
		}

		public virtual int Width
		{
			get
			{
				return size.Width;
			}
			set
			{
				if (value < MinimumSize.Width)
					value = MinimumSize.Width;

				if (size.Width != value) {
					Size change = new Size(value - Width, 0);
					if (IsResizing) //TODO: ez kell?
						mouseDownLocation.X += change.Width;
					size.Width = value;
					OnResize(new ResizeEventArgs(change));
				}
			}
		}

		public virtual int Height
		{
			get
			{
				return size.Height;
			}
			set
			{
				if (value < MinimumSize.Height)
					value = MinimumSize.Height;

				if (size.Height != value) {
					Size change = new Size(0, value - Height);
					if (IsResizing) //TODO: ez kell?
						mouseDownLocation.Y += change.Height;
					size.Height = value;
					OnResize(new ResizeEventArgs(change));
				}
			}
		}

		public int Left
		{
			get { return X; }
			set { X = value; }
		}

		public int Right
		{
			get { return X + Width; }
			set { X = value - Width; }
		}

		public int Top
		{
			get { return Y; }
			set { Y = value; }
		}

		public int Bottom
		{
			get { return Y + Height; }
			set { Y = value - Height; }
		}

		public Rectangle BorderRectangle
		{
			get { return new Rectangle(Location, Size); }
		}

		public Point CenterPoint
		{
			get { return new Point(HorizontalCenter, VerticalCenter); }
		}

		public int HorizontalCenter
		{
			get
			{
				return ((Left + Right) / 2);
			}
		}

		public int VerticalCenter
		{
			get
			{
				return ((Top + Bottom) / 2);
			}
		}

		protected abstract int GetBorderWidth(Style style);

		protected virtual Rectangle CalculateDrawingArea(Style style, bool printing)
		{
			Rectangle area = BorderRectangle;
			
			int borderSize = GetBorderWidth(style) / 2 + 1;
			if (IsSelected && !printing) {
				borderSize = Math.Max(borderSize, (int) (SelectionMargin / Zoom) + 1);
				area.Inflate(borderSize, borderSize);
			}
			else {
				area.Inflate(borderSize, borderSize);
				Size shadowSize = new Size(
					Math.Max(style.ShadowOffset.Width - borderSize + 1, 0),
					Math.Max(style.ShadowOffset.Height - borderSize + 1, 0)
				);
				area.Size += shadowSize;
			}

			return area;
		}

		protected internal sealed override RectangleF GetDirtyArea(Style style)
		{
			if (!IsDirty) {
				return oldDrawingArea;
			}
			else {
				Rectangle drawingArea = CalculateDrawingArea(style, false);

				if (firstDraw) {
					return drawingArea;
				}
				else {
					return RectangleF.FromLTRB(
						Math.Min(drawingArea.Left, oldDrawingArea.Left),
						Math.Min(drawingArea.Top, oldDrawingArea.Top),
						Math.Max(drawingArea.Right, oldDrawingArea.Right),
						Math.Max(drawingArea.Bottom, oldDrawingArea.Bottom)
					);
				}
			}
		}

		protected internal sealed override Rectangle GetPrintingClip(Style style)
		{
			return CalculateDrawingArea(style, true);
		}

		internal bool IsResizing
		{
			get { return (resizeMode != ResizeMode.None); }
		}

		private RectangleF GetSelectionRectangle()
		{
			float borderOffset = SelectionMargin / 2 / Zoom;

			return new RectangleF(Left - borderOffset, Top - borderOffset,
				Width + borderOffset * 2, Height + borderOffset * 2);
		}

		public virtual void Collapse()
		{
		}

		public virtual void Expand()
		{
		}

		protected virtual ResizeMode GetResizeMode(PointF mouseLocation)
		{
			if (Zoom <= UndreadableZoom)
				return ResizeMode.None;

			ResizeMode mode = ResizeMode.None;
			float squareSize = SelectionMargin / Zoom;
			int horCenter = HorizontalCenter;
			int verCenter = VerticalCenter;

			bool left   = (mouseLocation.X >= Left - squareSize && mouseLocation.X < Left);
			bool top    = (mouseLocation.Y >= Top  - squareSize && mouseLocation.Y < Top);
			bool right  = (mouseLocation.X >  Right  && mouseLocation.X < Right  + squareSize);
			bool bottom = (mouseLocation.Y >  Bottom && mouseLocation.Y < Bottom + squareSize);
			bool center = (mouseLocation.X >= horCenter - squareSize / 2 &&
				mouseLocation.X < horCenter + squareSize / 2);
			bool middle = (mouseLocation.Y >= verCenter - squareSize / 2 &&
				mouseLocation.Y < verCenter + squareSize / 2);

			if (right && (top || middle || bottom))
				mode |= ResizeMode.Right;

			if (bottom && (left || center || right))
				mode |= ResizeMode.Bottom;

			return mode;
		}

		public Cursor GetCursor(PointF mouseLocation)
		{
			ResizeMode mode = GetResizeMode(mouseLocation);

			switch (mode) {
				case ResizeMode.Bottom:
					return Cursors.SizeNS;

				case ResizeMode.Right:
					return Cursors.SizeWE;

				case ResizeMode.Bottom | ResizeMode.Right:
					return Cursors.SizeNWSE;

				default:
					return Cursors.Default;
			}
		}

        protected internal sealed override Rectangle GetLogicalArea()
        {
            return BorderRectangle;
        }

		private void DrawSelectionFrame(Graphics g)
		{
			if (Zoom > UndreadableZoom) {
				// Draw selection border and resizing squares
				RectangleF selectionRectanlge = GetSelectionRectangle();

				SmoothingMode oldSmmothingMode = g.SmoothingMode;
				g.SmoothingMode = SmoothingMode.HighSpeed;
				g.DrawRectangle(SelectionPen, selectionRectanlge.X, selectionRectanlge.Y,
					selectionRectanlge.Width, selectionRectanlge.Height);
				DrawResizingSquares(g, selectionRectanlge);
				g.SmoothingMode = oldSmmothingMode;
			}
			else {
				// Draw only selection border
				RectangleF border = BorderRectangle;
				float pixelSize = 1 / Zoom;
				g.DrawRectangle(SelectionPen, border.X - pixelSize * 2, border.Y - pixelSize * 2,
					border.Width + pixelSize * 4, border.Height + pixelSize * 4);
			}
		}

		private void DrawResizingSquares(Graphics g, RectangleF border)
		{
			selectionSquarePen.Width = 1 / Zoom;
			float squareSize = (SelectionMargin - 4) / Zoom;

			for (int row = 0; row < 3; row++) {
				for (int column = 0; column < 3; column++) {
					if (row != 1 || column != 1) { // It's not the center point
						float x = (border.X + (border.Width / 2) * column) - squareSize / 2;
						float y = (border.Y + (border.Height / 2) * row) - squareSize / 2;

						g.FillRectangle(Brushes.White, x, y, squareSize, squareSize);
						g.DrawRectangle(selectionSquarePen, x, y, squareSize, squareSize);
					}
				}
			}
		}

		protected internal override void Draw(Graphics g, bool onScreen, Style style)
		{
			if (IsSelected && onScreen)
				DrawSelectionFrame(g);
			if (onScreen) {
				IsDirty = false;
				firstDraw = false;
				oldDrawingArea = CalculateDrawingArea(style, false);
			}
		}

		protected internal sealed override void TrySelect(RectangleF container)
		{
			if (container.IntersectsWith(BorderRectangle))
				IsSelected = true;
		}

		protected internal sealed override void Offset(Size offset)
		{
			this.Location += offset;
		}

		protected internal override Size GetMaximalOffset(Size offset, Rectangle frame)
		{
			if (IsSelected) {
				Rectangle newPosition = this.BorderRectangle;
				newPosition.Offset(offset.Width, offset.Height);

				if (newPosition.Left < frame.Left)
					offset.Width += (frame.Left - newPosition.Left);
				if (newPosition.Right > frame.Right)
					offset.Width -= (newPosition.Right - frame.Right);

				if (newPosition.Top < frame.Top)
					offset.Height += (frame.Top - newPosition.Top);
				if (newPosition.Bottom > frame.Bottom)
					offset.Height -= (newPosition.Bottom - frame.Bottom);

			}

			return offset;
		}

		protected internal bool Contains(PointF point)
		{
			return (
				point.X >= Left && point.X <= Right &&
				point.Y >= Top  && point.Y <= Bottom
			);
		}

		internal void AutoWidth()
		{
			if (graphics != null)
				this.Width = (int) GetRequiredWidth(graphics, Style.CurrentStyle) + 1;
		}

		protected virtual float GetRequiredWidth(Graphics g, Style style)
		{
			return MinimumSize.Width;
		}

		internal void AutoHeight()
		{
			this.Height = GetRequiredHeight();
		}

		protected virtual int GetRequiredHeight()
		{
			return MinimumSize.Height;
		}

		protected internal override IEnumerable<ToolStripItem> GetContextMenuItems(IDiagram diagram)
		{
			return ShapeContextMenu.Default.GetMenuItems(diagram);
		}

		void IClipboardItem.Cut()
		{
			throw new NotImplementedException();
		}

		/// <exception cref="ArgumentNullException">
		/// <paramref name="diagram"/> is null.
		/// </exception>
		bool IClipboardItem.Paste(DiagramControl diagram)
		{
			if (diagram == null)
				throw new ArgumentNullException("diagram");

			throw new NotImplementedException();
		}
        protected virtual void OnSerializing(SerializeEventArgsBinary e)
        {
            e.Bformatter.Serialize(e.Stream,Location);
            e.Bformatter.Serialize(e.Stream,Size); 
            //e.Info.AddValue("_location", Location);
            //e.Info.AddValue("_size", Size);
        }
		protected virtual void OnSerializing(SerializeEventArgs e)
		{
			XmlElement locationNode = e.Node.OwnerDocument.CreateElement("Location");
			locationNode.SetAttribute("left", Left.ToString());
			locationNode.SetAttribute("top", Top.ToString());
			e.Node.AppendChild(locationNode);

			XmlElement sizeNode = e.Node.OwnerDocument.CreateElement("Size");
			sizeNode.SetAttribute("width", Width.ToString());
			sizeNode.SetAttribute("height", Height.ToString());
			e.Node.AppendChild(sizeNode);
		}
        protected virtual void OnDeserializing(SerializeEventArgsBinary e)
        {
            Location = (Point) e.Bformatter.Deserialize(e.Stream);
            Size = (Size)e.Bformatter.Deserialize(e.Stream);
            //e.Stream.GetValue("_location", typeof(Point));
            //Size = (Size) e.Info.GetValue("_size", typeof(Size));
        }
		protected virtual void OnDeserializing(SerializeEventArgs e)
		{
			XmlElement locationNode = e.Node["Location"];
			if (locationNode != null) {
				int left, top;

				int.TryParse(locationNode.GetAttribute("left"), out left);
				int.TryParse(locationNode.GetAttribute("top"), out top);
				this.Location = new Point(left, top);
			}

			XmlElement sizeNode = e.Node["Size"];
			if (sizeNode != null) {
				int width, height;

				int.TryParse(sizeNode.GetAttribute("width"), out width);
				int.TryParse(sizeNode.GetAttribute("height"), out height);
				this.Size = new Size(width, height);
			}
		}

		[Obsolete]
		protected internal sealed override void Serialize(XmlElement node)
		{
			OnSerializing(new SerializeEventArgs(node));
		}

		[Obsolete]
		protected internal sealed override void Deserialize(XmlElement node)
		{
			OnDeserializing(new SerializeEventArgs(node));
		}

        internal sealed override bool MousePressed(AbsoluteMouseEventArgs e)
        {
            bool pressed = Contains(e.Location);

            if (e.Button == MouseButtons.Left)
                pressed |= (IsSelected && GetResizeMode(e.Location) != ResizeMode.None);

            if (pressed)
                OnMouseDown(e);

            return pressed;
        }

        internal sealed override bool MouseMoved(AbsoluteMouseEventArgs e)
        {
            bool contains = IsResizing;

            if (!IsResizing) {
                contains = Contains(e.Location);

                contains |= (IsSelected && GetResizeMode(e.Location) != ResizeMode.None);

                if (contains && mouseLeaved)
                    OnMouseEnter(EventArgs.Empty);
                else if (!contains && !mouseLeaved)
                    OnMouseLeave(EventArgs.Empty);
            }

            bool moved = IsMousePressed || contains;
            if (moved)
                OnMouseMove(e);

            return moved;
        }

        internal sealed override bool MouseUpped(AbsoluteMouseEventArgs e)
		{
			bool upped = IsMousePressed;

			if (upped)
				OnMouseUp(e);

			return upped;
		}

		internal sealed override void MouseLeaved()
		{
			if (!mouseLeaved)
				OnMouseLeave(EventArgs.Empty);
		}

		internal sealed override bool DoubleClicked(AbsoluteMouseEventArgs e)
		{
			bool doubleClicked = Contains(e.Location);

			if (e.Button == MouseButtons.Left)
				doubleClicked |= (IsSelected && GetResizeMode(e.Location) != ResizeMode.None);

			if (doubleClicked)
				OnDoubleClick(e);

			return doubleClicked;
		}

		private void PerformResize(PointF mouseLocation)
		{
			if ((resizeMode & ResizeMode.Right) != 0) {
				int offset = (int) (mouseLocation.X - mouseDownLocation.X);
				Width += offset;
			}
			if ((resizeMode & ResizeMode.Bottom) != 0) {
				int offset = (int) (mouseLocation.Y - mouseDownLocation.Y);
				Height += offset;
			}
		}

		protected override void OnMouseDown(AbsoluteMouseEventArgs e)
		{
			base.OnMouseDown(e);
			resizeMode = GetResizeMode(e.Location);
		}

		protected override void OnMouseMove(AbsoluteMouseEventArgs e)
		{
			base.OnMouseMove(e);

			if (IsResizing) {
				PerformResize(e.Location);
			}
			else if (IsMousePressed && e.Button == MouseButtons.Left) {
				Size offset = new Size(
					(int) (e.X - mouseDownLocation.X),
					(int) (e.Y - mouseDownLocation.Y));
				
				OnDragging(new MoveEventArgs(offset));
			}
		}

		protected override void OnMouseUp(AbsoluteMouseEventArgs e)
		{
			base.OnMouseUp(e);
			resizeMode = ResizeMode.None;
		}

		protected override void OnDoubleClick(AbsoluteMouseEventArgs e)
		{
			base.OnDoubleClick(e);
			ResizeMode resizeMode = GetResizeMode(e.Location);

			if ((resizeMode & ResizeMode.Right) != 0)
				AutoWidth();
			if ((resizeMode & ResizeMode.Bottom) != 0)
				AutoHeight();
		}

		protected virtual void OnMove(MoveEventArgs e)
		{
			if (Move != null)
				Move(this, e);
		}

		protected virtual void OnResize(ResizeEventArgs e)
		{
			if (Resize != null)
				Resize(this, e);
		}

		protected virtual void OnMouseEnter(EventArgs e)
		{
			mouseLeaved = false;
		}

		protected virtual void OnMouseLeave(EventArgs e)
		{
			mouseLeaved = true;
		}

		public override string ToString()
		{
			return Entity.ToString();
		}
	}
}
