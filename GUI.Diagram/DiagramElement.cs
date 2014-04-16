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
using System.Windows.Forms;
using System.Collections.Generic;
using System.Runtime.Serialization;
using System.Runtime.Serialization.Formatters.Binary;

namespace NClass.GUI.Diagram
{
    [Serializable]
	public abstract class DiagramElement : ISerializable
	{
        public DiagramElement(SerializationInfo info, StreamingContext ctxt) 
        {
        }
        public DiagramElement() { }
        public virtual void GetObjectData(SerializationInfo info, StreamingContext ctxt)
        {
        }
		protected const float UndreadableZoom = 0.25F;
		const float DashSize = 3;

		static Pen selectionPen;
		static float zoom = 1.0F;

		bool isSelected = false;
		bool isDirty = true;
		bool isMousePressed = false;
		protected PointF mouseDownLocation = PointF.Empty; //TODO: legyen írásvédett!

		protected internal event MoveEventHandler Dragging;
		protected internal event AbsoluteMouseEventHandler MouseDown;
		protected internal event AbsoluteMouseEventHandler MouseMove;
		protected internal event AbsoluteMouseEventHandler MouseUp;
		protected internal event AbsoluteMouseEventHandler DoubleClick;
		protected internal event EventHandler SelectionChanged;

		static DiagramElement()
		{
			selectionPen = new Pen(Color.Black);
			selectionPen.DashPattern = new float[] { DashSize, DashSize };
		}

		protected internal static float Zoom
		{
			get
			{
				return zoom;
			}
			set
			{
				zoom = value;

				selectionPen.Width = 1 / zoom;
				if (zoom > 1)
					selectionPen.DashPattern = new float[] { DashSize / zoom, DashSize / zoom };
				else
					selectionPen.DashPattern = new float[] { DashSize, DashSize };
			}
		}

		protected Pen SelectionPen
		{
			get { return selectionPen; }
		}

		public bool IsSelected
		{
			get
			{
				return isSelected;
			}
			internal set
			{
				if (isSelected != value) {
					isDirty = true;
					isSelected = value;
					OnSelectionChanged(EventArgs.Empty);
				}
			}
		}

		protected internal virtual bool IsDirty
		{
			get { return isDirty; }
			protected set { isDirty = value; }
		}

		protected bool IsMousePressed
		{
			get { return isMousePressed; }
		}

		internal RectangleF GetDirtyArea()
		{
			return GetDirtyArea(Style.CurrentStyle);
		}

		protected internal abstract RectangleF GetDirtyArea(Style style);

		protected internal abstract Rectangle GetPrintingClip(Style style);

		internal void Draw(Graphics g)
		{
			Draw(g, true, Style.CurrentStyle);
		}

		internal void Draw(Graphics g, bool onScreen)
		{
			Draw(g, onScreen, Style.CurrentStyle);
		}

        protected internal abstract Rectangle GetLogicalArea();

		protected internal abstract void Draw(Graphics g, bool onScreen, Style style);

		protected internal abstract void TrySelect(RectangleF container);

		protected internal abstract void Offset(Size offset);

		protected internal abstract Size GetMaximalOffset(Size offset, Rectangle frame);

		protected internal abstract IEnumerable<ToolStripItem> GetContextMenuItems(IDiagram diagram);

		[Obsolete]
		protected internal abstract void Serialize(XmlElement node);

		[Obsolete]
		protected internal abstract void Deserialize(XmlElement node);

		protected virtual void OnMouseDown(AbsoluteMouseEventArgs e)
		{
			isMousePressed = true;
			mouseDownLocation = e.Location;
			IsSelected = true;

			if (MouseDown != null)
				MouseDown(this, e);
		}

		protected virtual void OnDragging(MoveEventArgs e)
		{
			if (Dragging != null)
				Dragging(this, e);
		}

		protected virtual void OnMouseMove(AbsoluteMouseEventArgs e)
		{
			if (MouseMove != null)
				MouseMove(this, e);
		}

		protected virtual void OnMouseUp(AbsoluteMouseEventArgs e)
		{
			isMousePressed = false;
			if (MouseUp != null)
				MouseUp(this, e);
		}

		protected virtual void OnDoubleClick(AbsoluteMouseEventArgs e)
		{
			if (DoubleClick != null)
				DoubleClick(this, e);
		}

		protected virtual void OnSelectionChanged(EventArgs e)
		{
			if (SelectionChanged != null)
				SelectionChanged(this, e);
		}

        internal abstract bool MousePressed(AbsoluteMouseEventArgs e);

        internal abstract bool MouseMoved(AbsoluteMouseEventArgs e);

        internal abstract bool MouseUpped(AbsoluteMouseEventArgs e);

        internal abstract void MouseLeaved();

		internal abstract bool DoubleClicked(AbsoluteMouseEventArgs e);
    }
}
