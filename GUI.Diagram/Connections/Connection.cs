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
using System.Collections.Generic;
using System.Drawing;
using System.Drawing.Drawing2D;
using System.Windows.Forms;
using System.Xml;
using NClass.Core;
using System.Runtime.Serialization;
using System.Runtime.Serialization.Formatters.Binary;

namespace NClass.GUI.Diagram
{
    [Serializable]
	public abstract class Connection : DiagramElement
	{
		private enum LineOrientation
		{
			Horizontal,
			Vertical
		}

		const int PrecisionSize = 6;
		const int PickTolerance = 4;
		internal const int Spacing = 25;
		static readonly float[] dashPattern = new float[] { 5, 5 };
		static Pen linePen = new Pen(Color.Black);
		static SolidBrush textBrush = new SolidBrush(Color.Black);
		static StringFormat stringFormat = new StringFormat();

		DiagramShape startShape;
		DiagramShape endShape;
		LineOrientation startOrientation = LineOrientation.Vertical;
		LineOrientation endOrientation = LineOrientation.Vertical;
		OrderedList<BendPoint> bendPoints = new OrderedList<BendPoint>();
		BendPoint selectedBendPoint = null;
		bool copied = false;

		List<Point> routeCache = new List<Point>();
		Point[] routeCacheArray = null;
		Rectangle oldDrawingArea = Rectangle.Empty;
		bool firstDraw = true;

		public event EventHandler Changed;

		/// <exception cref="ArgumentNullException">
		/// <paramref name="relation"/> is null.-or-
		/// <paramref name="startShape"/> is null.-or-
		/// <paramref name="endShape"/> is null.
		/// </exception>
		protected Connection(Relation relation, DiagramShape startShape, DiagramShape endShape)
		{
			if (relation == null)
				throw new ArgumentNullException("relation");
			if (startShape == null)
				throw new ArgumentNullException("startShape");
			if (endShape == null)
				throw new ArgumentNullException("endShape");

			this.startShape = startShape;
			this.endShape = endShape;
			if (startShape == endShape)
				startOrientation = LineOrientation.Horizontal;
			bendPoints.Add(new BendPoint(startShape, true));
			bendPoints.Add(new BendPoint(endShape, false));

			startShape.Move += ShapeMoving;
			startShape.Resize += StartShapeResizing;
			endShape.Move += ShapeMoving;
			endShape.Resize += EndShapeResizing;

			relation.Detaching += delegate {
				startShape.Move -= ShapeMoving;
				startShape.Resize -= StartShapeResizing;
				endShape.Move -= ShapeMoving;
				endShape.Resize -= EndShapeResizing;
			};
            relation.my_connection = this;
			relation.Serializing += delegate(object sender, SerializeEventArgsBinary e) {
				OnSerializing(e);
			};
			relation.Deserializing += delegate(object sender, SerializeEventArgsBinary e) {
				OnDeserializing(e);
			};
			Reroute();
		}

		protected internal abstract Relation Relation
		{
			get;
		}

		private BendPoint StartPoint
		{
			get { return bendPoints.FirstValue; }
		}

		private BendPoint EndPoint
		{
			get { return bendPoints.LastValue; }
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

		protected virtual bool IsDashed
		{
			get { return false; }
		}

		private Rectangle GetRouteArea()
		{
			Point topLeft = routeCache[0];
			Point bottomRight = routeCache[0];

			for (int i = 1; i < routeCache.Count; i++) {
				if (topLeft.X > routeCache[i].X)
					topLeft.X = routeCache[i].X;
				if (topLeft.Y > routeCache[i].Y)
					topLeft.Y = routeCache[i].Y;
				if (bottomRight.X < routeCache[i].X)
					bottomRight.X = routeCache[i].X;
				if (bottomRight.Y < routeCache[i].Y)
					bottomRight.Y = routeCache[i].Y;
			}

			return Rectangle.FromLTRB(topLeft.X, topLeft.Y, bottomRight.X, bottomRight.Y);
		}

		protected virtual Rectangle CalculateDrawingArea(Style style, bool printing)
		{
			Rectangle area = GetRouteArea();

			int lineSize = style.RelationWidth + 5; //TODO: itt nem ennyi kell
			if (IsSelected && !printing)
				lineSize = Math.Max(lineSize, (int) (BendPoint.SquareSize / Zoom) + 1);
			area.Inflate(lineSize, lineSize);

			//TODO: start + end caps
			//TODO: label

			//HACK: quick and dirty solution for arrow caps:
			area.Inflate(10, 10);

			return area;
		}

		protected internal sealed override Rectangle GetPrintingClip(Style style)
		{
			return CalculateDrawingArea(style, true);
		}

		private void RouteChanged()
		{
			IsDirty = true;
			Reroute();
			OnChanged(EventArgs.Empty);
		}

		private void ShapeMoving(object sender, MoveEventArgs e)
		{
			RouteChanged();
		}

		private void StartShapeResizing(object sender, ResizeEventArgs e)
		{
			foreach (BendPoint bendPoint in bendPoints) {
				if (!bendPoint.RelativeToStartShape)
					break;
				bendPoint.ShapeResized(e.Change);
			}

			RouteChanged();
		}

		private void EndShapeResizing(object sender, ResizeEventArgs e)
		{
			foreach (BendPoint bendPoint in bendPoints.GetReversedList()) {
				if (bendPoint.RelativeToStartShape)
					break;
				bendPoint.ShapeResized(e.Change);
			}

			RouteChanged();
		}

		internal void AutoRoute()
		{
			if (bendPoints.Count > 0) {
				ClearBendPoints();
				IsDirty = true;
				Reroute();
				OnChanged(EventArgs.Empty);
			}
		}

		private void ClearBendPoints()
		{
			BendPoint startPoint = StartPoint;
			BendPoint endPoint = EndPoint;

			bendPoints.Clear();
			bendPoints.Add(startPoint);
			bendPoints.Add(endPoint);
			startPoint.AutoPosition = true;
			endPoint.AutoPosition = true;
		}

		internal void ShowPropertiesDialog()
		{
			throw new NotImplementedException();
		}

		protected internal sealed override Rectangle GetLogicalArea()
		{
			return GetRouteArea();
		}

		protected internal override void Draw(Graphics g, bool onScreen, Style style)
		{
			DrawLine(g, onScreen, style);
			DrawCaps(g, onScreen, style);
			if (Relation.HasLabel)
				DrawLabel(g, onScreen, style);
			if (onScreen) {
				IsDirty = false;
				firstDraw = false;
				oldDrawingArea = CalculateDrawingArea(style, false);
			}
		}

		private void DrawLine(Graphics g, bool onScreen, Style style)
		{
			if (IsSelected && onScreen) {
				SmoothingMode smoothingMode = g.SmoothingMode;
				g.SmoothingMode = SmoothingMode.HighSpeed;

				g.DrawLines(SelectionPen, routeCacheArray);
				if (Zoom > UndreadableZoom) {
					foreach (BendPoint point in bendPoints)
						point.Draw(g, onScreen, style, Zoom);
				}
				g.SmoothingMode = smoothingMode;
			}
			else {
				linePen.Width = style.RelationWidth;
				linePen.Color = style.RelationColor;
				if (IsDashed) {
					dashPattern[0] = style.RelationDashSize;
					dashPattern[1] = style.RelationDashSize;
					linePen.DashPattern = dashPattern;
				}
				else {
					linePen.DashStyle = DashStyle.Solid;
				}

				g.DrawLines(linePen, routeCacheArray);
			}
		}

		private void DrawCaps(Graphics g, bool onScreen, Style style)
		{
			Matrix transformState = g.Transform;
			g.TranslateTransform(routeCache[0].X, routeCache[0].Y);
			g.RotateTransform(GetAngle(routeCache[0], routeCache[1]));
			DrawStartCap(g, onScreen, style);
			g.Transform = transformState;

			int last = routeCache.Count - 1;
			g.TranslateTransform(routeCache[last].X, routeCache[last].Y);
			g.RotateTransform(GetAngle(routeCache[last], routeCache[last - 1]));
			DrawEndCap(g, onScreen, style);
			g.Transform = transformState;
		}

		private float GetAngle(Point point1, Point point2)
		{
			if (point1.X == point2.X) {
				return (point1.Y < point2.Y) ? 0 : 180;
			}
			else if (point1.Y == point2.Y) {
				return (point1.X < point2.X) ? -90 : 90;
			}
			else {
				return (float) (
					Math.Atan2(point2.Y - point1.Y, point2.X - point1.X) * (180 / Math.PI)) - 90;
			}
		}

		protected virtual void DrawStartCap(Graphics g, bool onScreen, Style style)
		{
		}

		protected virtual void DrawEndCap(Graphics g, bool onScreen, Style style)
		{
		}

		private void DrawLabel(Graphics g, bool onScreen, Style style)
		{
			if (!string.IsNullOrEmpty(Relation.Label)) {
				bool horizontal;
				PointF center = GetLineCenter(out horizontal);

				textBrush.Color = style.RelationTextColor;
				if (horizontal) {
					stringFormat.Alignment = StringAlignment.Center;
					stringFormat.LineAlignment = StringAlignment.Far;
				}
				else {
					stringFormat.Alignment = StringAlignment.Near;
					stringFormat.LineAlignment = StringAlignment.Center;
				}
				g.DrawString(Relation.Label, style.RelationTextFont, textBrush, center, stringFormat);
			}
		}

		private PointF GetLineCenter(out bool horizontal)
		{
			int lineLength = 0;
			for (int i = 0; i < routeCache.Count - 1; i++) {
				if (routeCache[i].X == routeCache[i + 1].X)
					lineLength += Math.Abs(routeCache[i].Y - routeCache[i + 1].Y);
				else
					lineLength += Math.Abs(routeCache[i].X - routeCache[i + 1].X);
			}

			int distance = lineLength / 2;
			int index = 0;
			horizontal = true;
			while (distance >= 0) {
				if (routeCache[index].X == routeCache[index + 1].X) {
					distance -= Math.Abs(routeCache[index].Y - routeCache[index + 1].Y);
					horizontal = false;
				}
				else {
					distance -= Math.Abs(routeCache[index].X - routeCache[index + 1].X);
					horizontal = true;
				}
				index++;
			}

			return new PointF(
				(float) (routeCache[index - 1].X + routeCache[index].X) / 2,
				(float) (routeCache[index - 1].Y + routeCache[index].Y) / 2
			);
		}

		protected virtual void Reroute()
		{
			RecalculateOrientations();
			RelocateAutoBendPoints();
			RerouteFromBendPoints();
		}

		private void RecalculateOrientations()
		{
			if (!StartPoint.AutoPosition) {
				if (StartPoint.X >= startShape.Left &&
					StartPoint.X <= startShape.Right)
				{
					startOrientation = LineOrientation.Vertical;
				}
				else if (StartPoint.Y >= startShape.Top &&
					StartPoint.Y <= startShape.Bottom)
				{
					startOrientation = LineOrientation.Horizontal;
				}
			}
			if (!EndPoint.AutoPosition) {
				if (EndPoint.X >= endShape.Left &&
					EndPoint.X <= endShape.Right)
				{
					endOrientation = LineOrientation.Vertical;
				}
				else if (EndPoint.Y >= endShape.Top &&
					EndPoint.Y <= endShape.Bottom)
				{
					endOrientation = LineOrientation.Horizontal;
				}
			}
		}

		private void RelocateAutoBendPoints()
		{
			if (StartPoint.AutoPosition && EndPoint.AutoPosition) {
				if (startOrientation == endOrientation && startShape == endShape) {
					startOrientation = LineOrientation.Horizontal;
					endOrientation = LineOrientation.Vertical;
				}

				if (startOrientation == LineOrientation.Horizontal &&
					endOrientation == LineOrientation.Horizontal)
				{
					if (startShape.Right <= endShape.Left - 2 * Spacing) {
						StartPoint.X = startShape.Right + Spacing;
						EndPoint.X = endShape.Left - Spacing;
					}
					else if (startShape.Left >= endShape.Right + 2 * Spacing) {
						StartPoint.X = startShape.Left - Spacing;
						EndPoint.X = endShape.Right + Spacing;
					}
					else {
						if (Math.Abs(startShape.Left - endShape.Left) <
							Math.Abs(startShape.Right - endShape.Right))
						{
							StartPoint.X = startShape.Left - Spacing;
							EndPoint.X = endShape.Left - Spacing;
						}
						else {
							StartPoint.X = startShape.Right + Spacing;
							EndPoint.X = endShape.Right + Spacing;
						}
					}

					DiagramShape smallerShape, biggerShape;
					if (startShape.Height < endShape.Height) {
						smallerShape = startShape;
						biggerShape = endShape;
					}
					else {
						smallerShape = endShape;
						biggerShape = startShape;
					}

					if (biggerShape.Top <= smallerShape.VerticalCenter &&
						biggerShape.Bottom >= smallerShape.VerticalCenter)
					{
						int center = (
							Math.Max(startShape.Top, endShape.Top) +
							Math.Min(startShape.Bottom, endShape.Bottom)) / 2;

						StartPoint.Y = center;
						EndPoint.Y = center;
					}
					else {
						StartPoint.Y = startShape.VerticalCenter;
						EndPoint.Y = endShape.VerticalCenter;
					}
				}
				else if (startOrientation == LineOrientation.Vertical &&
					endOrientation == LineOrientation.Vertical)
				{
					if (startShape.Bottom <= endShape.Top - 2 * Spacing) {
						StartPoint.Y = startShape.Bottom + Spacing;
						EndPoint.Y = endShape.Top - Spacing;
					}
					else if (startShape.Top >= endShape.Bottom + 2 * Spacing) {
						StartPoint.Y = startShape.Top - Spacing;
						EndPoint.Y = endShape.Bottom + Spacing;
					}
					else {
						if (Math.Abs(startShape.Top - endShape.Top) <
							Math.Abs(startShape.Bottom - endShape.Bottom))
						{
							StartPoint.Y = startShape.Top - Spacing;
							EndPoint.Y = endShape.Top - Spacing;
						}
						else {
							StartPoint.Y = startShape.Bottom + Spacing;
							EndPoint.Y = endShape.Bottom + Spacing;
						}
					}

					DiagramShape smallerShape, biggerShape;
					if (startShape.Width < endShape.Width) {
						smallerShape = startShape;
						biggerShape = endShape;
					}
					else {
						smallerShape = endShape;
						biggerShape = startShape;
					}

					if (biggerShape.Left <= smallerShape.HorizontalCenter &&
						biggerShape.Right >= smallerShape.HorizontalCenter)
					{
						int center = (
							Math.Max(startShape.Left, endShape.Left) +
							Math.Min(startShape.Right, endShape.Right)) / 2;

						StartPoint.X = center;
						EndPoint.X = center;
					}
					else {
						StartPoint.X = startShape.HorizontalCenter;
						EndPoint.X = endShape.HorizontalCenter;
					}
				}
				else {
					if (startOrientation == LineOrientation.Horizontal) {
						StartPoint.Y = startShape.VerticalCenter;
						EndPoint.X = endShape.HorizontalCenter;

						if (EndPoint.X >= startShape.HorizontalCenter)
							StartPoint.X = startShape.Right + Spacing;
						else
							StartPoint.X = startShape.Left - Spacing;

						if (StartPoint.Y >= endShape.VerticalCenter)
							EndPoint.Y = endShape.Bottom + Spacing;
						else
							EndPoint.Y = endShape.Top - Spacing;
					}
					else {
						StartPoint.X = startShape.HorizontalCenter;
						EndPoint.Y = endShape.VerticalCenter;

						if (EndPoint.Y >= startShape.VerticalCenter)
							StartPoint.Y = startShape.Bottom + Spacing;
						else
							StartPoint.Y = startShape.Top - Spacing;

						if (StartPoint.X >= endShape.HorizontalCenter)
							EndPoint.X = endShape.Right + Spacing;
						else
							EndPoint.X = endShape.Left - Spacing;
					}
				}
			}
			else if (StartPoint.AutoPosition) {
				if (startOrientation == LineOrientation.Horizontal) {
					if (bendPoints.SecondValue.X < startShape.HorizontalCenter)
						StartPoint.X = startShape.Left - Spacing;
					else
						StartPoint.X = startShape.Right + Spacing;
					StartPoint.Y = startShape.VerticalCenter;
				}
				else {
					if (bendPoints.SecondValue.Y < startShape.VerticalCenter)
						StartPoint.Y = startShape.Top - Spacing;
					else
						StartPoint.Y = startShape.Bottom + Spacing;
					StartPoint.X = startShape.HorizontalCenter;
				}
			}
			else if (EndPoint.AutoPosition) {
				if (endOrientation == LineOrientation.Horizontal) {
					if (bendPoints.SecondLastValue.X < endShape.HorizontalCenter)
						EndPoint.X = endShape.Left - Spacing;
					else
						EndPoint.X = endShape.Right + Spacing;
					EndPoint.Y = endShape.VerticalCenter;
				}
				else {
					if (bendPoints.SecondLastValue.Y < endShape.VerticalCenter)
						EndPoint.Y = endShape.Top - Spacing;
					else
						EndPoint.Y = endShape.Bottom + Spacing;
					EndPoint.X = endShape.HorizontalCenter;
				}
			}
		}

		private void RerouteFromBendPoints()
		{
			routeCache.Clear();

			FlowDirection direction = AddStartSegment();
			
			LinkedListNode<BendPoint> current = bendPoints.First;
			while (current != bendPoints.Last) {
				direction = AddInnerSegment(current, direction);
				current = current.Next;
			}

			AddEndSegment();

			routeCacheArray = routeCache.ToArray();
			Array.Reverse(routeCacheArray);
		}

		private FlowDirection AddInnerSegment(LinkedListNode<BendPoint> current, FlowDirection direction)
		{
			BendPoint activePoint = current.Value;
			BendPoint nextPoint = current.Next.Value;

			if (nextPoint.X == activePoint.X) {
				routeCache.Add(nextPoint.Location);

				if (nextPoint.Y < activePoint.Y)
					return FlowDirection.BottomUp;
				else
					return FlowDirection.TopDown;
			}
			else if (nextPoint.Y == activePoint.Y) {
				routeCache.Add(nextPoint.Location);

				if (nextPoint.X < activePoint.X)
					return FlowDirection.RightToLeft;
				else
					return FlowDirection.LeftToRight;
			}

			else if (direction == FlowDirection.TopDown) {
				if (nextPoint.Y < activePoint.Y) {
					routeCache.Add(new Point(nextPoint.X, activePoint.Y));
					routeCache.Add(nextPoint.Location);
					return FlowDirection.BottomUp;
				}
				else {
					Point nextNextPoint = GetNextNextPoint(current);

					if (current.Next.Next == null &&
						nextNextPoint.X == nextPoint.X &&
						nextNextPoint.Y >= nextPoint.Y)
					{
						int center = (nextPoint.Y + activePoint.Y) / 2;
						routeCache.Add(new Point(activePoint.X, center));
						routeCache.Add(new Point(nextPoint.X, center));
						routeCache.Add(nextPoint.Location);
						return FlowDirection.TopDown;
					}
					else if (nextPoint.X < activePoint.X) {
						if (nextNextPoint.X >= activePoint.X ||
							(nextNextPoint.Y >= nextPoint.Y &&
							 nextNextPoint.X > nextPoint.X))
						{
							routeCache.Add(new Point(nextPoint.X, activePoint.Y));
							routeCache.Add(nextPoint.Location);
							return FlowDirection.TopDown;
						}
						else {
							routeCache.Add(new Point(activePoint.X, nextPoint.Y));
							routeCache.Add(nextPoint.Location);
							return FlowDirection.RightToLeft;
						}
					}
					else {
						if (nextNextPoint.X <= activePoint.X ||
							(nextNextPoint.Y >= nextPoint.Y &&
							nextNextPoint.X < nextPoint.X))
						{
							routeCache.Add(new Point(nextPoint.X, activePoint.Y));
							routeCache.Add(nextPoint.Location);
							return FlowDirection.TopDown;
						}
						else {
							routeCache.Add(new Point(activePoint.X, nextPoint.Y));
							routeCache.Add(nextPoint.Location);
							return FlowDirection.LeftToRight;
						}
					}
				}
			}
			else if (direction == FlowDirection.BottomUp) {
				if (nextPoint.Y > activePoint.Y) {
					routeCache.Add(new Point(nextPoint.X, activePoint.Y));
					routeCache.Add(nextPoint.Location);
					return FlowDirection.TopDown;
				}
				else {
					Point nextNextPoint = GetNextNextPoint(current);

					if (current.Next.Next == null &&
						nextNextPoint.X == nextPoint.X &&
						nextNextPoint.Y <= nextPoint.Y)
					{
						int center = (nextPoint.Y + activePoint.Y) / 2;
						routeCache.Add(new Point(activePoint.X, center));
						routeCache.Add(new Point(nextPoint.X, center));
						routeCache.Add(nextPoint.Location);
						return FlowDirection.BottomUp;
					}
					else if (nextPoint.X > activePoint.X) {

						if (nextNextPoint.X <= activePoint.X ||
							(nextNextPoint.Y <= nextPoint.Y &&
							 nextNextPoint.X < nextPoint.X))
						{
							routeCache.Add(new Point(nextPoint.X, activePoint.Y));
							routeCache.Add(nextPoint.Location);
							return FlowDirection.BottomUp;
						}
						else {
							routeCache.Add(new Point(activePoint.X, nextPoint.Y));
							routeCache.Add(nextPoint.Location);
							return FlowDirection.LeftToRight;
						}
					}
					else {
						if (nextNextPoint.X >= activePoint.X ||
							(nextNextPoint.Y <= nextPoint.Y &&
							nextNextPoint.X > nextPoint.X))
						{
							routeCache.Add(new Point(nextPoint.X, activePoint.Y));
							routeCache.Add(nextPoint.Location);
							return FlowDirection.BottomUp;
						}
						else {
							routeCache.Add(new Point(activePoint.X, nextPoint.Y));
							routeCache.Add(nextPoint.Location);
							return FlowDirection.RightToLeft;
						}
					}
				}
			}
			else if (direction == FlowDirection.LeftToRight) {
				if (nextPoint.X < activePoint.X) {
					routeCache.Add(new Point(activePoint.X, nextPoint.Y));
					routeCache.Add(nextPoint.Location);
					return FlowDirection.RightToLeft;
				}
				else {
					Point nextNextPoint = GetNextNextPoint(current);

					if (current.Next.Next == null &&
						nextNextPoint.Y == nextPoint.Y &&
						nextNextPoint.X >= nextPoint.X)
					{
						int center = (nextPoint.X + activePoint.X) / 2;
						routeCache.Add(new Point(center, activePoint.Y));
						routeCache.Add(new Point(center, nextPoint.Y));
						routeCache.Add(nextPoint.Location);
						return FlowDirection.LeftToRight;
					}
					if (nextPoint.Y > activePoint.Y) {
						if (nextNextPoint.Y <= activePoint.Y ||
							(nextNextPoint.X >= nextPoint.X &&
							nextNextPoint.Y < nextPoint.Y))
						{
							routeCache.Add(new Point(activePoint.X, nextPoint.Y));
							routeCache.Add(nextPoint.Location);
							return FlowDirection.LeftToRight;
						}
						else {
							routeCache.Add(new Point(nextPoint.X, activePoint.Y));
							routeCache.Add(nextPoint.Location);
							return FlowDirection.TopDown;
						}
					}
					else {
						if (nextNextPoint.Y >= activePoint.Y ||
							(nextNextPoint.X >= nextPoint.X &&
							nextNextPoint.Y > nextPoint.Y))
						{
							routeCache.Add(new Point(activePoint.X, nextPoint.Y));
							routeCache.Add(nextPoint.Location);
							return FlowDirection.LeftToRight;
						}
						else {
							routeCache.Add(new Point(nextPoint.X, activePoint.Y));
							routeCache.Add(nextPoint.Location);
							return FlowDirection.BottomUp;
						}
					}
				}
			}
			else if (direction == FlowDirection.RightToLeft) {
				if (nextPoint.X > activePoint.X) {
					routeCache.Add(new Point(activePoint.X, nextPoint.Y));
					routeCache.Add(nextPoint.Location);
					return FlowDirection.LeftToRight;
				}
				else {
					Point nextNextPoint = GetNextNextPoint(current);

					if (current.Next.Next == null &&
						nextNextPoint.Y == nextPoint.Y &&
						nextNextPoint.X <= nextPoint.X)
					{
						int center = (nextPoint.X + activePoint.X) / 2;
						routeCache.Add(new Point(center, activePoint.Y));
						routeCache.Add(new Point(center, nextPoint.Y));
						routeCache.Add(nextPoint.Location);
						return FlowDirection.RightToLeft;
					}
					if (nextPoint.Y < activePoint.Y) {
						if (nextNextPoint.Y >= activePoint.Y ||
							(nextNextPoint.X <= nextPoint.X &&
							nextNextPoint.Y > nextPoint.Y))
						{
							routeCache.Add(new Point(activePoint.X, nextPoint.Y));
							routeCache.Add(nextPoint.Location);
							return FlowDirection.RightToLeft;
						}
						else {
							routeCache.Add(new Point(nextPoint.X, activePoint.Y));
							routeCache.Add(nextPoint.Location);
							return FlowDirection.BottomUp;
						}
					}
					else {
						if (nextNextPoint.Y <= activePoint.Y ||
							(nextNextPoint.X <= nextPoint.X &&
							nextNextPoint.Y < nextPoint.Y))
						{
							routeCache.Add(new Point(activePoint.X, nextPoint.Y));
							routeCache.Add(nextPoint.Location);
							return FlowDirection.RightToLeft;
						}
						else {
							routeCache.Add(new Point(nextPoint.X, activePoint.Y));
							routeCache.Add(nextPoint.Location);
							return FlowDirection.TopDown;
						}
					}
				}
			}
			else {
				routeCache.Add(nextPoint.Location);
				return direction;
			}
		}

		private Point GetNextNextPoint(LinkedListNode<BendPoint> current)
		{
			LinkedListNode<BendPoint> next = current.Next;
			LinkedListNode<BendPoint> nextNext = next.Next;

			if (nextNext != null) {
				return nextNext.Value.Location;
			}
			else {
				Point nextNextPoint = next.Value.Location;

				if (nextNextPoint.X < endShape.Left)
					nextNextPoint.X = endShape.Left;
				else if (nextNextPoint.X > endShape.Right)
					nextNextPoint.X = endShape.Right;
				if (nextNextPoint.Y < endShape.Top)
					nextNextPoint.Y = endShape.Top;
				else if (nextNextPoint.Y > endShape.Bottom)
					nextNextPoint.Y = endShape.Bottom;

				return nextNextPoint;
			}
		}

		private FlowDirection AddStartSegment()
		{
			if (startOrientation == LineOrientation.Horizontal) {
				int startX, startY;

				if (StartPoint.X < startShape.HorizontalCenter)
					startX = startShape.Left;
				else
					startX = startShape.Right;

				if (StartPoint.Y >= startShape.Top &&
					StartPoint.Y <= startShape.Bottom)
				{
					startY = StartPoint.Y;
					routeCache.Add(new Point(startX, startY));
					routeCache.Add(StartPoint.Location);

					if (startX == startShape.Left)
						return FlowDirection.RightToLeft;
					else
						return FlowDirection.LeftToRight;
				}
				else {
					startY = startShape.VerticalCenter;
					routeCache.Add(new Point(startX, startY));
					routeCache.Add(new Point(StartPoint.X, startY));
					routeCache.Add(StartPoint.Location);

					if (StartPoint.Y < startY)
						return FlowDirection.BottomUp;
					else
						return FlowDirection.TopDown;
				}
			}
			else {
				int startX, startY;

				if (StartPoint.Y < startShape.VerticalCenter)
					startY = startShape.Top;
				else
					startY = startShape.Bottom;

				if (StartPoint.X >= startShape.Left &&
					StartPoint.X <= startShape.Right)
				{
					startX = StartPoint.X;
					routeCache.Add(new Point(startX, startY));
					routeCache.Add(StartPoint.Location);

					if (startY == startShape.Top)
						return FlowDirection.BottomUp;
					else
						return FlowDirection.TopDown;
				}
				else {
					startX = startShape.HorizontalCenter;
					routeCache.Add(new Point(startX, startY));
					routeCache.Add(new Point(startX, StartPoint.Y));
					routeCache.Add(StartPoint.Location);

					if (StartPoint.X < startX)
						return FlowDirection.RightToLeft;
					else
						return FlowDirection.LeftToRight;
				}
			}
		}

		private void AddEndSegment()
		{
			if (endOrientation == LineOrientation.Horizontal) {
				int endX, endY;

				if (EndPoint.X < endShape.HorizontalCenter)
					endX = endShape.Left;
				else
					endX = endShape.Right;

				if (EndPoint.Y >= endShape.Top &&
					EndPoint.Y <= endShape.Bottom)
				{
					endY = EndPoint.Y;
				}
				else {
					endY = endShape.VerticalCenter;
					routeCache.Add(new Point(EndPoint.X, endY));
				}
				routeCache.Add(new Point(endX, endY));
			}
			else {
				int endX, endY;

				if (EndPoint.Y < endShape.VerticalCenter)
					endY = endShape.Top;
				else
					endY = endShape.Bottom;

				if (EndPoint.X >= endShape.Left &&
					EndPoint.X <= endShape.Right)
				{
					endX = EndPoint.X;
				}
				else {
					endX = endShape.HorizontalCenter;
					routeCache.Add(new Point(endX, EndPoint.Y));
				}
				routeCache.Add(new Point(endX, endY));
			}
		}

		protected internal sealed override void TrySelect(RectangleF container)
		{
			if (Picked(container))
				IsSelected = true;
		}

		protected internal sealed override void Offset(Size offset)
		{
			// Do nothing
		}

		protected internal override Size GetMaximalOffset(Size offset, Rectangle frame)
		{
			if (!IsSelected && !startShape.IsSelected && !endShape.IsSelected)
				return offset;

			foreach (BendPoint bendPoint in bendPoints)
			{
				if (IsSelected || (bendPoint.RelativeToStartShape && startShape.IsSelected) ||
					(!bendPoint.RelativeToStartShape && endShape.IsSelected))
				{
					Point newLocation = bendPoint.Location + offset;

					if (newLocation.X < frame.Left)
						offset.Width += (frame.Left - newLocation.X);
					if (newLocation.X > frame.Right)
						offset.Width -= (newLocation.X - frame.Right);
					if (newLocation.Y < frame.Top)
						offset.Height += (frame.Top - newLocation.Y);
					if (newLocation.Y > frame.Bottom)
						offset.Height -= (newLocation.Y - frame.Bottom);
				}
			}
			return offset;
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

		private BendPoint GetBendPoint(PointF mouseLocation)
		{
			foreach (BendPoint point in bendPoints) {
				if (point.Contains(mouseLocation, Zoom))
					return point;
			}
			return null;
		}

		private bool BendPointPressed(PointF mouseLocation)
		{
			BendPoint point = GetBendPoint(mouseLocation);

			selectedBendPoint = point;
			if (point != null) {
				if (point.AutoPosition) {
					point.AutoPosition = false;
					IsDirty = true;
					Reroute();
					OnChanged(EventArgs.Empty);
				}
				return true;
			}
			else {
				return false;
			}
		}

		private bool BendPointDoubleClicked(PointF mouseLocation)
		{
			BendPoint point = GetBendPoint(mouseLocation);

			if (point != null) {
				if (!point.AutoPosition) {
					if (point == StartPoint && !bendPoints.SecondValue.RelativeToStartShape ||
						point == EndPoint && bendPoints.SecondLastValue.RelativeToStartShape)
					{
						point.AutoPosition = true;
					}
					else {
						bendPoints.Remove(point);
					}
					IsDirty = true;
					Reroute();
					OnChanged(EventArgs.Empty);
				}
				return true;
			}
			else {
				return false;
			}
		}

		private bool Picked(PointF mouseLocation)
		{
			float Tolerance = PickTolerance / Zoom;

			for (int i = 0; i < routeCache.Count - 1; i++) {
				float x = mouseLocation.X;
				float y = mouseLocation.Y;
				float x1 = routeCache[i].X;
				float y1 = routeCache[i].Y;
				float x2 = routeCache[i + 1].X;
				float y2 = routeCache[i + 1].Y;

				if (x1 == x2) {
					if ((x >= x1 - Tolerance) && (x <= x1 + Tolerance) &&
					    (y >= y1 && y <= y2 || y >= y2 && y <= y1))
					{
						return true;
					}
				}
				else { // y1 == y2
					if ((y >= y1 - Tolerance) && (y <= y1 + Tolerance) &&
					    (x >= x1 && x <= x2 || x >= x2 && x <= x1))
					{
						return true;
					}
				}
			}

			return false;
		}

		private bool Picked(RectangleF rectangle)
		{
			float Tolerance = PickTolerance / Zoom;

			for (int i = 0; i < routeCache.Count - 1; i++) {
				if (rectangle.Contains(routeCache[i]) || rectangle.Contains(routeCache[i + 1]))
					return true;

				float x1 = routeCache[i].X;
				float y1 = routeCache[i].Y;
				float x2 = routeCache[i + 1].X;
				float y2 = routeCache[i + 1].Y;

				if (x1 == x2) {
					if (x1 >= rectangle.Left && x1 <= rectangle.Right && (
						y1 < rectangle.Top && y2 > rectangle.Bottom ||
					    y2 < rectangle.Top && y1 > rectangle.Bottom))
					{
						return true;
					}
				}
				else { // y1 == y2
					if (y1 >= rectangle.Top && y1 <= rectangle.Bottom && (
						x1 < rectangle.Left && x2 > rectangle.Right ||
					    x2 < rectangle.Left && x1 > rectangle.Right))
					{
						return true;
					}
				}
			}

			return false;
		}

		internal override bool MousePressed(AbsoluteMouseEventArgs e)
		{
			bool pressed = Picked(e.Location);
				
			if (e.Button == MouseButtons.Left)
				pressed |= (IsSelected && BendPointPressed(e.Location));

			if (pressed)
				OnMouseDown(e);

			return pressed;
		}

		internal override bool MouseMoved(AbsoluteMouseEventArgs e)
		{
			bool moved = IsMousePressed;

			if (moved)
				OnMouseMove(e);

			return moved;
		}

		internal override bool MouseUpped(AbsoluteMouseEventArgs e)
		{
			bool upped = IsMousePressed;

			if (upped)
				OnMouseUp(e);

			return upped;
		}

		internal override void MouseLeaved()
		{
			// Keep it blank
		}

		internal override bool DoubleClicked(AbsoluteMouseEventArgs e)
		{
			bool doubleClicked = Picked(e.Location);
			
			if (e.Button == MouseButtons.Left)
				doubleClicked |= (IsSelected && BendPointDoubleClicked(e.Location));

			if (doubleClicked)
				OnDoubleClick(e);

			return doubleClicked;
		}

		protected override void OnMouseDown(AbsoluteMouseEventArgs e)
		{
			base.OnMouseDown(e);
			copied = false;
		}

		protected override void OnMouseMove(AbsoluteMouseEventArgs e)
		{
			base.OnMouseMove(e);

			if (e.Button == MouseButtons.Left && selectedBendPoint != null) {
				Point newLocation = Point.Truncate(e.Location);

				if (selectedBendPoint.Location != newLocation) {
					if (!copied && Control.ModifierKeys == Keys.Control) {
						BendPoint newPoint = (BendPoint) selectedBendPoint.Clone();
						newPoint.Location = newLocation;
						if (selectedBendPoint.RelativeToStartShape)
							bendPoints.AddAfter(bendPoints.Find(selectedBendPoint), newPoint);
						else
							bendPoints.AddBefore(bendPoints.Find(selectedBendPoint), newPoint);
						selectedBendPoint = newPoint;
						copied = true;
					}
					else {
						selectedBendPoint.Location = newLocation;
					}
					
					if (Settings.UsePrecisionSnapping && Control.ModifierKeys != Keys.Shift)
						SnapActiveBendPoint();

					IsDirty = true;
					Reroute();
					OnChanged(EventArgs.Empty);
				}
			}
		}

		private void SnapActiveBendPoint()
		{
			foreach (BendPoint bendPoint in bendPoints) {
				if (bendPoint != selectedBendPoint) {
					if (Math.Abs(bendPoint.X - selectedBendPoint.X) <= PrecisionSize)
						selectedBendPoint.X = bendPoint.X;
					if (Math.Abs(bendPoint.Y - selectedBendPoint.Y) <= PrecisionSize)
						selectedBendPoint.Y = bendPoint.Y;
				}
			}
		}

		protected override void OnMouseUp(AbsoluteMouseEventArgs e)
		{
			base.OnMouseUp(e);
			selectedBendPoint = null;
		}

		protected virtual void OnChanged(EventArgs e)
		{
			if (Changed != null)
				Changed(this, e);
		}

		protected internal override IEnumerable<ToolStripItem> GetContextMenuItems(IDiagram diagram)
		{
			return ConnectionContextMenu.Default.GetMenuItems(diagram);
		}
        public Connection(SerializationInfo info, StreamingContext ctxt) : base(info,ctxt)
        {
            // New file format
            startOrientation = (LineOrientation)info.GetValue("_StartOrientation",
                typeof(LineOrientation));
            endOrientation = (LineOrientation)info.GetValue("_EndOrientation",
                typeof(LineOrientation));
            bendPoints.Clear();
            startShape = (NClass.Core.BinarySerializationHelper.diagram as DiagramControl).GetShape(
                NClass.Core.BinarySerializationHelper.first_entity);
            endShape = (NClass.Core.BinarySerializationHelper.diagram as DiagramControl).GetShape(
                NClass.Core.BinarySerializationHelper.second_entity); 
            int numpoints = info.GetInt32("_numBends");
            for (int i=0; i<numpoints; i++)
            {
                BendPoint point = (BendPoint) info.GetValue("_point"+i,typeof(BendPoint));
                bendPoints.Add(point);
            }
            if (bendPoints.Count == 0 || !StartPoint.RelativeToStartShape)
                bendPoints.AddFirst(new BendPoint(startShape, true));
            if (EndPoint.RelativeToStartShape)
                bendPoints.Add(new BendPoint(endShape, false));

            startShape.Move += ShapeMoving;
            startShape.Resize += StartShapeResizing;
            endShape.Move += ShapeMoving;
            endShape.Resize += EndShapeResizing;

            /*relation.Detaching += delegate
            {
                startShape.Move -= ShapeMoving;
                startShape.Resize -= StartShapeResizing;
                endShape.Move -= ShapeMoving;
                endShape.Resize -= EndShapeResizing;
            };
            relation.my_connection = this;*/
            Reroute();
        }
        public override void GetObjectData(SerializationInfo info, StreamingContext ctxt)
        {
            info.AddValue("_StartOrientation", startOrientation);
            info.AddValue("_EndOrientation", endOrientation);
            int i = 0;
            foreach (BendPoint point in bendPoints)
            {
                if (!point.AutoPosition)
                {
                    i++;
                }
            }
            info.AddValue("_numBends", i);
            i = 0;
            foreach (BendPoint point in bendPoints)
            {
                if (!point.AutoPosition)
                {
                    info.AddValue("_point"+i, point);
                    i++;
                }
            }
            base.GetObjectData(info, ctxt);
        }
        protected virtual void OnSerializing(SerializeEventArgsBinary e)
        {
            //e.Info.AddValue("_StartOrientation", startOrientation);
            //e.Info.AddValue("_EndOrientation", endOrientation);
            e.Bformatter.Serialize(e.Stream, startOrientation);
            e.Bformatter.Serialize(e.Stream, endOrientation);
            int i = 0;
            foreach (BendPoint point in bendPoints)
            {
                if (!point.AutoPosition)
                {
                    i++;
                }
            }
            //e.Info.AddValue("_numBends", i);
            e.Bformatter.Serialize(e.Stream,i);
            i = 0;
            foreach (BendPoint point in bendPoints)
            {
                if (!point.AutoPosition)
                {
                    //e.Info.AddValue("_point" + i, point);
                    e.Bformatter.Serialize(e.Stream,point);
                    i++;
                }
            }
        }

        protected virtual void OnDeserializing(SerializeEventArgsBinary e)
        {
            // New file format
            startOrientation = (LineOrientation)e.Bformatter.Deserialize(e.Stream); 
            endOrientation = (LineOrientation)e.Bformatter.Deserialize(e.Stream);
            /*startOrientation = (LineOrientation)e.Info.GetValue("_StartOrientation",
                typeof(LineOrientation));
            endOrientation = (LineOrientation)e.Info.GetValue("_EndOrientation",
                typeof(LineOrientation));*/
            bendPoints.Clear();
            //int numpoints = e.Info.GetInt32("_numBends");
            int numpoints = (int) e.Bformatter.Deserialize(e.Stream);
            for (int i = 0; i < numpoints; i++)
            {
                BendPoint point = (BendPoint)e.Bformatter.Deserialize(e.Stream);
                    //(BendPoint)e.Info.GetValue("_point" + i, typeof(BendPoint));
                bendPoints.Add(point);
            }
            if (bendPoints.Count == 0 || !StartPoint.RelativeToStartShape)
                bendPoints.AddFirst(new BendPoint(startShape, true));
            if (EndPoint.RelativeToStartShape)
                bendPoints.Add(new BendPoint(endShape, false));
            Reroute();
        }
        protected virtual void OnSerializing(SerializeEventArgs e)
		{
			XmlDocument document = e.Node.OwnerDocument;

			XmlElement startNode = document.CreateElement("StartOrientation");
			startNode.InnerText = startOrientation.ToString();
			e.Node.AppendChild(startNode);

			XmlElement endNode = document.CreateElement("EndOrientation");
			endNode.InnerText = endOrientation.ToString();
			e.Node.AppendChild(endNode);

			foreach (BendPoint point in bendPoints) {
				if (!point.AutoPosition) {
					XmlElement node = document.CreateElement("BendPoint");
					node.SetAttribute("relativeToStartShape", point.RelativeToStartShape.ToString());
					point.Serialize(node);
					e.Node.AppendChild(node);
				}
			}
		}

		protected virtual void OnDeserializing(SerializeEventArgs e)
		{
			// Old file format
			XmlElement oldStartNode = e.Node["StartNode"];
			XmlElement oldEndNode = e.Node["EndNode"];
			if (oldStartNode != null && oldEndNode != null) {
				bool isHorizontal;
				bool.TryParse(oldStartNode.GetAttribute("isHorizontal"), out isHorizontal);
				startOrientation = (isHorizontal) ? LineOrientation.Horizontal : LineOrientation.Vertical;
				bool.TryParse(oldEndNode.GetAttribute("isHorizontal"), out isHorizontal);
				endOrientation = (isHorizontal) ? LineOrientation.Horizontal : LineOrientation.Vertical;

				int startLocation, endLocation;
				int.TryParse(oldStartNode.GetAttribute("location"), out startLocation);
				int.TryParse(oldEndNode.GetAttribute("location"), out endLocation);

				Reroute();
				if (startOrientation == LineOrientation.Vertical)
					StartPoint.X = startShape.Left + startLocation;
				else
					StartPoint.Y = startShape.Top + startLocation;

				if (endOrientation == LineOrientation.Vertical)
					EndPoint.X = endShape.Left + endLocation;
				else
					EndPoint.Y = endShape.Top + endLocation;

				StartPoint.AutoPosition = false;
				EndPoint.AutoPosition = false;
				Reroute();
			}
			else {
				// New file format
				XmlElement startNode = e.Node["StartOrientation"];
				if (startNode != null) {
					if (startNode.InnerText == "Horizontal")
						startOrientation = LineOrientation.Horizontal;
					else
						startOrientation = LineOrientation.Vertical;
				}
				XmlElement endNode = e.Node["EndOrientation"];
				if (endNode != null) {
					if (endNode.InnerText == "Horizontal")
						endOrientation = LineOrientation.Horizontal;
					else
						endOrientation = LineOrientation.Vertical;
				}

				if (startNode != null && endNode != null) { // To be ensured it's the new file format
					bendPoints.Clear();

					XmlNodeList nodes = e.Node.SelectNodes("child::BendPoint");
					foreach (XmlElement node in nodes) {
						bool relativeToStartShape;
						bool.TryParse(node.GetAttribute("relativeToStartShape"), out relativeToStartShape);
						DiagramShape relativeShape = relativeToStartShape ? startShape : endShape;

						BendPoint point = new BendPoint(relativeShape, relativeToStartShape, false);
						point.Deserialize(node);
						bendPoints.Add(point);
					}
					if (bendPoints.Count == 0 || !StartPoint.RelativeToStartShape)
						bendPoints.AddFirst(new BendPoint(startShape, true));
					if (EndPoint.RelativeToStartShape)
						bendPoints.Add(new BendPoint(endShape, false));
				}
				Reroute();
			}
		}

		public override string ToString()
		{
			return Relation.ToString();
		}
	}
}
