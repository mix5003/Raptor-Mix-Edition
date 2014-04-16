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
using System.Drawing.Text;
using System.Drawing.Imaging;
using System.Drawing.Drawing2D;
using System.Collections.Generic;
using System.ComponentModel;
using System.Windows.Forms;
using NClass.Core;
using NClass.Translations;

namespace NClass.GUI.Diagram
{
	public sealed partial class DiagramControl : UserControl, IDiagram, IPrintableDiagram
	{
		private enum State
		{
			Normal,
			Selectioning,
			CreatingRelationship,
			Dragging
		}

		private enum Relationship
		{
			Association,
			Aggregation,
			Composition,
			Dependency,
			Generalization,
			Nesting,
			Realization,
			Comment
		}

		public const float MinZoom = 0.1F;
		public const float MaxZoom = 4.0F;

		const int PrecisionSize = 10;
		const int DiagramPadding = 10;
		static Size diagramSize = new Size(Settings.DiagramWidth, Settings.DiagramHeight);
		static Pen diagramBorderPen;

		Project project;
        OrderedList<DiagramShape> shapes = new OrderedList<DiagramShape>();
        OrderedList<Connection> connections = new OrderedList<Connection>();
        List<DiagramShape> selectedShapes = new List<DiagramShape>();
        List<Connection> selectedConnections = new List<Connection>();
        DiagramClipboard clipboard = new DiagramClipboard();

        State state = State.Normal;
        Relationship newRelationship;
        DiagramShape startShape = null;
        DiagramShape endShape = null;

		float zoom = 1.0F;
        Point oldMouseLocation = Point.Empty;
        Point selectionStart = Point.Empty;
        bool selectionChanged = false;
        DiagramElement firstSelectedElement = null;
        Intersector<ToolStripItem> intersector = new Intersector<ToolStripItem>();

        public event EventHandler ZoomChanged;
        public event EventHandler SelectionChanged;

		static DiagramControl()
		{
			diagramBorderPen = new Pen(Color.FromArgb(128, Color.Black));
			diagramBorderPen.DashPattern = new float[] { 5, 5 };
		}

		/// <exception cref="ArgumentNullException">
		/// <paramref name="project"/> is null.
		/// </exception>
		public DiagramControl(Project project)
		{
			if (project == null)
				throw new ArgumentNullException("project");

			this.project = project;
			project.Cleared += new EventHandler(project_Cleared);
			project.ContentChanged += new EventHandler(project_ContentChanged);
			project.EntityAdded += new EntityEventHandler(project_EntityAdded);
			project.EntityRemoved += new EntityEventHandler(project_EntityRemoved);
			project.RelationAdded += new RelationEventHandler(project_RelationAdded);
			project.RelationRemoved += new RelationEventHandler(project_RelationRemoved);
			project.Deserializing += new SerializeEventHandler(project_Deserializing);
			Strings.LanguageChanged += new EventHandler(Strings_LanguageChanged);

			InitializeComponent();
			UpdateContextMenuTexts();
			this.SetStyle(ControlStyles.UserPaint, true);
			this.DoubleBuffered = true;
			BackColor = Style.CurrentStyle.DiagramBackColor;
			AutoScrollMinSize = diagramSize;

			DiagramShape.graphics = this.CreateGraphics();
		}

		public Project Project
		{
			get { return project; }
		}

		[DefaultValue(1.0F)]
		public float Zoom
		{
			get
			{
				return zoom;
			}
			private set
			{
				if (value < MinZoom) value = MinZoom;
				if (value > MaxZoom) value = MaxZoom;

				if (zoom != value) {
					zoom = value;
					DiagramElement.Zoom = value;
					diagramBorderPen.Width = 1 / Zoom;
					UpdateWorkspaceSize();
					RefreshDiagram();

					if (ZoomChanged != null)
						ZoomChanged(this, EventArgs.Empty);
				}
			}
		}

		public int ZoomPercentage
		{
			get
			{
				return (int) Math.Round(Zoom * 100);
			}
			set
			{
				float zoom = (float) value / 100;
				ChangeZoom(zoom);
			}
		}

		public Size DiagramSize
		{
			get
			{
				return diagramSize;
			}
			set
			{
				Size minSize = GetMinimumDiagramSize();

				if (value.Width < minSize.Width)  value.Width = minSize.Width;
				if (value.Height < minSize.Height) value.Height = minSize.Height;

				if (DiagramSize != value) {
					diagramSize = value;
					Settings.DiagramWidth = diagramSize.Width;
					Settings.DiagramHeight = diagramSize.Height;
					UpdateWorkspaceSize();
					RefreshDiagram();
				}
			}
		}

		private Point DisplayPosition
		{
			get
			{
				return new Point(HorizontalScroll.Value, VerticalScroll.Value);
			}
			set
			{
				AutoScrollPosition = value;
			}
		}

		private PointF AbsoluteDisplayPosition
		{
			get
			{
				float horizontal = (HorizontalScroll.Value / Zoom);
				float vertical = (VerticalScroll.Value / Zoom);
				PointF position = new PointF(horizontal, vertical);

				return position;
			}
		}

		private SizeF AbsoluteDisplayOffset
		{
			get
			{
				float horizontal = (HorizontalScroll.Value / Zoom);
				float vertical = (VerticalScroll.Value / Zoom);
				SizeF offset = new SizeF(horizontal, vertical);

				return offset;
			}
		}

		private SizeF AbsoluteDisplaySize
		{
			get
			{
				return new SizeF(Width / Zoom, Height / Zoom);
			}
		}

		private RectangleF AbsoluteDisplayRectangle
		{
			get
			{
				return new RectangleF(AbsoluteDisplayPosition, AbsoluteDisplaySize);
			}
		}

		private PointF AbsoluteCenterPoint
		{
			get
			{
				return new PointF(
					((float) Width / 2 + HorizontalScroll.Value) / Zoom,
					((float) Height / 2 + VerticalScroll.Value) / Zoom
				);
			}
		}

		public DiagramElement FirstSelectedElement
		{
			get { return firstSelectedElement; }
		}

		public IEnumerable<DiagramElement> GetElements()
		{
			foreach (DiagramShape shape in shapes)
				yield return shape;
			foreach (Connection connection in connections) {
				yield return connection;
			}
		}

		public IEnumerable<DiagramElement> GetElementsInDisplayOrder()
		{
			foreach (DiagramShape shape in shapes) {
				if (shape.IsSelected)
					yield return shape;
			}
			foreach (Connection connection in connections) {
				yield return connection;
			}
			foreach (DiagramShape shape in shapes) {
				if (!shape.IsSelected)
					yield return shape;
			}
		}

		public IEnumerable<DiagramElement> GetElementsInReversedDisplayOrder()
		{
			foreach (DiagramShape shape in shapes.GetReversedList()) {
				if (!shape.IsSelected)
					yield return shape;
			}
			foreach (Connection connection in connections.GetReversedList()) {
				yield return connection;
			}
			foreach (DiagramShape shape in shapes.GetReversedList()) {
				if (shape.IsSelected)
					yield return shape;
			}
		}

		public int ElementsCount
		{
			get { return shapes.Count + connections.Count; }
		}

		public bool IsEmpty
		{
			get { return (ElementsCount == 0); }
		}

		public IEnumerable<DiagramElement> GetSelectedElements()
		{
			for (int i = 0; i < selectedShapes.Count; i++)
				yield return selectedShapes[i];
			for (int i = 0; i < selectedConnections.Count; i++)
				yield return selectedConnections[i];
		}

		public IEnumerable<DiagramShape> SelectedShapes
		{
			get { return selectedShapes; }
		}

		public IEnumerable<Connection> SelectedConnections
		{
			get { return selectedConnections; }
		}

		public int SelectedElementCount
		{
			get { return SelectedShapeCount + SelectedConnectionCount; }
		}

		public int SelectedShapeCount
		{
			get { return selectedShapes.Count; }
		}

		public int SelectedConnectionCount
		{
			get { return selectedConnections.Count; }
		}

		public bool HasSelectedElement
		{
			get { return (SelectedElementCount > 0); }
		}

		public bool HasSelectedShape
		{
			get { return (SelectedShapeCount > 0); }
		}

		public bool HasSelectedRelationship
		{
			get { return (SelectedConnectionCount > 0); }
		}

		public bool SingleSelection
		{
			get { return SelectedElementCount == 1; }
		}

		public bool MultipleSelection
		{
			get { return SelectedElementCount >= 2; }
		}

		public void RefreshDiagram()
		{
			if (BackColor != Style.CurrentStyle.DiagramBackColor)
				BackColor = Style.CurrentStyle.DiagramBackColor;
			else
				Invalidate();
		}

		private void UpdateWorkspaceSize()
		{
			AutoScrollMinSize = new Size(
				(int) (DiagramSize.Width * Zoom),
				(int) (DiagramSize.Height * Zoom)
			);
		}

		public Size GetMinimumDiagramSize()
		{
			int maxRight = DiagramPadding * 2;
			int maxBottom = DiagramPadding * 2;

			foreach (DiagramElement element in GetElements()) {
				Rectangle logicalArea = element.GetLogicalArea();

				if (maxRight < logicalArea.Right + DiagramPadding)
					maxRight = logicalArea.Right + DiagramPadding;
				if (maxBottom < logicalArea.Bottom + DiagramPadding)
					maxBottom = logicalArea.Bottom + DiagramPadding;
			}

			return new Size(maxRight, maxBottom);
		}

		private void RedrawDirtyElements()
		{
			Region dirtyArea = new Region(Rectangle.Empty);
			RectangleF screen = AbsoluteDisplayRectangle;

			foreach (DiagramElement element in GetElements()) {
				if (element.IsDirty) {
					RectangleF area = element.GetDirtyArea();
					if (screen.IntersectsWith(area))
						dirtyArea.Union(area);
				}
			}

			Matrix transformation = new Matrix();
			transformation.Translate(-HorizontalScroll.Value, -VerticalScroll.Value);
			transformation.Scale(Zoom, Zoom);
			dirtyArea.Transform(transformation);

			Invalidate(dirtyArea);
			dirtyArea.Dispose();
		}

		private SizeF GetSelectionOffsetFromCenter()
		{
			RectangleF selectedElementsArea = GetPrintingArea(true);
			PointF selectionCenter = new PointF(
				selectedElementsArea.X + selectedElementsArea.Width / 2,
				selectedElementsArea.Y + selectedElementsArea.Height / 2
			);
			PointF screenCenter = AbsoluteCenterPoint;
			SizeF offset = new SizeF(
				selectionCenter.X - screenCenter.X,
				selectionCenter.Y - screenCenter.Y
			);

			return offset;
		}

		public void ChangeZoom(bool enlarge)
		{
			if (HasSelectedElement)
				ChangeZoom(enlarge, AbsoluteCenterPoint, GetSelectionOffsetFromCenter());
			else
				ChangeZoom(enlarge, AbsoluteCenterPoint);
		}

		public void ChangeZoom(bool enlarge, PointF zoomingCenter)
		{
			ChangeZoom(enlarge, zoomingCenter, Size.Empty);
		}

		private void ChangeZoom(bool enlarge, PointF zoomingCenter, SizeF additionalOffset)
		{
			float zoomValue;

			if (enlarge)
				zoomValue = ((ZoomPercentage + 10) / 10) / 10F;
			else
				zoomValue = ((ZoomPercentage - 1) / 10) / 10F;

			ChangeZoom(zoomValue, zoomingCenter, additionalOffset);
		}

		public void ChangeZoom(float zoom)
		{
			if (HasSelectedElement)
				ChangeZoom(zoom, AbsoluteCenterPoint, GetSelectionOffsetFromCenter());
			else
				ChangeZoom(zoom, AbsoluteCenterPoint);
		}

		public void ChangeZoom(float zoomValue, PointF zoomingCenter)
		{
			ChangeZoom(zoomValue, zoomingCenter, Size.Empty);
		}

		private void ChangeZoom(float zoomValue, PointF zoomingCenter, SizeF additionalOffset)
		{
			Point oldLocation = ConvertAbsoluteToRelative(zoomingCenter);
			Zoom = zoomValue;
			Point newLocation = ConvertAbsoluteToRelative(zoomingCenter);

			Size offset = new Size(newLocation.X - oldLocation.X, newLocation.Y - oldLocation.Y);
			offset += new Size(
				(int) (additionalOffset.Width * Zoom),
				(int) (additionalOffset.Height *= Zoom)
			);

			DisplayPosition += offset;
		}

		public void AutoZoom()
		{
			AutoZoom(true);
		}

		public void AutoZoom(bool selectedOnly)
		{
			const int Margin = DiagramShape.SelectionMargin;

			Rectangle visibleRectangle = this.ClientRectangle;
			Rectangle diagramRectangle = this.GetPrintingArea(selectedOnly && HasSelectedElement);

			visibleRectangle.X += Margin;
			visibleRectangle.Y += Margin;
			visibleRectangle.Width -= Margin * 2;
			visibleRectangle.Height -= Margin * 2;

			float scaleX = (float) visibleRectangle.Width / diagramRectangle.Width;
			float scaleY = (float) visibleRectangle.Height / diagramRectangle.Height;
			float scale = Math.Min(scaleX, scaleY);

			Zoom = scale;

			int offsetX = (int) ((visibleRectangle.Width - diagramRectangle.Width * Zoom) / 2);
			int offsetY = (int) ((visibleRectangle.Height - diagramRectangle.Height * Zoom) / 2);
			DisplayPosition = new Point(
				(int) (diagramRectangle.X * Zoom) - Margin - offsetX,
				(int) (diagramRectangle.Y * Zoom) - Margin - offsetY);
		}

		private PointF ConvertRelativeToAbsolute(Point location)
		{
			float newX = (location.X + HorizontalScroll.Value) / Zoom;
			float newY = (location.Y + VerticalScroll.Value) / Zoom;
			return new PointF(newX, newY);
		}

		private RectangleF ConvertRelativeToAbsolute(Rectangle rectangle)
		{
			return new RectangleF(
				(rectangle.X + HorizontalScroll.Value) / Zoom,
				(rectangle.Y + VerticalScroll.Value) / Zoom,
				rectangle.Width / Zoom, rectangle.Height / Zoom
			);
		}


		private Point ConvertAbsoluteToRelative(PointF location)
		{
			int newX = (int) (location.X * Zoom - HorizontalScroll.Value);
			int newY = (int) (location.Y * Zoom - VerticalScroll.Value);
			return new Point(newX, newY);
		}

		private void MoveOtherSelectedShapes(DiagramShape shape, Size offset)
		{
			if (!offset.IsEmpty) {
				foreach (DiagramShape otherShape in selectedShapes) {
					if (otherShape != shape)
						otherShape.Location += offset;
				}
			}
		}

		private void ValidateShapePositions(DiagramShape first)
		{
			// Align to other shapes
			bool cancelAlignment =
				(Control.ModifierKeys == Keys.Shift || !Settings.UsePrecisionSnapping);
			RectangleF absoluteClip = AbsoluteDisplayRectangle;

			if (!cancelAlignment) {
				Size offset = Size.Empty;

				//TODO: szebben
				foreach (DiagramShape otherShape in shapes) {
					Point oldLocation = first.Location;
					bool visible = absoluteClip.IntersectsWith(otherShape.BorderRectangle);

					if (visible && TryAlignToOtherShape(first, otherShape)) {
						Point newLocation = first.Location;
						offset += new Size(
							newLocation.X - oldLocation.X, newLocation.Y - oldLocation.Y);
					}
				}
				MoveOtherSelectedShapes(first, offset); //TODO: ha lehet, ne használjam
			}
		}

		private bool TryAlignToOtherShape(DiagramShape shape, DiagramShape otherShape)
		{
			if (shape == otherShape || otherShape.IsSelected)
				return false;

			bool moved = false;

			if (Math.Abs(shape.Left - otherShape.Left) <= PrecisionSize) {
				shape.Left = otherShape.Left;
				moved = true;
			}
			if (Math.Abs(shape.Top - otherShape.Top) <= PrecisionSize) {
				shape.Top = otherShape.Top;
				moved = true;
			}

			return moved;
		}

		public void SelectAll()
		{
			foreach (DiagramElement element in GetElements())
				element.IsSelected = true;

			if (selectionChanged) {
				OnSelectionChanged(EventArgs.Empty);
				RedrawDirtyElements();
			}
		}

		private void DeselectAll()
		{
			foreach (DiagramElement element in GetElements())
				element.IsSelected = false;
		}

		private void DeselectAllOthers(DiagramElement onlySelected)
		{
			foreach (DiagramElement element in GetElements()) {
				if (element != onlySelected)
					element.IsSelected = false;
			}
		}

		private Rectangle GetPaddingFrame()
		{
			return new Rectangle(
				DiagramPadding, DiagramPadding,
				DiagramSize.Width - DiagramPadding, DiagramSize.Height - DiagramPadding
			);
		}

		private void element_Dragging(object sender, MoveEventArgs e)
		{
			Size offset = e.Offset;

			Rectangle frame = GetPaddingFrame();
			foreach (DiagramElement element in GetElements())
				offset = element.GetMaximalOffset(offset, frame);

			if (!offset.IsEmpty) {
				foreach (DiagramElement element in GetSelectedElements())
					element.Offset(offset);
			}
		}

		private void shape_SelectionChanged(object sender, EventArgs e)
		{
			DiagramShape shape = (DiagramShape) sender;

			if (shape.IsSelected) {
				selectedShapes.Add(shape);
				selectionChanged = true;
				if (firstSelectedElement == null)
					firstSelectedElement = shape;
			}
			else {
				selectedShapes.Remove(shape);
				selectionChanged = true;
				if (shape == firstSelectedElement) {
					if (selectedShapes.Count > 0)
						firstSelectedElement = selectedShapes[0];
					else if (selectedConnections.Count > 0)
						firstSelectedElement = selectedConnections[0];
					else
						firstSelectedElement = null;
				}
			}
		}

		private void connection_SelectionChanged(object sender, EventArgs e)
		{
			Connection connection = (Connection) sender;

			if (connection.IsSelected) {
				selectedConnections.Add(connection);
				selectionChanged = true;
				if (firstSelectedElement == null)
					firstSelectedElement = connection;
			}
			else {
				selectedConnections.Remove(connection);
				selectionChanged = true;
				if (connection == firstSelectedElement) {
					if (selectedConnections.Count > 0)
						firstSelectedElement = selectedConnections[0];
					else if (selectedShapes.Count > 0)
						firstSelectedElement = selectedShapes[0];
					else
						firstSelectedElement = null;
				}
			}
		}

		private bool ConfirmDelete()
		{
			DialogResult result = MessageBox.Show(
				Strings.GetString("delete_confirmation"), Strings.GetString("confirmation"),
				MessageBoxButtons.YesNo, MessageBoxIcon.Warning);

			return result == DialogResult.Yes;
		}

		public void DeleteSelectedElements()
		{
			if (HasSelectedElement && ConfirmDelete())
			{
				if (selectedShapes.Count > 0) {
					foreach (DiagramShape shape in selectedShapes)
						project.RemoveEntity(shape.Entity);
					selectedShapes.Clear();
					selectionChanged = true;
				}
				if (selectedConnections.Count > 0) {
					foreach (Connection connection in selectedConnections)
						project.RemoveRelation(connection.Relation);
					selectedConnections.Clear();
					selectionChanged = true;
				}

				firstSelectedElement = null;
				if (selectionChanged)
					OnSelectionChanged(EventArgs.Empty);
			}
		}

		//TODO: átdolgozni!
		private void CreateRelationship(PointF point)
		{
			DiagramShape clickedShape = null;

			foreach (DiagramShape shape in shapes) {
				if (shape.Contains(point)) {
					clickedShape = shape;
					break;
				}
			}
			if (clickedShape != null) {
				if (startShape == null)
					startShape = clickedShape;
				else
					endShape = clickedShape;
			}

			if (startShape != null && endShape != null) {
				try {
					if (newRelationship == Relationship.Association) {
						project.AddAssociationRelation(
							startShape.Entity as TypeBase, endShape.Entity as TypeBase);
					}
					else if (newRelationship == Relationship.Composition) {
						project.AddCompositionRelation(
							startShape.Entity as TypeBase, endShape.Entity as TypeBase);
					}
					else if (newRelationship == Relationship.Aggregation) {
						project.AddAggregationRelation(
							startShape.Entity as TypeBase, endShape.Entity as TypeBase);
					}
					else if (newRelationship == Relationship.Generalization) {
						project.AddGeneralizationRelation(
							startShape.Entity as CompositeType, endShape.Entity as CompositeType);
					}
					else if (newRelationship == Relationship.Realization) {
						project.AddRealizationRelation(
							startShape.Entity as TypeBase, endShape.Entity as InterfaceType);
					}
					else if (newRelationship == Relationship.Dependency) {
						project.AddDependencyRelation(
							startShape.Entity as TypeBase, endShape.Entity as TypeBase);
					}
					else if (newRelationship == Relationship.Nesting) {
						project.AddNestingRelation(
							startShape.Entity as CompositeType, endShape.Entity as TypeBase);
					}
					else if (newRelationship == Relationship.Comment) {
						if (startShape.Entity is Comment) {
							project.AddCommentRelation(
								startShape.Entity as Comment, endShape.Entity);
						}
						else {
							project.AddCommentRelation(
								endShape.Entity as Comment, startShape.Entity);
						}
					}
				}
				catch (ArgumentNullException) {
					MessageBox.Show(
						Strings.GetString("error_cannot_create_relation",
						Strings.GetString("error"), MessageBoxButtons.OK,
						MessageBoxIcon.Error));
				}
				catch (RelationException ex) {
					MessageBox.Show(ex.Message, Strings.GetString("error"),
						MessageBoxButtons.OK, MessageBoxIcon.Error);
				}
				startShape = null;
				endShape = null;
				state = State.Normal;
			}
		}

		public void CreateNewAssociation()
		{
			newRelationship = Relationship.Association;
			state = State.CreatingRelationship;
		}

		public void CreateNewComposition()
		{
			newRelationship = Relationship.Composition;
			state = State.CreatingRelationship;
		}

		public void CreateNewAggregation()
		{
			newRelationship = Relationship.Aggregation;
			state = State.CreatingRelationship;
		}

		public void CreateNewGeneralization()
		{
			newRelationship = Relationship.Generalization;
			state = State.CreatingRelationship;
		}

		public void CreateNewRealization()
		{
			newRelationship = Relationship.Realization;
			state = State.CreatingRelationship;
		}

		public void CreateNewDependency()
		{
			newRelationship = Relationship.Dependency;
			state = State.CreatingRelationship;
		}

		public void CreateNewNesting()
		{
			newRelationship = Relationship.Nesting;
			state = State.CreatingRelationship;
		}

		public void CreateNewCommentRelationsip()
		{
			newRelationship = Relationship.Comment;
			state = State.CreatingRelationship;
		}

		public void AlignLeft()
		{
			if (SelectedShapeCount >= 2) {
				int left = DiagramSize.Width;

				foreach (DiagramShape shape in SelectedShapes)
					left = Math.Min(left, shape.Left);
				foreach (DiagramShape shape in SelectedShapes)
					shape.Left = left;
				RefreshDiagram();
			}
		}

		public void AlignRight()
		{
			if (SelectedShapeCount >= 2) {
				int right = 0;

				foreach (DiagramShape shape in SelectedShapes)
					right = Math.Max(right, shape.Right);
				foreach (DiagramShape shape in SelectedShapes)
					shape.Right = right;
				RefreshDiagram();
			}
		}

		public void AlignTop()
		{
			if (SelectedShapeCount >= 2) {
				int top = DiagramSize.Height;

				foreach (DiagramShape shape in SelectedShapes)
					top = Math.Min(top, shape.Top);
				foreach (DiagramShape shape in SelectedShapes)
					shape.Top = top;
				RefreshDiagram();
			}
		}

		public void AlignBottom()
		{
			if (SelectedShapeCount >= 2) {
				int bottom = 0;

				foreach (DiagramShape shape in SelectedShapes)
					bottom = Math.Max(bottom, shape.Bottom);
				foreach (DiagramShape shape in SelectedShapes)
					shape.Bottom = bottom;
				RefreshDiagram();
			}
		}

		public void AlignHorizontal()
		{
			if (SelectedShapeCount >= 2) {
				int center = 0;

				foreach (DiagramShape shape in SelectedShapes)
					center += (shape.Top + shape.Bottom) / 2;
				center /= SelectedShapeCount;

				foreach (DiagramShape shape in SelectedShapes)
					shape.Top = center - shape.Height / 2;
				RefreshDiagram();
			}
		}

		public void AlignVertical()
		{
			if (SelectedShapeCount >= 2) {
				int center = 0;

				foreach (DiagramShape shape in SelectedShapes)
					center += (shape.Left + shape.Right) / 2;
				center /= SelectedShapeCount;

				foreach (DiagramShape shape in SelectedShapes)
					shape.Left = center - shape.Width / 2;
				RefreshDiagram();
			}
		}

		public void AdjustToSameWidth()
		{
			if (SelectedShapeCount >= 2) {
				int maxWidth = 0;

				foreach (DiagramShape shape in SelectedShapes)
					maxWidth = Math.Max(maxWidth, shape.Width);
				foreach (DiagramShape shape in SelectedShapes)
					shape.Width = maxWidth;
				RefreshDiagram();
			}
		}

		public void AdjustToSameHeight()
		{
			if (SelectedShapeCount >= 2) {
				int maxHeight = 0;

				foreach (DiagramShape shape in SelectedShapes)
					maxHeight = Math.Max(maxHeight, shape.Height);
				foreach (DiagramShape shape in SelectedShapes)
					shape.Height = maxHeight;
				RefreshDiagram();
			}
		}

		public void AdjustToSameSize()
		{
			if (SelectedShapeCount >= 2) {
				Size maxSize = Size.Empty;

				foreach (DiagramShape shape in SelectedShapes) {
					maxSize.Width = Math.Max(maxSize.Width, shape.Width);
					maxSize.Height = Math.Max(maxSize.Height, shape.Height);
				}
				foreach (DiagramShape shape in SelectedShapes)
					shape.Size = maxSize;
				RefreshDiagram();
			}
		}

		public void AutoWidthOfSelectedShapes()
		{
			foreach (DiagramShape shape in SelectedShapes)
				shape.AutoWidth();

			RedrawDirtyElements();
		}

		public void AutoHeightOfSelectedShapes()
		{
			foreach (DiagramShape shape in SelectedShapes)
				shape.AutoHeight();
			RedrawDirtyElements();
		}

		public void CollapseAll()
		{
			bool selectedOnly = HasSelectedElement;
			CollapseAll(selectedOnly);
		}

		public void CollapseAll(bool selectedOnly)
		{
			foreach (DiagramShape shape in shapes) {
				if (shape.IsSelected || !selectedOnly)
					shape.Collapse();
			}
			RedrawDirtyElements();
		}

		public void ExpandAll()
		{
			bool selectedOnly = HasSelectedElement;
			ExpandAll(selectedOnly);
		}

		public void ExpandAll(bool selectedOnly)
		{
			foreach (DiagramShape shape in shapes) {
				if (shape.IsSelected || !selectedOnly)
					shape.Expand();
			}
			RedrawDirtyElements();
		}

		protected override void OnMouseDown(MouseEventArgs e)
		{
			base.OnMouseDown(e);

			AbsoluteMouseEventArgs abs_e = new AbsoluteMouseEventArgs(e, AbsoluteDisplayOffset, Zoom);
			if (state == State.CreatingRelationship) {
				CreateRelationship(abs_e.Location);
				if (startShape != null)
					oldMouseLocation = startShape.CenterPoint;
				return;
			}

			if (e.Button == MouseButtons.Middle) {
				oldMouseLocation = e.Location;
				state = State.Dragging;
				return;
			}

			bool multiSelection =
				(Control.ModifierKeys == Keys.Control && e.Button != MouseButtons.Right);
			bool onBlankArea = true;

			foreach (DiagramElement element in GetElementsInDisplayOrder()) {
				bool selectedBefore = element.IsSelected;

				if (element.MousePressed(abs_e)) {
					if (!multiSelection && element.IsSelected && !selectedBefore)
						DeselectAllOthers(element);
					onBlankArea = false;
					break;
				}
			}

			if (onBlankArea) {
				if (!multiSelection)
					DeselectAll();

				if (e.Button == MouseButtons.Left)
					state = State.Selectioning;

				selectionStart = e.Location;
				oldMouseLocation = e.Location;
			}

			if (selectionChanged)
				OnSelectionChanged(EventArgs.Empty);

			SetContextMenu();
			RedrawDirtyElements();
		}

		private void SetContextMenu()
		{
			if (HasSelectedElement) {
				dynamicContextMenu.Items.Clear();
				intersector.ClearSets();
				foreach (DiagramShape shape in SelectedShapes)
					intersector.AddSet(shape.GetContextMenuItems(this));
				foreach (Connection connection in SelectedConnections)
					intersector.AddSet(connection.GetContextMenuItems(this));
				
				foreach (ToolStripItem menuItem in intersector.GetIntersection())
					dynamicContextMenu.Items.Add(menuItem);
				this.ContextMenuStrip = dynamicContextMenu;
			}
			else {
				this.ContextMenuStrip = defaultContextMenu;
			}
		}

		private void DrawSelectionRectangle(Point selectionEnd)
		{
			// Create a rectangle with positive size
			int x = Math.Min(selectionStart.X, selectionEnd.X);
			int y = Math.Min(selectionStart.Y, selectionEnd.Y);
			int width = Math.Abs(selectionEnd.X - selectionStart.X);
			int height = Math.Abs(selectionEnd.Y - selectionStart.Y);

			Rectangle rectangle = new Rectangle(x, y, width, height);

			// Draw the frame in screen coordinates
			if (width > 0 && height > 0) {
				rectangle.Location += (Size) this.PointToScreen(Point.Empty);
				ControlPaint.DrawReversibleFrame(rectangle, this.BackColor, FrameStyle.Dashed);
			}
		}

		private void DragDisplayPosition(Point newMouseLocation)
		{
			// Relative movement
			Size offset = new Size(
				newMouseLocation.X - oldMouseLocation.X,
				newMouseLocation.Y - oldMouseLocation.Y
			);
			
			this.DisplayPosition -= offset;
		}
		
		[Obsolete]
		private void DrawCreatingRelationshipLine(DiagramShape shape, Point point)
		{
			Size offset = (Size) this.PointToScreen(Point.Empty);
			Point centerPoint = shape.CenterPoint + offset;
			point += offset;

			ControlPaint.DrawReversibleLine(centerPoint, point, this.BackColor);
		}

		protected override void OnMouseMove(MouseEventArgs e)
		{
			base.OnMouseMove(e);

			if (state == State.Selectioning) {
				// Clear old selection rectangle
				DrawSelectionRectangle(oldMouseLocation);

				// Draw new selection rectangle
				oldMouseLocation = e.Location;
				if (oldMouseLocation.X < 1)
					oldMouseLocation.X = 1;
				if (oldMouseLocation.X > ClientRectangle.Width - 1)
					oldMouseLocation.X = ClientRectangle.Width - 1;
				if (oldMouseLocation.Y < 1)
					oldMouseLocation.Y = 1;
				if (oldMouseLocation.Y > ClientRectangle.Height - 1)
					oldMouseLocation.Y = ClientRectangle.Height - 1;

				DrawSelectionRectangle(oldMouseLocation);
			}
			else if (state == State.CreatingRelationship && startShape != null) {
				DrawCreatingRelationshipLine(startShape, oldMouseLocation);
				oldMouseLocation = e.Location;
				DrawCreatingRelationshipLine(startShape, e.Location);
			}
			else if (state == State.Dragging) {
				DragDisplayPosition(e.Location);
				oldMouseLocation = e.Location;
			}
			else {
				AbsoluteMouseEventArgs abs_e = new AbsoluteMouseEventArgs(e, AbsoluteDisplayOffset, Zoom);
				Cursor cursor = Cursors.Default; //TODO: nem kell
				bool handled = false;

				foreach (DiagramElement element in GetElementsInDisplayOrder()) {
					if (handled) {
						element.MouseLeaved();
					}
					else {
						handled = element.MouseMoved(abs_e);
						//TODO: ronda!
						if (element is DiagramShape)
							cursor = ((DiagramShape) element).GetCursor(abs_e.Location);
					}
				}

				this.Cursor = cursor; //TODO: nem kell
				if (!handled)
					this.Cursor = Cursors.Default;
				RedrawDirtyElements();
			}
		}

		private void FinishSelectioning(bool selectElements)
		{
			DrawSelectionRectangle(oldMouseLocation);

			if (selectElements) {
				Rectangle selectionRectangle = new Rectangle(
					selectionStart.X, selectionStart.Y,
					oldMouseLocation.X - selectionStart.X,
					oldMouseLocation.Y - selectionStart.Y);

				if (selectionRectangle.Width < 0) {
					selectionRectangle.X += selectionRectangle.Width;
					selectionRectangle.Width *= -1;
				}
				if (selectionRectangle.Height < 0) {
					selectionRectangle.Y += selectionRectangle.Height;
					selectionRectangle.Height *= -1;
				}
				RectangleF absoluteRectangle = ConvertRelativeToAbsolute(selectionRectangle);

				foreach (DiagramElement element in GetElements())
					element.TrySelect(absoluteRectangle);

				if (selectionChanged)
					OnSelectionChanged(EventArgs.Empty);
			}
			state = State.Normal;
		}

		protected override void OnMouseUp(MouseEventArgs e)
		{
			base.OnMouseUp(e);
			AbsoluteMouseEventArgs abs_e = new AbsoluteMouseEventArgs(e, AbsoluteDisplayOffset, Zoom);

			if (state == State.Selectioning) {
				FinishSelectioning(true);
			}
			else if (state == State.Dragging) {
				state = State.Normal;
			}
			else {
				foreach (DiagramElement element in GetElementsInDisplayOrder()) {
					if (element.MouseUpped(abs_e))
						break;
				}
			}

			RedrawDirtyElements();
		}

		protected override void OnMouseWheel(MouseEventArgs e)
		{
			if (state == State.Selectioning)
				FinishSelectioning(false);

			if (Control.ModifierKeys == Keys.Control) {
				bool enlarge = (e.Delta > 0);

				if (ClientRectangle.Contains(e.Location))
					ChangeZoom(enlarge, ConvertRelativeToAbsolute(e.Location));
				else
					ChangeZoom(enlarge);
			}
			else {
				base.OnMouseWheel(e);
			}
		}

		protected override void OnMouseDoubleClick(MouseEventArgs e)
		{
			base.OnMouseDoubleClick(e);
			AbsoluteMouseEventArgs abs_e = new AbsoluteMouseEventArgs(e, AbsoluteDisplayOffset, Zoom);

			foreach (DiagramElement element in GetElements()) {
				if (element.DoubleClicked(abs_e)) {
					RedrawDirtyElements();
					return;
				}
			}
		}

		protected override void OnKeyDown(KeyEventArgs e)
		{
			base.OnKeyDown(e);

			switch (e.KeyCode) {
				// Zoom in
				case Keys.Add:
					if (e.Control)
						ChangeZoom(true); 
					break;

				// Zoom out
				case Keys.Subtract:
					if (e.Control)
						ChangeZoom(false);
					break;

				// Finish selectioning
				case Keys.Escape:
					if (state == State.Selectioning)
						FinishSelectioning(false);
					break;

				// Delete
				case Keys.Delete:
					DeleteSelectedElements();
					break;

				// Select all
				case Keys.A:
					if (e.Control)
						SelectAll();
					break;
			}
		}

		protected override void OnLostFocus(EventArgs e)
		{
			base.OnLostFocus(e);
			if (state == State.Selectioning)
				FinishSelectioning(false);
		}

		protected override void OnPaint(PaintEventArgs e)
		{
			base.OnPaint(e);

			Graphics g = e.Graphics;
			SizeF offset = AbsoluteDisplayOffset;

			// Transform the screen to absolute coordinate system
			g.ScaleTransform(Zoom, Zoom);
			g.TranslateTransform(-offset.Width, -offset.Height);

			// Set the drawing quality
			g.SmoothingMode = SmoothingMode.AntiAlias;
			if (Zoom != 1.0F)
				g.TextRenderingHint = TextRenderingHint.AntiAlias;

			// Draw diagram elements
			RectangleF absoluteClip = ConvertRelativeToAbsolute(e.ClipRectangle);
			foreach (DiagramElement element in GetElementsInReversedDisplayOrder()) {
				if (absoluteClip.IntersectsWith(element.GetDirtyArea()))
					element.Draw(g);
			}

			// Draw diagram border
			g.SmoothingMode = SmoothingMode.HighSpeed;
			RectangleF diagramRectangle = new RectangleF(PointF.Empty, DiagramSize);
			if (absoluteClip.IntersectsWith(diagramRectangle)) {
				g.DrawLines(diagramBorderPen, new Point[] {
					new Point(DiagramSize.Width, 0),
					new Point(DiagramSize.Width, DiagramSize.Height),
					new Point(0, DiagramSize.Height)
				});
			}
		}

		private void OnSelectionChanged(EventArgs e)
		{
			selectionChanged = false;
			if (SelectionChanged != null)
				SelectionChanged(this, e);
		}

		private Rectangle GetPrintingArea(bool selectedOnly)
		{
			Rectangle area = Rectangle.Empty;
			Style style = Style.CurrentStyle;

			foreach (DiagramElement element in GetElements()) {
				if (element.IsSelected || !selectedOnly) {
					if (area.IsEmpty)
						area = element.GetPrintingClip(style);
					else
						area = Rectangle.Union(area, element.GetPrintingClip(style));
				}
			}

			return area;
		}

		Rectangle IPrintableDiagram.GetPrintingArea(bool selectedOnly)
		{
			return GetPrintingArea(selectedOnly);
		}

		private void DrawDiagram(Graphics g, bool selectedOnly, bool fillBackground)
		{
			DrawDiagram(g, selectedOnly, fillBackground, Size.Empty, Style.CurrentStyle);
		}

		private void DrawDiagram(Graphics g, bool selectedOnly, bool fillBackground,
			Style style)
		{
			DrawDiagram(g, selectedOnly, fillBackground, Size.Empty, style);
		}

		private void DrawDiagram(Graphics g, bool selectedOnly, bool fillBackground,
			Size offset)
		{
			DrawDiagram(g, selectedOnly, fillBackground, offset, Style.CurrentStyle);
		}

		private void DrawDiagram(Graphics g, bool selectedOnly, bool fillBackground,
			Size offset, Style style)
		{
			if (!offset.IsEmpty)
				g.TranslateTransform(offset.Width, offset.Height);
			if (fillBackground)
				g.Clear(style.DiagramBackColor);

			foreach (DiagramElement element in GetElements()) {
				if (element.IsSelected || !selectedOnly)
					element.Draw(g, false, style);
			}
		}

		public void Print()
		{
			using (PrintDialog printDialog = new PrintDialog(this))
				printDialog.ShowDialog();
		}

		void IPrintableDiagram.Print(Graphics g, bool selectedOnly, Style style)
		{
			DrawDiagram(g, selectedOnly, false, style);
		}

		public void CopyAsImage()
		{
			Rectangle area = GetPrintingArea(true);
			Size marginSize = new Size(-area.Left, -area.Top);

			using (Image image = new Bitmap(area.Width, area.Height))
			using (Graphics g = Graphics.FromImage(image))
			{
				g.SmoothingMode = SmoothingMode.HighQuality;
				DrawDiagram(g, true, true, marginSize);
				Clipboard.SetImage(image);
			}
		}

		public void SaveAsImage()
		{
			SaveAsImage(false);
		}

		public void SaveAsImage(bool selectedOnly)
		{
			if (IsEmpty)
				return;

			saveAsImageDialog.FileName = project.ProjectFileNameWithoutExtension;
			saveAsImageDialog.InitialDirectory = project.ProjectDirectory;

			if (saveAsImageDialog.ShowDialog() == DialogResult.OK) {
				string extension = System.IO.Path.GetExtension(saveAsImageDialog.FileName);
				ImageFormat format;

				switch (extension.ToLower()) {
					case ".bmp":
						format = ImageFormat.Bmp;
						break;

					case ".gif":
						format = ImageFormat.Gif;
						break;

					case ".jpg":
					case ".jpeg":
						format = ImageFormat.Jpeg;
						break;

					case ".emf":
						format = ImageFormat.Emf;
						break;

					case ".png":
					default:
						format = ImageFormat.Png;
						break;
				}
				bool transparent = (saveAsImageDialog.FilterIndex == 5);
				SaveAsImage(saveAsImageDialog.FileName, format, selectedOnly, transparent);
			}
		}

		/// <exception cref="ArgumentNullException">
		/// <paramref name="path"/> is null.-or-
		/// <paramref name="format"/> is null.
		/// </exception>
		private void SaveAsImage(string path, ImageFormat format, bool selectedOnly, bool transparent)
		{
			const int Margin = 20;

			if (format == null)
				throw new ArgumentNullException("format");
			if (path == null)
				throw new ArgumentNullException("path");

			Rectangle area = GetPrintingArea(selectedOnly);

			if (format == ImageFormat.Emf) { // Save to metafile
				Graphics metaG = this.CreateGraphics();
				IntPtr hc = metaG.GetHdc();
				Graphics g = null;

				try {
					Metafile meta = new Metafile(path, hc);
					g = Graphics.FromImage(meta);
					g.SmoothingMode = System.Drawing.Drawing2D.SmoothingMode.HighQuality;
					g.TextRenderingHint = System.Drawing.Text.TextRenderingHint.AntiAliasGridFit;
					DrawDiagram(g, selectedOnly, true, new Size(-area.Left, -area.Top));
					meta.Dispose();
				}
				catch (Exception ex) {
					MessageBox.Show(
						string.Format("{0}\n{1}: {2}", Strings.GetString("error_in_saving_image"),
							Strings.GetString("errors_reason"), ex.Message),
						Strings.GetString("error"), MessageBoxButtons.OK, MessageBoxIcon.Error);
				}
				finally {
					metaG.ReleaseHdc();
					metaG.Dispose();
					if (g != null)
						g.Dispose();
				}
			}
			else { // Save to rastered image
				Size marginSize = new Size(Margin - area.Left, Margin - area.Top);

				using (Image image = new Bitmap(area.Width + Margin * 2, area.Height + Margin * 2))
				using (Graphics g = Graphics.FromImage(image))
				{
					g.SmoothingMode = SmoothingMode.HighQuality;
					DrawDiagram(g, selectedOnly, !transparent, marginSize);

					try {
						image.Save(path, format);
					}
					catch (Exception ex) {
						MessageBox.Show(
							string.Format("{0}\n{1}: {2}", Strings.GetString("error_in_saving_image"),
								Strings.GetString("errors_reason"), ex.Message),
							Strings.GetString("error"), MessageBoxButtons.OK, MessageBoxIcon.Error);
					}
				}
			}
		}

		private void Strings_LanguageChanged(object sender, EventArgs e)
		{
			UpdateContextMenuTexts();
		}

		private void UpdateContextMenuTexts()
		{
			mnuAddNewElement.Text = Strings.GetString("menu_new");
			mnuNewClass.Text = Strings.GetString("menu_class");
			mnuNewStructure.Text = Strings.GetString("menu_struct");
			mnuNewInterface.Text = Strings.GetString("menu_interface");
			mnuNewEnum.Text = Strings.GetString("menu_enum");
			mnuNewDelegate.Text = Strings.GetString("menu_delegate");
			mnuNewComment.Text = Strings.GetString("menu_comment");
			mnuNewAssociation.Text = Strings.GetString("menu_association");
			mnuNewComposition.Text = Strings.GetString("menu_composition");
			mnuNewAggregation.Text = Strings.GetString("menu_aggregation");
			mnuNewGeneralization.Text = Strings.GetString("menu_generalization");
			mnuNewRealization.Text = Strings.GetString("menu_realization");
			mnuNewDependency.Text = Strings.GetString("menu_dependency");
			mnuNewNesting.Text = Strings.GetString("menu_nesting");
			mnuNewCommentRelation.Text = Strings.GetString("menu_comment_relation");
			//------
			mnuZoom.Text = Strings.GetString("menu_zoom");
			mnuZoomIn.Text = Strings.GetString("menu_zoom_in");
			mnuZoomOut.Text = Strings.GetString("menu_zoom_out");
			mnuAutoZoom.Text = Strings.GetString("menu_auto_zoom");
			//------
			mnuMembersFormat.Text = Strings.GetString("menu_members_format");
			mnuShowType.Text = Strings.GetString("menu_type");
			mnuShowParameters.Text = Strings.GetString("menu_parameters");
			mnuShowParameterNames.Text = Strings.GetString("menu_parameter_names");
			mnuShowInitialValue.Text = Strings.GetString("menu_initial_value");
			//------
			mnuPaste.Text = Strings.GetString("menu_paste");
			mnuSaveAsImage.Text = Strings.GetString("menu_save_as_image");
			mnuSelectAll.Text = Strings.GetString("menu_select_all");
		}

		private void project_Cleared(object sender, EventArgs e)
		{
			shapes.Clear();
			connections.Clear();
			clipboard.Clear();

			Zoom = 1.0F;
			DisplayPosition = Point.Empty;
			state = State.Normal;
			selectedShapes.Clear();
			selectedConnections.Clear();
			firstSelectedElement = null;
			selectionChanged = false;
			RefreshDiagram();
		}

		private void project_ContentChanged(object sender, EventArgs e)
		{
			RefreshDiagram();
		}

		//TODO: jobbat kéne!
		private void CalculateNewShapeLocation(DiagramShape shape)
		{
			if (shapes.Count == 0) {
				PointF topLeft = this.AbsoluteDisplayPosition;
				shape.Left = (int) topLeft.X + DiagramPadding;
				shape.Top = (int) topLeft.Y + DiagramPadding;
			}
			else {
				DiagramShape firstShape = shapes.FirstValue;
				shape.Top = firstShape.Top;
				shape.Left = firstShape.Right + DiagramPadding;
			}
		}

		private void AddShape(DiagramShape shape)
		{
			shape.SelectionChanged += new EventHandler(shape_SelectionChanged);
			shape.Dragging += new MoveEventHandler(element_Dragging);
			shape.Move += delegate { project.IsDirty = true; };
			shape.Resize += delegate { project.IsDirty = true; };
			shape.MouseDown += delegate { shapes.ShiftToFirstPlace(shape); };
			CalculateNewShapeLocation(shape);
			shapes.AddFirst(shape);
		}

		private void AddRelation(Connection connection)
		{
			connection.SelectionChanged += new EventHandler(connection_SelectionChanged);
			connection.Changed += delegate { project.IsDirty = true; };
			connections.AddFirst(connection);
		}

		private void project_EntityAdded(object sender, EntityEventArgs e)
		{
			IEntity entity = e.Entity;

			if (entity is ClassType)
				AddShape(new ClassShape((ClassType) entity));
			else if (entity is StructureType)
				AddShape(new StructShape((StructureType) entity));
			else if (entity is InterfaceType)
				AddShape(new InterfaceShape((InterfaceType) entity));
			else if (entity is EnumType)
				AddShape(new EnumShape((EnumType) entity));
			else if (entity is DelegateType)
				AddShape(new DelegateShape((DelegateType) entity));
			else if (entity is Comment)
				AddShape(new CommentShape((Comment) entity));
		}

		private void project_EntityRemoved(object sender, EntityEventArgs e)
		{
			shapes.RemoveOn(delegate(DiagramShape shape) {
				return (shape.Entity == e.Entity);
			});
		}

		public DiagramShape GetShape(IEntity entity)
		{
			foreach (DiagramShape shape in shapes) {
				if (shape.Entity == entity)
					return shape;
			}
			return null;
		}

		private void project_RelationAdded(object sender, RelationEventArgs e)
		{
			Relation relation = e.Relation;

			DiagramShape startShape = GetShape(e.Relation.First);
			DiagramShape endShape   = GetShape(e.Relation.Second);

			if (startShape == null || endShape == null)
				return;

			if (relation is AssociationRelation) {
				AssociationRelation association = (AssociationRelation) relation;
				AddRelation(new Association(association, startShape, endShape));
			}
			else if (relation is GeneralizationRelation) {
				GeneralizationRelation generalization = (GeneralizationRelation) relation;
				AddRelation(new Generalization(generalization, startShape, endShape));
			}
			else if (relation is RealizationRelation) {
				RealizationRelation realization = (RealizationRelation) relation;
				AddRelation(new Realization(realization, startShape, endShape));
			}
			else if (relation is DependencyRelation) {
				DependencyRelation dependency = (DependencyRelation) relation;
				AddRelation(new Dependency(dependency, startShape, endShape));
			}
			else if (relation is NestingRelation) {
				NestingRelation nesting = (NestingRelation) relation;
				AddRelation(new Nesting(nesting, startShape, endShape));
			}
			else if (relation is CommentRelation) {
				CommentRelation commentRelation = (CommentRelation) relation;
				AddRelation(new CommentConnection(commentRelation, startShape, endShape));
			}
		}

		private void project_RelationRemoved(object sender, RelationEventArgs e)
		{
			connections.RemoveOn(delegate(Connection connection) {
				return (connection.Relation == e.Relation);
			});
		}

		private void project_Deserializing(object sender, SerializeEventArgsBinary e)
		{
			// Calculate minimum diagram size
			Size minSize = GetMinimumDiagramSize();
			int width = Math.Max(Settings.DiagramWidth, minSize.Width);
			int height = Math.Max(Settings.DiagramHeight, minSize.Height);

			diagramSize = new Size(width, height);
			Settings.DiagramWidth = diagramSize.Width;
			Settings.DiagramHeight = diagramSize.Height;

			UpdateWorkspaceSize();
		}

		#region Context menu eventhandlers

		private void dynamicContextMenu_Opening(object sender, CancelEventArgs e)
		{
			state = State.Normal;
		}

		private void defaultContextMenu_Opening(object sender, CancelEventArgs e)
		{
			mnuNewInterface.Visible = project.Language.SupportsInterfaces;
			mnuNewStructure.Visible = project.Language.SupportsStructures;
			mnuNewEnum.Visible = project.Language.SupportsEnums;
			mnuNewDelegate.Visible = project.Language.SupportsDelegates;

			mnuShowType.Checked = Settings.ShowType;
			mnuShowParameters.Checked = Settings.ShowParameters;
			mnuShowParameterNames.Checked = Settings.ShowParameterNames;
			mnuShowInitialValue.Checked = Settings.ShowInitialValue;

			mnuPaste.Enabled = false;
			mnuSaveAsImage.Enabled = !IsEmpty;
			mnuSelectAll.Enabled = !IsEmpty;
			state = State.Normal;
		}

		private void mnuNewClass_Click(object sender, EventArgs e)
		{
			project.AddClass();
		}

		private void mnuNewStructure_Click(object sender, EventArgs e)
		{
			if (project.Language.SupportsStructures)
				project.AddStructure();
		}

		private void mnuNewInterface_Click(object sender, EventArgs e)
		{
			if (project.Language.SupportsInterfaces)
				project.AddInterface();
		}

		private void mnuNewEnum_Click(object sender, EventArgs e)
		{
			if (project.Language.SupportsEnums)
				project.AddEnum();
		}

		private void mnuNewDelegate_Click(object sender, EventArgs e)
		{
			if (project.Language.SupportsDelegates)
				project.AddDelegate();
		}

		private void mnuNewComment_Click(object sender, EventArgs e)
		{
			project.AddComment();
		}

		private void mnuNewAssociation_Click(object sender, EventArgs e)
		{
			CreateNewAssociation();
		}

		private void mnuNewComposition_Click(object sender, EventArgs e)
		{
			CreateNewComposition();
		}

		private void mnuNewAggregation_Click(object sender, EventArgs e)
		{
			CreateNewAggregation();
		}

		private void mnuNewGeneralization_Click(object sender, EventArgs e)
		{
			CreateNewGeneralization();
		}

		private void mnuNewRealization_Click(object sender, EventArgs e)
		{
			CreateNewRealization();
		}

		private void mnuNewDependency_Click(object sender, EventArgs e)
		{
			CreateNewDependency();
		}

		private void mnuNewNesting_Click(object sender, EventArgs e)
		{
			CreateNewNesting();
		}

		private void mnuNewCommentRelation_Click(object sender, EventArgs e)
		{
			CreateNewCommentRelationsip();
		}

		private void mnuZoomIn_Click(object sender, EventArgs e)
		{
			ChangeZoom(Zoom * 2);
		}

		private void mnuZoomOut_Click(object sender, EventArgs e)
		{
			ChangeZoom(Zoom / 2);
		}

		private void mnuAutoZoom_Click(object sender, EventArgs e)
		{
			AutoZoom();
		}

		private void mnuShowType_Click(object sender, EventArgs e)
		{
			Settings.ShowType = ((ToolStripMenuItem) sender).Checked;
			RefreshDiagram();
		}

		private void mnuShowParameters_Click(object sender, EventArgs e)
		{
			Settings.ShowParameters = ((ToolStripMenuItem) sender).Checked;
			RefreshDiagram();
		}

		private void mnuShowParameterNames_Click(object sender, EventArgs e)
		{
			Settings.ShowParameterNames = ((ToolStripMenuItem) sender).Checked;
			RefreshDiagram();
		}

		private void mnuShowInitialValue_Click(object sender, EventArgs e)
		{
			Settings.ShowInitialValue = ((ToolStripMenuItem) sender).Checked;
			RefreshDiagram();
		}

		private void mnuPaste_Click(object sender, EventArgs e)
		{
			//UNDONE: mnuPaste_Click
		}

		private void mnuSaveAsImage_Click(object sender, EventArgs e)
		{
			SaveAsImage();
		}

		private void mnuSelectAll_Click(object sender, EventArgs e)
		{
			SelectAll();
		}

		#endregion
	}
}
