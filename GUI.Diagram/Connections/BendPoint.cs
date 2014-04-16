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
using System.Runtime.Serialization;
using System.Runtime.Serialization.Formatters.Binary;

namespace NClass.GUI.Diagram
{
    [Serializable]
	internal sealed class BendPoint : ICloneable, ISerializable
	{
		const int Spacing = Connection.Spacing;
		internal const int SquareSize = 8;

		static Color darkStartColor = Color.Blue;
		static Color darkEndColor = Color.Red;
		static Color lightStartColor = Color.FromArgb(178, 178, 255);
		static Color lightEndColor = Color.FromArgb(255, 178, 178);
		static Pen squarePen = new Pen(Color.Black);
		static SolidBrush squareBrush = new SolidBrush(Color.Black);

		DiagramShape relativeShape;
		bool relativeToStartShape;
		bool autoPosition = true;
		Size relativePosition = Size.Empty;

		/// <exception cref="ArgumentNullException">
		/// <paramref name="relativeShape"/> is null.
		/// </exception>
		public BendPoint(DiagramShape relativeShape, bool relativeToStartShape)
		{
			if (relativeShape == null)
				throw new ArgumentNullException("relativeShape");

			this.relativeShape = relativeShape;
			this.relativeToStartShape = relativeToStartShape;
		}

		/// <exception cref="ArgumentNullException">
		/// <paramref name="relativeShape"/> is null.
		/// </exception>
		public BendPoint(DiagramShape relativeShape, bool relativeToStartShape, bool autoPosition)
			: this(relativeShape, relativeToStartShape)
		{
			this.autoPosition = autoPosition;
		}

		public bool RelativeToStartShape
		{
			get { return relativeToStartShape; }
		}

		internal bool AutoPosition
		{
			get { return autoPosition; }
			set { autoPosition = value; }
		}

		public int X
		{
			get
			{
				return (relativeShape.X + relativePosition.Width);
			}
			internal set
			{
				relativePosition.Width = value - relativeShape.X;
			}
		}

		public int Y
		{
			get
			{
				return (relativeShape.Y + relativePosition.Height);
			}
			internal set
			{
				relativePosition.Height = value - relativeShape.Y;
			}
		}

		public Point Location
		{
			get
			{
				return (relativeShape.Location + relativePosition);
			}
			set
			{
				if (value.X > relativeShape.Left - Spacing &&
					value.X < relativeShape.Right + Spacing &&
					value.Y > relativeShape.Top - Spacing &&
					value.Y < relativeShape.Bottom + Spacing)
				{
					if (X <= relativeShape.Left - Spacing) {
						X = relativeShape.Left - Spacing;
						Y = value.Y;
					}
					else if (X >= relativeShape.Right + Spacing) {
						X = relativeShape.Right + Spacing;
						Y = value.Y;
					}
					else if (Y <= relativeShape.Top - Spacing) {
						X = value.X;
						Y = relativeShape.Top - Spacing;
					}
					else {
						X = value.X;
						Y = relativeShape.Bottom + Spacing;
					}
				}
				else {
					X = value.X;
					Y = value.Y;
				}
			}
		}

		public object Clone()
		{
			return this.MemberwiseClone();
		}

		internal void Draw(Graphics g, bool onScreen, Style style, float zoom)
		{
			squarePen.Width = 1 / zoom;

			float size = SquareSize / zoom;
			RectangleF square = new RectangleF(
				X - size / 2, Y - size / 2, size, size);

			if (AutoPosition) {
				squarePen.Color = RelativeToStartShape ? lightStartColor : lightEndColor;
				g.DrawRectangle(squarePen, square.X, square.Y, square.Width, square.Height);
			}
			else {
				squarePen.Color = RelativeToStartShape ? darkStartColor : darkEndColor;
				squareBrush.Color = RelativeToStartShape ? lightStartColor : lightEndColor;

				g.FillRectangle(squareBrush, square);
				g.DrawRectangle(squarePen, square.X, square.Y, square.Width, square.Height);
			}
		}

		internal bool Contains(PointF point, float zoom)
		{
			float halfSize = SquareSize / zoom / 2;

			return (
				point.X >= X - halfSize && point.X <= X + halfSize &&
				point.Y >= Y - halfSize && point.Y <= Y + halfSize
			);
		}

		internal void ShapeResized(Size size)
		{
			if (X >= relativeShape.Left && X <= relativeShape.Right && Y > relativeShape.Top)
				Y += size.Height;

			if (Y >= relativeShape.Top && Y <= relativeShape.Bottom && X > relativeShape.Left)
				X += size.Width;
		}

        public BendPoint(SerializationInfo info, StreamingContext ctxt)
		{
			//Get the values from info and assign them to the appropriate properties

            relativeToStartShape = info.GetBoolean("_relativeToStartShape");
            DiagramShape startShape, endShape;
            startShape = (NClass.Core.BinarySerializationHelper.diagram as DiagramControl).GetShape(
                NClass.Core.BinarySerializationHelper.first_entity);
            endShape = (NClass.Core.BinarySerializationHelper.diagram as DiagramControl).GetShape(
                NClass.Core.BinarySerializationHelper.second_entity); 
            this.relativeShape = relativeToStartShape ? startShape : endShape;
            this.autoPosition = false;
            X = (int)info.GetValue("_x", typeof(int));
            Y = (int)info.GetValue("_y", typeof(int));
        }
        		//Serialization function.
        public void GetObjectData(SerializationInfo info, StreamingContext ctxt)
        {
            info.AddValue("_x", X); 
            info.AddValue("_y", Y);
            info.AddValue("_relativeToStartShape", RelativeToStartShape);
        }
		internal void Serialize(XmlElement node)
		{
			XmlDocument document = node.OwnerDocument;

			XmlElement xNode = document.CreateElement("X");
			xNode.InnerText = X.ToString();
			node.AppendChild(xNode);

			XmlElement yNode = document.CreateElement("Y");
			yNode.InnerText = Y.ToString();
			node.AppendChild(yNode);
		}

		internal void Deserialize(XmlElement node)
		{
			XmlElement xNode = node["X"];
			if (xNode != null) {
				int x;
				int.TryParse(xNode.InnerText, out x);
				X = x;
			}
			XmlElement yNode = node["Y"];
			if (yNode != null) {
				int y;
				int.TryParse(yNode.InnerText, out y);
				Y = y;
			}
		}
	}
}
