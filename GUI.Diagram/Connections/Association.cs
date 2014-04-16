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
using System.Collections.Generic;
using NClass.Core;
using System.Runtime.Serialization;
using System.Runtime.Serialization.Formatters.Binary;

namespace NClass.GUI.Diagram
{
    [Serializable]
	internal sealed class Association : Connection
	{
		const int DiamondWidth = 10;
		const int DiamondHeight = 18;
		static readonly Point[] diamondPoints =  {
			new Point(0, 0),
			new Point(DiamondWidth / 2, DiamondHeight / 2),
			new Point(0, DiamondHeight),
			new Point(-DiamondWidth / 2, DiamondHeight / 2)
		};
		static Pen linePen = new Pen(Color.Black);
		static SolidBrush lineBrush = new SolidBrush(Color.Black);

		AssociationRelation association;
        public override void GetObjectData(SerializationInfo info, StreamingContext ctxt)
        {
            base.GetObjectData(info, ctxt);
        }
        public Association(SerializationInfo info, StreamingContext ctxt)
            : base(info, ctxt)
        {
        }
		static Association()
		{
			linePen.MiterLimit = 1.0F;
		}

		/// <exception cref="ArgumentNullException">
		/// <paramref name="association"/> is null.-or-
		/// <paramref name="startShape"/> is null.-or-
		/// <paramref name="endShape"/> is null.
		/// </exception>
		public Association(AssociationRelation association, DiagramShape startShape,
			DiagramShape endShape) : base(association, startShape, endShape)
		{
			this.association = association;
		}

		internal AssociationRelation AssociationRelation
		{
			get { return association; }
		}

		protected internal override Relation Relation
		{
			get { return association; }
		}

		protected internal override IEnumerable<ToolStripItem> GetContextMenuItems(IDiagram diagram)
		{
			return AssociationContextMenu.Default.GetMenuItems(diagram);
		}

		protected override void DrawStartCap(Graphics g, bool onScreen, Style style)
		{
			linePen.Color = style.RelationColor;
			linePen.Width = style.RelationWidth;

			if (association.IsAggregation) {
				g.FillPolygon(Brushes.White, diamondPoints);
				g.DrawPolygon(linePen, diamondPoints);
			}
			else if (association.IsComposition) {
				lineBrush.Color = style.RelationColor;

				g.FillPolygon(lineBrush, diamondPoints);
				g.DrawPolygon(linePen, diamondPoints);
			}
		}

		protected override void DrawEndCap(Graphics g, bool onScreen, Style style)
		{
			if (association.Direction == Direction.Unidirectional) {
				linePen.Color = style.RelationColor;
				linePen.Width = style.RelationWidth;
				g.DrawLines(linePen, Arrowhead.OpenArrowPoints);
			}
		}

		protected override void Reroute()
		{
			base.Reroute();
			//TODO: asszociációnál felül kell írni a role-ok miatt
		}
	}
}
