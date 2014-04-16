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
using NClass.Core;

namespace NClass.GUI.Diagram
{
	internal sealed class Realization : Connection
	{
		static Pen linePen = new Pen(Color.Black);

		RealizationRelation realization;

		static Realization()
		{
			linePen.MiterLimit = 2.0F;
		}

		/// <exception cref="ArgumentNullException">
		/// <paramref name="realization"/> is null.-or-
		/// <paramref name="startShape"/> is null.-or-
		/// <paramref name="endShape"/> is null.
		/// </exception>
		public Realization(RealizationRelation realization, DiagramShape startShape,
			DiagramShape endShape) : base(realization, startShape, endShape)
		{
			this.realization = realization;
		}

		internal RealizationRelation RealizationRelation
		{
			get { return realization; }
		}

		protected internal override Relation Relation
		{
			get { return realization; }
		}

		protected override bool IsDashed
		{
			get { return true; }
		}

		protected override void DrawEndCap(System.Drawing.Graphics g, bool onScreen, Style style)
		{
			linePen.Color = style.RelationColor;
			linePen.Width = style.RelationWidth;

			g.FillPath(Brushes.White, Arrowhead.ClosedArrowPath);
			g.DrawPath(linePen, Arrowhead.ClosedArrowPath);
		}
	}
}
