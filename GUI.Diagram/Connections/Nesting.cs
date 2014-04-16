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
using NClass.Core;

namespace NClass.GUI.Diagram
{
	internal sealed class Nesting : Connection
	{
		const int Radius = 9;
		const int CrossSize = 8;
		static Pen linePen = new Pen(Color.Black);

		NestingRelation nesting;

		/// <exception cref="ArgumentNullException">
		/// <paramref name="nesting"/> is null.-or-
		/// <paramref name="startShape"/> is null.-or-
		/// <paramref name="endShape"/> is null.
		/// </exception>
		public Nesting(NestingRelation nesting, DiagramShape startShape,
			DiagramShape endShape) : base(nesting, startShape, endShape)
		{
			this.nesting = nesting;
		}

		internal NestingRelation NestingRelation
		{
			get { return nesting; }
		}

		protected internal override Relation Relation
		{
			get { return nesting; }
		}

		protected override void DrawStartCap(System.Drawing.Graphics g, bool onScreen, Style style)
		{
			linePen.Color = style.RelationColor;
			linePen.Width = style.RelationWidth;

			g.FillEllipse(Brushes.White, -Radius, 0, Radius * 2, Radius * 2);
			g.DrawEllipse(linePen, -Radius, 0, Radius * 2, Radius * 2);
			g.DrawLine(linePen, 0, Radius - CrossSize / 2, 0, Radius + CrossSize / 2);
			g.DrawLine(linePen, -CrossSize / 2, Radius, CrossSize / 2, Radius);
		}
	}
}
