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
    [Serializable]
	internal sealed class EnumShape : TypeShape
	{
		static EnumDialog enumDialog = new EnumDialog();
		static SolidBrush itemBrush = new SolidBrush(Color.Black);

		EnumType _enum;

		/// <exception cref="ArgumentNullException">
		/// <paramref name="enumType"/> is null.
		/// </exception>
		internal EnumShape(EnumType _enum) : base(_enum)
		{
			this._enum = _enum;
		}

		public override TypeBase TypeBase
		{
			get { return _enum; }
		}

		protected override Color GetBackgroundColor(Style style)
		{
			return style.EnumBackgroundColor;
		}

		protected override Color GetBorderColor(Style style)
		{
			return style.EnumBorderColor;
		}

		protected override int GetBorderWidth(Style style)
		{
			return style.EnumBorderWidth;
		}

		protected override bool IsBorderDashed(Style style)
		{
			return style.IsEnumBorderDashed;
		}

		protected override Color GetHeaderColor(Style style)
		{
			return style.EnumHeaderColor;
		}

		protected override int GetRoundingSize(Style style)
		{
			return style.EnumRoundingSize;
		}

		protected override bool UseGradientHeader(Style style)
		{
			return style.EnumUseGradientHeader;
		}

		protected internal override void EditMembers()
		{
			enumDialog.ShowDialog(_enum);
		}

		private void DrawItem(Graphics g, EnumValue value, Rectangle record, Style style)
		{
			Font font = GetFont(style);
			string memberString = value.ToString();
			itemBrush.Color = style.EnumItemColor;

			if (style.UseIcons) {
				Image icon = Properties.Resources.EnumItem;
				g.DrawImage(icon, record.X, record.Y, icon.Width, icon.Height);

				Rectangle textBounds = new Rectangle(
					record.X + IconSpacing, record.Y,
					record.Width - IconSpacing, record.Height);

				g.DrawString(memberString, font, itemBrush, textBounds, memberFormat);
			}
			else {
				g.DrawString(memberString, font, itemBrush, record, memberFormat);
			}
		}

		protected override void DrawContent(Graphics g, Style style)
		{
			Rectangle record = new Rectangle(
				Left + MarginSize, Top + HeaderHeight + MarginSize,
				Width - MarginSize * 2, MemberHeight);

			foreach (EnumValue value in _enum.Values) {
				DrawItem(g, value, record, style);
				record.Y += MemberHeight;
			}
		}

		protected override float GetRequiredWidth(Graphics g, Style style)
		{
			float requiredWidth = 0;

			Font font = GetFont(style);
			foreach (EnumValue value in _enum.Values) {
				float itemWidth = g.MeasureString(value.ToString(),
					font, PointF.Empty, memberFormat).Width;
				requiredWidth = Math.Max(requiredWidth, itemWidth);
			}

			if (style.UseIcons)
				requiredWidth += IconSpacing;
			requiredWidth += MarginSize * 2;

			return Math.Max(requiredWidth, base.GetRequiredWidth(g, style));
		}

		protected override int GetRequiredHeight()
		{
			return (HeaderHeight + (MarginSize * 2) + (_enum.ValueCount * MemberHeight));
		}
	}
}
