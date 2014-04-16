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
	public sealed class DelegateShape : TypeShape
	{
		static DelegateDialog delegateDialog = new DelegateDialog();
		static SolidBrush parameterBrush = new SolidBrush(Color.Black);

		DelegateType _delegate;

		/// <exception cref="ArgumentNullException">
		/// <paramref name="_delegate"/> is null.
		/// </exception>
		internal DelegateShape(DelegateType _delegate) : base(_delegate)
		{
			this._delegate = _delegate;
		}

		public override TypeBase TypeBase
		{
			get { return _delegate; }
		}

		public DelegateType DelegateType
		{
			get { return _delegate; }
		}

		protected override Color GetBackgroundColor(Style style)
		{
			return style.DelegateBackgroundColor;
		}

		protected override Color GetBorderColor(Style style)
		{
			return style.DelegateBorderColor;
		}

		protected override int GetBorderWidth(Style style)
		{
			return style.DelegateBorderWidth;
		}

		protected override bool IsBorderDashed(Style style)
		{
			return style.IsDelegateBorderDashed;
		}

		protected override Color GetHeaderColor(Style style)
		{
			return style.DelegateHeaderColor;
		}

		protected override int GetRoundingSize(Style style)
		{
			return style.DelegateRoundingSize;
		}

		protected override bool UseGradientHeader(Style style)
		{
			return style.DelegateUseGradientHeader;
		}

		protected internal override void EditMembers()
		{
			delegateDialog.ShowDialog(_delegate);
		}

		private void DrawItem(Graphics g, Parameter parameter, Rectangle record, Style style)
		{
			Font font = GetFont(style);
			string memberString = parameter.ToString();
			parameterBrush.Color = style.EnumItemColor;

			if (style.UseIcons) {
				Image icon = Properties.Resources.Parameter;
				g.DrawImage(icon, record.X, record.Y, icon.Width, icon.Height);

				Rectangle textBounds = new Rectangle(
					record.X + IconSpacing, record.Y,
					record.Width - IconSpacing, record.Height);

				g.DrawString(memberString, font, parameterBrush, textBounds, memberFormat);
			}
			else {
				g.DrawString(memberString, font, parameterBrush, record, memberFormat);
			}
		}

		protected override void DrawContent(Graphics g, Style style)
		{
			Rectangle record = new Rectangle(
				Left + MarginSize, Top + HeaderHeight + MarginSize,
				Width - MarginSize * 2, MemberHeight);

			foreach (Parameter parameter in _delegate.Arguments) {
				DrawItem(g, parameter, record, style);
				record.Y += MemberHeight;
			}
		}

		protected override float GetRequiredWidth(Graphics g, Style style)
		{
			float requiredWidth = 0;

			Font font = GetFont(style);
			foreach (Parameter parameter in _delegate.Arguments) {
				float itemWidth = g.MeasureString(parameter.ToString(),
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
			return (HeaderHeight + (MarginSize * 2) + (_delegate.ArgumentCount * MemberHeight));
		}
	}
}
