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
    [Serializable]
	public abstract class CompositeTypeShape : TypeShape
	{
		const int AccessSpacing = 12;

		static MembersDialog membersDialog = new MembersDialog();
		static SolidBrush memberBrush = new SolidBrush(Color.Black);
		static StringFormat accessFormat = new StringFormat();

		static CompositeTypeShape()
		{
			accessFormat.Alignment = StringAlignment.Center;
			accessFormat.LineAlignment = StringAlignment.Center;
		}

		/// <exception cref="ArgumentNullException">
		/// <paramref name="compositeType"/> is null.
		/// </exception>
		protected CompositeTypeShape(CompositeType compositeType) : base(compositeType)
		{			
		}

		public sealed override TypeBase TypeBase
		{
			get { return CompositeType; }
		}

		protected abstract CompositeType CompositeType { get; }

		protected internal override void EditMembers()
		{
			membersDialog.ShowDialog(CompositeType);
		}

		private static string GetAccessString(Member member)
		{
			switch (member.Access) {
				case AccessModifier.Public:
					return "+";

				case AccessModifier.Internal:
					return "~";

				case AccessModifier.ProtectedInternal:
				case AccessModifier.Protected:
					return "#";

				case AccessModifier.Private:
				default:
					return "-";
			}
		}
		
		private static string GetMemberString(Member member)
		{
			return member.GetCaption(
				Settings.ShowType,
				Settings.ShowParameters,
				Settings.ShowParameterNames,
				Settings.ShowInitialValue);
		}

		private static bool IsVisibleMember(Member member)
		{
			switch (member.Access) {
				case AccessModifier.Public:
					return Settings.IsPublicVisible;

				case AccessModifier.ProtectedInternal:
					return Settings.IsProtintVisible;

				case AccessModifier.Internal:
					return Settings.IsInternalVisible;

				case AccessModifier.Protected:
					return Settings.IsProtectedVisible;

				case AccessModifier.Private:
				default:
					return Settings.IsPrivateVisible;
			}
		}

		private Font GetMemberFont(Member member, Style style)
		{
			Font memberFont;
			if (member.IsStatic) {
				memberFont = style.StaticMemberFont;
			}
			else if (member is Operation &&
				(((Operation) member).IsAbstract || member.Parent is InterfaceType)) {
				memberFont = style.AbstractMemberFont;
			}
			else {
				memberFont = GetFont(style);
			}

			return memberFont;
		}

		private void DrawMember(Graphics g, Member member, Rectangle record, Style style)
		{
			Font memberFont = GetMemberFont(member, style);

			if (member is Field)
				memberBrush.Color = style.AttributeColor;
			else
				memberBrush.Color = style.OperationColor;

			if (style.UseIcons) {
				Image icon = Icons.GetImage(member);
				g.DrawImage(icon, record.X, record.Y, icon.Width, icon.Height);

				Rectangle textBounds = new Rectangle(
					record.X + IconSpacing, record.Y,
					record.Width - IconSpacing, record.Height);

				string memberString = GetMemberString(member);
				g.DrawString(memberString, memberFont, memberBrush, textBounds, memberFormat);
			}
			else {
				Rectangle accessBounds = new Rectangle(
					record.X, record.Y, AccessSpacing, record.Height);
				Rectangle textBounds = new Rectangle(
					record.X + AccessSpacing, record.Y,
					record.Width - AccessSpacing, record.Height);

				g.DrawString(GetAccessString(member), GetFont(style),
					memberBrush, accessBounds, accessFormat);
				g.DrawString(GetMemberString(member), memberFont,
					memberBrush, textBounds, memberFormat);
			}
		}

		protected override void DrawContent(Graphics g, Style style)
		{
			Rectangle record = new Rectangle(
				Left + MarginSize, Top + HeaderHeight + MarginSize,
				Width - MarginSize * 2, MemberHeight);

			// Draw fields
			foreach (Field field in CompositeType.Fields) {
				if (IsVisibleMember(field)) {
					DrawMember(g, field, record, style);
					record.Y += MemberHeight;
				}
			}

			//Draw separator line 
			if (CompositeType.SupportsFields) {
				DrawSeparatorLine(g, record.Top + MarginSize);
				record.Y += MarginSize * 2;
			}

			// Draw operations
			foreach (Operation operation in CompositeType.Operations) {
				if (IsVisibleMember(operation)) {
					DrawMember(g, operation, record, style);
					record.Y += MemberHeight;
				}
			}
		}

		protected override float GetRequiredWidth(Graphics g, Style style)
		{
			float requiredWidth = 0;

			foreach (Field field in CompositeType.Fields) {
				if (IsVisibleMember(field)) {
					float fieldWidth = g.MeasureString(GetMemberString(field),
						GetMemberFont(field, style), PointF.Empty, memberFormat).Width;
					requiredWidth = Math.Max(requiredWidth, fieldWidth);
				}
			}
			foreach (Operation operation in CompositeType.Operations) {
				if (IsVisibleMember(operation)) {
					float operationWidth = g.MeasureString(GetMemberString(operation),
						GetMemberFont(operation, style), PointF.Empty, memberFormat).Width;
					requiredWidth = Math.Max(requiredWidth, operationWidth);
				}
			}
			requiredWidth += (style.UseIcons) ? IconSpacing : AccessSpacing;
			requiredWidth += MarginSize * 2;

			return Math.Max(requiredWidth, base.GetRequiredWidth(g, style));
		}

		protected override int GetRequiredHeight()
		{
			int memberCount = 0;
			int spacingHeight = 0;

			if (CompositeType.SupportsFields) {
				memberCount += CompositeType.FieldCount;
				spacingHeight += MarginSize * 2;
			}
			if (CompositeType.SupportsOperations) {
				memberCount += CompositeType.OperationCount;
				spacingHeight += MarginSize * 2;
			}

			return (HeaderHeight + spacingHeight + (memberCount * MemberHeight));
		}

		private int GetRowIndex(int height)
		{
			height -= HeaderHeight + MarginSize;

			if (CompositeType.SupportsFields && (height > CompositeType.FieldCount * MemberHeight))
				height -= MarginSize * 2;

			return (height / MemberHeight);
		}
	}
}
