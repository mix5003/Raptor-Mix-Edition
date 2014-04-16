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
using System.Windows.Forms;
using NClass.Translations;

namespace NClass.GUI.Diagram
{
	internal sealed class CommentShapeContextMenu : ContextMenu
	{
		static CommentShapeContextMenu _default;

		ToolStripMenuItem mnuEditComment;

		static CommentShapeContextMenu()
		{
			_default = new CommentShapeContextMenu();
			Strings.LanguageChanged += delegate { _default.UpdateTexts(); };
		}

		private CommentShapeContextMenu()
		{
			InitMenuItems();
		}

		public static CommentShapeContextMenu Default
		{
			get { return _default; }
		}

		private void UpdateTexts()
		{
			mnuEditComment.Text = Strings.GetString("menu_edit_comment");
		}

		public override void ValidateMenuItems(IDiagram diagram)
		{
			base.ValidateMenuItems(diagram);
			ShapeContextMenu.Default.ValidateMenuItems(diagram);
			mnuEditComment.Enabled = diagram.SingleSelection;
		}

		private void InitMenuItems()
		{
			mnuEditComment = new ToolStripMenuItem(
				Strings.GetString("menu_edit_comment"),
				Properties.Resources.EditComment, mnuEditComment_Click);

			MenuList.AddRange(ShapeContextMenu.Default.MenuItems);
			MenuList.AddRange(new ToolStripItem[] {
				new ToolStripSeparator(),
				mnuEditComment,
			});
		}

		private void mnuEditComment_Click(object sender, EventArgs e)
		{
			if (Diagram != null) {
				CommentShape commentShape = Diagram.FirstSelectedElement as CommentShape;
				if (commentShape != null)
					commentShape.EditText();
			}
		}
	}
}
