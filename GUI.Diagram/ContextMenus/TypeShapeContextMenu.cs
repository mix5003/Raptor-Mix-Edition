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
	internal sealed class TypeShapeContextMenu : ContextMenu
	{
		static TypeShapeContextMenu _default;

		ToolStripMenuItem mnuSize, mnuAutoWidth, mnuAutoHeight;
		ToolStripMenuItem mnuCollapseAllSelected, mnuExpandAllSelected;
		ToolStripMenuItem mnuEditMembers;

		static TypeShapeContextMenu()
		{
			_default = new TypeShapeContextMenu();
			Strings.LanguageChanged += delegate { _default.UpdateTexts(); };
		}

		private TypeShapeContextMenu()
		{
			InitMenuItems();
		}

		public static TypeShapeContextMenu Default
		{
			get { return _default; }
		}

		private void UpdateTexts()
		{
			mnuSize.Text = Strings.GetString("menu_size");
			mnuAutoWidth.Text = Strings.GetString("menu_auto_width");
			mnuAutoHeight.Text = Strings.GetString("menu_auto_height");
			mnuCollapseAllSelected.Text = Strings.GetString("menu_collapse_all_selected");
			mnuExpandAllSelected.Text = Strings.GetString("menu_expand_all_selected");
			mnuEditMembers.Text = Strings.GetString("menu_edit_members");
		}

		public override void ValidateMenuItems(IDiagram diagram)
		{
			base.ValidateMenuItems(diagram);
			ShapeContextMenu.Default.ValidateMenuItems(diagram);
			mnuEditMembers.Enabled = diagram.SingleSelection;
		}

		private void InitMenuItems()
		{
			mnuEditMembers = new ToolStripMenuItem(Strings.GetString("menu_edit_members"),
				Properties.Resources.EditMembers, mnuEditMembers_Click);
			mnuAutoWidth = new ToolStripMenuItem(Strings.GetString("menu_auto_width"),
				null, mnuAutoWidth_Click);
			mnuAutoHeight = new ToolStripMenuItem(Strings.GetString("menu_auto_height"),
				null, mnuAutoHeight_Click);
			mnuCollapseAllSelected = new ToolStripMenuItem(
				Strings.GetString("menu_collapse_all_selected"),
				null, mnuCollapseAllSelected_Click);
			mnuExpandAllSelected = new ToolStripMenuItem(
				Strings.GetString("menu_expand_all_selected"),
				null, mnuExpandAllSelected_Click);
			mnuSize = new ToolStripMenuItem(Strings.GetString("menu_size"), null,
				mnuAutoWidth,
				mnuAutoHeight,
				new ToolStripSeparator(),
				mnuCollapseAllSelected,
				mnuExpandAllSelected
			);

			MenuList.AddRange(ShapeContextMenu.Default.MenuItems);
			MenuList.AddRange(new ToolStripItem[] {
				mnuSize,
				new ToolStripSeparator(),
				mnuEditMembers,
			});
		}

		private void mnuAutoWidth_Click(object sender, EventArgs e)
		{
			if (Diagram != null)
				Diagram.AutoWidthOfSelectedShapes();
		}

		private void mnuAutoHeight_Click(object sender, EventArgs e)
		{
			if (Diagram != null)
				Diagram.AutoHeightOfSelectedShapes();
		}

		private void mnuCollapseAllSelected_Click(object sender, EventArgs e)
		{
			if (Diagram != null)
				Diagram.CollapseAll(true);
		}

		private void mnuExpandAllSelected_Click(object sender, EventArgs e)
		{
			if (Diagram != null)
				Diagram.ExpandAll(true);
		}

		private void mnuEditMembers_Click(object sender, EventArgs e)
		{
			if (Diagram != null) {
				TypeShape typsShape = Diagram.FirstSelectedElement as TypeShape;
				if (typsShape != null)
					typsShape.EditMembers();
			}
		}
	}
}
