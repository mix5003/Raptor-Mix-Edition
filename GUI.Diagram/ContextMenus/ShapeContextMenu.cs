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
	internal sealed class ShapeContextMenu : ContextMenu
	{
		static ShapeContextMenu _default;

		ToolStripMenuItem mnuAlign;
		ToolStripMenuItem mnuAlignTop, mnuAlignLeft, mnuAlignBottom, mnuAlignRight;
		ToolStripMenuItem mnuAlignHorizontal, mnuAlignVertical;
		ToolStripMenuItem mnuMakeSameSize;
		ToolStripMenuItem mnuSameWidth, mnuSameHeight, mnuSameSize;

		static ShapeContextMenu()
		{
			_default = new ShapeContextMenu();
			Strings.LanguageChanged += delegate { _default.UpdateTexts(); };
		}

		private ShapeContextMenu()
		{
			InitMenuItems();
		}

		public static ShapeContextMenu Default
		{
			get { return _default; }
		}

		private void UpdateTexts()
		{
			mnuAlign.Text = Strings.GetString("menu_align");
			mnuAlignTop.Text = Strings.GetString("menu_align_top");
			mnuAlignLeft.Text = Strings.GetString("menu_align_left");
			mnuAlignBottom.Text = Strings.GetString("menu_align_bottom");
			mnuAlignRight.Text = Strings.GetString("menu_align_right");
			mnuAlignHorizontal.Text = Strings.GetString("menu_align_horizontal");
			mnuAlignVertical.Text = Strings.GetString("menu_align_vertical");
			mnuMakeSameSize.Text = Strings.GetString("menu_make_same_size");
			mnuSameWidth.Text = Strings.GetString("menu_same_width");
			mnuSameHeight.Text = Strings.GetString("menu_same_height");
			mnuSameSize.Text = Strings.GetString("menu_same_size");
		}

		public override void ValidateMenuItems(IDiagram diagram)
		{
			base.ValidateMenuItems(diagram);
			GeneralContextMenu.Default.ValidateMenuItems(diagram);

			bool multiSelection = diagram.MultipleSelection;
			mnuAlign.Enabled = multiSelection;
			mnuAlignTop.Enabled = multiSelection;
			mnuAlignLeft.Enabled = multiSelection;
			mnuAlignBottom.Enabled = multiSelection;
			mnuAlignRight.Enabled = multiSelection;
			mnuAlignHorizontal.Enabled = multiSelection;
			mnuAlignVertical.Enabled = multiSelection;
			mnuMakeSameSize.Enabled = multiSelection;
			mnuSameWidth.Enabled = multiSelection;
			mnuSameHeight.Enabled = multiSelection;
			mnuSameSize.Enabled = multiSelection;
		}

		private void InitMenuItems()
		{
			mnuAlignTop = new ToolStripMenuItem(Strings.GetString("menu_align_top"),
				Properties.Resources.AlignTop, mnuAlignTop_Click);
			mnuAlignLeft = new ToolStripMenuItem(Strings.GetString("menu_align_left"),
				Properties.Resources.AlignLeft, mnuAlignLeft_Click);
			mnuAlignBottom = new ToolStripMenuItem(Strings.GetString("menu_align_bottom"),
				Properties.Resources.AlignBottom, mnuAlignBottom_Click);
			mnuAlignRight = new ToolStripMenuItem(Strings.GetString("menu_align_right"),
				Properties.Resources.AlignRight, mnuAlignRight_Click);
			mnuAlignHorizontal = new ToolStripMenuItem(Strings.GetString("menu_align_horizontal"),
				Properties.Resources.AlignHorizontal, mnuAlignHorizontal_Click);
			mnuAlignVertical = new ToolStripMenuItem(Strings.GetString("menu_align_vertical"),
				Properties.Resources.AlignVertical, mnuAlignVertical_Click);
			mnuAlign = new ToolStripMenuItem(Strings.GetString("menu_align"), null,
				mnuAlignTop,
				mnuAlignLeft,
				mnuAlignBottom,
				mnuAlignRight,
				new ToolStripSeparator(),
				mnuAlignHorizontal,
				mnuAlignVertical
			);

			mnuSameWidth = new ToolStripMenuItem(Strings.GetString("menu_same_width"),
				null, mnuSameWidth_Click);
			mnuSameHeight = new ToolStripMenuItem(Strings.GetString("menu_same_height"),
				null, mnuSameHeight_Click);
			mnuSameSize = new ToolStripMenuItem(Strings.GetString("menu_same_size"),
				null, mnuSameSize_Click);
			mnuMakeSameSize = new ToolStripMenuItem(Strings.GetString("menu_make_same_size"), null,
				mnuSameWidth,
				mnuSameHeight,
				mnuSameSize
			);

			MenuList.AddRange(GeneralContextMenu.Default.MenuItems);
			MenuList.AddRange(new ToolStripItem[] {
				new ToolStripSeparator(),
				mnuAlign,
				mnuMakeSameSize,
			});
		}

		private void mnuAlignTop_Click(object sender, EventArgs e)
		{
			if (Diagram != null)
				Diagram.AlignTop();
		}

		private void mnuAlignLeft_Click(object sender, EventArgs e)
		{
			if (Diagram != null)
				Diagram.AlignLeft();
		}

		private void mnuAlignBottom_Click(object sender, EventArgs e)
		{
			if (Diagram != null)
				Diagram.AlignBottom();
		}

		private void mnuAlignRight_Click(object sender, EventArgs e)
		{
			if (Diagram != null)
				Diagram.AlignRight();
		}

		private void mnuAlignHorizontal_Click(object sender, EventArgs e)
		{
			if (Diagram != null)
				Diagram.AlignHorizontal();
		}

		private void mnuAlignVertical_Click(object sender, EventArgs e)
		{
			if (Diagram != null)
				Diagram.AlignVertical();
		}

		private void mnuSameWidth_Click(object sender, EventArgs e)
		{
			if (Diagram != null)
				Diagram.AdjustToSameWidth();
		}

		private void mnuSameHeight_Click(object sender, EventArgs e)
		{
			if (Diagram != null)
				Diagram.AdjustToSameHeight();
		}

		private void mnuSameSize_Click(object sender, EventArgs e)
		{
			if (Diagram != null)
				Diagram.AdjustToSameSize();
		}
	}
}
