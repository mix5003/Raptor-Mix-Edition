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
	public sealed class GeneralContextMenu : ContextMenu
	{
		static GeneralContextMenu _default;

		ToolStripMenuItem mnuCut;
		ToolStripMenuItem mnuDelete;
		ToolStripMenuItem mnuCopyAsImage;
		ToolStripMenuItem mnuSaveAsImage;

		static GeneralContextMenu()
		{
			_default = new GeneralContextMenu();
			Strings.LanguageChanged += delegate { _default.UpdateTexts(); };
		}

		private GeneralContextMenu()
		{
			InitMenuItems();
		}

		public static GeneralContextMenu Default
		{
			get { return _default; }
		}

		private void UpdateTexts()
		{
			mnuCut.Text = Strings.GetString("menu_cut");
			mnuDelete.Text = Strings.GetString("menu_delete");
			mnuCopyAsImage.Text = Strings.GetString("menu_copy_image_to_clipboard");
			mnuSaveAsImage.Text = Strings.GetString("menu_save_selection_as_image");
		}

		public override void ValidateMenuItems(IDiagram diagram)
		{
			base.ValidateMenuItems(diagram);
			mnuCut.Enabled = false; //TODO: ez nem kell
		}

		private void InitMenuItems()
		{
			mnuCut = new ToolStripMenuItem(Strings.GetString("menu_cut"),
				Properties.Resources.Cut, mnuCut_Click);
			mnuDelete = new ToolStripMenuItem(Strings.GetString("menu_delete"),
				Properties.Resources.Delete, mnuDelete_Click);
			mnuCopyAsImage = new ToolStripMenuItem(
				Strings.GetString("menu_copy_image_to_clipboard"),
				Properties.Resources.CopyAsImage, mnuCopyAsImage_Click);
			mnuSaveAsImage = new ToolStripMenuItem(
				Strings.GetString("menu_save_selection_as_image"),
				Properties.Resources.Image, mnuSaveAsImage_Click);

			MenuList.AddRange(new ToolStripItem[] {
				mnuCut,
				mnuDelete,
				new ToolStripSeparator(),
				mnuCopyAsImage,
				mnuSaveAsImage,
			});
		}

		private void mnuCut_Click(object sender, EventArgs e)
		{
			//UNDONE: mnuCut_Click
		}

		private void mnuDelete_Click(object sender, EventArgs e)
		{
			if (Diagram != null)
				Diagram.DeleteSelectedElements();
		}

		private void mnuCopyAsImage_Click(object sender, EventArgs e)
		{
			if (Diagram != null)
				Diagram.CopyAsImage();
		}

		private void mnuSaveAsImage_Click(object sender, EventArgs e)
		{
			if (Diagram != null)
				Diagram.SaveAsImage(true);
		}
	}
}
