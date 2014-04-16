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
using NClass.Translations;

namespace NClass.GUI
{
	public partial class DiagramSizeDialog : Form
	{
		public DiagramSizeDialog(Size diagramSize, Size minSize)
		{
			InitializeComponent();
			numWidth.Minimum = minSize.Width;
			numWidth.Value = diagramSize.Width;
			numHeight.Minimum = minSize.Height;
			numHeight.Value = diagramSize.Height;
		}

		public Size DiagramSize
		{
			get
			{
				return new Size((int) numWidth.Value, (int) numHeight.Value);
			}
		}

		private void UpdateTexts()
		{
			this.Text = Strings.GetString("diagram_size");
			lblDescription.Text = Strings.GetString("diagram_size_dialog_description");
			lblWidth.Text = Strings.GetString("diagram_width");
			lblHeight.Text = Strings.GetString("diagram_height");
			btnOK.Text = Strings.GetString("button_ok");
			btnCancel.Text = Strings.GetString("button_cancel");

			if (lblDescription.Right + 20 > this.Width)
				this.Width = lblDescription.Right + 20;
			int maxRight = Math.Max(lblWidth.Right, lblHeight.Right) + 6;
			if (numWidth.Left < maxRight)
				numWidth.Left = maxRight;
			if (numHeight.Left < maxRight)
				numHeight.Left = maxRight;
		}

		protected override void OnLoad(EventArgs e)
		{
			base.OnLoad(e);
			UpdateTexts();
		}
	}
}