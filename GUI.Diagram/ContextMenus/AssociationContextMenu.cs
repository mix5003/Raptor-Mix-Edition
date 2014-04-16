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
using NClass.Core;
using NClass.Translations;

namespace NClass.GUI.Diagram
{
	internal sealed class AssociationContextMenu : ContextMenu
	{
		static AssociationContextMenu _default;

		ToolStripMenuItem mnuDirection, mnuUnidirectional, mnuBidirectional;
		ToolStripMenuItem mnuType, mnuAssociation, mnuComposition, mnuAggregation;

		static AssociationContextMenu()
		{
			_default = new AssociationContextMenu();
			Strings.LanguageChanged += delegate { _default.UpdateTexts(); };
		}

		private AssociationContextMenu()
		{
			InitMenuItems();
		}

		public static AssociationContextMenu Default
		{
			get { return _default; }
		}

		private void UpdateTexts()
		{
			mnuDirection.Text = Strings.GetString("menu_direction");
			mnuUnidirectional.Text = Strings.GetString("menu_unidirectional");
			mnuBidirectional.Text = Strings.GetString("menu_bidirectional");
			mnuType.Text = Strings.GetString("menu_type");
			mnuAssociation.Text = Strings.GetString("menu_association");
			mnuComposition.Text = Strings.GetString("menu_composition");
			mnuAggregation.Text = Strings.GetString("menu_aggregation");
		}

		public override void ValidateMenuItems(IDiagram diagram)
		{
			base.ValidateMenuItems(diagram);
			ConnectionContextMenu.Default.ValidateMenuItems(diagram);
		}

		private void InitMenuItems()
		{
			mnuUnidirectional = new ToolStripMenuItem(Strings.GetString("menu_unidirectional"),
				Properties.Resources.Unidirectional, mnuUnidirectional_Click);
			mnuBidirectional = new ToolStripMenuItem(Strings.GetString("menu_bidirectional"),
				Properties.Resources.Bidirectional, mnuBidirectional_Click);
			mnuDirection = new ToolStripMenuItem(Strings.GetString("menu_direction"), null,
				mnuUnidirectional,
				mnuBidirectional
			);

			mnuAssociation = new ToolStripMenuItem(Strings.GetString("menu_association"),
				Properties.Resources.Association, mnuAssociation_Click);
			mnuComposition = new ToolStripMenuItem(Strings.GetString("menu_composition"),
				Properties.Resources.Composition, mnuComposition_Click);
			mnuAggregation = new ToolStripMenuItem(Strings.GetString("menu_aggregation"),
				Properties.Resources.Aggregation, mnuAggregation_Click);
			mnuType = new ToolStripMenuItem(Strings.GetString("menu_type"), null,
				mnuAssociation,
				mnuComposition,
				mnuAggregation
			);

			MenuList.AddRange(ConnectionContextMenu.Default.MenuItems);
			MenuList.InsertRange(5, new ToolStripItem[] {
				new ToolStripSeparator(),
				mnuDirection,
				mnuType,
			});
		}

		private void mnuUnidirectional_Click(object sender, EventArgs e)
		{
			if (Diagram != null) {
				foreach (Association association in Diagram.GetSelectedElements())
					association.AssociationRelation.Direction = Direction.Unidirectional;
			}
		}

		private void mnuBidirectional_Click(object sender, EventArgs e)
		{
			if (Diagram != null) {
				foreach (Association association in Diagram.GetSelectedElements())
					association.AssociationRelation.Direction = Direction.Bidirectional;
			}
		}

		private void mnuAssociation_Click(object sender, EventArgs e)
		{
			if (Diagram != null) {
				foreach (Association association in Diagram.GetSelectedElements()) {
					association.AssociationRelation.IsComposition = false;
					association.AssociationRelation.IsAggregation = false;
				}
			}
		}

		private void mnuComposition_Click(object sender, EventArgs e)
		{
			if (Diagram != null) {
				foreach (Association association in Diagram.GetSelectedElements())
					association.AssociationRelation.IsComposition = true;
			}
		}

		private void mnuAggregation_Click(object sender, EventArgs e)
		{
			if (Diagram != null) {
				foreach (Association association in Diagram.GetSelectedElements())
					association.AssociationRelation.IsAggregation = true;
			}
		}
	}
}
