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
using System.ComponentModel;
using System.Collections.Generic;
using System.Windows.Forms;
using NClass.Core;
using NClass.Translations;

namespace NClass.GUI.Diagram
{
	public partial class MembersDialog : Form
	{
		CompositeType parent = null;
		Member member = null;
		bool locked = false;
		int attributeCount = 0;

		public event EventHandler ContentsChanged;

		public MembersDialog()
		{
			InitializeComponent();
			lstMembers.SmallImageList = Icons.IconList;
			toolStrip.Renderer = ToolStripSimplifiedRenderer.Default;
		}

		private void OnContentsChanged(EventArgs e)
		{
			if (ContentsChanged != null)
				ContentsChanged(this, e);
		}

		private void UpdateTexts()
		{
			lblSyntax.Text = Strings.GetString("syntax");
			lblName.Text = Strings.GetString("name");
			lblType.Text = Strings.GetString("type");
			lblAccess.Text = Strings.GetString("access");
			lblInitValue.Text = Strings.GetString("initial_value");
			grpOperationModifiers.Text = Strings.GetString("modifiers");
			grpFieldModifiers.Text = Strings.GetString("modifiers");
			toolNewField.Text = Strings.GetString("new_field");
			toolNewMethod.Text = Strings.GetString("new_method");
			toolNewConstructor.Text = Strings.GetString("new_constructor");
			toolNewDestructor.Text = Strings.GetString("new_destructor");
			toolNewProperty.Text = Strings.GetString("new_property");
			toolNewEvent.Text = Strings.GetString("new_event");
			toolOverrideList.Text = Strings.GetString("override_members");
			toolImplementList.Text = Strings.GetString("implementing");
			toolSortByKind.Text = Strings.GetString("sort_by_kind");
			toolSortByAccess.Text = Strings.GetString("sort_by_access");
			toolSortByName.Text = Strings.GetString("sort_by_name");
			toolMoveUp.Text = Strings.GetString("move_up");
			toolMoveDown.Text = Strings.GetString("move_down");
			toolDelete.Text = Strings.GetString("delete");
			lstMembers.Columns[1].Text = Strings.GetString("name");
			lstMembers.Columns[2].Text = Strings.GetString("type");
			lstMembers.Columns[3].Text = Strings.GetString("access");
			lstMembers.Columns[4].Text = Strings.GetString("modifiers");
			btnClose.Text = Strings.GetString("button_close");
		}

		public void ShowDialog(CompositeType parent)
		{
			if (parent == null)
				return;

			this.parent = parent;
			this.Text = Strings.GetString("members_of_type", parent.Name);

			LanguageSpecificInitialization(parent.Language);
			FillMembersList();
			if (lstMembers.Items.Count > 0) {
				lstMembers.Items[0].Focused = true;
				lstMembers.Items[0].Selected = true;
			}

			toolNewField.Visible = parent.SupportsFields;
			toolNewConstructor.Visible = parent.SupportsConstuctors;
			toolNewDestructor.Visible = parent.SupportsDestructors;
			toolNewProperty.Visible = parent.SupportsProperties;
			toolNewEvent.Visible = parent.SupportsEvents;
			toolOverrideList.Visible = parent is SingleInharitanceType;
			toolImplementList.Visible = parent is IInterfaceImplementer;
			toolImplementList.Enabled = (parent is IInterfaceImplementer) &&
				((IInterfaceImplementer) parent).ImplementsInterface;
			toolSepAddNew.Visible = parent is SingleInharitanceType ||
				parent is IInterfaceImplementer;

			errorProvider.SetError(txtSyntax, null);
			errorProvider.SetError(txtName, null);
			errorProvider.SetError(cboType, null);

			base.ShowDialog();
		}

		private void LanguageSpecificInitialization(Language language)
		{
			cboAccess.Items.Clear();
			foreach (string modifier in language.ValidAccessModifiers.Values)
				cboAccess.Items.Add(modifier);

			// chkFieldStatic
			if (language.IsValidModifier(FieldModifier.Static)) {
				chkFieldStatic.Enabled = true;
				chkFieldStatic.Text = language.ValidFieldModifiers[FieldModifier.Static];
			}
			else {
				chkFieldStatic.Enabled = false;
				chkFieldStatic.Text = "Static";
			}
			// chkReadonly
			if (language.IsValidModifier(FieldModifier.Readonly)) {
				chkReadonly.Enabled = true;
				chkReadonly.Text = language.ValidFieldModifiers[FieldModifier.Readonly];
			}
			else {
				chkReadonly.Enabled = false;
				chkReadonly.Text = "Readonly";
			}
			// chkConstant
			if (language.IsValidModifier(FieldModifier.Constant)) {
				chkConstant.Enabled = true;
				chkConstant.Text = language.ValidFieldModifiers[FieldModifier.Constant];
			}
			else {
				chkConstant.Enabled = false;
				chkConstant.Text = "Constant";
			}
			// chkFieldHider
			if (language.IsValidModifier(FieldModifier.Hider)) {
				chkFieldHider.Enabled = true;
				chkFieldHider.Text = language.ValidFieldModifiers[FieldModifier.Hider];
			}
			else {
				chkFieldHider.Enabled = false;
				chkFieldHider.Text = "Hider";
			}
			// chkVolatile
			if (language.IsValidModifier(FieldModifier.Volatile)) {
				chkVolatile.Enabled = true;
				chkVolatile.Text = language.ValidFieldModifiers[FieldModifier.Volatile];
			}
			else {
				chkVolatile.Enabled = false;
				chkVolatile.Text = "Volatile";
			}

			// chkOperationStatic
			if (language.IsValidModifier(OperationModifier.Static)) {
				chkOperationStatic.Enabled = true;
				chkOperationStatic.Text =
					language.ValidOperationModifiers[OperationModifier.Static];
			}
			else {
				chkOperationStatic.Enabled = false;
				chkOperationStatic.Text = "Static";
			}
			// chkVirtual
			if (language.IsValidModifier(OperationModifier.Virtual)) {
				chkVirtual.Enabled = true;
				chkVirtual.Text =
					language.ValidOperationModifiers[OperationModifier.Virtual];
			}
			else {
				chkVirtual.Enabled = false;
				chkVirtual.Text = "Virtual";
			}
			// chkAbstract
			if (language.IsValidModifier(OperationModifier.Abstract)) {
				chkAbstract.Enabled = true;
				chkAbstract.Text =
					language.ValidOperationModifiers[OperationModifier.Abstract];
			}
			else {
				chkAbstract.Enabled = false;
				chkAbstract.Text = "Abstract";
			}
			// chkOverride
			if (language.IsValidModifier(OperationModifier.Override)) {
				chkOverride.Enabled = true;
				chkOverride.Text =
					language.ValidOperationModifiers[OperationModifier.Override];
			}
			else {
				chkOverride.Enabled = false;
				chkOverride.Text = "Override";
			}
			// chkSealed
			if (language.IsValidModifier(OperationModifier.Sealed)) {
				chkSealed.Enabled = true;
				chkSealed.Text =
					language.ValidOperationModifiers[OperationModifier.Sealed];
			}
			else {
				chkSealed.Enabled = false;
				chkSealed.Text = "Sealed";
			}
			// chkOperationHider
			if (language.IsValidModifier(OperationModifier.Hider)) {
				chkOperationHider.Enabled = true;
				chkOperationHider.Text =
					language.ValidOperationModifiers[OperationModifier.Hider];
			}
			else {
				chkOperationHider.Enabled = false;
				chkOperationHider.Text = "Hider";
			}
		}

		private void FillMembersList()
		{
			lstMembers.Items.Clear();
			attributeCount = 0;

			foreach (Field field in parent.Fields)
				AddFieldToList(field);

			foreach (Operation operation in parent.Operations)
				AddOperationToList(operation);

			DisableFields();
		}

		/// <exception cref="ArgumentNullException">
		/// <paramref name="field"/> is null.
		/// </exception>
		private ListViewItem AddFieldToList(Field field)
		{
			if (field == null)
				throw new ArgumentNullException("field");

			ListViewItem item = lstMembers.Items.Insert(attributeCount, "");

			item.Tag = field;
			item.ImageIndex = Icons.GetImageIndex(field);
			item.SubItems.Add(field.Name);
			item.SubItems.Add(field.Type);
			item.SubItems.Add(parent.Language.GetAccessString(field.AccessModifier));
			item.SubItems.Add(parent.Language.GetFieldModifierString(field.Modifier));
			item.SubItems.Add("");
			attributeCount++;

			return item;
		}

		/// <exception cref="ArgumentNullException">
		/// <paramref name="operation"/> is null.
		/// </exception>
		private ListViewItem AddOperationToList(Operation operation)
		{
			if (operation == null)
				throw new ArgumentNullException("operation");

			ListViewItem item = lstMembers.Items.Add("");

			item.Tag = operation;
			item.ImageIndex = Icons.GetImageIndex(operation);
			item.SubItems.Add(operation.Name);
			item.SubItems.Add(operation.Type);
			item.SubItems.Add(parent.Language.GetAccessString(operation.AccessModifier));
			item.SubItems.Add(parent.Language.GetOperationModifierString(operation.Modifier));

			return item;
		}

		private void ShowNewMember(Member actualMember)
		{
			if (locked || actualMember == null)
				return;
			else
				member = actualMember;

			RefreshValues();
		}

		private void RefreshValues()
		{
			if (member == null)
				return;

			locked = true;

			txtSyntax.Enabled = true;
			txtName.Enabled = true;
			txtSyntax.ReadOnly = (member is Destructor);
			txtName.ReadOnly = (member == null || member.IsNameReadonly);
			cboType.Enabled = (member != null && !member.IsTypeReadonly);
			cboAccess.Enabled = (member != null && member.CanSetAccess);
			txtInitialValue.Enabled = (member is Field);
			toolSortByKind.Enabled = true;
			toolSortByAccess.Enabled = true;
			toolSortByName.Enabled = true;

			if (lstMembers.Items.Count > 0) {
				toolSortByKind.Enabled = true;
				toolSortByAccess.Enabled = true;
				toolSortByName.Enabled = true;
			}

			txtSyntax.Text = member.ToString();
			txtName.Text = member.Name;
			cboType.Text = member.Type;

			// Access selection
			cboAccess.SelectedItem = member.Language.ValidAccessModifiers[member.AccessModifier];

			if (member is Field) {
				Field field = (Field) member;

				grpFieldModifiers.Enabled = true;
				grpFieldModifiers.Visible = true;
				grpOperationModifiers.Visible = false;

				chkFieldStatic.Checked = field.IsStatic;
				chkReadonly.Checked = field.IsReadonly;
				chkConstant.Checked = field.IsConstant;
				chkFieldHider.Checked = field.IsHider;
				chkVolatile.Checked = field.IsVolatile;
			}
			else if (member is Operation) {
				Operation operation = (Operation) member;

				grpOperationModifiers.Enabled = true;
				grpOperationModifiers.Visible = true;
				grpFieldModifiers.Visible = false;

				chkOperationStatic.Checked = operation.IsStatic;
				chkVirtual.Checked = operation.IsVirtual;
				chkAbstract.Checked = operation.IsAbstract;
				chkOverride.Checked = operation.IsOverride;
				chkSealed.Checked = operation.IsSealed;
				chkOperationHider.Checked = operation.IsHider;
			}

			RefreshMembersList();

			locked = false;

			errorProvider.SetError(txtSyntax, null);
			errorProvider.SetError(txtName, null);
			errorProvider.SetError(cboType, null);
			errorProvider.SetError(cboAccess, null);
		}

		private void RefreshMembersList()
		{
			ListViewItem item = null;

			if (lstMembers.FocusedItem != null)
				item = lstMembers.FocusedItem;
			else if (lstMembers.SelectedItems.Count > 0)
				item = lstMembers.SelectedItems[0];

			if (item != null && member != null) {
				item.ImageIndex = Icons.GetImageIndex(member);
				item.SubItems[1].Text = txtName.Text;
				item.SubItems[2].Text = cboType.Text;
				item.SubItems[3].Text = cboAccess.Text;
				if (member is Field) {
					item.SubItems[4].Text = member.Language.GetFieldModifierString(
						((Field) member).Modifier);
				}
				else if (member is Operation) {
					item.SubItems[4].Text = member.Language.GetOperationModifierString(
						((Operation) member).Modifier);
				}
			}
		}

		private void DisableFields()
		{
			member = null;

			locked = true;

			txtSyntax.Text = null;
			txtName.Text = null;
			cboType.Text = null;
			cboAccess.Text = null;
			txtInitialValue.Text = null;

			txtSyntax.Enabled = false;
			txtName.Enabled = false;
			cboType.Enabled = false;
			cboAccess.Enabled = false;
			txtInitialValue.Enabled = false;

			grpFieldModifiers.Enabled = false;
			grpOperationModifiers.Enabled = false;

			if (lstMembers.Items.Count == 0) {
				toolSortByKind.Enabled = false;
				toolSortByAccess.Enabled = false;
				toolSortByName.Enabled = false;
			}
			toolMoveUp.Enabled = false;
			toolMoveDown.Enabled = false;
			toolDelete.Enabled = false;

			locked = false;
		}

		private void SwapListItems(ListViewItem item1, ListViewItem item2)
		{
			int image = item1.ImageIndex;
			item1.ImageIndex = item2.ImageIndex;
			item2.ImageIndex = image;

			object tag = item1.Tag;
			item1.Tag = item2.Tag;
			item2.Tag = tag;

			for (int i = 0; i < item1.SubItems.Count; i++) {
				string text = item1.SubItems[i].Text;
				item1.SubItems[i].Text = item2.SubItems[i].Text;
				item2.SubItems[i].Text = text;
			}
			OnContentsChanged(EventArgs.Empty);
		}

		private void DeleteSelectedMember()
		{
			if (lstMembers.SelectedItems.Count > 0) {
				ListViewItem item = lstMembers.SelectedItems[0];
				int index = item.Index;

				if (item.Tag is Field)
					attributeCount--;
                if ((item.Tag is Method) && ((item.Tag as Method).Parent is ClassType))
                {
                    NClass.Core.ProjectCore.raptorUpdater.deleteMethod(
                        ((item.Tag as Method).Parent as ClassType).raptorTab,
                        (item.Tag as Method).raptorTab);
                }
                if ((item.Tag is Field) && ((item.Tag as Field).Parent is ClassType))
                {
                    NClass.Core.ProjectCore.raptorUpdater.resetAttributes(
                        ((item.Tag as Field).Parent as ClassType).raptorTab,
                        (item.Tag as Field).Parent.Fields);
                }
				parent.RemoveMember(item.Tag as Member);
				lstMembers.Items.Remove(item);
				OnContentsChanged(EventArgs.Empty);

				int count = lstMembers.Items.Count;
				if (count > 0) {
					if (index >= count)
						index = count - 1;
					lstMembers.Items[index].Selected = true;
				}
				else {
					DisableFields();
				}
			}
		}

		protected override void OnLoad(EventArgs e)
		{
			base.OnLoad(e);
			UpdateTexts();
			errorProvider.SetError(grpFieldModifiers, null);
			errorProvider.SetError(grpOperationModifiers, null);
		}

		private void PropertiesDialog_KeyDown(object sender, KeyEventArgs e)
		{
			if (e.KeyCode == Keys.Escape)
				RefreshValues();
			else if (e.KeyCode == Keys.Enter)
				lstMembers.Focus();
		}

		private void txtSyntax_Validating(object sender, CancelEventArgs e)
		{
			if (!locked && member != null) {
				try {
					string oldValue = member.ToString();

					member.InitFromString(txtSyntax.Text);
					errorProvider.SetError(txtSyntax, null);

					RefreshValues();
                    if (oldValue != txtSyntax.Text)
                    {
                        if (member is Method)
                        {
                            Method method = member as Method;
                            int num_params;
                            string[] param_names;
                            bool[] param_is_input;
                            bool[] param_is_output;
                            method.getParameters(out num_params, out param_names, 
                                out param_is_input, out param_is_output);
                            if (method.Parent is ClassType && !method.IsAbstract)
                            {
                                NClass.Core.ProjectCore.raptorUpdater.changeParameters(
                                    (method.Parent as ClassType).raptorTab,
                                    method.raptorTab, num_params, param_names, param_is_input, param_is_output);
                            }
                        }
                        OnContentsChanged(EventArgs.Empty);
                    }
				}
				catch (BadSyntaxException ex) {
					e.Cancel = true;
					errorProvider.SetError(txtSyntax, ex.Message);
				}
			}
		}

		private void txtName_Validating(object sender, CancelEventArgs e)
		{
			if (!locked && member != null) {
				try {
					string oldValue = member.Name;

					member.Name = txtName.Text;
					errorProvider.SetError(txtName, null);
                    if (member is Method && oldValue!=txtName.Text)
                    {
                        if ((member as Method).Parent is ClassType)
                        {
                            NClass.Core.ProjectCore.raptorUpdater.renameMethod(
                                ((member as Method).Parent as ClassType).raptorTab,
                                (member as Method).raptorTab,
                                member.Name);
                        }
                    }
                    if (member is Field && oldValue != txtName.Text)
                    {
                        if ((member as Field).Parent is ClassType)
                        {
                            NClass.Core.ProjectCore.raptorUpdater.resetAttributes(
                                ((member as Field).Parent as ClassType).raptorTab,
                                (member as Field).Parent.Fields);
                        }
                    }
					RefreshValues();
					if (oldValue != txtName.Text)
						OnContentsChanged(EventArgs.Empty);
				}
				catch (BadSyntaxException ex) {
					e.Cancel = true;
					errorProvider.SetError(txtName, ex.Message);
				}
			}
		}

		private void cboType_Validating(object sender, CancelEventArgs e)
		{
			if (!locked && member != null) {
				try {
					string oldValue = member.Type;

					member.Type = cboType.Text;
					if (!cboType.Items.Contains(cboType.Text))
						cboType.Items.Add(cboType.Text);
					errorProvider.SetError(cboType, null);
					cboType.Select(0, 0);

					RefreshValues();
					if (oldValue != cboType.Text)
						OnContentsChanged(EventArgs.Empty);
				}
				catch (BadSyntaxException ex) {
					e.Cancel = true;
					errorProvider.SetError(cboType, ex.Message);
				}
			}
		}

		private void cboAccess_SelectedIndexChanged(object sender, EventArgs e)
		{
			int index = cboAccess.SelectedIndex;

			if (!locked && member != null) {
				try {
					string selectedModifierString = cboAccess.SelectedItem.ToString();

					foreach (AccessModifier modifier in member.Language.ValidAccessModifiers.Keys)
					{
						if (member.Language.ValidAccessModifiers[modifier] == selectedModifierString)
						{
							member.AccessModifier = modifier;
							RefreshValues();
							OnContentsChanged(EventArgs.Empty);
							break;
						}
					}
				}
				catch (BadSyntaxException ex) {
					errorProvider.SetError(cboAccess, ex.Message);
				}
			}
		}

		private void cboAccess_Validated(object sender, EventArgs e)
		{
			if (!string.IsNullOrEmpty(errorProvider.GetError(cboAccess))) {
				errorProvider.SetError(cboAccess, null);
				RefreshValues();
			}
		}

		private void chkFieldStatic_CheckedChanged(object sender, EventArgs e)
		{
			if (!locked && member is Field) {
				try {
					((Field) member).IsStatic = chkFieldStatic.Checked;
					RefreshValues();
					errorProvider.SetError(grpFieldModifiers, null);
					OnContentsChanged(EventArgs.Empty);
				}
				catch (BadSyntaxException ex) {
					RefreshValues();
					errorProvider.SetError(grpFieldModifiers, ex.Message);
				}
			}
		}

		private void chkReadonly_CheckedChanged(object sender, EventArgs e)
		{
			if (!locked && member is Field) {
				try {
					((Field) member).IsReadonly = chkReadonly.Checked;
					RefreshValues();
					errorProvider.SetError(grpFieldModifiers, null);
					OnContentsChanged(EventArgs.Empty);
				}
				catch (BadSyntaxException ex) {
					RefreshValues();
					errorProvider.SetError(grpFieldModifiers, ex.Message);
				}
			}
		}

		private void chkConstant_CheckedChanged(object sender, EventArgs e)
		{
			if (!locked && member is Field) {
				try {
					((Field) member).IsConstant = chkConstant.Checked;
					RefreshValues();
					errorProvider.SetError(grpFieldModifiers, null);
					OnContentsChanged(EventArgs.Empty);
				}
				catch (BadSyntaxException ex) {
					RefreshValues();
					errorProvider.SetError(grpFieldModifiers, ex.Message);
				}
			}
		}

		private void chkFieldHider_CheckedChanged(object sender, EventArgs e)
		{
			if (!locked && member is Field) {
				try {
					((Field) member).IsHider = chkFieldHider.Checked;
					RefreshValues();
					errorProvider.SetError(grpFieldModifiers, null);
					OnContentsChanged(EventArgs.Empty);
				}
				catch (BadSyntaxException ex) {
					RefreshValues();
					errorProvider.SetError(grpFieldModifiers, ex.Message);
				}
			}
		}

		private void chkVolatile_CheckedChanged(object sender, EventArgs e)
		{
			if (!locked && member is Field) {
				try {
					((Field) member).IsVolatile = chkVolatile.Checked;
					RefreshValues();
					errorProvider.SetError(grpFieldModifiers, null);
					OnContentsChanged(EventArgs.Empty);
				}
				catch (BadSyntaxException ex) {
					RefreshValues();
					errorProvider.SetError(grpFieldModifiers, ex.Message);
				}
			}
		}

		private void chkOperationStatic_CheckedChanged(object sender, EventArgs e)
		{
			if (!locked && member is Operation) {
				try {
					((Operation) member).IsStatic = chkOperationStatic.Checked;
					RefreshValues();
					errorProvider.SetError(grpOperationModifiers, null);
					OnContentsChanged(EventArgs.Empty);
				}
				catch (BadSyntaxException ex) {
					RefreshValues();
					errorProvider.SetError(grpOperationModifiers, ex.Message);
				}
			}
		}

		private void chkVirtual_CheckedChanged(object sender, EventArgs e)
		{
			if (!locked && member is Operation) {
				try {
					((Operation) member).IsVirtual = chkVirtual.Checked;
					RefreshValues();
					errorProvider.SetError(grpOperationModifiers, null);
					OnContentsChanged(EventArgs.Empty);
				}
				catch (BadSyntaxException ex) {
					RefreshValues();
					errorProvider.SetError(grpOperationModifiers, ex.Message);
				}
			}
		}

		private void chkAbstract_CheckedChanged(object sender, EventArgs e)
		{
			if (!locked && member is Operation) {
				try {
					if (parent is ClassType &&
						((ClassType) parent).Modifier != ClassModifier.Abstract)
					{
						DialogResult result = MessageBox.Show(
							Strings.GetString("abstract_changing"), Strings.GetString("confirmation"),
							MessageBoxButtons.YesNo, MessageBoxIcon.Warning);

						if (result == DialogResult.No) {
							RefreshValues();
							return;
						}
					}
                    if (member is Method && parent is ClassType)
                    {
                        if (chkAbstract.Checked &&
                            (!((Method)member).IsAbstract))
                        {
                            if (!NClass.Core.ProjectCore.raptorUpdater.makeAbstract(
                                (parent as ClassType).raptorTab,
                                (member as Method).raptorTab))
                            {
                                chkAbstract.Checked = false;
                            }
                        }
                        else if (!chkAbstract.Checked &&
                            ((Method)member).IsAbstract)
                        {
                            (member as Method).raptorTab = NClass.Core.ProjectCore.raptorUpdater.createMethod(
                                (parent as ClassType).raptorTab,
                                member.Name,
                                member as Method);
                        }
                    }

					((Operation) member).IsAbstract = chkAbstract.Checked;
					RefreshValues();
					errorProvider.SetError(grpOperationModifiers, null);
					OnContentsChanged(EventArgs.Empty);
				}
				catch (BadSyntaxException ex) {
					RefreshValues();
					errorProvider.SetError(grpOperationModifiers, ex.Message);
				}
			}
		}

		private void chkOperationHider_CheckedChanged(object sender, EventArgs e)
		{
			if (!locked && member is Operation) {
				try {
					((Operation) member).IsHider = chkOperationHider.Checked;
					RefreshValues();
					errorProvider.SetError(grpOperationModifiers, null);
					OnContentsChanged(EventArgs.Empty);
				}
				catch (BadSyntaxException ex) {
					RefreshValues();
					errorProvider.SetError(grpOperationModifiers, ex.Message);
				}
			}
		}

		private void chkOverride_CheckedChanged(object sender, EventArgs e)
		{
			if (!locked && member is Operation) {
				try {
					((Operation) member).IsOverride = chkOverride.Checked;
					RefreshValues();
					errorProvider.SetError(grpOperationModifiers, null);
					OnContentsChanged(EventArgs.Empty);
				}
				catch (BadSyntaxException ex) {
					RefreshValues();
					errorProvider.SetError(grpOperationModifiers, ex.Message);
				}
			}
		}

		private void chkSealed_CheckedChanged(object sender, EventArgs e)
		{
			if (!locked && member is Operation) {
				try {
					((Operation) member).IsSealed = chkSealed.Checked;
					RefreshValues();
					errorProvider.SetError(grpOperationModifiers, null);
					OnContentsChanged(EventArgs.Empty);
				}
				catch (BadSyntaxException ex) {
					RefreshValues();
					errorProvider.SetError(grpOperationModifiers, ex.Message);
				}
			}
		}

		private void grpFieldModifiers_Validated(object sender, EventArgs e)
		{
			errorProvider.SetError(grpFieldModifiers, null);
		}

		private void grpOperationModifiers_Validated(object sender, EventArgs e)
		{
			errorProvider.SetError(grpOperationModifiers, null);
		}

		private void txtInitialValue_Validating(object sender, CancelEventArgs e)
		{
			if (!locked && member is Field) {
				if (txtInitialValue.Text.Length > 0 && txtInitialValue.Text[0] == '"' &&
					!txtInitialValue.Text.EndsWith("\""))
				{
					txtInitialValue.Text += '"';
				}
				string oldValue = ((Field) member).InitialValue;
				((Field) member).InitialValue = txtInitialValue.Text;

				RefreshValues();
				if (oldValue != txtInitialValue.Text)
					OnContentsChanged(EventArgs.Empty);
			}
		}

		private void lstMembers_ItemSelectionChanged(object sender,
			ListViewItemSelectionChangedEventArgs e)
		{
			if (e.IsSelected && e.Item.Tag is Member) {
				ShowNewMember((Member) e.Item.Tag);

				toolDelete.Enabled = true;
				if (e.ItemIndex < attributeCount) {
					toolMoveUp.Enabled = (e.ItemIndex > 0);
					toolMoveDown.Enabled = (e.ItemIndex < attributeCount - 1);
				}
				else {
					toolMoveUp.Enabled = (e.ItemIndex > attributeCount);
					toolMoveDown.Enabled = (e.ItemIndex < lstMembers.Items.Count - 1);
				}
			}
			else {
				toolMoveUp.Enabled = false;
				toolMoveDown.Enabled = false;
				toolDelete.Enabled = false;
			}
		}

		private void lstMembers_KeyDown(object sender, KeyEventArgs e)
		{
			if (e.KeyCode == Keys.Delete)
				DeleteSelectedMember();
		}

		private void toolNewField_Click(object sender, EventArgs e)
		{
			if (parent.SupportsFields) {
				Field field = parent.AddField();
				ListViewItem item = AddFieldToList(field);

				OnContentsChanged(EventArgs.Empty);
				item.Focused = true;
				item.Selected = true;
				txtName.SelectAll();
				txtName.Focus();
			}
		}

		private void toolNewMethod_Click(object sender, EventArgs e)
		{
			Method method = parent.AddMethod();
			ListViewItem item = AddOperationToList(method);
            if (method.Parent is ClassType)
            {
                method.raptorTab =
                    NClass.Core.ProjectCore.raptorUpdater.createMethod((method.Parent as ClassType).raptorTab, 
                    method.Name,
                    method);
            }
			OnContentsChanged(EventArgs.Empty);
			item.Focused = true;
			item.Selected = true;
			txtName.SelectAll();
			txtName.Focus();
		}

		private void toolNewProperty_Click(object sender, EventArgs e)
		{
			if (parent.SupportsProperties) {
				Property property = parent.AddProperty();
				ListViewItem item = AddOperationToList(property);

				OnContentsChanged(EventArgs.Empty);
				item.Focused = true;
				item.Selected = true;
				txtName.SelectAll();
				txtName.Focus();
			}
		}

		private void toolNewEvent_Click(object sender, EventArgs e)
		{
			if (parent.SupportsEvents) {
				Event _event = parent.AddEvent();
				ListViewItem item = AddOperationToList(_event);

				OnContentsChanged(EventArgs.Empty);
				item.Focused = true;
				item.Selected = true;
				txtName.SelectAll();
				txtName.Focus();
			}
		}

		private void toolNewConstructor_Click(object sender, EventArgs e)
		{
			if (parent.SupportsConstuctors) {
				Method constructor = parent.AddConstructor();
				ListViewItem item = AddOperationToList(constructor);
                if (constructor.Parent is ClassType)
                {
                    constructor.raptorTab =
                        NClass.Core.ProjectCore.raptorUpdater.createMethod(
                        (constructor.Parent as ClassType).raptorTab, 
                        constructor.Name,
                        constructor);
                }
				OnContentsChanged(EventArgs.Empty);
				item.Focused = true;
				item.Selected = true;
			}
		}

		private void toolNewDestructor_Click(object sender, EventArgs e)
		{
			if (parent.SupportsDestructors) {
				Method destructor = parent.AddDestructor();
				ListViewItem item = AddOperationToList(destructor);

				OnContentsChanged(EventArgs.Empty);
				item.Focused = true;
				item.Selected = true;
			}
		}

		private void toolOverrideList_Click(object sender, EventArgs e)
		{
			if (parent is SingleInharitanceType) {
				SingleInharitanceType derivedType = (SingleInharitanceType) parent;
				using (OverrideDialog dialog = new OverrideDialog())
				{
					if (dialog.ShowDialog(derivedType) == DialogResult.OK) {
						foreach (Operation operation in dialog.GetSelectedOperations()) {
							Operation overridden = (derivedType).Override(operation);
							AddOperationToList(overridden);
                            checkAddMethod(overridden);
						}
						OnContentsChanged(EventArgs.Empty);
					}
				}
			}
		}

		private void toolImplementList_Click(object sender, EventArgs e)
		{
			if (parent is IInterfaceImplementer) {
				using (ImplementDialog dialog = new ImplementDialog())
				{
					if (dialog.ShowDialog(parent as IInterfaceImplementer) == DialogResult.OK) {
						foreach (Operation operation in dialog.GetSelectedOperations()) {
							Implement((IInterfaceImplementer) parent, operation,
								dialog.ImplementExplicitly);
						}
						OnContentsChanged(EventArgs.Empty);
					}
				}
			}
		}

		private void Implement(IInterfaceImplementer parent, Operation operation,
			bool mustExplicit)
		{
			if (parent == null || operation == null)
				return;

			Operation defined = parent.GetDefinedOperation(operation);
			if (!operation.Language.SupportsExplicitImplementation)
				mustExplicit = false;

			if (defined == null) {
				Operation implemented = parent.Implement(operation, mustExplicit);
				AddOperationToList(implemented);
                checkAddMethod(implemented);
			}
			else if (defined.Type != operation.Type) {
				Operation implemented = parent.Implement(operation, true);
				AddOperationToList(implemented);
                checkAddMethod(implemented);
            }
		}

        private static void checkAddMethod(Operation implemented)
        {
            if (implemented is Method && implemented.Parent is ClassType)
            {
                Method implemented_method = implemented as Method;
                implemented_method.raptorTab =
                    NClass.Core.ProjectCore.raptorUpdater.createMethod(
                    (implemented_method.Parent as ClassType).raptorTab,
                    implemented_method.Name,
                    implemented_method);
            }
        }

		private void toolSortByKind_Click(object sender, EventArgs e)
		{
			parent.SortMembers(SortingMode.ByKind);
			FillMembersList();
            if (parent is ClassType)
            {
                reorderRaptor();
            }
            OnContentsChanged(EventArgs.Empty);
		}

		private void toolSortByAccess_Click(object sender, EventArgs e)
		{
			parent.SortMembers(SortingMode.ByAccess);
			FillMembersList();
            if (parent is ClassType)
            {
                reorderRaptor();
            }
            OnContentsChanged(EventArgs.Empty);
		}

		private void toolSortByName_Click(object sender, EventArgs e)
		{
			parent.SortMembers(SortingMode.ByName);
			FillMembersList();
            if (parent is ClassType)
            {
                reorderRaptor();
            }
            OnContentsChanged(EventArgs.Empty);
		}

        private void reorderRaptor()
        {
            NClass.Core.ProjectCore.raptorUpdater.reorderMethods(
                (parent as ClassType).raptorTab,
                parent.Operations);
            NClass.Core.ProjectCore.raptorUpdater.resetAttributes(
                (parent as ClassType).raptorTab,
                parent.Fields);
        }

		private void toolMoveUp_Click(object sender, EventArgs e)
		{
			if (lstMembers.SelectedItems.Count > 0) {
				ListViewItem item1 = lstMembers.SelectedItems[0];
				int index = item1.Index;

				if (index > 0) {
					ListViewItem item2 = lstMembers.Items[index - 1];

					if (item1.Tag is Field && item2.Tag is Field ||
						item1.Tag is Operation && item2.Tag is Operation)
					{
						locked = true;
						parent.MoveUpItem(item1.Tag);
						SwapListItems(item1, item2);
						item2.Focused = true;
						item2.Selected = true;
						locked = false;
						OnContentsChanged(EventArgs.Empty);
					}
				}
                updateRaptor(item1);
			}
		}

        private void updateRaptor(ListViewItem item1)
        {
            if ((parent is ClassType) && (item1.Tag is Method))
            {
                NClass.Core.ProjectCore.raptorUpdater.reorderMethods(
                    (parent as ClassType).raptorTab,
                    parent.Operations);
                NClass.Core.ProjectCore.raptorUpdater.resetAttributes(
                    (parent as ClassType).raptorTab,
                    parent.Fields);
            }
        }

		private void toolMoveDown_Click(object sender, EventArgs e)
		{
			if (lstMembers.SelectedItems.Count > 0) {
				ListViewItem item1 = lstMembers.SelectedItems[0];
				int index = item1.Index;

				if (index < lstMembers.Items.Count - 1) {
					ListViewItem item2 = lstMembers.Items[index + 1];

					if (item1.Tag is Field && item2.Tag is Field ||
						item1.Tag is Operation && item2.Tag is Operation)
					{
						locked = true;
						parent.MoveDownItem(item1.Tag);
						SwapListItems(item1, item2);
						item2.Focused = true;
						item2.Selected = true;
						locked = false;
						OnContentsChanged(EventArgs.Empty);
					}
				}
                updateRaptor(item1);
			}
		}

		private void toolDelete_Click(object sender, EventArgs e)
		{
			DeleteSelectedMember();
		}

		private void btnClose_Click(object sender, EventArgs e)
		{
            if (parent is ClassType)
            {
                NClass.Core.ProjectCore.raptorUpdater.resetAttributes(
                    (parent as ClassType).raptorTab,
                    parent.Fields);
            }
			this.Close();
		}
	}
}