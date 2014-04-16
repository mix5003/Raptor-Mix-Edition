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
using System.IO;
using System.Xml;
using System.Text.RegularExpressions;
using System.Collections.Generic;
using NClass.Translations;
using System.Runtime.Serialization;
using System.Runtime.Serialization.Formatters.Binary;

namespace NClass.Core
{
    [Serializable]
	public abstract class CompositeType : TypeBase
	{
		List<Field> fieldList          = new List<Field>();
        List<Operation> operationList = new List<Operation>();
        List<TypeBase> nestedChilds = new List<TypeBase>();

		/// <exception cref="BadSyntaxException">
		/// The <paramref name="name"/> does not fit to the syntax.
		/// </exception>
		protected CompositeType(string name) : base(name)
		{
		}

		public abstract bool SupportsFields { get; }

		public bool SupportsOperations
		{
			get
			{
				return (
					SupportsMethods || SupportsConstuctors || SupportsDestructors ||
					SupportsProperties || SupportsEvents
				);
			}
		}

		public abstract bool SupportsMethods { get; }

		public abstract bool SupportsConstuctors { get; }

		public abstract bool SupportsDestructors { get; }

		public abstract bool SupportsProperties { get; }

		public abstract bool SupportsEvents { get; }

		public abstract bool SupportsNesting { get; }

		public abstract bool IsAllowedParent { get; }

		public abstract bool IsAllowedChild { get; }

		public abstract bool HasExplicitBase { get; }

		public abstract AccessModifier DefaultMemberAccess { get; }

		protected List<Field> FieldList
		{
			get { return fieldList; }
		}

		public IEnumerable<Field> Fields
		{
			get { return fieldList; }
		}

		public int FieldCount
		{
			get { return fieldList.Count; }
		}

		protected List<Operation> OperationList
		{
			get { return operationList; }
		}

		public IEnumerable<Operation> Operations
		{
			get { return operationList; }
		}

		public int OperationCount
		{
			get { return operationList.Count; }
		}

		public IEnumerable<TypeBase> NestedChilds
		{
			get { return nestedChilds; }
		}

		/// <exception cref="InvalidOperationException">
		/// The type does not support fields.
		/// </exception>
		public abstract Field AddField();

		/// <exception cref="InvalidOperationException">
		/// The type does not support methods.
		/// </exception>
		public abstract Method AddMethod();

		/// <exception cref="InvalidOperationException">
		/// The type does not support constructors.
		/// </exception>
		public abstract Constructor AddConstructor();

		/// <exception cref="InvalidOperationException">
		/// The type does not support destructors.
		/// </exception>
		public abstract Destructor AddDestructor();

		/// <exception cref="InvalidOperationException">
		/// The type does not support properties.
		/// </exception>
		public abstract Property AddProperty();

		/// <exception cref="InvalidOperationException">
		/// The type does not support events.
		/// </exception>
		public abstract Event AddEvent();

		protected void AddField(Field field)
		{
			if (field != null && !FieldList.Contains(field)) {
				FieldList.Add(field);
				field.Changed += delegate { Modified(); };
				Modified();
			}
		}

		protected void AddOperation(Operation operation)
		{
			if (operation != null && !OperationList.Contains(operation)) {
				OperationList.Add(operation);
				operation.Changed += delegate { Modified(); };
				Modified();
			}
		}

		public void RemoveMember(Member member)
		{
			if (member is Field) {
				if (FieldList.Remove((Field) member))
					Modified();
			}
			else if (member is Operation) {
				if (OperationList.Remove((Operation) member))
					Modified();
			}
		}

		internal void AddNestedChild(TypeBase type)
		{
			if (type != null && !nestedChilds.Contains(type)) {
				nestedChilds.Add(type);
				Modified();
			}
		}

		internal void RemoveNestedChild(TypeBase type)
		{
			if (type != null && nestedChilds.Remove(type))
				Modified();
		}

		public Operation GetDefinedOperation(Operation operation)
		{
			if (operation == null)
				return null;

			for (int i = 0; i < OperationList.Count; i++) {
				if (OperationList[i].HasSameSignatureAs(operation))
					return OperationList[i];
			}

			return null;
		}

		public sealed override void MoveUpItem(object item)
		{
			if (item is Field) {
				if (MoveUp(FieldList, item))
					Modified();
			}
			else if (item is Operation) {
				if (MoveUp(OperationList, item))
					Modified();
			}
		}

		public sealed override void MoveDownItem(object item)
		{
			if (item is Field) {
				if (MoveDown(FieldList, item))
					Modified();
			}
			else if (item is Operation) {
				if (MoveDown(OperationList, item))
					Modified();
			}
		}

		public void SortMembers(SortingMode sortingMode)
		{
			switch (sortingMode) {
				case SortingMode.ByName:
					FieldList.Sort(MemberComparisonByName);
					OperationList.Sort(MemberComparisonByName);
					Modified();
					break;

				case SortingMode.ByAccess:
					FieldList.Sort(MemberComparisonByAccess);
					OperationList.Sort(MemberComparisonByAccess);
					Modified();
					break;

				case SortingMode.ByKind:
					FieldList.Sort(MemberComparisonByKind);
					OperationList.Sort(MemberComparisonByKind);
					Modified();
					break;
			}
		}

		private static int MemberComparisonByName(Member member1, Member member2)
		{
			return member1.Name.CompareTo(member2.Name);
		}

		private static int MemberComparisonByAccess(Member member1, Member member2)
		{
			int access1 = (int) member1.Access;
			int access2 = (int) member2.Access;

			if (access1 == access2)
				return MemberComparisonByKind(member1, member2);
			else
				return access1 - access2;
		}

		private static int MemberComparisonByKind(Member member1, Member member2)
		{
			int ret = GetMemberOrdinal(member1) - GetMemberOrdinal(member2);

			if (ret == 0)
				return MemberComparisonByName(member1, member2);

			return ret;
		}

		private static int GetMemberOrdinal(Member member)
		{
			if (member is Field) {
				if (((Field) member).IsConstant)
					return 0;
				else
					return 1;
			}
			if (member is Property) {
				Property property = (Property) member;

				if (property.IsReadonly)
					return 2;
				else if (property.IsWriteonly)
					return 3;
				else
					return 4;
			}
			if (member is Constructor)
				return 5;
			if (member is Method && ((Method) member).IsOperator)
				return 6;
			if (member is Destructor)
				return 8; // (!)
			if (member is Method)
				return 7;
			if (member is Event)
				return 9;

			// Unreachable case
			return 10;
		}

        //Serialization function.
        public override void GetObjectData(SerializationInfo info, StreamingContext ctxt)
        {
            info.AddValue("_num_fields", FieldList.Count);
            info.AddValue("_num_operations", OperationList.Count);
            int i = 0;
            foreach (Field field in FieldList)
            {
                info.AddValue("_field_type" + i, field.MemberType);
                info.AddValue("_field"+i, field.ToString());
                i++;
            }
            i = 0;
            foreach (Operation operation in OperationList)
            {
                info.AddValue("_operation_type" + i, operation.MemberType);
                info.AddValue("_operation" + i, operation.ToString());
                i++;
            }
            base.GetObjectData(info, ctxt);
        }

        public CompositeType(SerializationInfo info, StreamingContext ctxt)
            : base(info, ctxt)
        {
            int i;
            int field_count = info.GetInt32("_num_fields");
            int operation_count = info.GetInt32("_num_operations");
            RaiseChangedEvent = false;
            for (i=0; i<field_count; i++)
            {
                Field field = AddField();
                field.InitFromString(
                    info.GetString("_field"+i));
            }
            for (i = 0; i < operation_count; i++)
            {
                Operation operation = GetOperation(
                    info.GetString("_operation_type"+i));

                if (operation == null)
                {
                    throw new InvalidDataException(
                        Strings.GetString("error_corrupt_save_format"));
                }
                operation.InitFromString(
                    info.GetString("_operation"+i));

            }

            RaiseChangedEvent = true;
        }
		/// <exception cref="ArgumentNullException">
		/// <paramref name="node"/> is null.
		/// </exception>
		protected internal override void Serialize(XmlElement node)
		{
			base.Serialize(node);

			foreach (Field field in FieldList) {
				XmlElement child = node.OwnerDocument.CreateElement("Member");
				child.SetAttribute("type", field.MemberType);
				child.InnerText = field.ToString();
				node.AppendChild(child);
			}
			foreach (Operation operation in OperationList) {
				XmlElement child = node.OwnerDocument.CreateElement("Member");
				child.SetAttribute("type", operation.MemberType);
				child.InnerText = operation.ToString();
				node.AppendChild(child);
			}
		}

		/// <exception cref="BadSyntaxException">
		/// An error occured while deserializing.
		/// </exception>
		/// <exception cref="InvalidOperationException">
		/// The XML document is corrupt.
		/// </exception>
		/// <exception cref="ArgumentNullException">
		/// <paramref name="node"/> is null.
		/// </exception>
		protected internal override void Deserialize(XmlElement node)
		{
			RaiseChangedEvent = false;
			                                                         // Old file format
			foreach (XmlElement childNode in node.SelectNodes("Member|Field|Operation")) {
				string type = childNode.GetAttribute("type");

				if (type == "Field" || type == "CSharpField" || type == "JavaField") {
					Field field = AddField();
					field.InitFromString(childNode.InnerText);
				}
				else {
					Operation operation = GetOperation(type);

					if (operation == null) {
						throw new InvalidDataException(
							Strings.GetString("error_corrupt_save_format"));
					}
					operation.InitFromString(childNode.InnerText);
				}
			}

			base.Deserialize(node);
			RaiseChangedEvent = true;
		}

		private Operation GetOperation(string type)
		{
			switch (type) {
				case "Constructor":
				case "CSharpConstructor":  // Old file format
				case "JavaConstructor":    // Old file format
					return AddConstructor();

				case "Destructor":
					return AddDestructor();

				case "Method":
				case "CSharpMethod":       // Old file format
				case "JavaMethod":         // Old file format
					return AddMethod();

				case "Property":
					return AddProperty();

				case "Event":
					return AddEvent();

				default:
					return null;
			}
		}
	}
}
