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
using System.Text;
using System.Xml;
using NClass.Translations;
using System.Runtime.Serialization;
using System.Runtime.Serialization.Formatters.Binary;
namespace NClass.Core
{
    [Serializable]
	public abstract class EnumType : TypeBase
	{
		List<EnumValue> values = new List<EnumValue>();

		/// <exception cref="BadSyntaxException">
		/// The <paramref name="name"/> does not fit to the syntax.
		/// </exception>
		protected EnumType(string name) : base(name)
		{
		}
        public EnumType(SerializationInfo info, StreamingContext ctxt) : base(info,ctxt)
        {
            int count = info.GetInt32("_count");
            for (int i = 0; i < count; i++)
            {
                values.Add((EnumValue) info.GetValue("_value" + i, typeof(EnumValue)));
            }
        }
        public override void GetObjectData(SerializationInfo info, StreamingContext ctxt)
        {
            info.AddValue("_count", ValueCount);
            for (int i = 0; i < ValueCount; i++)
            {
                info.AddValue("_value"+i, values[i]);
            }
            base.GetObjectData(info, ctxt);
            //OnSerializing(new SerializeEventArgsBinary(info,ctxt));
        }
		public sealed override string EntityType
		{
			get { return "Enum"; }
		}

		public IEnumerable<EnumValue> Values
		{
			get { return values; }
		}

		public int ValueCount
		{
			get { return values.Count; }
		}

		public sealed override string Signature
		{
			get
			{
				return (Language.GetAccessString(Access, false) + " Enum");
			}
		}

		public override string Stereotype
		{
			get { return "«enumeration»"; }
		}

		/// <exception cref="BadSyntaxException">
		/// The name does not fit to the syntax.
		/// </exception>
		/// <exception cref="ReservedNameException">
		/// The name is a reserved name.
		/// </exception>
		public abstract EnumValue AddValue(string declaration);

		/// <exception cref="ReservedNameException">
		/// The name is a reserved name.
		/// </exception>
		protected void AddValue(EnumValue newValue)
		{
			if (newValue != null) {
				foreach (EnumValue value in Values) {
					if (value.Name == newValue.Name)
						throw new ReservedNameException(newValue.Name);
				}

				values.Add(newValue);
				newValue.Changed += delegate { Modified(); };
				Modified();
			}
		}

		/// <exception cref="BadSyntaxException">
		/// The name does not fit to the syntax.
		/// </exception>
		/// <exception cref="ReservedNameException">
		/// The name is a reserved name.
		/// </exception>
		public abstract EnumValue ModifyValue(EnumValue value, string declaration);

		/// <exception cref="ReservedNameException">
		/// The new name is a reserved name.
		/// </exception>
		protected bool ChangeValue(EnumValue oldValue, EnumValue newValue)
		{
			if (oldValue == null || newValue == null)
				return false;

			int index = -1;
			for (int i = 0; i < values.Count; i++) {
				if (values[i] == oldValue)
					index = i;
				else if (values[i].Name == newValue.Name)
					throw new ReservedNameException(newValue.Name);
			}

			if (index == -1) {
				return false;
			}
			else {
				values[index] = newValue;
				return true;
			}
		}

		public void RemoveValue(EnumValue value)
		{
			if (values.Remove(value))
				Modified();
		}

		public override void MoveUpItem(object item)
		{
			if (item is EnumValue && MoveUp(values, item))
				Modified();
		}

		public override void MoveDownItem(object item)
		{
			if (item is EnumValue && MoveDown(values, item))
				Modified();
		}

		/// <exception cref="ArgumentNullException">
		/// <paramref name="node"/> is null.
		/// </exception>
		protected internal override void Serialize(XmlElement node)
		{
			base.Serialize(node);

			foreach (EnumValue value in values) {
				XmlElement child = node.OwnerDocument.CreateElement("Value");
				child.InnerText = value.ToString();
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

			XmlNodeList nodeList = node.SelectNodes("Value");
			foreach (XmlNode valueNode in nodeList)
				AddValue(valueNode.InnerText);

			base.Deserialize(node);
			RaiseChangedEvent = true;
		}
	}
}
