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
using System.Xml;
using System.Runtime.Serialization;
using System.Runtime.Serialization.Formatters.Binary;

namespace NClass.Core
{
    [Serializable]
	public abstract class Relation : Element, ISerializableElement
	{
		string label = string.Empty;
		bool attached = false;

		public event EventHandler Attaching;
		public event EventHandler Detaching;
		public event SerializeEventHandler Serializing;
		public event SerializeEventHandler Deserializing;
        public object my_connection;

        public Relation() { }

		public abstract IEntity First
		{
			get;
		}

		public abstract IEntity Second
		{
			get;
		}

		public abstract string Name
		{
			get;
		}

		public string Label
		{
			get { return label; }
			set { label = value; }
		}

		public abstract bool HasLabel
		{
			get;
		}

		/// <exception cref="RelationException">
		/// Cannot finalize relationship.
		/// </exception>
		internal void Attach()
		{
			if (!attached)
				OnAttaching(EventArgs.Empty);
			attached = true;
		}

		internal void Detach()
		{
			if (attached)
				OnDetaching(EventArgs.Empty);
			attached = false;
		}

		void ISerializableElement.Serialize(XmlElement node)
		{
			Serialize(node);
		}

		void ISerializableElement.Deserialize(XmlElement node)
		{
			Deserialize(node);
		}
        public override void GetObjectData(SerializationInfo info, StreamingContext ctxt)
        {
            info.AddValue("_label", Label.ToString());
            //info.AddValue("_connection", my_connection);
            //OnSerializing(new SerializeEventArgsBinary(info, ctxt));
        }

        public Relation(SerializationInfo info, StreamingContext ctxt) 
        {
            if (HasLabel)
            {
                Label = info.GetString("_label");
            }
            //this.my_connection = info.GetValue("_connection", typeof(object));
            //OnDeserializing(new SerializeEventArgsBinary(info, ctxt));
        }
		/// <exception cref="ArgumentNullException">
		/// <paramref name="node"/> is null.
		/// </exception>
		protected internal virtual void Serialize(XmlElement node)
		{
			if (node == null)
				throw new ArgumentNullException("node");

			if (HasLabel && !string.IsNullOrEmpty(Label)) {
				XmlElement labelNode = node.OwnerDocument.CreateElement("Label");
				labelNode.InnerText = Label.ToString();
				node.AppendChild(labelNode);
			}
			//OnSerializing(new SerializeEventArgs(node));
		}

		/// <exception cref="ArgumentNullException">
		/// <paramref name="node"/> is null.
		/// </exception>
		protected internal virtual void Deserialize(XmlElement node)
		{
			if (node == null)
				throw new ArgumentNullException("node");

			if (HasLabel) {
				XmlElement labelNode = node["Label"];
				if (labelNode != null)
					Label = labelNode.InnerText;
			}
			//OnDeserializing(new SerializeEventArgs(node));
		}

		protected virtual void OnAttaching(EventArgs e)
		{
			if (Attaching != null)
				Attaching(this, e);
		}

		protected virtual void OnDetaching(EventArgs e)
		{
			if (Detaching != null)
				Detaching(this, e);
		}

		public void OnSerializing(SerializeEventArgsBinary e)
		{
			if (Serializing != null)
				Serializing(this, e);
		}

		public void OnDeserializing(SerializeEventArgsBinary e)
		{
			if (Deserializing != null)
				Deserializing(this, e);
		}

		public abstract override string ToString();
	}
}
