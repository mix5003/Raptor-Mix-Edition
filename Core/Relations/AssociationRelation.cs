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
using System.Text;
using System.Xml;
using NClass.Translations;
using System.Runtime.Serialization;
using System.Runtime.Serialization.Formatters.Binary;

namespace NClass.Core
{
    [Serializable]
	public sealed class AssociationRelation : TypeRelation
	{
		bool isAggregation = false;
		bool isComposition = false;
		Direction direction = Direction.Unidirectional;

		/// <exception cref="ArgumentNullException">
		/// <paramref name="first"/> is null.-or-
		/// <paramref name="second"/> is null.
		/// </exception>
		internal AssociationRelation(TypeBase first, TypeBase second) : base(first, second)
		{
			Attach();
		}

		public override string Name
		{
			get { return "Association"; }
		}

		public override bool HasLabel
		{
			get { return true; }
		}

		public Direction Direction
		{
			get
			{
				return direction;
			}
			set
			{
				if (direction != value) {
					direction = value;
					Modified();
				}
			}
		}

		public bool IsAggregation
		{
			get
			{
				return isAggregation;
			}
			set
			{
				if (isAggregation != value) {
					if (value)
						isComposition = false;
					isAggregation = value;
					Modified();
				}
			}
		}

		public bool IsComposition
		{
			get
			{
				return isComposition;
			}
			set
			{
				if (isComposition != value) {
					if (value)
						isAggregation = false;
					isComposition = value;
					Modified();
				}
			}
		}
        public AssociationRelation(SerializationInfo info, StreamingContext ctxt)
            : base(info, ctxt)
        {
            Direction = (NClass.Core.Direction) 
                info.GetValue("_Direction", typeof(NClass.Core.Direction));
            IsComposition = info.GetBoolean("_IsAggregation");
            IsComposition = info.GetBoolean("_IsComposition");
        }
        public override void GetObjectData(SerializationInfo info, StreamingContext ctxt)
        {
            info.AddValue("_Direction", Direction);
            info.AddValue("_IsAggregation", IsAggregation);
            info.AddValue("_IsComposition", IsComposition);
            base.GetObjectData(info, ctxt);
        }
		/// <exception cref="ArgumentNullException">
		/// <paramref name="node"/> is null.
		/// </exception>
		protected internal override void Serialize(XmlElement node)
		{
			base.Serialize(node);

			XmlElement directionNode = node.OwnerDocument.CreateElement("Direction");
			directionNode.InnerText = Direction.ToString();
			node.AppendChild(directionNode);
			
			XmlElement aggregationNode = node.OwnerDocument.CreateElement("IsAggregation");
			aggregationNode.InnerText = IsAggregation.ToString();
			node.AppendChild(aggregationNode);

			XmlElement compositionNode = node.OwnerDocument.CreateElement("IsComposition");
			compositionNode.InnerText = IsComposition.ToString();
			node.AppendChild(compositionNode);
		}

		/// <exception cref="ArgumentNullException">
		/// <paramref name="node"/> is null.
		/// </exception>
		protected internal override void Deserialize(XmlElement node)
		{
			base.Deserialize(node);

			XmlElement child = node["Direction"];

			RaiseChangedEvent = false;
			if (child != null) {                           /* Old file format */
				if (child.InnerText == "Unidirectional" || child.InnerText == "SourceDestination")
					Direction = Direction.Unidirectional;
				else
					Direction = Direction.Bidirectional;
			}

			try {
				child = node["IsAggregation"];
				if (child != null)
					IsAggregation = bool.Parse(child.InnerText);

				child = node["IsComposition"];
				if (child != null)
					IsComposition = bool.Parse(child.InnerText);
			}
			catch (ArgumentException) {
				// Wrong format
			}
			RaiseChangedEvent = true;
		}

		public override string ToString()
		{
			StringBuilder builder = new StringBuilder(50);

			if (IsAggregation)
				builder.Append(Strings.GetString("aggregation"));
			else if (IsComposition)
				builder.Append(Strings.GetString("composition"));
			else
				builder.Append(Strings.GetString("association"));
			builder.Append(": ");
			builder.Append(First.Name);

			switch (Direction) {
				case Direction.Bidirectional:
					if (IsAggregation)
						builder.Append(" <>-- ");
					else if (IsComposition)
						builder.Append(" #-- ");
					else
						builder.Append(" --- ");
					break;
				case Direction.Unidirectional:
					if (IsAggregation)
						builder.Append(" <>-> ");
					else if (IsComposition)
						builder.Append(" #-> ");
					else
						builder.Append(" --> ");
					break;
				default:
					builder.Append(", ");
					break;
			}
			builder.Append(Second.Name);

			return builder.ToString();
		}
	}
}
