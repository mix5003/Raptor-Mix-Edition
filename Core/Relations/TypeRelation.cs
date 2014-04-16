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
	public abstract class TypeRelation : Relation
	{
		TypeBase first;
		TypeBase second;

		/// <exception cref="ArgumentNullException">
		/// <paramref name="first"/> is null.-or-
		/// <paramref name="second"/> is null.
		/// </exception>
		protected TypeRelation(TypeBase first, TypeBase second)
		{
			if (first == null)
				throw new ArgumentNullException("first");
			if (second == null)
				throw new ArgumentNullException("second");

			this.first = first;
			this.second = second;
		}
        public TypeRelation(SerializationInfo info, StreamingContext ctxt)
            : base(info, ctxt)
        {
            this.first = (TypeBase) BinarySerializationHelper.first_entity;
            this.second = (TypeBase) BinarySerializationHelper.second_entity;
        }
        public override void GetObjectData(SerializationInfo info, StreamingContext ctxt)
        {
            base.GetObjectData(info, ctxt);
        }
		public sealed override IEntity First
		{
			get { return first; }
		}

		public sealed override IEntity Second
		{
			get { return second; }
		}
	}
}
