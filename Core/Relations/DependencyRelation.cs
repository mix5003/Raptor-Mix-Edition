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
using NClass.Translations;
using System.Runtime.Serialization;
using System.Runtime.Serialization.Formatters.Binary;

namespace NClass.Core
{
    [Serializable]
	public sealed class DependencyRelation : TypeRelation
	{
		/// <exception cref="ArgumentNullException">
		/// <paramref name="first"/> is null.-or-
		/// <paramref name="second"/> is null.
		/// </exception>
		internal DependencyRelation(TypeBase first, TypeBase second) : base(first, second)
		{
			Attach();
		}
        public DependencyRelation(SerializationInfo info, StreamingContext ctxt)
            : base(info, ctxt)
        {
            Attach();
        }
        public override void GetObjectData(SerializationInfo info, StreamingContext ctxt)
        {
            base.GetObjectData(info, ctxt);
        }
		public override string Name
		{
			get { return "Dependency"; }
		}

		public override bool HasLabel
		{
			get { return true; }
		}

		public override string ToString()
		{
			return string.Format("{0}: {1} --> {2}",
				Strings.GetString("dependency"), First.Name, Second.Name);
		}
	}
}
