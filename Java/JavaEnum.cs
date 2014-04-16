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
using NClass.Core;
using System.Runtime.Serialization;
using System.Runtime.Serialization.Formatters.Binary;
namespace NClass.Java
{
    [Serializable]
	internal sealed class JavaEnum : EnumType
	{
		internal JavaEnum() : this("NewEnum")
		{
		}

		/// <exception cref="BadSyntaxException">
		/// The <paramref name="name"/> does not fit to the syntax.
		/// </exception>
		internal JavaEnum(string name) : base(name)
		{
		}
        public JavaEnum(SerializationInfo info, StreamingContext ctxt) : base(info,ctxt)
        {
        }
		public override AccessModifier AccessModifier
		{
			get
			{
				return base.AccessModifier;
			}
			set
			{
				if (IsNested ||
					value == AccessModifier.Default ||
					value == AccessModifier.Public)
				{
					base.AccessModifier = value;
				}
			}
		}

		public override AccessModifier DefaultAccess
		{
			get { return AccessModifier.Internal; }
		}

		/// <exception cref="ArgumentException">
		/// The <paramref name="value"/> is already a child member of the type.
		/// </exception>
		public override CompositeType NestingParent
		{
			get
			{
				return base.NestingParent;
			}
			protected set
			{
				try {
					RaiseChangedEvent = false;

					base.NestingParent = value;
					if (NestingParent == null && Access != AccessModifier.Public)
						AccessModifier = AccessModifier.Default;
				}
				finally {
					RaiseChangedEvent = true;
				}
			}
		}

		public override Language Language
		{
			get { return JavaLanguage.Instance; }
		}

		/// <exception cref="BadSyntaxException">
		/// The name does not fit to the syntax.
		/// </exception>
		/// <exception cref="ReservedNameException">
		/// The name is a reserved name.
		/// </exception>
		public override EnumValue AddValue(string declaration)
		{
			EnumValue value = new JavaEnumValue(declaration);

			AddValue(value);
			return value;
		}

		/// <exception cref="BadSyntaxException">
		/// The name does not fit to the syntax.
		/// </exception>
		/// <exception cref="ReservedNameException">
		/// The name is a reserved name.
		/// </exception>
		public override EnumValue ModifyValue(EnumValue value, string declaration)
		{
			EnumValue newValue = new JavaEnumValue(declaration);

			if (ChangeValue(value, newValue))
				return newValue;
			else
				return value;
		}
		
		public override string GetDeclarationLine()
		{
			StringBuilder builder = new StringBuilder();

			if (AccessModifier != AccessModifier.Default) {
				builder.Append(Language.GetAccessString(AccessModifier, true));
				builder.Append(" ");
			}
			builder.AppendFormat("enum {0}", Name);

			return builder.ToString();
		}
	}
}
