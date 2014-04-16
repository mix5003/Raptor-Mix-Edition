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
	internal sealed class JavaEnumValue : EnumValue
	{
		/// <exception cref="BadSyntaxException">
		/// The <paramref name="declaration"/> does not fit to the syntax.
		/// </exception>
		internal JavaEnumValue(string declaration) : base(declaration)
		{
		}
        public JavaEnumValue(SerializationInfo info, StreamingContext ctxt) : base(info,ctxt)
        {
            string declaration = info.GetString("name");
            Name = JavaLanguage.Instance.GetValidName(declaration, false);
        }
        public override void GetObjectData(SerializationInfo info, StreamingContext ctxt)
        {
            info.AddValue("name", Name);
            base.GetObjectData(info, ctxt);
            //OnSerializing(new SerializeEventArgsBinary(info,ctxt));
        }
		/// <exception cref="BadSyntaxException">
		/// The <paramref name="declaration"/> does not fit to the syntax.
		/// </exception>
		public override void InitFromString(string declaration)
		{
			Name = JavaLanguage.Instance.GetValidName(declaration, false);
		}

		public override string GetDeclarationLine()
		{
			return Name.ToString();
		}
	}
}
