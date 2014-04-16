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
using System.Runtime.Serialization;
using NClass.Translations;

namespace NClass.Core
{
	public class ReservedNameException : BadSyntaxException
	{
		string name;

		public ReservedNameException() : base(Strings.GetString("error_reserved_name"))
		{
			name = null;
		}

		public ReservedNameException(string name)
			: base(Strings.GetString("error_reserved_name"))
		{
			this.name = name;
		}

		public ReservedNameException(string name, Exception innerException)
			: base(Strings.GetString("error_reserved_name"), innerException)
		{
			this.name = name;
		}

		public string ReservedName
		{
			get { return name; }
		}
	}
}
