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
using System.Drawing;
using System.Windows.Forms;
using NClass.Core;
using System.Drawing.Imaging;

namespace NClass.GUI.Diagram
{
	public static class Icons
	{
		public const int InterfaceImageIndex = 46;
		public const int EnumItemImageIndex  = 47;
		public const int ParameterImageIndex = 48;
		public const int ClassImageIndex     = 49;

		static Bitmap[] images;
		static ImageList imageList;

		static Icons()
		{
			LoadImages();
		}

		public static ImageList IconList
		{
			get
			{
				return imageList;
			}
		}

		private static void LoadImages()
		{
			images = new Bitmap[] {
				Properties.Resources.PublicConst,
				Properties.Resources.ProtintConst,
				Properties.Resources.InternalConst,
				Properties.Resources.ProtectedConst,
				Properties.Resources.PrivateConst,

				Properties.Resources.PublicField,
				Properties.Resources.ProtintField,
				Properties.Resources.InternalField,
				Properties.Resources.ProtectedField,
				Properties.Resources.PrivateField,

				Properties.Resources.PublicConstructor,
				Properties.Resources.ProtintConstructor,
				Properties.Resources.InternalConstructor,
				Properties.Resources.ProtectedConstructor,
				Properties.Resources.PrivateConstructor,

				Properties.Resources.PublicOperator,
				Properties.Resources.ProtintOperator,
				Properties.Resources.InternalOperator,
				Properties.Resources.ProtectedOperator,
				Properties.Resources.PrivateOperator,

				Properties.Resources.PublicMethod,
				Properties.Resources.ProtintMethod,
				Properties.Resources.InternalMethod,
				Properties.Resources.ProtectedMethod,
				Properties.Resources.PrivateMethod,

				Properties.Resources.PublicReadonly,
				Properties.Resources.ProtintReadonly,
				Properties.Resources.InternalReadonly,
				Properties.Resources.ProtectedReadonly,
				Properties.Resources.PrivateReadonly,

				Properties.Resources.PublicWriteonly,
				Properties.Resources.ProtintWriteonly,
				Properties.Resources.InternalWriteoly,
				Properties.Resources.ProtectedWriteonly,
				Properties.Resources.PrivateWriteonly,

				Properties.Resources.PublicProperty,
				Properties.Resources.ProtintProperty,
				Properties.Resources.InternalProperty,
				Properties.Resources.ProtectedProperty,
				Properties.Resources.PrivateProperty,

				Properties.Resources.PublicEvent,
				Properties.Resources.ProtintEvent,
				Properties.Resources.InternalEvent,
				Properties.Resources.ProtectedEvent,
				Properties.Resources.PrivateEvent,
			
				Properties.Resources.PrivateDestructor,
				Properties.Resources.Interface24,       // 46.
				Properties.Resources.EnumItem,          // 47.
				Properties.Resources.Parameter,         // 48.
				Properties.Resources.Class              // 49.
			};

			imageList = new ImageList();
			imageList.ColorDepth = ColorDepth.Depth32Bit;
			imageList.Images.AddRange(images);
		}

		/// <exception cref="ArgumentNullException">
		/// A <paramref name="member"/> nem lehet null.
		/// </exception>
		public static int GetImageIndex(Member member)
		{
			if (member == null)
				throw new ArgumentNullException("member");

			int group = 0;

			if (member is Field) {
				if (((Field) member).IsConstant)
					group = 0;
				else
					group = 1;
			}
			else if (member is Method) {
				if (member is Destructor)
					return 45;
				else if (member is Constructor)
					group = 2;
				else if (((Method) member).IsOperator)
					group = 3;
				else
					group = 4;
			}
			else if (member is Property) {
				Property property = (Property) member;

				if (property.IsReadonly)
					group = 5;
				else if (property.IsWriteonly)
					group = 6;
				else
					group = 7;
			}
			else if (member is Event) {
				group = 8;
			}

			return group * 5 + (int) member.Access - 1;
		}

		/// <exception cref="ArgumentNullException">
		/// <paramref name="member"/> is null.
		/// </exception>
		public static Image GetImage(Member member)
		{
			int imageIndex = GetImageIndex(member);
			return images[imageIndex];
		}
	}
}
