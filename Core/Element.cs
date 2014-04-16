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
	public abstract class Element : ISerializable
	{
		bool modified = false;
		bool initializing = false;
		int dontRaiseRequestCount = 0;
        public Element() { }
        public Element(SerializationInfo info, StreamingContext ctxt)
        {
        }
        public virtual void GetObjectData(SerializationInfo info, StreamingContext ctxt)
        {

        }
		public event EventHandler Changed;

		protected bool Initializing
		{
			get { return initializing; }
			set { initializing = value; }
		}

		protected bool RaiseChangedEvent
		{
			get
			{
				return (dontRaiseRequestCount == 0);
			}
			set
			{
				if (!value)
					dontRaiseRequestCount++;
				else if (dontRaiseRequestCount > 0)
					dontRaiseRequestCount--;

				if (RaiseChangedEvent && modified)
					OnChanged(EventArgs.Empty);
			}
		}

		protected void Modified()
		{
			if (!Initializing) {
				if (RaiseChangedEvent)
					OnChanged(EventArgs.Empty);
				else
					modified = true;
			}
		}

		private void OnChanged(EventArgs e)
		{
			modified = false;
			if (Changed != null)
				Changed(this, e);
		}
	}
}
