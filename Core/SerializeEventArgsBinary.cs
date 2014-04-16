using System;
using System.Collections.Generic;
using System.Text;
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
using System.IO;

namespace NClass.Core
{
    public delegate void SerializeEventHandler(object sender, SerializeEventArgsBinary e);
    public class SerializeEventArgsBinary : EventArgs
    {
        Stream stream;
        BinaryFormatter bformatter;
        SerializationInfo info;
        StreamingContext ctxt;

        public SerializeEventArgsBinary(BinaryFormatter bformatter, Stream stream)
        {
            this.bformatter = bformatter;
            this.stream = stream;
        }
        public SerializeEventArgsBinary(SerializationInfo info, StreamingContext ctxt)
        {
            this.info = info;
            this.ctxt = ctxt;
        }
        public BinaryFormatter Bformatter
        {
            get { return bformatter; }
        }
        public Stream Stream
        {
            get { return stream; }
        }
        public SerializationInfo Info
        {
            get { return info; }
        }
        public StreamingContext Ctxt
        {
            get { return ctxt; }
        }
    }
}
