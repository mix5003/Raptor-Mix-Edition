using System;
using System.Collections.Generic;
using System.Text;
using System.Windows.Forms;

namespace raptor
{
    class ClipboardMultiplatform
    {
        private class mydata : IDataObject
        {
            private object data;
            public mydata(object data)
            {
                this.data = data;
            }
            public object GetData(String format) {
                if (data!=null && data.GetType().ToString() == format)
                {
                    return data;
                }
                else
                {
                    return null;
                }
            }
            public object GetData(String format, bool autoConvert)
            {
                return GetData(format);
            }
            public object GetData(Type type)
            {
                if (data!=null && data.GetType() == type)
                {
                    return data;
                }
                else
                {
                    return null;
                }
            }
            public void SetData(String format, bool autoConvert, object data)
            {
                this.data = data;
            }
            public void SetData(String format, object data)
            {
                this.data = data;
            }
            public void SetData(Type type, object data)
            {
                this.data = data;
            }
            public void SetData(object data)
            {
                this.data = data;
            }
            public bool GetDataPresent(String format, bool autoConvert)
            {
                return (data != null && this.data.GetType().ToString() == format);
            }
            public bool GetDataPresent(String format)
            {
                return GetDataPresent(format, false);
            }
            public bool GetDataPresent(Type type)
            {
                return (data != null && this.data.GetType() == type);
            }
            public String[] GetFormats(bool autoconvert)
            {
                throw new System.Exception("not supported");
            }
            public String[] GetFormats()
            {
                throw new System.Exception("not supported");
            }
        }
        private static mydata clipboard_data;
        public static void SetDataObject(object data, bool afterExit)
        {
            if (Component.MONO)
            {
                clipboard_data = new mydata(data);
            }
            else
            {
                Clipboard.SetDataObject(data, afterExit);
            }
        }
        public static IDataObject GetDataObject()
        {
            if (Component.MONO)
            {
                return clipboard_data;
            }
            else
            {
                return Clipboard.GetDataObject();
            }
        }
    }
}
