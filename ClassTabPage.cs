using System;
using System.Collections.Generic;
using System.ComponentModel;
using System.Drawing;
using System.Data;
using System.Text;
using System.Windows.Forms;

namespace raptor
{
    public partial class ClassTabPage : System.Windows.Forms.TabPage
    {
        // currently unused
        internal NClass.Core.ClassType ct;
        public ClassTabPage(Visual_Flow_Form form, string name)
        {
            InitializeComponent();
            this.Text = name;
        }
    }
}
