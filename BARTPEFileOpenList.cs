using System;
using System.Collections.Generic;
using System.ComponentModel;
using System.Data;
using System.Drawing;
using System.Text;
using System.Windows.Forms;

namespace raptor
{
    public partial class BARTPEFileOpenList : Form
    {
        public static string filename;
        public BARTPEFileOpenList()
        {
            InitializeComponent();
            filename = null;
            if (!Component.BARTPE)
            {
                this.filesListBox1.SelectedPath = "x:\\";
            }
            else
            {
                this.filesListBox1.SelectedPath = Component.BARTPE_ramdrive_path;
            }
        }

        private void filesListBox1_FileSelected(object sender, FilesBrowser.FileSelectEventArgs fse)
        {
            filename = this.filesListBox1.SelectedFile;
            this.Close();
        }

        private void checkBox1_CheckedChanged(object sender, EventArgs e)
        {
            if (this.checkBox1.Checked)
            {
                this.filesListBox1.Extension = "";
            }
            else
            {
                this.filesListBox1.Extension = ".rap";
            }
        }
        public void View_HD()
        {
            this.filesListBox1.Extension = ".aes";
            this.label1.Text = "";
            this.filesListBox1.SelectedPath = Component.BARTPE_partition_path;
            this.checkBox1.Visible = false;
        }
    }
}
