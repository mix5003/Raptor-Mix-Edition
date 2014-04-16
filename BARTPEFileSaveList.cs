using System;
using System.Collections.Generic;
using System.ComponentModel;
using System.Data;
using System.Drawing;
using System.Text;
using System.Windows.Forms;

namespace raptor
{
    public partial class BARTPEFileSaveList : Form
    {
        public static string filename;
        public BARTPEFileSaveList()
        {
            InitializeComponent();
            this.textBox1.Focus();
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
            CheckOverwrite(this.filesListBox1.SelectedFile);
        }

        private void buttonCancel_Click(object sender, EventArgs e)
        {
            this.Close();
        }

        private void buttonOK_Click(object sender, EventArgs e)
        {
            string test = this.textBox1.Text;
            if (test.StartsWith("x:\\", true, null) ||
                test.StartsWith("y:\\", true, null) ||
                test.StartsWith("b:\\", true, null))
            {
                test = test.Substring(3);
            }
            if (test.EndsWith(".rap", true, null))
            {
                test = test.Substring(0, test.Length - 4);
            }
            if (test.Length < 1)
            {
                MessageBox.Show("Filename must not be blank!",
                    "Invalid filename", MessageBoxButtons.OK, MessageBoxIcon.Error);
                return;
            }
            for (int i = 0; i < test.Length; i++)
            {
                if (!Char.IsLetterOrDigit(test[i]) && test[i]!='_' && test[i]!='-')
                {
                    MessageBox.Show("Filename can only contain letters, numbers, dashes and underscores.",
                        "Invalid filename", MessageBoxButtons.OK, MessageBoxIcon.Error);
                    return;
                }
            }
            test = this.filesListBox1.SelectedPath + test + ".rap";
            if (System.IO.File.Exists(test))
            {
                CheckOverwrite(test);
            }
            else
            {
                filename = test;
                this.Close();
            }
        }

        private void CheckOverwrite(string test)
        {
            DialogResult result = MessageBox.Show(test + " already exists.\n" +
               "Do you want to overwrite it?", "Overwrite file?", MessageBoxButtons.YesNo);
            if (result == DialogResult.Yes)
            {
                filename = test;
                this.Close();
            }
        }

        private void textBox1_KeyDown(object sender, KeyEventArgs e)
        {
            if (e.KeyCode == Keys.Enter || e.KeyCode == Keys.Return)
            {
                this.buttonOK_Click(sender, e);
            }
        }

        private void filesListBox1_SelectedValueChanged(object sender, EventArgs e)
        {
            this.textBox1.Text = System.IO.Path.GetFileName(
                this.filesListBox1.SelectedFile);
        }
    }
}
