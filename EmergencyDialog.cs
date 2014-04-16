using System;
using System.Collections.Generic;
using System.ComponentModel;
using System.Data;
using System.Drawing;
using System.Text;
using System.Windows.Forms;
using System.Diagnostics;
using Microsoft.Win32;

namespace raptor
{
    public partial class EmergencyDialog : Form
    {
        private static int challenge;
        private BigInteger bi_m, bi_n, bi_e, bi_r, bi_check;
        Random random = new Random();
        public EmergencyDialog()
        {
            InitializeComponent();
            challenge =  random.Next(99999);
            this.label3.Text = "" + challenge;
            this.textBox1.Focus();
        }

        private void button1_Click(object sender, EventArgs e)
        {
            try
            {
                this.bi_e = new BigInteger(65537);
                this.bi_m = new BigInteger(challenge);
                this.bi_n = new BigInteger("5239739256519985939", 10);
                this.bi_r = new BigInteger(this.textBox1.Text, 10);
                bi_check = this.bi_r.modPow(this.bi_e, this.bi_n);
                if (bi_check.Equals(bi_m))
                {
                    Process proc = new Process();

                    try
                    {
                        RegistryKey HKLM = Registry.LocalMachine;
                        RegistryKey System_Key = HKLM.OpenSubKey("System");
                        RegistryKey CurrentControlSet = System_Key.OpenSubKey("CurrentControlSet");
                        RegistryKey Control_Key = CurrentControlSet.OpenSubKey("Control");
                        RegistryKey SDP = Control_Key.OpenSubKey("StorageDevicePolicies",true);
                        SDP.SetValue("WriteProtect", 0, RegistryValueKind.DWord);
                    }
                    catch (System.Exception ex)
                    {
                    }

                    if (System.IO.File.Exists("c:\\windows\\system32\\cmd.exe"))
                    {
                        proc.StartInfo.FileName = "c:\\windows\\system32\\cmd.exe";
                    }
                    else if (System.IO.File.Exists("x:\\minint\\system32\\cmd.exe"))
                    {
                        proc.StartInfo.FileName = "x:\\minint\\system32\\cmd.exe";
                    }
                    else if (System.IO.File.Exists("x:\\windows\\system32\\cmd.exe"))
                    {
                        proc.StartInfo.FileName = "x:\\windows\\system32\\cmd.exe";
                    }
                    else 
                    {
                        proc.StartInfo.FileName = "x:\\i386\\system32\\cmd.exe";
                    }
                    proc.StartInfo.ErrorDialog = false;
                    proc.StartInfo.WorkingDirectory = Environment.CurrentDirectory;
                    proc.Start();
                    this.Close();
                }
                else
                {
                    challenge = random.Next(9999999);
                    this.label3.Text = "" + challenge;
                }

            }
            catch
            {
                MessageBox.Show("exception");
                this.Close();
            }
        }
    }
}
