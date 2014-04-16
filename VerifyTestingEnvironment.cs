using System;
using System.Collections.Generic;
using System.Text;
using System.Windows.Forms;

namespace raptor
{
    class VerifyTestingEnvironment
    {
        static string drive = Component.BARTPE_partition_path;
        static string[] hashes = {
            "007771AFE87B37CACD105C0FBDFB9135", // autorun.inf
            "0DE0DAFA887669ECDC6AC13F98599818", // autorun.cmd
            "B31D299FF98B4C7D20BE6AC0ADFD2344", // autorun0raptor_regadd
            "22C24408D784EC590CC07AA33BBCDC5D", // gpg.exe
            "B93A94DFA32197F21D95C6A3FAD9DC17", // setres.exe
            "171F0F8D05C090600B3633B9DCDC4732", // nu2menu.exe
            "74F6B503A0F0DB3C25D0F27595C645AF", // nu2menu.xml
            "2AC26248E0AEB120AF4BF12B1E17F329", // perl
            "0D23D78B35BA6D1FAD60AF9FB7FB39B0", // disablecd.reg
            "FD432628CE8B41ADAFE52E81C4B43A44", // vc2005rt.reg
//            "B668A629DC6D1D134F56FD20570FC616", // setupreg.hiv
            "D45AC76AFF1438925578BBAEFF0A07A9"  // unzip.exe
        };
        static string[] files_to_verify = {
            "autorun.inf",
            "minint\\system32\\autorun.cmd",
            "minint\\system32\\autorun0raptor_regadd.cmd",
            "programs\\raptor\\gpg.exe",
            "Programs\\Nu2Menu\\setres.exe",
            "Programs\\Nu2Menu\\nu2menu.exe",
            "Programs\\Nu2Menu\\nu2menu.xml",
            "Programs\\RAPTOR\\Perl\\bin\\perl.exe", 
            "minint\\system32\\disablecd.reg",
            "minint\\system32\\vc2005rt.reg",
//            "minint\\system32\\setupreg.hiv",
            "programs\\raptor\\unzip.exe"
        };

        
        public static bool VerifyEnvironment()
        {
            int i;
            return true;
            // most of this has now been moved outside to a separate program
            for (i = 0; i < files_to_verify.Length; i++)
            {
                if ((MD5Helper.ComputeHash(drive + files_to_verify[i]) != hashes[i]) &&
                    (MD5Helper.ComputeHash(drive + files_to_verify[i]) != "27607C4E3FA222860B9C4CB416ACE1B8"))
                {
                    MessageBox.Show(drive + files_to_verify[i] +
                        " has been corrupted");
                }
            }
            
            if (System.IO.Directory.Exists(drive + "minint"))
            {
                for (i = 0; i < files_to_verify.Length; i++)
                {
                    if ((MD5Helper.ComputeHash(drive + files_to_verify[i]) != hashes[i]) &&
                        (MD5Helper.ComputeHash(drive + files_to_verify[i]) != "27607C4E3FA222860B9C4CB416ACE1B8"))
                    {
                        return false;
                    }
                }
            }
            return true;
        }
    }
}
