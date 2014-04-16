using System;
using System.Collections.Generic;
using System.Text;

namespace GeneratorAda
{
    public interface Imperative_Interface : generate_interface.typ
    {
        void Done_Method();
        void Declare_Procedure(
            string name,
            string[] args,
            bool[] arg_is_input,
            bool[] arg_is_output);
    }
}
