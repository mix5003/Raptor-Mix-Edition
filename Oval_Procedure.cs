using System;
using System.Collections.Generic;
using System.Text;
using System.Runtime.Serialization;
using System.Drawing;

namespace raptor
{
    [Serializable]
    class Oval_Procedure : Oval
    {
        protected int num_params;
        protected string[] param_names;
        protected bool[] param_is_input;
        protected bool[] param_is_output;
        public void changeParameters(int num_params, string[] param_names, bool[] param_is_input, bool[] param_is_output)
        {
            this.num_params = num_params;
            this.param_names = param_names;
            this.param_is_input = param_is_input;
            this.param_is_output = param_is_output;
            this.SetText();
        }
        public string RunDialog(string name, Visual_Flow_Form form)
        {
            string result;
            result = Procedure_name.RunDialog(name, ref param_names, ref param_is_input, ref param_is_output,
                form);
            if (result != null && result != "")
            {
                num_params = param_is_output.Length;
                this.SetText();
            }
            return result;
        }
        public string[] getArgs()
        {
            return param_names;
        }
        public bool[] getArgIsInput()
        {
            return param_is_input;
        }
        public bool[] getArgIsOutput()
        {
            return param_is_output;
        }
        public int Parameter_Count
        {
            get
            {
                return num_params;
            }
        }
        public string Parameter_String
        {
            get
            {
                // skip past "Start "
                return this.Text.Substring(6);
            }
        }
        public string Param_Name(int i)
        {
            return param_names[i];
        }
        public string Param_String(int i)
        {
            string result = "";
            if (param_is_input[i])
            {
                result += "in ";
            }
            if (param_is_output[i])
            {
                result += "out ";
            }
            result += param_names[i];
            return result;
        }
        public bool is_input_parameter(int i)
        {
            return param_is_input[i];
        }
        public bool is_output_parameter(int i)
        {
            return param_is_output[i];
        }
        public Oval_Procedure(Component Successor, int height, int width, String str_name, int param_count)
			: base(Successor, height, width, str_name)
		{
            // placeholder until full allocation later
            // this is not for long-term use
            num_params = param_count;
		}
        public Oval_Procedure(Component Successor, int height, int width, String str_name,
            string[] incoming_param_names,
            bool[] is_input, bool[] is_output)
            : base(Successor, height, width, str_name)
        {
            param_names = incoming_param_names;
            num_params = incoming_param_names.Length;
            param_is_input = is_input;
            param_is_output = is_output;
            SetText();
        }

        private void SetText()
        {
            this.Text = "Start (";
            for (int i = 0; i < num_params; i++)
            {
                if (i > 0)
                {
                    this.Text = this.Text + ",";
                }
                if (param_is_input[i])
                {
                    this.Text = this.Text + "in ";
                }
                if (param_is_output[i])
                {
                    this.Text = this.Text + "out ";
                }
                this.Text = this.Text + param_names[i];
            }
            this.Text = this.Text + ")";
        }
        public Oval_Procedure(SerializationInfo info, StreamingContext ctxt)
			: base(info,ctxt)
		{
            this.num_params = info.GetInt32("_numparams");
            this.param_names = new string[this.num_params];
            this.param_is_input = new bool[this.num_params];
            this.param_is_output = new bool[this.num_params];
            for (int i = 0; i < num_params; i++)
            {
                this.param_names[i] = info.GetString("_paramname" + i);
                this.param_is_input[i] = info.GetBoolean("_paraminput" + i);
                this.param_is_output[i] = info.GetBoolean("_paramoutput" + i);
            }
        }
        public override void GetObjectData(SerializationInfo info, StreamingContext ctxt)
        {
            base.GetObjectData(info, ctxt);
            info.AddValue("_numparams", this.num_params);
            for (int i = 0; i < num_params; i++)
            {
                info.AddValue("_paramname" + i, this.param_names[i]);
                info.AddValue("_paraminput" + i, this.param_is_input[i]);
                info.AddValue("_paramoutput" + i, this.param_is_output[i]);
            }
        }
        public override void collect_variable_names(System.Collections.Generic.IList<string> l,
            System.Collections.Generic.IDictionary<string, string> types)
        {
            if (this.param_names != null)
            {
                for (int i = 0; i < this.param_names.Length; i++)
                {
                    l.Add(this.param_names[i]);
                }
            }
            if (this.Successor != null)
            {
                this.Successor.collect_variable_names(l,types);
            }
        }
        public override void wide_footprint(System.Drawing.Graphics gr)
        {
            int height_of_text, width_of_text = 2 * W;
            SizeF sz;

            height_of_text = Convert.ToInt32((gr.MeasureString(
                "Yes", PensBrushes.default_times)).Height);

            // loop starting at 2*W until you get on 2 lines.
            while (true)
            {
                sz = gr.MeasureString(
                    this.getDrawText() + "XX", PensBrushes.default_times,
                    width_of_text);
                if (sz.Height < height_of_text * 5 / 2)
                {
                    break;
                }
                width_of_text = width_of_text + W / 2;
            }

            if (sz.Height > height_of_text * 3 / 2)
            {
                FP.left = (width_of_text - W) / 2 + W / 2;
                FP.right = (width_of_text - W) / 2 + W / 2;
                drawing_text_width = width_of_text;
            }
            else if ((int)sz.Width > W)
            {
                width_of_text = W;
                while (width_of_text < (int)sz.Width)
                {
                    width_of_text += W / 2;
                }
                FP.left = (width_of_text - W) / 2 + W / 2;
                FP.right = (width_of_text - W) / 2 + W / 2;
                drawing_text_width = width_of_text;
            }
            else
            {
                drawing_text_width = 0;
            }
        }
        public override void draw(Graphics gr, int x, int y)
        {
            base.draw(gr, x, y);
        }
        public override bool setText(int x, int y, Visual_Flow_Form form)
        {
            bool textset = false;
            if (contains(x, y))
            {
                textset = true;
                if (Component.Current_Mode != Mode.Expert)
                {
                    form.menuRenameSubchart_Click(null, null);
                }
                return textset;
            }

            if (this.Successor != null)
            {
                return (this.Successor.setText(x, y, form));
            }

            return textset;
        }

    }
}
