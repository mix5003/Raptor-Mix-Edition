using System;
using System.Collections.Generic;
using System.Text;
using System.Reflection;
using System.Reflection.Emit;

namespace raptor
{
    public class Procedure_Chart : Subchart
    {
        public MethodBuilder subMethodBuilder;
        internal NClass.Core.Method method;
        public override int num_params
        {
            get
            {
                return ((Oval_Procedure) this.Start).Parameter_Count;
            }
        }
        public override string getFullName()
        {
            if (method != null && method.Parent != null)
            {
                return method.Parent.Name + "." + this.Text;
            }
            else
            {
                return this.Text;
            }
        }
        public string[] getArgs()
        {
            return ((Oval_Procedure)this.Start).getArgs();
        }
        public bool[] getArgIsInput()
        {
            return ((Oval_Procedure)this.Start).getArgIsInput();
        }
        public bool[] getArgIsOutput()
        {
            return ((Oval_Procedure)this.Start).getArgIsOutput();
        }
        public string param_string
        {
            get
            {
                return ((Oval_Procedure)this.Start).Parameter_String;
            }
        }
        public string parameter_name(int i)
        {
            return ((Oval_Procedure)this.Start).Param_Name(i);
        }
        public string parameter_string(int i)
        {
            return ((Oval_Procedure)this.Start).Param_String(i);
        }
        public bool is_input_parameter(int i)
        {
            return ((Oval_Procedure)this.Start).is_input_parameter(i);
        }
        public bool is_output_parameter(int i)
        {
            return ((Oval_Procedure)this.Start).is_output_parameter(i);
        }
        public string RunDialog(string name, Visual_Flow_Form form)
        {
            string result = ((Oval_Procedure)this.Start).RunDialog(name, form);
            this.flow_panel.Invalidate();
            return result;
        }
        public Procedure_Chart(Visual_Flow_Form the_form, string name, int param_count)
        {
            this.initialize(the_form, name);
            Start = new Oval_Procedure(End, Visual_Flow_Form.flow_height, Visual_Flow_Form.flow_width, "Oval",
                param_count);
            Start.Text = "Start";
            Start.scale = form.scale;
            Start.Scale(form.scale);
            this.flow_panel.Invalidate();
            this.kind = Subchart_Kinds.Procedure;
            if (!Component.MONO)
            {
                this.Initialize_Ink();
            }
        }
        public Procedure_Chart(Visual_Flow_Form the_form, string name, string[] incoming_param_names,
            bool[] is_input, bool[] is_output)
        {
            this.initialize(the_form, name);

            Start = new Oval_Procedure(End, Visual_Flow_Form.flow_height, Visual_Flow_Form.flow_width, "Oval",
                incoming_param_names,is_input,is_output);
            Start.scale = form.scale;
            Start.Scale(form.scale);
            this.flow_panel.Invalidate();
            this.kind = Subchart_Kinds.Procedure;
            if (!Component.MONO)
            {
                this.Initialize_Ink();
            }
        }
    }
}
