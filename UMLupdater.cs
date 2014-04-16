using System;
using System.Collections.Generic;
using System.Text;
using System.Windows.Forms;

namespace raptor
{
    public class UMLupdater : NClass.Core.RAPTORUpdater
    {
        Visual_Flow_Form form;
        public UMLupdater(Visual_Flow_Form form)
        {
            this.form = form;
        }
        public void resetAttributes(object theClass, IEnumerable<NClass.Core.Field> fields)
        {
            ClassTabPage ctp = (theClass as ClassTabPage);
            ctp.listBox1.Nodes.Clear();
            foreach (NClass.Core.Field field in fields)
            {
                string s = "";
                if (field.IsReadonly || field.IsConstant)
                {
                    s += "(constant)";
                }
                if (field.IsStatic)
                {
                    s += "(static)";
                }
                ctp.listBox1.Nodes.Add(field.GetCaption()+s);
            }
            form.modified = true;
        }
        public object createClass(string name, NClass.Core.ClassType ct)
        {
            form.Clear_Undo();
            ClassTabPage ctp = new ClassTabPage(form, name);
            ctp.ct = ct;
            form.carlisle.TabPages.Add(ctp);
            form.modified = true;
            return ctp;
        }
        public void deleteClass(object theClass)
        {
            form.Clear_Undo();
            form.carlisle.TabPages.Remove(theClass as ClassTabPage);
            form.modified = true;
        }
        public void renameClass(object theClass, string name)
        {
            (theClass as ClassTabPage).Text = name;
            form.modified = true;
        }
        public object createMethod(object theClass, string name, NClass.Core.Method method)
        {
            form.Clear_Undo();
            Procedure_Chart sbchrt = new Procedure_Chart(form, name, 0);
            sbchrt.method = method;
            (theClass as ClassTabPage).tabControl1.TabPages.Add(sbchrt);
            form.modified = true;
            return sbchrt;
        }
        public bool makeAbstract(object theClass, object subchart)
        {
            if ((subchart as Procedure_Chart).Start.Count_Symbols() > 2)
            {
                DialogResult dr = MessageBox.Show("This will delete the code in " +
                    (subchart as Procedure_Chart).Text + ".\n" +
                    "Do you want to continue?", "Delete method?", MessageBoxButtons.YesNo);
                if (dr == DialogResult.No)
                {
                    return false;
                }
            }
            deleteMethod(theClass, subchart);
            form.modified = true;
            return true;
        }
        public void changeParameters(object theClass,
            object subchart,
            int num_params, string[] param_names, bool[] param_is_input, bool[] param_is_output)
        {
            ((subchart as Procedure_Chart).Start as Oval_Procedure).changeParameters(num_params,
                param_names, param_is_input, param_is_output);
            form.modified = true;
        }
        public void deleteMethod(object theClass, object subchart)
        {
            form.Clear_Undo();
            form.modified = true;
            (theClass as ClassTabPage).tabControl1.TabPages.Remove(subchart as Procedure_Chart);
        }
        public void renameMethod(object theClass, object subchart, string name)
        {
            (subchart as Procedure_Chart).Text = name;
            form.modified = true;
        }
        public void reorderMethods(object theClass, IEnumerable<NClass.Core.Operation> operations)
        {
            ClassTabPage ctp = (theClass as ClassTabPage);
            ctp.tabControl1.TabPages.Clear();
            foreach (NClass.Core.Operation operation in operations)
            {
                if (operation is NClass.Core.Method)
                {
                    (theClass as ClassTabPage).tabControl1.TabPages.Add(
                        (operation as NClass.Core.Method).raptorTab as Procedure_Chart);
                }
            }
            form.modified = true;
        }
    }
}
