using System;

namespace raptor
{
	/// <summary>
	/// Summary description for Step_Helpers.
	/// </summary>
	public class Step_Helpers
	{
		public Step_Helpers()
		{
		}
        private enum State_Enum { Entering, Leaving };
        private static State_Enum state;
        public static void Set_State_Entering()
        {
            state = State_Enum.Entering;
        }
        public static void Set_State_Leaving()
        {
            state = State_Enum.Leaving;
        }
        public static bool Am_Leaving()
        {
            return state == State_Enum.Leaving;
        }
        public static Component call_rect;
        public static Component end_oval;
        public static Subchart next_chart;
        public static void Invoke_Super_Constructor(parse_tree.parameter_list parameters)
        {
            ClassTabPage ctp = Runtime.parent.currentClass();
            if (ctp == null)
            {
                throw new System.Exception("no constructor in " + Runtime.parent.running_tab.Text);
            }
            if (state == State_Enum.Entering)
            {
                int count = parse_tree_pkg.count_parameters(parameters);
                NClass.Core.ClassType theClass = Runtime.parent.projectCore.findClass(ctp.Text);
                // here's the super part
                theClass = theClass.BaseClass;
                findAndCallConstructor(theClass.Name, parameters, ctp, count, theClass);
            }
            else
            {
                call_rect.running = false;
            }
        }
        public static void Invoke_This_Constructor(parse_tree.parameter_list parameters)
        {
            ClassTabPage ctp = Runtime.parent.currentClass();
            if (ctp == null)
            {
                throw new System.Exception("no constructor in " + Runtime.parent.running_tab.Text);
            }
            if (state == State_Enum.Entering)
            {
                int count = parse_tree_pkg.count_parameters(parameters);
                NClass.Core.ClassType theClass = Runtime.parent.projectCore.findClass(ctp.Text);
                findAndCallConstructor(
                    theClass.Name, parameters, ctp, count, theClass);
            }
            else
            {
                call_rect.running = false;
            }
        }

        private static void findAndCallConstructor(
            string name,
            parse_tree.parameter_list parameters, 
            ClassTabPage ctp, int count, NClass.Core.ClassType theClass)
        {
            foreach (NClass.Core.Operation op in theClass.Operations)
            {
                if (op is NClass.Core.Constructor)
                {
                    if ((op as NClass.Core.Constructor).numberArguments == count)
                    {
                        Procedure_Chart sc = (op as NClass.Core.Constructor).raptorTab
                            as Procedure_Chart;
                        if (sc != null)
                        {
                            Runtime.Set_Running(sc);
                            Invoke_Tab_Procedure_Enter(
                                parameters, sc, Runtime.getContext());
                            CallStack.setContext(Runtime.getContext());
                            return;
                        }
                    }
                }
            }
            throw new System.Exception("could not find constructor for " + name +
                " with " + count + " parameter(s).");
        }
        public static bool Invoke_Constructor(string name, parse_tree.parameter_list parameters)
        {
            int count = parse_tree_pkg.count_parameters(parameters);
            NClass.Core.ClassType theClass = Runtime.parent.projectCore.findClass(name);
            while (theClass != null)
            {
                foreach (NClass.Core.Operation op in theClass.Operations)
                {
                    if (op is NClass.Core.Constructor)
                    {
                        if ((op as NClass.Core.Constructor).numberArguments == count)
                        {
                            Procedure_Chart sc = (op as NClass.Core.Constructor).raptorTab
                                as Procedure_Chart;
                            if (sc != null)
                            {
                                Runtime.Set_Running(sc);
                                Runtime.Variable this_value = Runtime.getContext();
                                Runtime.setContext(null);
                                // we stored the "this" parameter in the context
                                // but need to set it to null before entering the
                                // method
                                Invoke_Tab_Procedure_Enter(parameters, sc, this_value);
                                CallStack.setContext(Runtime.getContext());
                                return true;
                            }
                        }
                    }
                }
                theClass = theClass.BaseClass;
            }
            if (count == 0)
            {
                return false;
            }
            else
            {
                throw new System.Exception("could not find constructor for " + name +
                    " with " + count + " parameter(s).");
            }
        }
        public static void Invoke_Method(string name, parse_tree.parameter_list parameters)
        {
            if (Component.Current_Mode != Mode.Expert)
            {
                throw new Exception("there is no function named " + name);
            }
            Procedure_Chart sc = Runtime.Find_Method_Set_Running(name, parse_tree_pkg.count_parameters(parameters));

            if (state == State_Enum.Entering)
            {
                Runtime.Variable this_value = Runtime.getContext();
                Runtime.setContext(null);
                // we stored the "this" parameter in the context
                // but need to set it to null before entering the
                // method
                Invoke_Tab_Procedure_Enter(parameters, sc, this_value);
                // this used to be a call to getContext, but I'm not
                // at all sure why.
                CallStack.setContext(this_value);
            }
            else
            {
                Invoke_Tab_Procedure_Exit(parameters, sc);
            }
        }

        public static void Invoke_Tab_Procedure(string name,
            parse_tree.parameter_list parameters)
        {
            Subchart sc = Runtime.parent.Find_Tab(name) as Subchart;
            if (!(sc is Procedure_Chart))
            {
                end_oval.running = false;
                call_rect.running = false;
                Runtime.parent.Possible_Tab_Update(next_chart);
                return;
            }
            if (state == State_Enum.Entering)
            {
                Invoke_Tab_Procedure_Enter(parameters, sc as Procedure_Chart, null);
            }
            else
            {
                Invoke_Tab_Procedure_Exit(parameters, sc as Procedure_Chart);
            }
        }

        private static void Invoke_Tab_Procedure_Exit(
            parse_tree.parameter_list parameters,
            Procedure_Chart chart)
        {
            end_oval.running = true;
            call_rect.running = false;
            Runtime.parent.currentObj = end_oval;

            parse_tree.parameter_list walk;
            int num_parameters = chart.num_params;
            walk = parameters;
            numbers.value[] the_values = new numbers.value[num_parameters];
            string[] param_strings = new string[num_parameters];
            bool[] is_array = new bool[num_parameters];
            bool[] is_2d_array = new bool[num_parameters];
            object[] old_values = new object[num_parameters];
            for (int i = 0; i < num_parameters; i++)
            {
                if (chart.is_output_parameter(i))
                {
                    try
                    {
                        param_strings[i] = ((parse_tree.expr_output)
                            walk.parameter).get_string();
                    }
                    catch
                    {
                        param_strings[i] = "";
                    } 
                    if (Runtime.isArray(chart.parameter_name(i)))
                    {
                        is_array[i] = true;
                        is_2d_array[i] = false;
                        old_values[i] = Runtime.getValueArray(chart.parameter_name(i));
                    }
                    else if (Runtime.is_2D_Array(chart.parameter_name(i)))
                    {
                        is_array[i] = false;
                        is_2d_array[i] = true;
                        old_values[i] = Runtime.get2DValueArray(chart.parameter_name(i));
                    }
                    else
                    {
                        is_array[i] = false;
                        is_2d_array[i] = false;
                        old_values[i] = Runtime.getVariable(chart.parameter_name(i));
                    }
                }
                walk = walk.next;
            }
            Runtime.Decrease_Scope();
            end_oval.running = false;
            call_rect.running = true;
            Runtime.parent.currentObj = call_rect;
            Runtime.parent.Possible_Tab_Update(next_chart);

            walk = parameters;
            for (int i = 0; i < num_parameters; i++)
            {
                if (chart.is_output_parameter(i))
                {
                    if (is_array[i])
                    {
                        numbers.value[] values = (numbers.value []) old_values[i];
                        for (int ind1 = values.Length - 1; ind1 >= 0; ind1--)
                        {
                            Runtime.setArrayElement(param_strings[i], ind1 + 1, values[ind1]);
                        }
                    }
                    else if (is_2d_array[i])
                    {
                        numbers.value[][] values = old_values[i] as numbers.value[][];
                        for (int ind1 = values.Length - 1; ind1 >= 0; ind1--)
                        {
                            for (int ind2 = values[0].Length - 1; ind2 >= 0; ind2--)
                            {
                                Runtime.set2DArrayElement(param_strings[i],
                                    ind1 + 1, ind2 + 1, values[ind1][ind2]);
                            }
                        }
                    }
                    else
                    {
                        numbers.value t = (numbers.value) old_values[i];
                        parse_tree_pkg.ms_assign_to(walk.parameter, t, "Parameter " + (i + 1) + ":");
                        //Runtime.setVariable(param_strings[i],
                        //    t);
                    }
                }
                walk = walk.next;
            }
            call_rect.running = false;
        }
        private static void Invoke_Tab_Procedure_Enter(
            parse_tree.parameter_list parameters,
            Procedure_Chart chart,
            Runtime.Variable this_value)
        {
            parse_tree.parameter_list walk;
            int num_parameters = chart.num_params;
			numbers.value[] the_values = new numbers.value[num_parameters];
			//string[] param_strings = new string[num_parameters];
            bool[] is_array = new bool[num_parameters];
            bool[] is_2d_array = new bool[num_parameters];
            object[] array_values = new object[num_parameters];
            int call_site_param_count = 0;
            // count the parameters
            walk = parameters;
            while (walk != null)
            {
                call_site_param_count++;
                walk = walk.next;
            }
            // start walk at beginning again
            walk = parameters;
            if (call_site_param_count != num_parameters)
            {
                string paramstr, verb;
                if (num_parameters != 1)
                {
                    paramstr = "parameters";
                }
                else
                {
                    paramstr = "parameter";
                }
                if (call_site_param_count != 1)
                {
                    verb = "were";
                }
                else
                {
                    verb = "was";
                }
                throw new System.Exception(chart.Text + " requires " + num_parameters + " " + paramstr + " but " +
                    call_site_param_count + " " + verb + " provided.");
            }
			for (int i=0; i<num_parameters; i++)
			{
                if (chart.is_input_parameter(i)) 
                {
                    try
                    {
                        raptor.Runtime.processing_parameter_list = true;
                        the_values[i] = ((parse_tree.expr_output)
                            walk.parameter).expr.execute();
                    }
                    catch
                    {
                        raptor.Runtime.processing_parameter_list = false;
                        throw;
                    }
                    raptor.Runtime.processing_parameter_list = false;

                    if (numbers_pkg.is_ref_1d(the_values[i]))
                    {
                        is_array[i] = true;
                        is_2d_array[i] = false;
                        array_values[i] = Runtime.getValueArray(
                            numbers_pkg.object_of(the_values[i]) as Runtime.Variable);
                    }
                    else if (numbers_pkg.is_ref_2d(the_values[i]))
                    {
                        is_array[i] = false;
                        is_2d_array[i] = true;
                        array_values[i] = Runtime.get2DValueArray(
                            numbers_pkg.object_of(the_values[i]) as Runtime.Variable);
                    }
                    else
                    {
                        is_array[i] = false;
                        is_2d_array[i] = false;
                    }
                }
                walk = walk.next;
            }
            Runtime.Increase_Scope(chart.Text);
            if (this_value != null)
            {
                if (chart.method.IsStatic)
                {
                    if (this_value.Kind != Runtime.Variable_Kind.Class_Value)
                    {
                        throw new System.Exception("can't call static method " + chart.Text + " with object");
                    }
                }
                else if (this_value.Kind == Runtime.Variable_Kind.Heap_Object)
                {
                    Runtime.setVariable("this", this_value.Variable_Value);
                }
                else
                {
                    throw new System.Exception("can't call dispatching method " + chart.Text + " without object");
                }

            }
            for (int i = 0; i < num_parameters; i++)
            {
                if (chart.is_input_parameter(i))
                {
                    if (is_array[i])
                    {
                        numbers.value[] values = array_values[i] as numbers.value[];
                        for (int ind1 = values.Length - 1; ind1 >= 0; ind1--)
                        {
                            Runtime.setArrayElement(chart.parameter_name(i), ind1+1, values[ind1]);
                        }
                    }
                    else if (is_2d_array[i])
                    {
                        numbers.value[][] values = array_values[i] as numbers.value[][];
                        for (int ind1 = values.Length - 1; ind1 >= 0; ind1--)
                        {
                            for (int ind2 = values[0].Length - 1; ind2 >= 0; ind2--)
                            {
                                Runtime.set2DArrayElement(chart.parameter_name(i), 
                                    ind1+1, ind2+1, values[ind1][ind2]);
                            }
                        }
                    }
                    else
                    {
                        Runtime.setVariable(chart.parameter_name(i), 
                                the_values[i]);
                    }
                }
            }
        }

		internal static Component Step_Once(Component currentObj, Visual_Flow_Form form)
		{
            Component next;
            if (currentObj.need_to_decrease_scope)
            {
                Runtime.Decrease_Scope();
                currentObj.need_to_decrease_scope=false;
                if (parse_tree_pkg.did_function_call)
                {
                    if (Runtime.method_return_value != null)
                    {
                        currentObj.values.Add(Runtime.method_return_value);
                    }
                    Runtime.method_return_value = null;
                }
            }
            while (currentObj.number_method_expressions_run <
                currentObj.method_expressions.Count)
            {
                CallStack.Push(currentObj, (Subchart)form.running_tab);
                Step_Helpers.Set_State_Entering();
                bool b = interpreter_pkg.run_expon(
                   currentObj.method_expressions[currentObj.number_method_expressions_run]
                   as parse_tree.expon,
                   currentObj.Text,
                   currentObj);
                currentObj.number_method_expressions_run++; 
                if (b)
                {
                    currentObj.need_to_decrease_scope=true;
                    form.Possible_Tab_Update(form.running_tab);
                    form.currentObj = ((Subchart)form.running_tab).Start;
                    form.currentObj.running = true;
                    return form.currentObj;
                }
                else
                {
                    CallStack.Pop();
                }
            }
            if (currentObj.Name=="Oval")
			{
				if (currentObj.Successor!=null) 
				{
                    next = currentObj.Successor.First_Of();
                    next.reset_this_method_expressions_run();
					return next;
				}
				else
				{
					return null;
				}
			}
            else if (currentObj.Name == "Return")
            {
                numbers.value v = interpreter_pkg.run_return_value(currentObj.parse_tree,
                    currentObj.Text);
                Runtime.method_return_value = v;
                // hack for recursion in OO-mode, 1 level deep only!
                currentObj.values.Clear();
                return null;
            }
            else if (currentObj.Name == "Rectangle")
            {
                // when running belownormal, upgrade priority to close graphics window
                if (currentObj.Text.ToLower() == "close_graph_window")
                {
                    System.Threading.ThreadPriority p = System.Threading.Thread.CurrentThread.Priority;
                    System.Threading.Thread.CurrentThread.Priority = System.Threading.ThreadPriority.AboveNormal;
                    interpreter_pkg.run_assignment(currentObj.parse_tree,
                        currentObj.Text);
                    System.Threading.Thread.CurrentThread.Priority = p;
                }
                else
                {
                    interpreter_pkg.run_assignment(currentObj.parse_tree,
                        currentObj.Text);
                }
                // hack for recursion in OO-mode, 1 level deep only!
                currentObj.values.Clear();
                next = Find_Successor(currentObj);
                next.reset_this_method_expressions_run();
                return next;
            }
            else if (currentObj.Name == "IF_Control")
            {
                bool choice = interpreter_pkg.run_boolean(
                    currentObj.parse_tree,
                    currentObj.Text);
                if (choice == true)
                {
                    // go to left child if possible, otherwise
                    // go to the successor of the if
                    if (((IF_Control)currentObj).yes_child != null)
                    {
                        next = ((IF_Control)currentObj).
                            yes_child.First_Of();
                        next.reset_this_method_expressions_run();
                        return next;
                    }
                    else
                    {
                        next = Find_Successor(currentObj);
                        next.reset_this_method_expressions_run();
                        return next;
                    }
                }
                else
                {
                    if (((IF_Control)currentObj).no_child != null)
                    {
                        next = ((IF_Control)currentObj).
                            no_child.First_Of();
                        next.reset_this_method_expressions_run();
                        return next;
                    }
                    else
                    {
                        next = Find_Successor(currentObj);
                        next.reset_this_method_expressions_run();
                        return next;
                    }
                }
            }
            else if (currentObj.Name == "Parallelogram")
            {
                if (((Parallelogram)currentObj).is_input)
                {
                    interpreter_pkg.run_input(currentObj.parse_tree,
                        currentObj.Text,
                        ((Parallelogram)currentObj).prompt,
                        ((Parallelogram)currentObj).input_is_expression,
                        ((Parallelogram)currentObj).prompt_tree);
                }
                else
                {
                    interpreter_pkg.run_output(currentObj.parse_tree,
                        currentObj.Text);
                }
                next = Find_Successor(currentObj);
                next.reset_this_method_expressions_run();
                return next;
            }
            else if ((currentObj.Name == "Loop") &&
                (((Loop)currentObj).light_head))
            {
                ((Loop)currentObj).light_head = false;
                if (((Loop)currentObj).before_Child != null)
                {
                    next = ((Loop)currentObj).before_Child.First_Of();
                    next.reset_this_method_expressions_run();
                    return next;
                }
                else
                {
                    return currentObj;
                }
            }
            else if (currentObj.Name == "Loop")
            {
                bool choice = interpreter_pkg.run_boolean(
                    currentObj.parse_tree,
                    currentObj.Text);
                // keep going on yes if loop logic reversed, no if not
                if ((!choice && !Component.reverse_loop_logic) ||
                    (choice && Component.reverse_loop_logic))
                {
                    // on false, go to first of after_child if
                    // we can, otherwise back to the top of the loop!
                    if (((Loop)currentObj).after_Child != null)
                    {
                        next = ((Loop)currentObj).after_Child.First_Of();
                        next.reset_this_method_expressions_run();
                        return next;
                    }
                    else
                    {
                        next = currentObj.First_Of();
                        next.reset_this_method_expressions_run();
                        return next;
                    }
                }
                else
                {
                    // on true, go to successor of loop
                    next = Find_Successor(currentObj);
                    next.reset_this_method_expressions_run();
                    return next;
                }
            }
            else
            {
                throw new System.Exception("unrecognized object: " +
                    currentObj.Name);
            }
		}
		public static Component Find_Successor(Component c)
		{
			// if I've got a successor, just go there!
			if (c.Successor!=null)
			{
				return c.Successor.First_Of();
			}
			if (c.parent==null)
			{
				throw new System.Exception(
					"I have no successor or parent!");
			}
			// if I'm the child of an IF statement, then I just
			// want the successor of my parent
			if (c.parent.Name=="IF_Control")
			{
				return Find_Successor(c.parent);
			}
			else if (c.parent.Name=="Loop")
			{
				// if I'm the before child of a loop,
				// then go to the loop (test)
				if (c.is_beforeChild) 
				{
					return c.parent;
				}
				else
				{
					// since I'm an after child, I just want
					// to go back to the top of the loop
					return c.parent.First_Of();
				}
			}
			else
			{
				throw new Exception(
					"My parent isn't a loop or if_control!");
			}
		}
	}
}
