using System;
using System.Drawing;

namespace raptor
{
	/// <summary>
	/// Summary description for Dialog_Helpers.
	/// </summary>
	public class Dialog_Helpers
	{
        private static int current_suggestion_line = -1;
        private static bool have_bold;
        private static System.Collections.Generic.List<string> list;
        private static System.Collections.Generic.List<string> oo_suggestion_list;
        private static System.Collections.Generic.Dictionary<string, string> types;
        public static System.Collections.Generic.List<string> Get_List()
        {
            return list;
        }

        public static System.Collections.Generic.List<string> Get_OO_List()
        {
            return oo_suggestion_list;
        }

		public Dialog_Helpers()
		{
			//
			// TODO: Add constructor logic here
			//
		}
        internal static int Suggestions(NClass.Core.CompositeType startingClass, string name, bool isStatic)
        {
            NClass.Core.CompositeType ct = startingClass;
            while (ct != null && !ct.Name.Equals("Object"))
            {
                foreach (NClass.Core.Field f in ct.Fields)
                {
                    if ((f.IsStatic==isStatic) && f.Name.ToLower().StartsWith(name))
                    {
                        oo_suggestion_list.Add(f.Name);
                    }
                }
                foreach (NClass.Core.Operation o in ct.Operations)
                {
                    if (o is NClass.Core.Method && (o.IsStatic==isStatic) && 
                        (!(o is NClass.Core.Constructor)) && 
                        o.Name.ToLower().StartsWith(name))
                    {
                        NClass.Core.Method m = (o as NClass.Core.Method);
                        int num_params;
                        string[] param_names;
                        bool[] param_is_input;
                        bool[] param_is_output;
                        System.Text.StringBuilder sb = new System.Text.StringBuilder();
                        sb.Append(m.Name + "(");
                        m.getParameters(out num_params, out param_names, 
                            out param_is_input, out param_is_output);
                        for (int i = 0; i < num_params; i++)
                        {
                            sb.Append(param_names[i]);
                            if (i < num_params-1)
                            {
                                sb.Append(",");
                            }
                        }
                        sb.Append(")");
                        oo_suggestion_list.Add(sb.ToString());
                    }
                }
                if (ct is NClass.Core.ClassType)
                {
                    ct = (ct as NClass.Core.ClassType).BaseClass;
                }
                else
                {
                    ct = null;
                }
            }
            return oo_suggestion_list.Count;
        }
        internal static int Prefix_Suggestion_Count(string name)
        {
            oo_suggestion_list.Clear();
            int index = name.IndexOf('.');
            if (name.LastIndexOf('.') != index)
            {
                return 0;
            }
            if (name.StartsWith("this."))
            {
                if (Runtime.parent.carlisle.SelectedTab is ClassTabPage &&
                    ((Runtime.parent.carlisle.SelectedTab as ClassTabPage).tabControl1.SelectedTab
                      as Procedure_Chart).method != null)
                {
                    NClass.Core.ClassType ct =
                        ((Runtime.parent.carlisle.SelectedTab as ClassTabPage).tabControl1.SelectedTab
                      as Procedure_Chart).method.Parent
                        as NClass.Core.ClassType;
                    return Suggestions(ct,name.Substring(index+1),false);
                }
            }
            else if (name.StartsWith("super."))
            {
                if (Runtime.parent.carlisle.SelectedTab is ClassTabPage &&
                    ((Runtime.parent.carlisle.SelectedTab as ClassTabPage).tabControl1.SelectedTab
                      as Procedure_Chart).method != null)
                {
                    NClass.Core.ClassType ct =
                        ((Runtime.parent.carlisle.SelectedTab as ClassTabPage).tabControl1.SelectedTab
                      as Procedure_Chart).method.Parent
                        as NClass.Core.ClassType;
                    return Suggestions(ct.BaseClass,name.Substring(index+1),false);
                }
            }
            else
            {
                foreach (NClass.Core.IEntity ie in Runtime.parent.projectCore.Entities)
                {
                    if (name.StartsWith(ie.Name.ToLower() + "."))
                    {
                        if (ie is NClass.Core.CompositeType)
                        {
                            NClass.Core.CompositeType ct = ie as NClass.Core.CompositeType;
                            return Suggestions(ct, name.Substring(index + 1), true);
                        }
                        else if (ie is NClass.Core.EnumType)
                        {
                            NClass.Core.EnumType et = ie as NClass.Core.EnumType;
                        }
                    }
                }
                if (types.ContainsKey(name.Substring(0, index).ToLower()))
                {
                    string typename = types[name.Substring(0, index).ToLower()];
                    foreach (NClass.Core.IEntity ie in Runtime.parent.projectCore.Entities)
                    {
                        if (ie.Name.ToLower().Equals(typename.ToLower()))
                        {
                            if (ie is NClass.Core.CompositeType)
                            {
                                NClass.Core.CompositeType ct = ie as NClass.Core.CompositeType;
                                return Suggestions(ct, name.Substring(index + 1), false);
                            }
                            else if (ie is NClass.Core.EnumType)
                            {
                            }
                        }
                    }
                }
            }
            return 0;

        }

        public static void Init()
        {
            have_bold = false;
            current_suggestion_line = -1;
            list = new System.Collections.Generic.List<string>();
            types = new System.Collections.Generic.Dictionary<string, string>();
            oo_suggestion_list = new System.Collections.Generic.List<string>();
            if (Runtime.parent.carlisle.SelectedTab is ClassTabPage)
            {
                ((Procedure_Chart)((ClassTabPage)Runtime.parent.carlisle.SelectedTab).tabControl1.SelectedTab).Start.collect_variable_names(list,types);
                NClass.Core.Method method = ((Procedure_Chart)((ClassTabPage)Runtime.parent.carlisle.SelectedTab).tabControl1.SelectedTab).method;
                if (method != null)
                {
                    for (int i = 0; i < method.numberArguments; i++)
                    {
                        NClass.Core.Parameter p = method.getParameter(i);
                        if (!types.ContainsKey(p.Name.ToLower()))
                        {
                            types.Add(p.Name.ToLower(), p.Type);
                        }
                    }
                }
            }
            else if (Runtime.parent.carlisle.SelectedTab is Procedure_Chart)
            {
                ((Procedure_Chart)Runtime.parent.carlisle.SelectedTab).Start.collect_variable_names(list,types);
            }
            else
            {
                for (int i = Runtime.parent.mainIndex; i < Runtime.parent.carlisle.TabCount; i++)
                {
                    if ((Runtime.parent.carlisle.TabPages[i] is Subchart) &&
                        !(Runtime.parent.carlisle.TabPages[i] is Procedure_Chart))
                    {
                        ((Subchart)Runtime.parent.carlisle.TabPages[i]).Start.collect_variable_names(list,types);
                    }
                }
            }
            if (Component.Current_Mode == Mode.Expert)
            {
                if (Runtime.parent.carlisle.SelectedIndex!=1)
                {
                   list.Add("this");
                   list.Add("super");
                }
                foreach (NClass.Core.IEntity ie in Runtime.parent.projectCore.Entities)
                {
                    if (ie is NClass.Core.ClassType)
                    {
                        list.Add(ie.Name);
                    }
                }
            }
            list.Sort();
            int k = 0;
            while (k < list.Count - 1)
            {
                if (list[k].ToLower() == list[k + 1].ToLower())
                {
                    list.Remove(list[k + 1]);
                }
                else
                {
                    k++;
                }
            }
        }
        // Does s begin and end with " and have only "" or \" in the middle?
        public static bool Is_Simple_String(string s)
		{
			int i;
			if (s[0]!='"'||s[s.Length-1]!='"')
			{
				return false;
			}
			else
			{
				i = 1;
				while (i<s.Length-1) 
				{
					if (s[i]=='\\')
					{
						// if end with \", this is bad
						if (i==s.Length-2)
						{
							return false;
						}
					    // otherwise, \ escapes next character, so skip
						else
						{
							i=i+1;
						}
					}
					else if (s[i]=='"')
					{
						// if end with "", this is not simple string,
						// also if not ""
						if (i==s.Length-2 || s[i+1]!='"')
						{
							return false;
						}
						else
						{
							i=i+1;
						}
					}
					i++;
				}
				return true;
			}
		}

		public static void Check_Hint(
			System.Windows.Forms.TextBox textbox,
			System.Windows.Forms.RichTextBox textBox1,
            int kind,
			ref string current_suggestion,
			ref interpreter.suggestion_result suggestion_result,
			ref bool error,
			System.Drawing.Font this_Font
			)
		{
            // collapse into a single line
			if (textbox.Lines.Length>1)
			{
				textbox.Text=textbox.Lines[0]+textbox.Lines[1];
				textbox.Select(textbox.Text.Length,0);
			}
			suggestion_result = interpreter_pkg.suggestion(textbox.Text,kind);
            have_bold = suggestion_result.bold_start > 0 &&
                    suggestion_result.bold_finish >= suggestion_result.bold_start;
            if (have_bold)
            {
                string[] copy = new string[suggestion_result.suggestions.Length-1];
                for (int k = 1; k < suggestion_result.suggestions.Length; k++)
                {
                    copy[k - 1] = suggestion_result.suggestions[k];
                }
                Array.Sort(copy);
                for (int k = 1; k < suggestion_result.suggestions.Length; k++)
                {
                    suggestion_result.suggestions[k] = copy[k-1];
                }
            }
            else
            {
                Array.Sort(suggestion_result.suggestions);
            }
            if (!Component.MONO)
            {
                textBox1.Lines = suggestion_result.suggestions;
            }
            else
            {
                textBox1.Lines = addNewlines(suggestion_result.suggestions);
            }

			textBox1.SelectAll();
			textBox1.SelectionColor=System.Drawing.Color.Black;
			// if the current suggestion is unavailable, switch to the first one
            if (suggestion_result.suggestions.Length == 0)
            {
                current_suggestion = "";
                current_suggestion_line = -1;
                textBox1.Hide();
            }
            else
            {
                if (suggestion_result.suggestions.Length > 1)
                {
                    int loc;
                    if (current_suggestion != null)
                    {
                        try
                        {
                            loc = textBox1.Find(current_suggestion);
                        }
                        catch
                        {
                            loc = -1;
                        }
                    }
                    else
                    {
                        loc = -1;
                    }
                    if (loc < 0 && !have_bold)
                    {
                        current_suggestion = suggestion_result.suggestions[0];
                        current_suggestion_line = 0;
                    }
                    else if (loc <= 0 && have_bold)
                    {
                        current_suggestion = suggestion_result.suggestions[1];
                        current_suggestion_line = 1;
                    }
                    int index;
                    try
                    {
                        index = textBox1.Find(current_suggestion);
                        current_suggestion_line = textBox1.GetLineFromCharIndex(index);

                        textBox1.Select(index,
                            current_suggestion.Length);
                    }
                    catch
                    {
                    }
                    textBox1.SelectionColor = System.Drawing.Color.Red;
                    error = false;
                    textBox1.Show();
                }
                else if (suggestion_result.suggestions.Length == 1)
                {
                    current_suggestion = suggestion_result.suggestions[0];
                    current_suggestion_line = 0;
                    try
                    {
                        textBox1.Select(textBox1.Find(current_suggestion),
                            current_suggestion.Length);
                        if (have_bold)
                        {
                            textBox1.SelectionColor = System.Drawing.Color.Black;
                        }
                        else
                        {
                            textBox1.SelectionColor = System.Drawing.Color.Red;
                        }
                    }
                    catch
                    {
                    }
                }
                if (have_bold)
                {
                    textBox1.Select(suggestion_result.bold_start - 1, suggestion_result.bold_finish - suggestion_result.bold_start + 1);
                    textBox1.SelectionFont = new Font(this_Font, FontStyle.Bold);
                    textBox1.Select(0, textBox1.Lines[0].Length);
                    textBox1.SelectionColor = System.Drawing.Color.Black;
                }
                error = false;
                textBox1.Show();
            }
		}

        private static string[] addNewlines(string[] p)
        {
            string[] result = new string[p.Length];
            for (int i = 0; i < p.Length; i++)
            {
                result[i] = p[i] + '\n';
            }
            return result;
        }

        // is_rhs indicates if this is an expression (right hand side) where no procedure calls may occur
        public static bool Complete_Suggestion(
			System.Windows.Forms.TextBox textbox,
            int kind,
			string current_suggestion,
			ref interpreter.suggestion_result suggestion_result)
		{
			int location;
			int paren_location;
			bool Handled = false;

			suggestion_result = interpreter_pkg.suggestion(textbox.Text,kind);

			if ((current_suggestion!=null) &&
				current_suggestion!="" &&
                textbox.Text.Length>1 &&
				textbox.Text[textbox.Text.Length-1] != ')' &&
                // how can I fix this?  I don't want to complete if I'm in the bolded suggestion
				(suggestion_result.bold_start < 0 ||
				suggestion_result.bold_finish < suggestion_result.bold_start ||
                current_suggestion_line!=0))
			{
				// if we're inside the parens-- don't bother
				Handled = true;
				location = textbox.Text.Length-1;
				while ((location > 0) && 
					(Char.IsLetterOrDigit(textbox.Text,location)
					|| textbox.Text[location]=='_'))
				{
					location--;
				}
				// ok, so now from location to end will be replaced by 
				// the suggestion up to the parenthesis
				paren_location = current_suggestion.IndexOf("(");
				if (paren_location < 0) 
				{
					paren_location = current_suggestion.Length;
				}

                // we've got no () to do and are at the end.
				if (textbox.Text.Length-location>=current_suggestion.Length)
				{
					return false;
				}
				if (location == 0)
				{
					textbox.Text = 
						current_suggestion.Substring(0,paren_location);
				}
				else
				{
					textbox.Text = 
						textbox.Text.Substring(0,location+1) +
						current_suggestion.Substring(0,paren_location);
				}
				textbox.SelectionStart =
					textbox.Text.Length;
				textbox.SelectionLength = 0;
			}
			return Handled;
		}

		public static void Paint_Helper(
			System.Drawing.Graphics labelGraphics,
			string text,
			System.Windows.Forms.Label label,
			string error_msg,
			int location,
			bool error)
		{
		    string first_half;
			string second_half;
			string error_part;

			if (error)
			{
				labelGraphics.Clear(System.Drawing.Color.White);
				System.Drawing.StringFormat stringFormat = new System.Drawing.StringFormat();
				// Center the block of text (top to bottom) in the rectangle.
				stringFormat.LineAlignment = System.Drawing.StringAlignment.Center;

				labelGraphics.DrawString(error_msg, PensBrushes.arial8, PensBrushes.blackbrush, 0,12, stringFormat);
				
				int l = text.Length;

				if(l < location)
				{
					first_half = text.Substring(0,l);
					labelGraphics.DrawString(first_half, PensBrushes.arial8, PensBrushes.blackbrush, 0, 25, stringFormat);
					int first_half_length = Convert.ToInt32((labelGraphics.MeasureString(
						first_half, PensBrushes.arial8)).Width);
					second_half = "_";
					labelGraphics.DrawString(second_half, PensBrushes.arial8, PensBrushes.redbrush, first_half_length-3,25, stringFormat);
				}

				if(l == location)
				{
					first_half = text.Substring(0,l-1);
					labelGraphics.DrawString(first_half, PensBrushes.arial8, PensBrushes.blackbrush, 0, 25, stringFormat);
					int first_half_length = Convert.ToInt32((labelGraphics.MeasureString(
						first_half, PensBrushes.arial8)).Width);
					second_half = text.Substring(location-1,1);
					labelGraphics.DrawString(second_half, PensBrushes.arial8, PensBrushes.redbrush, first_half_length-3,25, stringFormat);
				}
				if (l > location)
				{
					first_half = text.Substring(0,location-1);
					labelGraphics.DrawString(first_half, PensBrushes.arial8, PensBrushes.blackbrush, 0,25, stringFormat);
					int first_half_length = Convert.ToInt32((labelGraphics.MeasureString(
						first_half, PensBrushes.arial8)).Width);
					second_half = text.Substring(location,l-location);
					error_part = text.Substring(location-1,1);
					int error_part_length = Convert.ToInt32((labelGraphics.MeasureString(
						error_part, PensBrushes.arial8)).Width);
					labelGraphics.DrawString(first_half, PensBrushes.arial8, PensBrushes.blackbrush, 0,25, stringFormat);
					labelGraphics.DrawString(error_part, PensBrushes.arial8, PensBrushes.redbrush, first_half_length-3,25, stringFormat);
					labelGraphics.DrawString(second_half, PensBrushes.arial8, PensBrushes.blackbrush, first_half_length + error_part_length-6,25, stringFormat);
				}
			}
			else
			{
				labelGraphics.Clear(label.BackColor);
			}
			
		}
		public static void suggestions_downarrow(
			System.Windows.Forms.RichTextBox textBox1,
			ref string current_suggestion)
		{
			int line = -1;
			if (textBox1.Visible)
			{
				// if it's the last line, don't bother
				for (int i=0; i < textBox1.Lines.Length-1; i++)
				{
					if (textBox1.Lines[i].Equals(current_suggestion))
					{
						line = i;
					}
				}
				if (line >= 0)
				{
                    try
                    {
                        textBox1.Select(
                            textBox1.Find(current_suggestion),
                            textBox1.Lines[line].Length);
                        textBox1.SelectionColor = System.Drawing.Color.Black;
                        textBox1.Select(
                            textBox1.Find(textBox1.Lines[line + 1]),
                            textBox1.Lines[line + 1].Length);
                        textBox1.SelectionColor = System.Drawing.Color.Red;
                        current_suggestion_line = line + 1;
                        current_suggestion = textBox1.Lines[current_suggestion_line];
                        int index = textBox1.GetCharIndexFromPosition(
                            new System.Drawing.Point(3, 3));
                        int top_line = textBox1.GetLineFromCharIndex(index);
                        if (line - top_line > 5)
                        {
                            textBox1.Select(
                                textBox1.Find(textBox1.Lines[top_line + 1]),
                                textBox1.Lines[top_line + 1].Length);
                            textBox1.ScrollToCaret();
                        }
                    }
                    catch
                    {
                    }
                }
			}
		}

		public static void suggestions_uparrow(
			System.Windows.Forms.RichTextBox textBox1,
			ref string current_suggestion)
		{
			int line = -1;

			if (textBox1.Visible)
			{
				// if it's the first line, don't bother
				for (int i=1; i < textBox1.Lines.Length; i++)
				{
					if (textBox1.Lines[i].Equals(current_suggestion))
					{
						line = i;
					}
				}
                if (line > ((have_bold) ? 1 : 0))
				{
                    try
                    {
                        textBox1.Select(
                            textBox1.Find(current_suggestion),
                            textBox1.Lines[line].Length);
                        textBox1.SelectionColor = System.Drawing.Color.Black;
                        textBox1.Select(
                            textBox1.Find(textBox1.Lines[line - 1]),
                            textBox1.Lines[line - 1].Length);
                        textBox1.SelectionColor = System.Drawing.Color.Red;
                        current_suggestion = textBox1.Lines[line - 1];
                        current_suggestion_line = line - 1;
                        int index = textBox1.GetCharIndexFromPosition(
                            new System.Drawing.Point(3, 3));
                        int top_line = textBox1.GetLineFromCharIndex(index);
                        if (line <= top_line)
                        {
                            textBox1.Select(
                                textBox1.Find(textBox1.Lines[top_line - 1]),
                                textBox1.Lines[top_line - 1].Length);
                            textBox1.ScrollToCaret();
                        }
                    }
                    catch
                    {
                    }
				}
			}
		}

		public static void suggestions_mousedown(
			System.Windows.Forms.RichTextBox textBox1,
			ref string current_suggestion,
			System.Windows.Forms.MouseEventArgs e)
		{
			int index = textBox1.GetCharIndexFromPosition(
				new System.Drawing.Point(e.X,e.Y));
			int line = textBox1.GetLineFromCharIndex(index);
			// if it's the last line, don't bother
			if ((line >= ((have_bold) ? 1 : 0)) && (line < textBox1.Lines.Length) &&
				textBox1.Lines.Length > 1)
			{
                try
                {
                    if (current_suggestion_line >= 0)
                    {
                        textBox1.Select(textBox1.Find(current_suggestion),
                            textBox1.Lines[current_suggestion_line].Length);
                    }
                    textBox1.SelectionColor = System.Drawing.Color.Black;
                    current_suggestion = textBox1.Lines[line];
                    current_suggestion_line = line;
                    textBox1.Select(
                        textBox1.Find(current_suggestion),
                        textBox1.Lines[line].Length);
                    textBox1.SelectionColor = System.Drawing.Color.Red;
                }
                catch
                {
                }
			}
		}
	}
}
