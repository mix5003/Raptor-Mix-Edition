using System;
using System.Windows.Forms;
using System.Drawing;
using Microsoft.Ink;

namespace raptor
{
	/// <summary>
	/// Summary description for Subchart.
	/// </summary>
    public enum Subchart_Kinds { Subchart, Procedure, Function, UML };

	public class Subchart : System.Windows.Forms.TabPage
	{
		public System.Windows.Forms.Panel panel1;
		public Buffered flow_panel;
		protected static Visual_Flow_Form form;
		public Oval Start, End;
		private int selected_comment_x, selected_comment_y;
		public bool am_compiling=false;

		public System.Drawing.Rectangle selection_rectangle;
		public System.Drawing.Point scroll_location = 
			new System.Drawing.Point(0,0);
		public bool region_selected = false;
        protected Subchart_Kinds kind = Subchart_Kinds.Subchart;
        public InkOverlay tab_overlay;

        public virtual string getFullName()
        {
            return this.Text;
        }
        public virtual int num_params
        {
            get
            {
                return 0;
            }
        }
        public Subchart_Kinds Subchart_Kind
        {
            get
            {
                return kind;
            }
        }
		public Component Current_Selection 
		{
			set 
			{
				current_selection = value;
				region_selected = false;
			}
			get
			{
				return current_selection;
			}
		}
        private bool have_cut_drag = false;
        private bool have_drag = false;
        public bool Am_Dragging
        {
            get
            {
                return have_drag;
            }
        }
        private Component current_selection = null;
		public Component Breakpoint_Selection = null;
		public CommentBox selectedComment = null;
        private int drag_x = 0;
        private int drag_y = 0;
        private bool possible_drag_drop = false;
        public double ink_resolution;
        
		protected void initialize(Visual_Flow_Form the_form, string s) {
			form = the_form;
			this.Text=s;

			this.panel1 = new System.Windows.Forms.Panel();
			this.flow_panel = new raptor.Buffered();
			// 
			// panel1
			// 
			this.panel1.AutoScroll = true;
			this.panel1.BackColor = System.Drawing.SystemColors.Window;
			this.panel1.Controls.Add(this.flow_panel);
			this.panel1.Dock = System.Windows.Forms.DockStyle.Fill;
			this.panel1.Location = new System.Drawing.Point(136, 32);
			this.panel1.Name = "panel1";
			this.panel1.Size = new System.Drawing.Size(578, 480);
			this.panel1.TabIndex = 3;

			// 
			// flow_panel
			// 
            this.flow_panel.AllowDrop = true;
			this.flow_panel.AutoScrollMargin = new System.Drawing.Size(5, 5);
			this.flow_panel.BackColor = System.Drawing.SystemColors.Window;
			this.flow_panel.Location = new System.Drawing.Point(0, 0);
			this.flow_panel.Name = "flow_panel";
			this.flow_panel.Size = new System.Drawing.Size(512, 296);
			this.flow_panel.TabIndex = 2;
			this.flow_panel.MouseUp += new System.Windows.Forms.MouseEventHandler(this.flow_panel_MouseUp);
			this.flow_panel.Paint += new System.Windows.Forms.PaintEventHandler(this.flow_panel_Paint);
			this.flow_panel.MouseEnter += new System.EventHandler(this.flow_panel_MouseEnter);
			this.flow_panel.Leave += new System.EventHandler(this.flow_panel_Leave);
			this.flow_panel.DoubleClick += new System.EventHandler(this.flow_dbl_Click);
			this.flow_panel.MouseMove += new System.Windows.Forms.MouseEventHandler(this.Set_Hover);
			this.flow_panel.MouseLeave += new System.EventHandler(this.flow_panel_MouseLeave);
			this.flow_panel.MouseDown += new System.Windows.Forms.MouseEventHandler(this.select_flow_shape);
            this.flow_panel.DragDrop +=new DragEventHandler(carlisle_DragDrop);
            this.flow_panel.DragEnter +=new DragEventHandler(carlisle_DragEnter);
            this.flow_panel.DragOver += new DragEventHandler(carlisle_DragOver);
            
            form.GiveFeedback += new GiveFeedbackEventHandler(form_GiveFeedback);
            this.GiveFeedback += new GiveFeedbackEventHandler(carlisle_GiveFeedback);
            ((System.Windows.Forms.Control)flow_panel).KeyDown +=
				new System.Windows.Forms.KeyEventHandler(form.Visual_Flow_Form_KeyDown);
			this.Controls.Add(this.panel1);
			End = new Oval(Visual_Flow_Form.flow_height, Visual_Flow_Form.flow_width, "Oval");
			if (!Component.USMA_mode)
			{
				End.Text = "End";
			}
			else
			{
				End.Text = "Stop";
			}
        }

        protected Subchart()
        {
        }
        public void tab_disposed(object o, EventArgs e)
        {
            if (!Component.BARTPE && !Component.VM && !Component.MONO)
            {
                this.tab_overlay.Dispose();
            }
        }
		public Subchart(Visual_Flow_Form the_form, string s) : base()
		{ 
            this.initialize(the_form,s);
			Start = new Oval(End, Visual_Flow_Form.flow_height, Visual_Flow_Form.flow_width, "Oval");
			Start.Text = "Start";
			Start.scale=form.scale;
			Start.Scale(form.scale);

            if (!Component.MONO)
            {
                Initialize_Ink();
            }
            
            this.flow_panel.Invalidate();
            this.kind = Subchart_Kinds.Subchart;
		}

        protected void Initialize_Ink()
        {
            if (!Component.BARTPE && !Component.VM && !Component.compiled_flowchart &&
                !Component.MONO)
            {
                this.tab_overlay = new InkOverlay(this.flow_panel);
                this.tab_overlay.Enabled = false;
                this.tab_overlay.EditingMode = InkOverlayEditingMode.Ink;
                this.tab_overlay.CursorInRange += new InkCollectorCursorInRangeEventHandler(tab_overlay_CursorInRange);
                this.tab_overlay.Stroke += new InkCollectorStrokeEventHandler(tab_overlay_Stroke);
                this.tab_overlay.StrokesDeleted += new InkOverlayStrokesDeletedEventHandler(tab_overlay_StrokesDeleted);
                this.Disposed += new EventHandler(tab_disposed);
                System.Drawing.Drawing2D.Matrix matrix = new System.Drawing.Drawing2D.Matrix();
                System.Drawing.Drawing2D.Matrix matrix2 = new System.Drawing.Drawing2D.Matrix();
                this.tab_overlay.Renderer.GetViewTransform(ref matrix2);
                this.tab_overlay.Renderer.SetViewTransform(matrix);
                Graphics g = this.flow_panel.CreateGraphics();
                Point pt1 = new Point(50, 50);
                Point pt2 = new Point(100, 100);
                this.tab_overlay.Renderer.InkSpaceToPixel(g, ref pt1);
                this.tab_overlay.Renderer.InkSpaceToPixel(g, ref pt2);
                ink_resolution = System.Math.Abs(50.0 / (pt2.X - pt1.X));
                this.tab_overlay.Renderer.SetViewTransform(matrix2);
                g.Dispose();
            }
        }

        void tab_overlay_StrokesDeleted(object sender, EventArgs e)
        {
            form.modified = true;
        }

        void tab_overlay_Stroke(object sender, InkCollectorStrokeEventArgs e)
        {
            form.modified = true;
        }

        void tab_overlay_CursorInRange(object sender, InkCollectorCursorInRangeEventArgs e)
        {
            if (e.Cursor.Inverted && this.tab_overlay.EditingMode!=InkOverlayEditingMode.Delete)
            {
                this.tab_overlay.Enabled = false;
                this.tab_overlay.EditingMode = InkOverlayEditingMode.Delete;

                // specify stroke deleting
                this.tab_overlay.EraserMode = Microsoft.Ink.InkOverlayEraserMode.StrokeErase;

                // re-enable the overlay, we're done!
                this.tab_overlay.Enabled = true;
            }
            else
            {
                if (form.menuItemInkErase.Checked==false && form.menuItemInkSelect.Checked==false)
                {
                    this.tab_overlay.Enabled = false;
                    this.tab_overlay.EditingMode = InkOverlayEditingMode.Ink;
                    // re-enable the overlay, we're done!
                    this.tab_overlay.Enabled = true;
                }
            }

        }
        public void scale_ink(float scale)
        {
            if (!Component.BARTPE && !Component.VM && !Component.MONO)
            {
                float f;
                System.Drawing.Drawing2D.Matrix matrix = new System.Drawing.Drawing2D.Matrix();
                this.tab_overlay.Renderer.SetViewTransform(matrix);
                f = (float)((float)(Visual_Flow_Form.flow_width+5) * ink_resolution);
                matrix.Translate(f, (float)0.0);
                matrix.Scale(scale, scale);
                this.tab_overlay.Renderer.SetViewTransform(matrix);
            }
        }
		// private void flow_panel_Paint(object sender, System.Windows.Forms.PaintEventArgs e)
		// This method is called anytime the flow panel is invalidated.
		// It will traverse the object tree once to calculate the sizes
		// and x,y needed to draw the objects. Drawing the actual objects
		// occurs on the second pass through the tree.
		private void flow_panel_Paint(object sender, System.Windows.Forms.PaintEventArgs e)
		{
			int x1, y1;

			if (Start != null)
			{
				Start.footprint(e.Graphics); // calculate all sizes and needed x,y points
                Point pt1 = new Point(50, 50);
                Point pt2 = new Point(100, 100);
                // compute ink resolution
                x1 = (int)Start.FP.left + Visual_Flow_Form.flow_width;
				y1 = (int) Math.Round(form.scale*30);
				form.my_layout();
				if (Component.compiled_flowchart)
				{
					// redo X and Y b/c layout changes them
					x1 = (int) Start.FP.left+Visual_Flow_Form.flow_width;
					y1 = (int) Math.Round(form.scale*30);
                }
				Start.draw(e.Graphics,x1,y1); // traverse the tree and draw
				if (selection_rectangle.Width > 0)
				{
					e.Graphics.DrawRectangle(PensBrushes.black_dash_pen,
						selection_rectangle);
				}
			}
		}
	
		// private void select_flow_shape(object sender, System.Windows.Forms.MouseEventArgs e)
		// This method is called whenever the mouse button is pressed.  It will do
		// two things.  If a control figure is selected in the left panel, and
		// the mouse is over a valid point to insert an object, then it will insert
		// the object at that mouse x,y.  If the mouse is over the head of and object
		// then the entire object and it's children will be highlighted (colored red).
		//
		// It is also used to get the mouse position x,y whenever a double click occurs
		private void select_flow_shape(object sender, System.Windows.Forms.MouseEventArgs e)
		{
            // don't do this if inking or compiled 
            if ((!Component.BARTPE && !Component.VM && !Component.MONO && this.tab_overlay.Enabled) || 
                Component.compiled_flowchart)
            {
                form.mouse_x = e.X;
                form.mouse_y = e.Y;
                return;
            }

            this.possible_drag_drop = false;
			// if we have a left click that is an expansion click, don't go on
			if (e.Button == MouseButtons.Left && Start.check_expansion_click(e.X,e.Y))
			{
				// it is important to set mouse_x and mouse_y for double-click and
				// region selected computations later
				form.mouse_x = e.X;
				form.mouse_y = e.Y;
				this.flow_panel.Invalidate();
				return;
			}

			if (!form.runningState)
			{
				form.mouse_x = e.X;
				form.mouse_y = e.Y;

				if (e.Button == MouseButtons.Left)
				{
                    Component found_component = Start.Find_Component(form.mouse_x, form.mouse_y);

                    if (found_component != null && found_component.selected)
                    {
                        //Start_DragDrop(e);
                        this.possible_drag_drop = true;
                        return;
                    }
                    if (form.control_figure_selected < 0)
                    {
                        this.Current_Selection = Start.select(form.mouse_x, form.mouse_y);
                    }

                    if (form.control_figure_selected == Visual_Flow_Form.assignment_fig)
					{
                        Insert_Figure(new Rectangle(
                            Visual_Flow_Form.flow_height,
                            Visual_Flow_Form.flow_width, "Rectangle",
                            Rectangle.Kind_Of.Assignment));
					}
					else if (form.control_figure_selected == Visual_Flow_Form.call_fig)
					{
                        Insert_Figure(new Rectangle(Visual_Flow_Form.flow_height, 
							Visual_Flow_Form.flow_width, "Rectangle", 
							Rectangle.Kind_Of.Call));
					}
					else if (form.control_figure_selected == Visual_Flow_Form.input_fig)
					{
                        Insert_Figure(new Parallelogram(
                            Visual_Flow_Form.flow_height,
                            Visual_Flow_Form.flow_width, "Parallelogram", true));
					}
					else if (form.control_figure_selected == Visual_Flow_Form.output_fig)
					{
                        Insert_Figure(new Parallelogram(
                            Visual_Flow_Form.flow_height,
                            Visual_Flow_Form.flow_width, "Parallelogram", false));
					}
					else if (form.control_figure_selected == Visual_Flow_Form.if_control_fig)
					{
                        Insert_Figure(new IF_Control(
                            Visual_Flow_Form.flow_height,
                            Visual_Flow_Form.flow_width, "IF_Control"));
					}
					else if (form.control_figure_selected == Visual_Flow_Form.loop_fig)
					{
                        Insert_Figure(new Loop(
							Visual_Flow_Form.flow_height, 
							Visual_Flow_Form.flow_width, "Loop"));
					}
                    else if (form.control_figure_selected == Visual_Flow_Form.return_fig)
                    {
                        Insert_Figure(new Oval_Return(
                            Visual_Flow_Form.flow_height,
                            Visual_Flow_Form.flow_width, "Return"));
                    } 
                    if (found_component != null && found_component.selected)
                    {
                        Start.Scale(form.scale);
                        form.my_layout();
                        flow_panel.Invalidate();

                        //Undo_Stack.Make_Undoable(form);
                        //Start_DragDrop(e);
                        this.possible_drag_drop = true;
                        return;
                    }

					selectedComment = Start.selectComment(form.mouse_x,form.mouse_y);
					// remember where the comment was relative to the click location
					// so that we can keep updating accordingly
					if (selectedComment!=null)
					{
						selected_comment_x = (form.mouse_x-selectedComment.parent.x_location)-
							((int) (((float) selectedComment.X)*form.scale));
						selected_comment_y = (form.mouse_y-selectedComment.parent.y_location)-
							((int) (((float) selectedComment.Y)*form.scale));
					}

					Start.Scale(form.scale);
					form.my_layout();
					flow_panel.Invalidate();

					
				}
				else if (e.Button == MouseButtons.Right) 
				{
					if (!this.region_selected)
					{
						this.Current_Selection = Start.select(form.mouse_x, form.mouse_y);
						selectedComment = Start.selectComment(form.mouse_x,form.mouse_y);

						flow_panel.Invalidate();
					}
					if (this.flow_panel.Cursor==System.Windows.Forms.Cursors.Hand)
					{
						IDataObject obj = ClipboardMultiplatform.GetDataObject();
                        Component.warned_about_error = true;
						Clipboard_Data cd = (Clipboard_Data) obj.GetData(
							"raptor.Clipboard_Data");
						form.contextMenu2Paste.Enabled=(
							(cd != null && 
							 cd.kind==Clipboard_Data.kinds.symbols && 
							 this.Current_Selection==null) ||
							(cd != null &&
							 cd.kind==Clipboard_Data.kinds.comment &&
							 this.Current_Selection!=null));
						form.contextMenuInsert.Show(
							this.flow_panel,
							new System.Drawing.Point(form.mouse_x,form.mouse_y));

					}
					else 
					{
						this.Breakpoint_Selection = this.Current_Selection;
						form.contextMenu1.Show(this.flow_panel,
							new System.Drawing.Point(form.mouse_x,form.mouse_y));
					}
					
				}
			}
				// running
			else if (e.Button == MouseButtons.Right) 
			{
				form.mouse_x = e.X;
				form.mouse_y = e.Y;

				this.Breakpoint_Selection = Start.select(e.X, e.Y);
				this.Current_Selection = Start.select(-1000,-1000);
				form.contextMenu2.Show(this.flow_panel,
					new System.Drawing.Point(e.X,e.Y));
			}
		}

        private void Start_DragDrop(System.Windows.Forms.MouseEventArgs e)
        {
            Point p = this.panel1.PointToClient(new Point(e.X, e.Y));
            int local_x = p.X + Math.Abs(this.panel1.AutoScrollPosition.X);
            int local_y = p.Y + Math.Abs(this.panel1.AutoScrollPosition.Y);
            this.have_cut_drag = false;
            this.have_drag = true;
            this.drag_x = e.X;
            this.drag_y = e.Y;
            this.DoDragDrop("raptor_DRAG", DragDropEffects.Move | DragDropEffects.Link);
        }

        private void Insert_Figure(Component c)
        {
            form.Make_Undoable();
            if (Start.insert(c, form.mouse_x, form.mouse_y, 0))
            {
                this.Current_Selection = Start.select(-1000, -1000);
            }
            else
            {
                Undo_Stack.Decrement_Undoable(form);
                this.Current_Selection = Start.select(form.mouse_x, form.mouse_y);
            }
        }

		private void flow_panel_MouseLeave(object sender, System.EventArgs e)
		{
			this.scroll_location = this.panel1.AutoScrollPosition;
		}

		private void flow_panel_Leave(object sender, System.EventArgs e)
		{
			this.scroll_location = this.panel1.AutoScrollPosition;
		}

		// private void flow_dbl_Click(object sender, System.EventArgs e) 
		// This method is called whenever a double-click occurs.  If the dbl-click
		// occurs over an object head, the text dialog option will pop-up.  This
		// will allow a user to enter the text that will appear in an object.
		private void flow_dbl_Click(object sender, System.EventArgs e)
		{
            // don't do this if inking or compiled
            if ((!Component.BARTPE && !Component.VM && !Component.MONO && this.tab_overlay.Enabled) || 
                Component.compiled_flowchart) return;
			if (!form.runningState)
			{
				if (Start.setText(form.mouse_x, form.mouse_y, form))
				{
                    form.my_layout();
					flow_panel.Invalidate();
                    form.my_layout();
				}
				else if (this.selectedComment != null)
				{
					this.selectedComment.setText(form);
				}

				flow_panel.Invalidate();

			}
		}

		private void Set_Hover(object sender, System.Windows.Forms.MouseEventArgs e)
		{
            // don't do this if inking or compiled 
            if (!Component.BARTPE && !Component.VM && !Component.MONO && this.tab_overlay.Enabled && !Component.compiled_flowchart) return;

            if (!(form.full_speed && form.continuous_Run) && 
				!Component.compiled_flowchart)
			{
				form.tooltip_text = Start.getText(e.X,e.Y);
				form.toolTip1.SetToolTip(this.flow_panel,form.tooltip_text);
			}
			if (!form.runningState) 
			{
				if (Start.insert(null,e.X,e.Y,0))
				{
					this.Cursor = System.Windows.Forms.Cursors.Hand;
				}
				else
				{
					this.Cursor = System.Windows.Forms.Cursors.Default;
				}

				if (e.Button==System.Windows.Forms.MouseButtons.Left)
				{
                    Component found_component = Start.Find_Component(e.X,e.Y);

                    if (found_component != null && found_component.selected && this.possible_drag_drop)
                    {
                        this.possible_drag_drop = false;
                        Start_DragDrop(e);
                        return;
                    }

					if (selectedComment != null) 
					{
						if (e.X < 0)
						{
							this.selectedComment.X = (int) ((-this.selectedComment.parent.x_location)/form.scale);
						}
						else
						{
							this.selectedComment.X = (int) ((e.X-selected_comment_x - this.selectedComment.parent.x_location)/form.scale);
						}
						if (e.Y < 0)
						{
							this.selectedComment.Y = (int) ((-this.selectedComment.parent.y_location)/form.scale);
						}
						else
						{
							this.selectedComment.Y = (int) ((e.Y-selected_comment_y - this.selectedComment.parent.y_location)/form.scale);
						}
						this.flow_panel.Invalidate();
					}
					else
					{
						int min_x = (form.mouse_x < e.X) ? form.mouse_x : e.X;
						int min_y = (form.mouse_y < e.Y) ? form.mouse_y : e.Y;
						int width = Math.Abs(form.mouse_x-e.X);
						int height = Math.Abs(form.mouse_y-e.Y);
						if (width > 0 && height > 0)
						{
							selection_rectangle = new System.Drawing.Rectangle(
								min_x,min_y,width,height);
							this.region_selected = Start.SelectRegion(selection_rectangle);
						}
						else
						{
							selection_rectangle.Width=0;
						}
						this.flow_panel.Invalidate();
					}
				}
			}
		}

		private void flow_panel_MouseUp(object sender, System.Windows.Forms.MouseEventArgs e)
		{
            // don't do this if inking
            if (!Component.BARTPE && !Component.VM && !Component.MONO && this.tab_overlay.Enabled && !Component.compiled_flowchart) return;

            this.have_drag = false;
			if (this.selection_rectangle.Width>0)
			{
				this.selection_rectangle.Width=0;
				this.flow_panel.Invalidate();
			}
		}

		public void flow_panel_MouseEnter(object sender, System.EventArgs e)
		{
			// fix so that we don't force on top of About box, e.g.
			if (form.ContainsFocus && (!form.runningState || PromptForm.current==null))
			{
				// ok, so this is sort of odd-- it seems that
				// the call to focus moves the flow_panel back to
				// the top if if wasn't focused already 
				// (or the panel), so I have to do the Focus()
				// call after the test, but before the reset
				// of the AutoScrollPosition.
				// Recall the reason we need focus is so that
				// the tooltips appear.
				if (!this.flow_panel.Focused && !this.panel1.Focused)
				{
					this.flow_panel.Focus();
					// this feels like a .NET bug to me
					if (this.scroll_location.Y<0)
					{
						this.scroll_location.Y = -this.scroll_location.Y;
					}
					if (this.scroll_location.X<0)
					{
						this.scroll_location.X = -this.scroll_location.X;
					}
					this.panel1.AutoScrollPosition = this.scroll_location;
				}
				else
				{
					this.flow_panel.Focus();
				}
			}

		}

		public void Activated(object sender, System.EventArgs e)
		{
			// this feels like a .NET bug to me
			if (this.scroll_location.Y<0)
			{
				this.scroll_location.Y = -this.scroll_location.Y;
			}
			if (this.scroll_location.X<0)
			{
				this.scroll_location.X = -this.scroll_location.X;
			}

			this.panel1.AutoScrollPosition = this.scroll_location;
		}

		public static bool Is_Subchart_Name(string s)
		{
			return form.Is_Subchart_Name(s);
		}
		public static int Parameter_Count(string s)
		{
            Subchart chart = form.Find_Tab(s);
			return chart.num_params;
		}

        private void InitializeComponent()
        {
            this.SuspendLayout();
            this.ResumeLayout(false);

        }
        private bool droppable_icon = false;
        private bool droppable_file = false;
        private void carlisle_DragEnter(object sender, DragEventArgs e)
        {
            droppable_icon = false;
            droppable_file = false;
            if (e.Data.GetDataPresent(DataFormats.Text))
            {
                string s = (string)e.Data.GetData(DataFormats.Text);
                if (s == "raptor_ASGN" || s == "raptor_CALL" ||
                    s == "raptor_INPUT" || s == "raptor_OUTPUT" ||
                    s == "raptor_SELECTION" || s == "raptor_LOOP" ||
                    s == "raptor_RETURN")
                {
                    droppable_icon = true;
                    this.have_cut_drag = true;
                }
                if (s == "raptor_DRAG" && !this.have_drag)
                {
                    this.have_drag = true;
                }
            }
            else if (e.Data.GetDataPresent(DataFormats.FileDrop))
            {
                droppable_file = true;
            }
        }

        private void form_GiveFeedback(object sender, GiveFeedbackEventArgs e)
        {
            carlisle_GiveFeedback(sender, e);
        }

        private void carlisle_GiveFeedback(object sender, GiveFeedbackEventArgs e)
        {
            if (e.Effect == DragDropEffects.Link)
            {
                e.UseDefaultCursors = false;
                System.Windows.Forms.Cursor.Current = System.Windows.Forms.Cursors.Help;
            }
            else
            {
                e.UseDefaultCursors = true;
            }
        }
        
        private void carlisle_DragOver(object sender, DragEventArgs e)
        {
            if (droppable_file)
            {
                e.Effect = DragDropEffects.Copy;
                return;
            }
            Point p = this.panel1.PointToClient(new Point(e.X, e.Y));
            int local_x = p.X + Math.Abs(this.panel1.AutoScrollPosition.X);
            int local_y = p.Y + Math.Abs(this.panel1.AutoScrollPosition.Y);
            //form.MC.set_text("over!" + local_x + ":" + local_y + ":" + this.drag_x + ":" + this.drag_y + "\n");
            if (droppable_icon && Start.insert(null,
                local_x,
                local_y, 0))
            {
                e.Effect = DragDropEffects.Copy | DragDropEffects.Scroll;
            }
            else if (this.have_drag)
            {
                if (!this.have_cut_drag && (
                    Math.Abs(local_x-this.drag_x) > 5 || Math.Abs(local_y-this.drag_y)>5))
                //if (!this.have_cut_drag)
                {
                    //form.MC.set_text("cut:" + this.drag_x + ":" + this.drag_y + "\n");
                    form.Cut_Click(sender, e);
                    this.have_cut_drag = true;
                }
                if (Start.insert(null,
                    p.X + Math.Abs(this.panel1.AutoScrollPosition.X),
                    p.Y + Math.Abs(this.panel1.AutoScrollPosition.Y), 0))
                {
                    e.Effect = DragDropEffects.Move;
                }
                else
                {
                    // was none
                    e.Effect = DragDropEffects.Link;
                }
            }
            else
            {
                // was none
                e.Effect = DragDropEffects.Link;
            }
        }

        private void carlisle_DragDrop(object sender, DragEventArgs e)
        {
            if (e.Effect == DragDropEffects.Link && this.have_drag && this.have_cut_drag)
            {
                this.have_drag = false;
                this.have_cut_drag = false;
                form.Undo_Click(sender, e);
                return;
            }
            if (droppable_file)
            {
                string[] name = (string []) e.Data.GetData(DataFormats.FileDrop);
                form.Load_MRU(name[0]);
            }
            //form.MC.set_text("dropped!");
            e.Effect = DragDropEffects.Copy;
            Point p = this.panel1.PointToClient(new Point(e.X, e.Y));
            if ((droppable_icon || have_drag) && Start.insert(null,
                p.X + Math.Abs(this.panel1.AutoScrollPosition.X),
                p.Y + Math.Abs(this.panel1.AutoScrollPosition.Y), 0))
            {
                form.mouse_x = p.X + Math.Abs(this.panel1.AutoScrollPosition.X);
                form.mouse_y = p.Y + Math.Abs(this.panel1.AutoScrollPosition.Y);
                string s = (string)e.Data.GetData(DataFormats.Text);
                if (s == "raptor_ASGN")
                {
                    form.menuItemAssignment_Click(sender, null);
                }
                else if (s == "raptor_CALL")
                {
                    form.menuItemCall_Click(sender, null);
                }
                else if (s == "raptor_INPUT")
                {
                    form.menuItemParallelogram_Click(sender, null);
                }
                else if (s == "raptor_OUTPUT")
                {
                    form.menuItemOutput_Click(sender, null);
                }
                else if (s == "raptor_SELECTION")
                {
                    form.menuItemIf_Click(sender, null);
                }
                else if (s == "raptor_LOOP")
                {
                    form.menuItemLoop_Click(sender, null);
                }
                else if (s == "raptor_RETURN")
                {
                    form.menuItemReturn_Click(sender, null);
                }
                else if (s == "raptor_DRAG")
                {
                    form.paste_Click(null, null);
                }

            }
            this.have_drag = false;
        }

	}
}
