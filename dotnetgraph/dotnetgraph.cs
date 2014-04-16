using System;
using System.Collections.Generic;
using System.ComponentModel;
using System.Data;
using System.Drawing;
using System.Text;
using System.Windows.Forms;
using System.Diagnostics;

namespace dotnetgraphlibrary
{
    public enum Color_Type
    {
        Black, Blue, Green, Cyan, Red, Magenta, Brown,
        Light_Gray, Dark_Gray, Light_Blue, Light_Green,
        Light_Cyan, Light_Red, Light_Magenta, Yellow, White,
        RGB_00_00_33, RGB_00_00_66, RGB_00_00_CC, RGB_00_33_00, RGB_00_33_33,
        RGB_00_33_66, RGB_00_33_99, RGB_00_33_CC, RGB_00_33_FF, RGB_00_66_00,
        RGB_00_66_33, RGB_00_66_66, RGB_00_66_99, RGB_00_66_CC, RGB_00_66_FF,
        RGB_00_99_00, RGB_00_99_33, RGB_00_99_66, RGB_00_99_CC, RGB_00_99_FF,
        RGB_00_CC_00, RGB_00_CC_33, RGB_00_CC_66, RGB_00_CC_99, RGB_00_CC_CC,
        RGB_00_CC_FF, RGB_00_FF_33, RGB_00_FF_66, RGB_00_FF_99, RGB_00_FF_CC,
        RGB_33_00_00, RGB_33_00_33, RGB_33_00_66, RGB_33_00_99, RGB_33_00_CC,
        RGB_33_00_FF, RGB_33_33_00, RGB_33_33_33, RGB_33_33_66, RGB_33_33_99,
        RGB_33_33_CC, RGB_33_33_FF, RGB_33_66_00, RGB_33_66_33, RGB_33_66_66,
        RGB_33_66_99, RGB_33_66_CC, RGB_33_66_FF, RGB_33_99_00, RGB_33_99_33,
        RGB_33_99_66, RGB_33_99_99, RGB_33_99_CC, RGB_33_99_FF, RGB_33_CC_00,
        RGB_33_CC_33, RGB_33_CC_66, RGB_33_CC_99, RGB_33_CC_CC, RGB_33_CC_FF,
        RGB_33_FF_00, RGB_33_FF_33, RGB_33_FF_66, RGB_33_FF_99, RGB_33_FF_CC,
        RGB_33_FF_FF, RGB_66_00_00, RGB_66_00_33, RGB_66_00_66, RGB_66_00_99,
        RGB_66_00_CC, RGB_66_00_FF, RGB_66_33_00, RGB_66_33_33, RGB_66_33_66,
        RGB_66_33_99, RGB_66_33_CC, RGB_66_33_FF, RGB_66_66_00, RGB_66_66_33,
        RGB_66_66_66, RGB_66_66_99, RGB_66_66_CC, RGB_66_66_FF, RGB_66_99_00,
        RGB_66_99_33, RGB_66_99_66, RGB_66_99_99, RGB_66_99_CC, RGB_66_99_FF,
        RGB_66_CC_00, RGB_66_CC_33, RGB_66_CC_66, RGB_66_CC_99, RGB_66_CC_CC,
        RGB_66_CC_FF, RGB_66_FF_00, RGB_66_FF_33, RGB_66_FF_66, RGB_66_FF_99,
        RGB_66_FF_CC, RGB_66_FF_FF, RGB_99_00_00, RGB_99_00_33, RGB_99_00_66,
        RGB_99_00_99, RGB_99_00_CC, RGB_99_00_FF, RGB_99_33_00, RGB_99_33_33,
        RGB_99_33_66, RGB_99_33_99, RGB_99_33_CC, RGB_99_33_FF, RGB_99_66_00,
        RGB_99_66_33, RGB_99_66_66, RGB_99_66_99, RGB_99_66_CC, RGB_99_66_FF,
        RGB_99_99_33, RGB_99_99_66, RGB_99_99_99, RGB_99_99_CC, RGB_99_99_FF,
        RGB_99_CC_00, RGB_99_CC_33, RGB_99_CC_66, RGB_99_CC_99, RGB_99_CC_CC,
        RGB_99_CC_FF, RGB_99_FF_00, RGB_99_FF_33, RGB_99_FF_66, RGB_99_FF_99,
        RGB_99_FF_CC, RGB_99_FF_FF, RGB_CC_00_00, RGB_CC_00_33, RGB_CC_00_66,
        RGB_CC_00_99, RGB_CC_00_CC, RGB_CC_00_FF, RGB_CC_33_00, RGB_CC_33_33,
        RGB_CC_33_66, RGB_CC_33_99, RGB_CC_33_CC, RGB_CC_33_FF, RGB_CC_66_00,
        RGB_CC_66_33, RGB_CC_66_66, RGB_CC_66_99, RGB_CC_66_CC, RGB_CC_66_FF,
        RGB_CC_99_00, RGB_CC_99_33, RGB_CC_99_66, RGB_CC_99_99, RGB_CC_99_CC,
        RGB_CC_99_FF, RGB_CC_CC_00, RGB_CC_CC_33, RGB_CC_CC_66, RGB_CC_CC_99,
        RGB_CC_CC_CC, RGB_CC_CC_FF, RGB_CC_FF_00, RGB_CC_FF_33, RGB_CC_FF_66,
        RGB_CC_FF_99, RGB_CC_FF_CC, RGB_CC_FF_FF, RGB_FF_00_33, RGB_FF_00_66,
        RGB_FF_00_CC, RGB_FF_33_00, RGB_FF_33_33, RGB_FF_33_66, RGB_FF_33_99,
        RGB_FF_33_CC, RGB_FF_33_FF, RGB_FF_66_00, RGB_FF_66_33, RGB_FF_66_66,
        RGB_FF_66_99, RGB_FF_66_CC, RGB_FF_66_FF, RGB_FF_99_33, RGB_FF_99_66,
        RGB_FF_99_99, RGB_FF_99_CC, RGB_FF_99_FF, RGB_FF_CC_00, RGB_FF_CC_33,
        RGB_FF_CC_66, RGB_FF_CC_99, RGB_FF_CC_CC, RGB_FF_CC_FF, RGB_FF_FF_33,
        RGB_FF_FF_66, RGB_FF_FF_99, RGB_FF_FF_CC, RGB_0D_0D_0D,
        RGB_14_14_14, RGB_1A_1A_1A, RGB_27_27_27,
        RGB_3B_3B_3B, RGB_42_42_42,
        RGB_55_55_55, RGB_5C_5C_5C, RGB_69_69_69, RGB_76_76_76,
        RGB_7C_7C_7C, RGB_83_83_83, RGB_8A_8A_8A,
        RGB_A4_A4_A4, RGB_B1_B1_B1, RGB_B7_B7_B7, RGB_BE_BE_BE,
        RGB_C5_C5_C5, RGB_D2_D2_D2, RGB_DA_DA_DA,
        RGB_E5_E5_E5, RGB_EC_EC_EC, RGB_F5_F5_F5
    };

    public enum Mouse_Button { Left_Button, Right_Button };
    public enum Event_Type { None, Moved, Left_Up, Left_Down, Right_Up, Right_Down };

    public partial class dotnetgraph : Form
    {
        // I originally had the idea of making the BMP bigger than the screen,
        // but then flood fill goes off the visible window and does weird stuff
        public const int slop = 0;
        public static dotnetgraph form;
        private static Brush[] brushes;
        private static Pen[] pens;
        private static Font[] fonts;
        private static Color[] colors;
        public static bool start_topmost = false;
        private const int default_font_size = 10;
        private int current_font_size = default_font_size;
        private bool left_is_down = false;
        private bool right_is_down = false;
        private bool[] key_is_down = new bool[255];
        private char pressed_key;
        private Point mouse;
        private int click_x, click_y;
        private int left_click_x, left_click_y;
        private int right_click_x, right_click_y;
        private int x_size, y_size;

        private System.Threading.ManualResetEvent key_available_event, left_pressed_event, right_pressed_event, left_released_event,
            right_released_event, mouse_event_event;

        private System.Collections.ArrayList bitmaps = new System.Collections.ArrayList();
        static uint[] standard_color =  {
           (  0xFF000000),    /* Black         */
           (  0xFF000080),    /* Blue          */
           (  0xFF008000),    /* Green         */
           (  0xFF008080),    /* Cyan          */
           (0xFF800000),    /* Red           */
           (0xFF800080),    /* Magenta       */
           (0xFF808000),    /* Brown         */
           (0xFFACACAC),    /* Light_Gray    */
           (0xFF4F4F4F),    /* Dark_Gray     */
           (0xFF0000FF),    /* Light_Blue    */
           (0xFF00FF00),    /* Light_Green   */
           (0xFF00FFFF),    /* Light_Cyan    */
           (0xFFFF0000),    /* Light_Red     */
           (0xFFFF00FF),    /* Light_Magenta */
           (0xFFFFFF00),    /* Yellow        */
           (0xFFFFFFFF),     /* White         */
           (0xFF000033),
           (0xFF000066),
           (0xFF0000CC),
           (0xFF003300),
           (0xFF003333),
           (0xFF003366),
           (0xFF003399),
           (0xFF0033CC),
           (0xFF0033FF),
           (0xFF006600),
           (0xFF006633),
           (0xFF006666),
           (0xFF006699),
           (0xFF0066CC),
           (0xFF0066FF),
           (0xFF009900),
           (0xFF009933),
           (0xFF009966),
           (0xFF0099CC),
           (0xFF0099FF),
           (0xFF00CC00),
           (0xFF00CC33),
           (0xFF00CC66),
           (0xFF00CC99),
           (0xFF00CCCC),
           (0xFF00CCFF),
           (0xFF00FF33),
           (0xFF00FF66),
           (0xFF00FF99),
           (0xFF00FFCC),
           (0xFF330000),
           (0xFF330033),
           (0xFF330066),
           (0xFF330099),
           (0xFF3300CC),
           (0xFF3300FF),
           (0xFF333300),
           (0xFF333333),
           (0xFF333366),
           (0xFF333399),
           (0xFF3333CC),
           (0xFF3333FF),
           (0xFF336600),
           (0xFF336633),
           (0xFF336666),
           (0xFF336699),
           (0xFF3366CC),
           (0xFF3366FF),
           (0xFF339900),
           (0xFF339933),
           (0xFF339966),
           (0xFF339999),
           (0xFF3399CC),
           (0xFF3399FF),
           (0xFF33CC00),
           (0xFF33CC33),
           (0xFF33CC66),
           (0xFF33CC99),
           (0xFF33CCCC),
           (0xFF33CCFF),
           (0xFF33FF00),
           (0xFF33FF33),
           (0xFF33FF66),
           (0xFF33FF99),
           (0xFF33FFCC),
           (0xFF33FFFF),
           (0xFF660000),
           (0xFF660033),
           (0xFF660066),
           (0xFF660099),
           (0xFF6600CC),
           (0xFF6600FF),
           (0xFF663300),
           (0xFF663333),
           (0xFF663366),
           (0xFF663399),
           (0xFF6633CC),
           (0xFF6633FF),
           (0xFF666600),
           (0xFF666633),
           (0xFF666666),
           (0xFF666699),
           (0xFF6666CC),
           (0xFF6666FF),
           (0xFF669900),
           (0xFF669933),
           (0xFF669966),
           (0xFF669999),
           (0xFF6699CC),
           (0xFF6699FF),
           (0xFF66CC00),
           (0xFF66CC33),
           (0xFF66CC66),
           (0xFF66CC99),
           (0xFF66CCCC),
           (0xFF66CCFF),
           (0xFF66FF00),
           (0xFF66FF33),
           (0xFF66FF66),
           (0xFF66FF99),
           (0xFF66FFCC),
           (0xFF66FFFF),
           (0xFF990000),
           (0xFF990033),
           (0xFF990066),
           (0xFF990099),
           (0xFF9900CC),
           (0xFF9900FF),
           (0xFF993300),
           (0xFF993333),
           (0xFF993366),
           (0xFF993399),
           (0xFF9933CC),
           (0xFF9933FF),
           (0xFF996600),
           (0xFF996633),
           (0xFF996666),
           (0xFF996699),
           (0xFF9966CC),
           (0xFF9966FF),
           (0xFF999933),
           (0xFF999966),
           (0xFF999999),
           (0xFF9999CC),
           (0xFF9999FF),
           (0xFF99CC00),
           (0xFF99CC33),
           (0xFF99CC66),
           (0xFF99CC99),
           (0xFF99CCCC),
           (0xFF99CCFF),
           (0xFF99FF00),
           (0xFF99FF33),
           (0xFF99FF66),
           (0xFF99FF99),
           (0xFF99FFCC),
           (0xFF99FFFF),
           (0xFFCC0000),
           (0xFFCC0033),
           (0xFFCC0066),
           (0xFFCC0099),
           (0xFFCC00CC),
           (0xFFCC00FF),
           (0xFFCC3300),
           (0xFFCC3333),
           (0xFFCC3366),
           (0xFFCC3399),
           (0xFFCC33CC),
           (0xFFCC33FF),
           (0xFFCC6600),
           (0xFFCC6633),
           (0xFFCC6666),
           (0xFFCC6699),
           (0xFFCC66CC),
           (0xFFCC66FF),
           (0xFFCC9900),
           (0xFFCC9933),
           (0xFFCC9966),
           (0xFFCC9999),
           (0xFFCC99CC),
           (0xFFCC99FF),
           (0xFFCCCC00),
           (0xFFCCCC33),
           (0xFFCCCC66),
           (0xFFCCCC99),
           (0xFFCCCCCC),
           (0xFFCCCCFF),
           (0xFFCCFF00),
           (0xFFCCFF33),
           (0xFFCCFF66),
           (0xFFCCFF99),
           (0xFFCCFFCC),
           (0xFFCCFFFF),
           (0xFFFF0033),
           (0xFFFF0066),
           (0xFFFF00CC),
           (0xFFFF3300),
           (0xFFFF3333),
           (0xFFFF3366),
           (0xFFFF3399),
           (0xFFFF33CC),
           (0xFFFF33FF),
           (0xFFFF6600),
           (0xFFFF6633),
           (0xFFFF6666),
           (0xFFFF6699),
           (0xFFFF66CC),
           (0xFFFF66FF),
           (0xFFFF9933),
           (0xFFFF9966),
           (0xFFFF9999),
           (0xFFFF99CC),
           (0xFFFF99FF),
           (0xFFFFCC00),
           (0xFFFFCC33),
           (0xFFFFCC66),
           (0xFFFFCC99),
           (0xFFFFCCCC),
           (0xFFFFCCFF),
           (0xFFFFFF33),
           (0xFFFFFF66),
           (0xFFFFFF99),
           (0xFFFFFFCC),
           (0xFF0D0D0D),
           (0xFF141414),
           (0xFF1A1A1A),
           (0xFF272727),
           (0xFF3B3B3B),
           (0xFF424242),
           (0xFF555555),
           (0xFF5C5C5C),
           (0xFF696969),
           (0xFF767676),
           (0xFF7C7C7C),
           (0xFF838383),
           (0xFF8A8A8A),
           (0xFFA4A4A4),
           (0xFFB1B1B1),
           (0xFFB7B7B7),
           (0xFFBEBEBE),
           (0xFFC5C5C5),
           (0xFFD2D2D2),
           (0xFFDADADA),
           (0xFFE5E5E5),
           (0xFFECECEC),
           (0xFFF5F5F5)
        };
        static int MAX_COLORS = standard_color.Length;
        private static Form uc_private;
        private static object[] sync_args = new object[2];
        private bool frozen = false;
        private Rectangle draw_rect;
        private static System.Threading.Thread isolated_thread;
        private static void runthread()
        {
            uc_private = new Form();
            uc_private.Visible = false;
            uc_private.ShowDialog();
        }
        private static Form uc
        {
            get
            {
                if (uc_private != null)
                {
                    return uc_private;
                }
                else
                {
                    isolated_thread = new System.Threading.Thread(
                        new System.Threading.ThreadStart(runthread));
                    isolated_thread.Start();
                    while (uc_private == null)
                    {
                        System.Threading.Thread.Sleep(10);
                    }
                    return uc_private;
                }
            }
        }
        public void UpdateWindowUnlessFrozen()
        {
            // mcc: received bug report on 1/22 of window not updating correctly
            // appears to be on the border of the update, so I added 1 to each
            // side
            if (!this.frozen)
            {
                this.Invalidate(new Rectangle(
                    draw_rect.Left-dotnetgraph.slop-1,
                    draw_rect.Y-dotnetgraph.slop-1,
                    draw_rect.Width+2,draw_rect.Height+2));
            }
        }
        public delegate void Clear_Window_Delegate_Type(Color_Type color);
        private static Clear_Window_Delegate_Type Clear_Window_Delegate = new Clear_Window_Delegate_Type(Clear_Window_Static);

        public static void Init(Form f)
        {
            uc_private = f;
        }
        // clear the window to the given color
        public static void Clear_Window(Color_Type color)
        {
            Check_Window_Open("Clear_Window");
            //lock (form.gBmp)
            //{
            //    Clear_Window_Static(color);
            //}
            if (form.frozen)
            {
                    Clear_Window_Static(color);
            }
            else
            {
                uc.Invoke(Clear_Window_Delegate, new object[] { color });
            }
        }
        public delegate int int_Delegate_Type();
        
        private static int_Delegate_Type Get_Window_Width_Delegate = new int_Delegate_Type(
            Get_Window_Width_Static);
        public static int Get_Window_Width()
        {
            Check_Window_Open("Get_Window_Width"); 
            return (int)uc.Invoke(Get_Window_Width_Delegate);
        }
        public delegate void Draw_Bitmap_Delegate_Type(int bitmap, int x, int y, int width, int height);
        private static Draw_Bitmap_Delegate_Type Draw_Bitmap_Delegate = new Draw_Bitmap_Delegate_Type(
            Draw_Bitmap_Static);
        public static void Draw_Bitmap(int bitmap, int x, int y, int width, int height)
        {
            Check_Window_Open("Draw_Bitmap");
            if (form.frozen)
            {
                Draw_Bitmap_Static(bitmap, x, y, width, height);
            }
            else
            {
                uc.Invoke(Draw_Bitmap_Delegate, new object[] { bitmap, x, y, width, height });
            }
        }

        public delegate void Display_Text_Delegate_Type(int x,
            int y,
            string text,
            Color_Type hue);
        private static Display_Text_Delegate_Type Display_Text_Delegate = new Display_Text_Delegate_Type(
            Display_Text_Static);
        public static void Display_Text(int x,
            int y,
            string text,
            Color_Type hue)
        {
            Check_Window_Open("Display_Text");
            if (form.frozen)
            {
                Display_Text_Static(x, y, text, hue);
            }
            else
            {
                uc.Invoke(Display_Text_Delegate, new object[] { x, y, text, hue });
            }
        }
        public delegate void Draw_Arc_Delegate_Type(
            int x1,
            int y1,
            int x2,
            int y2,
            int startx,
            int starty,
            int endx,
            int endy,
            Color_Type hue);
        private static Draw_Arc_Delegate_Type Draw_Arc_Delegate = new Draw_Arc_Delegate_Type(
            Draw_Arc_Static);
        public static void Draw_Arc(
            int x1,
            int y1,
            int x2,
            int y2,
            int startx,
            int starty,
            int endx,
            int endy,
            Color_Type hue)
        {
            Check_Window_Open("Draw_Arc");
            if (form.frozen)
            {
                Draw_Arc_Static(x1, y1, x2, y2, startx, starty, endx, endy, hue);
            }
            else
            {
                uc.Invoke(Draw_Arc_Delegate, new object[] { x1, y1, x2, y2, startx, starty, endx, endy, hue });
            }
        }
        public delegate void Draw_Box_Delegate_Type(
            int x1,
            int y1,
            int x2,
            int y2,
            Color_Type hue,
            bool filled);
        private static Draw_Box_Delegate_Type Draw_Box_Delegate = new Draw_Box_Delegate_Type(
            Draw_Box_Static);
        public static void Draw_Box(
            int x1,
            int y1,
            int x2,
            int y2,
            Color_Type hue,
            bool filled)
        {
            Check_Window_Open("Draw_Box");
            if (form.frozen)
            {
                Draw_Box_Static(x1, y1, x2, y2, hue, filled);
            }
            else
            {
                uc.Invoke(Draw_Box_Delegate, new object[] { x1, y1, x2, y2, hue, filled });
            }
        }
        public delegate void Draw_Circle_Delegate_Type(
            int x,
            int y,
            int radius,
            Color_Type hue,
            bool filled);
        private static Draw_Circle_Delegate_Type Draw_Circle_Delegate = new Draw_Circle_Delegate_Type(
            Draw_Circle_Static);
        public static void Draw_Circle(
            int x,
            int y,
            int radius,
            Color_Type hue,
            bool filled)
        {
            Check_Window_Open("Draw_Circle");
            //lock (form.gBmp)
            if (form.frozen)
            {
                Draw_Circle_Static(x, y, radius, hue, filled);
            }
            else {
                uc.Invoke(Draw_Circle_Delegate, new object[] { x, y, radius, hue, filled });
            }
        }	
        public delegate void Draw_Ellipse_Delegate_Type(
            int x1,
            int y1,
            int x2,
            int y2,
            Color_Type hue,
            bool filled);
        private static Draw_Ellipse_Delegate_Type Draw_Ellipse_Delegate = new Draw_Ellipse_Delegate_Type(
            Draw_Ellipse_Static);
        public static void Draw_Ellipse(
            int x1,
            int y1,
            int x2,
            int y2,
            Color_Type hue,
            bool filled)
        {
            Check_Window_Open("Draw_Ellipse"); 
            uc.Invoke(Draw_Ellipse_Delegate, new object[] { x1, y1, x2, y2, hue, filled });
        }	
        public delegate void Draw_Ellipse_Rotate_Delegate_Type(
            int x1,
            int y1,
            int x2,
            int y2,
            double angle,
            Color_Type hue,
            bool filled);
        private static Draw_Ellipse_Rotate_Delegate_Type Draw_Ellipse_Rotate_Delegate = new Draw_Ellipse_Rotate_Delegate_Type(
            Draw_Ellipse_Rotate_Static);
        public static void Draw_Ellipse_Rotate(
            int x1,
            int y1,
            int x2,
            int y2,
            double angle,
            Color_Type hue,
            bool filled)
        {
            Check_Window_Open("Draw_Ellipse_Rotate"); 
            uc.Invoke(Draw_Ellipse_Rotate_Delegate, new object[] { x1, y1, x2, y2, angle, hue, filled });
        }
        public delegate void Draw_Line_Delegate_Type(
            int x1,
            int y1,
            int x2,
            int y2,
            Color_Type hue);
        private static Draw_Line_Delegate_Type Draw_Line_Delegate = new Draw_Line_Delegate_Type(
            Draw_Line_Static);
        public static void Draw_Line(
            int x1,
            int y1,
            int x2,
            int y2,
            Color_Type hue)
        {
            Check_Window_Open("Draw_Line"); 
            uc.Invoke(Draw_Line_Delegate, new object[] { x1, y1, x2, y2, hue });
        }
        public delegate void Flood_Fill_Delegate_Type(
            int x,
            int y,
            Color_Type hue);
        private static Flood_Fill_Delegate_Type Flood_Fill_Delegate = new Flood_Fill_Delegate_Type(
            Flood_Fill_Static);
        public static void Flood_Fill(
            int x,
            int y,
            Color_Type hue)
        {
            Check_Window_Open("Flood_Fill");

            if (form.frozen)
            {
                Flood_Fill_Static(x, y, hue);
            }
            else
            {
                uc.Invoke(Flood_Fill_Delegate, new object[] { x, y, hue });
            }
        }	
/*        private static int_Delegate_Type Get_Key_Int_Delegate = new int_Delegate_Type(
            Get_Key_Int_Static);
        public static int Get_Key_Int()
        {
            return (int)uc.Invoke(Get_Key_Int_Delegate);
        }
        public delegate void Get_Mouse_Location_Delegate_Type(
            ref int x,
            ref int y);
        private static Get_Mouse_Location_Delegate_Type Get_Mouse_Location_Delegate = new Get_Mouse_Location_Delegate_Type(
            Get_Mouse_Location_Static);
        public static void Get_Mouse_Location(
            ref int x,
            ref int y)
        {
            uc.Invoke(Get_Mouse_Location_Delegate, new object[] { x, y });
            x = (int)sync_args[0];
            y = (int)sync_args[1];
        }	*/
        public delegate void Get_Mouse_Button_Delegate_Type(
            Mouse_Button which_button,
            ref int x,
            ref int y);
        private static Get_Mouse_Button_Delegate_Type Get_Mouse_Button_Delegate = new Get_Mouse_Button_Delegate_Type(
            Get_Mouse_Button_Static);
        public static void Get_Mouse_Button(
            Mouse_Button which_button,
            ref int x,
            ref int y)
        {
            Check_Window_Open("Get_Mouse_Button");
            Get_Mouse_Button_Static(which_button, ref x, ref y);
        }
        public delegate Color_Type Get_Pixel_Delegate_Type(
            int x,
            int y);
        private static Get_Pixel_Delegate_Type Get_Pixel_Delegate = new Get_Pixel_Delegate_Type(
            Get_Pixel_Static);
        public static Color_Type Get_Pixel(
            int x,
            int y)
        {
            Check_Window_Open("Get_Pixel");
            if (form.frozen)
            {
                return Get_Pixel_Static(x, y);
            }
            else
            {
                return (Color_Type)uc.Invoke(Get_Pixel_Delegate, new object[] { x, y });
            }
        }
        public delegate bool bool_Delegate_Type();

        public static bool Key_Hit()
        {
            Check_Window_Open("Key_Hit");
            return form.key_available_event.WaitOne(10, false);
        }
        public delegate bool mouse_bool_Delegate_Type(Mouse_Button which_button);

        public delegate bool int_bool_Delegate_Type(int key);
        private static int_bool_Delegate_Type Key_Down_Delegate = new int_bool_Delegate_Type(
            Key_Down_Static);
        public static bool Key_Down(
            int key)
        {
            Check_Window_Open("Key_Down"); 
            return (bool)uc.Invoke(Key_Down_Delegate, new object[] { key });
        }
        //private static mouse_bool_Delegate_Type Mouse_Button_Pressed_Delegate = new mouse_bool_Delegate_Type(
        //    Mouse_Button_Pressed_Static);
        public bool Mouse_Button_Pressed_Object(
            Mouse_Button which_button)
        {
            if (which_button == Mouse_Button.Left_Button)
            {
                return left_pressed_event.WaitOne(1, false);
            }
            else
            {
                return right_pressed_event.WaitOne(1, false);
            }
        }
        public static bool Mouse_Button_Pressed(
            Mouse_Button which_button)
        {
            Check_Window_Open("Mouse_Button_Pressed");
            return form.Mouse_Button_Pressed_Object(which_button);
        }
        public bool Mouse_Button_Released_Object(
            Mouse_Button which_button)
        {
            if (which_button == Mouse_Button.Left_Button)
            {
                return left_released_event.WaitOne(1, false);
            }
            else
            {
                return right_released_event.WaitOne(1, false);
            }
        }
        public static bool Mouse_Button_Released(
            Mouse_Button which_button)
        {
            Check_Window_Open("Mouse_Button_Released");
            return form.Mouse_Button_Released_Object(which_button);
        }

        public delegate void Open_Graph_Window_Delegate_Type(
            int x,
            int y);
        private static Open_Graph_Window_Delegate_Type Open_Graph_Window_Delegate = new Open_Graph_Window_Delegate_Type(
            Open_Graph_Window_Static);
        public static void Open_Graph_Window(
            int x_size,
            int y_size)
        {
            if (Is_Open())
            {
                throw new Exception("Error: Graphics window already open");
            }
            uc.Invoke(Open_Graph_Window_Delegate, new object[] { x_size, y_size });
        }
        public delegate void Put_Pixel_Delegate_Type(
            int x,
            int y,
            Color_Type hue);
        private static Put_Pixel_Delegate_Type Put_Pixel_Delegate = new Put_Pixel_Delegate_Type(
            Put_Pixel_Static);
        public static void Put_Pixel(
            int x,
            int y,
            Color_Type hue)
        {
            Check_Window_Open("Put_Pixel");
            if (form.frozen)
            {
                Put_Pixel_Static(x, y, hue);
            }
            else
            {
                uc.Invoke(Put_Pixel_Delegate, new object[] { x, y, hue });
            }
        }
        public delegate void Set_Window_Title_Delegate_Type(
            string title);
        private static Set_Window_Title_Delegate_Type Set_Window_Title_Delegate = new Set_Window_Title_Delegate_Type(
            Set_Window_Title_Static);
        public static void Set_Window_Title(
            string title)
        {
            Check_Window_Open("Set_Window_Title");
            uc.Invoke(Set_Window_Title_Delegate, new object[] { title });
        }
        public delegate void Save_Bitmap_Delegate_Type(
            string filename);
        private static Save_Bitmap_Delegate_Type Save_Bitmap_Delegate = new Save_Bitmap_Delegate_Type(
            Save_Bitmap_Static);
        public static void Save_Bitmap(
            string filename)
        {
            Check_Window_Open("Save_Bitmap");
            uc.Invoke(Save_Bitmap_Delegate, new object[] { filename });
        }
        public delegate void Wait_For_Mouse_Button_Delegate_Type(
            Mouse_Button which_button);
        private static Wait_For_Mouse_Button_Delegate_Type Wait_For_Mouse_Button_Delegate = new Wait_For_Mouse_Button_Delegate_Type(
            Wait_For_Mouse_Button_Static);
        public void Wait_For_Mouse_Button_Object(
            Mouse_Button which_button)
        {
            if (which_button == Mouse_Button.Left_Button)
            {
                left_released_event.WaitOne();
                left_released_event.Reset();
                left_pressed_event.Reset();
                mouse.X = left_click_x;
                mouse.Y = left_click_y;
            }
            else
            {
                right_released_event.WaitOne();
                right_released_event.Reset();
                right_pressed_event.Reset();
                mouse.X = right_click_x;
                mouse.Y = right_click_y;
            }
        }
        public static void Wait_For_Mouse_Button(
            Mouse_Button which_button)
        {
            Check_Window_Open("Wait_For_Mouse_Button");
            form.Wait_For_Mouse_Button_Object(which_button);
        }
        private static int_Delegate_Type Get_Font_Width_Delegate = new int_Delegate_Type(
            Get_Font_Width_Static);
        public static int Get_Font_Width()
        {
            Check_Window_Open("Get_Font_Width");
            return (int)uc.Invoke(Get_Font_Width_Delegate);
        }
        private static int_Delegate_Type Get_Font_Height_Delegate = new int_Delegate_Type(
            Get_Font_Height_Static);
        public static int Get_Font_Height()
        {
            Check_Window_Open("Get_Font_Height");
            return (int)uc.Invoke(Get_Font_Height_Delegate);
        }
        public delegate void void_Delegate_Type();
        public delegate void Close_Delegate_Type(bool force);
        private static Close_Delegate_Type Close_Graph_Window_Delegate = new Close_Delegate_Type(
            Close_Graph_Window_Static);
        public static void Close_Graph_Window()
        {
            uc.Invoke(Close_Graph_Window_Delegate, new object[] { false });
        }
        public static void Close_Graph_Window_Force()
        {
            uc.Invoke(Close_Graph_Window_Delegate, new object[] { true });
        }
        public delegate void Get_Max_Size_Delegate_Type(
            ref int width, ref int height);
        private static Get_Max_Size_Delegate_Type Get_Max_Size_Delegate = new Get_Max_Size_Delegate_Type(
            Get_Max_Size_Static);
        public static void Get_Max_Size(
            ref int width, ref int height)
        {
            uc.Invoke(Get_Max_Size_Delegate, new object[] { width, height });
            width = (int)sync_args[0];
            height = (int)sync_args[1];
        }
        private static void_Delegate_Type MakeTopMost_Delegate = new void_Delegate_Type(
            MakeTopMost_Static);
        public static void MakeTopMost()
        {
            if (uc_private != null)
            {
                uc.Invoke(MakeTopMost_Delegate);
            }
            else
            {
                start_topmost = true;
            }
        }
        private static void_Delegate_Type MakeNonTopMost_Delegate = new void_Delegate_Type(
            MakeNonTopMost_Static);
        public static void MakeNonTopMost()
        {
            if (uc_private != null)
            {
                uc.Invoke(MakeNonTopMost_Delegate);
            }
            else
            {
                start_topmost = false;
            }
        }
        private static void_Delegate_Type UpdateGraphWindow_Delegate = new void_Delegate_Type(
            UpdateGraphWindow_Static);
        public static void UpdateGraphWindow()
        {
            Check_Window_Open("UpdateGraphWindow");
            uc.Invoke(UpdateGraphWindow_Delegate);
        }
        private static void_Delegate_Type FreezeGraphWindow_Delegate = new void_Delegate_Type(
            FreezeGraphWindow_Static);
        public static void FreezeGraphWindow()
        {
            Check_Window_Open("FreezeGraphWindow");
            uc.Invoke(FreezeGraphWindow_Delegate);
        }
        private static void_Delegate_Type UnfreezeGraphWindow_Delegate = new void_Delegate_Type(
            UnfreezeGraphWindow_Static);
        public static void UnfreezeGraphWindow()
        {
            Check_Window_Open("UnfreezeGraphWindow");
            uc.Invoke(UnfreezeGraphWindow_Delegate);
        }
        public delegate void SetFontSize_Delegate_Type(
            int size);
        private static SetFontSize_Delegate_Type SetFontSize_Delegate = new SetFontSize_Delegate_Type(
            SetFontSize_Static);
        public static void SetFontSize(
            int size)
        {
            Check_Window_Open("SetFontSize");
            uc.Invoke(SetFontSize_Delegate, new object[] { size });
        }
        public static int RED(uint color)
        {
            return (int)((color >> 16) & 0xFF);
        }
        public static int GREEN(uint color)
        {
            return (int)((color >> 8) & 0xFF);
        }
        public static int BLUE(uint color)
        {
            return (int)(color & 0xFF);
        }
        public void Color_To_RGB(int color,
            ref int red,
            ref int green,
            ref int blue)
        {
            if (color >= 0 && color < MAX_COLORS)
            {
                red = (RED(standard_color[color]));
                green = (GREEN(standard_color[color]));
                blue = (BLUE(standard_color[color]));

            }
            else
            {
                throw new Exception("invalid color: " + color);
            }
        }
        private Bitmap bmp, bmp_swap;
        private Graphics gBmp, gBmp_swap;
        private void copy_buffers()
        {
            gBmp_swap.DrawImage(bmp, 0,0,bmp.Width,bmp.Height);
        }
        private void swap_buffers()
        {
            Graphics temp_g;
            Bitmap temp_b;
            lock (bmp_swap) {
                gBmp_swap.DrawImage(bmp, 0, 0, bmp.Width, bmp.Height);
                /*temp_g = gBmp;
                gBmp = gBmp_swap;
                gBmp_swap = temp_g;

                temp_b = bmp;
                bmp = bmp_swap;
                bmp_swap = temp_b;*/
            }
        }
        private static Random random = new Random();
 
        //--------------------------------------------------------------------
        //--
        //-- The Make_0_Based function converts an x coordinate from
        //-- 1-based to 0-based referencing.
        //--
        //--------------------------------------------------------------------

        private int Make_0_Based(
            int Coordinate)
        {
            return Coordinate - 1 + dotnetgraph.slop;
        }

        //--------------------------------------------------------------------
        //--
        //-- The Make_0_Based_And_Flip function converts a y coordinate from
        //-- 1-based to 0-based referencing and unflips the orientation.
        //--
        //--------------------------------------------------------------------

        private int Make_0_Based_And_Unflip(
            int Coordinate)
        {
            return Get_Window_Height() - Coordinate + dotnetgraph.slop;
        }

        //--------------------------------------------------------------------
        //--
        //-- The Make_1_Based function converts an x coordinate from
        //-- 0-based to 1-based referencing.
        //--
        //--------------------------------------------------------------------

        private int Make_1_Based(
            int Coordinate)
        {
            return Coordinate + 1;
        }

        //--------------------------------------------------------------------
        //--
        //-- The Make_1_Based_And_Flip function converts a y coordinate from
        //-- 0-based to 1-based referencing and flips the orientation.
        //--
        //--------------------------------------------------------------------

        private int Make_1_Based_And_Flip(
            int Coordinate)
        {
            try
            {
                return Get_Window_Height() + 1 - Coordinate;
            }
            catch (Exception error)
            {
                throw new Exception("ERROR!: Graph Window not open");
            }
        }


        // return a random basic color
        public static Color_Type Random_Color()
        {
            return (Color_Type)((int)(random.NextDouble() * 16));
        }
        // return a random extended color
        public static Color_Type Random_Extended_Color()
        {
            return (Color_Type)((int)(random.NextDouble() *
                (((int)Color_Type.RGB_F5_F5_F5) + 1)));
        }

        private static void Swap(ref int x, ref int y) 
		{
			int z;
			z = y;
			y = x;
			x = z;
		}

        //--------------------------------------------------------------------
        //--
        //-- The Crop_Coordinates procedure crops the end point coordinates
        //-- to move them to the edge of the window, only if one of them = -1 AND 
        //-- only if appropriate (object not refside window)
        //-- We need to pass the line boolean parameter in because, for
        //-- lines we want to slide a -1 coordinate to 0, but for boxes
        //-- we want to slide a -1 coordinate to -2 (so that line of the
        //-- box doesn't get displayed)
        //-- Algorithm :
        //--    If Y1 is above the top of the window
        //--       Adjust X1 and Y1 if Y2 not above top of window and 
        //--       X1 and X2 not both to left of window
        //--    If Y2 is above the top of the window
        //--       Adjust X2 and Y2 if Y1 not above top of window and 
        //--       X1 and X2 not both to left of window
        //--    If X1 is to the left of the window
        //--       Adjust X1 and Y1 if X2 not to the left of the window and 
        //--       Y1 and Y2 not both above top of window
        //--    If X2 is to the left of the window
        //--       Adjust X2 and Y2 if X1 not to the left of the window and 
        //--       Y1 and Y2 not both above top of window
        //--
        //--------------------------------------------------------------------

        private void Crop_Coordinates(
            bool Line,
            float Slope,
            bool Undefined_Slope,
            ref int X1,
            ref int Y1,
            ref int X2,
            ref int Y2)
        {

            //--    If Y1 is above the top of the window
            if (Y1 == -1)
            {
                //--       Adjust X1 and Y1 if Y2 not above top of window and 
                //--       X1 and X2 not both to left of window
                if ((Y2 >= 0) && !((X1 < 0) && (X2 < 0)))
                {
                    if (!Undefined_Slope)
                    {
                        X1 = X1 + (int)(((float)(0 - Y1)) * Slope);
                    }
                    if (Line)
                    {
                        Y1 = 0;
                    }
                    else
                    {
                        Y1 = -2;
                    }

                }
                //-- set Y1 to -2, since the object shouldn't appear anyway
                else
                {
                    Y1 = -2;
                }
            }

            //--    If Y2 is above the top of the window
            if (Y2 == -1)
            {
                //--       Adjust X2 and Y2 if Y1 not above top of window and 
                //--       X1 and X2 not both to left of window
                if ((Y1 >= 0) && !((X1 < 0) && (X2 < 0)))
                {
                    if (!Undefined_Slope)
                    {
                        X2 = X2 + (int)(((float)(0 - Y2)) * Slope);
                    }
                    if (Line)
                    {
                        Y2 = 0;
                    }
                    else
                    {
                        Y2 = -2;
                    }

                }	//-- set Y2 to -2, since the object shouldn't appear anyway
                else
                {
                    Y2 = -2;
                }
            }


            //--    If X1 is to the left of the window
            if (X1 == -1)
            {
                //--       Adjust X1 and Y1 if X2 not to the left of the window and 
                //--       Y1 and Y2 not both above top of window
                if ((X2 >= 0) && !((Y1 < 0) && (Y2 < 0)))
                {
                    if (!Undefined_Slope)
                    {
                        Y1 = Y1 + (int)(((float)(0 - X1)) * Slope);
                    }

                    if (Line)
                    {
                        X1 = 0;
                    }
                    else
                    {
                        X1 = -2;
                    }

                }
                //-- set X1 to -2, since the object shouldn't appear anyway
                else
                {

                    X1 = -2;
                }
            }
            //--    If X2 is to the left of the window
            if (X2 == -1)
            {
                //--       Adjust X2 and Y2 if X1 not to the left of the window and 
                //--       Y1 and Y2 not both above top of window
                if ((X1 >= 0) && !((Y1 < 0) && (Y2 < 0)))
                {
                    if (!Undefined_Slope)
                    {
                        Y2 = Y2 + (int)(((float)(0 - X2)) * Slope);
                    }
                    if (Line)
                    {
                        X2 = 0;
                    }
                    else
                    {
                        X2 = -2;
                    }
                }
                //-- set X2 to -2, since the object shouldn't appear anyway
                else
                {
                    X2 = -2;
                }
            }
        }
        //-------------------------------------------------------------
        //-- Draw Line draws a line from (X1,Y1) to (X2,Y2) in the
        //-- given color
        //-------------------------------------------------------------
        public void Draw_Line_Object(
            int x1,
            int y1,
            int x2,
            int y2,
            Color_Type hue)
        {
            int x_coord1, y_coord1;
            x_coord1 = Make_0_Based(x1);
            y_coord1 = Make_0_Based_And_Unflip(y1);
            int x_coord2, y_coord2;
            x_coord2 = Make_0_Based(x2);
            y_coord2 = Make_0_Based_And_Unflip(y2);
            bool Undefined_Slope;
            float Slope;

            //-- Calculate slope of the line
            if ((x_coord2 - x_coord1) != 0)
            {
                Undefined_Slope = false;
                Slope = ((float)(y_coord2 - y_coord1)) / ((float)(x_coord2 -
                       x_coord1));
            }
            //-- undefined slope
            else
            {
                Undefined_Slope = true;
                Slope = 0.0F;
            }
            //-- Adjust invalid (-1) coordinates to crop the line 
            //-- to fit in the window
            Crop_Coordinates(
                true,
                Slope,
                Undefined_Slope,
                ref x_coord1,
                ref y_coord1,
                ref x_coord2,
                ref y_coord2);
            if (x_coord2 >= x_coord1)
            {
                draw_rect.X = (x_coord1 > 0) ? x_coord1 : 0;
                draw_rect.Width = ((x_coord2 < x_size) ? x_coord2 + 1 : x_size)
                    - draw_rect.X;
            }
            else
            {
                draw_rect.X = (x_coord2 > 0) ? x_coord2 : 0;
                draw_rect.Width = ((x_coord1 < x_size) ? x_coord1 + 1 : x_size)
                    - draw_rect.X;
            }

            if (y_coord2 >= y_coord1)
            {
                draw_rect.Y = (y_coord1 > 0) ? y_coord1 : 0;
                draw_rect.Height = ((y_coord2 < y_size) ? y_coord2 + 1 : y_size)
                    - draw_rect.Y;
            }
            else
            {
                draw_rect.Y = (y_coord2 > 0) ? y_coord2 : 0;
                draw_rect.Height = ((y_coord1 < y_size) ? y_coord1 + 1 : y_size)
                    - draw_rect.Y;
            }
            try
            {
                gBmp.DrawLine(pens[(int) hue], x_coord1, y_coord1, x_coord2, y_coord2);
            }
            catch (Exception error)
            {

                    throw new Exception("ERROR!: Draw_Line failed (" + x1 + "," + y1 +
                        "," + x2 + "," + y2 + ")" +
                        "color:" + hue + " message: " + error.Message);
            }
            this.UpdateWindowUnlessFrozen();
        }
        public static void Draw_Line_Static(
            int x1,
            int y1,
            int x2,
            int y2,
            Color_Type hue)
        {
            Check_Window_Open("Draw_Line");
            form.Draw_Line_Object(x1, y1, x2, y2, hue);
        }
        public void Save_Bitmap_Object(
            string filename)
        {
            this.bmp.Save(filename);
        }
        public static void Save_Bitmap_Static(
            string filename)
        {
            Check_Window_Open("Save_Bitmap");
            form.Save_Bitmap_Object(filename);
        }
        public int Load_Bitmap_Object(
            string filename)
        {
            Bitmap x = new Bitmap(filename);
            return bitmaps.Add(x);
        }
        public static int Load_Bitmap(
            string filename)
        {
            Check_Window_Open("Load_Bitmap");
            return form.Load_Bitmap_Object(filename);
        }
        //-------------------------------------------------------------
		//-- Draw_Box draws a box with (X1,Y1) as one corner and 
		//-- (X2,Y2) as the opposite corner with the given
		//-- color and fill.
		//-------------------------------------------------------------
        public static void Draw_Box_Static(
            int x1,
            int y1,
            int x2,
            int y2,
            Color_Type hue,
            bool filled)
        {
            Check_Window_Open("Draw_Box");
            form.Draw_Box_Object(x1, y1, x2, y2, hue, filled);
        }
        public void Draw_Box_Object(
            int x1,
            int y1,
            int x2,
            int y2,
            Color_Type hue,
            bool filled)
        {
            int x_coord1, y_coord1;
            x_coord1 = this.Make_0_Based(x1);
            y_coord1 = this.Make_0_Based_And_Unflip(y1);
            int x_coord2, y_coord2;
            x_coord2 = this.Make_0_Based(x2);
            y_coord2 = this.Make_0_Based_And_Unflip(y2);
            //-- X1, Y1 should be upper left and X2, Y2 lower right
            if (x_coord2 < x_coord1)
            {
                Swap(ref x_coord1, ref x_coord2);
            }

            if (y_coord2 < y_coord1)
            {
                Swap(ref y_coord1, ref y_coord2);
            }
            if (filled)
            {
                gBmp.DrawRectangle(pens[(int)hue], x_coord1, y_coord1,
                    x_coord2 - x_coord1, y_coord2 - y_coord1);
                gBmp.FillRectangle(brushes[(int)hue], new Rectangle(x_coord1, y_coord1,
                    x_coord2 - x_coord1, y_coord2 - y_coord1));
            }
            else
            {
                gBmp.DrawRectangle(pens[(int)hue], x_coord1, y_coord1,
                    x_coord2 - x_coord1, y_coord2 - y_coord1);
            }
            draw_rect = new Rectangle(x_coord1, y_coord1,
                    x_coord2 - x_coord1 + 1, y_coord2 - y_coord1 + 1);
            this.UpdateWindowUnlessFrozen();
        }

        //
        // find the closest color to any RGB value
        public static int GetClosestColor(int red, int green, int blue)
        {
            int i;
            int difference;
            int min_difference = 3 * (256 * 256);
            int red_diff, green_diff, blue_diff;

            int closest = 0;
            for (i = 0; i < MAX_COLORS; i++)
            {
                red_diff = (RED(standard_color[i]) - red);
                green_diff = (GREEN(standard_color[i]) - green);
                blue_diff = (BLUE(standard_color[i]) - blue);
                difference = red_diff * red_diff + green_diff * green_diff +
                   blue_diff * blue_diff;
                if (difference < min_difference)
                {
                    min_difference = difference;
                    closest = i;
                }
            }
            return closest;
        }

        public static Color_Type Closest_Color(
            int red,
            int green,
            int blue)
        {
            if ((red < 0) || (red > 255))
            {
                throw new Exception("Red value must be between 0 and 255, not: " +
                    red);
            }
            if ((green < 0) || (green > 255))
            {
                throw new Exception("Green value must be between 0 and 255, not: " +
                    green);
            }
            if ((blue < 0) || (blue > 255))
            {
                throw new Exception("Blue value must be between 0 and 255, not: " +
                    blue);
            }
            return (Color_Type)GetClosestColor(
                red, green, blue);
        }
        public static void SetFontSize_Static(int size)
        {
            Check_Window_Open("SetFontSize");
            form.SetFontSizeObject(size);
        }
        public void SetFontSizeObject(int size)
        {
            if (size == 0)
            {
                this.current_font_size = default_font_size;
            }
            else if (size > 0 && size <= 100)
            {
                this.current_font_size = size;
            }
            else
            {
                throw new System.Exception("Error in Set_Font_Size: size must be in [0,100]");
            }
        }
        public void SetWindowTitleObject(string title)
        {
            form.Text = title;
        }
        public static void Set_Window_Title_Static(string title)
        {
            Check_Window_Open("SetWindowTitle");
            form.SetWindowTitleObject(title);
        }
        public int Get_Window_Width_Object()
        {
            return this.ClientRectangle.Width;
        }
        public static int Get_Window_Width_Static()
        {
            Check_Window_Open("Get_Window_Width");
            return form.Get_Window_Width_Object();
        }
        public int Get_Window_Height_Object()
        {
            return this.ClientRectangle.Height;
        }
        public static int Get_Window_Height()
        {
            Check_Window_Open("Get_Window_Height");
            return form.Get_Window_Height_Object();
        }
        public static void Open_Graph_Window_Static(int width, int height)
        {
            if (form != null)
            {
                throw new Exception("RAPTORGraph window already open");
            }
            try
            {
                form = new dotnetgraph(width, height);
                form.Show();
                if (start_topmost)
                {
                    MakeTopMost_Static();
                }
            }
            catch (Exception e)
            {
                MessageBox.Show("open2: " + e.Message);
            }

            form.Update();
        }
        public static void Close_Graph_Window_Static(bool force)
        {
            if (!force && form == null)
            {
                throw new Exception("RAPTORGraph window not open");
            }
            Close_Graph_Window_Force_Static();
        }

        public static void Get_Mouse_Button_Static(
            Mouse_Button which_button,
            ref int x,
            ref int y)
        {
            Wait_For_Mouse_Button(which_button);
            if (which_button == Mouse_Button.Left_Button)
            {
                x = form.left_click_x;
                y = form.left_click_y;
            }
            else
            {
                x = form.right_click_x;
                y = form.right_click_y;
            }
            form.click_x = x;
            form.click_y = y;
        }
        public static void Shutdown_Dotnetgraph()
        {
            if (Is_Open())
            {
                Close_Graph_Window_Force();
            }
        }
        public static int Get_Click_X()
        {
            return form.click_x;
        }
        public static int Get_Click_Y()
        {
            return form.click_y;
        }
        public static void Get_Mouse_Button(
            Mouse_Button which_button)
        {
            int x = 0;
            int y = 0;
            Check_Window_Open("Get_Mouse_Button");
            Get_Mouse_Button(which_button, ref x, ref y);
        }
        public static void Close_Graph_Window_Force_Static()
        {
            form.gBmp.Dispose();
            form.Close();
            form = null;
        }
        //----------------------------------------------------
        //-- Get_Font_Width returns the width of a character
        //-- in pixels.  Use to determine how big an area a
        //-- string passed to Display_Text will occupy.
        //----------------------------------------------------
        public int Get_Font_Width_Object()
        {
            // Set up string.
            string measureString = "MMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMM";
            // Measure string.
            SizeF stringSize = new SizeF();
            stringSize = gBmp.MeasureString(measureString, fonts[current_font_size]);
            // Draw rectangle representing size of string.
            //gBmp.DrawRectangle(
            //new Pen(Color.Red, 1),
            //40.0F, 40.0F, stringSize.Width, stringSize.Height);
            // Draw string to screen.
            //gBmp.DrawString(
            //measureString,
            //fonts[current_font_size],
            //Brushes.Black,
            //new PointF(40, 40));
            return (int)(stringSize.Width/measureString.Length);
        }
        public static int Get_Font_Width_Static()
        {
            Check_Window_Open("Get_Font_Width");
            return form.Get_Font_Width_Object();
        }
        public int Get_Font_Height_Object()
        {

            SizeF size = gBmp.MeasureString("W", fonts[current_font_size]);
            return (int)size.Height;
        }
        public static int Get_Font_Height_Static()
        {
            Check_Window_Open("Get_Font_Height");
            return form.Get_Font_Height_Object();
        }
        //-------------------------------------------------------------
        //-- Draws text on the window at the given location.
        //-- X,Y represent the lower left corner of where the text
        //-- will be displayed.  Hue is the color of the text to 
        //-- be drawn.
        //-------------------------------------------------------------
        public void Display_Text_Object(
            int x,
            int y,
            string text,
            Color_Type hue)
        {
            int x_coord, y_coord;
            int len = (text.Length > 254) ? 254 : text.Length;
            x_coord = Make_0_Based(x);
            y_coord = Make_0_Based_And_Unflip(y);

            try
            {
                gBmp.DrawString(text,fonts[current_font_size],brushes[(int) hue],(float) x_coord, (float) y_coord);
            }
            catch (Exception error)
            {
                throw new Exception("ERROR!: Draw_Text failed:" + len + " @(" + x + "," + y + ")" +
                        " color: " + hue + ": " + error.Message);
            }
            draw_rect.X = x_coord;
            draw_rect.Y = y_coord;
            draw_rect.Width = bmp.Width;
            draw_rect.Height = bmp.Height;
            this.UpdateWindowUnlessFrozen();
        }
        public static void Display_Text_Static(
            int x,
            int y,
            string text,
            Color_Type hue)
        {
            Check_Window_Open("Display_Text");
            form.Display_Text_Object(x, y, text, hue);
        }
        public void Clear_Window_Object(Color_Type hue)
        {
            gBmp.FillRectangle(brushes[(int) hue], 0, 0, bmp.Width, bmp.Height);
            draw_rect.X = dotnetgraph.slop;
            draw_rect.Y = dotnetgraph.slop;
            draw_rect.Width = bmp.Width;
            draw_rect.Height = bmp.Height;
            this.UpdateWindowUnlessFrozen();
        }
        public static void Clear_Window_Static(Color_Type hue)
        {
            Check_Window_Open("Clear_Window");
            form.Clear_Window_Object(hue);
        }
        public static void Get_Max_Size_Static(
            ref int width, ref int height)
        {
            width = System.Windows.Forms.Screen.PrimaryScreen.WorkingArea.Width;
            height = System.Windows.Forms.Screen.PrimaryScreen.WorkingArea.Height;
        }
        public static int Get_Max_X()
        {
            return System.Windows.Forms.Screen.PrimaryScreen.WorkingArea.Width;
        }
        public static int Get_Max_Y()
        {
            return System.Windows.Forms.Screen.PrimaryScreen.WorkingArea.Height;
        }
        public void MakeTopMostObject()
        {
            this.TopMost = true;
        }
        public void MakeNonTopMostObject()
        {
            this.TopMost = false;
        }
        public static void MakeTopMost_Static()
        {
            Check_Window_Open("MakeTopMost");
            form.MakeTopMostObject();
        }
        public static void MakeNonTopMost_Static()
        {
            Check_Window_Open("MakeNonTopMost");
            form.MakeNonTopMostObject();
        }
        public static bool Is_Open()
        {
            return form != null;
        }
        public void Draw_Arc_Object(
            int x1,
            int y1,
            int x2,
            int y2,
            int startx,
            int starty,
            int endx,
            int endy,
            Color_Type hue)
        {
            int x_coord1, y_coord1;
            x_coord1 = Make_0_Based(x1);
            y_coord1 = Make_0_Based_And_Unflip(y1);
            int x_coord2, y_coord2;
            x_coord2 = Make_0_Based(x2);
            y_coord2 = Make_0_Based_And_Unflip(y2);
            //-- X1, Y1 should be upper left and X2, Y2 lower right
            if (x_coord2 < x_coord1)
            {
                Swap(ref x_coord1, ref x_coord2);
            }

            if (y_coord2 < y_coord1)
            {
                Swap(ref y_coord1, ref y_coord2);
            }

            double x_coord_start, y_coord_start;
            x_coord_start = (double) Make_0_Based(startx);
            y_coord_start = (double) Make_0_Based_And_Unflip(starty);
            double x_coord_end, y_coord_end;
            x_coord_end = (double) Make_0_Based(endx);
            y_coord_end = (double) Make_0_Based_And_Unflip(endy);
            double mid_x = ((double)x1+x2) / 2.0;
            double mid_y = ((double)y1+y2) / 2.0;
            double end_theta = Math.Atan2(starty - mid_y, startx - mid_x);
            double start_theta = Math.Atan2(endy - mid_y, endx - mid_x);

            if (end_theta < 0.0) { end_theta += 2.0 * Math.PI; }
            if (start_theta < 0.0) { start_theta += 2.0 * Math.PI; }
            double range = start_theta-end_theta;
            if (range <= 0.0) { range += 2.0 * Math.PI;  }
                
            // our help says we draw counter-clockwise, but C# does
            // clockwise!?!
            gBmp.DrawArc(pens[(int)hue], (float)x_coord1, (float)y_coord1,
                (float)(x_coord2 - x_coord1 + 1.0), (float)(y_coord2 - y_coord1 + 1.0),
                (float) ((2.0*Math.PI-start_theta)*180.0/Math.PI), 
                (float) (range*180.0/Math.PI));
            draw_rect.X = x_coord1;
            draw_rect.Y = y_coord1;
            draw_rect.Width = x_coord2-x_coord1+1;
            draw_rect.Height = y_coord2-y_coord1+1; 
            this.UpdateWindowUnlessFrozen();
        }
        public static void Draw_Arc_Static(
            int x1,
            int y1,
            int x2,
            int y2,
            int startx,
            int starty,
            int endx,
            int endy,
            Color_Type hue)
        {
            Check_Window_Open("Draw_Arc");
            form.Draw_Arc_Object(x1, y1, x2, y2, startx, starty, endx, endy, hue);
        }
        public static void Check_Window_Open(string method)
        {
            if (!Is_Open())
            {
                throw new Exception("Error in " + method + ": graphics window not open");
            }
        }
        public void Draw_Circle_Object(
            int x,
            int y,
            int radius,
            Color_Type hue,
            bool filled)
        {
            int x_coord, y_coord;
            x_coord = Make_0_Based(x);
            y_coord = Make_0_Based_And_Unflip(y);
            if (filled) {
                gBmp.DrawEllipse(pens[(int)hue], x_coord - radius, y_coord - radius,
                    2 * radius, 2 * radius);
                gBmp.FillEllipse(brushes[(int) hue],x_coord-radius,y_coord-radius,
                    2*radius,2*radius);
            }
            else {
                gBmp.DrawEllipse(pens[(int) hue],x_coord-radius,y_coord-radius,
                    2*radius,2*radius);
            }
            draw_rect.X = x_coord - radius - 1;
            draw_rect.Y = y_coord - radius - 1;
            draw_rect.Width =  2*radius + 2;
            draw_rect.Height =  2*radius + 2;
            this.UpdateWindowUnlessFrozen();
        }
        public static void Draw_Circle_Static(
            int x,
            int y,
            int radius,
            Color_Type hue,
            bool filled)
        {
            Check_Window_Open("Draw_Circle");
            form.Draw_Circle_Object(x, y, radius, hue, filled);
        }
        public Color_Type Get_Pixel_Object(int x, int y)
        {
            int x_coord, y_coord;
            x_coord = this.Make_0_Based(x);
            y_coord = this.Make_0_Based_And_Unflip(y);
            Color color = this.bmp.GetPixel(x_coord,y_coord);

            Color_Type ct = (Color_Type) GetClosestColor(color.R,color.G,color.B);
            return ct;
        }
        public static Color_Type Get_Pixel_Static(int x, int y)
        {
            Check_Window_Open("Get_Pixel");
            return form.Get_Pixel_Object(x, y);
        }
        public void Put_Pixel_Object(
            int x,
            int y,
            Color_Type hue)
        {
            int x_coord, y_coord;
            x_coord = this.Make_0_Based(x);
            y_coord = this.Make_0_Based_And_Unflip(y);
            this.bmp.SetPixel(x_coord, y_coord, colors[(int)hue]);
            this.UpdateWindowUnlessFrozen();
        }
        public static void Put_Pixel_Static(
            int x,
            int y,
            Color_Type hue)
        {
            Check_Window_Open("Put_Pixel");
            form.Put_Pixel_Object(x, y, hue);
        }
        public void Draw_Ellipse_Object(
            int x1,
            int y1,
            int x2,
            int y2,
            Color_Type hue,
            bool filled)
        {
            int x_coord1, y_coord1;
            x_coord1 = this.Make_0_Based(x1);
            y_coord1 = this.Make_0_Based_And_Unflip(y1);
            int x_coord2, y_coord2;
            x_coord2 = this.Make_0_Based(x2);
            y_coord2 = this.Make_0_Based_And_Unflip(y2);
            //-- X1, Y1 should be upper left and X2, Y2 lower right
            if (x_coord2 < x_coord1)
            {
                Swap(ref x_coord1, ref x_coord2);
            }

            if (y_coord2 < y_coord1)
            {
                Swap(ref y_coord1, ref y_coord2);
            }
            if (filled)
            {
                gBmp.DrawEllipse(pens[(int)hue], x_coord1, y_coord1,
                    x_coord2 - x_coord1, y_coord2 - y_coord1); 
                gBmp.FillEllipse(brushes[(int)hue], new Rectangle(x_coord1, y_coord1,
                    x_coord2 - x_coord1, y_coord2 - y_coord1));
            }
            else
            {
                gBmp.DrawEllipse(pens[(int)hue], x_coord1, y_coord1,
                    x_coord2 - x_coord1, y_coord2 - y_coord1);
            }
            draw_rect.X = x_coord1-3;
            draw_rect.Y = y_coord1-3;
            draw_rect.Width = x_coord2 - x_coord1 + 6;
            draw_rect.Height = y_coord2 - y_coord1 + 6;
            this.UpdateWindowUnlessFrozen();
        }
        public static void Draw_Ellipse_Static(
            int x1,
            int y1,
            int x2,
            int y2,
            Color_Type hue,
            bool filled)
        {
            Check_Window_Open("Draw_Ellipse");
            form.Draw_Ellipse_Object(x1, y1, x2, y2, hue, filled);
        }

        // from http://www.codeguru.com/Cpp/G-M/gdi/article.php/c131
        // Llew Goodstadt
        // adapted to C# by Martin Carlisle

        // Create points to simulate ellipse using beziers
        void EllipseToBezier(RectangleF r, PointF[] cCtlPt)
        {
            // MAGICAL CONSTANT to map ellipse to beziers
            //  			2/3*(sqrt(2)-1) 
            const double EToBConst = 0.2761423749154;

            SizeF offset = new SizeF();
            //	offset.cx = ((int)((abs(r.right-r.left+1)) * EToBConst));
            //	offset.cy = ((int)((abs(r.top-r.bottom+1)) * EToBConst));
            offset.Width = (float)(Math.Abs(r.Right - r.Left) * EToBConst);
            offset.Height = (float)(Math.Abs(r.Top - r.Bottom) * EToBConst);
            //  Use the following line instead for mapping systems where +ve Y is upwards
            //  CSize offset((int)(r.Width() * EToBConst), -(int)(r.Height() * EToBConst));

            PointF centre = new PointF();
            centre.X = (float)(((r.Left + r.Right)) / 2.0);
            centre.Y = (float)(((r.Top + r.Bottom)) / 2.0);

            cCtlPt[0].X =                            //------------------------/
            cCtlPt[1].X =                            //                        /
            cCtlPt[11].X =                            //        2___3___4       /
            cCtlPt[12].X = r.Left;                   //     1             5    /
            cCtlPt[5].X =                            //     |             |    /
            cCtlPt[6].X =                            //     |             |    /
            cCtlPt[7].X = r.Right;                  //     0,12          6    /
            cCtlPt[2].X =                            //     |             |    /
            cCtlPt[10].X = centre.X - offset.Width;   //     |             |    /
            cCtlPt[4].X =                            //    11             7    /
            cCtlPt[8].X = centre.X + offset.Width;   //       10___9___8       /
            cCtlPt[3].X =                            //                        /
            cCtlPt[9].X = centre.X;                  //------------------------*

            cCtlPt[2].Y =
            cCtlPt[3].Y =
            cCtlPt[4].Y = r.Top;
            cCtlPt[8].Y =
            cCtlPt[9].Y =
            cCtlPt[10].Y = r.Bottom;
            cCtlPt[7].Y =
            cCtlPt[11].Y = centre.Y + offset.Height;
            cCtlPt[1].Y =
            cCtlPt[5].Y = centre.Y - offset.Height;
            cCtlPt[0].Y =
            cCtlPt[12].Y =
            cCtlPt[6].Y = centre.Y;
        }

        void Rotate(PointF c, PointF[] vCtlPt, uint Cnt, double sinAng, double cosAng)
        {
            PointF constTerm = new PointF();
            constTerm.X = ((float)(c.X - c.X * cosAng - c.Y * sinAng));
            constTerm.Y = ((float)(c.Y + c.X * sinAng - c.Y * cosAng));
            double c1, c2;

            for (int i = (int)Cnt - 1; i >= 0; --i)
            {
                c1 = (vCtlPt[i].X * cosAng + vCtlPt[i].Y * sinAng) + constTerm.X;
                c2 = (-vCtlPt[i].X * sinAng + vCtlPt[i].Y * cosAng) + constTerm.Y;
                vCtlPt[i].X = (float)c1;
                vCtlPt[i].Y = (float)c2;
            }
        }

        public void DrawEllipseRotateHelper(int x1, int y1, int x2, int y2, double sinAng, double cosAng, Color_Type hue, bool filled)
        {
            int d_x1, d_x2, d_y1, d_y2;

            if ((x1 == x2) && (y1 == y2))
            {
                this.Put_Pixel_Object(x1, y1, hue);
            }

            if ((x1 == x2) || (y1 == y2))
            {
                this.Draw_Line_Object(x1, x2, y1, y2, hue);
            }

            if (x2 > x1)
            {
                d_x1 = x1;
                d_x2 = x2;
            }
            else
            {
                d_x1 = x2;
                d_x2 = x1;
            }

            if (y2 > y1)
            {
                d_y1 = y1;
                d_y2 = y2;
            }
            else
            {
                d_y1 = y2;
                d_y2 = y1;
            }
            RectangleF draw_rect = new RectangleF((float)d_x1, (float)d_y1,
                (float)d_x2 - d_x1 + 1, (float)d_y2 - d_y1 + 1);

            PointF[] ellipsePts = new PointF[13];
            PointF midPoint = new PointF();
            EllipseToBezier(draw_rect, ellipsePts);

            midPoint.X = (float)(((draw_rect.Left) + (draw_rect.Right)) / 2.0);
            midPoint.Y = (float)(((draw_rect.Top) + (draw_rect.Bottom)) / 2.0);



            // Rotate
            Rotate(midPoint, ellipsePts, 13, sinAng, cosAng);

            if (filled)
            {
                System.Drawing.Drawing2D.GraphicsPath path = new System.Drawing.Drawing2D.GraphicsPath(
                    ellipsePts, new byte[] { (byte) System.Drawing.Drawing2D.PathPointType.Start,
               (byte) System.Drawing.Drawing2D.PathPointType.Bezier,
               (byte) System.Drawing.Drawing2D.PathPointType.Bezier,
               (byte) System.Drawing.Drawing2D.PathPointType.Bezier,
               (byte) System.Drawing.Drawing2D.PathPointType.Bezier,
               (byte) System.Drawing.Drawing2D.PathPointType.Bezier,
               (byte) System.Drawing.Drawing2D.PathPointType.Bezier,
               (byte) System.Drawing.Drawing2D.PathPointType.Bezier,
               (byte) System.Drawing.Drawing2D.PathPointType.Bezier,
               (byte) System.Drawing.Drawing2D.PathPointType.Bezier,
               (byte) System.Drawing.Drawing2D.PathPointType.Bezier,
               (byte) System.Drawing.Drawing2D.PathPointType.Bezier,
               (byte) System.Drawing.Drawing2D.PathPointType.Bezier});
                gBmp.FillPath(brushes[(int)hue], path);
                gBmp.DrawBeziers(pens[(int)hue], ellipsePts);
            }
            else
            {
                gBmp.DrawBeziers(pens[(int)hue], ellipsePts);
            }

        }

        public void Draw_Ellipse_Rotate_Object(
            int x1,
            int y1,
            int x2,
            int y2,
            double angle,
            Color_Type hue,
            bool filled)
        {
            int x_coord1, y_coord1;
            x_coord1 = this.Make_0_Based(x1);
            y_coord1 = this.Make_0_Based_And_Unflip(y1);
            int x_coord2, y_coord2;
            x_coord2 = this.Make_0_Based(x2);
            y_coord2 = this.Make_0_Based_And_Unflip(y2);
            //-- X1, Y1 should be upper left and X2, Y2 lower right
            if (x_coord2 < x_coord1)
            {
                Swap(ref x_coord1, ref x_coord2);
            }

            if (y_coord2 < y_coord1)
            {
                Swap(ref y_coord1, ref y_coord2);
            }
            this.DrawEllipseRotateHelper(x_coord1, y_coord1, x_coord2, y_coord2, Math.Sin(angle),
                Math.Cos(angle), hue, filled);
            draw_rect = new Rectangle(dotnetgraph.slop, dotnetgraph.slop,
                bmp.Width, bmp.Height);
            this.UpdateWindowUnlessFrozen();
        }
        public static void Draw_Ellipse_Rotate_Static(
            int x1,
            int y1,
            int x2,
            int y2,
            double angle,
            Color_Type hue,
            bool filled)
        {
            Check_Window_Open("Draw_Ellipse_Rotate");
            form.Draw_Ellipse_Rotate_Object(x1, y1, x2, y2, angle, hue, filled);
        }

        public void check_and_enqueue(
            int x,
            int y,
            System.Collections.Queue flood_queue,
            int[,] marked,
            Color color)
        {
            Color pix_color = Color.Beige;
            if (x < 0 || x >= bmp.Width ||
                y < 0 || y >= bmp.Height ||
                marked[x, y] == 1)
            {
                return;

            }
            pix_color = this.bmp.GetPixel(x, y); 

            if (pix_color != color)
            {
                return;
            }
            else
            {
                marked[x, y] = 1;
                flood_queue.Enqueue(new Point(x, y));
            }
        }
        public void Flood_Fill_Recursive(
            int x,
            int y,
            Color_Type start_color,
            Color_Type to_color)
        {
            int x_coord, y_coord;
            System.Collections.Queue flood_queue;
            int[,] marked;
            x_coord = this.Make_0_Based(x);
            y_coord = this.Make_0_Based_And_Unflip(y);
            Color color = this.bmp.GetPixel(x_coord, y_coord);
            marked = new int[this.bmp.Width,this.bmp.Height];

            {
                flood_queue = new System.Collections.Queue();
                flood_queue.Enqueue(new Point(x_coord, y_coord));
                marked[x_coord, y_coord] = 1;
                while (flood_queue.Count > 0)
                {
                    Point p = (Point)flood_queue.Dequeue();
                    bool worked = false;

                        //string s1 = ("x,y,color:" +
                        //    p.X + "," + p.Y + "," + this.bmp.GetPixel(p.X, p.Y));
                        //System.Diagnostics.Trace.WriteLine(s1);
                    //while (!worked)
                    {
                        //try
                        {
                            this.bmp.SetPixel(p.X, p.Y, colors[(int)to_color]);
                            worked = true;
                        }
                        //catch
                        {
                        //    System.Threading.Thread.Sleep(1);
                        }
                    }
                        marked[p.X, p.Y] = 1;
                        this.check_and_enqueue(p.X + 1, p.Y, flood_queue, marked, color);
                        this.check_and_enqueue(p.X, p.Y + 1, flood_queue, marked, color);
                        this.check_and_enqueue(p.X - 1, p.Y, flood_queue, marked, color);
                        this.check_and_enqueue(p.X, p.Y - 1, flood_queue, marked, color);

                }
            }
        }
        public static void Flood_Fill_Static(
            int x,
            int y,
            Color_Type hue)
        {
            if (x < 1 || y < 1 || x > form.bmp.Width || y > form.bmp.Height)
            {
                return;
            }
            form.Flood_Fill_Recursive(x, y, Get_Pixel_Static(x, y), hue);
            form.draw_rect = new Rectangle(dotnetgraph.slop,dotnetgraph.slop,
                form.bmp.Width,form.bmp.Height);
            form.UpdateWindowUnlessFrozen();
        }
        public static void FreezeGraphWindow_Static()
        {
            form.frozen = true;
            form.copy_buffers();
        }
        public static void UnfreezeGraphWindow_Static()
        {
            form.frozen = false;
            form.swap_buffers();
            form.Invalidate();
        }
        public static void Wait_For_Mouse_Button_Static(
            Mouse_Button which_button)
        {
            form.Wait_For_Mouse_Button_Object(which_button);
        }

        public void Wait_For_Key_Object()
        {

            this.key_available_event.WaitOne();
            this.key_available_event.Reset();
        }
        public static void Wait_For_Key()
        {
            Check_Window_Open("Wait_For_Key");
            form.Wait_For_Key_Object();
        }
        public void Draw_Bitmap_Object(int bitmap, int x, int y, int width, int height)
        {
            int x_coord1, y_coord1;
            x_coord1 = Make_0_Based(x);
            y_coord1 = Make_0_Based_And_Unflip(y);
            gBmp.DrawImage((Bitmap) bitmaps[bitmap], x_coord1, y_coord1, width, height);
            draw_rect = new Rectangle(x_coord1, y_coord1, width, height);
            this.UpdateWindowUnlessFrozen();
        }
        public static void Draw_Bitmap_Static(int bitmap, int x, int y, int width, int height)
        {
            Check_Window_Open("Draw_Bitmap");
            form.Draw_Bitmap_Object(bitmap, x, y, width, height);
        }
        //-------------------------------------------------------------
        //-- Mouse_Button_Pressed returns true if the specified mouse
        //-- button has been pressed.  This always returns true
        //-- until Get_Mouse_Button or Wait_for_Mouse_Button has 
        //-- been called.
        //-- Example:
        //--    if (Mouse_Button_Pressed(Left_Button)) {
        //--       Get_Mouse_Button(
        //--          Left_Button,
        //--          ref X_Location,
        //--          ref Y_Location);
        //--    }
        //-------------------------------------------------------------
        //-------------------------------------------------------------
        //-- Mouse_Button_Released returns true if the specified mouse
        //-- button has been released.  This always returns true
        //-- until Get_Mouse_Button or Wait_for_Mouse_Button has 
        //-- been called.
        //-- Example:
        //--    if (Mouse_Button_Released(Left_Button)) {
        //--       Get_Mouse_Button(
        //--          Left_Button,
        //--          ref X_Location,
        //--          ref Y_Location);
        //--    }
        //-------------------------------------------------------------
        public bool Mouse_Button_Down_Object(Mouse_Button which_button)
        {
            if (which_button == Mouse_Button.Left_Button)
            {
                return this.left_is_down;
            }
            else
            {
                return this.right_is_down;
            }
        }
        public static bool Mouse_Button_Down(Mouse_Button which_button)
        {
            Check_Window_Open("Mouse_Button_Down");
            return form.Mouse_Button_Down_Object(which_button);
        }
        public static void UpdateGraphWindow_Static()
        {
            form.swap_buffers();
            form.Invalidate();
        }
        public dotnetgraph(int width, int height)
        {
            InitializeComponent();
            this.Width = (this.Width - this.ClientRectangle.Width) + width;
            this.Height = (this.Height - this.ClientRectangle.Height) + height;
            this.MinimumSize = this.Size;
            this.MaximumSize = this.Size;
            this.Text = "RAPTORGraph";
            bmp = new Bitmap(this.ClientRectangle.Width+2*dotnetgraph.slop, 
                this.ClientRectangle.Height+2*dotnetgraph.slop);
            gBmp = Graphics.FromImage(bmp);
            x_size = this.bmp.Width;
            y_size = this.bmp.Height;
            bmp_swap = new Bitmap(this.ClientRectangle.Width + 2 * dotnetgraph.slop,
                this.ClientRectangle.Height + 2 * dotnetgraph.slop);
            gBmp_swap = Graphics.FromImage(bmp_swap);

            key_available_event = new System.Threading.ManualResetEvent(false);
            left_pressed_event = new System.Threading.ManualResetEvent(false);
            right_pressed_event= new System.Threading.ManualResetEvent(false);
            left_released_event= new System.Threading.ManualResetEvent(false);
            right_released_event= new System.Threading.ManualResetEvent(false);
            mouse_event_event = new System.Threading.ManualResetEvent(false);

            if (brushes == null)
            {
                brushes = new Brush[MAX_COLORS];
                pens = new Pen[MAX_COLORS];
                colors = new Color[MAX_COLORS];
                for (int i = 0; i < brushes.Length; i++)
                {
                    brushes[i] = new SolidBrush(Color.FromArgb((int)standard_color[i]));
                    pens[i] = new Pen(Color.FromArgb((int)standard_color[i]));
                }
                fonts = new Font[101];
                for (int i = 1; i < fonts.Length; i++)
                {
                    fonts[i] = new Font("Lucida Console", (float)i);
                }
                for (int i = 0; i < colors.Length; i++)
                {
                    colors[i] = Color.FromArgb((int)standard_color[i]);
                }
            }
            gBmp.FillRectangle(Brushes.White, 0, 0, bmp.Width, bmp.Height);
        }
        public static bool Key_Down_Static(
            int key)
        {
            return form.key_is_down[key];
        }
        public static int get_key_code()
        {
            for (int i = 0; i < 255; i++)
            {
                if (Key_Down(i)) return i;
            }
            return 0;
        }
        //-------------------------------------------------------------
        //-- Get_Key returns the last key pressed by the user in the
        //-- Adagraph window.  If no key was pressed, it waits until
        //-- a key is pressed before returning.
        //-- Example:
        //--   Letter = Get_Key;
        //-------------------------------------------------------------
        public static string Get_Key_String()
        {
            Check_Window_Open("Get_Key_String");
            int x = Get_Key_Int();
            if (x > 27 && x < 160)
            {
                return ((char)x).ToString();
            }
            else
            {
                switch (x)
                {
                    case 8:
                        return "Backspace";
                    case 9:
                        return "Tab";
                    case 13:
                        return "Enter";
                    case 27:
                        return "Esc";
                    case 161:
                        return "PageUp";
                    case 162:
                        return "PageDn";
                    case 163:
                        return "End";
                    case 164:
                        return "Home";
                    case 165:
                        return "Left";
                    case 166:
                        return "Up";
                    case 167:
                        return "Right";
                    case 168:
                        return "Down";
                    case 173:
                        return "Insert";
                    case 174:
                        return "Delete";
                    default:
                        if (x >= 1 && x <= 26)
                        {
                            return "Ctrl-" + ((char)(x + 64)).ToString();
                        }
                        else if (x >= 240 && x <= 251)
                        {
                            return "F" + (x - 239).ToString();
                        }
                        return x.ToString();
                }
            }
        }
        public static char Get_Key()
        {
            Check_Window_Open("Get_Key");
            Wait_For_Key();
            return form.pressed_key;
        }
        private static int Get_Key_Int_Static()
        {
            if (!Is_Open())
            {
                throw new Exception("ERROR in Get_Key: Graph Window not open");
            }
            Wait_For_Key();
            return (int) form.pressed_key;
        }
        private static int_Delegate_Type Get_Key_Int_Delegate = new int_Delegate_Type(
            Get_Key_Int_Static);
        public static int Get_Key_Int()
        {
            return Get_Key_Int_Static();
        }
        public static bool Key_Down_String(string key)
        {
            Check_Window_Open("Key_Down_String");
            string lower_key = key.ToLower();
            if (key.Length == 1)
            {
                int x = key.ToUpper()[0];
                return Key_Down(x);
            }
            else if (lower_key == "backspace")
            {
                return Key_Down(8);
            }
            else if (lower_key == "esc")
            {
                return Key_Down(27);
            }
            else if (lower_key == "tab")
            {
                return Key_Down(9);
            }
            else if (lower_key == "return" || lower_key == "enter")
            {
                return Key_Down(0x0D);
            }
            else if (lower_key == "shift")
            {
                return Key_Down(0x10);
            }
            else if (lower_key == "control" || lower_key == "ctrl")
            {
                return Key_Down(0x11);
            }
            else if (lower_key == "end")
            {
                return Key_Down(0xA3);
            }
            else if (lower_key == "home")
            {
                return Key_Down(0xA4);
            }
            else if (lower_key == "left")
            {
                return Key_Down(0xA5);
            }
            else if (lower_key == "up")
            {
                return Key_Down(0xA6);
            }
            else if (lower_key == "right")
            {
                return Key_Down(0xA7);
            }
            else if (lower_key == "down")
            {
                return Key_Down(0xA8);
            }
            else if (lower_key == "pgup" || lower_key == "page up")
            {
                return Key_Down(0xA1);
            }
            else if (lower_key == "pgdn" || lower_key == "page down")
            {
                return Key_Down(0xA2);
            }
            else if (lower_key == "insert" || lower_key == "ins")
            {
                return Key_Down(0xAD);
            }
            else if (lower_key == "delete" || lower_key == "del")
            {
                return Key_Down(0xAE);
            }
            else if (lower_key == "f1")
            {
                return Key_Down(0xF0);
            }
            else if (lower_key == "f2")
            {
                return Key_Down(0xF1);
            }
            else if (lower_key == "f3")
            {
                return Key_Down(0xF2);
            }
            else if (lower_key == "f4")
            {
                return Key_Down(0xF3);
            }
            else if (lower_key == "f5")
            {
                return Key_Down(0xF4);
            }
            else if (lower_key == "f6")
            {
                return Key_Down(0xF5);
            }
            else if (lower_key == "f7")
            {
                return Key_Down(0xF6);
            }
            else if (lower_key == "f8")
            {
                return Key_Down(0xF7);
            }
            else if (lower_key == "f9")
            {
                return Key_Down(0xF8);
            }
            else if (lower_key == "f10")
            {
                return Key_Down(0xF9);
            }
            else if (lower_key == "f11")
            {
                return Key_Down(0xFA);
            }
            else if (lower_key == "f12")
            {
                return Key_Down(0xFB);
            }
            else
            {
                return false;
            }
        }

        //-------------------------------------------------------------
        //-- Get_Mouse_Location puts the current location of the mouse
        //-- into its two ref parameters, X and Y.
        //-------------------------------------------------------------
        public static void Get_Mouse_Location_Static(
            ref int x,
            ref int y)
        {
            try
            {
                x = form.mouse.X;
                y = form.mouse.Y;
            }
            catch (Exception error)
            {
                if (!Is_Open())
                {
                    throw new Exception("ERROR in Get_Mouse_Location: Graph Window not open");
                }
                else
                {
                    throw new Exception("ERROR!: Get_Mouse_Location failed: " +
                        "(" + x + "," + y + "):" + error.Message);
                }
            }
            sync_args[0] = x;
            sync_args[1] = y;
        }
        public delegate void Get_Mouse_Location_Delegate_Type(
            ref int x,
            ref int y);
        private static Get_Mouse_Location_Delegate_Type Get_Mouse_Location_Delegate = new Get_Mouse_Location_Delegate_Type(
            Get_Mouse_Location_Static);
        public static void Get_Mouse_Location(
            ref int x,
            ref int y)
        {
            Check_Window_Open("Get_Mouse_Location");
            uc.Invoke(Get_Mouse_Location_Delegate, new object[] { x, y });
            x = (int)sync_args[0];
            y = (int)sync_args[1];
        }	
        public static int Get_Mouse_X()
        {
            int x = 0, y = 0;

            try
            {
                Get_Mouse_Location(ref x, ref y);
            }
            catch (Exception error)
            {
                if (!Is_Open())
                {
                    throw new Exception("ERROR in Get_Mouse_X: Graph Window not open");
                }
                else
                {
                    throw new Exception("ERROR!: Get_Mouse_X failed: " + error.Message);
                }
            }
            return x;
        }
        public static int Get_Mouse_Y()
        {
            int x = 0, y = 0;
            try
            {
                Get_Mouse_Location(ref x, ref y);
            }
            catch (Exception error)
            {
                if (!Is_Open())
                {
                    throw new Exception("ERROR in Get_Mouse_Y: Graph Window not open");
                }
                else
                {
                    throw new Exception("ERROR!: Get_Mouse_Y failed: " + error.Message);
                }
            }
            return y;
        }
        private void Form1_Paint(object sender, PaintEventArgs e)
        {
            Graphics gForm = e.Graphics;
            //gForm.FillRectangle(Brushes.White, this.ClientRectangle);

            // Create a bitmap in memory
            //Bitmap bmp = new Bitmap(6,6);

            if (frozen)
            {
                lock (bmp_swap)
                {
                    gForm.DrawImage(bmp_swap, e.ClipRectangle,
                         new Rectangle(e.ClipRectangle.X + dotnetgraph.slop,
                                e.ClipRectangle.Y + dotnetgraph.slop,
                                e.ClipRectangle.Width,
                                e.ClipRectangle.Height),
                         GraphicsUnit.Pixel);
                }
            }
            else
            {
                gForm.DrawImage(bmp, e.ClipRectangle,
                         new Rectangle(e.ClipRectangle.X+dotnetgraph.slop, 
                                e.ClipRectangle.Y+dotnetgraph.slop,
                                e.ClipRectangle.Width, 
                                e.ClipRectangle.Height),
                         GraphicsUnit.Pixel);
            }
            //}
            //}
      
            //// Finally, get the pixel information
            //for (int y = 0; y < bmp.Height; ++y)
            //{
            //    for (int x = 0; x < bmp.Width; ++x)
            //    {
            //        Color c = bmp.GetPixel(x, y);
            //        Console.Write("{0,2:x}{1,2:x}{2,2:x}{3,2:x}  ",
            //          c.A, c.R, c.G, c.G);
            //    }
            //    Console.WriteLine();
            //}
        }

        private void Form1_FormClosed(object sender, FormClosedEventArgs e)
        {
            form = null;
        }

        private void dotnetgraph_KeyDown(object sender, KeyEventArgs e)
        {
            switch (e.KeyCode) {
                case Keys.F1:
                case Keys.F2:
                case Keys.F3:
                case Keys.F4:
                case Keys.F5:
                case Keys.F6:
                case Keys.F7:
                case Keys.F8:
                case Keys.F9:
                case Keys.F10:
                case Keys.F11:
                case Keys.F12:
                case Keys.PageDown:
                case Keys.PageUp:
                case Keys.Insert:
                case Keys.Home:
                case Keys.Delete:
                case Keys.End:
                case Keys.Up:
                case Keys.Down:
                case Keys.Left:
                case Keys.Right:
                    key_available_event.Set();
                    pressed_key = (char) ((e.KeyValue+128)%256);
                    key_is_down[(e.KeyValue+128)%256] = true;
                    break;
                default:
                    key_is_down[e.KeyValue] = true;
                    break;
            }
        }

        private void dotnetgraph_KeyPress(object sender, KeyPressEventArgs e)
        {
            key_available_event.Set();
            pressed_key = e.KeyChar;
        }

        private void dotnetgraph_KeyUp(object sender, KeyEventArgs e)
        {
            switch (e.KeyCode)
            {
                case Keys.F1:
                case Keys.F2:
                case Keys.F3:
                case Keys.F4:
                case Keys.F5:
                case Keys.F6:
                case Keys.F7:
                case Keys.F8:
                case Keys.F9:
                case Keys.F10:
                case Keys.F11:
                case Keys.F12:
                case Keys.PageDown:
                case Keys.PageUp:
                case Keys.Insert:
                case Keys.Home:
                case Keys.Delete:
                case Keys.End:
                case Keys.Up:
                case Keys.Down:
                case Keys.Left:
                case Keys.Right:
                    key_is_down[(e.KeyValue + 128) % 256] = false;
                    break;
                default:
                    key_is_down[e.KeyValue] = false;
                    break;
            }
        }

        private void dotnetgraph_MouseDown(object sender, MouseEventArgs e)
        {
            mouse.X = this.Make_1_Based(e.X);
            mouse.Y = Make_1_Based_And_Flip(e.Y);
            if (e.Button == MouseButtons.Left)
            {
                left_pressed_event.Set();
                this.left_is_down = true;
            }
            else if (e.Button == MouseButtons.Right)
            {
                right_pressed_event.Set();
                this.right_is_down = true;
            }
        }

        private void dotnetgraph_MouseUp(object sender, MouseEventArgs e)
        {
            mouse.X = this.Make_1_Based(e.X);
            mouse.Y = Make_1_Based_And_Flip(e.Y);
            if (e.Button == MouseButtons.Left)
            {
                left_click_x = mouse.X;
                left_click_y = mouse.Y;
                left_released_event.Set();
                this.left_is_down = false;
            }
            else if (e.Button == MouseButtons.Right)
            {
                right_click_x = mouse.X;
                right_click_y = mouse.Y;
                right_released_event.Set();
                this.right_is_down = false;
            }
        }

        private void dotnetgraph_MouseMove(object sender, MouseEventArgs e)
        {
            mouse.X = this.Make_1_Based(e.X);
            mouse.Y = Make_1_Based_And_Flip(e.Y);
        }
    }
}