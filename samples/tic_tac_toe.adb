procedure tic_tac_toe is
   Board : ??_2D_Array;
   Row : ??_Variable;
   Col : ??_Variable;
   X : ??_Variable;
   Y : ??_Variable;
   Winner : ??_Variable;
begin
   Board(3,3) := 0;
   open_graph_window(300,300);
   set_window_title("Tic Tac Toe");
   loop
      Draw_Board(Board);
      exit when Game_Over(Board);
      loop
         wait_for_mouse_button(left_button);
         X :=Get_Click_X;
         Y :=Get_Click_Y;
         Row :=Get_Row(X,Y);
         Col :=Get_Col(X,Y);
         exit when BoardRowColBoard=0;
      end loop;
      Board(Row,Col) := 1;
      Draw_Board(Board);
      if Game_Over(Board) then 
      else
         loop
            Row :=floor(3*random)+1;
            Col :=floor(3*random)+1;
            exit when BoardRowColBoard=0;
         end loop;
         Board(Row,Col) := 2;
      end if;
   end loop;
   Winner :=Who_Won(Board);
   if Winner=2 then 
      Put_Line("Computer won!");
   else
      if Winner=1 then 
         Put_Line("User won!");
      else
         Put_Line("Tie!");
      end if;
   end if;
end tic_tac_toe;
