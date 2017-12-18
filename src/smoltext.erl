-module(smoltext).
-compile(export_all).
-include_lib("wx/include/wx.hrl").
 
start() ->
    State = make_window().
    loop(State),
    wx:destroy().
 
make_window() ->
    Server = wx:new(),
    Frame = wxFrame:new(Server, -1, "smoltext"),
    wxTopLevelWindow:maximize(Frame),
    Panel = wxPanel:new(Frame),
    Entry = wxTextCtrl:new(Panel, -1, [{style, ?wxTE_MULTILINE}]),

loop(State) ->
receive
        #wx{event=#wxClose{}} ->	
			if
				Pid /= self() -> Pid ! { -1 };
				true -> ok
            end,
			io:format("~p Closing window ~n",[self()]),
			wxWindow:destroy(Frame),
			ok;
        #wx{id = ?wxID_EXIT, event=#wxCommand{type = command_button_clicked} } ->
			%%     {wx, ?wxID_EXIT, _,_,_} ->
			%this message is sent when the exit button (ID 102) is clicked
            %the other fields in the tuple are not important to us.
			if
				Pid /= self() -> Pid ! { -1 };
				true -> ok
			end,
            io:format("~p Closing window ~n",[self()]), %optional, goes to shell
			wxWindow:destroy(Frame),
			ok;  % we exit the loop
 
        #wx{id = 101, event=#wxCommand{type = command_button_clicked}} ->
            %this message is sent when the Countdown button (ID 101) is clicked
            T1001_val = wxTextCtrl:getValue(T1001),
            case is_valid_list_to_integer(T1001_val) of
                true ->
                    T1001_int = list_to_integer(T1001_val),
					MyPid = self(),
					CntdwnPid = spawn( fun() -> cntdwn( T1001_int, MyPid ) end );
                _ ->
                    wxStaticText:setLabel(ST2001, "Only integers are allowed"),
					CntdwnPid = Pid
            end,
            loop( {Frame, T1001, ST2001, CntdwnPid } );
				{ 0 } ->
					OutputStr = "ZERO",
					wxStaticText:setLabel(ST2001, OutputStr),
					loop( State );            
                { N } ->
					OutputStr = integer_to_list(N),
					wxStaticText:setLabel(ST2001, OutputStr),
					loop( State );
        Msg ->
            %everything else ends up here
            io:format("loop default triggered: Got ~n ~p ~n", [Msg]),
            loop(State)
 
    end.
