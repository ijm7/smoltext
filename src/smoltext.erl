-module(smoltext).
-compile(export_all).
-include_lib("wx/include/wx.hrl").
 
start() ->
    State = make_window(),
    loop(State),
    wx:destroy().
 
make_window() ->
    Server = wx:new(),
    Frame = wxFrame:new(Server, -1, "smoltext"),
    wxTopLevelWindow:maximize(Frame),
    Panel = wxPanel:new(Frame),
    TextBox = wxTextCtrl:new(Panel, -1, [{style, ?wxTE_MULTILINE}]),
    TextBoxSizer = wxBoxSizer:new(?wxHORIZONTAL),
    wxSizer:add(TextBoxSizer, TextBox, []),
    wxFrame:show(Frame),
    wxFrame:connect(Frame, close_window),
    wxPanel:connect(Panel, command_button_clicked),
    {Frame, TextBox, self()}.

loop(State) ->
	{Frame, TextBox, Pid} = State,
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
			if
				Pid /= self() -> Pid ! { -1 };
				true -> ok
			end,
            io:format("~p Closing window ~n",[self()]), %optional, goes to shell
			wxWindow:destroy(Frame),
			ok;  % we exit the loop
        Msg ->
            loop(State)
 
    end.
