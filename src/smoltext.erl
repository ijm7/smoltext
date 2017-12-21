-module(smoltext).
-compile(export_all).
-include_lib("wx/include/wx.hrl").
 
start() ->
    State = makeWindow(),
    loop(State),
    wx:destroy().
 
makeWindow() ->
    Server = wx:new(),
    Frame = wxFrame:new(Server, -1, "smoltext"),
    Panel = wxPanel:new(Frame, [{style, ?wxDEFAULT_FRAME_STYLE}]),
    Menu = wxMenuBar:new(),
    wxMenuBar:append(Menu, wxMenu:new(), "Test"),
    wxFrame:setMenuBar(Frame, Menu),
    TextBox = wxStyledTextCtrl:new(Panel, [{style, ?wxTE_MULTILINE}, {style, ?wxTE_DONTWRAP}, {size, wxFrame:getSize(Frame)}]),
    %SIZERS
    TextBoxSizer = wxBoxSizer:new(?wxHORIZONTAL),
    wxSizer:add(TextBoxSizer, TextBox, [{flag, ?wxEXPAND}, {proportion, 1}]),
    wxPanel:setSizer(Panel, TextBoxSizer),
    wxFrame:show(Frame),
    %CONNECTORS
    wxFrame:connect(Frame, close_window),
    %wxFrame:connect(Frame, size),
    wxPanel:connect(Panel, command_button_clicked),
    wxTextCtrl:connect(TextBox, command_text_updated),
    {Frame, Panel, TextBox, TextBoxSizer, self()}.

%makeMenuBar()

loop(State) ->
	{Frame, Panel, TextBox, TextBoxSizer, Pid} = State,
	receive
        #wx{event=#wxClose{}} ->	
			if
				Pid /= self() -> Pid ! { -1 };
				true -> ok
            end,
			io:format("~p Closing window ~n",[self()]),
			wxWindow:destroy(Frame),
			ok;
		%#wx{event=#wxSize{}} ->
		%	io:fwrite("Resizing~n~w~n" ,[wxWindow:getSize(Frame)]),
		%	%wxSizer:setItemMinSize(TextBoxSizer, 0, wxWindow:getSize(Frame)),
		%	wxSizer:setMinSize(TextBoxSizer, wxWindow:getSize(Frame)),
		%	wxSizer:layout(TextBoxSizer),
		%	wxFrame:show(Frame),
		%	loop({Frame, Panel, TextBox, TextBoxSizer, Pid});
        #wx{id = ?wxID_EXIT, event=#wxCommand{type = command_button_clicked} } ->
			if
				Pid /= self() -> Pid ! { -1 };
				true -> ok
			end,
            io:format("~p Closing window ~n",[self()]), %optional, goes to shell
			wxWindow:destroy(Frame),
			ok;  % we exit the loop
		%#wx{id = 22, event=#wxCommand{type = command_text_updated}} ->
		%	io:fwrite("~p~n", [wxTextCtrl:getValue(TextBox)]),
		%	loop(State);
        Msg ->
			io:fwrite("LOOP"),
            loop(State)
 
    end.
