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
    Menu = makeMenuBar(),
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
    {Frame, Panel, TextBox, [], self()}.

makeMenuBar() ->
	Menu = wxMenuBar:new(),
	File = wxMenu:new(),
	wxMenu:append(File, 1, "New"),
	wxMenu:append(File, 2, "Open"),
	wxMenu:append(File, 3, "Save"),
	wxMenu:append(File, 4, "Save As"),
	wxMenu:append(File, ?wxID_EXIT, "Quit"),
	Edit = wxMenu:new(),
	Help = wxMenu:new(),
	wxMenu:append(Help, 1, "About"),
    wxMenuBar:append(Menu, File, "File"),
    wxMenuBar:append(Menu, Edit, "Edit"),
    wxMenuBar:append(Menu, Help, "Help"),
    wxMenu:connect(File, command_menu_selected),
    Menu.

loop(State) ->
	{Frame, _, TextBox, Files, Pid} = State,
	receive
        #wx{event=#wxClose{}} ->	
			closeWindow(Frame, Pid);
        #wx{id = ?wxID_EXIT, event=#wxCommand{type = command_menu_selected} } ->
			closeWindow(Frame, Pid);
		%#wx{id = 22, event=#wxCommand{type = command_text_updated}} ->
		%	io:fwrite("~p~n", [wxTextCtrl:getValue(TextBox)]),
		%	loop(State);
		#wx{id = 2, event=#wxCommand{type = command_menu_selected} } ->
			FileName = openFile(Frame, TextBox),
			AddFile = lists:append([Files, [FileName]]),
			updateTitle(Frame, AddFile),
			loop({Frame, [], TextBox, AddFile, Pid});
		#wx{id = 3, event=#wxCommand{type = command_menu_selected} } ->
			if 
				Files /= [] ->
					saveFile(TextBox, Files),
					loop(State);
				true ->
					loop(State)
				end;
			
		#wx{id = 4, event=#wxCommand{type = command_menu_selected} } ->
			FileName = saveAsFile(Frame, TextBox),
			AddFile = lists:append([Files, [FileName]]),
			updateTitle(Frame, AddFile),
			loop({Frame, [], TextBox, AddFile, Pid});
        _ ->
			io:fwrite("LOOP"),
            loop(State)
    end.

closeWindow(Frame, Pid) ->
	if
		Pid /= self() -> Pid ! { -1 };
		true -> ok
    end,
    io:format("~p Closing window ~n",[self()]),
	wxWindow:destroy(Frame),
	ok.

saveFile(TextBox, [H|_]) ->
	wxStyledTextCtrl:saveFile(TextBox, H).

saveAsFile(Frame, TextBox) ->
	SaveFileDialog = wxFileDialog:new(Frame, [{style, ?wxFD_SAVE}]),
	ReturnCode = wxDialog:showModal(SaveFileDialog),
	if 
		ReturnCode == ?wxID_OK ->
			SavePath = wxFileDialog:getPath(SaveFileDialog),
			wxStyledTextCtrl:saveFile(TextBox, SavePath),
			SavePath;
		true -> 
			""
	end.
	
openFile(Frame, TextBox) ->
	OpenFileDialog = wxFileDialog:new(Frame, [{style, ?wxFD_OPEN}]),
	ReturnCode = wxDialog:showModal(OpenFileDialog),
	if 
		ReturnCode == ?wxID_OK ->
			LoadPath = wxFileDialog:getPath(OpenFileDialog),
			wxStyledTextCtrl:loadFile(TextBox, LoadPath),
			LoadPath;
		true -> 
			""
	end.

updateTitle(Frame, [H|_]) ->
	wxWindow:setLabel(Frame, "smoltext - " ++ filename:basename(H)),
	ok.
