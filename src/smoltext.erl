-module(smoltext).
-compile(export_all).
-include_lib("wx/include/wx.hrl").

start() ->
    State = makeWindow(),
    loop(State),
    wx:destroy(),
    init:stop().

makeWindow() ->
    Server = wx:new(),
    Frame = wxFrame:new(Server, -1, "smoltext"),
    Panel = wxPanel:new(Frame, [{style, ?wxDEFAULT_FRAME_STYLE}]),
    Menu = makeMenuBar(),
    wxFrame:setMenuBar(Frame, Menu),
    StatusBar = wxStatusBar:new(Frame),
    wxFrame:setStatusBar(Frame, StatusBar),
    TextBox = wxStyledTextCtrl:new(Panel, [{style, ?wxTE_MULTILINE}, {style, ?wxTE_DONTWRAP}, {id, 1}, {size, wxFrame:getSize(Frame)}]),
    %SIZERS
    TextBoxSizer = wxBoxSizer:new(?wxHORIZONTAL),
    wxSizer:add(TextBoxSizer, TextBox, [{flag, ?wxEXPAND}, {proportion, 1}]),
    wxPanel:setSizer(Panel, TextBoxSizer),
    wxFrame:show(Frame),
    %CONNECTORS
    wxFrame:connect(Frame, close_window),
    %wxFrame:connect(Frame, size),
    wxPanel:connect(Panel, command_button_clicked),
    wxTextCtrl:connect(TextBox, stc_updateui),
    {Frame, Panel, TextBox, [], self()}.

makeMenuBar() ->
    Menu = wxMenuBar:new(),
    File = wxMenu:new(),
    wxMenu:append(File, 1, "New"),
    wxMenu:append(File, 2, "Open"),
    wxMenu:append(File, 3, "Save"),
    wxMenu:append(File, 4, "Save As"),
    wxMenu:append(File, ?wxID_EXIT, "Quit"),
    %Edit = wxMenu:new(),
    Help = wxMenu:new(),
    wxMenu:append(Help, ?wxID_ABOUT, "About"),
    wxMenuBar:append(Menu, File, "File"),
    %wxMenuBar:append(Menu, Edit, "Edit"),
    wxMenuBar:append(Menu, Help, "Help"),
    wxMenu:connect(Menu, command_menu_selected),
    Menu.

loop(State) ->
    {Frame, _, TextBox, Files, Pid} = State,
    receive
        #wx{event=#wxClose{}} ->
            menu_file:closeWindow(Frame, Pid);
        #wx{id = ?wxID_EXIT, event=#wxCommand{type = command_menu_selected} } ->
            menu_file:closeWindow(Frame, Pid);
        #wx{id = ?wxID_ABOUT, event= #wxCommand{type = command_menu_selected} } ->
            menu_file:aboutDialog(Frame),
            loop(State);
        #wx{event=#wxStyledText{type = stc_updateui}} ->
            updateStatusBar(Frame, TextBox),
            loop(State);
        #wx{id = 1, event=#wxCommand{type = command_menu_selected} } ->
            spawn(fun() -> start() end),
            loop(State);
        #wx{id = 2, event=#wxCommand{type = command_menu_selected} } ->
            FileName = menu_file:openFile(Frame, TextBox),
            AddFile = lists:append([Files, [FileName]]),
            updateTitle(Frame, AddFile),
            loop({Frame, [], TextBox, AddFile, Pid});
        #wx{id = 3, event=#wxCommand{type = command_menu_selected} } ->
            if
                Files /= [] ->
                    menu_file:saveFile(TextBox, Files),
                    loop(State);
                true ->
                    loop(State)
                end;

            #wx{id = 4, event=#wxCommand{type = command_menu_selected} } ->
                FileName = menu_file:saveAsFile(Frame, TextBox),
                AddFile = lists:append([Files, [FileName]]),
                updateTitle(Frame, AddFile),
                loop({Frame, [], TextBox, AddFile, Pid});
            _ ->
                %io:fwrite("~w~n", [Msg]),
                loop(State)
            end.

        updateTitle(Frame, [H|_]) ->
            wxWindow:setLabel(Frame, "smoltext - " ++ filename:basename(H)),
            ok.

        updateStatusBar(Frame, TextBox) ->
            Row = wxStyledTextCtrl:getCurrentLine(TextBox),
            Column = wxStyledTextCtrl:getColumn(TextBox, wxStyledTextCtrl:getCurrentPos(TextBox)),
            StatusText = "line: " ++ integer_to_list(Row + 1) ++ "\tcol: " ++ integer_to_list(Column),
            wxFrame:setStatusText(Frame, StatusText),
            ok.
