-module(smoltext).
-compile(export_all).
-include_lib("wx/include/wx.hrl").

start(Args) ->
  if
    Args == "bin" ->
      KillPid = spawn(fun() -> vmKill(1) end),
      spawn(fun() -> newWindow(KillPid) end);
    true ->
      exit("Incorrect argument provided")
    end.

start() ->
  spawn(fun() -> newWindow(self()) end).

newWindow(KillPid) ->
  State = makeWindow(),
  {Frame, TextBox, Files, Pid} = State,
  BuildState = {Frame, TextBox, Files, Pid, KillPid},
  loop(BuildState).

vmKill(Count) ->
  receive
    Msg ->
      NewCount = Count + Msg,
      if
        NewCount < 1 ->
          init:stop();
        true ->
          vmKill(NewCount)
        end
      end.

makeWindow() ->
  Server = wx:new(),
  Frame = wxFrame:new(Server, -1, "smoltext"),
  Panel = wxPanel:new(Frame, [{style, ?wxDEFAULT_FRAME_STYLE}]),
  Menu = makeMenuBar(),
  wxFrame:setMenuBar(Frame, Menu),
  StatusBar = wxStatusBar:new(Frame),
  wxFrame:setStatusBar(Frame, StatusBar),
  TextBox = wxStyledTextCtrl:new(Panel, [{style, ?wxTE_MULTILINE}, {style, ?wxTE_DONTWRAP}, {id, 1}, {size, wxFrame:getSize(Frame)}]),
  wxStyledTextCtrl:setMarginWidth(TextBox, 0, 16),
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
  updateStatusBar(Frame, TextBox),
  {Frame, TextBox, [], self()}.

makeMenuBar() ->
  Menu = wxMenuBar:new(),
  File = wxMenu:new(),
  wxMenu:append(File, 1, "New"),
  wxMenu:append(File, 2, "Open"),
  wxMenu:append(File, 3, "Save"),
  wxMenu:append(File, 4, "Save As"),
  wxMenu:append(File, ?wxID_EXIT, "Quit"),
  Edit = wxMenu:new(),
  wxMenu:append(Edit, 5, "Undo"),
  wxMenu:append(Edit, 6, "Redo"),
  wxMenu:append(Edit, 7, "Cut"),
  wxMenu:append(Edit, 8, "Copy"),
  wxMenu:append(Edit, 9, "Paste"),
  wxMenu:append(Edit, 10, "Select All"),
  %Tools = wxMenu:new(),
  %View = wxMenu:new(),
  %Options = wxMenu:new(),
  Help = wxMenu:new(),
  wxMenu:append(Help, ?wxID_ABOUT, "About"),
  wxMenuBar:append(Menu, File, "File"),
  wxMenuBar:append(Menu, Edit, "Edit"),
  %wxMenuBar:append(Menu, Tools, "Tools"),
  %wxMenuBar:append(Menu, View, "View"),
  %wxMenuBar:append(Menu, Options, "Options"),
  wxMenuBar:append(Menu, Help, "Help"),
  wxMenu:connect(Menu, command_menu_selected),
  Menu.

loop(State) ->
  {Frame, TextBox, Files, Pid, KillPid} = State,
  receive
    #wx{event=#wxClose{}} ->
      menu_file:closeWindow(Frame, Pid),
      KillPid ! -1;
    #wx{id = ?wxID_EXIT, event=#wxCommand{type = command_menu_selected} } ->
      menu_file:closeWindow(Frame, Pid),
      KillPid ! -1;
    #wx{id = ?wxID_ABOUT, event= #wxCommand{type = command_menu_selected} } ->
      menu_file:aboutDialog(Frame),
      loop(State);
    #wx{event=#wxStyledText{type = stc_updateui}} ->
      updateStatusBar(Frame, TextBox),
      loop(State);
    #wx{id = 1, event=#wxCommand{type = command_menu_selected} } ->
      spawn(fun() -> newWindow(KillPid) end),
      KillPid ! 1,
      loop(State);
    #wx{id = 2, event=#wxCommand{type = command_menu_selected} } ->
      FileName = menu_file:openFile(Frame, TextBox),
      if
        FileName /= "" ->
          AddFile = lists:append([Files, [FileName]]),
          updateTitle(Frame, AddFile),
          loop({Frame, TextBox, AddFile, Pid, KillPid});
        true ->
          loop(State)
        end;
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
      if
        FileName /= "" ->
          AddFile = lists:append([Files, [FileName]]),
          updateTitle(Frame, AddFile),
          loop({Frame, TextBox, AddFile, Pid, KillPid});
        true ->
          loop(State)
        end;
      #wx{id = 5, event=#wxCommand{type = command_menu_selected} } ->
        wxStyledTextCtrl:undo(TextBox),
        loop(State);
      #wx{id = 6, event=#wxCommand{type = command_menu_selected} } ->
        wxStyledTextCtrl:redo(TextBox),
        loop(State);
      #wx{id = 7, event=#wxCommand{type = command_menu_selected} } ->
        wxStyledTextCtrl:cut(TextBox),
        loop(State);
      #wx{id = 8, event=#wxCommand{type = command_menu_selected} } ->
        wxStyledTextCtrl:copy(TextBox),
        loop(State);
      #wx{id = 9, event=#wxCommand{type = command_menu_selected} } ->
        wxStyledTextCtrl:paste(TextBox),
        loop(State);
      #wx{id = 10, event=#wxCommand{type = command_menu_selected} } ->
        wxStyledTextCtrl:selectAll(TextBox),
        loop(State);
    _ ->
      loop(State)
      end.

updateTitle(Frame, [H|_]) ->
  wxWindow:setLabel(Frame, "smoltext - " ++ filename:basename(H)),
  ok.

updateStatusBar(Frame, TextBox) ->
  Row = wxStyledTextCtrl:getCurrentLine(TextBox),
  Column = wxStyledTextCtrl:getColumn(TextBox, wxStyledTextCtrl:getCurrentPos(TextBox)),
  StatusText = "line: " ++ integer_to_list(Row + 1) ++ "    column: " ++ integer_to_list(Column),
  wxFrame:setStatusText(Frame, StatusText),
  ok.
