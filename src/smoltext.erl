-module(smoltext).
-compile(export_all).
-include_lib("wx/include/wx.hrl").
 
start() ->
    State = make_window().
    loop (State),
    wx:destroy().
 
make_window() ->
    Server = wx:new(),
    Frame = wxFrame:new(Server, -1, "smoltext"),
    wxTopLevelWindow:maximize(Frame),
    Panel = wxPanel:new(Frame),
    Entry = wxTextCtrl:new(Panel, -1, [{style, ?wxTE_MULTILINE}]),
