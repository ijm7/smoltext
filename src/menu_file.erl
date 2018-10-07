-module(menu_file).
-export([aboutDialog/1,saveFile/2,saveAsFile/2,openFile/2,closeWindow/2]).
-include_lib("wx/include/wx.hrl").

aboutDialog(Frame) ->
	About = wxDialog:new(Frame, ?wxID_ABOUT, "About", [{style, ?wxDEFAULT_DIALOG_STYLE}]),
	AboutText = "Smoltext - Version 0.1\n\nSmoltext is a basic text editor with simple functionality.\nTo report issues or suggest additions, please\ngo to https://github.com/ijm7/smoltext/issues\n\n",
	AboutMessage = wxStaticText:new(About, -1, AboutText, [{style, ?wxALIGN_CENTRE}]),
	OkButton = wxButton:new(About, ?wxID_OK, [{label, "OK"}]),
	AboutSizer = wxBoxSizer:new(?wxVERTICAL),
	{_, VerticalSize} = wxWindow:getSize(About),
	AboutMessagePosition = trunc(VerticalSize/8),
	wxSizer:addSpacer(AboutSizer, AboutMessagePosition),
	wxSizer:add(AboutSizer, AboutMessage, [{flag, ?wxALIGN_CENTRE_HORIZONTAL}, {flag, ?wxALIGN_CENTRE_VERTICAL}]),
	wxSizer:add(AboutSizer, OkButton, [{flag, ?wxALIGN_CENTRE_HORIZONTAL}, {flag, ?wxALIGN_CENTRE_VERTICAL}]),
	wxDialog:setSizer(About, AboutSizer),
	wxDialog:showModal(About),
	ok.

saveFile(TextBox, FileName) ->
	wxStyledTextCtrl:saveFile(TextBox, FileName).

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

closeWindow(Frame, Pid) ->
	if
		Pid /= self() -> Pid ! { -1 };
	true -> ok
end,
io:format("~p Closing window ~n",[self()]),
wxWindow:destroy(Frame),
ok.
