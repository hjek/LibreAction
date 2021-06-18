:- module(admin, [set_password/0,ensure_password/0]).
:- use_module(act_now).
:- use_module(outreach).
:- use_module(docs).
:- autoload_path(library(http)).

:- http_handler(root(admin),
	admin_handler(Method),
	[method(Method), methods([get,post]),
	authentication(basic(pw,admin))]).

ensure_password():-
% 	BUG: Has to be set on every run, or get "Forbidden"
% 	Somehow passwords are not loaded properly by this?
	exists_file(pw),
	http_read_passwd_file(pw, _Data).

ensure_password():-
	set_password.

set_password():-
	writeln("Please enter password for admin account:"),
	current_input(In), read_line_to_string(In,Password),
	crypt(Password,Hash_codes), format(atom(Hash),'~s',[Hash_codes]),
	http_write_passwd_file(pw, [passwd('',Hash,[])]).

admin_handler(get,Request):-
	http_parameters(Request,[
		id(Action, [atom, optional(true)])
		]),
	editor(Action,Element),
	reply_action_page(Element).

admin_handler(post,Request):-
	http_parameters(Request,[
		id(Action, [atom]),
		content(Content, [atom]),
		preview(Preview, [atom, optional(true)])
		]),
	(ground(Preview) -> reply_preview(Request,Action,Content) ; reply_edit(Request,Action,Content)).

reply_preview(_Request,_Action,Content):-
% PREVIEW
	open_string(Content,Stream),
	load_xml(stream(Stream),DOM,[]),
	reply_action_page(DOM).


reply_edit(Request,Action,Content):-
% EDIT -> SAVE
 	make_directory_path('~/actions'),
	action_id(Path,Action),
	open(Path,write,Stream),
	write(Stream,Content),
	close(Stream),
	http_link_to_id(action_handler, [id(Action)], Edit_link),
	http_redirect(moved, Edit_link, Request).

editor(Action,Element):-
% edit existing action
	ground(Action),
	outreach(Action,Outreach_element),
	action_header(Action,Header),
	action_id(Path,Action),
	exists_file(Path),
	read_file_to_string(Path,String,[]),
	docs('new action',Docs),
	Element = div([
	Header,
	Outreach_element,
	fieldset([legend('edit action'),form([method(post),action(admin)],[
		input([hidden(hidden),name(id),value(Action)]),
		textarea([id(editor),name(content),required(required)],String),
		details([summary("Editing an action..."),Docs]),
		% preview doesn't quite work yet. action viewing is very file-based.
%		input([type(submit),name(preview),formtarget('_blank'),value(preview)]),
		button(b(update))])])]).

editor(_Action,Element):-
% create new action
	read_file_to_string('source/action.template.xml',String,[]),
	uuid(Id),
	docs('new action',Docs),
	Element = fieldset([legend('new action'),form([method(post),action(admin)],[
		p([
			label("action ID (must be unique)"),
			input([required(required),name(id),value(Id)])
			]),
		textarea([name(content),required(required)],String),
		details([summary("Creating a new action..."),Docs]),
		button(b(create))])]).
