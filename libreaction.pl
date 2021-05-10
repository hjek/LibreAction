#!/usr/bin/env -S swipl -q --stack_limit=4m -g main

:- module(libreaction, [main/1,action/3]).
:- autoload_path(library(http)).
:- use_module(library(persistency)).
:- use_module(library(xpath)).
:- (not(pack_info(smtp)) -> pack_install(smtp); true), use_module(library(smtp)).


 %%%%%%%%%
 % MODEL %
 %%%%%%%%%
 
%% database location has to be fixed
:- persistent commitment(details).
:- db_attach('db.pl', []).

% app configuration
actions_dir(actions) :-
	exists_directory(actions),!.
% if 'data' doesn't exist, use 'data.example'
actions_dir('actions.example').


action_id(File,Id) :-
  actions_dir(Actions_dir),
  swritef(Glob,'./%w/*.xml', [Actions_dir]),
  expand_file_name(Glob, Files),
  member(File,Files),
  file_base_name(File,File_base),
  file_name_extension(Id, 'xml', File_base).

action(Id,Details,Description) :-
% parse the actions from XML
  action_id(File,Id),
  load_xml(File,DOM,[]),
  xpath(DOM, //action, element(action, Details, Action)),
  xpath(Action, //description, element(description, _, Description)).

action_role(Id,Role) :-
  action_id(File,Id),
  load_xml(File,DOM,[]), xpath(DOM, //role(content), [Role]).

action_need(Id,Need) :-
  action_id(File,Id),
  load_xml(File,DOM,[]), xpath(DOM, //need(content), [Need]).

action_commitments_count(Action_id, Commitments_count) :-
  findall(_Commitment, (commitment(Details), member(action(Action_id),Details)), Commitments),
  length(Commitments, Commitments_count).

target_reached(Action_id) :-
  action(Action_id, Details, _Description),
  member(target=Target, Details),
  action_commitments_count(Action_id, Commitments_count),
  % Commitments_count >= Target.
  % only send notification when the *exact* taget is reached
  Commitments_count = Target.

 %%%%%%%%%%%%%%%%%%%%%
 % VIEW / CONTROLLER %
 %%%%%%%%%%%%%%%%%%%%%

:- http_handler(root(.),
	root_handler,[]).

:- http_handler(root(action),
	action_handler(Method),
	[method(Method), methods([get,post])]).

user:file_search_path(static, static).
:- http_handler(root(.), serve_files_in_directory(static), [prefix]).


root_handler(Request):-
	http_parameters(Request,[
		category(Category, [atom, optional(true)]),
		location(Location, [atom, optional(true)])
		]),
	action_list_page(Category,Location,Page),
	reply_action_page(Page).

reply_action_page(Page) :-
	reply_html_page([
	    title('LibreAction'),
	    \html_requires(root('default.css'))],
	  section([h1(a(href(/),'LibreAction')),Page])).

action_role_select(Id, Element) :-
	Role_label = 'I\'d like to help out with ',
	action_role(Id,_Role), !,
	findall(li([input([type(radio),name(role),value(Role)],[]),label(Role)]),action_role(Id,Role),Roles),
	Element = label([p(Role_label), ul(Roles)]).

% if no roles are defined
action_role_select(_Id, p([])).

action_need_select(Id, Element) :-
	Need_label = 'Which of the following would support you sufficiently to be ready?',
	action_role(Id,_Need), !,
	findall(li([input([type(checkbox),name(need),value(Need)],[]),label(Need)]),action_need(Id,Need),Needs),
	Element = label([p(Need_label), ul(Needs)]).

% if no needs are defined
action_need_select(_Id, p([])).

action_header(Id,Element) :-
	action(Id, Details, _Description),
	member(title=Title,Details),
	member(category=Category,Details),
	member(location=Location,Details),
	http_link_to_id(action_handler, [id(Id)], Action_link),
	http_link_to_id(root_handler, [category(Category)], Category_link),
	http_link_to_id(root_handler, [location(Location)], Location_link),
	action_progress(Id,Progress),
	Element = header(class(action),[
	h2(a(href(Action_link),Title)),
	div([
		span(class(location),a(href(Location_link),Location)), ' | ',
		span(class(category),a(href(Category_link),Category))]),
	Progress]).

action_body(Id,Element) :-
	action(Id,_Details,Description),
	action_signup_form(Id,Signup_form),
	Element = div([
		article(p(Description)),
		Signup_form]).

action_share(Id,Element) :-
	site(Site),
	http_link_to_id(action_handler, [id(Id)], Action_link),
	action(Id,Details,_Description),
	member(title=Title,Details),
	swritef(Mailto_link,'mailto:?subject=%w&body=%w%w',[Title,Site,Action_link]),
	Element = p(class(share),a(href(Mailto_link), button('Invite friends to attend'))).

action_progress(Id,Element) :-
	action(Id,Details,_Description),
	member(target=Target,Details),
	action_commitments_count(Id, Signups_count),
	Element = div([
			small(label([Signups_count,' of ',Target,' signed up.'])),
			progress([max(Target), value(Signups_count)],[])]),!.

action_progress(Id,Element):-
% If there's no target for signups.
	action_commitments_count(Id, Signups_count),
	Element = div([small(label([Signups_count,' signed up.']))]).

action_signup_form(Id,Element) :-
	http_link_to_id(action_handler, [id(Id),ready('not_yet')], Not_ready_link),
	Element = form([action(action),method(post)],fieldset([
		legend('Get involved'),
		input([name(action), type(hidden), value(Id)],[]),
		p(input([name(email), type(email), required(required), placeholder('my_name@example.com')],[])),
		p(button([class(ready),type(submit), name(ready), value(true)], 'I\'m ready, sign me up now')),
		p(a(href(Not_ready_link), button(form(none),'I\'m not ready yet')))])).

category_location_actions(Category,Location,Action_list):-
	findall(List_item,
	  (action(Id,Details,_Description),
	   member(category=Category, Details),
	   member(location=Location, Details),
	   action_header(Id,Element),
	   List_item = li(Element)
	   ),
	  Action_list).

active_filter(Category,_Location,Filter):-
	ground(Category),
	Filter=p(['Category: ', Category]).

active_filter(_Category,Location,Filter):-
	ground(Location),
	Filter=p(['Location: ', Location]).

active_filter(_,_,p([])).

action_list_page(Category,Location,Page) :-
	active_filter(Category,Location,Filter),
	category_location_actions(Category,Location,List),
	Page = div([Filter,nav(ul(List))]).

not_ready_page(Id,Page):-
	action_header(Id, Action_header),
	action_need_select(Id, Need_select),
	http_link_to_id(action_handler, [id(Id),ready('support')], Support_link),
	Support_label = 'I cannot join regardless but can support in other ways',
	Page = div([
		Action_header,
		form([action(action),method(post)],
			fieldset([legend('I\'m not ready yet'),
			input([name(action), type(hidden), value(Id)],[]),
			p(Need_select),
			p(input([name(email), type(email), required(required), placeholder('my_name@example.com')],[])),
			p(button([class(ready),type(submit), name(ready), value(true)], 'Sign me up now')),
			p(a(href(Support_link), button(form(none),Support_label)))
			]))]).

support_page(Id,Page):-
	action_header(Id, Action_header),
	action_role_select(Id, Role_select),
	Page = div([
		Action_header,
		form([action(action),method(post)],
			fieldset([legend('Supporting roles'),
			p(Role_select),
			input([name(action), type(hidden), value(Id)],[]),
			p(input([name(email), type(email), required(required), placeholder('my_name@example.com')],[])),
			p(button([class(ready),type(submit), name(ready), value(true)], 'Sign me up for support'))
			]))]).



action_handler(get, Request):-
% can't join
	http_parameters(Request,[
		id(Action_id,[]),
		ready(Ready,[optional(true)])]),
	ground(Ready),
	Ready = 'support',
	support_page(Action_id,Page),
	reply_action_page(Page).

action_handler(get, Request):-
% not ready yet
	http_parameters(Request,[
		id(Action_id,[]),
		ready(Ready,[optional(true)])]),
	ground(Ready),
	Ready = 'not_yet',
	not_ready_page(Action_id,Page),
	reply_action_page(Page).

action_handler(get, Request):-
% initial action view
	http_parameters(Request,[
		id(Action_id,[])]),
	signup_page(Action_id,Page),
	reply_action_page(Page).

action_handler(post, Request):-
% sign up!
	http_parameters(Request,[
		action(Id, [optional(false)]),
		email(Email, [length > 1]),
		ready(Ready, [length > 1]),
		need(Need, [list(atom)]),
		role(Role, [list(atom)])
		]),
	get_time(Now),
	% check that action exists
	action(Id, _Details, _Decription),
	% prevent duplicate signups
	(commitment(Details),
	 member(email(Email),Details),
	 member(action(Id),Details) ->
	   retract_commitment(Details); true),
	assert_commitment([
	  action(Id),
	  email(Email),
	  time(Now),
	  ready(Ready),
	  need(Need),
	  role(Role)]),
	action_share(Id,Share),
	send_signup_confirmation_email(Email,Id),
	notify_everyone_if_ready(Id),
	http_link_to_id(action_handler, [id(Id)], Link),
	Page = div([
	  p('Thanks for signing up!'),
	  Share,
	  % Should show similar actions instead, perhaps
	  p(a(href(Link),'Return to action page.'))]),
	reply_action_page(Page).

signup_page(Action_id,Page):-
	action_header(Action_id, Action_header),
	action_body(Action_id, Action_body),
	Page = section([
	Action_header,
	Action_body]).

action_ready_mail_text(Action_id, Out) :-
	action(Action_id, _Details, Description),
	format(Out, 'Ready for action. Enough people signed up:,\n\n', []),
	format(Out, Description, []).

text_stream(Text, Out) :-
% just for sending emails
	format(Out, Text, []).

send_email(Email_to,Subject,Body) :-
	clause(email(_,_,_),_),
	email(smtp(SMTP),from(From),auth(Password)),
	thread_create(
		smtp_send_mail(
		  Email_to,
		  text_stream(Body),
		  [subject(Subject),
		   from(From),
		   smtp(SMTP),
		   auth(Password),
		   auth_method(login),
		   security(starttls)
		  ]),
		_Thread),!.

send_email(_,_,_) :-
% if email is not set up
  true.

send_signup_confirmation_email(Email_to,Action_id) :-
	Subject = 'Signed up',
	action(Action_id, Details, _Description),
	member(title=Title, Details),
	string_concat('You signed up for the following action: \n\n',Title,Text),
	send_email(Email_to,Subject,Text).

notify(Email_to,Action_id):-
	Subject = 'Action ready',
	action(Action_id, Details, _Description),
	member(title=Title, Details),
	string_concat('The target has been reached for following action: \n\n',Title,Text),
	send_email(Email_to,Subject,Text).

notify_everyone_if_ready(Action_id):-
	target_reached(Action_id),
	findall(
	  Email,
	  (commitment(Details),
	   member(id(Action_id),Details),
	   member(email(Email),Details),
	   notify(Email,Action_id)),
	  _Emails).

notify_everyone_if_ready(_Action_id):-
	true.

 %%%%%%%%
 % MAIN %
 %%%%%%%%

main(Argv):-
	argv_options(Argv, _RestArgv, Options),
	(member(port(Port),Options) -> true; Port=8080),
	writef("Serving on :%w.\n", [Port]),
	consult('config.example.pl'),
	http_server(http_dispatch, [port(Port)]).

