#!/usr/bin/env -S swipl -q --stack_limit=4m -g main

% © 2021 Pelle Hjek
% GNU Affero General Public License version 3 or later

:- module(act_now, [main/1,reply_action_page/1,actions_dir/1,action_id/2,commitment/1,action_header/2]).
:- autoload_path(library(http)).
:- use_module(library(persistency)).
:- use_module(library(xpath)).
:- use_module(email).
:- use_module(admin).

 %%%%%%%%%
 % MODEL %
 %%%%%%%%%
 
%% database location has to be fixed
:- persistent commitment(details).
:- db_attach('db.pl', []).

% app configuration
actions_dir(Dir) :-
	Dir = 'actions',
	exists_directory(Dir),!.
% if 'data' doesn't exist, use 'data.example'
actions_dir('actions.example').

action_id(Path,Id) :-
% create a filepath for a given Id,
  ground(Id),!,
  actions_dir(Actions_dir),
  file_name_extension(Id, 'xml', File),
  directory_file_path(Actions_dir,File,Path).

action_id(File,Id) :-
  actions_dir(Actions_dir),
  swritef(Glob,'./%w/*.xml', [Actions_dir]),
  expand_file_name(Glob, Files),
  member(File,Files),
  file_base_name(File,File_base),
  file_name_extension(Id, 'xml', File_base).

action_attribute_value(Id,Detail,Value):-
  action_id(File,Id),
  load_xml(File,DOM,[]),
  xpath(DOM, //action, element(action, Details, _Action)),
  member(Detail=Value,Details).

action_element_content(Id,Element,Content):-
  action_id(File,Id),
  load_xml(File,DOM,[]),
  xpath(DOM, //action, element(action, _Details, Action)),
  xpath(Action, //Element, element(Element, _Attributes, Content)).

action_commitments_count(Action_id, Commitments_count) :-
  findall(_Commitment, (commitment(Details), member(action(Action_id),Details)), Commitments),
  length(Commitments, Commitments_count).

target_reached(Action_id) :-
  action_attribute_value(Action_id,target,Target),
  action_commitments_count(Action_id, Commitments_count),
  % Commitments_count >= Target.
  % only send notification when the *exact* taget is reached
  Commitments_count = Target.

 %%%%%%%%%%%%%%%%%%%%%
 % VIEW / CONTROLLER %
 %%%%%%%%%%%%%%%%%%%%%

csp(Content, Policy) -->
  html(meta([
    'http-equiv'('Content-Security-Policy'),
    content([Content, Policy])])).

:- http_handler(root(.),
	root_handler,[]).

:- http_handler(root(action),
	action_handler(Method),
	[method(Method), methods([get,post])]).

user:file_search_path(www, www).
:- http_handler(root(.), serve_files_in_directory(www), [prefix]).


root_handler(Request):-
	http_parameters(Request,[
		category(Category, [atom, optional(true)]),
		location(Location, [atom, optional(true)])
		]),
	action_list_page(Category,Location,Page),
	reply_action_page(Page).

reply_action_page(Page) :-
	reply_html_page(
		[title('Act Now'),
		% don't include resources from other sites
		\csp('default-src', "'self' 'unsafe-inline' data:;"),
		\csp('style-src', "'self' 'unsafe-inline';"),
		% disable javascript ;)
		\csp('script-src', "'none';"),
		\csp('img-src', "'self' data:;"),
		\html_requires(root('default.css'))],
		div([h1(a(href('/'),"Act Now")),section(Page)])).

action_role_select(Id, Element) :-
	Role_label = 'I\'d like to help out with ',
	action_element_content(Id,role,_Content), !,
	findall(li(label([input([type(radio),name(role),value(Role)],[]),Role])),action_element_content(Id,role,[Role]),Roles),
	Element = p([p(Role_label), ul(Roles)]).

% if no roles are defined
action_role_select(_Id, p([])).

action_need_select(Id, Element) :-
	Need_label = 'Which of the following would support you sufficiently to be ready?',
	action_element_content(Id,need,_Content), !,
	findall(li(label([input([type(checkbox),name(need),value(Need)],[]),Need])),action_element_content(Id,need,[Need]),Needs),
	Element = p([p(Need_label), ul(Needs)]).

% if no needs are defined
action_need_select(_Id, p([])).

action_header(Id,Element) :-
	action_attribute_value(Id,title,Title),
	action_attribute_value(Id,category,Category),
	action_attribute_value(Id,location,Location),
	http_link_to_id(action_handler, [id(Id)], Action_link),
	http_link_to_id(root_handler, [category(Category)], Category_link),
	http_link_to_id(root_handler, [location(Location)], Location_link),
	http_link_to_id(admin_handler, [id(Id)], Admin_link),
	action_progress(Id,Progress),
	Element = header(class(action),[
	h2(a(href(Action_link),Title)),
	div([
		span(class(location),a(href(Location_link),Location)), ' | ',
		span(class(category),a(href(Category_link),Category)), ' | ',
		span([class(admin),title(edit)],a(href(Admin_link),'~'))]),
	Progress]).

action_body(Id,Element) :-
	action_element_content(Id,description,Description),
	action_signup_form(Id,Signup_form),
	Element = div([
		article(p(Description)),
		Signup_form]).

% Todo: Read domain. Maybe it can be read from the requests?
site(localhost).

action_share(Id,Element) :-
	site(Site),
	http_link_to_id(action_handler, [id(Id)], Action_link),
	action_attribute_value(Id,title,Title),
	swritef(Mailto_link,'mailto:?subject=%w&body=%w%w',[Title,Site,Action_link]),
	Element = p(class(share),a(href(Mailto_link), 'Invite friends to attend')).

action_progress(Id,Element) :-
	action_attribute_value(Id,target,Target),
	action_commitments_count(Id, Signups_count),
	Element = div([
			small(label([Signups_count,' signed up out of minimum ',Target,'.'])),
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
	  (action_attribute_value(Id,category,Category),
	   action_attribute_value(Id,location,Location),
	   action_header(Id,Element),
	   List_item = li(Element)),
	  Action_list).

active_filter(Category,_Location,Filter):-
	ground(Category),
	Filter=div(['Category: ', Category]).

active_filter(_Category,Location,Filter):-
	ground(Location),
	Filter=div(['Location: ', Location]).

active_filter(_,_,div([])).

action_list_page(Category,Location,Page) :-
	active_filter(Category,Location,Filter),
	category_location_actions(Category,Location,List),
	http_link_to_id(admin_handler, [], New_link),
	Page = div([Filter,nav(ul(List)),div([class(admin),title(new)],a(href(New_link),'+'))]).

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
	action_id(_File,Id),
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

send_signup_confirmation_email(Email_to,Action_id) :-
	Subject = 'Signed up',
	action_attribute_value(Action_id,title,Title),
	string_concat('You signed up for the following action: \n\n',Title,Text),
	send_email(Email_to,Subject,Text).

notify(Email_to,Action_id):-
	Subject = 'Action ready',
	action_attribute_value(Action_id,title,Title),
	string_concat('The target has been reached for following action: \n\n',Title,Text),
	thread_create(send_email(Email_to,Subject,Text),_Thread).

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
	http_server(http_dispatch, [port(Port)]),
	set_password().
