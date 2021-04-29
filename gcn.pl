#!/usr/bin/env -S swipl -q --stack_limit=4m -g main

:- module(gcn, [main/1]).
:- autoload_path(library(http)).
:- use_module(library(persistency)).

:- (not(pack_info(smtp)) -> pack_install(smtp); true), use_module(library(smtp)).
:- persistent commitment(action_id, email, time, ready).
:- db_attach('data/signups.pl', []).

:- dynamic action/2.
:- consult('data/email.pl').
:- retractall(action(_,_)),
   expand_file_name('data/actions/*.pl',Action_files),
   maplist(load_files,Action_files).

serve(Port) :-
  http_server(http_dispatch, [port(Port)]).

:- http_handler(root(.),
	root_handler,[]).

:- http_handler(root(action),
	action_handler(Method),
	[method(Method), methods([get,post])]).

user:file_search_path(static, 'data/static').
:- http_handler(root(.), serve_files_in_directory(static), [prefix]).

root_handler(Request):-
	http_parameters(Request,[
		category(Category, [atom, optional(true)]),
		location(Location, [atom, optional(true)])
		]),
	action_list_page(Category,Location,Page),
	reply_gcn_page(Page).

reply_gcn_page(Page) :-
	reply_html_page([
	    title('Get Courage Now'),
	    \html_requires(root('gcn.css'))],
	  section([h1(a(href(/),'Get Courage Now')),Page])).

action_header(Id,Element) :-
	action(Id, Details),
	member(title(Title),Details),
	member(category(Category),Details),
	member(location(Location),Details),
	http_link_to_id(action_handler, [id(Id)], Action_link),
	http_link_to_id(root_handler, [category(Category)], Category_link),
	http_link_to_id(root_handler, [location(Location)], Location_link),
	Element = header([
	a(href(Action_link),h2(Title)),
	div(class(location),a(href(Location_link),Location)),
	div(class(category),a(href(Category_link),Category))]).

action_body(Id,Element) :-
	action(Id,Details),
	member(description(Description),Details),
	action_progress(Id,Progress),
	action_signup_form(Id,Signup_form),
	Element = div([
		Progress,
		article(p(Description)),
		Signup_form]).

action_progress(Id,Element) :-
	action(Id,Details),
	member(target(Target),Details),
	action_commitments_count(Id, Signups_count),
	Element = p([
			label([Signups_count,' of ',Target,' have committed.']),
			progress([max(Target), value(Signups_count)],[])]).

action_signup_form(Id,Element) :-
	Element = form([action(action),method(post)],fieldset([
		legend('Get involved'),
		div(input([name(email), type(email), required(required), placeholder('my_name@example.com')],[])),
		div(button([id(ready),type(submit), name(ready), value(true)], 'I\'m ready, sign me up now')),
		div(button([id(not_ready),type(submit), name(ready), value(false)], 'I\'m not ready yet')),
		input([name(action), type(hidden), value(Id)],[])
		])).


category_location_actions(Category,Location,Action_list):-
	findall(List_item,
	  (action(Id,Details),
	   member(category(Category), Details),
	   member(location(Location), Details),
	   action_header(Id,Element),
	   List_item = li(Element)
	   ),
	  Action_list).

action_list_page(Category,Location,Page) :-
	category_location_actions(Category,Location,List),
	Page = nav(ul(List)).

action_handler(get, Request):-
	http_parameters(Request,[
		id(Action_id,[])]),
	signup_page(Action_id,Page),
	reply_gcn_page(Page).

action_handler(post, Request):-
	http_parameters(Request,[
		action(Id, [optional(false)]),
		email(Email, [length > 1]),
		ready(Ready, [length > 1])
		]),
	get_time(Now),
	% check that action exists
	action(Id, _),
	assert_commitment(Id, Email, Now, Ready),
	send_signup_confirmation_email(Email,Id),
	notify_everyone_if_ready(Id),
	http_link_to_id(action_handler, [id(Id)], Link),
	Page = div([
	  p('Thanks for signing up!'),
	  % Should show similar actions instead, perhaps
	  p(a(href(Link),'Return to action page.'))]),
	reply_gcn_page(Page).

action_commitments_count(Action_id, Commitments_count) :-
  findall(_Commitment, commitment(Action_id,_Email,_Time,_Ready), Commitments),
  length(Commitments, Commitments_count).

target_reached(Action_id) :-
	action(Action_id, Details),
	member(target(Target), Details),
	action_commitments_count(Action_id, Commitments_count),
	% Commitments_count >= Target.
	% only send notification when the *exact* taget is reached
	Commitments_count = Target.

signup_page(Action_id,Page):-
	action_header(Action_id, Action_header),
	action_body(Action_id, Action_body),
	Page = section([
	Action_header,
	Action_body]).

action_ready_mail_text(Action_id, Out) :-
	action(Action_id, Details),
	member(description(Description), Details),
	format(Out, 'Ready for action. Enough people have signed up:,\n\n', []),
	format(Out, Description, []).

text_stream(Text, Out) :-
% just for sending emails
	format(Out, Text, []).

send_email(Email_to,Subject,Body) :-
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
	action(Action_id, Details),
	member(description(Description), Details),
	string_concat('You signed up for the following action: \n\n',Description,Text),
	send_email(Email_to,Subject,Text).

notify(Email_to,Action_id):-
	Subject = 'Action ready',
	action(Action_id, Details),
	member(description(Description), Details),
	string_concat('The target has been reached for following action: \n\n',Description,Text),
	send_email(Email_to,Subject,Text).

notify_everyone_if_ready(Action_id):-
	target_reached(Action_id),
	findall(
	  Email,
	  (commitment(Action_id, Email, _Time, _Ready),
	    notify(Email,Action_id)),
	  _Emails).

notify_everyone_if_ready(_Action_id):-
	true.

main(_Args):-
	serve(8080).

