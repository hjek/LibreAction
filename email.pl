:- module(email, [send_email/3]).
:- (not(pack_info(smtp)) -> pack_install(smtp); true), use_module(library(smtp)).

:- use_module(library(xpath)).

email_config(Host,From,Password) :-
  load_xml('email.xml',DOM,[]),
  xpath(DOM, //host(content), [Host]),
  xpath(DOM, //from(content), [From]),
  xpath(DOM, //password(content), [Password]).

text_stream(Text, Out) :-
% hack alert: the smtp pack apparently only accepts a closure for the email body
	format(Out, Text, []).

send_email(Email_to,Subject,Body) :-
	exists_file('email.xml'),
	email_config(Host,From,Password),
	smtp_send_mail(
	  Email_to,
	  text_stream(Body),
	  [subject(Subject),
	   from(From),
	   smtp(Host),
	   auth(From-Password),
	   auth_method(login),
	   security(starttls)
	  ]),!.

% if there's no email config available, then it's fine anyway
send_email(_,_,_).
