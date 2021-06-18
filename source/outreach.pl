% © 2021 Pelle Hjek
% GNU Affero General Public License version 3 or later

:- module(outreach, [action_emails/2,email_mailto/2,list_html/2,outreach/2]).

outreach(Action,Element):-
  action_emails(Action,Emails),
  maplist(email_mailto,Emails,Mailto_links),
  list_html(Mailto_links,Mailto_list),
  Element = fieldset([
  	legend(outreach),
		div([
			h3('direct email to comitted'),
			Mailto_list])
	]).


action_emails(Id,Emails):-
	findall(Email,(commitment(Commitment),member(action(Id),Commitment),member(email(Email),Commitment)),Emails).

list_html(List,ul(Items)) :-
	findall(li(Item),member(Item,List),Items).

email_mailto(Email,code(a(href(Link),Email))):-
	string_concat("mailto:",Email,Link).

% <a href="mailto:someone@example.com?subject=This%20is%20the%20subject&cc=someone_else@example.com&body=This%20is%20the%20body">Send email</a>
% mailto:?subject=This%20is%20the%20subject&bcc=someone_else@example.com&bcc=foo@bar.com&body=This%20is%20the%20body"
%
mailto_link(_,Link):-
	Link= "mailto:?subject=This is the subject&cc=someone_else@example.com&cc=foo@bar.com&body=This is the body".

mailto_link(Mail,Link):-
	member(to(To),Mail),
	member(cc(CC),Mail),
	member(bcc(BCC),Mail),
	member(subject(Subject),Mail),
	member(body(Body),Mail),
	format(atom(Link),"mailto:~s",[To]).

%
%
%mumble://[username[:password]@]<address>[:port]/[channelpath]?version=<serverversion>[&title=<servername>][&url=<serverurl>]
%mailto:
%signal-cli
