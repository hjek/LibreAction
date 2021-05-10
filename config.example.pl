:- module(email, [email/3, site/1]).

email(
   % host
   smtp('localhost'),
   % from
   from('user@localhost'),
   % user-password
   auth('user@localhost'-'1234')
    ).

% name of the site
site('localhost').
