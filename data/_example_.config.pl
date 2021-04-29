:- module(email, [email/3, site/1]).

% Please fill in your email configuration and save as email.pl

email(
   smtp('smtp.example.com'), % host
   from('hello@example.com'), % from
   auth('hello@example.com'-'1234') % user-password
    ).

site('example.com'). % name of the site
