% © 2021 Pelle Hjek
% GNU Affero General Public License version 3 or later

:- module(docs, [docs/2]).

docs('new action',div([
	p('Extended help...'),
	p(a(href('https://github.com/glacambre/firenvim'),'Firenvim'))
])).
