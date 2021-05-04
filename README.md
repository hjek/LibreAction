# Get Courage Now
A web app to sign up for conditional commitment.

## How do I get started?
This documentation should help you get GCN running on your own computer. If you find the documentation confusing or inadequate, [please send me an email](mailto:hjek@member.fsf.org) or report an issue here.

## Running the app
First download [the latest version of GCN](https://github.com/hjek/get-courage-now/archive/refs/heads/main.zip) and unzip it. Then:
### Mac OS
* Double-click on `gcn.command`.
### Windows
* [Download SWI Prolog for Windows](https://www.swi-prolog.org/download/stable/bin/swipl-8.2.4-1.x64.exe.envelope)
* Double-click on `gcn.pl`
### Debian
* Install SWI Prolog: `sudo apt install swi-prolog`
* Run the app: `./gcn.pl`

## Trying the app
- You can try the web app [here](https://localhost:8080) and sign up for example actions.

## Next steps
- Add configuration to `data/config.pl`. See `data.example/config.pl` for reference.
- Create a folder `data/actions/` and create action files, such as `data/actions/my_action.pl`. See `data.example/actions/` for reference.

## License
[GNU AGPLv3 or later](https://www.gnu.org/licenses/agpl-3.0.en.html)  
© Pelle Hjek 2021
