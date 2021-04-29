# Get Courage Now
A web app to sign up for conditional commitment.

## How do I get started?
This documentation should help you get GCN running on your own computer. If you find the documentation confusing or inadequate, [please send me an email](mailto:hjek@member.fsf.org) or report an issue here.
## Running the app
- Install SWI Prolog:
	* On Debian/Ubuntu: `sudo apt install swi-prolog`
	* [Download SWI Prolog for Mac OS](https://www.swi-prolog.org/download/stable/bin/swipl-8.2.4-1.x86_64.dmg)
	* [Download SWI Prolog for Windows](https://www.swi-prolog.org/download/stable/bin/swipl-8.2.4-1.x64.exe.envelope)
- Download [the latest version of GCN](https://github.com/hjek/get-courage-now/archive/refs/heads/main.zip) and unzip it.
- Double-click on `gcn.pl` (or run it in your terminal: `./gcn.pl`).
- **That's it!** You can view the web app [here](https://localhost:8080) and sign up for example actions.

## Next steps
- Add configuration to `data/config.pl`. See `data/_example_.config.pl` for reference.
- Create a folder `data/actions` and create action files, e.g. `data/actions/my_action.pl`. See `data/_example_.actions` for reference.

## License
[GNU AGPLv3 or later](https://www.gnu.org/licenses/agpl-3.0.en.html)  
© Pelle Hjek 2021
