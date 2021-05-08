**LibreAction** — A web app for promoting actions allowing for conditional commitment sign-up.

## Testing / development setup
Download [the latest version of LibreAction](https://github.com/hjek/LibreAction/archive/refs/heads/main.zip) and unzip it.

- Mac OS
	* [Disable Gatekeeper](https://osxdaily.com/2012/07/27/app-cant-be-opened-because-it-is-from-an-unidentified-developer/).
	* Double-click on `libreaction.command`.
- Windows
	* [Download SWI Prolog for Windows](https://www.swi-prolog.org/download/stable/bin/swipl-8.2.4-1.x64.exe.envelope)
	* Double-click on `libreaction.pl`
- Debian
	* Install SWI Prolog: `sudo apt install swi-prolog`
	* Run the app: `./libreaction.pl`

Try it out by visiting [localhost:8080](https://localhost:8080) in your web browser.

## Next steps
- Add configuration to `data/config.pl`. See `data.example/config.pl` for reference.
- Create a folder `data/actions/` and add some action files (XML). See `data.example/actions/` for examples.

## License
[GNU AGPLv3 or later](https://www.gnu.org/licenses/agpl-3.0.en.html)  
© Pelle Hjek 2021  
