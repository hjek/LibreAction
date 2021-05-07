A web app for promoting actions allowing for conditional commitment sign-up.

## How do I get started?
This documentation should help you get LibreAction running on your own computer. If you find the documentation confusing or inadequate, [please send me an email](mailto:hjek@member.fsf.org) or report an issue here.

## Testing / development setup
First download [the latest version of LibreAction](https://github.com/hjek/get-courage-now/archive/refs/heads/main.zip) and unzip it.

#### Mac OS
* [Disable Gatekeeper](https://osxdaily.com/2012/07/27/app-cant-be-opened-because-it-is-from-an-unidentified-developer/).
* Double-click on `libreaction.command`.
#### Windows
* [Download SWI Prolog for Windows](https://www.swi-prolog.org/download/stable/bin/swipl-8.2.4-1.x64.exe.envelope)
* Double-click on `libreaction.pl`
#### Debian
* Install SWI Prolog: `sudo apt install swi-prolog`
* Run the app: `./libreaction.pl`

### Trying it out
- Open [localhost:8080](https://localhost:8080) in your web browser and you're ready to sign up for example actions.

## Next steps
- Add configuration to `data/config.pl`. See `data.example/config.pl` for reference.
- Create a folder `data/actions/` and create action files, such as `data/actions/my_action.pl`. See `data.example/actions/` for reference.

## License
[GNU AGPLv3 or later](https://www.gnu.org/licenses/agpl-3.0.en.html)  
© Pelle Hjek 2021
