# Act Now
Act Now is a web app for creating action sign-up forms, allowing for conditional commitment.

> I developed a ‘conditional commitment’ routine for canvassers. Would you go on strike if a critical mass would act likewise? This worked and led to a hall going on rent strike. The fear factor of eviction disappeared and many more joined the strike, leading to an agreed rent reduction.  
— Roger Hallam in [Common Sense for the 21st Century](https://www.rogerhallam.com/wp-content/uploads/2019/10/Common-Sense-for-the-21st-Century_by-Roger-Hallam-Early-Draft-v0.3.pdf)

## Setup
Download [the latest version of Act Now](https://github.com/hjek/act-now/archive/refs/heads/main.zip) and unzip it.

- Mac OS
	* [Disable Gatekeeper](https://osxdaily.com/2012/07/27/app-cant-be-opened-because-it-is-from-an-unidentified-developer/).
	* Double-click on `libreaction.mac_os.command`.
- Windows
	* [Download SWI Prolog for Windows](https://www.swi-prolog.org/download/stable/bin/swipl-8.2.4-1.x64.exe.envelope)
	* Double-click on `libreaction.windows.cmd`.
	* Continue past warnings about "software from the internet".
- Debian
	* Install SWI Prolog: `sudo apt install swi-prolog`
	* Run the app: `./libreaction.pl`

Try it out by visiting [`localhost:8080`](http://localhost:8080) in your web browser.

To run on a different port, `./libreaction.pl --port=9090`.

#### Docker install
Alternatively, you can run **Act Now** in **Docker** ([for OS X](https://docs.docker.com/docker-for-mac/install/):

```
./act-now.docker.sh
```

## Admin
### Password
You'll be asked to enter a password for the admin account on first run. It can be changed later by running `admin:set_password.` from the Prolog console.

### Creating a new action
Click on the small `+` below the action list, then log in with *blank username* and with *the password you set*.

### Action dashboard (edit / outreach / ...)
Click on the small `~` by the action in the list, then log in.

## Development
### Design goals
- No JavaScript [because not everyone have JS](https://kryogenix.org/code/browser/everyonehasjs.html)
- Minimalist: Less than 1000 lines of code
- Light on external dependencies: Use your own preferred email client for outreach

### Roadmap
#### v0.1
- [x] User-defined actions in XML
- [x] Basic conditional action signup
- [x] Conditional action signup if certain needs can be met
- [x] Non-action support roles
- [x] Email comfirmation on signup
- [x] Email notification when target is reached
- [ ] Captcha / spam prevention / verify email
- [ ] Action deadlines
- [ ] Only show conditional commitment options for actions that have enabled those

#### v0.2
- [x] Admin dashboard
- [ ] Email notifications using your own email client using `mailto:` with BCC
- [ ] Advanced queries

#### v0.3
- [ ] GDPR compliance helper scripts
- [ ] Buddy pair-up
- [ ] Signal / SMS text message confirmation
- [ ] Nearby actions
- [ ] Trainings

## License
Act Now is free / libre / copyleft software.  
[GNU AGPLv3 or later](./COPYING.md)  
© Pelle Hjek 2021  
