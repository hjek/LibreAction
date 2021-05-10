**LibreAction** — A web app for organizing actions, allowing for conditional signup.

## Conditional commitment
In [Common Sense for the 21st Century](https://www.rogerhallam.com/wp-content/uploads/2019/10/Common-Sense-for-the-21st-Century_by-Roger-Hallam-Early-Draft-v0.3.pdf) RH describes how conditional commitment can help mobilisation:

> We need to focus on the specific blocks to mobilisation and campaign success and then think about a range of micro designs to maximise the chances of reducing these blocks. For instance, I designed London’s first organised rent strike by working out that the key block was not that people were not pissed off with their rent levels. They were. The problem was that tenants would not go on strike because they were not confident others would do likewise.
>
> I developed a ‘conditional commitment’ routine for canvassers. Would you go on strike if a critical mass would act likewise? This worked and led to a hall going on rent strike. The fear factor of eviction disappeared and many more joined the strike, leading to an agreed rent reduction.

## Setup
Download [the latest version of LibreAction](https://github.com/hjek/LibreAction/archive/refs/heads/main.zip) and unzip it.

- Mac OS
	* [Disable Gatekeeper](https://osxdaily.com/2012/07/27/app-cant-be-opened-because-it-is-from-an-unidentified-developer/).
	* Double-click on `libreaction.mac_os.command`.
- Windows
	* [Download SWI Prolog for Windows](https://www.swi-prolog.org/download/stable/bin/swipl-8.2.4-1.x64.exe.envelope)
	* Double-click on `libreaction.pl`
- Debian
	* Install SWI Prolog: `sudo apt install swi-prolog`
	* Run the app: `./libreaction.pl`

Try it out by visiting [localhost:8080](https://localhost:8080) in your web browser.

### Further configuration
- Create a folder `actions` and add some action files, like `actions/my-action.xml`. See [`actions.example`](./actions.example/) for examples.
- For sending notification emails, please enter you SMTP details in `email.xml`. See `email.example.xml` for reference.
- If you'd like to embed images or video in your action descriptions, you can put them in the `static` folder and they'll be available in the web app root, `/`.

## Development

## Design goals
- No JavaScript
- Light on external dependencies
- Minimalist

## Roadmap

### v0.1
- [x] User-defined actions in XML
- [x] Basic conditional action signup
- [x] Conditional action signup if certain needs can be met
- [x] Non-action support roles
- [x] Email comfirmation on signup
- [x] Email notification when target is reached
- [ ] Captcha / spam prevention / verify email
- [ ] Action deadlines
- [ ] Only show conditional commitment options for actions that have enabled those

### v0.2
- [ ] Admin dashboard
- [ ] Email notifications using your own email client using `mailto:` with BCC
- [ ] Advanced queries
- [ ] GDPR compliance helper scripts

### v0.3
- [ ] Buddy pair-up
- [ ] Signal / SMS text message confirmation
- [ ] Nearby actions
- [ ] Trainings

## License
[GNU AGPLv3 or later](./COPYING.md)  
© Pelle Hjek 2021  
