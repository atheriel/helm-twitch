# helm-twitch --- Navigate Twitch.tv via Helm

In the spirit of doing as much as possible inside Emacs, this package provides a
means of checking whether your favourite [Twitch.tv](http://www.twitch.tv/)
streamer is online, using the [Helm](https://emacs-helm.github.io/helm/)
framework. It's as simple as `M-x helm-twitch`.

## Customizing

`helm-twitch` has very few customization parameters. If you have a Twitch API
client ID (which is optional, unless you're worried about rate limiting), you
can tell the package to use it with

```emacs-lisp
(setq twitch-api-client-id "my-id-string")
```

The only other parameter at the moment is `twitch-game-type`, which allows you
to narrow the stream results you see to a single game. For example:

```emacs-lisp
(setq twitch-game-type "League of Legends")
```

`helm-twitch` also defines a few fonts, which are modelled after the Zenburn
colours by default. You can change them by modifying

* `helm-twitch-streamer-face`,
* `helm-twitch-viewers-face`,
* `helm-twitch-status-face`, and
* `helm-twitch-prefix-face`

All of which have self-explanitory names.

## License

This project is distributed under the GNU General Public License, version 3 or
greater. See the LICENSE file for details.
