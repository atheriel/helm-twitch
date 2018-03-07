# helm-twitch.el

In the spirit of doing as much as possible inside Emacs, this package provides a
means of checking whether your favourite [Twitch.tv](http://www.twitch.tv/)
streamer is online, using the [Helm](https://emacs-helm.github.io/helm/)
framework. It's as simple as `M-x helm-twitch`.

You can open live streams in the browser, or join the streamer's Twitch chat
directly in Emacs through `erc`.

This package also contains a special `livestreamer-mode` that supports opening
streams with [Livestreamer](http://docs.livestreamer.io/), for those of us who
want to avoid playback in the browser itself.

## Customizing

`helm-twitch` has very few customization parameters. The most immediately useful
one is `twitch-game-type`, which you can use to narrow the stream results to a
particular game. For example:

```emacs-lisp
(setq twitch-game-type "League of Legends")
```

If you would like to connect to Twitch chat directly in Emacs, you will need to
set the `helm-twitch-username` and `helm-twitch-oauth-token` variables. For
example:

```emacs-lisp
(setq helm-twitch-username "[REDACTED]"
	  helm-twitch-oauth-token "oauth:8lgmfrplmxw65qkjml5lg41z36u9ga")
```

You can get an OAUTH token for this purpose [here](http://twitchapps.com/tmi/).

`helm-twitch` also defines a few faces, which are modelled after the Zenburn
colours by default. You can change them by modifying

* `helm-twitch-streamer-face`,
* `helm-twitch-viewers-face`,
* `helm-twitch-status-face`, and
* `helm-twitch-prefix-face`

All of which have self-explanitory names.

## Using Livestreamer

### Using Twitch.tv Credentials with `livestreamer.el`

As a result of the now-required authentication for all Twitch.tv API calls
(circa September 2016), Livestreamer [now requires an OAuth token](https://github.com/chrippa/livestreamer/issues/1478)
to play Twitch.tv streams. Luckily, it's easy to achieve this by adding this
credential (which is the same as the one required by `twitch-api-oauth-token` as
explained above) to your custom `livestreamer-opts`. For example:

``` emacs-lisp
(setq livestreamer-opts
      (concat "--twitch-oauth-token " twitch-api-oauth-token " "
              livestreamer-opts))
```

See also [the Twitch.tv blog post](https://blog.twitch.tv/client-id-required-for-kraken-api-calls-afbb8e95f843).

## License

This project is distributed under the GNU General Public License, version 3 or
greater. See the LICENSE file for details.
