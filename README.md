### Oviramcs

This repo is to save my Emacs configurations. It's a basic one that supports evil (disabled by default).

I started with the one referenced in the [relevant chapter](http://www.braveclojure.com/basic-emacs/) of the Brave Clojure book, which I modified.

The word Oviramcs itself is my first name reversed, with a (Emacs flavoured) *cs* suffix and a *v* (for Vim) thrown in the mix.

### Troubleshooting

If after loading the packages Emacs complains on missing dependencies, usually a `M-x package-list-packages` or `list-packages / U` makes it happy.

If on the splash screen the icons seem broken, a round of `M-x all-the-icons-install-fonts` and a restart should fix them.