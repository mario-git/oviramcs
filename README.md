## My Emacs Rabbit Hole

This repo is to save my Emacs configurations. The baseline is the one referenced in the [relevant chapter](http://www.braveclojure.com/basic-emacs/) of the Brave Clojure book and I'll build on top of that.

### Troubleshooting

If after loading the packages Emacs complains on missing dependencies, usually a `M-x package-list-packages` or `list-packages / U` makes it happy.

If on the splash screen the icons seem broken, a round of `M-x all-the-icons-install-fonts` and a restart should fix them.