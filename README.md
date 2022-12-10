# Oviramcs

This repo is to save my Emacs configuration, currently a Spacemacs/hybrid one.

In the other (non default) branch there is a more basic one.

The word Oviramcs itself is my first name reversed, with a (Emacs flavoured) *cs* suffix and a *v* (for Vim) thrown in the mix.

## Setup

Clone this repo in `~/.spacemacs.d`;

Alternatively, if you clone it somewhere else, you'll have to either:
- link the root directory of this repository to `~/.spacemacs.d`, as in `ln -s $(pwd) ~/.spacemacs.d`;
- link `init.el` to `~/.spacemacs` as in `ln -s $(pwd)/init.el ~/.spacemacs`.

## Dependencies

- optional *tomorrow* and *vscode* custom themes, installable via `get-custom-themes` script.
- `rg` (or `ag` or `pt` or `ack`) search tool installed, the default `helm` with `grep` doesn't skip ignored files.

## Troubleshooting

- If on the splash screen the icons seem broken, a round of `SPC SPC all-the-icons-install-fonts` and a restart should fix them.
