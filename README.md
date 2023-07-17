# dotfiles

These are my collection of dotfiles. I switched to using **nixos** and decided to properly track my config files in git.

Currently my setup is not fully managed by nixos. When I switch to home-manager this will be completed fully.

## tmux

In order to fully load tmux you will need to pull in *tpm*

``` shell
git clone https://github.com/tmux-plugins/tpm ~/.tmux/plugins/tpm
```

And then source the configuration while tmux is already running.

``` shell
tmux source ~/.tmux.conf
```

The package manager will the retrieve all the plugins.

## ZSH

**zsh** has the same issue. It can be fully managed by nix, but due to not using home-manager it is not. You will need to manually install oh-my-zsh (https://ohmyz.sh/). 

## Stackstorage

The stackstorage daemon is manually downloaded from the support site as an AppImage and placed in **~/.local/bin**.
