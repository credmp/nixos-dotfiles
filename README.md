# dotfiles

These are my collection of dotfiles. I switched to using **nixos** and decided to properly track my config files in git.

Currently my setup is not fully managed by nixos. When I switch to home-manager this will be completed fully.

To run this configuration you will need to install `home-manager` and add the channels for nixos-hardware:

``` shell
sudo nix-channel --add https://github.com/NixOS/nixos-hardware/archive/master.tar.gz nixos-hardware
sudo nix-channel --update
```

## Create your own

To create this type of configuration for yourself, use git to create your initial repository.

``` shell
git init --bare $HOME/.dotfiles
```

Then create an alias (and also add it to your shell config) to work with this repository.

``` shell
alias dotfiles='/usr/bin/git --git-dir=$HOME/.dotfiles/ --work-tree=$HOME'
```

Make sure that the repository does not show untracked files, otherwise you will always get to see all your home folder files.

``` shell
dotfiles config --local status.showUntrackedFiles no
```

You can then use the `dotfiles` command as if it is `git`.

## Restore on another computer

``` shell
git clone --bare https://github.com/USERNAME/dotfiles.git $HOME/.dotfiles
# Make sure you have the alias setup
dotfiles checkout
```

## Stackstorage

The stackstorage daemon is manually downloaded from the support site as an AppImage and placed in **~/.local/bin**.
