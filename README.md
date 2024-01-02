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

Make sure git is available in the shell.

``` shell
nix-shell -p git
```

Create the dotfiles alias.

``` shell
alias dotfiles='git --git-dir=$HOME/.dotfiles/ --work-tree=$HOME'
```

Clone the configuration repository to this machine into the .dotfiles directory.

``` shell
git clone --bare https://github.com/credmp/nixos-dotfiles.git $HOME/.dotfiles
```

Then checkout the files, this will take them from the cloned repository to the required places, such as `~/.config/home-manager/home.nix`.

``` shell
dotfiles checkout
```

Install home-manager, matching the current release version. I use the channels method as an installation mechanism.

``` shell
sudo nix-channel --add https://github.com/nix-community/home-manager/archive/release-23.11.tar.gz home-manager
```

Update the channels, so that all the indexes are up to date.

``` shell
sudo nix-channel --update
```

Then install using `nix-shell`.

``` shell
sudo nix-shell '<home-manager>' -A install
```

finally, run `home-manager switch` to get the environment up and running.

``` shell
home-manager switch
```

The base configuration of my system is in the `shared.nix` file. From your `configuration.nix` import it, and then run `sudo nixos-rebuild switch`. Make sure you remove duplicate lines from your original `configuration.nix` file.

## Stackstorage

The stackstorage daemon is manually downloaded from the support site as an AppImage and placed in **~/.local/bin**.
