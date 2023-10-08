{ config, lib, pkgs, ... }:

{
  # Home Manager needs a bit of information about you and the
  # paths it should manage.
  home.username = "arjen";
  home.homeDirectory = "/home/arjen";

  nixpkgs.config.allowUnfree = true;

  # This value determines the Home Manager release that your
  # configuration is compatible with. This helps avoid breakage
  # when a new Home Manager release introduces backwards
  # incompatible changes.
  #
  # You can update Home Manager without changing this value. See
  # the Home Manager release notes for a list of state version
  # changes in each release.
  home.stateVersion = "23.05";

  # Let Home Manager install and manage itself.
  programs.home-manager.enable = true;

  home.packages = with pkgs; [
    # desktop apps
    firefox
    chromium
    emacs29
    ripgrep
    coreutils
    fd
    clang
    graphviz
    bitwarden
    gnupg
    appimage-run # to run stack appimage
    polkit_gnome
    # okular # my goto for presentations
    hunspell
    hunspellDicts.nl_NL
    hunspellDicts.en_US
    flameshot
    discord
    zotero
    emote
    # direnv
    direnv
    nix-direnv
    # i3 environment
    polybarFull
    kitty
    alacritty
    rofi
    nitrogen
    xss-lock
    pulseaudio
    gtk-engine-murrine
    catppuccin-gtk
    gnome.gnome-themes-extra
    gnome.nautilus
    evince
    # fonts
    nerdfonts
    emacs-all-the-icons-fonts
    inter
    # shell
    fzf
    bat
    # sound and display 
    pavucontrol
    mons
    lxappearance
    # networking
    networkmanager
    networkmanager_dmenu
    protonvpn-gui
    # lockscreen
    betterlockscreen
    # docker
    docker-compose
    # tmux-yank dependency
    xsel
    xclip
    # Office
    libreoffice
    (makeDesktopItem {
      name = "org-protocol";
      exec = "emacsclient -- %u";
      comment = "Org protocol";
      desktopName = "org-protocol";
      type = "Application";
      mimeTypes = ["x-scheme-handler/org-protocol"];
    })
  ];
  programs.git = {
    enable = true;
    userName  = "Arjen Wiersma";
    userEmail = "arjen@wiersma.org";
    signing = {
      key = "94026A70F61228C894456942BFA8EABC51AF1D65";
      signByDefault = true;
    };
  };
  programs.tmux = {
    enable = true;
    shortcut = "space";
    baseIndex = 1;
    keyMode = "vi";
    sensibleOnTop = true;
    terminal = "xterm-256color";
    mouse = true;
    plugins = with pkgs; [
      tmuxPlugins.yank
      tmuxPlugins.vim-tmux-navigator
      tmuxPlugins.sensible
      tmuxPlugins.catppuccin
    ];
    extraConfig = ''
      # https://old.reddit.com/r/tmux/comments/mesrci/tmux_2_doesnt_seem_to_use_256_colors/
      set -ga terminal-overrides ",*256col*:Tc"
      set -ga terminal-overrides '*:Ss=\E[%p1%d q:Se=\E[ q'
      set-environment -g COLORTERM "truecolor"

      bind C-l send-keys "clear"\; send-keys "Enter"
    '';
  };

  programs.zsh = {
    enable = true;
    shellAliases = {
      dotfiles="/home/$USER/.nix-profile/bin/git --git-dir=$HOME/.dotfiles/ --work-tree=$HOME";
      createdirenv="nix --extra-experimental-features \"nix-command flakes\" flake new -t github:nix-community/nix-direnv .";
    };
    plugins = [
      {
        # will source zsh-autosuggestions.plugin.zsh
        name = "zsh-autosuggestions";
        src = pkgs.fetchFromGitHub {
          owner = "zsh-users";
          repo = "zsh-autosuggestions";
          rev = "v0.4.0";
          sha256 = "0z6i9wjjklb4lvr7zjhbphibsyx51psv50gm07mbb0kj9058j6kc";
        };
      }
    ];
    oh-my-zsh = {
      enable = true;
      theme = "amuse";
      plugins = ["git" "direnv" "docker" "docker-compose" "fzf" "sudo" "mvn" "virtualenv"];
    };
  };

  programs.direnv = {
    enable = true;
    enableZshIntegration = true;
  };

  home.sessionVariables = rec {
    OPENAI_API_KEY="$(cat ~/.config/openai/key)";
  };

  services.picom = {
    enable = true;
    backend = "glx";
    vSync = true;
    shadow = false;
  };

  services.emacs = {
    enable = true;
    package = pkgs.emacs29; # replace with emacs-gtk, or a version provided by the community overlay if desired.
  };

  fonts.fontconfig.enable = true;
}
