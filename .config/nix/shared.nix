{ config, pkgs, ... }:

{
  # Enable networking
  networking.networkmanager.enable = true;

  services.xserver.enable = true;
  services.xserver.displayManager.gdm.enable = true;
  services.xserver.desktopManager.gnome.enable = true;

  hardware.bluetooth.enable = true;

  programs.dconf.enable = true;
  programs.zsh.enable = true;

  services.blueman.enable = true;
  services.gnome.gnome-keyring.enable = true; # stack will start without asking for password

  security.polkit.enable = true;
  services.pcscd.enable = true;

  services.geoclue2.enable = true;
  programs.gnupg.agent = {
    enable = true;
    pinentryFlavor = "gtk2";
    enableSSHSupport = true;
  };

  virtualisation = {
    docker = {
 	    enable = true;

      # Create a `docker` alias for podman, to use it as a drop-in replacement
      #dockerCompat = true;

      # Required for containers under podman-compose to be able to talk to each other.
      #defaultNetwork.settings.dns_enabled = true;
    };
  };
  virtualisation.vmware.host.enable = true;

  # Set your time zone.
  time.timeZone = "Europe/Amsterdam";

  # Select internationalisation properties.
  i18n.defaultLocale = "en_US.UTF-8";

  i18n.extraLocaleSettings = {
    LC_ADDRESS = "nl_NL.UTF-8";
    LC_IDENTIFICATION = "nl_NL.UTF-8";
    LC_MEASUREMENT = "nl_NL.UTF-8";
    LC_MONETARY = "nl_NL.UTF-8";
    LC_NAME = "nl_NL.UTF-8";
    LC_NUMERIC = "nl_NL.UTF-8";
    LC_PAPER = "nl_NL.UTF-8";
    LC_TELEPHONE = "nl_NL.UTF-8";
    LC_TIME = "nl_NL.UTF-8";
  };

  # Enable the X11 windowing system.
  #services.xserver.enable = true;

  # Enable the XFCE Desktop Environment.
#  services.xserver.displayManager.lightdm.enable = true;
#   services.xserver.desktopManager.xfce.enable = true;
  #   Enable the X11 windowing system.

  #  services.xserver = {
  #    enable = true;

  #    wacom.enable = true;
  #    desktopManager = {
  #      xterm.enable = false;
  #    };
   
  #    displayManager = {
  #      defaultSession = "none+i3";
  #    };

  #    windowManager.i3 = {
  #      enable = true;
  #      extraPackages = with pkgs; [
  #        dmenu #application launcher most people use
  #        i3status # gives you the default i3 status bar
  #        i3lock #default i3 screen locker
  #        i3blocks #if you are planning on using i3blocks over i3status
  #      ];
  #    };
  # };

  # Configure keymap in X11
  services.xserver = {
    layout = "us";
    xkbVariant = "";
  };

  # Enable CUPS to print documents.
  services.printing.enable = true;

  services.fwupd.enable = true;


  # Enable sound with pipewire.
  sound.enable = true;
  hardware.pulseaudio.enable = false;
  security.rtkit.enable = true;
  services.pipewire = {
    enable = true;
    alsa.enable = true;
    alsa.support32Bit = true;
    pulse.enable = true;
    # If you want to use JACK applications, uncomment this
    jack.enable = true;

    # use the example session manager (no others are packaged yet so this is enabled by default,
    # no need to redefine it in your config for now)
    #media-session.enable = true;
  };

  # I am me on all my systems

  # Define a user account. Don't forget to set a password with ‘passwd’.
  users.users.arjen = {
    isNormalUser = true;
    description = "Arjen Wiersma";
    extraGroups = [ "networkmanager" "wheel" "docker" ];
    shell = pkgs.zsh;
    packages = with pkgs; [
      alacritty
    ];
  };

  fonts.packages = with pkgs; [
    nerdfonts
  ];
  # Allow unfree packages
  nixpkgs.config.allowUnfree = true;

  # List packages installed in system profile. To search, run:
  # $ nix search wget
  #environment.systemPackages = with pkgs; [
  #];

  nix.settings = {
    keep-outputs = true;
    keep-derivations = true;
  };
  environment.pathsToLink = [
    "/libexec"
    "/share/nix-direnv"
  ];
}
