{ config, pkgs, ... }:

{
  # Enable networking
  networking.networkmanager.enable = true;

  hardware.bluetooth.enable = true;

  programs.dconf.enable = true;
  programs.zsh.enable = true;

  services.blueman.enable = true;
  services.gnome.gnome-keyring.enable = true; # stack will start without asking for password

  security.polkit.enable = true;
  services.pcscd.enable = true;

  services.devmon.enable = true;
  services.gvfs.enable = true;
  services.udisks2.enable = true;

  services.dbus.packages = [ pkgs.gcr ];
  services.geoclue2.enable = true;
  programs.gnupg.agent = {
    enable = true;
    pinentryPackage = pkgs.pinentry-gnome3;
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
  hardware.nvidia-container-toolkit.enable = true;
  
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
  # services.xserver.enable = true;

  # Enable the XFCE Desktop Environment.
  #  services.xserver.displayManager.lightdm.enable = true;
  #   services.xserver.desktopManager.xfce.enable = true;
  #   Enable the X11 windowing system.

   services.xserver = {
     enable = true;

     wacom.enable = true;
     desktopManager = {
       xterm.enable = false;
     };
   
     displayManager.gdm.enable = true;
     #displayManager.gdm.wayland = true;
     #windowManager.hypr.enable = true;
     # desktopManager.plasma6.enable = true;
     #desktopManager.gnome.enable = true;

     windowManager.i3 = {
       enable = true;
       extraPackages = with pkgs; [
         dmenu #application launcher most people use
         i3status # gives you the default i3 status bar
         i3lock #default i3 screen locker
         i3blocks #if you are planning on using i3blocks over i3status
       ];
     };
   };
   #services.displayManager.sddm.enable = true;
   #services.desktopManager.plasma6.enable = true;

   # # -- hyprland
   # programs.hyprland = {
   #   # Install the packages from nixpkgs
   #   enable = true;
   #   # Whether to enable XWayland
   #   xwayland.enable = true;
   # };
   # services.hypridle.enable = true;
   # programs.hyprlock.enable = true;
   
   services.displayManager = {
       defaultSession = "none+i3";
   };
   #programs.xwayland.enable = true;
   xdg.portal.enable = true;
   xdg.portal.config.common.default = "*";

   xdg.portal.xdgOpenUsePortal = true;
   xdg.portal.extraPortals = [
           pkgs.xdg-desktop-portal-gtk
	   pkgs.xdg-desktop-portal-gnome
	   #pkgs.xdg-desktop-portal-wlr
   ];

  networking.firewall.trustedInterfaces = [ "p2p-wl+" ];

  networking.firewall.allowedTCPPorts = [ 7236 7250 ];
  networking.firewall.allowedUDPPorts = [ 7236 5353 ];
  # Configure keymap in X11
  services.xserver = {
    xkb = {
	    layout = "us";
	    variant = "";
    };
  };

  # Enable CUPS to print documents.
  services.printing.enable = true;

  services.avahi = {
    enable = true;
    nssmdns4 = true;
    openFirewall = true;
  };
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
    extraGroups = [ "networkmanager" "wheel" "docker" "storage" "video" "audio"];
    shell = pkgs.zsh;
    packages = with pkgs; [
    ];
  };

  fonts.packages = with pkgs; [
    nerdfonts
  ];
  # Allow unfree packages
  nixpkgs.config.allowUnfree = true;

  # List packages installed in system profile. To search, run:
  # $ nix search wget
  environment.systemPackages = with pkgs; [
  	gnomeExtensions.appindicator
  ];
  services.udev.packages = with pkgs; [ gnome.gnome-settings-daemon ];

  nix.settings = {
    keep-outputs = true;
    keep-derivations = true;
  };
  environment.pathsToLink = [
    "/libexec"
    "/share/nix-direnv"
  ];

  #environment.sessionVariables.NIXOS_OZONE_WL = "1";
}
