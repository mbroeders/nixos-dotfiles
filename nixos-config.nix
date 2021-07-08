#
# this file is auto-generated from "system.org"
#
{ name, config, pkgs, lib, inputs, ... }:
let
  machine-config = lib.getAttr name {
    wheeler = [
      {
        imports = [
          inputs.nixpkgs.nixosModules.notDetected
        ];
      
        boot.initrd.availableKernelModules = [ "xhci_pci" "ahci" "nvme" "usb_storage" "sd_mod" ];
        boot.initrd.kernelModules = [ ];
        boot.kernelModules = [ "kvm-intel" ];
        boot.extraModulePackages = [ ];
        # enable ntfs support
        boot.supportedFilesystems = [ "ntfs" ];
      
        powerManagement.cpuFreqGovernor = lib.mkDefault "powersave";
      
        # use the systemd-boot efi boot loader.
        boot.loader.systemd-boot.enable = true;
        boot.loader.efi.canTouchEfiVariables = true;
        boot.loader.grub.useOSProber = true;
      }
      {
        fileSystems."/" =
        { device = "/dev/disk/by-uuid/d1f8c7ae-ecd8-4fc1-937c-838360d8ad28";
          fsType = "ext4";
        };
      
      boot.initrd.luks.devices."cryptroot".device = "/dev/disk/by-uuid/79763e12-47f4-48a5-8957-a6947fa81a9d";
      
      fileSystems."/boot" =
        { device = "/dev/disk/by-uuid/0037-9737";
          fsType = "vfat";
        };
      
      swapDevices =
        [ { device = "/dev/disk/by-uuid/c264e2d9-0eed-4fd3-b19e-8b9574459d7e"; }
        ];
      
        # mount ntfs-filesystem as read/write
        # fileSystems."/path/to/mount/to" =
        #  { device = "/path/to/the/device";
        #    fsType = "ntfs";
        #    options = [ "rw" "uid=theuidofyouruser"];
        #  };
      }
      {
        # enable touchpad support (enabled default in most desktopmanager).
        services.xserver.libinput = {
          enable = true;
          touchpad.tapping = true;
          touchpad.clickMethod = "clickfinger";
          touchpad.naturalScrolling = true;
        };
      }
      {
        networking.hostName = "wheeler";
      
        # the global useDHCP flag is deprecated, therefore explicitly set to false here.
        # per-interface useDHCP will be mandatory in the future, so this generated config
        # replicates the default behaviour.
        networking.useDHCP = false;
        networking.interfaces.wlo1.useDHCP = true;
      }
      # enable tlp package for power management
      {
        services.tlp.enable = true;
      
        # enable bluetooth
        hardware.bluetooth.enable = true;
        services.blueman.enable = true;
      
        programs.nm-applet.enable = true;
      }
    ];
    hopper = [
      {
        imports = [
          inputs.nixpkgs.nixosModules.notDetected
        ];
      
        boot.initrd.availableKernelModules = [ "xhci_pci" "ahci" "usb_storage" "usbhid" "sd_mod" ];
        boot.initrd.kernelModules = [ ];
        boot.kernelModules = [ "kvm-amd" ];
        boot.extraModulePackages = [ ];
        # enable ntfs support
        boot.supportedFilesystems = [ "ntfs" ];
      
        # use the systemd-boot efi boot loader.
        boot.loader.systemd-boot.enable = true;
        boot.loader.efi.canTouchEfiVariables = true;
        boot.loader.grub.useOSProber = true;
      }
      {
        fileSystems."/" =
          { device = "/dev/disk/by-uuid/cb10508b-e6dc-43fd-baf6-38f66851d6a3";
            fsType = "ext4";
          };
      
        fileSystems."/boot" =
          { device = "/dev/disk/by-uuid/9E07-E045";
            fsType = "vfat";
          };
      
        fileSystems."/home" =
          { device = "/dev/disk/by-uuid/38fb5ec3-1215-4c9f-bc54-7c206f5176ec";
            fsType = "ext4";
          };
      
        swapDevices =
          [ { device = "/dev/disk/by-uuid/273ba5a6-2ca3-4cb9-be62-94bc16fce9cd"; }
          ];
      
        # mount ntfs-filesystem as read/write
        #fileSystems."/path/to/mount/to" =
        #  { device = "/path/to/the/device";
        #    fsType = "ntfs";
        #    options = [ "rw" "uid=theuidofyouruser"];
        #  };
      }
      {
        networking.hostName = "hopper";
      
        # the global useDHCP flag is deprecated, therefore explicitly set to false here.
        # per-interface useDHCP will be mandatory in the future, so this generated config
        # replicates the default behaviour.
        networking.useDHCP = false;
        networking.interfaces.enp4s0.useDHCP = true;
      
        # enable the openssh daemon.
        services.openssh.enable = true;
      }
    ];
  };

in
{
  imports = [
    {
      nixpkgs.config.allowUnfree = true;

      # the nixos release to be compatible with for stateful data such as databases.
      system.stateVersion = "21.05";
    }

    {
      nix = {
        package = pkgs.nixFlakes;
        extraOptions = ''
          experimental-features = nix-command flakes ca-references
        '';
      };
    }
    {
      imports = [inputs.home-manager.nixosModules.home-manager];
      home-manager = {
        useUserPackages = true;
        useGlobalPkgs = true;
        users.mark = inputs.self.lib.home-manager-common;
      };
    }
    {
      # for compatibility with nix-shell, nix-build, etc.
      environment.etc.nixpkgs.source = inputs.nixpkgs;
      nix.nixPath = ["nixpkgs=/etc/nixpkgs"];
    
      # register self and nixpkgs as flakes for quick access
      nix.registry = {
        self.flake = inputs.self;
    
        nixpkgs.flake = inputs.nixpkgs;
      };
    }
    {
      users.extraUsers.mark = {
        isNormalUser = true;
        # uid = 1000;
        shell = pkgs.zsh;
        extraGroups = [ "wheel" "audio" "video" "input" "sound" "networkmanager" "power" ]; # enable ‘sudo’ for the user.
      };
      nix.trustedUsers = ["mark"];
    }
    
    ( 
      with pkgs;
      let
        my-python-packages = python-packages: with python-packages; [
          requests
        ];
        python-with-my-packages = python3.withPackages my-python-packages;
      in
        {
          environment.systemPackages = [
            python-with-my-packages
          ];
        }
    )
    
    {
      environment.systemPackages = [ pkgs.sqlite ];
    }
    {
      environment.systemPackages = [
        pkgs.xorg.xhost
      ];
      services.xserver.windowManager.session = lib.singleton {
        name = "exwm";
        start = ''
          xmodmap $HOME/.dotfiles/.Xmodmap
          source $HOME/.zprofile
          xhost +si:localuser:$USER
          exec emacs --debug-init
        '';
        # exec ${pkgs.my-emacs}/bin/emacsclient -a "" -c
      };
      services.xserver.displayManager.lightdm.enable = true;
      # services.xserver.displayManager.startx.enable = true;
      services.xserver.displayManager.defaultSession = "none+exwm";
    }
    {
      programs.slock.enable = true;
    
      services.physlock = {
        enable = true;
        lockOn.suspend = true;
        };
    }
    {
      services.xserver.enable = true;
    }
    {
      i18n.defaultLocale = "nl_NL.UTF-8";
      # i18n.supportedlocales = [ "en_US.utf-8/utf-8" ];
    }
    {
      time.timeZone = "Europe/Amsterdam";
    }
    {
      # configure keymap in x11
      services.xserver.layout = "us";
      services.xserver.xkbOptions = "eurosign:e";
    
      console = {
        font = "Lat2-Terminus16";
        keyMap = "us-acentos";
      };
    }
    (
      let
        myXmodmap = pkgs.writeText "xkb-layout" ''
        clear lock
        clear control
        keycode 66 = control_L
        add control = control_L
        add Lock = control_R
        '';
      in
        {
          environment.systemPackages = [
            pkgs.xorg.xmodmap
          ];
    
          services.xserver.displayManager.sessionCommands = "${pkgs.xorg.xmodmap}/bin/xmodmap ${myXmodmap}";
        }
    )
    
    {
      networking = {
        networkmanager.enable = true;
      };
    }
    {
      programs.gnupg.agent = {
        enable = true;
        enableSSHSupport = true;
        pinentryFlavor = "emacs";
      };
    }
    # enable sound.
    {
      sound.enable = true;
      hardware.pulseaudio.enable = true;
      hardware.pulseaudio.support32Bit = true;    ## if compatibility with 32-bit applications is desired.
    }
    {
      services.syncthing = {
        enable = true;
        user = "mark";
      };
    }
    {
      # hardware.acpilight.enable = true;
      # environment.systemPackages = [ pkgs.acpilight ];
      environment.systemPackages = [ pkgs.brightnessctl ];
    }
  ] ++ machine-config;
}
