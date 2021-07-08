#
# this file is auto-generated from "system.org"
#
{
  description = "my packages and nixos/home-manager configurations";

  inputs = {
    nixpkgs = {
      type = "github";
      owner = "nixos";
      repo = "nixpkgs";
      ref = "nixpkgs-unstable";
    };

    nixpkgs-stable = {
      type = "github";
      owner = "nixos";
      repo = "nixpkgs";
      ref = "nixos-21.05";
    };
    home-manager = {
      type = "github";
      owner = "rycee";
      repo = "home-manager";
      ref = "master";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    emacs-overlay = {
      type = "github";
      owner = "nix-community";
      repo = "emacs-overlay";
    };
  };

  outputs = { self, ... }@inputs:
    let
      # flakes are evaluated hermetically, thus are unable to access
      # host environment (including looking up current system).
      #
      # that's why flakes must explicitly export sets for each system
      # supported.
      systems = ["x86_64-linux"];

      # genAttrs applies f to all elements of a list of strings, and
      # returns an attrset { name -> result }
      #
      # useful for generating sets for all systems or hosts.
      genAttrs = list: f: inputs.nixpkgs.lib.genAttrs list f;

      # generate pkgs set for each system. this takes into account my
      # nixpkgs config (allowUnfree) and my overlays.
      pkgsBySystem =
        let mkPkgs = system: import inputs.nixpkgs {
              inherit system;
              overlays = self.overlays.${system};
              config = { allowUnfree = true; };
            };
        in genAttrs systems mkPkgs;

      # genHosts takes an attrset { name -> options } and calls mkHost
      # with options+name. the result is accumulated into an attrset
      # { name -> result }.
      #
      # used in nixos and home manager configurations.
      genHosts = hosts: mkHost:
        genAttrs (builtins.attrNames hosts) (name: mkHost ({ inherit name; } // hosts.${name}));

      # merges a list of attrsets into a single attrset
      mergeSections = inputs.nixpkgs.lib.foldr inputs.nixpkgs.lib.mergeAttrs {};

    in mergeSections [
      (let
        nixosHosts = {
          wheeler = { system = "x86_64-linux";  config = ./nixos-config.nix; };
      
          # i'll use the same configuration file for hopper, because the configuration
          # details are mostly the same. let's see if this works.
          hopper = { system = "x86_64-linux";  config = ./nixos-config.nix; };
        };
      
        mkNixosConfiguration = { name, system, config }:
          let pkgs = pkgsBySystem.${system};
          in inputs.nixpkgs.lib.nixosSystem {
            inherit system;
            modules = [
              { nixpkgs = { inherit pkgs; }; }
              (import config)
            ];
            specialArgs = { inherit name inputs; };
          };
      
      in {
        nixosConfigurations = genHosts nixosHosts mkNixosConfiguration;
      })
      (let
        homeManagerHosts = {
          markBroeders = {
            system = "x86_64-linux";
            config = ./home.nix;
            username = "mark";
            homeDirectory = "/home/mark";
          };
        };
      
        mkHomeManagerConfiguration = { system, name, config, username, homeDirectory }:
          let pkgs = pkgsBySystem.${system};
          in inputs.home-manager.lib.homeManagerConfiguration {
            inherit system pkgs username homeDirectory;
            configuration = { ... }: {
              nixpkgs.config.allowUnfree = true;
              nixpkgs.config.firefox.enableTridactylNative = true;
              nixpkgs.overlays = self.overlays.${system};
              imports = [
                self.lib.home-manager-common
      
                (import config)
              ];
            };
          };
      
      in {
        # re-export common home-manager configuration to be reused between
        # nixos module and standalone home-manager config.
        lib.home-manager-common = { lib, pkgs, config, ... }: {
          imports = [
            {
              home.file."nixpkgs".source = inputs.nixpkgs;
              systemd.user.sessionVariables.NIX_PATH = lib.mkForce "nixpkgs=$home/nixpkgs\${NIX_PATH:+:}$NIX_PATH";
            
              xdg.configFile."nix/registry.json".text = builtins.toJSON {
                version = 2;
                flakes = [
                  {
                    from = { id = "self"; type = "indirect"; };
                    to = ({
                      type = "path";
                      path = inputs.self.outPath;
                    } // lib.filterAttrs
                      (n: v: n == "lastModified" || n == "rev" || n == "revCount" || n == "narHash")
                      inputs.self);
                  }
                  {
                    from = { id = "nixpkgs"; type = "indirect"; };
                    to = ({
                      type = "path";
                      path = inputs.nixpkgs.outPath;
                    } // lib.filterAttrs
                      (n: v: n == "lastModified" || n == "rev" || n == "revCount" || n == "narHash")
                      inputs.nixpkgs);
                  }
                ];
              };
            }
            {
              programs.emacs = {
                enable = true;
                package = pkgs.my-emacs.base;
                extraPackages = pkgs.my-emacs.packages;
              };
            
              # fonts and extra packages used by emacs
              home.packages = [
                pkgs.binutils       # native comp needs this
            
                # optional dependencies for emacs
                pkgs.fd
                pkgs.pinentry_emacs
                (pkgs.ripgrep.override {withPCRE2 = true;})
                pkgs.zstd                # for undo-fu-session/undo-tree compression
            
                pkgs.powerline-fonts
                pkgs.fira-code
                pkgs.fira-mono
                pkgs.cantarell-fonts
                pkgs.jetbrains-mono
                pkgs.emacs-all-the-icons-fonts
                pkgs.libertine
                pkgs.iosevka
            
                # development packages
                pkgs.cmake
                pkgs.coreutils
                pkgs.gcc
                pkgs.gnumake
                pkgs.gnutls
                pkgs.libtool
            
                pkgs.rustup
              ];
            
              # add the .emacs.d/bin folder to path // needed for doom emacs
              # home.sessionpath = [ "$home/.emacs.d/bin/" ];
            
              # for doom to properly use mu/mu4e (email) i need to make a symlink:
              # environment.etc."mu4e".source = "${pkgs.mu}/share/emacs/site-lisp/mu4e";
            }
            {
              home.packages = [
                pkgs.xss-lock
              ];
            }
            {
              services.picom.enable = true; # enable compositor
            }
            {
              home.keyboard = null;
            }
            {
              home.packages = [ pkgs.networkmanagerapplet ];
            }
            {
              home.packages = [ pkgs.acpilight ];
            }
            {
              home.packages = [
                pkgs.qutebrowser
                # pkgs.firefox
              ];
            }
            {
              # both config files get tangled from emacs.org
              programs.mbsync =
                {
                  enable = true;
                };
              home.file.".mbsyncrc".source = ./.mbsyncrc;
            
              programs.msmtp =
                {
                  enable = true;
                };
              home.file.".msmtprc".source = ./.msmtprc;
            }
            {
              services.udiskie = {
                enable = true;
                automount = true;
                notify = true;
                tray = "auto";
              };
            }
            {
              home.packages = [ pkgs.zathura ];
            }
            {
              programs.mpv = {
                enable = true;
                package = pkgs.wrapMpv (pkgs.mpv-unwrapped.override { vapoursynthSupport = true; }) { youtubeSupport = true; };
                config = {
                  vo = "gpu";
                  hwdec = "vaapi";
                  profile = "gpu-hq";
                  scale = "ewa_lanczossharp";
                  cscale = "ewa_lanczossharp";
                  ytdl-format = "bestvideo+bestaudio";
                };
              };
            }
            {
              programs.zsh =
                {
                  enable = true;
                  enableAutosuggestions = true;
                  enableCompletion = true;
                  shellAliases =
                    {
                      ll="ls -all";
                    };
                  oh-my-zsh =
                    {
                      enable = true;
                      plugins = [
                        "git"
                        "history"
                        "sudo"
                      ];
                      theme = "agnoster";
                    };
                };
            }
            {
              programs.git =
                {
                  enable = true;
                  userName = "Mark Broeders";
                  userEmail = "mail@markBroeders.nl";
                  aliases = {
                    c = "commit";
                    s = "status";
                    b = "branch";
                    p = "pull";
                    pu = "push";
                  };
                };
            }
            {
              home.packages = [
                pkgs.dunst             # keep me posted!
                pkgs.polybar           # what to do without a panel
                pkgs.feh               # background
                pkgs.udiskie
            
                # media
                pkgs.playerctl         # mainly for polybar script
            
                # work
                pkgs.mu                # mail
                pkgs.remmina           # rdp
            
                # other
                pkgs.pass              # password manager
            
                pkgs.vim
                pkgs.git
                pkgs.stow
              ];
            
              # texlive
              programs.texlive = {
                enable = true;
                extraPackages = tpkgs: {
                  inherit (tpkgs)
                    scheme-medium
                    capt-of
                    wrapfig;
                };
              };
            }
          ];
          # home.stateVersion = "20.09";
          home.stateVersion = "21.03";
        };
        homeManagerConfigurations = genHosts homeManagerHosts mkHomeManagerConfiguration;
      })
      (let
        mkPackages = system:
          let
            pkgs = pkgsBySystem.${system};
          in
            mergeSections [
              (let
                emacs-base = pkgs.emacsGcc;
                emacs-packages = (epkgs:
                  (with epkgs.melpaPackages; [
                    f
                    use-package
                    epkgs.exwm
                    desktop-environment
                    doom-themes
                    doom-modeline
                      all-the-icons
                    smartparens
                      rainbow-delimiters
                    default-text-scale
                    general
                    evil
                      evil-collection  
                    which-key
                    evil-nerd-commenter
                    ws-butler
                    dired-single
                      dired-hacks-utils
                      all-the-icons-dired
                    flycheck
                    yasnippet
                      yasnippet-snippets
                    projectile
                      ripgrep
                    magit
                    lsp-mode
                      lsp-ui
                      lsp-treemacs
                    company
                    dap-mode
                    pyvenv
                    org-bullets
                      org-appear
                      evil-org
                      org-make-toc
                      deft
                      org-re-reveal
                      visual-fill-column
                    org-journal
                    org-caldav
                      oauth2-request
                    erc-image
                      erc-hl-nicks
                    expand-region
                    helpful
                    mu4e-alert
                    alert
                    hydra
                    eshell-toggle
                      eshell-syntax-highlighting
                    emacs-libvterm
                    password-store
                      # auth-source-pass
                    daemons]) ++ (with epkgs.elpaPackages; [
                      pinentry
                      undo-tree]) ++ (with epkgs.orgPackages; [
                        org
                        org-plus-contrib]) ++ [
                            pkgs.notmuch   # from main packages set
                          ]
                );
                emacs-final = (pkgs.emacsPackagesGen emacs-base).emacsWithPackages emacs-packages;
              
              in {
                my-emacs = emacs-final // {
                  base = emacs-base;
                  packages = emacs-packages;
                };
              })
            ];
      
      in {
        packages = genAttrs systems mkPackages;
      })
      (let
        mkOverlays = system: [
          # mix-in all local packages, so they are available as pkgs.${packages-name}
          (final: prev: self.packages.${system})
      
          (final: prev: {
            stable = import inputs.nixpkgs-stable {
              inherit system;
              overlays = self.overlays.${system};
              config = { allowUnfree = true; };
            };
          })
          inputs.emacs-overlay.overlay
        ];
      in {
        overlays = genAttrs systems mkOverlays;
      })
    ];
}
