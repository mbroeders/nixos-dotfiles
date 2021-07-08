# ZPROFILE - my environment variables

# Make sure we can reach the GPG agent for SSH auth
export SSH_AUTH_SOCK=$(gpgconf --list-dirs agent-ssh-socket)

# Make Flatpak apps visible to launcher
export XDG_DATA_DIRS="$XDG_DATA_DIRS:$HOME/.local/share/flatpak/exports/share"
