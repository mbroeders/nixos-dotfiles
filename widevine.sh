#!/usr/bin/env bash
set -euo pipefail

# Basic script to link the correct widevine libs
# Necessary to play Netflix in Qutebrowser
#
# Files are in ../desktop/
#
# Run as Root!!

sudo cp -rv ./desktop/chromium     /usr/lib/
sudo cp -rv ./desktop/chromium-dev /usr/lib/
