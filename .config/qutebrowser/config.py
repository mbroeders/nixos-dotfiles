# Open every tab as a new window, Vimb style
# c.tabs.tabs_are_windows = True
# c.tabs.show = "multiple"
c.tabs.last_close = "close"

c.auto_save.session = True
c.scrolling.smooth = True
c.session.lazy_restore = True
c.content.autoplay = False

# Scale pages and UI better for hidpi
c.zoom.default = "100%"
c.fonts.hints = "bold 20pt monospace"

# Better default fonts
c.fonts.web.family.standard = "Bitstream Vera Sans"
c.fonts.web.family.serif = "Bitstream Vera Serif"
c.fonts.web.family.sans_serif = "Bitstream Vera Sans"
c.fonts.web.family.fixed = "Fira Mono"
c.fonts.statusbar = "12pt Cantarell"

# Use dark mode where possible
#c.colors.webpage.darkmode.enabled = True
#c.colors.webpage.darkmode.policy.images = "never"
#c.colors.webpage.bg = "black"

# Automatically turn on insert mode when a loaded page focuses a text field
# c.input.insert_mode.auto_load = True

# Edit fields in Emacs with Ctrl+E
c.editor.command = ["emacsclient", "+{line}:{column}", "{file}"]

# Make Ctrl+g quit everything like in Emacs
config.bind('<Ctrl-g>', 'leave-mode', mode='insert')
config.bind('<Ctrl-g>', 'leave-mode', mode='command')
config.bind('<Ctrl-g>', 'leave-mode', mode='prompt')
config.bind('<Ctrl-g>', 'leave-mode', mode='hint')

# Tweak some keybindings
# config.unbind('d') # Don't close window on lower-case 'd'
config.bind('yy', 'yank')

# Vim-style movement keys in command mode
config.bind('<Ctrl-j>', 'completion-item-focus --history next', mode='command')
config.bind('<Ctrl-k>', 'completion-item-focus --history prev', mode='command')

# More binding hints here: https://gitlab.com/Kaligule/qutebrowser-emacs-config/blob/master/config.py
# Search Machines
c.url.searchengines = {'DEFAULT': 'https://duckduckgo.com/?q={}',
                       'g': 'https://google.com/search?q={}',
                       'r': 'https://reddit.com/r/{}',
                       "osm": "https://www.openstreetmap.org/search?query={}",
                       }

# Load the autoconfig file (quteconfig.py)
config.load_autoconfig()
