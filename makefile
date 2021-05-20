BASE_PKGS = bat biber calibre cmus conky ctags diff-so-fancy easytag emacs evince \
	exa fzf gimp github-cli kitty mpc mpd mpv neofetch neomutt ncmpcpp redshift \
	ripgrep rsync stalonetray stow sxiv texlive-most the_silver_searcher tmux \
	tree vint vivaldi vivaldi-ffmpeg-codecs youtube-dl xboard \
	zathura zathura-pdf-mupdf zathura-ps

GTK_PKGS = gnome-disk-utility gnome-themes-extra gcolor2 parcellite transmission-gtk

PROG_PKGS = boost clang cmake extra-cmake-modules gdb ninja tree-sitter \
	ghc ghc-libs ghc-static \
	go go-tools gopls \
	lua ldoc \
	ipython jupyter-notebook python-matplotlib python-pip python-pygame \
	python-ueberzug python-language-server python-pycodestyle python-rope \
	python-pyflakes python-pylint python-pynvim \
	ruby-docs ruby-rake ruby-rdoc \
	bash-language-server npm texlab

AUR_HELPER = auracle-git

HOMIES=.ghc .cabal .local/bin

CONFIGS=emacs git gtk-2.0 gtk-3.0 mpd/playlists neomutt nnn nvim \
	radiotray-ng surf

DATAS= nvim/site/pack nvim/site/spell xorg fonts surf stack

STOW = git emacs neovim texlive music mail shell haskell x-window


help:
	@echo "Supported targets:"
	@echo ""
	@echo "help                    > Display this help message"
	@echo "pkgs                    > Install all the pacman packages"
	@echo "aur                     > Install Aur helper"
	@echo "filesystem              > Make directory for stowing"
	@echo "stow                    > Stow all the dotfiles packages"
	@echo "all                     > Install pkgs, aur_helper, prepare directory and stow"
	@echo ""

all: pkgs aur filesystem stow

pkgs: base_pkgs gtk_pkgs prog_pkgs

base_pkgs:
	sudo pacman -S --needed $(BASE_PKGS)

gtk_pkgs:
	sudo pacman -S --needed $(GTK_PKGS)

prog_pkgs:
	sudo pacman -S --needed $(PROG_PKGS)

aur:
	cd /tmp; git clone http://aur.archlinux.org/$(AUR_HELPER); cd $(AUR_HELPER); makepkg -sri

filesystem: home configs datas

home:
	mkdir -p $(addprefix $(HOME)/, $(HOMIES))

configs:
	mkdir -p $(addprefix $(XDG_CONFIG_HOME)/, $(CONFIGS))

datas:
	mkdir -p $(addprefix $(XDG_DATA_HOME)/, $(DATAS))

stow:
	stow -R $(STOW)


.PHONY: base_pkgs gtk_pkgs prog_pkgs pkgs aur stow filesystem homes configs datas
