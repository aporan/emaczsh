# Install zsh
# Install OhMyZsh


# Install treesitter libc

# Download Emacs

brew install autoconf pkgconfig zlib
brew install libgccjit tree-sitter texinfo imagemagick gnutls gcc


# Install Emacs
git clone --depth 1 --branch emacs-30.2 https://github.com/emacs-mirror/emacs.git

cd emacs

./autogen.sh

# ./pkg-config cannot find the path to zlib.gc file even though it's installed
# because it's installed at a non-default path
export PKG_CONFIG_PATH="/opt/homebrew/opt/zlib/lib/pkgconfig:${PKG_CONFIG_PATH}"

CFLAGS="-O3 -pipe -mtune=native -march=native" ./configure --with-native-compilation=aot --with-tree-sitter  --without-sqlite3 --disable-gc-mark-trace --with-small-ja-dic --with-x-toolkit=lucid --with-cairo --with-xft --with-mailutils

make -j(nproc)

# zsh
brew install zsh
# ohmyzsh
sh -c "$(wget https://raw.githubusercontent.com/ohmyzsh/ohmyzsh/master/tools/install.sh -O -)"

cd ~.oh-my-zsh/custom/plugins/

for i in https://github.com/unixorn/fzf-zsh-plugin.git https://github.com/zsh-users/zsh-autosuggestions https://github.com/jeffreytse/zsh-vi-mode; do
    git clone $i
done

# emacs:: markdown support
brew install multimarkdown 
