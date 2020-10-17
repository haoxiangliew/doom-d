#!/bin/bash
read -p "Have you installed git ripgrep and fd-find / fd (optional)? " -n 1 -r
echo
if [[ ! $REPLY =~ ^[Yy]$ ]]
then
    exit 1
fi

git clone https://github.com/hlissner/doom-emacs ~/.emacs.d
~/.emacs.d/bin/doom install

echo '' >> ~/.bashrc
echo '# add doom to PATH' >> ~/.bashrc
echo 'export PATH="$HOME/.emacs.d/bin:$PATH"' >> ~/.bashrc
