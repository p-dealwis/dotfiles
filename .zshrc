# Zshrc

# If not running interactively return
[[ $- != *i* ]] && return

export PATH=$HOME/bin:$PATH

setopt AUTO_CD BANG_HIST EXTENDED_HISTORY HIST_EXPIRE_DUPS_FIRST
setopt HIST_IGNORE_ALL_DUPS HIST_FIND_NO_DUPS HIST_IGNORE_DUPS
setopt HIST_IGNORE_SPACE HIST_REDUCE_BLANKS HIST_SAVE_NO_DUPS
setopt INC_APPEND_HISTORY SHARE_HISTORY

# Alias
alias l='ls'
alias la='ls -A'
alias ll='ls -lA'
alias ls='ls --color=auto'
alias upd='sudo pacman -Syyu'
alias pac='sudo pacman --color auto'
alias merge='xrdb -merge ~/.Xresources'
alias update-grub='sudo grub-mkconfig -o /boot/grub/grub.cfg'
alias mirrors='sudo reflector --score 100 --fastest 25 --sort rate --save /etc/pacman.d/mirrorlist --verbose'

# MINE
alias vssh='ssh vagrant@localhost -p 2222'
alias vup='cd /home/pramodya/Documents/gits/app-server && sudo vagrant up'
alias vhalt='cd /home/pramodya/Documents/gits/app-server && sudo vagrant halt'
alias gits='/home/pramodya/Documents/gits'
alias vpn='/home/pramodya/bin/vpn.sh'
alias suspend='systemctl suspend'
alias zshrc='vim /home/pramodya/.zshrc'
alias ezyvet='/home/pramodya/Documents/gits/app-server/code/ezyvet/'

neofetch

# tabtab source for serverless package
# uninstall by removing these lines or running `tabtab uninstall serverless`
[[ -f /home/pramodya/Downloads/nodejs-serverless/pkg/nodejs-serverless/usr/lib/node_modules/serverless/node_modules/tabtab/.completions/serverless.zsh ]] && . /home/pramodya/Downloads/nodejs-serverless/pkg/nodejs-serverless/usr/lib/node_modules/serverless/node_modules/tabtab/.completions/serverless.zsh
# tabtab source for sls package
# uninstall by removing these lines or running `tabtab uninstall sls`
[[ -f /home/pramodya/Downloads/nodejs-serverless/pkg/nodejs-serverless/usr/lib/node_modules/serverless/node_modules/tabtab/.completions/sls.zsh ]] && . /home/pramodya/Downloads/nodejs-serverless/pkg/nodejs-serverless/usr/lib/node_modules/serverless/node_modules/tabtab/.completions/sls.zsh
