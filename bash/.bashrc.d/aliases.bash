alias tree="tree -C"
alias be="bundle exec"
alias serve="python -m SimpleHTTPServer 8000"
alias oports="echo 'User:      Command:   Port:'; echo '----------------------------' ; lsof -i 4 -P -n | grep -i 'listen' | awk '{print \$3, \$1, \$9}' | sed 's/ [a-z0-9\.\*]*:/ /' | sort -k 3 -n |xargs printf '%-10s %-10s %-10s\n' | uniq"
alias myip="ifconfig wlan0 | grep 'inet ' | cut --delimiter=' ' -f12 | sed s/addr://"
alias em="/usr/local/Cellar/emacs-plus/25.3/bin/emacsclient"

alias pulse="kubectl --namespace pulse"
alias btf="kubectl --namespace below-the-fold"
