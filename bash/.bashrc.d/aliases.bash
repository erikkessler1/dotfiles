alias tree="tree -C"
alias serve="python3 -m http.server"
alias mirror="PORT=8081 http-echo-server"
alias oports="echo 'User:      Command:   Port:'; echo '----------------------------' ; lsof -i 4 -P -n | grep -i 'listen' | awk '{print \$3, \$1, \$9}' | sed 's/ [a-z0-9\.\*]*:/ /' | sort -k 3 -n |xargs printf '%-10s %-10s %-10s\n' | uniq"
alias myip="ifconfig | grep -Eo 'inet (addr:)?([0-9]*\.){3}[0-9]*' | grep -Eo '([0-9]*\.){3}[0-9]*'"
alias preview="fzf --preview 'bat --color \"always\" {}'"
alias sc="git diff --name-only master | cspell --config /Users/ekessler/dotfiles/resources/cspell.json --no-must-find-files --file-list stdin"

alias be="bundle exec"

alias aws="mv ~/Downloads/credentials ~/.aws/"
alias okta="ruby -e \"require 'rotp'; puts ROTP::TOTP.new(ENV.fetch('OKTA_2FA_KEY')).now\" | pbcopy"
alias adrwatch="fswatch -0 */XXXX-XX-XX-*.md | xargs -0 -I {} yarn prettier lint --write */XXXX-XX-XX-*.md"
