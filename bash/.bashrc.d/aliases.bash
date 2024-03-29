alias tree="tree -C"
alias serve="python -m SimpleHTTPServer 8000"
alias mirror="PORT=8081 http-echo-server"
alias oports="echo 'User:      Command:   Port:'; echo '----------------------------' ; lsof -i 4 -P -n | grep -i 'listen' | awk '{print \$3, \$1, \$9}' | sed 's/ [a-z0-9\.\*]*:/ /' | sort -k 3 -n |xargs printf '%-10s %-10s %-10s\n' | uniq"
alias myip="ifconfig | grep -Eo 'inet (addr:)?([0-9]*\.){3}[0-9]*' | grep -Eo '([0-9]*\.){3}[0-9]*'"
alias preview="fzf --preview 'bat --color \"always\" {}'"

alias be="bundle exec"
alias sr='docker-compose -f ~/sal/internal/pkg/sharedservices/service-registry.yml'
alias c="kubectl run-cmd -n content-flow-service --context production rails c --no-sandbox -- -- --nomultiline"
alias d="kubectl run-cmd -n dandelion --context production rails c --no-sandbox -- -- --nomultiline"

alias aws="mv ~/Downloads/credentials ~/.aws/"
alias okta="ruby -e \"require 'rotp'; puts ROTP::TOTP.new(ENV.fetch('OKTA_2FA_KEY')).now\" | pbcopy"
