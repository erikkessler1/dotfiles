alias tree="tree -C"
alias be="bundle exec"
alias serve="python -m SimpleHTTPServer 8000"
alias mirror="PORT=8081 http-echo-server"
alias oports="echo 'User:      Command:   Port:'; echo '----------------------------' ; lsof -i 4 -P -n | grep -i 'listen' | awk '{print \$3, \$1, \$9}' | sed 's/ [a-z0-9\.\*]*:/ /' | sort -k 3 -n |xargs printf '%-10s %-10s %-10s\n' | uniq"
alias myip="ifconfig wlan0 | grep 'inet ' | cut --delimiter=' ' -f12 | sed s/addr://"
alias em="/usr/local/Cellar/emacs-plus/25.3/bin/emacsclient"
alias preview="fzf --preview 'bat --color \"always\" {}'"

alias zoo="$KAFKA_DIR/bin/zookeeper-server-start.sh $KAFKA_DIR/config/zookeeper.properties"
alias kafka="$KAFKA_DIR/bin/kafka-server-start.sh $KAFKA_DIR/config/server.properties"
alias audits_dev="cd /Users/ekessler/audits-service; docker run --rm --env \"KAFKA_HOST_PORTS=host.docker.internal:9092\" --env \"ELASTICSEARCH_URL=http://host.docker.internal:9200\" --env \"AVRO_SCHEMA_REGISTRY_URL=http://host.docker.internal:21000\" --name logstash audits-logstash & be rails server"

alias pulse="kubectl --namespace pulse"
alias btf="kubectl --namespace below-the-fold"

alias aws="mv ~/Downloads/credentials ~/.aws/"
alias pryc="bundle exec pry -r ./config/environment"

alias fixtime="sudo ntpdate -u time.apple.com"
