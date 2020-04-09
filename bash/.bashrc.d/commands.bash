k8c() { kubectl run-cmd --namespace "$1" --context production bundle exec rails console "$2"; }
k8c-s() { kubectl run-cmd --namespace "$1" --context staging bundle exec rails console "$2"; }
k8r() { kubectl run-cmd --namespace "$1" --context production bundle exec rake "$2" "${@:3}"; }
k8s() { kubectl "$2" --namespace "$1" -context prodution "${@:3}"; }
