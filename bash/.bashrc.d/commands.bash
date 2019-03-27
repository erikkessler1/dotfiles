k8c() { salsifyk8s run cmd -a "$1" -e prod rails console "$2"; }
k8c-s() { salsifyk8s run cmd -a "$1" -e staging rails console "$2"; }
k8r() { salsifyk8s run cmd -a "$1" -e prod bundle exec rake "$2" "${@:3}"; }
k8s() { salsifyk8s "$2" -a "$1" -e prod "${@:3}"; }
