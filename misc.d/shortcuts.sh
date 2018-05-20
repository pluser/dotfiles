#!/bis/sh

#alias pulse-remote='pacmd load-module module-esound-protocol-tcp auth-anonymous=1'
alias pulse-remote-server='pacmd load-module module-native-protocol-tcp auth-ip-acl="127.0.0.1;192.168.0.0/16"'
alias pulse-remote-client='pacmd load-module module-tunnel-sink-new server=Hektor.eska'
alias brightness="sudo sh -c 'echo 414 > /sys/class/backlight/intel_backlight/brightness'"
alias net='sudo rc-service net.wlp3s0 restart'
