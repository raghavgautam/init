#!/usr/bin/env bash
#tmux new-session -s "storm-hb24" -n gateway \; send-keys "ssh storm-hb24-ssh.cloudapp.net" C-m \; neww -n "storm-hb24" \; send-keys "ssh storm-hb24-ssh.cloudapp.net" C-m "echo def" \; next-window
ssh_opt=${ssh_opt:-"-o StrictHostKeyChecking=no"}

[[ -n $1 ]] || {
    echo "usage: $0 <all_hosts file>";
    cat <<"EOF"
launches a tmux with multiple windows
  each window does ssh to a host
  each host is picked from file(passed as arg)
  file has one host on each line
$ssh_opt environment variable controls the default ssh option
$cmd environment variable controls the command that will be launched after ssh
  should not contain \;
EOF
    exit 1;
}
file=$1
session_name=${gateway%%.[-0-9a-z]*}
out="tmux new-session -n gateway \\;"
while read line
do
    host=$line
    win_name=${host%%.[a-z]*}
    one_window="neww -n '${win_name}' \\; send-keys 'ssh $ssh_opt $host' C-m"
    one_cmd="$cmd C-m"
    out="$out $one_window $one_cmd \\;"
done<"$file"
out="$out next-window"
echo $out
eval $out
