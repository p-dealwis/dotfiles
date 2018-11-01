#!/usr/bin/env sh

# Terminate already running bar instances
killall -q polybar

# Wait until the processes have been shut down
while pgrep -u $UID -x polybar >/dev/null; do sleep 1; done

# Launch bar1 and bar2
polybar i3-bar &
polybar i3-bar2 &
polybar i3-bar3 &

echo "Bars launched..."
