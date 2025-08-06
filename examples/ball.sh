#!/bin/bash
# Create bouncing ball animation
#
# This writes frames as binary PPM files, demonstrating jaq's binary writing.
mkdir -p ball
i=0
JQ="${JQ:-cargo run --release --quiet --}"
$JQ -nc -L . -n 'include "ball"; {x: 50, y: 200, r: 25, col: [0, 255, 0], vx: 2.5, vy: 0} | animate_ball(9600; 1/100) | select(.step % 100 == 0)' | while read line
do
  #echo $line
  file=ball/$(printf "%02d" $i).ppm
  echo $file
  echo $line | $JQ -L . --to raw 'include "ball"; 320 as $w | 240 as $h | [ppm_raw($w; $h), render_ball($w; $h)] | tobytes' > $file
  ((i++))
done
# `-plays 0` for looping, and `-y` for overwriting without asking
ffmpeg -r 24 -i ball/%02d.ppm -y -plays 0 -f apng ball.png
mplayer ball.png
