# Run `ball.sh` to create a little animation using these functions

# Create scene data for ball animation
def animate_ball($steps; $dt): foreach range($steps) as $step (.;
  .step = $step | .vy -= 2*$dt | .x += .vx*$dt | .y += .vy*$dt |
  # collision with the floor
  if .y < .r then .y = .r*2 - .y | .vy *= -0.9 end
);

# Render ball to RGB bitmap
def render_ball($w; $h):
  [1, 1, 1] as $black |
  range($h; 0; -1) as $y | # turn image upside down
  range($w       ) as $x |
  if pow(.x - $x; 2) + pow(.y - $y; 2) < pow(.r; 2)
  then .col
  else $black
  end;

# ASCII PPM header
def ppm_plain($w; $h): "P3\n\($w) \($h)\n255\n";
# Binary PPM header
def ppm_raw  ($w; $h): "P6\n\($w) \($h)\n255\n";
