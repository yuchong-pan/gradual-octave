## Copyright (C) 2008 Indrek Mandre <indrek(at)mare.ee>
## For more information please see http://www.mare.ee/indrek/octave/

## Compute:
##    magnetic field of a cube polywell
##
## usage: [B] = polywell_bfield(radius, spacing, current, pos)
##
## radius - radius of the coils
## spacing - the space between the coils at closest, so the
##   coils are 2 * (radius + spacing/sqrt(2)) apart
## current - in amperes
## pos - 3 column matrix where each row is a vector
## returns - a 3 column matrix of vector-rows of the corresponding
## field in teslas
##
## Example:
##  polywell_bfield(0.15, 0.08, 1, [0.2 0 0; 1 2 3])
##    -2.5042e-06   0.0000e+00   0.0000e+00
##    -1.5621e-13  -9.1672e-14   4.1274e-13

## Author: Indrek Mandre <indrek(at)mare.ee>
## Created: 2008-07-17
## Disclaimer: my second octave script and I've read only a bit of manual so far

function [B] = polywell_bfield(radius, spacing, current, pos)
  rr = radius + spacing / sqrt(2);
  B = loop_bfield3d([-rr,   0,    0], [ 1,   0,    0], radius, current, pos) + \
      loop_bfield3d([ rr,   0,    0], [-1,   0,    0], radius, current, pos) + \
      loop_bfield3d([  0, -rr,    0], [ 0,   1,    0], radius, current, pos) + \
      loop_bfield3d([  0,  rr,    0], [ 0,  -1,    0], radius, current, pos) + \
      loop_bfield3d([  0,   0,  -rr], [ 0,   0,    1], radius, current, pos) + \
      loop_bfield3d([  0,   0,   rr], [ 0,   0,   -1], radius, current, pos);
endfunction

