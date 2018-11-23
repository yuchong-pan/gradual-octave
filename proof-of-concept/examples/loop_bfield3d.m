## Copyright (C) 2008 Indrek Mandre <indrek(at)mare.ee>
## For more information please see http://www.mare.ee/indrek/octave/

## Compute:
##    magnetic field of a loop of current
##
## usage: [B] = loop_bfield3d(center, normal, radius, current, pos)
##
## center - a 3d vector pointing to where the loop is
## normal - a 3d normal vector for the current loop plane
## radius - radius of the loop
## current - going through the loop, in amperes
## pos - 3 column matrix where each row is a vector
## returns - a 3 column matrix of vector-rows of the corresponding
## field in teslas
##
## Example:
##  loop_bfield3d([0 0 0], [1 0 0], 0.15, 1, [0.2 0 0; 1 2 3])
##    9.0478e-07   0.0000e+00   0.0000e+00
##   -1.0611e-10   5.7977e-11   8.6965e-11


## Author: Indrek Mandre <indrek(at)mare.ee>
## Created: 2008-07-18
## Disclaimer: my third octave script

function [B] = loop_bfield3d(center, normal, radius, current, pos)
  len = size(pos, 1);
  v = pos - repmat(center, len, 1); % vector from the center of a coil to the point
  nvs = repmat(normal, len, 1); % replicate the normal vectors
  a = dot(v, nvs, 2); % axial distance, axial vector is the normal vector
  radial = v - nvs .* repmat(a, 1, 3);
  r = sqrt(dot(radial, radial, 2));
  radialn = radial ./ repmat(r, 1, 3);
  idx = find(any(r, 2) == 0);
  if (!isempty(idx))
    radialn(idx, :) = zeros(size(idx, 1), 3);
  endif
  [Br,Ba] = loop_bfield (radius, current, r, a);
  B = radialn .* repmat(Br,1,3) + nvs .* repmat(Ba,1,3);
endfunction

