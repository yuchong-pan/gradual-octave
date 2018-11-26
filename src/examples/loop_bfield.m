## Copyright (C) 2008 Indrek Mandre <indrek(at)mare.ee>
## For more information please see http://www.mare.ee/indrek/octave/

## Compute:
##    loop of current bfield radial component
##    loop of current bfield axial component
##
## usage: [Br, Ba] = loop_bfield(radius, current, r, a)

## Author: Indrek Mandre <indrek(at)mare.ee>
## Created: 2008-07-17
## Disclaimer: my first octave script and I haven't read a bit of manual yet
## Comments:
##    The r and a arguments are vectorized.
##    The ellipke for octave is within the special functions package in the octave-forge

function [Br, Ba] = loop_bfield(radius, current, r, a)
  f1 = 2 * 10^-7 * current;
  R = radius;
  q = r .* r + R .* R + a .* a + 2 .* r .* R;
  m = 4 .* r .* R ./ q;

  [K,E] = ellipke(m);
  sqrt_q = sqrt(q);
  f2 = q - 4 .* r .* R;
  Br = f1 .* a .* (E .* (q - 2 .* r .* R) ./ f2 - K) ./ (sqrt_q .* r);
  Ba = f1 .* (E .* (R .* R - r .* r - a .* a) ./ f2 + K) ./ sqrt_q;
  % recalculate the Br for very small m values
  idx = find(m < 5e-7);
  if (!isempty(idx))
    a = a(idx);
    rr = r(idx);
    m = repmat (5e-7, size(a));
    m2 = m .^ 2;
    % assuming given m and a, find cutoff r
    r = -(sqrt((4 .- m2) .* R^2 - 2 * R * a .* m2 - a .^ 2 .* m2) .- 2 * R) ./ m;
    q = r .* r + R .* R + a .* a + 2 .* r .* R;
    m = 4 .* r .* R ./ q;
    [K,E] = ellipke(m);
    sqrt_q = sqrt(q);
    f2 = q - 4 .* r .* R;
    % use linear interpolation from the cutoff radius  
    Br(idx) = (rr ./ r) .* f1 .* a .* (E .* (q - 2 .* r .* R) ./ f2 - K) ./ (sqrt_q .* r);
  endif
endfunction

