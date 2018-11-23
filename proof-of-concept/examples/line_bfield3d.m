## Copyright (C) 2008 Indrek Mandre <indrek(at)mare.ee>
## For more information please see http://www.mare.ee/indrek/octave/

## Compute:
##    magnetic field of a finite line segment
##
## Usage: [B] = line_bfield3d(q0, q1, t, current, pos);
##
## q0 - start point of the line segment 
## q1 - end point of the line segment
## t - unit vector on the line from q0 towards q1 - (q1-q0)/|(q1-q0)|
## current - current running in the line
## pos - position where to evaluate the field
## returns - a 3 column matrix of vector-rows of the corresponding
## field in teslas
##
## All 3D vectors (q0, q1, t, and pos) are represented as 3-column matrices.
## All arguments are vectorized.  Number of rows in q0, q1, t and current must match.
##
## Example (single line):
##  octave:1> line_bfield3d([0 0 -1], [0 0 1], [0 0 1], 1, [1 0 0])
##  ans =
##
##     0.0000e+00   1.4142e-07   0.0000e+00
##
## Example (multiple lines and multiple points):
##  octave:1> q0 = [-.1 0 -1; .1 0 -1];
##  octave:2> q1 = [-.1 0 1; .1 0 1];
##  octave:3> t = q1 - q0;
##  octave:4> t = t ./ repmat (sqrt(dot(t, t, 2)), 1, 3);
##  octave:5> line_bfield3d(q0, q1, t, [1; 1], [1 1 1; 2 2 2; 3 3 3])
##  ans =
##
##    -8.2157e-08   8.1205e-08   0.0000e+00
##    -1.9735e-08   1.9683e-08   0.0000e+00
##    -8.6567e-09   8.6469e-09   0.0000e+00

## Author: Indrek Mandre
## Created: 2008-08-06

function [B] = line_bfield3d(q0, q1, t, current, pos)
  tlen = rows(t);
  poslen = rows(pos);
  t = repmat (t, poslen, 1);
  q0 = repmat (q0, poslen, 1);
  q1 = repmat (q1, poslen, 1);
  current = repmat (current * 1e-7, poslen, 3);
  pos = pos(repmat(1:poslen, tlen, 1)(:), :);

  r0 = pos - q0;
  r1 = pos - q1;
  a = dot(t, r0, 2);
  b = -dot(t, r1, 2);
  r0len = sqrt(dot(r0, r0, 2));
  r1len = sqrt(dot(r1, r1, 2));

  % swap values to avoid singularities
  idx = find(b + r1len < 1e-5);
  [r0(idx,:), r1(idx,:)] = swap(r0(idx,:), r1(idx,:));
  [a(idx), b(idx)] = swap(a(idx), b(idx));
  [r0len(idx), r1len(idx)] = swap(r0len(idx), r1len(idx));

  r0n = r0 ./ repmat(r0len, 1, 3);
  r1n = r1 ./ repmat(r1len, 1, 3);

  bc = current .* cross(((r1n - t) ./ repmat(b + r1len, 1, 3) - (r0n - t) ./ repmat(r0len - a, 1, 3)), t, 2);
  B = reshape(sum(reshape(bc, tlen, poslen * 3), 1), poslen, 3);
endfunction

