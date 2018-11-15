## Copyright (C) 2008 Indrek Mandre <indrek(at)mare.ee>
## For more information please see http://www.mare.ee/indrek/octave/

## Compute:
##    view projection plane normal
##
## usage: [N] = camera3dn()
##
## Example:
##    view(3);
##    N=camera3dn()
##    N =
##      -0.52720  -0.68706   0.50000
##

## Author: Indrek Mandre <indrek(at)mare.ee>
## Created: 2008-07-28

function [normal] = camera3dn()
  [theta,phi]=view();
  phi = 90 - phi;
  theta = theta + 270;
  phi = phi * pi / 180;
  theta = theta * pi / 180;
  normal = [cos(theta)*sin(phi), sin(theta)*sin(phi), cos(phi)];
endfunction

