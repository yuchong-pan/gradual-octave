## Copyright (C) 2008 Indrek Mandre <indrek(at)mare.ee>
## For more information please see http://www.mare.ee/indrek/octave/

## Compute:
##    circle 3D coordinates
##
## usage: [X, Y, Z] = circle3d(center, normal, radius)
##
## Example:
##    [X, Y, Z] = circle3d([0 0 0], [1 0 0], 1);
##

## Author: Indrek Mandre <indrek(at)mare.ee>
## Created: 2008-07-17

function [x: int, y: string, z: bool] = circle3d(center: int, normal: int, radius: int)
 x = center + radius
 y = x - normal ## this should fail
 z = true
endfunction

[X, Y, Z] = circle3d(1, 2, 3);
