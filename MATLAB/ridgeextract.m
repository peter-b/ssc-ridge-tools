function [R, Y] = ridgeextract(X, scale, step)          % -*-Matlab-*-
% RIDGEEXTRACT  Single-scale ridge extraction
%
% [R, Y] = ridgeextract(X, scale)
%
% INPUTS
%   X      Input image
%   scale  Variance of Gaussian low-pass filter to apply
%
% OUTPUTS
%   R      Array of extracted ridge segments
%   Y      Image with low-pass filter applie
%
% Single-scale ridge extraction algorithm.  A ridge is defined as a
% point for which the brightness assumes a maximum in the principal
% direction of curvature.  If the (p,q) axes at a given point P are
% defined as lieing parallel to the directions of principal
% curvature, then P is classified as a ridge point if:
%
%    Lq = 0, Lqq < 0, Lqq >= Lpp
%
% or:
%
%    Lp = 0, Lpp < 0, Lpp >= Lqq
%
%
% where Lx is the first derivative in the x direction and Lxx the 2nd
% derivative.  This is the same definition used in (Lindeberg, 1998)
% for multi-scale detection (in fact, the algorithm used here is
% identical to the algorithm described in that paper, except using
% only a single scale level).  The Gaussian low-pass filter and
% differencing filters used are as derived in (Lim and Stiehl, 2003).
%
% Ridge segments are found by considering the square formed by four
% adjacent pixels.  Each of the edges is checked for a sign change in
% Lq.  If a sign change is found, a zero-crossing point on the edge
% is found by linear interpolation.  If the curvature is positive,
% the edge is rejected.  If the square has exactly two edges with
% zero-crossing points, they are connected to define a ridge segment.
%
% The output R is a 2x2xN array, where N is the number of segments
% found.  Each column of the 2x2 matrix for a ridge segment is the
% column vector of one of its two endpoints.
%
% REFERENCES
%
%   Lindeberg, T. Edge detection and ridge detection with automatic
%   scale selection. International Journal of Computer Vision,
%   30(2):117--154, 1998.
%
%   Lim, J.Y. and H.S. Stiehl. A generalized discrete scale-space
%   formulation for 2-D and 3-D signals. Scale Space Methods in
%   Computer Vision:132--147, 2003
%
% See also ridgeplot, ridgedemo.

if (nargin < 3);
  % Very conservative downsampling.
  step = 2^(max(0, floor(log(scale)/log(2)) - 4));
end

DSS_KERNEL = [1/6 2/3 1/6];
DIFF_KERNEL = [1/2 0 -1/2];

n_ridge_lines = 0;
R = zeros(2,2,256);

%%%% Create low pass filter kernel
%%
%% This could be speeded up by using recursive self-convolution.
lpf_kern = DSS_KERNEL;
for t = 1:(3*scale);
  lpf_kern = conv(lpf_kern, DSS_KERNEL);
end


%%%% LPF the image
Y = conv2(lpf_kern, lpf_kern, X, 'same');


%%%% Calculate 1st & 2nd partial differences
Dx = conv2(1, DIFF_KERNEL, Y, 'same');
Dy = conv2(DIFF_KERNEL, 1, Y, 'same');
Dxx = conv2(1, DIFF_KERNEL, Dx, 'same');
Dyy = conv2(DIFF_KERNEL, 1, Dy, 'same');
Dxy = conv2(DIFF_KERNEL, 1, Dx, 'same');

%%%% Calculate metrics (Lq and Lqq)
%%
%% q is the unit vector in the most negative principal direction of
%% curvature. Lq is the component of gradient in that direction, and Lqq
%% is the corresponding curvature.
msize = ceil(size(Y)/step);
Lq = zeros(msize);
Lqq = zeros(msize);

for i = 1:msize(1);
  for j = 1:msize(2);
    si = (i-1)*step + 1;
    sj = (j-1)*step + 1;
    %% Calculate curvatures at (i,j) using the eigenvalues of the
    %% Hessian matrix. We choose the eigenvector corresponding with the
    %% greatest curvature *magnitude* as the direction of interest, but
    %% later we'll check that the eigenvalue is negative (what we care
    %% about are ridges rather than troughs, i.e. we're looking for
    %% maxima of value in the image).
    [V, L] = eig([Dxx(si,sj) Dxy(si,sj); Dxy(si,sj) Dyy(si,sj)]);
    if abs(L(1,1)) > abs(L(2,2)); k = 1; else; k = 2; end
    Lqq(i,j) = L(k,k);

    %% Find component of gradient in principal direction of curvature
    q1 = V(1,k); q2 = V(2,k);
    Lq(i,j) = Dx(si,sj)*q1 + Dy(si,sj)*q2;
  end
end

%%%% Find and print out ridge segments.
%%
%% Consider a square of four pixels:
%%
%%         a --- b
%%         |     |
%%         |     |
%%         c --- d
%%
%% Each of the four edges is checked to see if they contain a zero of
%% Lq. If exactly two edges contain a zero of Lq, *and* neither of those
%% edges has Lqq > 0 for at either end, interpolate the zero crossings
%% in each edge and draw a ridge segment.

edge_defs = [0 0 1 0; 1 0 1 1; 1 1 0 1; 0 1 0 0];

crossings = zeros(2);
for i = 1:(msize(1)-1);
  for j = 1:(msize(2)-1);
    n_edges = 0;
    bad_edges = 0;

    for k = 1:size(edge_defs, 1);
      lq1 = Lq(i+edge_defs(k,1),j+edge_defs(k,2));
      lq2 = Lq(i+edge_defs(k,3),j+edge_defs(k,4));
      lqq1 = Lqq(i+edge_defs(k,1),j+edge_defs(k,2));
      lqq2 = Lqq(i+edge_defs(k,3),j+edge_defs(k,4));

      if lq1*lq2 >= 0;  % Check for zero-crossing.
        continue;
      end

      if n_edges >= 2; % Too many zero-crossings?
        break;
      end

      % Interpolate zero-crossing
      delta = lq1 / (lq1 - lq2);

      if ((1-delta)*lqq1 + delta*lqq2) > 0; % Is it a trough?
        continue;
      end

      % Record zero-crossing point
      n_edges = n_edges + 1;

      edge_start = [i+edge_defs(k,1); j+edge_defs(k,2)];
      edge_end = [i+edge_defs(k,3); j+edge_defs(k,4)];
      crossings(:,n_edges) = step*((1 - delta)*edge_start + delta*edge_end);

    end

    if (n_edges ~= 2) || (bad_edges ~= 0);
      continue;
    end

    n_ridge_lines = n_ridge_lines + 1;
    if n_ridge_lines > size(R,3);
      R = cat(3,R,zeros(size(R)));
    end
    R(:,:,n_ridge_lines) = crossings;
  end
end

R = R(:,:,1:n_ridge_lines);


if nargout == 0;
  clf
  subplot(1,2,1);
  imagesc(Y);
  axis image;

  subplot(1,2,2);
  ridgeplot(R);
  axis image;
  axis([0 size(Y,2) 0 size(Y,1)])
end
end
