function ridgedemo(X, nlevels)                          % -*-Matlab-*-
% RIDGEDEMO  Plot ridges extracted at different scale levels
%
% ridgedemo(X, nlevels)
%
% INPUTS
%   X        Input image
%   nlevels  Number of scale levels to plot at.
%
% For each level n from 1 to nlevels, extracts ridges at a scale of
% 4^(n-1). Plots the filtered images in the left-hand column of the
% current figure, with extracted ridges in the right-hand column.
%
% See also ridgeextract, ridgeplot.

clf
for n = 1:nlevels;
  [R,Y] = ridgeextract(X,4^(n-1));

  subplot(nlevels,2,2*(n-1)+1);
  imagesc(Y);
  axis image;

  subplot(nlevels,2,2*n);
  ridgeplot(R);
  axis image;
  axis([0 size(X,2) 0 size(X,1)])
end
