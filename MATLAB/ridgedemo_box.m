function ridgedemo_box(X, nlevels)                      % -*-Octave-*-
% RIDGEDEMO_BOX  Plot ridges extracted at different scale levels
%
% ridgedemo_box(X, nlevels)
%
% INPUTS
%   X        Input image
%   nlevels  Number of scale levels to plot at.
%
% For each level n from 1 to nlevels, extracts ridges at a scale of
% 4^(n-1), using box filters. Plots the filtered images in the
% left-hand column of the current figure, with extracted ridges in
% the right-hand column.
%
% See also ridgeextract_box.

clf
for n = 1:nlevels;
  [R,Y] = ridgeextract_box(X,4^(n-1));

  subplot(nlevels,2,2*(n-1)+1);
  imagesc(Y);
  axis image;

  subplot(nlevels,2,2*n);
  ridgeplot(R);
  axis image;
  axis([0 size(X,2) 0 size(X,1)])
end
