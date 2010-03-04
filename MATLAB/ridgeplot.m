function ridgeplot (R)                                  % -*-Octave-*-
% RIDGEPLOT  Plot extracted ridge segments
%
% ridgeplot(R)
%
% INPUTS
%   R    Ridge segment data to plot.
%
% Plots ridge segments extracted using ridgeextract.
%
% See also ridgeextract.

hold on
for i = 1:size(R,3);
  plot(R(2,:,i), R(1,:,i), 'k')
end
hold off

axis ij
axis auto
