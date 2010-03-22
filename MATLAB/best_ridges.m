function [R, G] = best_ridges(R, G, n)                  % -*-Matlab-*-
% BEST_RIDGES  Select the best n ridges from extracted ridges.
%
% [R, G] = best_ridges(R, G, n)
%
% INPUTS
%   R  Extracted ridge segments
%   G  Ridge segment quality metric values
%   n  Number of ridge segments to choose
%
% OUTPUTS
%   R  Selected ridge segments
%   G  Corresponding ridge quality metric values
%
% See also ridgeextract.

N = length(G);

% If there are already fewer than n ridge segments, do nothing.
if n >= N; return; end;

v = [1:N; -G]';

v = sortrows(v, 2);
v = v(1:n,1);

R = R(:,:,v);
G = G(v);
