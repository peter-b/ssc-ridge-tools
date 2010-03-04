function Y = integralimage(X)                              % -*-Octave-*-
% INTEGRALIMAGE  Calculate an integral image.
%
% Y = imintegral(X)
%
% INPUTS
%   X    Image for which to calculate the integral image.
%
% OUTPUTS
%   Y    Integral image of X.
%
% Calculates the integral image for an input image, which can 
% then be used to perform fast box integrals.
%
% REFERENCES
%
%   Bay, Herbert, Andreas Ess, Tinne Tuytelaars and Luc Van Gool.
%   Speeded-up robust features (SURF). Computer Vision and Image
%   Understanding, 110(3):346 - 359, 2008.
%
% See also boxintegral.

Y = zeros(size(X));
for i = 1:size(X,1);
  for j = 1:size(X,2);
    v = X(i,j);
    if i > 1;
      v = v + Y(i-1,j);
    end
    if j > 1;
      v = v + Y(i,j-1);
    end
    if (i > 1) && (j > 1);
      v = v - Y(i-1,j-1);
    end
    Y(i,j) = v;
  end
end
