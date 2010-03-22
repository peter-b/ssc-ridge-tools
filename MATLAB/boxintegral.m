function s = boxintegral(I, row, col, rows, cols)       % -*-Matlab-*-
% BOXINTEGRAL  Calculate a box integral on an integral image.
%
% s = boxintegral(I, row, col, rows, cols)
%
% INPUTS
%   I     Integral image.
%   row   First row in region.
%   col   First column in region.
%   rows  Number of rows in region.
%   cols  Number of columns in region.
%
% OUTPUTS
%   s     Integral of pixel values in region
%
% REFERENCES
%
%   Bay, Herbert, Andreas Ess, Tinne Tuytelaars and Luc Van Gool.
%   Speeded-up robust features (SURF). Computer Vision and Image
%   Understanding, 110(3):346 - 359, 2008.
%
% See also integralimage.

%% Subtraction of 1 is because row/col is inclusive.
d = size(I);
row1 = min([row,      d(1)]) - 1;
row2 = min([row+rows, d(1)]) - 1;
col1 = min([col,      d(2)]) - 1;
col2 = min([col+cols, d(2)]) - 1;

s = 0;
if (row1 > 0) && (col1 > 0);
  s = s + I(row1,col1);
end
if (row1 > 0) && (col2 > 0);
  s = s - I(row1,col2);
end
if (row2 > 0) && (col1 > 0);
  s = s - I(row2,col1);
end
if (row2 > 0) && (col2 > 0);
  s = s + I(row2,col2);
end

s = max([s 0]);
