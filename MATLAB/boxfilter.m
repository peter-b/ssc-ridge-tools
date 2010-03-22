function s = boxfilter(I, row, col, ...                 % -*-Matlab-*-
                       startrow, startcol, endrow, endcol)
% BOXFILTER  Evaluate a box filter at a point in an integral image.
%
% s = boxfilter(I, row, col, startrow, endrow, startcol, endcol)
%
% INPUTS
%   I         Integral image
%   row, col  Position of box filter in image.
%   startrow  Relative offset of first row of image to include.
%   startcol  Relative offset of first col of image to include.
%   endrow    Relative offset of last row of image to include.
%   endcol    Relative offset of last col of image to include.
%
% OUTPUTS
%   s         Integral of pixel values in box.
%
% REFERENCES
%
%   Bay, Herbert, Andreas Ess, Tinne Tuytelaars and Luc Van Gool.
%   Speeded-up robust features (SURF). Computer Vision and Image
%   Understanding, 110(3):346 - 359, 2008.
%
% See also integralimage, boxintegral.

row = row+startrow
col = col+startcol

rows = endrow - startrow + 1
cols = endcol - startcol + 1

s = boxintegral(I,row,col,rows,cols);
