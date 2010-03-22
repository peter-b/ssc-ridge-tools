function R = ridgedetect_fast(X, scale)                 % -*-Matlab-*-

  [filt_size, step] = params_for_scale(scale);
  printf("Filter: %d\nStep: %d\n", filt_size, step);
  II = make_integral_image (X);
  [Lq, Lqq] = build_metrics_fast(II, filt_size, step);
%  R = find_ridge_segments(Lq, Lqq, step);

  imagesc(Lq);

  if nargout ~= 0;
    return;
  end

  clf;
  subplot(1,2,1); imagesc(X);
  subplot(1,2,2); axis([0 size(X,2) 0 size(X,1)]);
  hold on;
  for i = 1:size(R,3);
    plot (R(2,:,i), R(1,:,i), "k");
  end
  hold off;
end

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

function R = find_ridge_segments(Lq, Lqq, step);
%% Calculate ridge segment positions

%% Consider a square of four pixels:

%%         a --- b
%%         |     |
%%         |     |
%%         c --- d

%% Each of the four edges is checked to see if they contain a zero of
%% Lq. If exactly two edges contain a zero of Lq, *and* neither of those
%% edges has Lqq > 0 for at either end, interpolate the zero crossings
%% in each edge and add a ridge segment.

  edge_defs = [0 0 1 0; 1 0 1 1; 1 1 0 1; 0 1 0 0];

  R = zeros(2,2,256);
  num_segments = 0;

  crossings = zeros(2);

  for row = 1:(size(Lq,1)-1);
    for col = 1:(size(Lq,1)-1);
      n_edges = 0;

      for k = 1:size(edge_defs,1);
        % Check for zero-crossing.
        lq1 = Lq(row+edge_defs(k,1),col+edge_defs(k,2));
        lq2 = Lq(row+edge_defs(k,3),col+edge_defs(k,4));

        if lq1*lq2 >= 0;
          continue;
        end

        % Interpolate zero-crossing
        delta = lq1 / (lq1 - lq2);

        lqq1 = Lqq(row+edge_defs(k,1),col+edge_defs(k,2));
        lqq2 = Lqq(row+edge_defs(k,3),col+edge_defs(k,4));

        % Check sign of Lqq
        if lqq1*(1-delta) + lqq2*delta < 0;
          continue;
        end

        % Check if we have too many edges already
        n_edges = n_edges + 1;
        if n_edges > 2;
          break;
        end

        % Add crossing point
        edge_start = edge_defs(k,1:2)';
        edge_end = edge_defs(k,3:4)';
        crossings(:,n_edges) = ...
            step*([row;col] + (1-delta)*edge_start + delta*edge_end - 0.5);
      end

      % We need exactly two edges.
      if n_edges ~= 2;
        continue;
      end

      num_segments = num_segments+1;
      if num_segments > size(R,3); % Extend R if necessary
        R = cat(3, R, zeros(2,2,256));
      end
      R(:,:,num_segments) = crossings;
    end
  end

  R = R(:,:,1:num_segments); % Truncate unused entries.
end

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

function [Lq, Lqq] = build_metrics_fast(II, f, step)
%% Calculate metrics. II is an integral image, f is the box filter
%% size, and step is the downsampling step.
  l = f/3;
  b = floor((f - 1)/2 + 1);
  d1 = 2*l+1;
  d2 = 2*l-1;
  l2 = floor(l/2);
  inv_f2 = f^-2;

  %% Pre-allocate metric matrices
  msize = ceil(size(II)/step);
  Lq = zeros(msize);
  Lqq = zeros(msize);

  %% Calculate metric at each point.
  for row = 1:msize(1);
    for col = 1:msize(2);
      r = (row-1*step)+1;
      c = (col-1*step)+1;

      %% Cheng etc.
      Dx = + box_integral(II, r - l, c - l, l,  d1) ...
           - box_integral(II, r + 1, c - l, l,  d1);
      Dy = + box_integral(II, r - l, c - l, d1, l) ...
           - box_integral(II, r - l, c + 1, d1, l);

      %% SURF
      Dxx = + box_integral(II, r - l + 1, c - b,     d2, f) ...
            - box_integral(II, r - l + 1, c - l2,    d2, l)*3;
      Dyy = + box_integral(II, r - b,     c - l + 1, f,  d2) ...
            - box_integral(II, r - l2,    c - l + 1, l,  d2)*3;

      Dxy = + box_integral(II, r - l, c + 1, l, l) ...
            + box_integral(II, r + 1, c - l, l, l) ...
            - box_integral(II, r - l, c - l, l, l) ...
            - box_integral(II, r + 1, c + 1, l, l);

      %% Normalise filter responses
      Dx  = Dx  * inv_f2;
      Dy  = Dy  * inv_f2;
      Dxx = Dxx * inv_f2;
      Dyy = Dyy * inv_f2;
      Dxy = Dxy * inv_f2;

      %% Find principal directions/curvatures
      [V, L] = eig([Dxx Dxy; Dxy Dyy]);
      if abs(L(1,1)) > abs(L(2,2));
        k = 1;
      else
        k = 2;
      end;
      %% Save maximum principal curvature.
      Lqq(row,col) = L(k,k);

      %% Save component of gradient in direction of max. principal curvature.
      Lq(row,col) = Dx*V(1,k) + Dy*V(2,k)
    end
  end
end

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

function [f, step] = params_for_scale(t)
%% Calculate filter size and downsampling at scale t.
%%
%% Exploits the fact that a 9x9 filter has t = 1.2^2, and that the
%% filter size must be of the form 6x+9
  p = round(3/2 * (t - 1)/(1.2)^2);
  f = 6*p + 9;

  %% Magic formula to determine downsampling step.
  s = max(1, floor(log(f/9*1.2)/log(2)));
  step = 2^(s-1);
end

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

function s = box_integral(II, row, col, rows, cols)
%% Calculate a box integral on an integral image
  im_height = size(II, 1);
  im_width = size(II, 2);

  r1 = min([row      im_height]) - 1;
  r2 = min([row+rows im_height]) - 1;
  c1 = min([col      im_width])  - 1;
  c2 = min([col+cols im_width])  - 1;

  s = 0;
  if (r1 >= 1) && (c1 >= 1); s = s + II(r1,c1); end
  if (r1 >= 1) && (c2 >= 1); s = s - II(r1,c2); end
  if (r2 >= 1) && (c1 >= 1); s = s - II(r2,c1); end
  if (r2 >= 1) && (c2 >= 1); s = s + II(r2,c2); end
end

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

function II = make_integral_image(X)
%% Calculate an integral image
  II = zeros(size(X));
  for row = 1:size(X,1);
    for col = 1:size(X,2);
      v = X(row,col);
      if row > 1;
        v = v + II(row-1,col);
      end
      if j > 1;
        v = v + II(row,col-1);
      end
      if (col > 1) && (row > 1);
        v = v - II(row-1,col-1);
      end
      II(row,col) = v;
    end
  end
end
