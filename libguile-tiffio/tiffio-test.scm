;  libguile-tiffio: Guile interface to libtiff
;  Copyright (C) 2010 Peter TB Brett <peter@peter-b.co.uk>
;
;  This program is free software; you can redistribute it and/or modify
;  it under the terms of the GNU General Public License as published by
;  the Free Software Foundation; either version 2 of the License, or
;  (at your option) any later version.
;
;  This program is distributed in the hope that it will be useful,
;  but WITHOUT ANY WARRANTY; without even the implied warranty of
;  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;  GNU General Public License for more details.
;
;  You should have received a copy of the GNU General Public License along
;  with this program; if not, write to the Free Software Foundation, Inc.,
;  51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA.

(load-extension "libguile-tiffio" "init_tiffio")

(define A (make-typed-array 'f32 0 4 4))

(do ((i 0 (1+ i)))
    ((>= i 4))
  (array-set! A 10 i i))

(define fn (tmpnam))

(save-tiff A fn)

(define B (load-tiff fn))

(delete-file fn)

(if (array-equal? A B)
    (begin
      (display "PASSED: libguile-tiffio")
      (newline)
      (exit 0))
    (begin
      (display "FAILED: libguile-tiffio")
      (newline)
      (exit 1)))
