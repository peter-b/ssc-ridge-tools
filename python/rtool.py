from ridgeextract import *
from scalespace import *
from tifffile import TIFFfile
from PIL import Image
import cairo, math
from optparse import OptionParser

def strength_Ngamma(r, scale):
    A = (r.Dxx + r.Dyy)**2
    B = (r.Dxx - r.Dyy)**2
    if scale == None:
        return A*(B + 4*r.Dxy**2)
    else:
        return scale**2 * A*(B + 4*r.Dxy**2)

def strength_Agamma(r, scale):
    return scale**2 * ((r.Dxx - r.Dyy)**2 + 4 * r.Dxy**2)

def load_and_scale(filename, scale):
    # Load image
    page = TIFFfile(filename)[0]
    if page.is_rgb:
        print("Only single-channel TIFs are supported.")
        sys.exit(2)
    image = page.asarray().astype(float32)

    # Scale image
    if scale != None:
        image = scalespace(image, scale)

    return image

def start_svg(filename, image):
    h = image.shape[0]
    w = image.shape[1]

    svg = cairo.SVGSurface(filename, w, h)
    cr = cairo.Context(svg)
    cr.set_line_width(0.5)
    cr.set_line_cap(cairo.LINE_CAP_ROUND)

    # Draw background
    cr.save()
    cr.rectangle(0, 0, w, h)
    cr.set_source_rgb(0, 0, 0)
    cr.stroke_preserve()
    cr.set_source_rgb(1, 1, 1)
    cr.fill()
    cr.restore()

    return (svg, cr)

def start_png(image):
    h = image.shape[0]
    w = image.shape[1]
    im = Image.new("1", (w, h))
    return im

def rtool_points(extractor, scale=None, svg_file=None, png_file=None,
                 output_file=None, min_strength=None, min_value=None):

    if output_file != None:
        out_fp = open(output_file, 'wb')
    if svg_file != None:
        svg, cr = start_svg(svg_file, image)
    if png_file != None:
        png = start_png(image);

    # Extracting ridges
    for r in extractor.ridge_points():
        # Cull step 2: value threshold
        if min_value != None:
            if r.value < min_value:
                continue
        r.strength = strength_Ngamma(r,scale)
        # Cull step 1: ridge strength threshold
        if min_strength != None:
            if r.strength < min_strength:
                continue

        if output_file != None:
            out_fp.write("%g, %g, %g, %g\n" % (r.col, r.row, r.value, r.strength))
        if svg_file != None:
            cr.arc(r.col, r.row, math.sqrt(1.0/math.pi), 0.0, 2*math.pi)
            cr.fill()
        if png_file != None:
            png.im.putpixel((int(floor(r.col)),int(floor(r.row))), 256)
            png.im.putpixel((int(ceil(r.col)),int(ceil(r.row))), 256)


    if output_file != None:
        out_fp.close()
    if svg_file != None:
        svg.finish()
    if png_file != None:
        png.save(png_file, "PNG")

def rtool_segments(extractor, scale=None, svg_file=None, png_file=None,
                   output_file=None, min_strength=None, min_value=None):

    if output_file != None:
        out_fp = open(output_file, 'wb')
    if svg_file != None:
        svg, cr = start_svg(svg_file, image)
    if png_file != None:
        png = start_png(image);

    # Extracting ridges. We use mean value and strength to implement
    # the cull thresholds.
    for r in extractor.ridge_segments():
        # Cull step 2: value threshold
        r.value = (r.start.value + r.end.value)/2
        if min_value != None:
            if r.value < min_value:
                continue
        r.start.strength = strength_Ngamma(r.start,scale)
        r.end.strength = strength_Ngamma(r.end,scale)
        r.strength = (r.start.strength + r.end.strength)/2
        # Cull step 1: ridge strength threshold
        if min_strength != None:
            if r.strength < min_strength:
                continue

        if output_file != None:
            out_fp.write("%g, %g, %g, %g, %g, %g\n" %
                         (r.start.col, r.start.row, r.end.col, r.end.row,
                          r.value, r.strength))
        if svg_file != None:
            cr.move_to(r.start.col, r.start.row)
            cr.line_to(r.end.col, r.end.row)
            cr.stroke()
        if png_file != None:
            png.im.putpixel((int(floor(r.start.col)),int(floor(r.start.row))),
                            256)

    if output_file != None:
        out_fp.close()
    if svg_file != None:
        svg.finish()
    if png_file != None:
        png.save(png_file, "PNG")

def rtool_lines(extractor, scale=None, svg_file=None, output_file=None,
                min_strength=None, min_value=None):
    pass

if __name__ == "__main__" :
    parser = OptionParser()
    parser.add_option("-p", "--points", action="store_true")
    parser.add_option("-s", "--scale", type="float",
                      help="scale level for extraction")
    parser.add_option("--min-strength", type="float",
                      help="Min strength of extracted ridges")
    parser.add_option("--min-value", type="float",
                      help="Min value of extracted ridges")
    parser.add_option("--svg",
                      help="filename for SVG output")
    parser.add_option("--png",
                      help="filename for PNG output")
    parser.add_option("-o", "--output",
                      help="filename for ridge CSV output")
    parser.add_option("-t", "--threads", type="int",
                      help="number of threads to use")

    (options, args) = parser.parse_args()
    if len(args) != 1:
        parser.error("incorrect number of arguments")
    filename = args[0]

    # Load image
    image = load_and_scale(filename, options.scale)

    # Create ridge extractor
    extractor = RidgeExtraction(image, options.threads)

    if options.points:
        rtool_points(extractor=extractor,
                     scale=options.scale,
                     svg_file=options.svg,
                     png_file=options.png,
                     output_file=options.output,
                     min_strength=options.min_strength,
                     min_value=options.min_value)
    else:
        rtool_segments(extractor=extractor,
                       scale=options.scale,
                       svg_file=options.svg,
                       png_file=options.png,
                       output_file=options.output,
                       min_strength=options.min_strength,
                       min_value=options.min_value)
