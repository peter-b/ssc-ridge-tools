from fisher import *
from numpy import *
from math import *
from scipy.stats import norm
from ridgeextract import RidgeExtraction
from scalespace import scalespace
from rtool import strength_Ngamma

class BrightLineBayes:
    def __init__(self, BL_params, nBL_params, prob_BL):

        self.A_L = nBL_params['n']
        self.A_M = -nBL_params['alpha']
        self.A_mu = sqrt(-nBL_params['gamma']/nBL_params['alpha'])
        self.A_mean = nBL_params['mu']
        self.A_sigma = nBL_params['sigma']

        self.B_L = BL_params['n']
        self.B_M = -BL_params['alpha']
        self.B_mu = sqrt(-BL_params['gamma']/BL_params['alpha'])
        self.B_mean = BL_params['mu']
        self.B_sigma = BL_params['sigma']

        self.threshold = log(prob_BL / (1.0 - prob_BL))

    def classify_point(self, brightness, strength):
        def lognorm_pdf(x, mu, sigma):
            return norm.pdf(log(x), mu, sigma)

        pr1A = fisher_pdf(brightness, self.A_L, self.A_M, self.A_mu)
        pr1B = fisher_pdf(brightness, self.B_L, self.B_M, self.B_mu)
        pr2A = lognorm_pdf(strength, self.A_mean, self.A_sigma)
        pr2B = lognorm_pdf(strength, self.B_mean, self.B_sigma)

        return log(pr1A/pr1B) + log(pr2A/pr2B) + self.threshold > 0

    def classify_image(self, image, scale=None):
        # Scale image
        if scale != None:
            image = scalespace(image, scale)

        Ra = [] # Not bright lines
        Rb = [] # Bright lines
        for r in RidgeExtraction(image).ridge_points():
            # Calculate N-gamma-norm strength metric
            r.strength = strength_Ngamma(r, scale)

            # Apply classification test
            if self.classify_point(r.value, r.strength):
                Rb.append(r)
            else:
                Ra.append(r)
        # Return tuple of (not-bright-lines, bright-lines)
        return (Ra, Rb)

def train(image, train_mask, scale=None, debug_file=None):
    # Scale image, if required
    if scale != None:
        image = scalespace(image, scale)

    # Using training mask to establish training classification of
    # ridge points in the image.
    Ra = [] # Not bright lines
    Rb = [] # Bright lines
    for r in RidgeExtraction(image).ridge_points():
        # Calculate N-gamma-norm strength metric
        r.strength = strength_Ngamma(r, scale)

        # Determine whether both mask pixels adjacent to this
        # ridge point are non-zero
        row1 = floor(r.row)
        col1 = floor(r.col)
        row2 = ceil(r.row)
        col2 = ceil(r.col)
        if train_mask[row1,col1] > 0 and train_mask[row2, col2] > 0:
            Rb.append(r)
        else:
            Ra.append(r)

    # Calculate probability distribution parameters for each class
    def lognorm_fit(data):
        N = len(data)
        Sx = 0
        Sx2 = 0
        for x in data:
            if x == 0:
                continue
            l = log(x)
            Sx = Sx + l
            Sx2 = Sx2 + l*l
        mu = Sx / N
        Var = (Sx2/N - mu*mu)
        return (mu, sqrt(Var))

    fparmsA = fisher_fit([x.value for x in Ra])
    fparmsB = fisher_fit([x.value for x in Rb])
    lnparmsA = lognorm_fit([x.strength for x in Ra])
    lnparmsB = lognorm_fit([x.strength for x in Rb])

    if debug_file != None:
        fp = file(debug_file, 'wb')
        for r in Ra:
            fp.write('%g, %g, %g, %g, %g\n' % (r.col, r.row,
                                               r.value, r.strength,
                                               0))
        for r in Rb:
            fp.write('%g, %g, %g, %g, %g\n' % (r.col, r.row,
                                               r.value, r.strength,
                                               1))
        fp.close()

    return ({'alpha': -fparmsB[1],
             'gamma': fparmsB[1]*fparmsB[2]**2,
             'n':     fparmsB[0],
             'mu':    lnparmsB[0],
             'sigma': lnparmsB[1]},
            {'alpha': -fparmsA[1],
             'gamma': fparmsA[1]*fparmsA[2]**2,
             'n':     fparmsA[0],
             'mu':    lnparmsA[0],
             'sigma': lnparmsA[1]},
            float(len(Rb)) / (len(Ra) + len(Rb)))

if __name__ == '__main__':
    import sys
    import tifffile

    try:
        image_filename = sys.argv[1]
        mask_filename = sys.argv[2]
        scale = float(sys.argv[3])
    except:
        print "You must specify a training image, mask, and ridge scale."
        sys.exit(1)

    try:
        page = tifffile.TIFFfile(image_filename)[0]
        if page.is_rgb:
            raise Exception()
        image = page.asarray().astype(float32)
    except:
        print "The training image must be a single-channel TIFF"
        sys.exit(2)

    try:
        page = tifffile.TIFFfile(mask_filename)[0]
        if page.is_rgb:
            raise Exception()
        mask = page.asarray().astype(float32)
    except:
        print "The mask must be a single-channel TIFF"
        sys.exit(2)

    BL_params, nBL_params, prob_B = train(image, mask, scale)
    print
    print "Classifier parameters"
    print "---------------------"
    print "Class\talpha\t\tgamma\t\tn\t\tmu\t\tsigma"
    print "B\t%.3e\t%.3e\t%.3e\t%.3e\t%.3e" % (BL_params['alpha'],
                                               BL_params['gamma'],
                                               BL_params['n'],
                                               BL_params['mu'],
                                               BL_params['sigma'])
    print "Not B\t%.3e\t%.3e\t%.3e\t%.3e\t%.3e" % (nBL_params['alpha'],
                                               nBL_params['gamma'],
                                               nBL_params['n'],
                                               nBL_params['mu'],
                                               nBL_params['sigma'])
    print
    print "P(B)\t%.3e" % prob_B
    print

    classifier = BrightLineBayes(BL_params, nBL_params, prob_B)
    classifier.classify_image(image, scale)
