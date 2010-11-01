from math import *
from numpy import *
from scipy.special import polygamma, psi
from scipy.optimize import fmin_slsqp, fsolve
from scipy.stats import f

def fisher_fit(data):
    # Calculate log-cumulants
    N = 0
    k1 = 0
    for index, x in ndenumerate(data):
        if x == 0:
            continue
        N = N + 1
        k1 = k1 + log(x)
    k1 = 2 * k1 / N

    k2 = 0
    k3 = 0
    for index, x in ndenumerate(data):
        if x == 0:
            continue
        y = 2*log(x) - k1
        k2 = k2 + y**2
        k3 = k3 + y**3
    k2 = k2/N
    k3 = k3/N

    # Find L and M by optimisation
    def objective(p):
        A = polygamma(1,p[0]) + polygamma(1,p[1]) - k2
        B = polygamma(2,p[0]) - polygamma(2,p[1]) - k3
        return array([A,B])

    def objective_deriv(p):
        psiL = [polygamma(n,p[0]) for n in range(1,4)]
        psiM = [polygamma(n,p[1]) for n in range(1,4)]

        return array([[ polygamma(2,p[0]), polygamma(2,p[1]) ],
                      [ polygamma(3,p[0]), polygamma(3,p[1])]],
                     float)

    p = fsolve(objective, array([2,2]),
               fprime=objective_deriv, col_deriv=True)
#    p = fsolve(objective, array([10,10]))

    L = p[0]
    M = p[1]

    # Find mu by exact solution
    mu = exp((k1 - (psi(L) - log(L)) + (psi(M) - log(M)))/2)

    return (L,M,mu)

def fisher_pdf(x,L,M,mu):
    return (2*x)/(mu*mu) * f.pdf(x/mu,2*L,2*M)

if __name__ == '__main__':

    import tifffile
    import sys

    # Load image
    try:
        filename = sys.argv[1]
    except:
        print "You must specify a TIFF image filename"
        sys.exit(1)
    page = tifffile.TIFFfile(filename)[0]
    if page.is_rgb:
        print "Only single-channel TIFFs are supported."
        sys.exit(2)
    image = page.asarray().astype(float32)

    print fisher_fit(image)
