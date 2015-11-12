# This Site Has Changed!

The RSGHB model examples have moved to their own GitHub site: https://github.com/RSGInc/RSGHB_Examples

The 2014 ART Forum poster code has moved to its own GitHub site: https://github.com/RSGInc/ART2014_RSGHB

# The RSGHB Package

This package can be used to estimate models using a hierarchical Bayesian framework. The flexibility comes in allowing the user to specify the likelihood function directly instead of assuming predetermined model structures. Types of models that can be estimated with this code include the family of discrete choice models (Multinomial Logit, Mixed Logit, Nested Logit, Error Components Logit and Latent Class) as well ordered response models like ordered probit and ordered logit. In addition, the package allows for flexibility in specifying parameters as either fixed (non-varying across individuals) or random with continuous distributions. Parameter distributions supported include normal, positive log-normal, negative log-normal, positive truncated normal and the Johnson SB distribution.

Kenneth Train's Matlab and Gauss code for doing hierarchical Bayesian estimation has served as the basis for a few of the functions included in this package. These Matlab/Gauss functions have been rewritten to be optimized within R. Considerable code has been added to increase the flexibility and usability of the code base.   

Train's original Gauss and Matlab code can be found here: http://elsa.berkeley.edu/Software/abstracts/train1006mxlhb.html

See Train's chapter on HB in Discrete Choice with Simulation here: http://elsa.berkeley.edu/books/choice2.html; 

and his paper on using HB with non-normal distributions here: http://elsa.berkeley.edu/~train/trainsonnier.pdf

RSGHB and all its example files are covered by the GPL 3 license. 

See: https://github.com/jeffdumont/RSGHB/blob/master/LICENSE
