SECR methods account for heterogeneity in detection probabilities across
individuals due to spatial effects; the closer an individual is located
to a detector, the more likely it is to be detected. Information about
an animal’s location is usually provided by the locations of detectors
that did and did not detect it. In some cases the detectors collect
additional data, such as estimated bearings or distances to the
individual, or an acoustic signal’s received strength or time of
arrival. Incorporating these auxiliary data into SECR models can
substantially increase the precision of abundance and density
estimators. With the deployment of increasingly sophisticated detection
devices, collection of these data are likely to become more commonplace.

`ascr` is an `R` package designed to maximize the use of such
information. Its aim is to make analysis of SECR surveys with auxiliary
data accessible to practitioners – as existing software lacks the
capacity to use most kinds of auxiliary data. Parameter estimation is by
maximum likelihood, and optimization is carried out using an AD Model
Builder executable, providing efficient, stable and accurate results.

Consider a survey wher individuals are detected across an array of *k*
remote detectors. For each detected individual a capture history is
obtained, indicating which detectors it was detected by. In addition one
may also observe a vector of auxiliary data (e.g., estimated bearings or
distances, or acoustic signal strengths or times of arrival)

The focus of \`ascr' is to estimate animal density, *D*, and parameters
*γ* (parameters of a detection function, *g*(*d*; *γ*)). This models the
probability of detection as a function of the distance between a
detector and an animal. This allows calculation of an estimated
detection surface, *p*(*x*; *γ*), which returns the probability of
detection by at least one of the *k* detectors for an individual located
at x, thus explaining heterogeneity in detectability due to animals’
locations.

If auxiliary data is used then we also estimate *ϕ*, which contains the
parameters of the probability density function of these data, given the
activity centre location.

There are three compulsory components required to fit a `ascr` model:
detecion information, containing the capture history data and auxiliary
data; the trap information, *k* pairs of the Cartesian coordinates for
each detector location; and the mask, a fine mesh of points spanning the
survey area, known as the habitat mask.

A mask is created by specifying a buffer distance, *b*, and a spacing
distance, *s*. The mask is then a grid of points, all of which are
within distance *b* of at least one detector, and are separated from one
another by distance *s* It is sensible to choose *b* as the maximum
distance at which a detection is feasibly possible. This is clearly
dependent on the species being detected (i.e., a distance of ∼2*k**m*
might be feasible for a gibbon but not a moss frog).
