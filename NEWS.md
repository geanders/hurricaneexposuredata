# hurricaneexposuredata 0.1.0

* Add storms for 2016, 2017, and 2018. For these, we have storm tracks, storm winds, 
and flood and tornado events. These do not include storm rains, but we anticipate
adding rainfall for these storms in the future.
* Change to use storm tracks from IBTrACS database (except for wind radii, 
which come from HURDAT2---the IBTrACS and HURDAT2 data should be the same, although
IBTrACS includes data interpolated to 3 hours).
* Add a dataset (`excluded_tracks`) with the storm tracks that were excluded from
the main dataset. We only run the full wind model and generate other data for
storms that came within 250 km of at least one US county center. In this version
we included the tracking data for the storms that were excluded based on this 
restriction (in other words, storms that never came within 250 km of a 
county center in the continental US).
* Change interpolation methods for storm track interpolation, based on updates to
the stormwindmodel package. Maximum wind is now interpolated using linear splines.
Latitude and longitude are still interpolated using natural cubic splines, but
now using the R function 'spline'.
* Change interpolation method for wind radii estimates. There are three things that
need to be interpolated from 6-hour measurements to 15-minute intervals: (1) longitude 
position; (2) latitude position; and (3) wind radii values. We continue to use a cublic 
spline to interpolate longitude and  latitude positions. We have changed the interpolation 
method for the wind radii values. Previously, we used a cubic spline for this, as well. 
We now use a linear spline interpolation for the wind radii values.
For these interpolations, we use methods from Section 3.3 (pp. 107--110) in 
Press et al.'s "Numerical Recipes in Fortran 77: The Art of Scientific Computing"

# hurricaneexposuredata 0.0.2

* Finalized lists of authors, contributors, and funding support. 
* Finalized documentation for current data. 
* Added a `NEWS.md` file to track changes to the package.



