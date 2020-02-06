#include <math.h>
#include <R.h>

#define R_EARTH 6378.137
#define RADII_ADJ 0.85
#define NM_TO_KM 1.852

void find_storm_wind(int *storm_obs, double *slon, double *slat,
                   double *r_34_ne, double *r_50_ne, double *r_64_ne,
                   double *r_34_se, double *r_50_se, double *r_64_se,
                   double *r_34_sw, double *r_50_sw, double *r_64_sw,
                   double *r_34_nw, double *r_50_nw, double *r_64_nw,
                   int *counties, double *clon, double *clat,
                   double *dist, int *quadrant, double *v_sust,
                   double *peak_wind, double *wind_duration){

  // Initialize storm values
  double slon_rad[*storm_obs];
  double slat_rad[*storm_obs];

  // Initialize county values
  double clon_rad[*counties];
  double clat_rad[*counties];

  // Initialize intermediate values
  double diff_lon;
  double diff_lat;
  double h;

  // Calculate storm locations values in radians from values in degrees
  // Also, calculate 85% of wind radii, in kilometers (originally in nautical miles)
  int i;
  for (i = 0; i < *storm_obs; i++){
    slon_rad[i] = slon[i] * M_PI / 180.;
    slat_rad[i] = slat[i] * M_PI / 180.;

    r_34_ne[i] = r_34_ne[i] * NM_TO_KM * RADII_ADJ;
    r_50_ne[i] = r_50_ne[i] * NM_TO_KM * RADII_ADJ;
    r_64_ne[i] = r_64_ne[i] * NM_TO_KM * RADII_ADJ;
    r_34_se[i] = r_34_se[i] * NM_TO_KM * RADII_ADJ;
    r_50_se[i] = r_50_se[i] * NM_TO_KM * RADII_ADJ;
    r_64_se[i] = r_64_se[i] * NM_TO_KM * RADII_ADJ;
    r_34_sw[i] = r_34_sw[i] * NM_TO_KM * RADII_ADJ;
    r_50_sw[i] = r_50_sw[i] * NM_TO_KM * RADII_ADJ;
    r_64_sw[i] = r_64_sw[i] * NM_TO_KM * RADII_ADJ;
    r_34_nw[i] = r_34_nw[i] * NM_TO_KM * RADII_ADJ;
    r_50_nw[i] = r_50_nw[i] * NM_TO_KM * RADII_ADJ;
    r_64_nw[i] = r_64_nw[i] * NM_TO_KM * RADII_ADJ;
  }

  // Calculate county location values in radians from values in degrees
  int c;
  for (c = 0; c < *counties; c++) {
    clon_rad[c] = clon[c] * M_PI / 180.;
    clat_rad[c] = clat[c] * M_PI / 180.;
  }

  // Loop over every combination of storm location and county
  for(c = 0; c < *counties; c++){
    for(i = 0; i < *storm_obs; i++){
      diff_lat = slat_rad[i] - clat_rad[c];
      diff_lon = slon_rad[i] - clon_rad[c];

      h = sin(diff_lat / 2.0) * sin(diff_lat / 2.0) +
        cos(clat_rad[c]) * cos(slat_rad[i]) * sin(diff_lon / 2.0) * sin(diff_lon / 2.0);
      dist[i + (c * *storm_obs)] = 2.0 * R_EARTH * atan2(sqrt(h), sqrt(1.0 - h));

      // Assign quadrant for county at this point in the storm:
      // '1' = northeast
      // '2' = southeast
      // '3' = southwest
      // '4' = northwest
      if (clon[c] > slon[i] & clat[c] > slat[i]){ // County northeast of storm loc.
        quadrant[i + (c * *storm_obs)] = 1;
        if(dist[i + (c * *storm_obs)] > r_34_ne[i]){
          v_sust[i + (c * *storm_obs)] = 0.0;
        } else if(dist[i + (c * *storm_obs)] > r_50_ne[i]){
          v_sust[i + (c * *storm_obs)] = 34.0;
        } else if(dist[i + (c * *storm_obs)] > r_64_ne[i]){
          v_sust[i + (c * *storm_obs)] = 50.0;
        } else{
          v_sust[i + (c * *storm_obs)] = 64.0;
        }
      } else if (clon[c] > slon[i] & clat[c] <= slat[i]) { // County southeast of storm loc.
        quadrant[i + (c * *storm_obs)] = 2;
        if(dist[i + (c * *storm_obs)] > r_34_se[i]){
          v_sust[i + (c * *storm_obs)] = 0.0;
        } else if(dist[i + (c * *storm_obs)] > r_50_se[i]){
          v_sust[i + (c * *storm_obs)] = 34.0;
        } else if(dist[i + (c * *storm_obs)] > r_64_se[i]){
          v_sust[i + (c * *storm_obs)] = 50.0;
        } else{
          v_sust[i + (c * *storm_obs)] = 64.0;
        }
      } else if (clon[c] <= slon[i] & clat[c] <= slat[i]) { // County southwest of storm loc.
        quadrant[i + (c * *storm_obs)] = 3;
        if(dist[i + (c * *storm_obs)] > r_34_sw[i]){
          v_sust[i + (c * *storm_obs)] = 0.0;
        } else if(dist[i + (c * *storm_obs)] > r_50_sw[i]){
          v_sust[i + (c * *storm_obs)] = 34.0;
        } else if(dist[i + (c * *storm_obs)] > r_64_sw[i]){
          v_sust[i + (c * *storm_obs)] = 50.0;
        } else{
          v_sust[i + (c * *storm_obs)] = 64.0;
        }
        } else {// County northwest of storm loc.
        quadrant[i + (c * *storm_obs)] = 4;
        if(dist[i + (c * *storm_obs)] > r_34_nw[i]){
          v_sust[i + (c * *storm_obs)] = 0.0;
        } else if(dist[i + (c * *storm_obs)] > r_50_nw[i]){
          v_sust[i + (c * *storm_obs)] = 34.0;
        } else if(dist[i + (c * *storm_obs)] > r_64_nw[i]){
          v_sust[i + (c * *storm_obs)] = 50.0;
        } else{
          v_sust[i + (c * *storm_obs)] = 64.0;
        }
      }
        if(v_sust[i + (c * *storm_obs)] > peak_wind[c]){
          peak_wind[c] = v_sust[i + (c * *storm_obs)];
        }
        if(v_sust[i + (c * *storm_obs)] >= 34.0){
          wind_duration[c] = wind_duration[c] + 15.0; // Assumes 15-minute resolution of storm track
        }
    }
  }
}
