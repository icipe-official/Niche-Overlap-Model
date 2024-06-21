############ applying transition rules for the model@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@ 
# 1= An. gambiae complex, 2 = An. Funestus group 3 = An. pharoensis and 4 = An. coustani 


in_re_new1[(in_re_new1==0.5 &   ## in_re_new1==0.5 implies the neighborhood of the cell whose state is 1 (ie anopheles gambiae)
              build_up_raster_t_i >= 0.000 & build_up_raster_t_i <= 206.940 &  # this captures the conditions of the buildups ..
              temp_range_raster_t_i >= 18.233 & temp_range_raster_t_i <=  35.00 & 
              elevation_raster_t_i >= 0 & elevation_raster_t_i <= 2804.0 & 
              irrigation_raster_t_i >= 0 & irrigation_raster_t_i <= 1467.2 & 
              ndvi_raster_t_i >= -0.08956 & ndvi_raster_t_i <= 0.851 & 
              relative_humidity_raster_t_i >= 22.47 & relative_humidity_raster_t_i <= 89.730 &
              lulc_raster_t_i != 1 & lulc_raster_t_i != 3 & 
              precipitation_raster_t_i >= 0 & precipitation_raster_t_i <= 2138871)|   # | stands for or 
             (in_re_new1==0.5 & # this was included for temperatures above 35 degrees to know how other variables boundaries change to avoid over-generalization
                build_up_raster_t_i >= 0.000 & build_up_raster_t_i <= 206.940 & 
                temp_range_raster_t_i > 35 & temp_range_raster_t_i <=  37.74 & 
                elevation_raster_t_i >= 2 & elevation_raster_t_i <= 969.0 & 
                irrigation_raster_t_i >= 0 & irrigation_raster_t_i <= 364.78 & 
                ndvi_raster_t_i >= 0.07714 & ndvi_raster_t_i <= 0.5964 & 
                relative_humidity_raster_t_i >= 22.47 & relative_humidity_raster_t_i <= 63.02 & 
                lulc_raster_t_i != 1 & lulc_raster_t_i != 3 & 
                precipitation_raster_t_i >= 0 & precipitation_raster_t_i <= 2138871)
           |(species_data_raster_t_i==1)] = 1 # means assign neighboring cell (cell indentified as 0.5) which meets these conditions 1 for that specific timestep 

in_re_new1[(in_re_new1==1.5 &  ## in_re_new1==0.5 implies the neighborhood of the cell whose state is 2 (ie anopheles funestus)
              build_up_raster_t_i >= 0 & build_up_raster_t_i <= 202.800 & 
              temp_range_raster_t_i >= 21.29 & temp_range_raster_t_i <= 35.00 & 
              elevation_raster_t_i >= 0 & elevation_raster_t_i <= 2527.000 & 
              irrigation_raster_t_i >= 0 & irrigation_raster_t_i <= 1467.21 & 
              ndvi_raster_t_i >= -0.07879 & ndvi_raster_t_i <= 0.851 & 
              relative_humidity_raster_t_i >= 40.50 & relative_humidity_raster_t_i <= 88.61 & 
              lulc_raster_t_i != 1 & lulc_raster_t_i != 3 & 
              precipitation_raster_t_i >= 0 & precipitation_raster_t_i <= 2138871)|
             (in_re_new1==1.5 & # this was included for temperatures above 35 degrees to know how other variables boundaries change to avoid over-generalization
                build_up_raster_t_i >= 0 & build_up_raster_t_i <= 130.740 & 
                temp_range_raster_t_i >= 35 & temp_range_raster_t_i <= 37.12 & 
                elevation_raster_t_i >= 9.0 & elevation_raster_t_i <= 747.000 & 
                irrigation_raster_t_i >= 0 & irrigation_raster_t_i <= 225.02 & 
                ndvi_raster_t_i >= -0.07879 & ndvi_raster_t_i <= 0.851 & 
                relative_humidity_raster_t_i >= 40.50 & relative_humidity_raster_t_i <= 55.87 & 
                lulc_raster_t_i != 1 & lulc_raster_t_i != 3 & 
                precipitation_raster_t_i >= 0 & precipitation_raster_t_i <= 2138871)
           |(species_data_raster_t_i==2)] = 2 

in_re_new1[(in_re_new1==2.5 &  # for species 3 which is c, its neighbourhood is assigned 2.5
              build_up_raster_t_i >= 0 & build_up_raster_t_i <= 137.535 & 
              temp_range_raster_t_i >= 24.03 & temp_range_raster_t_i <= 35 & 
              elevation_raster_t_i >= 0 & elevation_raster_t_i <= 2031.0 & 
              irrigation_raster_t_i >= 0 & irrigation_raster_t_i <= 485.37 & 
              ndvi_raster_t_i >= -0.1335 & ndvi_raster_t_i <= 0.793 &
              relative_humidity_raster_t_i >= 38.14 & relative_humidity_raster_t_i <= 85.34 & 
              lulc_raster_t_i != 1 & lulc_raster_t_i != 3 & 
              precipitation_raster_t_i >= 0 & precipitation_raster_t_i <= 876076
)|
  (in_re_new1==2.5 & 
     build_up_raster_t_i >= 0 & build_up_raster_t_i <= 137.535 & 
     temp_range_raster_t_i > 35 & temp_range_raster_t_i <= 38.06 & 
     irrigation_raster_t_i >= 0 & irrigation_raster_t_i <= 485.37 & 
     ndvi_raster_t_i >= -0.1335 & ndvi_raster_t_i <= 0.793 & 
     relative_humidity_raster_t_i >= 38.14 & relative_humidity_raster_t_i <= 55.87 & 
     shrubs_raster_t_i >= 1 & shrubs_raster_t_i <= 328.90 &
     lulc_raster_t_i != 1 & lulc_raster_t_i != 3 & 
     precipitation_raster_t_i >= 0 & precipitation_raster_t_i <= 876076)
|(species_data_raster_t_i==3)] = 3 

in_re_new1[(in_re_new1==3.5 &  # for species 4 which is coustani, its neighbourhood is assigned 3.5
              build_up_raster_t_i >= 0 & build_up_raster_t_i <= 84.345 & 
              temp_range_raster_t_i >= 20.30 & temp_range_raster_t_i <= 35 & 
              elevation_raster_t_i >= 0.0 & elevation_raster_t_i <= 2056.0 & 
              irrigation_raster_t_i >= 0.0 & irrigation_raster_t_i <= 468.11 & 
              ndvi_raster_t_i >= -0.07879 & ndvi_raster_t_i <= 0.784 & 
              relative_humidity_raster_t_i >= 44.85 & relative_humidity_raster_t_i <= 84.61 & 
              lulc_raster_t_i != 1 & lulc_raster_t_i != 3 & 
              precipitation_raster_t_i >= 61 & precipitation_raster_t_i <= 2138871)
           |(in_re_new1==3.5 & 
               build_up_raster_t_i >= 0 & build_up_raster_t_i <= 84.345 & 
               temp_range_raster_t_i > 35 & temp_range_raster_t_i <= 37.29 & 
               elevation_raster_t_i >= 0.0 & elevation_raster_t_i <= 468.11 & 
               irrigation_raster_t_i >= 0.0 & irrigation_raster_t_i <= 485.37 & 
               ndvi_raster_t_i >= -0.07879 & ndvi_raster_t_i <= 0.784 & 
               relative_humidity_raster_t_i >= 44.85 & relative_humidity_raster_t_i <= 53.16 & 
               lulc_raster_t_i != 1 & lulc_raster_t_i != 3 & 
               precipitation_raster_t_i >= 61 & precipitation_raster_t_i <= 2138871)
           |(species_data_raster_t_i==4)] = 4 

### @@@@@@@@@@@@@@@@@@ continuation; this involves were we have intersections of neighbourhoods, or neighbourhood and cell occupied by a species but not a neighbourhood cell
in_re_new1[(in_re_new1==4.5 & 
              build_up_raster_t_i >= 0 & build_up_raster_t_i <= 202.800 & 
              temp_range_raster_t_i >= 21.290  & temp_range_raster_t_i <= 35.000 & 
              elevation_raster_t_i >= 0 & elevation_raster_t_i <= 2527.0 & 
              irrigation_raster_t_i >= 0  & irrigation_raster_t_i <= 1467.205 & 
              ndvi_raster_t_i >= -0.079 & ndvi_raster_t_i <= 0.851 & 
              relative_humidity_raster_t_i >= 40.500 & relative_humidity_raster_t_i <= 88.610 & 
              lulc_raster_t_i != 1 & lulc_raster_t_i != 3 & 
              precipitation_raster_t_i >= 0 & precipitation_raster_t_i <= 2138871)
           |(species_data_raster_t_i== 1| species_data_raster_t_i==2)] = 5

in_re_new1[(in_re_new1==4.5 & 
              build_up_raster_t_i >= 0.000 & build_up_raster_t_i <= 206.940 & 
              temp_range_raster_t_i >= 18.233 & temp_range_raster_t_i <=  35.00 & 
              elevation_raster_t_i >= 0 & elevation_raster_t_i <= 2804.0 & 
              irrigation_raster_t_i >= 0 & irrigation_raster_t_i <= 1467.2 & 
              ndvi_raster_t_i >= -0.08956 & ndvi_raster_t_i <= 0.851 & 
              relative_humidity_raster_t_i >= 22.47 & relative_humidity_raster_t_i <= 89.730 &
              lulc_raster_t_i != 1 & lulc_raster_t_i != 3 & 
              precipitation_raster_t_i >= 0 & precipitation_raster_t_i <= 2138871)|
             (in_re_new1==4.5 & 
                build_up_raster_t_i >= 0.000 & build_up_raster_t_i <= 206.940 & 
                temp_range_raster_t_i > 35 & temp_range_raster_t_i <=  37.74 & 
                elevation_raster_t_i >= 2 & elevation_raster_t_i <= 969.0 & 
                irrigation_raster_t_i >= 0 & irrigation_raster_t_i <= 364.78 & 
                ndvi_raster_t_i >= 0.07714 & ndvi_raster_t_i <= 0.5964 & 
                relative_humidity_raster_t_i >= 22.47 & relative_humidity_raster_t_i <= 63.02 & 
                lulc_raster_t_i != 1 & lulc_raster_t_i != 3 & 
                precipitation_raster_t_i >= 0 & precipitation_raster_t_i <= 2138871)
           |(species_data_raster_t_i==1)] = 1 

in_re_new1[(in_re_new1==4.5 &
              build_up_raster_t_i >= 0 & build_up_raster_t_i <= 202.800 & 
              temp_range_raster_t_i >= 21.29 & temp_range_raster_t_i <= 35.00 & 
              elevation_raster_t_i >= 0 & elevation_raster_t_i <= 2527.000 & 
              irrigation_raster_t_i >= 0 & irrigation_raster_t_i <= 1467.21 & 
              ndvi_raster_t_i >= -0.07879 & ndvi_raster_t_i <= 0.851 & 
              relative_humidity_raster_t_i >= 40.50 & relative_humidity_raster_t_i <= 88.61 & 
              lulc_raster_t_i != 1 & lulc_raster_t_i != 3 & 
              precipitation_raster_t_i >= 0 & precipitation_raster_t_i <= 2138871)|
             (in_re_new1==4.5 &
                build_up_raster_t_i >= 0 & build_up_raster_t_i <= 130.740 & 
                temp_range_raster_t_i >= 35 & temp_range_raster_t_i <= 37.12 & 
                elevation_raster_t_i >= 9.0 & elevation_raster_t_i <= 747.000 & 
                irrigation_raster_t_i >= 0 & irrigation_raster_t_i <= 225.02 & 
                ndvi_raster_t_i >= -0.07879 & ndvi_raster_t_i <= 0.851 & 
                relative_humidity_raster_t_i >= 40.50 & relative_humidity_raster_t_i <= 55.87 & 
                lulc_raster_t_i != 1 & lulc_raster_t_i != 3 & 
                precipitation_raster_t_i >= 0 & precipitation_raster_t_i <= 2138871)
           |(species_data_raster_t_i==2)] = 2 

### @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
in_re_new1[(in_re_new1==5.5 & 
              build_up_raster_t_i >= 0 & build_up_raster_t_i <= 137.535 & 
              temp_range_raster_t_i >= 24.030 & temp_range_raster_t_i <= 35 & 
              elevation_raster_t_i >= 0 & elevation_raster_t_i <= 2031.000 & 
              irrigation_raster_t_i >= 0 & irrigation_raster_t_i <= 485.37 & 
              ndvi_raster_t_i >= -0.090 & ndvi_raster_t_i <= 0.793 & 
              relative_humidity_raster_t_i >= 30.305 & relative_humidity_raster_t_i <= 85.340 & 
              lulc_raster_t_i != 1 & lulc_raster_t_i != 3 & 
              precipitation_raster_t_i >= 0 & precipitation_raster_t_i <= 876076)
           |(species_data_raster_t_i==1 & species_data_raster_t_i==3)] = 6

in_re_new1[(in_re_new1==5.5 & 
              build_up_raster_t_i >= 0.000 & build_up_raster_t_i <= 206.940 & 
              temp_range_raster_t_i >= 18.233 & temp_range_raster_t_i <=  35.00 & 
              elevation_raster_t_i >= 0 & elevation_raster_t_i <= 2804.0 & 
              irrigation_raster_t_i >= 0 & irrigation_raster_t_i <= 1467.2 & 
              ndvi_raster_t_i >= -0.08956 & ndvi_raster_t_i <= 0.851 & 
              relative_humidity_raster_t_i >= 22.47 & relative_humidity_raster_t_i <= 89.730 &
              lulc_raster_t_i != 1 & lulc_raster_t_i != 3 & 
              precipitation_raster_t_i >= 0 & precipitation_raster_t_i <= 2138871)|
             (in_re_new1==5.5 & 
                build_up_raster_t_i >= 0.000 & build_up_raster_t_i <= 206.940 & 
                temp_range_raster_t_i > 35 & temp_range_raster_t_i <=  37.74 & 
                elevation_raster_t_i >= 2 & elevation_raster_t_i <= 969.0 & 
                irrigation_raster_t_i >= 0 & irrigation_raster_t_i <= 364.78 & 
                ndvi_raster_t_i >= 0.07714 & ndvi_raster_t_i <= 0.5964 & 
                relative_humidity_raster_t_i >= 22.47 & relative_humidity_raster_t_i <= 63.02 & 
                lulc_raster_t_i != 1 & lulc_raster_t_i != 3 & 
                precipitation_raster_t_i >= 0 & precipitation_raster_t_i <= 2138871)
           |(species_data_raster_t_i==1)] = 1 

in_re_new1[(in_re_new1==5.5 & 
              build_up_raster_t_i >= 0 & build_up_raster_t_i <= 137.535 & 
              temp_range_raster_t_i >= 24.03 & temp_range_raster_t_i <= 35 & 
              elevation_raster_t_i >= 0 & elevation_raster_t_i <= 2031.0 & 
              irrigation_raster_t_i >= 0 & irrigation_raster_t_i <= 485.37 & 
              ndvi_raster_t_i >= -0.1335 & ndvi_raster_t_i <= 0.793 &
              relative_humidity_raster_t_i >= 38.14 & relative_humidity_raster_t_i <= 85.34 & 
              lulc_raster_t_i != 1 & lulc_raster_t_i != 3 & 
              precipitation_raster_t_i >= 0 & precipitation_raster_t_i <= 876076)|
             (in_re_new1==5.5 & 
                build_up_raster_t_i >= 0 & build_up_raster_t_i <= 137.535 & 
                temp_range_raster_t_i > 35 & temp_range_raster_t_i <= 38.06 & 
                elevation_raster_t_i >= 0 & elevation_raster_t_i <= 747.0 & 
                irrigation_raster_t_i >= 0 & irrigation_raster_t_i <= 485.37 & 
                ndvi_raster_t_i >= -0.1335 & ndvi_raster_t_i <= 0.793 & 
                relative_humidity_raster_t_i >= 38.14 & relative_humidity_raster_t_i <= 55.87 & 
                shrubs_raster_t_i >= 1 & shrubs_raster_t_i <= 328.90 &
                lulc_raster_t_i != 1 & lulc_raster_t_i != 3 & 
                precipitation_raster_t_i >= 0 & precipitation_raster_t_i <= 876076)
           |(species_data_raster_t_i==3)] = 3 

### @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
in_re_new1[(in_re_new1==6.5 & 
              build_up_raster_t_i >= 0 & build_up_raster_t_i <= 84.345 & 
              temp_range_raster_t_i >= 20.300 & temp_range_raster_t_i <= 35.00 & 
              elevation_raster_t_i >= 0 & elevation_raster_t_i <= 2056.000 & 
              irrigation_raster_t_i >= 0 & irrigation_raster_t_i <= 468.11 & 
              ndvi_raster_t_i >= -0.0845 & ndvi_raster_t_i <= 0.784 & 
              relative_humidity_raster_t_i >= 44.850 & relative_humidity_raster_t_i <= 84.610 & 
              lulc_raster_t_i != 1 & lulc_raster_t_i != 3 & 
              precipitation_raster_t_i >= 61 & precipitation_raster_t_i <= 2138871)|
             (species_data_raster_t_i==1 & species_data_raster_t_i==4)] = 7

in_re_new1[(in_re_new1==6.5 & 
              build_up_raster_t_i >= 0.000 & build_up_raster_t_i <= 206.940 & 
              temp_range_raster_t_i >= 18.233 & temp_range_raster_t_i <=  35.00 & 
              elevation_raster_t_i >= 0 & elevation_raster_t_i <= 2804.0 & 
              irrigation_raster_t_i >= 0 & irrigation_raster_t_i <= 1467.2 & 
              ndvi_raster_t_i >= -0.08956 & ndvi_raster_t_i <= 0.851 & 
              relative_humidity_raster_t_i >= 22.47 & relative_humidity_raster_t_i <= 89.730 &
              lulc_raster_t_i != 1 & lulc_raster_t_i != 3 & 
              precipitation_raster_t_i >= 0 & precipitation_raster_t_i <= 2138871)|
             (in_re_new1==6.5 & 
                build_up_raster_t_i >= 0.000 & build_up_raster_t_i <= 206.940 & 
                temp_range_raster_t_i > 35 & temp_range_raster_t_i <=  37.74 & 
                elevation_raster_t_i >= 2 & elevation_raster_t_i <= 969.0 & 
                irrigation_raster_t_i >= 0 & irrigation_raster_t_i <= 364.78 & 
                ndvi_raster_t_i >= 0.07714 & ndvi_raster_t_i <= 0.5964 & 
                relative_humidity_raster_t_i >= 22.47 & relative_humidity_raster_t_i <= 63.02 & 
                lulc_raster_t_i != 1 & lulc_raster_t_i != 3 & 
                precipitation_raster_t_i >= 0 & precipitation_raster_t_i <= 2138871)
           |(species_data_raster_t_i==1)] = 1 

in_re_new1[(in_re_new1==6.5 & 
              build_up_raster_t_i >= 0 & build_up_raster_t_i <= 84.345 & 
              temp_range_raster_t_i >= 20.30 & temp_range_raster_t_i <= 35 & 
              elevation_raster_t_i >= 0.0 & elevation_raster_t_i <= 2056.0 & 
              irrigation_raster_t_i >= 0.0 & irrigation_raster_t_i <= 468.11 & 
              ndvi_raster_t_i >= -0.07879 & ndvi_raster_t_i <= 0.784 & 
              relative_humidity_raster_t_i >= 44.85 & relative_humidity_raster_t_i <= 84.61 & 
              lulc_raster_t_i != 1 & lulc_raster_t_i != 3 & 
              precipitation_raster_t_i >= 61 & precipitation_raster_t_i <= 2138871)
           |(in_re_new1==6.5 & 
               build_up_raster_t_i >= 0 & build_up_raster_t_i <= 84.345 & 
               temp_range_raster_t_i > 35 & temp_range_raster_t_i <= 37.29 & 
               elevation_raster_t_i >= 0.0 & elevation_raster_t_i <= 468.11 & 
               irrigation_raster_t_i >= 0.0 & irrigation_raster_t_i <= 485.37 & 
               ndvi_raster_t_i >= -0.07879 & ndvi_raster_t_i <= 0.784 & 
               relative_humidity_raster_t_i >= 44.85 & relative_humidity_raster_t_i <= 53.16 & 
               lulc_raster_t_i != 1 & lulc_raster_t_i != 3 & 
               precipitation_raster_t_i >= 61 & precipitation_raster_t_i <= 2138871)
           |(species_data_raster_t_i==4)] = 4 

### @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
in_re_new1[(in_re_new1==7.5 & 
              build_up_raster_t_i >= 0 & build_up_raster_t_i <=  137.535 & 
              temp_range_raster_t_i >= 24.03 & temp_range_raster_t_i <= 35.00 & 
              elevation_raster_t_i >= 0 & elevation_raster_t_i <= 2031.000 & 
              irrigation_raster_t_i >= 0 & irrigation_raster_t_i <= 485.320 & 
              ndvi_raster_t_i >= -0.079 & ndvi_raster_t_i <= 0.793 & 
              relative_humidity_raster_t_i >= 40.500 & relative_humidity_raster_t_i <= 85.340 & 
              lulc_raster_t_i != 1 & lulc_raster_t_i != 3 & 
              precipitation_raster_t_i >= 0 & precipitation_raster_t_i <= 876076)
           |(species_data_raster_t_i==2 & species_data_raster_t_i==3)] = 8

in_re_new1[(in_re_new1==7.5 &
              build_up_raster_t_i >= 0 & build_up_raster_t_i <= 202.800 & 
              temp_range_raster_t_i >= 21.29 & temp_range_raster_t_i <= 35.00 & 
              elevation_raster_t_i >= 0 & elevation_raster_t_i <= 2527.000 & 
              irrigation_raster_t_i >= 0 & irrigation_raster_t_i <= 1467.21 & 
              ndvi_raster_t_i >= -0.07879 & ndvi_raster_t_i <= 0.851 & 
              relative_humidity_raster_t_i >= 40.50 & relative_humidity_raster_t_i <= 88.61 & 
              lulc_raster_t_i != 1 & lulc_raster_t_i != 3 & 
              precipitation_raster_t_i >= 0 & precipitation_raster_t_i <= 2138871)|
             (in_re_new1==7.5 &
                build_up_raster_t_i >= 0 & build_up_raster_t_i <= 130.740 & 
                temp_range_raster_t_i >= 35 & temp_range_raster_t_i <= 37.12 & 
                elevation_raster_t_i >= 9.0 & elevation_raster_t_i <= 747.000 & 
                irrigation_raster_t_i >= 0 & irrigation_raster_t_i <= 225.02 & 
                ndvi_raster_t_i >= -0.07879 & ndvi_raster_t_i <= 0.851 & 
                relative_humidity_raster_t_i >= 40.50 & relative_humidity_raster_t_i <= 55.87 & 
                lulc_raster_t_i != 1 & lulc_raster_t_i != 3 & 
                precipitation_raster_t_i >= 0 & precipitation_raster_t_i <= 2138871)
           |(species_data_raster_t_i==2)] = 2 

in_re_new1[(in_re_new1==7.5 & 
              build_up_raster_t_i >= 0 & build_up_raster_t_i <= 137.535 & 
              temp_range_raster_t_i >= 24.03 & temp_range_raster_t_i <= 35 & 
              elevation_raster_t_i >= 0 & elevation_raster_t_i <= 2031.0 & 
              irrigation_raster_t_i >= 0 & irrigation_raster_t_i <= 485.37 & 
              ndvi_raster_t_i >= -0.1335 & ndvi_raster_t_i <= 0.793 &
              relative_humidity_raster_t_i >= 38.14 & relative_humidity_raster_t_i <= 85.34 & 
              lulc_raster_t_i != 1 & lulc_raster_t_i != 3 & 
              precipitation_raster_t_i >= 0 & precipitation_raster_t_i <= 876076)|
             (in_re_new1==7.5 & 
                build_up_raster_t_i >= 0 & build_up_raster_t_i <= 137.535 & 
                temp_range_raster_t_i > 35 & temp_range_raster_t_i <= 38.06 & 
                elevation_raster_t_i >= 0 & elevation_raster_t_i <= 747.0 & 
                irrigation_raster_t_i >= 0 & irrigation_raster_t_i <= 485.37 & 
                ndvi_raster_t_i >= -0.1335 & ndvi_raster_t_i <= 0.793 & 
                relative_humidity_raster_t_i >= 38.14 & relative_humidity_raster_t_i <= 55.87 & 
                shrubs_raster_t_i >= 1 & shrubs_raster_t_i <= 328.90 &
                lulc_raster_t_i != 1 & lulc_raster_t_i != 3 & 
                precipitation_raster_t_i >= 0 & precipitation_raster_t_i <= 876076)
           |(species_data_raster_t_i==3)] = 3 

### @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
in_re_new1[(in_re_new1==8.5 & 
              build_up_raster_t_i >= 0 & build_up_raster_t_i <= 84.345 & 
              temp_range_raster_t_i >= 21.290 & temp_range_raster_t_i <= 35.00 & 
              elevation_raster_t_i >= 0 & elevation_raster_t_i <= 2056.000 & 
              irrigation_raster_t_i >= 0 & irrigation_raster_t_i <= 468.11 & 
              ndvi_raster_t_i >= -0.079 & ndvi_raster_t_i <= 0.784 & 
              relative_humidity_raster_t_i >= 44.850 & relative_humidity_raster_t_i <= 84.610 & 
              lulc_raster_t_i != 1 & lulc_raster_t_i != 3 & 
              precipitation_raster_t_i >= 61 & precipitation_raster_t_i <= 2138871)
           |(species_data_raster_t_i==2 & species_data_raster_t_i==4)] = 9

in_re_new1[(in_re_new1==8.5 &
              build_up_raster_t_i >= 0 & build_up_raster_t_i <= 202.800 & 
              temp_range_raster_t_i >= 21.29 & temp_range_raster_t_i <= 35.00 & 
              elevation_raster_t_i >= 0 & elevation_raster_t_i <= 2527.000 & 
              irrigation_raster_t_i >= 0 & irrigation_raster_t_i <= 1467.21 & 
              ndvi_raster_t_i >= -0.07879 & ndvi_raster_t_i <= 0.851 & 
              relative_humidity_raster_t_i >= 40.50 & relative_humidity_raster_t_i <= 88.61 & 
              lulc_raster_t_i != 1 & lulc_raster_t_i != 3 & 
              precipitation_raster_t_i >= 0 & precipitation_raster_t_i <= 2138871)|
             (in_re_new1==8.5 &
                build_up_raster_t_i >= 0 & build_up_raster_t_i <= 130.740 & 
                temp_range_raster_t_i >= 35 & temp_range_raster_t_i <= 37.12 & 
                elevation_raster_t_i >= 9.0 & elevation_raster_t_i <= 747.000 & 
                irrigation_raster_t_i >= 0 & irrigation_raster_t_i <= 225.02 & 
                ndvi_raster_t_i >= -0.07879 & ndvi_raster_t_i <= 0.851 & 
                relative_humidity_raster_t_i >= 40.50 & relative_humidity_raster_t_i <= 55.87 & 
                lulc_raster_t_i != 1 & lulc_raster_t_i != 3 & 
                precipitation_raster_t_i >= 0 & precipitation_raster_t_i <= 2138871)
           |(species_data_raster_t_i==2)] = 2 

in_re_new1[(in_re_new1==8.5 & 
              build_up_raster_t_i >= 0 & build_up_raster_t_i <= 84.345 & 
              temp_range_raster_t_i >= 20.30 & temp_range_raster_t_i <= 35 & 
              elevation_raster_t_i >= 0.0 & elevation_raster_t_i <= 2056.0 & 
              irrigation_raster_t_i >= 0.0 & irrigation_raster_t_i <= 468.11 & 
              ndvi_raster_t_i >= -0.07879 & ndvi_raster_t_i <= 0.784 & 
              relative_humidity_raster_t_i >= 44.85 & relative_humidity_raster_t_i <= 84.61 & 
              lulc_raster_t_i != 1 & lulc_raster_t_i != 3 & 
              precipitation_raster_t_i >= 61 & precipitation_raster_t_i <= 2138871)
           |(in_re_new1==8.5 & 
               build_up_raster_t_i >= 0 & build_up_raster_t_i <= 84.345 & 
               temp_range_raster_t_i > 35 & temp_range_raster_t_i <= 37.29 & 
               elevation_raster_t_i >= 0.0 & elevation_raster_t_i <= 468.11 & 
               irrigation_raster_t_i >= 0.0 & irrigation_raster_t_i <= 485.37 & 
               ndvi_raster_t_i >= -0.07879 & ndvi_raster_t_i <= 0.784 & 
               relative_humidity_raster_t_i >= 44.85 & relative_humidity_raster_t_i <= 53.16 & 
               lulc_raster_t_i != 1 & lulc_raster_t_i != 3 & 
               precipitation_raster_t_i >= 61 & precipitation_raster_t_i <= 2138871)
           |(species_data_raster_t_i==4)] = 4 

### @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
in_re_new1[(in_re_new1==9.5 & 
              build_up_raster_t_i >= 0 & build_up_raster_t_i <= 84.345 & 
              temp_range_raster_t_i >= 24.03 & temp_range_raster_t_i <= 35.00 & 
              elevation_raster_t_i >= 0 & elevation_raster_t_i <= 2031.000 & 
              irrigation_raster_t_i >= 0 & irrigation_raster_t_i <= 468.11 & 
              ndvi_raster_t_i >= -0.079 & ndvi_raster_t_i <= 0.784 & 
              relative_humidity_raster_t_i >= 44.850 & relative_humidity_raster_t_i <= 84.610 & 
              lulc_raster_t_i != 1 & lulc_raster_t_i != 3 & 
              precipitation_raster_t_i >= 61 & precipitation_raster_t_i <= 876076)
           |(species_data_raster_t_i==3 & species_data_raster_t_i==4)] = 10

in_re_new1[(in_re_new1==9.5 & 
              build_up_raster_t_i >= 0 & build_up_raster_t_i <= 137.535 & 
              temp_range_raster_t_i >= 24.03 & temp_range_raster_t_i <= 35 & 
              elevation_raster_t_i >= 0 & elevation_raster_t_i <= 2031.0 & 
              irrigation_raster_t_i >= 0 & irrigation_raster_t_i <= 485.37 & 
              ndvi_raster_t_i >= -0.1335 & ndvi_raster_t_i <= 0.793 &
              relative_humidity_raster_t_i >= 38.14 & relative_humidity_raster_t_i <= 85.34 & 
              lulc_raster_t_i != 1 & lulc_raster_t_i != 3 & 
              precipitation_raster_t_i >= 0 & precipitation_raster_t_i <= 876076)|
             (in_re_new1==9.5 & 
                build_up_raster_t_i >= 0 & build_up_raster_t_i <= 137.535 & 
                temp_range_raster_t_i > 35 & temp_range_raster_t_i <= 38.06 & 
                elevation_raster_t_i >= 0 & elevation_raster_t_i <= 747.0 & 
                irrigation_raster_t_i >= 0 & irrigation_raster_t_i <= 485.37 & 
                ndvi_raster_t_i >= -0.1335 & ndvi_raster_t_i <= 0.793 & 
                relative_humidity_raster_t_i >= 38.14 & relative_humidity_raster_t_i <= 55.87 & 
                shrubs_raster_t_i >= 1 & shrubs_raster_t_i <= 328.90 &
                lulc_raster_t_i != 1 & lulc_raster_t_i != 3 & 
                precipitation_raster_t_i >= 0 & precipitation_raster_t_i <= 876076)
           |(species_data_raster_t_i==3)] = 3 

in_re_new1[(in_re_new1==9.5 & 
              build_up_raster_t_i >= 0 & build_up_raster_t_i <= 84.345 & 
              temp_range_raster_t_i >= 20.30 & temp_range_raster_t_i <= 35 & 
              elevation_raster_t_i >= 0.0 & elevation_raster_t_i <= 2056.0 & 
              irrigation_raster_t_i >= 0.0 & irrigation_raster_t_i <= 468.11 & 
              ndvi_raster_t_i >= -0.07879 & ndvi_raster_t_i <= 0.784 & 
              relative_humidity_raster_t_i >= 44.85 & relative_humidity_raster_t_i <= 84.61 & 
              lulc_raster_t_i != 1 & lulc_raster_t_i != 3 & 
              precipitation_raster_t_i >= 61 & precipitation_raster_t_i <= 2138871)
           |(in_re_new1==9.5 & 
               build_up_raster_t_i >= 0 & build_up_raster_t_i <= 84.345 & 
               temp_range_raster_t_i > 35 & temp_range_raster_t_i <= 37.29 & 
               elevation_raster_t_i >= 0.0 & elevation_raster_t_i <= 468.11 & 
               irrigation_raster_t_i >= 0.0 & irrigation_raster_t_i <= 485.37 & 
               ndvi_raster_t_i >= -0.07879 & ndvi_raster_t_i <= 0.784 & 
               relative_humidity_raster_t_i >= 44.85 & relative_humidity_raster_t_i <= 53.16 & 
               lulc_raster_t_i != 1 & lulc_raster_t_i != 3 & 
               precipitation_raster_t_i >= 61 & precipitation_raster_t_i <= 2138871)
           |(species_data_raster_t_i==4)] = 4 

### @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
in_re_new1[(in_re_new1==10.5 & 
              build_up_raster_t_i >= 0 & build_up_raster_t_i <= 137.535 & 
              temp_range_raster_t_i >= 24.03 & temp_range_raster_t_i <= 35.00 & 
              elevation_raster_t_i >= 0 & elevation_raster_t_i <= 2031.000 & 
              irrigation_raster_t_i >= 0 & irrigation_raster_t_i <= 485.37 & 
              ndvi_raster_t_i >= -0.079 & ndvi_raster_t_i <= 0.793 & 
              relative_humidity_raster_t_i >= 40.500 & relative_humidity_raster_t_i <= 85.340 & 
              lulc_raster_t_i != 1 & lulc_raster_t_i != 3 & 
              precipitation_raster_t_i >= 0 & precipitation_raster_t_i <= 876076)
           |(species_data_raster_t_i==1 & species_data_raster_t_i==2 & species_data_raster_t_i==3)] = 11

### @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
in_re_new1[(in_re_new1==11.5 & 
              build_up_raster_t_i >= 0 & build_up_raster_t_i <= 84.345 & 
              temp_range_raster_t_i >= 19.941 & temp_range_raster_t_i <= 35.00 & 
              elevation_raster_t_i >= 0 & elevation_raster_t_i <= 2056.000 & 
              irrigation_raster_t_i >= 0 & irrigation_raster_t_i <= 468.11 & 
              ndvi_raster_t_i >= -0.079 & ndvi_raster_t_i <= 0.784 & 
              relative_humidity_raster_t_i >= 44.850 & relative_humidity_raster_t_i <= 84.610 & 
              lulc_raster_t_i != 1 & lulc_raster_t_i != 3 & 
              precipitation_raster_t_i >= 61 & precipitation_raster_t_i <= 2138871)
           |(species_data_raster_t_i==1 & species_data_raster_t_i==2 & species_data_raster_t_i==4)] = 12

### @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
in_re_new1[(in_re_new1==12.5 & 
              build_up_raster_t_i >= 0 & build_up_raster_t_i <= 84.345 & 
              temp_range_raster_t_i >= 24.03 & temp_range_raster_t_i <= 35.00 & 
              elevation_raster_t_i >= 0 & elevation_raster_t_i <= 2031.000 & 
              irrigation_raster_t_i >= 0 & irrigation_raster_t_i <= 468.110 & 
              ndvi_raster_t_i >= -0.079 & ndvi_raster_t_i <= 0.784 & 
              relative_humidity_raster_t_i >= 44.850 & relative_humidity_raster_t_i <= 84.610 & 
              lulc_raster_t_i != 1 & lulc_raster_t_i != 3 & 
              precipitation_raster_t_i >= 61 & precipitation_raster_t_i <= 2138871)
           |(species_data_raster_t_i==1 & species_data_raster_t_i==3 & species_data_raster_t_i==4)] = 13

### @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
in_re_new1[(in_re_new1==13.5 & 
              build_up_raster_t_i >= 0 & build_up_raster_t_i <= 84.345 & 
              temp_range_raster_t_i >= 24.03 & temp_range_raster_t_i <= 35.00 & 
              elevation_raster_t_i >= 0 & elevation_raster_t_i <= 2031.000 & 
              irrigation_raster_t_i >= 0 & irrigation_raster_t_i <= 468.110 & 
              ndvi_raster_t_i >= -0.079 & ndvi_raster_t_i <= 0.784 & 
              relative_humidity_raster_t_i >= 44.850 & relative_humidity_raster_t_i <= 84.610 & 
              lulc_raster_t_i != 1 & lulc_raster_t_i != 3 & 
              precipitation_raster_t_i >= 61 & precipitation_raster_t_i <= 876076)
           |(species_data_raster_t_i==2 & species_data_raster_t_i==3 & species_data_raster_t_i==4)] = 14

### @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
in_re_new1[(in_re_new1==14.5 & 
              build_up_raster_t_i >= 0 & build_up_raster_t_i <= 84.345 & 
              temp_range_raster_t_i >= 24.03 & temp_range_raster_t_i <= 35.00 & 
              elevation_raster_t_i >= 0 & elevation_raster_t_i <= 2031.000 & 
              irrigation_raster_t_i >= 0 & irrigation_raster_t_i <= 468.110 & 
              ndvi_raster_t_i >= -0.079 & ndvi_raster_t_i <= 0.784 & 
              relative_humidity_raster_t_i >= 44.850 & relative_humidity_raster_t_i <= 84.610 & 
              lulc_raster_t_i != 1 & lulc_raster_t_i != 3 & 
              precipitation_raster_t_i >= 61 & precipitation_raster_t_i <= 2138871)
           |(species_data_raster_t_i==1 & species_data_raster_t_i==2 & species_data_raster_t_i==3 & species_data_raster_t_i==4)] = 15
