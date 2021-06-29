# Evidence against 'secretive' Scaphiopus holbrookii
- Written by A. Devan-Song 2020 
- Corvallis, Oregon, USA 

This repo contains 6 .csv files and 4 .R files to reproduce Figure 3, 5, and appendix graphs for the upcoming manuscript in review at Journal of Herpetology (Devan-Song et al.)

## CSV descriptions 
    - SCHO_TRANSECT_R.csv 
        - transect info per survey 
    - Plot_data_for_graph.csv 
        - Plot info per survey 
    - ma3pred.csv 
        - detection probability at each transect survey; calculated from MA Walden's Code 
    - pond_points.csv 
        - GPS location of breeding pools
    - Transect_GPS.csv 
        - GPS points of each frog found during transect surveys
    - Transect_loc.csv
        - GPS location of start of transects 


## R Code descriptions 
- Code_to_produce_Fig3.R
    - Requires SCHO_TRANSECT_R.csv

- Code_to_produce_Fig_5.R
    - Requires Plot_data_for_graph.csv

- Appendix_weather_transect.R
    - Requires SCHO_Transec_R.csv

- Appendix_distance_from_pool.R
    - Requires 
        - ma3pred.csv
        - pond_points.csv
        - Transect_GPS.csv 
        - Transect_loc.csv