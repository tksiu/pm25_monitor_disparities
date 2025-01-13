###  An adaptive kernel density estimator for PM2.5 monitor density in eastern Canada with high-resolution model-derived surface concentrations for bandwidth selection
-----------------------------------------------------------------------------------------------------------------------------------------------------------------------

This repository presents the codes for implementing the analysis in the submitted manuscript "Disparities in fine particulate matter (PM2.5) monitoring in Eastern Canada".

This includes a data extraction of archived PurpleAir sensor data available from the "AQMap" developed by the UNBC (University of North British Columbia): https://aqmap.ca/aqmap/data/; finding the local-varying bandwidths based on the model-derived global surface PM2.5 dataset from the Atmospheric Composition Analysis Group (ACAG) at WUSL (Washington Universtiy, St. Louis) at 1 km x 1 km resolution: https://sites.wustl.edu/acag/datasets/surface-pm2-5/#V6.GL.02; and implementing the adaptive kernel density estimation (KDE) using the "spatstat" package in the R environment: https://www.rdocumentation.org/packages/spatstat/versions/1.64-1/topics/densityAdaptiveKernel. 

As deliverables, the findings can be used for: 
  1) identifying the PM2.5 monitoring gap;
  2) linking the lack of PM2.5 monitoring resources with social deprivations in environmental justice

