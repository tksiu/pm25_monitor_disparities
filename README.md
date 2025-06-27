###  An adaptive kernel density estimator for PM<sub>2.5</sub> monitor density in eastern Canada with high-resolution model-derived surface concentrations for bandwidth selection
-----------------------------------------------------------------------------------------------------------------------------------------------------------------------

This repository presents the codes for implementing the analysis in the submitted manuscript "Disparities in fine particulate matter (PM<sub>2.5</sub>) monitoring in Eastern Canada".

This includes:

<ul>
  <li>data extraction of archived PurpleAir sensor data available from the "AQMap" developed by the UNBC (University of North British Columbia): https://aqmap.ca/aqmap/data/; </li>
  <li>finding the local-varying bandwidths based on the model-derived global surface PM<sub>2.5</sub> dataset from the Atmospheric Composition Analysis Group (ACAG) at WUSL (Washington Universtiy, St. Louis) at 1 km x 1 km resolution: https://sites.wustl.edu/acag/datasets/surface-pm2-5/#V6.GL.02; </li>
  <li>implementing the adaptive kernel density estimation (KDE) using the "spatstat" package in the R environment: https://www.rdocumentation.org/packages/spatstat/versions/1.64-1/topics/densityAdaptiveKernel </li>
</ul>

As deliverables, the findings can be used for: 
  1) identifying the PM<sub>2.5</sub> monitoring gap;
  2) linking the lack of PM<sub>2.5</sub> monitoring resources with social deprivations in environmental justice

Refer to below the package versions:
>>
> packageVersion("dplyr") <br>
[1] ‘1.1.4’ <br>
> packageVersion("stringr") <br>
[1] ‘1.5.1’ <br>
> packageVersion("reshape2") <br>
[1] ‘1.4.4’ <br>
> packageVersion("openxlsx") <br>
[1] ‘4.2.7.1’ <br>
> packageVersion("data.table") <br>
[1] ‘1.16.4’ <br>
> packageVersion("lubridate") <br>
[1] ‘1.9.4’ <br>
> packageVersion("ggplot2") <br>
[1] ‘3.5.1’ <br>
> packageVersion("sp") <br>
[1] ‘2.1.4’ <br>
> packageVersion("sf") <br>
[1] ‘1.0.19’ <br>
> packageVersion("tmap") <br>
[1] ‘3.3.4’ <br>
> packageVersion("ncdf4") <br>
[1] ‘1.23’ <br>
> packageVersion("raster") <br>
[1] ‘3.6.30’ <br>
> packageVersion("spatstat") <br>
[1] ‘3.3.0’ <br>
> packageVersion("basemaps") <br>
[1] ‘0.0.8’ <br>
> packageVersion("terra") <br>
[1] ‘1.7.83’ <br>
> packageVersion("tidyterra") <br>
[1] ‘0.6.1’ <br>
> packageVersion("exactextractr") <br>
[1] ‘0.10.0’ <br>
> packageVersion("lme4") <br>
[1] ‘1.1.35.5’ <br>
