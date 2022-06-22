# Impacts of Marine Heatwaves to Phytoplankton
This fortran code is purposed to calculate MHWs(Marine Heat Waves) properties from daily SST data. 
This code detects individual MHWs in each grid that MHW properties, such as duration, intensity, frequency, are calculated.
This code is parallelized with openMP, which can be utilized with multi-core programming and is tested with 32 CPUs.

Marine heatwaes are firstly referred by Pearce et al 2011 and quantitively defined as prolonged, discrete anomalous warm water event (Hobday et al 2016). After the specific definition by Hobday, the related studies have attracted the attention of many researchers. Especially, the Oliver et al 2018 suggested that these extreme events have been intensified, extended and more frequently generated in recent 30 yeas. Not only for recent periods, climate models have projected the wider, stronger, longer MHWs either (Frolicher et al 2018). 

Deleterious impacts of MHWs to marine ecosystems have been reported, for example coral bleaching (Hughes et al 2017), toxic algal blooms (McCabe et al 2016, Roberts et al 2019),high mortality rates for seagrasses (Garrabou et al 2009, Arias-Ortiz et al 2017), seabirds (Jones et al 2018) and fish (Roberts et al 2019, Cheung and Frölicher 2020), which are all regarded as threats to biodiversity (Wernberg et al 2012, Smale et al 2019). Additionally, the phytoplankton, which consists the base of the marine ecosystem, has been investigated (Guta Sen et al 2020, Hayashida et al 2020, Le-Grix 2021). However, the global impact and the sensitivity of MHWs properties are not known well that the global chlorophyll responses to MHWs are suggested by our team.

Marine Heatwaves are detected and analyzed by the MHW_properties code and published to the Environmental Research Letters.

### Title : Global chlorophyll responses to marine heatwaves in satellite ocean color

Listed co-author(s) : Kyung Min Noh, Hyung-Gyu Lim, and Jong-Seong Kug

Corresponding Author : Hyung-Gyu Lim, and Jong-Seong Kug

### 1. Used Data
SST : National Oceanic and Atmospheric Administration (NOAA) 1/4◦ Optimum Interpolation daily SST version 2 (Reynolds et al 2007)
CHL : Ocean Colour Climate Change Initiative data 

### 2. Related papers
- Related paper : https://bit.ly/3bfyw7S
- Related presentation : https://bit.ly/3n94yoG

### 3. Output files
All output files are provided as netCDF format.
- SST Climatology, criteria percentile
- SST Anomaly
- MHWs Duration
- MHWs Intensity
- MHWs Cumulative Intensity
- MHWs Category
    
If you want more details,look at the EXAMPLE folder and compare what is different. 
