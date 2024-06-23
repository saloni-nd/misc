November 2022

Global Gridded Relative Deprivation Index (GRDI), v1 (2010-2020)

PURPOSE

To provide a global gridded relative deprivation index characterizing the levels of multidimensional deprivation in each pixel at ~1 km resolution


DESCRIPTION

The Global Gridded Relative Deprivation Index (GRDI), Version 1 (GRDIv1) data set characterizes the relative levels of multidimensional deprivation and poverty in each 30 arc-second (~1 km) pixel, where a value of 100 represents the highest level of deprivation and a value of 0 the lowest. GRDIv1 is built from sociodemographic and satellite data inputs that were spatially harmonized, indexed, and weighted into six main components to produce the final index raster. Inputs were selected from the best-available data that either continuously vary across space or have at least administrative level 1 (provincial/state) resolution, and which have global spatial coverage. 

GRDIv1 has six input components, or dimensions, that are combined to determine the degree of relative deprivation. First, the Child Dependency Ratio (CDR) is defined as the ratio between the population of children (ages 0 to 14) to the working-age population (ages 15 to 64) where a higher ratio implies a higher dependency on the working population (UN DESA, 2006). CDR is interpreted as a dimension where higher dependency ratios, generally associated with younger age structures, imply higher relative deprivation. Second, Infant Mortality Rates (IMR), defined as the number of deaths in children under 1 year of age per 1,000 live births in the same year, are a common indicator of population health (Reidpath and Allotey, 2003; Schell et al., 2007). Higher IMRs imply higher deprivation. Third, the Subnational Human Development Index (SHDI) attempts to assess human well-being through a combination of “three dimensions: education, health, and standard of living (Smits and Permanyer, 2019)”. Lower SHDIs imply higher deprivation. Fourth, global rural populations are more likely to experience a higher degree of multidimensional poverty when compared to urban populations, other things being equal (Castañeda et al., 2018; Laborde Debucquet and Martin, 2018; Lee and Kind, 2021; UN DESA, 2021; UNDP and OPHI, 2020). Therefore, the ratio of built-up area to non-built up area (BUILT) is considered as a dimension where low values imply higher deprivation. The final two dimensions relate to the intensity of nighttime lights, which is closely associated with anthropogenic activities, economic output, and infrastructure development (Elvidge et al., 2007; Ghosh et al., 2013; Lu et al., 2021; Small et al., 2013). For the fifth component, the average intensity of nighttime lights for the year 2020 (Visible Infrared Imaging Radiometer Suite [VIIRS] Night Lights (VNL) 2020) is interpreted as a dimension where lower values imply higher deprivation. And for the sixth component, the slope of a linear regression was calculated from annual VNL data between 2012 and 2020 (VNL slope) where higher values (increasing brightness) imply decreasing deprivation and lower values (decreasing brightness) imply increasing deprivation. 

GRDIv1 data is available in 228 countries or regions. Building footprints for twenty-two countries and regions identified by NIDv4.11 were not available within the data sources used; therefore, they are omitted from GRDIv1 (Appendix 4 in the Documentation).


ACCESSING THE DATA

The data may be downloaded at https://sedac.ciesin.columbia.edu/data/set/povmap-grdi-v1/data-download

Permanent URL: https://doi.org/10.7927/3xxe-ap97

Documentation for the data may be downloaded at https://doi.org/10.7927/xwf1-k532


DATA FORMAT

The data are available in GeoTIFF format at ~1 km resolution. The downloadables are compressed zip files containing: 1) GeoTIFFs, 2) Readme.TXT file, and 3) PDF Documentation. The GRDIv1 GeoTIFFs are:

- povmap-grdi-v1.tif - Global Gridded Relative Deprivation Index (GRDI), Version 1
- povmap-grdi-v1_BUILT.tif - BUILT Component, indexed 0 to 100
- povmap-grdi-v1_CDR.tif - Child Dependency Ratio (CDR) Component, indexed 0 to 100
- povmap-grdi-v1_IMR.tif - Infant Mortality Rates (IMR) Component, indexed 0 to 100
- povmap-grdi-v1_SHDI.tif - Subnational Human Development Index (SHDI) Component, indexed 0 to 100
- povmap-grdi-v1_VNL-2020.tif - VIIRS Nighttime Lights (VNL) 2020 Component, indexed 0 to 100
- povmap-grdi-v1_VNL-slope.tif - VIIRS Nighttime Lights (VNL) Slope Component, indexed 0 to 100
- povmap-grdi-v1_FilledMissingValues-Count.tif - Raster showing count of components that were filled-in per grid cell using the Fill Missing Values tool

where: 
- main GRDIv1 raster: povmap-grdi-v1.tif
- GRDIv1 component rasters: povmap-grdi-v1_<VARIABLE-NAME>.tif


DATA VALUES

GRDIv1 is a floating point index value from 0 to 100, where a value of 100 represents the highest level of relative deprivation and a value of 0 the lowest.

Constituent rasters show their corresponding indexed value from 0 to 100, where a value of 100 represents the highest level of deprivation and a value of 0 the lowest. Filled Missing Values Count is a per cell count of inputs that were filled in using an average of the eight nearest neighbors. 


SPATIAL EXTENT

Global, Bounding Box:

West  -180.0    East  179.8
North   82.18    South -56.0

The data are provided in the WGS84 Geographic Coordinate System at a resolution of 30 arc-seconds (~1 km at the equator).


DISCLAIMER

CIESIN follows procedures designed to ensure that data disseminated by CIESIN are of reasonable quality. If, despite these procedures, users encounter apparent errors or misstatements in the data, they should contact SEDAC User Services at ciesin.info@ciesin.columbia.edu. Neither CIESIN nor NASA verifies or guarantees the accuracy, reliability, or completeness of any data provided. CIESIN provides this data without warranty of any kind whatsoever, either expressed or implied. CIESIN shall not be liable for incidental, consequential, or special damages arising out of the use of any data provided by CIESIN.


USE CONSTRAINTS

This work is licensed under the Creative Commons Attribution 4.0 International License (https://creativecommons.org/licenses/by/4.0). 

Users are free to use, copy, distribute, transmit, and adapt the work for commercial and non-commercial purposes, without restriction, as long as clear attribution of the source is provided.


RECOMMENDED CITATION(S)

Data Set:

Center for International Earth Science Information Network (CIESIN), Columbia University. 2022. Global Gridded Relative Deprivation Index (GRDI), Version 1. Palisades, New York: NASA Socioeconomic Data and Applications Center (SEDAC). https://doi.org/10.7927/3xxe-ap97. Accessed DAY MONTH YEAR.


ACKNOWLEDGEMENTS

Funding for development and dissemination of this data set was provided under the U.S. National Aeronautics and Space Administration (NASA) contract 80GSFC18C0111 for the continued operation of the Socioeconomic Data and Applications Center (SEDAC), which is operated by the Center for International Earth Science Information Network (CIESIN) of Columbia University.


REFERENCES

UN Department of Economic and Social Affairs (DESA). 2006. Dependency Ratio. https://www.un.org/esa/sustdev/natlinfo/indicators/methodology_sheets/demographics/dependency_ratio.pdf.

Reidpath, D. D., and P. Allotey. 2003. Infant mortality rate as an indicator of population health. Journal of Epidemiology and Community Health, 57(5), 344–346. https://doi.org/10.1136/jech.57.5.344.

Schell, C. O., M. Reilly, H. Rosling, S. Peterson, and A. M. Ekstrom. 2007. Socioeconomic determinants of infant mortality: A worldwide study of 152 low-, middle-, and high-income countries. Scandinavian Journal of Public Health, 35(3), 288–297. https://doi.org/10.1080/14034940600979171.

Smits, J., and I. Permanyer. 2019. The Subnational Human Development Database. Scientific Data, 6(1), 190038. https://doi.org/10.1038/sdata.2019.38.

Castañeda, A., D. Doan, D. Newhouse, M. C. Nguyen, H. Uematsu, and J. P. Azevedo. 2018. A New Profile of the Global Poor. World Development, 101(C), 250–267. https://econpapers.repec.org/article/eeewdevel/v_3a101_3ay_3a2018_3ai_3ac_3ap_3a250-267.htm, https://openknowledge.worldbank.org/handle/10986/29225.

Laborde Debucquet, D., and W. Martin. 2018. Implications of the global growth slowdown for rural poverty. Agricultural Economics, 49(3), 325–338. https://doi.org/10.1111/agec.12419.

Lee, Y. F., and M. Kind. 2021. Reducing poverty and inequality in rural areas: Key to inclusive development. https://www.un.org/development/desa/dspd/2021/05/reducing-poverty/.

UN Department of Economic and Social Affairs (DESA). 2021. World Social Report 2021: Reconsidering Rural Development. https://www.un.org/development/desa/dspd/world-social-report/2021-2.html.

UN Development Programme (UNDP) and OPHI. 2020. Global Multidimensional Poverty Index 2020—Charting pathways out of multidimensional poverty: Achieving the SDGs. https://hdr.undp.org/content/2020-global-multidimensional-poverty-index-mpi?utm_source=EN&utm_medium=GSR&utm_content=US_UNDP_PaidSearch_Brand_English&utm_campaign=CENTRAL&c_src=CENTRAL&c_src2=GSR&gclid=CjwKCAjwvsqZBhAlEiwAqAHElV3dFJACpXE9ZKPBfp5h5pqNs7I6CvpeTluouYvZj8-QWw8jlE4pdRoCxwYQAvD_BwE.

Elvidge, C. D., M. Zhizhin, Y. Ghosh, F.-C. Hsu, and J. Taneja. 2021. Annual Time Series of Global VIIRS Nighttime Lights Derived from Monthly Averages: 2012 to 2019. Remote Sensing, 13(5), 922. https://doi.org/10.3390/rs13050922.

Ghosh, T., S. J. Anderson, C. D. Elvidge, and P. Sutton. 2013. Using Nighttime Satellite Imagery as a Proxy Measure of Human Well-Being. Sustainability, 5(12), 4988–5019. https://doi.org/10.3390/su5124988.

Lu, D., Y. Wang, Q. Yang, K. Su, H. Zhang, and Y. Li. 2021. Modeling Spatiotemporal Population Changes by Integrating DMSP-OLS and NPP-VIIRS Nighttime Light Data in Chongqing, China. Remote Sensing, 13(2), 284. https://doi.org/10.3390/rs13020284.

Small, C., C. D. Elvidge, and K. Baugh. 2013. Mapping urban structure and spatial connectivity with VIIRS and OLS night light imagery. Joint Urban Remote Sensing Event 2013, 230–233. https://doi.org/10.1109/JURSE.2013.6550707.




