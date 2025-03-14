# Crop diversification enhances water-use efficiency in California's Central Valley
This repository provides documentation on data processing and scripts to support our research on the effect of temporal crop diversification on water-use efficiency, gross primary productivity, and evapotranspiration.

## Requirements and installation instructions

To clone this repository to your local computer, paste the following line of code in your terminal. Cloning the repository should take about 1 minute. 

```bash
git clone https://github.com/sruehr/crop_diversity_WUE
```

This pipeline is written in both R (version 4.4.1) and Python (version 3.12.6). 

To reproduce the Python environment we use, follow the steps bellow: 

1. Install [Conda](http://conda.io/)

2. Create environment and install requirements

```bash
conda create -n crop_div_wue python=3.12.6 -y
conda activate crop_div_wue
pip install -r requirements.txt
pip install -U sklearn
```

## Pipeline overview
The pipeline contains 5 main steps. Code is in the `scripts` folder: 
1. Downloading data where necessary from Google Earth Engine (`scripts/data_download/GEE`) (Python)
2. Processing existing data into usable formats (`scripts/process_data`) (R)
   
     a. Intermediate formats (transforming datasets into standard 500m & monthly resolution)
   
     b. Final formats for analysis (combining, normalizing, and filtering data before analysis)

3. Running random forest and temporal gap analyses (`scripts/analysis`) (R)
4. Producing figures and tables (`scripts/generate_outputs`) (R)


## Acquiring the data
Data were downloaded from a variety of public sources. To run this code, users should download the same datafiles and insert them into the proper folders, currently empty with .gitkeep files as placeholders, in the repository. We do not upload these data here, as they are very large files (>20 GB total). Below is information on where to find these data and the variables they contain.
    
Some data were downloaded manually:
  - Annual DWR Statewde Crop Mapping (2018-2022): https://data.cnra.ca.gov/dataset/statewide-crop-mapping
  - Annual water table depth from Fan et al. 2013, DOI 10.1126/science.1229881: http://thredds-gfnl.usc.es/thredds/catalog/GLOBALWTDFTP/catalog.html
   - Daily solar-induced fluorescence data and GPP estimations are from Alex Turner et al. 2020 and 2021 (DOIs 10.5194/bg-17-405-2020 & 10.5194/bg-18-6579-2021), which were generously provided by the author. 

Google Earth Engine (GEE) was used to download additional datasets on evapotranspiration, soils, topography, crop cover, and monthly meteorologic variables. See below for Python scripts to download these at either constant, annual, or monthly timesteps. Below are the product names on GEE:
  - Monthly OpenET (evapotranspiration): `OpenET/ENSEMBLE/CONUS/GRIDMET/MONTHLY/v2_0`
  - Monthly GRIDMET (precip, VPD, TA, SRAD): `IDAHO_EPSCOR/GRIDMET`
  - Monthly GRIDMET Drought (SPI): `GRIDMET/DROUGHT`
  - Annual USDA Crop Data Layer (crop type & cultivated regions): `USDA/NASS/CDL`
  - Constant SOILGRIDS (clay, sand, silt fraction): `projects/soilgrids-isric/`

GEE requires user authentication before the Python scripts can be run. GEE data were processed using a study region shapefile (`data/study_area`) and a standardized grid, which were uploaded to GEE as assets.

## Running the code
Scripts should be run in the following order:
1. Download data from Google Earth Engine (all scripts in `scripts/data_download/GEE`)
2. Process data into intermediate forms and save to `data/processed_data` (all scripts in `scripts/process_data/intermediate`):
      
   a. `SIF_to_monthly.R`: convert SIF GPP estimates to mean monthly values. The output grid will serve as the target for following product processing.

   b. `wtd_to_raster.R`: convert Fan et al. 2013 into standardized raster data
   
   c. `CDL_to_species_diversity.R`: calculate species diversity for each _x,y_ pixel using CDL data over past 5 years + current year
   
   d. `CDL_to_legume_flag.R`: calculate number of legume plantings for each _x,y_ pixel using CDL data over past 5 years + current year
   
   e. `DWR_simplification.R`: simplify DWR 2018-2022 shapfile geometry and select features of interest
   
   f. `DWR_to_raster.R`: convert DWR crop maps 2018-2022 to rasters at 500m resolution and filter to unmixed pixels (>75% one crop type in pixel)

   g. `get_crop_acerage.R`: calculate acerage per crop and county for 2018-2022 based on DWR data
   
   h. `calculate_CWD.R`: calculate cumulative water deficit using precipitation and evapotranspiration for each water year 2018-2022 from OpenET and GRIDMET
   
4. Process data into final formats for analysis and save to `data/processed_data/for_analysis` (scripts in `scripts/process_data/final`):

   a. `combine_data.R`: combine all data into one file by month and _x,y_ pixel

   b. `filter_combined_data.R`: filter data to crops of interest, growing season, and normalize data by month, year, county, and crop for spatial anomalies
5. Run analysis (scripts in `scripts/analysis`):

   a. `random_forest.R`: run random forest models to predict WUE', GPP' and ET' and save outputs to `outputs/model_output`

   b. `gap.R`: run gap persistence analysis and save outputs to `outputs/gapyield_output`
6. Generate figures and tables (scripts in `scripts/generate_outputs`):

   a. `RF_models.R`: create regression, importance, and partial dependency plots and tables for WUE', GPP' and ET' models 

   b. `RF_maps.R`: create spatial maps of changes in WUE', GPP' and ET savings (mm) over California's Central Valley and tables with crop information 

   c. `gap_figures.R`: create plots and figures for temporal gap persistence analysis
