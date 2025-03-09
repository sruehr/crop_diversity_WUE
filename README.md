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
   
     a. Intermediate formats (transforming datasets into standard formats at 500m & monthly resolution)
   
     b. Final formats for analysis (combining, normalzing, and filtering data before analysis)

3. Running random forest and temporal gap analyses (`scripts/analysis`) (R)
4. Producing figures and tables (`scripts/generate_outputs`) (R)


## Acquiring the data
Data were downloaded from a variety of public sources. To run this code, users should download the same datafiles and insert them into the proper folders, currently empty, in the repository. We do not upload these data here, as they are very large files (>20 GB total). Below is information on where to find these data and the variables they contain.

Google Earth Engine stores data on evapotranspiration, soils, topography, crop cover, and monthly meteorologic variables. We use the `scripts/data_download/GEE` scripts to download these at either constant, annual, or monthly timesteps. Below are the product names on GEE:
  - Monthly OpenET (evapotranspiration): `OpenET/ENSEMBLE/CONUS/GRIDMET/MONTHLY/v2_0`
  - Monthly GRIDMET (precip, VPD, TA, SRAD): `IDAHO_EPSCOR/GRIDMET`
  - Monthly GRIDMET Drought (SPI): `GRIDMET/DROUGHT`
  - Annual USDA Crop Data Layer (crop type & cultivated regions): `USDA/NASS/CDL`
  - Constant SOILGRIDS (clay, sand, silt fraction): `projects/soilgrids-isric/`
Additional data were downloaded manually:
  - Annual DWR Statewde Crop Mapping (2018-2022): [https://data.cnra.ca.gov/dataset/statewide-crop-mapping]
  - Annual water table depth from Fan et al. 2013, DOI 10.1126/science.1229881: [http://thredds-gfnl.usc.es/thredds/catalog/GLOBALWTDFTP/catalog.html]

Daily solar-induced fluorescence data and GPP estimations are from Alex Turner et al. 2019 and 2021 (DOIs 10.5194/bg-18-6579-2021 & 10.5194/bg-17-405-2020), which were generously provided by the author.

We also provide the normalized, filtered dataset on which we run random forest and temporal gap analyses (`data/processed_data/for_analysis/filtered_data.csv`)
