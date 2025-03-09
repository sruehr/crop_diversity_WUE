#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Thu Jun 20 13:24:05 2024

@author: sophieruehr
"""

import ee

# Trigger the authentication flow.
ee.Authenticate()

# Initialize the library.
ee.Initialize(project='ee-sophieruehr')

# Select soil property of interest (loop thru these manually)
var = 'clay' # 'sand', 'silt', 'clay', 'bdod

# ** Input desired information: area of interest, dates, months, resampling scale
# ROI (Central Valley)
bbox = ee.FeatureCollection('projects/ee-sophieruehr/assets/central_valley_study_area')

# Name of Google Drive folder to save to
folder = 'CV_at_SIF'

# Get image
image = 'projects/soilgrids-isric/' + var + '_mean'
variable = var + '_0-5cm_mean' # Lithology classes
filename = 'SOILGRIDS_' + var + 'mean_0-5cm'


# Get monthly value by taking mean or summing
if variable == 'bdod': # for bdod, different scaling
      scale_value = 0.01
else: 
      scale_value = 0.1

# Add SIF Turner 2021 grid for resampling
grid = ee.Image('projects/ee-sophieruehr/assets/CA_Turner_grid_CWE_2025')
bbox_geometry = bbox.geometry()
grid = grid.clip(bbox_geometry)
grid_projection = grid.projection()
grid_geometry = grid.geometry()

# Load the image
image = ee.Image(image).select(variable).multiply(scale_value)

# Clip the image to the desired region
image_clipped = image.clip(grid_geometry)

# Set default projection to ensure valid projection for reduceResolution
image_clipped = image_clipped.setDefaultProjection(grid_projection)

reprojected = image_clipped.reduceResolution(
      reducer=ee.Reducer.mean(),
      maxPixels=1024
    ).reproject(
      crs=grid_projection
    )
    

# Export the image to Google Drive
task = ee.batch.Export.image.toDrive(
    image=reprojected,
    description=f"{filename}",
    folder=folder,
    region=bbox.geometry().bounds().getInfo()['coordinates'],
    maxPixels=1e13
)

task.start()
