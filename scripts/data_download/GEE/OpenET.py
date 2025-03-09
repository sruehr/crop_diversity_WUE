#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Thu Jun 20 09:58:53 2024

@author: sophieruehr
"""

import ee

# Trigger the authentication flow.
ee.Authenticate()

# Initialize the library.
ee.Initialize(project='ee-sophieruehr')

# ** Input desired information: area of interest, dates, months, resampling scale
# 1: ROI (Central Valley)
bbox = ee.FeatureCollection('projects/ee-sophieruehr/assets/central_valley_study_area')

# 2: Dates of interest
start_date = '2017-09-01'
end_date = '2023-01-01'

# 3: Months of interest 
first_month = 1
last_month = 12

# 5: Name of Google Drive folder to save to
folder = 'CV_at_SIF'

# 6: Variable of interest
variable = 'et_ensemble_mad'

# 7: Set export name
filename = 'OpenET_' + variable
print(filename)

# 8: Set name of collection
collection = 'OpenET/ENSEMBLE/CONUS/GRIDMET/MONTHLY/v2_0'

# ** Open data and visualize at native resolution
dataset = ee.ImageCollection(collection) \
    .filterDate(start_date, end_date) \
    .select(variable) \
    .filter(ee.Filter(ee.Filter.calendarRange(first_month, last_month, 'month')))

print(dataset)

# 9: Add SIF Turner 2021 grid for resampling
grid = ee.Image('projects/ee-sophieruehr/assets/CA_Turner_grid_CWE_2025')
bbox_geometry = bbox.geometry()
grid = grid.clip(bbox_geometry)
grid_projection = grid.projection()
grid_geometry = grid.geometry()

# ** Export: loop through each image + reduce resolution
# Add date property to collection
def addDateProperty(image):
    date = ee.Date(image.get('system:time_start'))
    return image.set('date', date.format('YYYY-MM'))

collectionWithDate = dataset.map(addDateProperty)
dates = collectionWithDate.aggregate_array('date').distinct().getInfo()

# Function to export images
def export_images_for_date(date):
    # Filter the collection for the specific date
    collection_filtered = collectionWithDate.filter(ee.Filter.eq('date', date)).select(variable)
    
    # Take the mean to get a single image for that month
    image_mean = collection_filtered.mean()
    
    # Clip the image to the desired region
    image_clipped = image_mean.clip(grid_geometry)
    
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
        folder=folder,
        description=f"{filename}_{date}",
        region=bbox.geometry().bounds().getInfo()['coordinates'],
        maxPixels=1e13
    )
    
    task.start()

# Loop over each date and call the export function
for date in dates:
    export_images_for_date(date)

print("All tasks are submitted.")
