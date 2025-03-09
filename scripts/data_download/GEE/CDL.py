#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Fri Jul 19 10:06:37 2024

@author: sophieruehr
"""

import ee

# Trigger the authentication flow.
# ee.Authenticate()

# Initialize the Earth Engine library.
ee.Initialize(project='ee-sophieruehr')

# ******** USER-DEFINED INPUTS ************* #
# 1: ROI
    # Davis
# bbox = ee.Geometry.Rectangle([-122, 38.3, -121.4, 38.8])  # [xmin, ymin, xmax, ymax]
    # Central Valley
bbox = ee.FeatureCollection('projects/ee-sophieruehr/assets/central_valley_study_area').geometry()

# 2: Dates of interest
start_date = '2013-01-01'
end_date = '2022-12-31'

# 3: Months of interest (summer, June-August = 6-8)
first_month = 6
last_month = 8

# 4: Set desired scale for exports
scale = 70

# 5: Name of Google Drive folder to save to
folder = 'CDL_CV'

# 6: Variable of interest: 'cropland' or 'cultivated'
variable = 'cultivated'

# 7: Specify export prefix 
export_name = 'CDL_cultivated_'

# **************** PROCESSING CODE ********************* # 

# Open data
dataset = ee.ImageCollection('USDA/NASS/CDL').filter(ee.Filter.date(start_date, end_date))

# First date for testing
dat1 = dataset.filterDate('2018-01-01', '2018-12-31').select(variable).first().clip(bbox)

# Reduce resolution
projection = ee.Image(dataset.first().select(variable)).projection()
projection_at_scale = projection.atScale(scale)

dat2 = dat1.setDefaultProjection(projection)
dat3 = dat2.reduceResolution(reducer=ee.Reducer.mode().unweighted(), maxPixels=100).reproject(crs=projection_at_scale)

# Function to add date property to each image
def add_date_property(image):
    date = ee.Date(image.get('system:time_start'))
    return image.set('date', date.format('YYYY-MM-dd'))

# Add date property to collection
collection_with_date = dataset.map(add_date_property)
dates = collection_with_date.aggregate_array('date').distinct().getInfo()

# Function to process and export each image
def process_and_export(date):
    collection_scale2 = collection_with_date.filter(ee.Filter.eq('date', date)).select(variable).first().clip(bbox)
    collection_proj = collection_scale2.setDefaultProjection(projection)
    
    reduced = collection_proj.reduceResolution(reducer=ee.Reducer.mode().unweighted(), maxPixels=100).reproject(crs=projection_at_scale)
    
    task = ee.batch.Export.image.toDrive(
        image=reduced,
        folder=folder,
        description=f"{export_name}_{date}",
        region=bbox,
        crs='EPSG:4326',
        scale=scale,
        maxPixels=1e13
    )
    task.start()

# Loop through dates and process each image
for date in dates:
    process_and_export(date)
