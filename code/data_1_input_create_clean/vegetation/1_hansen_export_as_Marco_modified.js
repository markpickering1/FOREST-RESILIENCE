var terra = ee.ImageCollection("MODIS/061/MOD09GA"),
    europe = ee.FeatureCollection("users/agataelia1991/aoi/Europe_BB"),
    modisSample = ee.Image("users/agataelia1991/Hansen/modisSample");

// Hansen forest cover dataset 
var gfc = ee.Image("UMD/hansen/global_forest_change_2021_v1_9");

// Get tree cover for the year 2000
var treecover = gfc.select(['treecover2000']);

// Get tree cover loss band
var loss = gfc.select(['loss']);

// Only pixels with a tree cover greater than or equal to 30%
var treecover30p = treecover.updateMask(treecover.gte(30));

// Only pixels with a tree cover greater than or equal to 30% and filter out pixels where there was loss
var treecover30pnoloss = treecover.updateMask(treecover.gte(30)).updateMask(loss.eq(0));

// Self mask tree cover layer
var sfMask = treecover.gte(30).selfMask();

// Self mask tree cover layer with no loss
var sfMaskNoLoss = treecover.updateMask(loss.eq(0)).gte(30).selfMask();

// Calculated count of pixels for different forest 
var patchCount = sfMask.connectedPixelCount(1024, false);

// Calculated count of pixels for different forest with no loss
var patchCountNoLoss = sfMaskNoLoss.connectedPixelCount(1024, false);

// Filter out small patches from cover dataset with no loss and create binary mask of the results
var treecover30pMask = treecover30p.updateMask(patchCount.gt(6)).mask().clip(europe).unmask(0);

// Filter out small patches from cover dataset and create binary mask of the results
var treecover30pnolossMask = treecover30pnoloss.updateMask(patchCountNoLoss.gt(6)).mask().clip(europe).unmask(0);

// Get the forest cover at MODIS export scale, crs and tranform
var hansenForestCover2000AtModisMean = treecover30pMask
    // Force the next reprojection to aggregate instead of resampling
    .reduceResolution({
      reducer: ee.Reducer.mean(),
      maxPixels: 1024
    })
    // Request the data at the scale and projection of the MODIS image
    .reproject({ 
      crs: modisSample.projection()
    }); 

// Add layer to map
Map.addLayer(hansenForestCover2000AtModisMean);

// Export the forest cover MODIS export scale, crs and tranform
Export.image.toDrive({
  image: hansenForestCover2000AtModisMean.clip(europe), 
  description: 'hansenForestCover2000AtModisMean', 
  folder: 'hansen',
  region: europe,
  crs: 'EPSG:4326',
  crsTransform: [0.0049999999999966955, 0, -10.66499999999295, 0, -0.0049999999999966955, 71.18499999995295],
  scale: 556.597453966,
  maxPixels: 1e13});

// Get the forest cover with no loss at MODIS export scale, crs and tranform
var hansenForestCoverNoLoss2000AtModisMean = treecover30pnolossMask
    // Force the next reprojection to aggregate instead of resampling
    .reduceResolution({
      reducer: ee.Reducer.mean(),
      maxPixels: 1024
    })
    // Request the data at the scale and projection of the MODIS image
    .reproject({ 
      crs: modisSample.projection()
    }); 

// Add layer to map
Map.addLayer(hansenForestCoverNoLoss2000AtModisMean);

// Export the forest cover with no loss MODIS export scale, crs and tranform
Export.image.toDrive({
  image: hansenForestCoverNoLoss2000AtModisMean.clip(europe), 
  description: 'hansenForestCoverNoLoss2000AtModisMean', 
  folder: 'hansen',
  region: europe,
  crs: 'EPSG:4326',
  crsTransform: [0.0049999999999966955, 0, -10.66499999999295, 0, -0.0049999999999966955, 71.18499999995295],
  scale: 556.597453966,
  maxPixels: 1e13});
