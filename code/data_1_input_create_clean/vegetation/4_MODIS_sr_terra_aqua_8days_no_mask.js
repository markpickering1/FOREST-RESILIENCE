var terra = ee.ImageCollection("MODIS/061/MOD09GA"),
    aqua = ee.ImageCollection("MODIS/061/MYD09GA"),
    europe = ee.FeatureCollection("users/agataelia1991/aoi/Europe_BB"),
    forest = ee.Image("users/agataelia1991/Hansen/hansenTreecover2000MaskAtModisBicubic");
    
// Define bounding box
//var europe = europe.geometry().bounds();
Map.addLayer(europe);

// Function to return an image containing just the specified QA bits
var getQCBits = function(image, start, end, newName) {
    // Compute the bits to extract
    var pattern = 0;
    for (var i = start; i <= end; i++) { 
       pattern += Math.pow(2, i);
    }
    return image.select([0], [newName])
                  .bitwiseAnd(pattern) 
                  .rightShift(start);  
};

// Function to select only the highest quality (0) data for band 1 and 2, 
// clear from clouds, clouds shadows, water, snow, and snow and ice
// and mask only on forested areas
var maskQA = function(image){
  var QC = image.select('QC_500m');
  var state = image.select('state_1km');
  var mband1  =  getQCBits(QC, 2,5, 'mask_band1').eq(0);
  var mband2  =  getQCBits(QC, 6,9, 'mask_band2').eq(0);
  var mcloud  =  getQCBits(state, 0,1, 'mask_cloud').eq(0);
  var mshadow =  getQCBits(state, 2,2, 'mask_cloudShadow').eq(0);
  var mwater = getQCBits(state, 3,5, 'mask_water').neq(0);
  var mwater2 = getQCBits(state, 3,5, 'mask_water').neq(6);
  var mwater3 = getQCBits(state, 3,5, 'mask_water').neq(7);
  var msnowice =  getQCBits(state, 12,12, 'mask_snowice').eq(0);
  var msnow =  getQCBits(state, 15,15, 'mask_snow').eq(0);
  return image.updateMask(mband1).updateMask(mband2).updateMask(mcloud)
              .updateMask(mshadow).updateMask(msnowice).updateMask(msnow)
              .updateMask(mwater).updateMask(mwater2).updateMask(mwater3);
              //.updateMask(forest.gt(20));// // Mask on FAO forest definition
};

// Function to add NDVI to each MODIS scene ((scaling factor of bands is 0.0001))
var addNDVI = function(image) {
  var nir  = image.select('sur_refl_b02').multiply(0.0001);
  var red  = image.select('sur_refl_b01').multiply(0.0001);
  var ndvi = nir.subtract(red).divide(nir.add(red)).rename('NDVI');
  return image.addBands(ndvi);
};

// Function to calculate kNDVI from NDVI 
var addKNDVI = function(image) {
  var rescaledNDVI = image.select(['NDVI']);
  //var kNDVI = ((rescaledNDVI.pow(2)).tanh()).rename(['kNDVI']); // This if we want to turn every NDVI into positive
  var kNDVI=((rescaledNDVI.pow(3)).divide(rescaledNDVI.abs())).tanh().rename('kNDVI'); // This if we want to account for snow
  return image.addBands(kNDVI);
};

// Merge terra and aqua collections
var modis = terra.merge(aqua).sort("system:time_start");
//print(modis);
//Map.addLayer(modis.first());

// Mask modis collection
var modisMasked = modis.map(maskQA);
//print(modisMasked);
//Map.addLayer(modisMasked.first());

// Add NDVI to modis collection
var modisNDVI = modisMasked.map(addNDVI);
//print(modisKNDVI);
//Map.addLayer(modisKNDVI.first());

// Add kNDVI to modis collection
var modisKNDVI = modisNDVI.map(addKNDVI);
//print(modisKNDVI);
//Map.addLayer(modisKNDVI.first());

// Create collection of daily kNDVI 
var modisKNDVIBand = modisKNDVI.select(['kNDVI']);
//print(modisKNDVIBand.first());
//Map.addLayer(modisKNDVIBand.first());

// Initialize dates
var date0 = ee.Date('1999-01-01');

// Create empty list to generate count later for one year only (example for 2003 with y = 4; y < 5)
var list = ee.List([]);

// Export stacks of 8 days averaged kNDVI
for(var y = 20; y < 23; y++) {
  for(var d = 0; d < 366; d+=8){
    
    // Define 8 days window start and end dates
    var dateStart = date0.advance(y, 'year').advance(d, 'day');
    var dateEnd = date0.advance(y, 'year').advance(ee.Number(d).add(8), 'day'); // Create 8 days timesteps
    
    if (d ==360) {dateEnd=ee.Date.fromYMD(year+1, 01, 01);} // Contain timesteps within the year
    
    print(dateStart.getInfo());
    print(dateEnd.getInfo());
    
    // Create filename 
    var dateFileName = dateStart.advance(3, 'day'); // Give filename as the middle of timestep
    var year = dateFileName.get('year').getInfo();
    var month = dateFileName.get('month').getInfo();
    var day = dateFileName.get('day').getInfo();
    
    //var fileName = year*10000+month*100+day+3;
    var fileName = year + '_' + month + '_' + day + '_kNDVI';
    print(fileName);
    
    // Create 8 days mean
    var eightDaysCollection = modisKNDVIBand.filterDate(dateStart, dateEnd);
    print(eightDaysCollection);
    var eightDaysMean = modisKNDVIBand.filterDate(dateStart, dateEnd).mean();
    /*
    // Print range of values in image
    var reducers = ee.Reducer.min().combine({
    reducer2: ee.Reducer.max(),
    sharedInputs: true
    });
    var statsValues = eightDaysMean.reduceRegion({
      reducer: reducers,
      geometry: europe,
      scale: 500,
      crs: modisProjection,
      maxPixels: 1e13});
    print(statsValues);

    // Add element to the list
    var list = list.add(eightDaysMean);
    */
    // Export data
    Export.image.toDrive({
    image: eightDaysMean.clip(europe).multiply(10000).toInt16().unmask(-32767), // Export as Int16 to save space, set NoData to -32767
    description: fileName,
    folder: 'modis',
    scale: 556.597453966, // Export at 0.005 resolution to match climate data
    crs: 'EPSG:4326',
    region: europe,
    fileFormat: 'GeoTIFF',
    maxPixels: 1e13});
    
}}
/*
print(list);

// Create image collection from list
var modisKNDVIBandYearly =  ee.ImageCollection.fromImages(list);
print(modisKNDVIBandYearly);

// Reduce collection to count
var modisKNDVIBandYearlyCount = modisKNDVIBandYearly.count();
print(modisKNDVIBandYearlyCount);
Map.addLayer(modisKNDVIBandYearlyCount.clip(europe));

// Export results of count
Export.image.toDrive({
  image: modisKNDVIBandYearlyCount.clip(europe).toInt8(),
  description: 'MODIS_terra_aqua_8daysSR_count_2003',
  fileNamePrefix: 'MODIS_terra_aqua_8daysSR_count_2003',
  folder: 'modis',
  scale: 500, //556.597453966,
  crs: 'EPSG:4326',
  region: europe,
  fileFormat: 'GeoTIFF',
  maxPixels: 1e13});

/*
// Generate 8 days long-term kNDVi average
var date0 = ee.Date('2002-01-01');
var years = ee.List.sequence(1, 21, 1);
var days = ee.List.sequence(0, 365, 8);
print(years);
print(days);

var eightDaysAverage = days.map(function(j){
  var yearly8Days = years.map(function(i){
    var annualC = modisKNDVIBand.filterDate(date0.advance(i, 'year'), date0.advance(ee.Number(i).add(1), 'year'));
    annualC = annualC.filterDate(date0.advance(i, 'year').advance(j, 'day'), 
    date0.advance(i, 'year').advance(ee.Number(j).add(8), 'day')).toBands();
    return annualC;
  });
  var average = ee.ImageCollection.fromImages(yearly8Days).mean().rename(['meankNDVI']);
  return average;
});
print(eightDaysAverage);

var eightDaysAverageImage = ee.ImageCollection.fromImages(eightDaysAverage).toBands();
print(eightDaysAverageImage);
//Map.addLayer(eightDaysAverageImage);

Export.image.toDrive({
image: eightDaysAverageImage,
description: 'climatological_8_days_kNDVI_average',
scale: 500,
crs: 'EPSG:4326',
fileFormat: 'GeoTIFF',
maxPixels: 1e13}); 
*/