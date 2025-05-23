/**
 *
 * Landsat Thermal Bands - 2018/2020 - Period Median, Mean, Min, Max, StdDev
 *
 **/

var start = '2018-01-01';
var end = '2020-12-31';

var geometry = table.geometry().bounds()

var loadLS8_ST = function(start, end, region) {

  var dataset = ee.ImageCollection('LANDSAT/LC08/C02/T1_L2')
    .filterDate(start, end)
    .filterBounds(region)
    .filter(ee.Filter.eq('PROCESSING_LEVEL', 'L2SP'));

  function maskClouds(image) {
    var cloudShadowBitMask1 = (1 << 4);
    var cloudsBitMask1 = (1 << 3);
    var highcloud1 = (1 << 9);
    var snow = (1 << 5);
    var qa1 = image.select('QA_PIXEL').rename('Cloud');

    var mask1 = qa1.bitwiseAnd(cloudsBitMask1).eq(0)
                 .and(qa1.bitwiseAnd(highcloud1).eq(0)
                 .or(qa1.bitwiseAnd(cloudShadowBitMask1).eq(0)))
                 .and(qa1.bitwiseAnd(snow).eq(0));

    var maskedComposite = image.updateMask(mask1);
    return maskedComposite.addBands(mask1)
  }

  // Applies scaling factors.
  function applyScaleFactors(image) {
    var opticalBands = image.select('SR_B.').multiply(0.0000275).add(-0.2);
    var thermalBands = image.select('ST_B.*').multiply(0.00341802).add(149.0).subtract(273.15);
    return image.addBands(opticalBands, null, true)
                .addBands(thermalBands, null, true);
  }

  return dataset.map(maskClouds).map(applyScaleFactors);
}

var LS8 = ee.ImageCollection(loadLS8_ST(start, end, geometry)).select('ST_B10')

var crs = LS8.first().projection().getInfo()

var LS_p5 = LS8.reduce(ee.Reducer.percentile([5])).rename('p5')
var LS_p25 = LS8.reduce(ee.Reducer.percentile([25])).rename('p25')
var LS_p75 = LS8.reduce(ee.Reducer.percentile([75])).rename('p75')
var LS_p95 = LS8.reduce(ee.Reducer.percentile([95])).rename('p95')
var LS_stdDEv = LS8.reduce(ee.Reducer.stdDev()).rename('stdDEv')
var LS_median = LS8.median().rename('median')
var LS_mean = LS8.mean().rename('mean')

var stats = ee.ImageCollection([
  LS_p5,
  LS_p25,
  LS_p75,
  LS_p95,
  LS_stdDEv,
  LS_median,
  LS_mean,
]).toBands().clip(geometry)

Map.centerObject(stats)
Map.addLayer(stats)

Export.image.toDrive({image:stats, description:'stats', region:geometry, crs:crs.crs, crsTransform: crs.transform})
