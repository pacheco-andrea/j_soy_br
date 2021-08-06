# Brazil's soy exports 
mapping soy exports from brazil

#### `makeMap.R`
uses [GADM](https://gadm.org/download_country_v3.html) spatial data (as sf's) and pairs it with [TRASE's](https://supplychains.trase.earth/flows/data-view?selectedYears%5B%5D=2007&selectedYears%5B%5D=2008&toolLayout=0&selectedMapDimensions=quant11&countries=27&commodities=1&selectedNodesIds%5B%5D=11140&selectedNodesIds%5B%5D=904&sources=11140%2C904&selectedColumnsIds=0_14-1_22-2_9-3_16)
data on soy exports to map:
1) how much soy brazil exports to germany, per municipality
2) how much soy brazil exports to the EU, per municipality
(with possibility to expand to other countries/regions)

#### current issues:
- not able to identify some of the municipalities listed by TRASE in the GADM municipalities

updates/help welcome! 
