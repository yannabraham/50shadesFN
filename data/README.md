# Preparing the maps

I followed the instructions created by Mike Bostock and listed [here](http://bost.ocks.org/mike/map/).

Maps have been downloaded from the [data.gouv.fr](https://www.data.gouv.fr/fr/datasets/contours-des-departements-francais-issus-d-openstreetmap/)
portal, and converted using [GSInternals](http://www.gisinternals.com/index.html)
and [TopoJSON](https://github.com/mbostock/topojson) tools.

Data is converted using the following commands:

```
ogr2ogr -f GeoJSON departements\departements-20140306-100m.json
  -where "code_insee not in ('976','973','972','971','974')"
  -lco ENCODING=UTF-8
  departements\departements-20140306-100m.shp
```

Followed by

```
topojson -o ..\web\departements.json
  --id-property nuts3
  -p code_insee -p nom -p nuts3
  departements\departements-20140306-100m.json
```

# Preparing the data

The results from previous elections where downloaded from [data.gouv.fr](https://www.data.gouv.fr/fr/datasets/elections-cantonales-1988-2011/) and results from Front National where extracted and expressed as percent of total votes for all departements using a custom R script
