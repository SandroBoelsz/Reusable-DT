## Preparation

### Set up container
Create a docker image by running the Makefile in the back-end directory of the project.
```bash
make build
```

### Set up HyperQueue

Download hq binary release:

    mkdir -p bin
    wget https://github.com/It4innovations/hyperqueue/releases/download/v0.15.0/hq-v0.15.0-linux-x64.tar.gz -O - | tar -xzf - -C bin

## A test case

### Download test data

Download `BEEHAVE_BeeMapp2016.zip` with test data from https://beehave-model.net/download/. Direct link:

    wget --content-disposition 'https://beehave-model.net/?smd_process_download=1&download_id=974'
    unzip BEEHAVE_BeeMapp2016.zip -d test

Download map (source https://doi.org/10.1594/PANGAEA.910837) and rename both files as map.tif and map.tif.aux.xml:

    wget https://hs.pangaea.de/Maps/Germany/Preidl-etal_2020/APiC_Agricultural-Land-Cover-Germany_RSE-2020.zip
    unzip APiC_Agricultural-Land-Cover-Germany_RSE-2020.zip

Also required are the following files:
- Beehave_BeeMapp2015_Netlogo6version_PolygonAggregation.nlogo from https://github.com/BioDT/uc-pollinators-beehave
- locations.csv from https://github.com/BioDT/uc-pollinators
- lookup_table.csv from https://github.com/BioDT/uc-pollinators
- parameters.csv from https://github.com/BioDT/uc-pollinators
- simulation.csv from https://github.com/BioDT/uc-pollinators

Which results in the following data/input folder:
- Beehave_BeeMapp2015_Netlogo6version_PolygonAggregation.nlogo
- locations.csv
- lookup_table.csv
- parameters.csv
- simulation.csv
- map.tif
- map.tif.aux.xml

# Run the Digital Twin
```bash
scripts/cloud_execution.sh ../data/input
```
