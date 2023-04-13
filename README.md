# EIAinEMF37

Format EIA historical and AEO projections into EMF37 data template format

The EMF37 template is in /data-raw/templates
Mappings of EIA API series_id to EMF37 variables is in /data-raw/mapping

The data pipeline is managed via targets. The overall plan is listed in _targets.R. To run the project, from the console run: ```targets::tar_make()``` from the project working directory. (e.g., via opening the EIAinEMF37.Rproj file). Final results are in outputs/EIAinEMF37.csv. Load intermediate objects of the data pipeline via ```targets::tar_load(everything())``` (or specify names of particular targets to load).

Input data is retrieved from the EIA API. To run the project, you must acquire a free EIA API key. The API key should be recorded in such a way that it can be accessed by the eia package, such as in a .Renviron file. See example.Renviron.

/R -- contains package-like functions only supporting the data pipeline
/scripts -- contains top-level scripts/code


## TODO items:

Have some checks that help to identify:
  - data variables present not in template
  - units in data / template which cannot be mapped by the code
  - where there are template variables we might expect to be mapped
  
Map more EMF variables where data is available from EIA without too much complexity
Priorities might be:
  - All CO2 variables listed in beta round
  - All Primary energy variables listed in beta round


