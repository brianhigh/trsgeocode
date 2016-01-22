# trsgeocode

Geocode Washington State alfalfa seed crop data given township
section TRS codes and plot a map showing acres of alfalfa per
township section. 

Copyright 2016 Brian High (https://github.com/brianhigh) and
Anika Larsen

License: GNU GPL v3 http://www.gnu.org/licenses/gpl.txt

This script serves as an example of using the following techniques:

* Querying an ArcGIS REST API using a URL with a "GET" query string
* Importing data in JSON format 
* Geocoding TRS codes using the BLM web service API
* Importing data in XML format
* Decoding HTML-encoded text
* Using regular expressions for pattern matching and string replacement
* Mapping with ggplot2
* Caching data in files to avoid redoing slower steps if re-running script

