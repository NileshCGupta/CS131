# A configuration file for the Twisted Places proxy herd

#Google Places API URL Prefix
URL_PREFIX = "https://maps.googleapis.com/maps/api/place/nearbysearch/json?"

# Google Places API key
API_KEY = "AIzaSyCC_1GZCX0VHEf-AFrTaSYbMj5AeTU7quQ"

# TCP port numbers for each server instance (server ID: case sensitive)
# Please use the port numbers allocated by the TA.
SERVERS = {
    "Alford": 	{"ip": "localhost", "port": 12480},
    "Ball": 	{"ip": "localhost", "port": 12481},
    "Hamilton": {"ip": "localhost", "port": 12482},
    "Holiday": 	{"ip": "localhost", "port": 12483},
    "Welsh": 	{"ip": "localhost", "port": 12484}
}

NEIGHBORS = {
	"Alford": 	["Hamilton", "Welsh"],
	"Ball": 	["Holiday", "Welsh"],
	"Hamilton":	["Holiday"],
	"Welsh":	["Alford", "Ball"],
	"Holiday": 	["Ball", "Hamilton"]
}

PROJ_TAG="Fall 2016"

