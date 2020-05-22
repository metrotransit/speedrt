# speedRT 0.1.1

* Added a `NEWS.md` file to track changes to the package.
* Fixed file upload bugs
* Added `launch_speedapp()` will launch the app directly from the package.
* `launch_speedapp` checks for packages required for the app before launching and
gives an error with install instructions for missing packages.
* Added useful error messages for input data
* Minor UI enhancements
* Simplified user interface for file uploads, GTFS currently required for both raw
and pre-processed. 
* Fix date range comparisons
* `logVehiclePositions` becomes `logProtobufFeed`, works for any protobuf
  * Fixed duration check for logging pb feeds
  * Logged filenames are based off of the feed name, e.g., tripupdates.pb 
  becomes tripupdates_20190610112300.pb and VehiclePositions.pb becomes
  VehiclePositions_20190610112300.pb
