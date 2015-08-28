 N = 1e7L
 f = path.expand("~/Data/NYTaxis/trip_data_1.csv")
 dyn.load("/tmp/foo.so")
# system.time({ o = as.data.frame(.Call("readCSV", f, N), stringsAsFactors = FALSE)})
 varNames = c("medallion", "hack_license", "vendor_id", "rate_code", "store_and_fwd_flag", 
"pickup_datetime", "dropoff_datetime", "passenger_count", "trip_time_in_secs", 
"trip_distance", "pickup_longitude", "pickup_latitude", "dropoff_longitude", 
"dropoff_latitude")
 system.time({ o = structure(.Call("readCSV", f, N), class = "data.frame", row.names = NULL, names = varNames)}) 
 system.time(read.csv(f, stringsAsFactors = FALSE, nrow = N))
