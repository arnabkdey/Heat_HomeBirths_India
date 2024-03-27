merge_dhs_climate <- function(path, region_boundary = india_boundary_buf, 
                              dhs_lat_long_sf = df_dhs_psu_geo_sf,
                              clim_var = "min_temp") {
    require(terra)  # Use terra instead of raster
    require(data.table)
    require(sf)
    
    # Get the list of files
    a <- list.files(path = path, pattern ="\\.nc")
    # Initialize empty list for data frames
    df_list <- list()
    # Run a loop 
    for (i in seq_along(a)) {
        # Read the files as raster (SpatRaster in terra)
        rd0 <- rast(paste0(path, "/", a[i]))
        
        ## Restrict the spatial data to the region boundary (use mask and crop in terra)
        # In terra, mask also crops, so you can just use mask
        cd2 <- crop(mask(rd0, region_boundary, inverse=TRUE), region_boundary)
        
        # Extract the climate data for each PSU location
        # Use terra's extract function. It automatically returns a data frame when new=TRUE.
        df1 <- extract(cd2, dhs_lat_long_sf, df=TRUE)
        
        # Add the PSU information from sf object, need to convert sf to data frame first
        dhs_data <- as.data.frame(dhs_lat_long_sf)
        df1 <- data.table(cbind(dhs_data, df1))
        # Assuming your SpatRaster has layer names that can be converted to dates, adjust if necessary
        df2 <- melt(df1, id.vars = c("psu", "lat", "long", "dist_name"), variable.name = "date", value.name = clim_var)
        df2[, date := as.Date(substring(date, 2), format = "%Y.%m.%d")][!is.na(clim_var)]
        # Add the data frame to the list
        df_list[[i]] <- df2
        print(paste("finished processing", i))
    }
    df_out <- rbindlist(df_list)
    return(df_out)
}
