#Load libraries
packages <- c("tidyverse", "rvest", "dplyr", "lubridate", "tidygeocoder", "ggplot2", "stringr", "readr", "leaflet", "htmltools", "htmlwidgets", "geojsonio",
              "rgdal", "leafem", "raster", "sf", "lattice", "xml2", "leaflet.extras")
lapply(packages, require, character.only = TRUE)    

#Store url and selectors
crime_url <- "https://safety.temple.edu/reports-logs/crime-log"
crime <- rvest::read_html(crime_url)

selector_base <- "//td[contains(@class, 'views-field views-field-field-"
selector_ids <- c("crime-classification", 
                  "incident-number", 
                  "reported", 
                  "occurred", 
                  "address", 
                  "building", 
                  "disposition")
for(i in selector_ids) {
  selectors <- paste(selector_base, selector_ids, "')]", sep="")
}

#Scrape crime log
selectors <- lapply(selectors, function(x) {
  crime %>% 
    rvest::html_nodes('body') %>% 
    xml2::xml_find_all(x) %>% 
    rvest::html_text(trim = TRUE)
})

#Store scraped data in data frame
crimelog_today <- as.data.frame(do.call(cbind, selectors)) %>% 
  rename(Classification = V1,
         Incident_Number = V2,
         DateTime_Reported = V3,
         DateTime_Occurred = V4,
         Location = V5,
         Building = V6,
         Disposition = V7)

#Clean "Location" column
crimelog_today$Location <- sub("Directions", "", crimelog_today$Location)
crimelog_today$Location <- sub("\n", " ", crimelog_today$Location)
crimelog_today$Location <- sub("\n", "", crimelog_today$Location)

#Add "Date_Reported" column (will be used in map labels)
crimelog_today$Date_Reported <- as.Date(sub("\\\n.*", "", crimelog_today$DateTime_Reported), "%m/%d/%y")
crimelog_today$Date_Reported <- format(crimelog_today$Date_Reported, "%B %d")

#Add "Location_Short" Column (will be used in map labels)
crimelog_today$Location_Short <- sub("\\Philadelphia.*", "", crimelog_today$Location)

#Geocode "Location" column (adds columns for latitude and longitude)
crimelog_today <- crimelog_today %>% 
  geocode(Location, method = 'osm', lat = latitude, long = longitude)

#Save coordinates defining patrol borders
patrol_lat <- c(39.98735036816927, 39.976802224353534, 39.976003290901424, 39.97162506760556, 39.97130031274797, 39.97575245658425, 39.97493865873836, 39.98537711065844, 39.98735036816927)
patrol_long <- c(-75.16261200192652, -75.16491548846739, -75.15850768846742, -75.15942964425508, -75.15727583750909, -75.15633858846746, -75.15007015963126, -75.14771538658265, -75.16261200192652)
CSSpatrol_border <- data.frame(patrol_lat, patrol_long)

#Write labels for map
labs <- lapply(seq(nrow(crimelog_today)), function(i) {
  paste0("<strong>", crimelog_today[i, "Classification"], " (", 
         crimelog_today[i, "Disposition"], ")", '</strong>', '<br />', 
         "Reported on ", crimelog_today[i, "Date_Reported"], " at ", 
         crimelog_today[i, "Location_Short"]) })

#Create map
incident_map <- leaflet(crimelog_today) %>% 
  addProviderTiles(providers$CartoDB.DarkMatter) %>% 
  addPolylines(lat = CSSpatrol_border$patrol_lat, lng = CSSpatrol_border$patrol_long, color = "#aaaaaa", weight = 5, opacity = 1) %>% 
  addCircleMarkers(lng = ~longitude, lat = ~latitude, label = lapply(labs, htmltools::HTML), color = "#82cfff", 
                   radius = 6, stroke = FALSE, fillOpacity = 1, 
                   
                   #Change color/boundary for clusters in case there's duplicate locations
                   clusterOptions = markerClusterOptions(freezeAtZoom = 19, iconCreateFunction=JS("function (cluster) {    
    var childCount = cluster.getChildCount(); 
    var c = ' marker-cluster-';  
    if (childCount < 1000) {  
      c += 'rgba(17, 146, 232, 0.9);'  
    }  
    return new L.DivIcon({ html: '<div><span>' + childCount + '</span></div>', className: 'marker-cluster' + c, iconSize: new L.Point(40, 40) });
  }"))) %>% 
  addLegend("bottomright", 
            colors = c("#82cfff", "#1192e8", "#aaaaaa"),
            labels = c("Incident", "Multiple Incidents (click to view)", 
                       "Campus Safety Services patrol border"),
            opacity = .9) %>% 
  setView(-75.15767818843477, 39.98045295969112, zoom = 15)
saveWidget(incident_map, file="crime_map.html")