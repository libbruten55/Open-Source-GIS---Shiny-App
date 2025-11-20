
#################################################################
##           Practical 6 for GEOM184 - Open Source GIS         ##
##                      14/11/2025                             ##
##                  Creating a ShinyApp                        ##
##                       Server.R                              ##
##        code by Diego Panici (d.panici@exeter.ac.uk)         ##
#################################################################

# S1 Render leaflet map ----
# Leaflet map output
output$map <- renderLeaflet ({
  
  leaflet() %>% 
    
    setView(lng=-4.206878, lat=50.916601, zoom=11.3) %>%
    
    addProviderTiles (providers$OpenStreetMap, group = "Colour")%>%
  
    addCircles(data = bridges_snapped,
             color = "black",
             fillColor="purple",
             fillOpacity=0.8,
             weight = 2,
             radius = 50,
             popup = ~paste("<b>OWNER:</b>", OWNER, "<br><b>BRIDGE_NAME:</b>", BRIDGE_, "<br><b>LW_Upstream:</b>", LW_pstr),
             group = "Bridges")%>%
    addCircles(data = catchers,
               color = "blue",
               fillColor="blue",
               fillOpacity=0.8,
               weight = 2,
               radius = 50,
               popup = ~paste("<br><b>Catchers:</b>", Catchers),
               group = "Catchers")%>%
  addRasterImage(heatmap, colors = pal_heatmap, opacity = 0.7, group = "Heatmap")%>%
  addRasterImage(aspect, colors = pal_aspect, opacity = 0.7, group = "Aspect")%>%
  addRasterImage(slope, colors = pal_slope, opacity = 0.7, group = "Slope")%>%
    addImageQuery(
      heatmap,
      layerId = "Heatmap",
      prefix = "Value: ",
      digits = 2,
      position = "topright",
      type = "mousemove", # Show values on mouse movement
      options = queryOptions(position = "topright"), # Remove the TRUE text
      group = "Heatmap"
    ) %>%
    addLayersControl(
      baseGroups = c("Colour"),
      overlayGroups = c("River", "Bridges", "Large Wood", "Heatmap", "Aspect", "Slope", "Catchers", "Nearest Distance"),
      options = layersControlOptions(collapsed = FALSE)
    )
  })

# Add popups for large wood points
observe({
  leafletProxy("map") %>%
    clearMarkers() %>%
    addCircleMarkers(data = clusters, 
                     fillColor = ~pal_clusters(CLUSTER_ID), 
                     color = "black", 
                     weight = 1, 
                     radius = 5, 
                     stroke = TRUE, 
                     fillOpacity = 0.7,
                     popup = ~paste("<b>Type:</b>", LW_Type, "<br><b>Cluster ID:</b>", CLUSTER_ID),
                     group = "Large Wood")
})

observe({
  leafletProxy("map")%>%
    clearGroup("Nearest Distance")%>%
    addPolylines(data = nearest_distance,
                 color = "black", weight=2, opacity=0.8,
                 group = "Nearest Distance")
})
