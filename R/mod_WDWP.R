#' mod_WDWP UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_WDWP_ui <- function(id){
  ns <- NS(id)
  tagList(
    fluidRow(
      column(width=4,
             checkboxGroupInput(ns("wd_type"),
                                "type",
                                choices=unique(glourbapp::wd_all_map$type),
                                selected="dam")
      ),#column
      column(width=8,
             leaflet::leafletOutput(ns("map_wd"),height=800)
      )#column
    )#fluidRow
)
}

#' mod_WDWP Server Functions
#'
#' @noRd
mod_WDWP_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    r_wd_data=reactive({
      print(input$wd_type)
      glourbapp::wd_all_map %>%
        dplyr::filter(type %in% input$wd_type)
    })

    output$map_wd=leaflet::renderLeaflet({
      mymap=leaflet::leaflet(glourbi::all_cities %>% dplyr::filter(selection1_GSW==TRUE)) %>%
        leaflet::addCircleMarkers(color="black") %>%
        leaflet::addCircleMarkers(data=r_wd_data(),
                                  popup=~popup,
                                  color=~color,
                                  stroke=FALSE,fillOpacity=0.8,radius=5,
                                  group="wikidata",
                                  #clusterOptions = leaflet::markerClusterOptions()
                                  ) %>%
        leaflet::addTiles(group = "OSM map") %>%
        leaflet::addProviderTiles(leaflet::providers$Esri.WorldImagery,
                                  group = "Photo") %>%
        leaflet::addProviderTiles(leaflet::providers$Esri.WorldStreetMap,
                                  group = "ESRI Street Map") %>%
        leaflet::addLayersControl(
          overlayGroups = c("wikidata","OSM map","ESRI Photo","ESRI Street Map"),
          options = leaflet::layersControlOptions(collapsed = FALSE)) %>%
        leaflet::hideGroup("ESRI Photo") %>%
        leaflet::hideGroup("ESRI Street Map")
      print(mymap)
    })


  })
}
