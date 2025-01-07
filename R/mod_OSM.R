#' mod_OSM UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_OSM_ui <- function(id){
  ns <- NS(id)

  selection1=glourbi::all_cities %>%
    dplyr::filter(selection1_GSW==TRUE) %>%
    dplyr::arrange(Urban.Aggl) %>%
    dplyr::pull(Urban.Aggl)
  tagList(
    fluidRow(
      column(width=4,
             div(id=ns("city_help"),
             selectInput(ns("city"),
                         "Choose city",
                         choices=selection1,
                         selected=selection1[2])
             ),#div
             checkboxInput(ns("wikidata"),
                           "Show Wikidata",
                           value=TRUE),
             checkboxGroupInput(ns("group"),
                         "OSM: Choose type",
                         choices=unique(glourbapp::map_elems_global$group),
                         selected="engineering"),
             plotOutput(ns("plot_osmglobal"))
             ),#column
      column(width=8,
             leaflet::leafletOutput(ns("map_city"),height=800)
             )#column
    )#fluidRow

  )
}

#' mod_OSM Server Functions
#'
#' @noRd
mod_OSM_server <- function(id, conn){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    get_mapinfo=reactive({
      thisCityCode=glourbi::to_citycode(input$city)
      result=glourbi::get_city_sf(name="osm_polygons",
                           thisCityCode,
                           conn=conn) %>%
        tidyr::separate(osm_keyvalue,into=c("key","value")) %>%
        dplyr::left_join(glourbi::tib_key_value,by=c("key","value"))
      }
    )
    get_wdinfo=reactive({
      wd=DBI::dbReadTable(conn=conn,
                          name="wikidata_table") %>%
        tibble::as_tibble()
      wd=wd %>%
        dplyr::mutate(osm_id=as.character(osm_id)) %>%
        dplyr::select(osm_id,wikidata_id)
      result=get_mapinfo()
      result=result %>%
        dplyr::left_join(wd,by="osm_id") %>%
        dplyr::filter(!is.na(wikidata_id)) %>%
        dplyr::mutate(popup=paste0("<a href='https://www.wikidata.org/wiki/",
                                   wikidata_id,
                                   "' target='_blank'>Wikidata item</a> ")) %>%
        sf::st_centroid()
      print(result)
      result
    })

    get_mapinfo_group=reactive({
      get_mapinfo() %>%
        sf::st_drop_geometry() %>%
        dplyr::filter(group %in% input$group) %>%
        dplyr::group_by(key,value,color,group) %>%
        dplyr::tally()
    }
    )
    output$plot_osmglobal=renderPlot({
      if(length(input$group)>0){
        info=get_mapinfo_group()
        colorscale=info %>%
          dplyr::select(key,value,color) %>%
          unique()
        colorscale_vec=colorscale$color
        names(colorscale_vec)=colorscale$value
        ggplot2::ggplot(info,
                        ggplot2::aes(x=forcats::fct_reorder(value,group),y=n,fill=value, group=group))+
          ggplot2::geom_bar(stat="identity",color="dark grey")+
          ggplot2::geom_text(ggplot2::aes(y=0, label=value),size=8,
                             hjust = 0, nudge_x = 0.1)+
          ggplot2::coord_flip()+
          ggplot2::scale_fill_manual(values=colorscale_vec)+
          ggplot2::theme(legend.position="none")+
          ggplot2::scale_y_sqrt()+
          ggplot2::scale_x_discrete(labels=info$group)+
          ggplot2::xlab("")
        }
    })
    output$map_city=leaflet::renderLeaflet({
      CityCode=glourbi::all_cities %>%
        dplyr::filter(Urban.Aggl==input$city) %>%
        dplyr::pull(ID)
      path=system.file("per_city/",package="glourbi")
      shape=sf::st_read(paste0(path,CityCode,".shp")) %>%
        sf::st_transform(crs = 4326) %>%
        dplyr::mutate(reach_color=dplyr::case_when(reach=="city"~"red",
                                                   reach=="upstream"~"blue",
                                                   reach=="downstream"~"purple"))
      mymap=leaflet::leaflet(shape) %>%
        leaflet::addPolygons(fill=FALSE,
                             color=~reach_color) %>%
        leaflet::addTiles(group = "OSM map") %>%
        leaflet::addProviderTiles(leaflet::providers$Esri.WorldImagery,
                         group = "ESRI Photo") %>%
        leaflet::addTiles(
          urlTemplate = "https://storage.googleapis.com/global-surface-water/tiles2021/change/{z}/{x}/{y}.png",
          attribution = "2016 EC JRC/Google",
          group="GWS") %>%
        leaflet::addProviderTiles(leaflet::providers$Esri.WorldStreetMap,
                                  group = "ESRI Street Map") %>%
        leaflet::addLayersControl(
              overlayGroups = c("OSM map","ESRI Map","GWS","ESRI Photo"),
              options = leaflet::layersControlOptions(collapsed = FALSE)) %>%

        leaflet::hideGroup("GWS") %>%
        leaflet::hideGroup("ESRI Photo") %>%
        leaflet::hideGroup("ESRI Map")
        print(mymap)
    })

    observeEvent(c(input$group,input$city),{
      mapinfo=get_mapinfo()
      mymap=leaflet::leafletProxy(ns("map_city"))
      groups=unique(mapinfo$group)
     # if(nrow(mapinfo)>1){
      for (i in 1:length(groups)){
        if(groups[i] %in% input$group){
          mymap=mymap %>%
            leaflet::addPolygons(data=mapinfo %>%
                       dplyr::filter(group==groups[i]),
                     color=~color,
                     popup=~value,
                     group=groups[i]
            )
        }
        if(!(groups[i] %in% input$group)){
        mymap=mymap %>%
          leaflet::clearGroup(groups[i])
        }
      }# end for loop
    #}#end if
    })
  observeEvent(input$wikidata,{
    mymap=leaflet::leafletProxy(ns("map_city"))

    if(input$wikidata){
      wd=get_wdinfo()

      mymap=mymap%>%
        leaflet::addCircleMarkers(data=wd,
                                  popup=~popup,
                                  color=~color,
                                  stroke=FALSE,fillOpacity=0.8,radius=5,
                                  group="wikidata")
    }
    if(!input$wikidata){
      mymap=mymap %>%
        leaflet::clearGroup("wikidata")
    }
  })

  })
}

