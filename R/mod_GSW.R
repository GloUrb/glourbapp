#' mod_GSW UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_GSW_ui <- function(id){
  ns <- NS(id)

  selection1=glourbi::all_cities %>%
    dplyr::filter(selection1_GSW==TRUE) %>%
    dplyr::arrange(Urban.Aggl) %>%
    dplyr::pull(Urban.Aggl)
  tagList(
    div(id="city_help",
    selectInput(ns("city"),
                "Choose city",
                choices=selection1,
                selected=selection1[2])
    ),#div
    fluidRow(
      column(width=6,
             #checkboxInput("onlyGSW","only cities with GSW density"),
             leaflet::leafletOutput(ns("map_city"),height=600)
      ),#column
      column(width=6,
             tabsetPanel(
               tabPanel("GSW description",
                        plotOutput(ns("GSWdescription"),
                                   width="700px",
                                   height="600px")),
               tabPanel("GSW table",
                        DT::DTOutput(ns("GSWtable")))
             )
      )#column
    )#fluidRow

  )
}

#' mod_GSW Server Functions
#'
#' @noRd
mod_GSW_server <- function(id,conn){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    r_get_gsw_patches_summary=reactive({
      thisCityCode=glourbi::to_citycode(input$city)
      tib=glourbi::get_city_sf(name="gsw_patches_summary",
                               thisCityCode,
                               conn=conn)
    })

    output$GSWdescription=renderPlot({
      result=r_get_gsw_patches_summary()%>%
        sf::st_drop_geometry() %>%
        dplyr::as_tibble()%>%
        dplyr::mutate(reach=factor(reach, levels=c("upstream","city","downstream"))) %>%
        dplyr::mutate(IDrow=1:dplyr::n())

      result1=result %>%
        dplyr::select(IDrow,dplyr::starts_with("ntype")) %>%
        tidyr::pivot_longer(cols=dplyr::starts_with("ntype_"),names_to="type",values_to="ntype",names_prefix="ntype_")
      result2=result %>%
        dplyr::select(IDrow,dplyr::starts_with("mutype")) %>%
        tidyr::pivot_longer(cols=dplyr::starts_with("mutype_"),names_to="type",values_to="mutype",names_prefix="mutype_")
      result=result %>%
        dplyr::select(-dplyr::starts_with("ntype_"),
               -dplyr::starts_with("mutype_")) %>%
        dplyr::left_join(result1,by=c("IDrow")) %>%
        dplyr::left_join(result2,by=c("IDrow","type")) %>%
        dplyr::select(-IDrow)
      ggplot2::ggplot(result,
                      ggplot2::aes(x=sizepatch, y=ntype,fill=type, alpha=abs(mutype)/100))+
        ggplot2::geom_bar(stat="identity")+
        ggplot2::facet_grid(rows=ggplot2::vars(reach),cols=ggplot2::vars(zone))+
        ggplot2::scale_fill_manual(values=c("#ff0000","#000000","#00ff00"))+
        ggplot2::theme(legend.position="none")
    })

    output$GSWdensity=renderPlot({
      conn=glourbi::connect_to_sandbox()
      gsw_result=gsw::gsw_summary(input$city,conn)
      DBI::dbDisconnect(conn)
      gsw::gsw_summary_plot(gsw_result)
    })
    output$GSWtable=DT::renderDT({
      result=r_get_gsw_patches_summary() %>%
        sf::st_drop_geometry() %>%
        dplyr::select(reach,zone,sizepatch,npatches,
                      n_neg=ntype_neg,
                      n_neutral=ntype_neutral,
                      n_pos=ntype_pos,
                      mu_neg=mutype_neg,
                      mu_neutral=mutype_neutral,
                      mu_pos=mutype_pos)
      DT::datatable(result)
    }
    )
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
      sf::sf_use_s2(FALSE)
      patches=r_get_gsw_patches_summary() %>%
        dplyr::select(sizepatch) %>%
        dplyr::mutate(geometry = dplyr::if_else(sf::st_is(geometry, "GEOMETRYCOLLECTION"),
                                  sf::st_union(sf::st_collection_extract(geometry, "POLYGON")),
                                  geometry)) %>%
        sf::st_as_sf() %>%
        dplyr::mutate(sizepatch_color=dplyr::case_when(sizepatch=="1_small"~"coral",
                                                sizepatch=="2_medium"~"orange",
                                                sizepatch=="3_largest"~"yellow"))


      mymap=leaflet::leaflet(shape) %>%
        leaflet::addPolygons(fill=FALSE,
                             color=~reach_color) %>%
        leaflet::addPolygons(data=patches,
                             fillColor=~sizepatch_color,
                             fill=TRUE,stroke=FALSE,
                             group="water patches size") %>%
        leaflet::addTiles(group = "OSM map") %>%
        leaflet::addWMSTiles(
          baseUrl = "https://geoserver-dev.evs.ens-lyon.fr/geoserver/glourb/wms?",
          layers = "gsw_change",  # Le nom de la couche
          options = leaflet::WMSTileOptions(
            version = "1.1.0",
            format = "image/png",
            transparent = TRUE,
            crs = "EPSG:4326"
          ),
          attribution = "2016 EC JRC/Google",
          group="GSW change intensity"
        ) %>%
        leaflet::addProviderTiles(leaflet::providers$Esri.WorldImagery,
                                  group = "Photo") %>%
        leaflet::addLayersControl(
          overlayGroups = c("OSM map","Photo","GSW change intensity","water patches size"),
          options = leaflet::layersControlOptions(collapsed = FALSE)) %>%
        leaflet::hideGroup("Photo") %>%
        leaflet::hideGroup("water patches size")

      print(mymap)
    })

  })
}
