#' mod_global UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_global_ui <- function(id){
  ns <- NS(id)
  tagList(
    golem_add_external_resources(),
    fluidRow(column(width=3,

                    wellPanel(
                      radioButtons(ns("selection"),
                                   "Show cities in selection",
                                   c("All (993)",
                                     "Selection 1 -Discourses- (302)",
                                     "Selection 1 -GSW- (298)"),
                                   selected="All (993)"),
                      div(id=ns("select_var_help"),
                          selectInput(ns("select_var"),
                                      label="Choose variable to display as color on map and plots",
                                      selected="cluster",
                                      choices=c("cluster",
                                                c(glourbi::sep_vars(glourbi::all_cities)$vars_cat,
                                                  glourbi::sep_vars(glourbi::all_cities)$vars_num))),
                          textOutput(ns("description_var")),
                          conditionalPanel(
                            condition = "input.select_var == 'cluster'",ns=ns,
                            numericInput(ns("nclust"),
                                         "cluster: nb of classes",
                                         min=2,max=30, value=5)
                          )
                      ),
                      plotOutput(ns("plot_palette"))
                    ),#wellPanel
    ),#column
    column(width=9,
           leaflet::leafletOutput(ns("global_map")),
           div(id=ns("mod_global_menu_help"),
               tabsetPanel(
                 tabPanel("city flash card",
                          fluidRow(
                            column(width=6,
                                   uiOutput(ns("city_name")),
                                   HTML("<p>The description of <b>one city</b> across all descriptors.</p>
                                                     <p>For <b>quantitative descriptors</b>, the position of the dot along the line corresponds to the <b>rank</b> of the considered city among all cities (on the left: lowest rank, on the right, highest rank).</p>
                                                     <p>For <b>categorical descriptors</b>, labels and colors indicate the value of the variable for the considered city.</p>
                                                     <p><b>Choose a city by clicking on the map.</b></p>")
                            ),#column
                            column(width=6,
                                   plotOutput(ns("plot_city")))#column
                          )#fluidRow

                 ),#tabPanel
                 tabPanel("cities univar",
                          HTML("The <b>distribution of the selected variable</b>, possibly differentiated according to clusters of cities."),
                          checkboxInput(ns("distrib_by_class"),
                                        "Differentiate clusters of cities"),
                          plotOutput(ns("plot_distrib"))
                 ),#tabPanel
                 tabPanel("cities multivar",
                          HTML("The <b>multivariate description of cities across all quantitative descriptors</b> (assessed through a principal component analysis)"),
                          fluidRow(
                            column(width=6,
                                   plotly::plotlyOutput(ns("varpcaplot"))),
                            column(width=6,
                                   plotly::plotlyOutput(ns("indpcaplot")))
                          )#fluidRow
                 ),#tabPanel
                 tabPanel("cities omnivar",
                          HTML("<p>The distribution of <b>all descriptors</b> across cities: boxplots for quantitative descriptors, barplots for categorical descriptors.</p>
                                           <p> The displayed values for quantitative descriptors can be either their ranks (by default), or their raw values.<p>"),
                          checkboxInput(ns("display_ranks"),
                                        label="Display ranks",
                                        value=TRUE),
                          plotOutput(ns("description_clusters"))
                 ),#tabPanel
                 tabPanel("data",
                          downloadButton(ns("download_btn"), "Download this data"),
                          DT::dataTableOutput(ns("tableclust"))
                 ),#tabPanel
                 # tabPanel("test",
                 #          textOutput(ns("test"))
                 # )#tabPanel
               )#tabsetPanel
           )#div
    )#column
    )#fluidRow
  )#tagList
}

#' mod_global Server Functions
#'
#' @noRd
mod_global_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    r_all_cities=reactive({
      input$nclust
      input$selection
      dataset=glourbi::run_hclust(glourbi::all_cities, nclust=input$nclust)
      if(input$selection=="Selection 1 -Discourses- (302)"){
        dataset=dataset %>%
          dplyr::filter(selection1_Discourses==TRUE)
      }
      if(input$selection=="Selection 1 -GSW- (298)"){
        dataset=dataset %>%
          dplyr::filter(selection1_GSW==TRUE)
      }

      dataset
    })
    r_get_city=reactive({
      if(is.null(input$global_map_marker_click$id)){
        clickid="Lyon--France"}else{
          clickid=input$global_map_marker_click$id}
      clickid
    })
    output$city_name=renderUI({h2(r_get_city())})
    output$tableclust=DT::renderDT({
      r_all_cities()
    })
    output$description_var=renderText({
      glourbi::meta_all_cities %>%
        dplyr::filter(varname==input$select_var) %>%
        dplyr::select(description) %>%
        dplyr::pull()
    })
    r_calc_pca=reactive({
      all_cities_clust=r_all_cities()
      mypca=glourbi::run_pca(all_cities_clust,quali.sup=input$select_var)
    })
    output$varpcaplot=plotly::renderPlotly({
      glourbi::plot_pca(dataset=r_all_cities(),
                        r_calc_pca(),
                        type="var",
                        highlight_subset=r_all_cities() %>%
                          dplyr::filter(ID==r_get_city()))
    })
    output$indpcaplot=plotly::renderPlotly({
      highlight_subset=r_all_cities() %>%
        dplyr::filter(name==r_get_city())
      glourbi::plot_pca(dataset=r_all_cities() %>%
                          dplyr::mutate(selection1=selection1_GSW),
                        r_calc_pca(),
                        type="ind",
                        highlight_subset=highlight_subset)
    })
    output$global_map=leaflet::renderLeaflet({
      glourbi::global_map(dataset=r_all_cities() %>% dplyr::mutate(selection1=selection1_GSW),
                          varname=input$select_var)
    })
    output$plot_palette=renderPlot({
      glourbi::plot_palette(dataset=r_all_cities(),
                            varname=input$select_var)
    })
    output$plot_distrib=renderPlot({
      glourbi::plot_distrib(dataset=r_all_cities(),
                            varname=input$select_var,
                            byclass=input$distrib_by_class)
    })
    output$plot_city=renderPlot({
      glourbi::describe_city(dataset=r_all_cities(),
                             r_get_city())
    })
    output$description_clusters=renderPlot({
      glourbi::describe_clusters(dataset=r_all_cities(),
                                 display_ranks=input$display_ranks)
    })
    output$test=renderText({
      r_get_city()
    })
    output$download_btn <- downloadHandler(
      filename = function() {
        "GloUrb_global_table.csv"
      },
      content = function(file) {
        write.csv(r_all_cities(), file)
      }
    )
  })
}

