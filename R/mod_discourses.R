#' discourses UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_discourses_ui <- function(id){
  ns <- NS(id)
  selection1=glourbi::all_cities %>%
    dplyr::filter(selection1_Discourses==TRUE) %>%
    dplyr::arrange(Urban.Aggl) %>%
    dplyr::pull(Urban.Aggl)
  tagList(
        tabsetPanel(
          id=ns("sub_menu_tab"),
          tabPanel("global",
                   tabsetPanel(id=ns("global_menu_tab"),
                     tabPanel("topics",
                              fluidRow(
                                column(width=3,
                                       HTML("<p>This map displays the <b>most specific topic for each city</b>
                                            (whichever river is considered)
                                            according to the results of the Search Engine Research Pages.</p>
                                            <p>The topics are <b>built automatically based on the contents of scraped pages</b>,
                                            through the clustering of text segments into classes,
                                            which are displayed in tab <b>topics tree</b>.")),
                                column(width=9,
                                       leaflet::leafletOutput(ns("topics_map")))),
                              div(id=ns("clusters"),
                              tags$img(src = "www/clusters_all_14_en.png", height = "700px", width = "1400px"))),
                     tabPanel("localness",
                              fluidRow(
                                column(width=3,
                                       HTML("<p>An <b>index of localness</b> for each scraped page is calculated based on the url's domain and page's language</p>
                                         <p> The following map shows, for each city, the proportion of web pages assessed as local.</p>"),
                                       selectInput(ns("localness"),
                                                   "Page localness based on",
                                                   c("URL","language","URL_and_language"))
                                ),
                                column(width=9,
                                       leaflet::leafletOutput(ns("global_localness_plot")))
                              )#fluidRow
                     ),
                     tabPanel("word search",
                              fluidRow(
                                column(width=3,
                                       textInput(ns("searched_word"),
                                                 "Search this word:",
                                                 value=""),
                                       p("Consider either roughly equally-sized text segments  or complete web pages."),
                                       radioButtons(ns("searched_table"),
                                                    "Consider :",
                                                    choices=c("txt_segment","txt_page"),
                                                    selected="txt_segment"),
                                       actionButton(ns("search_btn"), "Search"),
                                       HTML("<p>NB: in table txt_segment the words have been lemmatized: search for e.g. 'mine' rather than 'mining' or 'tree' rather than 'trees'.</p>
                                            <p> Examples of search: <ul><li>in txt_segment: 'gravel mine'</li><li> in txt_segment: 'global warm OR climate change'</li>
                                            <li>in txt_page: 'water scarcity OR global warming'</li>")
                                ),
                                column(width=9,
                                       leaflet::leafletOutput(ns("word_map"))),
                                downloadButton(ns("download_btn"), "Download this data"),
                                DT::dataTableOutput(ns("searched_lines"))
                              ) #fluidRow
                     )
                   )
          ),
          tabPanel("by city",
                   div(id=ns("city_river"),
                   fluidRow(column(width=3,
                                   selectInput(ns("city"),
                                               "Choose city",
                                               choices=selection1,
                                               selected=selection1[2])
                                   ),
                            column(width=3,
                                   uiOutput(ns("ui_river")))
                   )),#fluidRow #div
                   tabsetPanel(id=ns("by_river_menu"),
                     tabPanel("pages table",
                              DT::dataTableOutput(ns("txt_page"))),
                     tabPanel("segments table",
                              DT::dataTableOutput(ns("txt_segment"))),
                     tabPanel("words",
                              plotOutput(ns("city_words_freq"))),
                     tabPanel("topics",
                              fluidRow(
                                column(width=6,
                                       plotOutput(ns("city_topics_n"))),
                                column(width=6,
                                       plotOutput(ns("city_topics_spec")))
                              )),
                     tabPanel("localness",
                              plotOutput(ns("city_localness_plot")))
                   )
          )# by city tabPanel
        )#tabsetPanel
  )
}

#' mod_discourses Server Functions
#'
#' @noRd
mod_discourses_server <- function(id,conn, r_val){
  moduleServer(id, function(input, output, session){
    ns <- session$ns

    r_get_txt_city_rivers=reactive({
      glourbi::get_city_tib(name="txt_city_rivers",
                            thisCityCode=glourbi::to_citycode(input$city),
                            conn=conn)
    })
    r_get_txt_page=reactive({
      tib_page=glourbi::get_txt_page(thisCityCode=glourbi::to_citycode(input$city),
                                     thisRiver=input$river,
                                     conn=conn)

    })
    r_get_txt_segment=reactive({
      tib_segment=glourbi::get_txt_segment(thisCityCode=glourbi::to_citycode(input$city),
                                           thisRiver=input$river,
                                           conn=conn)
    })

    r_get_searched_lines <- eventReactive(input$search_btn, {
      var=switch(input$searched_table,
                 txt_page="text_en",
                 txt_segment="text")
      query_condition <- process_query(input$searched_word, var)
      query <- glue::glue("SELECT * FROM {input$searched_table} WHERE {query_condition};")
      result=DBI::dbGetQuery(conn=conn,
                             query)
      result
    })
    output$searched_lines=renderDataTable({
      searched_lines=r_get_searched_lines()
      searched_lines%>%
        format_table()
    })


    output$ui_river=renderUI({
      rivers=r_get_txt_city_rivers() %>%
        dplyr::pull(river_en) %>%
        unique()
      selectInput(ns("river"),"river",choices=rivers, selected=rivers[1])
    })
    output$city_words_freq=renderPlot({
      tib_segment=r_get_txt_segment()
      tib_word = tib_segment %>%
        tidytext::unnest_tokens(input=text,output=word)
      result_word=tib_word %>%
        # count the nb of occurrences of each word
        dplyr::group_by(word) %>%
        dplyr::count() %>%
        # arrange by decreasing order
        dplyr::arrange(desc(n)) %>%
        dplyr::ungroup()


      result_word[1:30,] %>% # show first 30 words
        dplyr::mutate(word = forcats::fct_reorder(word, n)) %>%
        ggplot2::ggplot(mapping = ggplot2::aes(x = word,
                                               y = n)) +
        ggplot2::geom_col(fill = "#b2df8a", position = "stack") +
        ggplot2::coord_flip() + # flip x and y coordinates
        ggplot2::labs(x = "lemma",
                      y = "frequency",
                      title = paste0(input$city, " and ", input$river),
                      subtitle = "Queries (in english and local languages)") +
        ggplot2::theme_bw(base_family = "CenturySch")
    })
    output$txt_page=DT::renderDT({
      result=r_get_txt_page() %>%
        format_table()
    },escape=FALSE)

    output$txt_segment=DT::renderDT({
      result=r_get_txt_segment() %>%
        format_table()
    },escape=FALSE)

    output$global_localness_plot=leaflet::renderLeaflet({
      tib_txt_localness=DBI::dbReadTable(name="txt_localness",
                                         conn=conn) %>%
        tibble::as_tibble()
      tib_txt_localness=tib_txt_localness %>%
        dplyr::filter(localness==input$localness) %>%
        dplyr::mutate(perc_local=round(perc_local,1)) %>%
        dplyr::group_by(citycode,river_en) %>%
        dplyr::summarise(urban_aggl=paste(urban_aggl,collapse="-"),
                         perc_all=paste(perc_local,collapse=" and "),
                         perc_local=round(mean(perc_local),2),
                         latitude=mean(latitude),
                         longitude=mean(longitude),
                         .groups="drop")
      sf_localness <- sf::st_as_sf(tib_txt_localness, coords = c("longitude", "latitude"), crs = 4326)
      # Créer une palette de couleurs
      pal <- leaflet::colorNumeric(
        palette = grDevices::colorRampPalette(c("red", "yellow", "blue"))(100),
        domain = tib_txt_localness$perc_local
      )

      #Carte Leaflet
      leaflet::leaflet() %>%
        leaflet::addProviderTiles("CartoDB.Positron") %>%
        # Ajouter les polygones du monde
        # Ajouter les points des villes avec la palette de couleurs
        leaflet::addCircleMarkers(data = sf_localness,
                                  radius = 5,
                                  color = ~pal(perc_local),
                                  fillOpacity = 0.8,
                                  popup = ~glue::glue("<p>City: {urban_aggl}</p>
                                            <p>River: {river_en}</p>
                                            <p>Proportion of local pages: {perc_local}%</p>")) %>%
        # Ajouter une légende pour la couleur
        leaflet::addLegend(pal = pal,
                           values = tib_txt_localness$perc_local,
                           title = "Pourcentage de pages web locales",
                           position = "bottomright")


    })

    output$city_localness_plot=renderPlot({
      tib_cityriver_localness=glourbi::get_city_tib(name="txt_localness",
                                                    thisCityCode=glourbi::to_citycode(input$city),
                                                    conn=conn) %>%
        dplyr::filter(river_en==input$river) %>%
        dplyr::select(localness,n=n_local,n_tot) %>%
        dplyr::mutate(left=n_tot-max(n))
      tib_cityriver_localness=
        dplyr::bind_rows(tib_cityriver_localness,
                         tibble::tibble(localness="none",
                                        n=unique(tib_cityriver_localness$left))) %>%
        dplyr::select(localness,n) %>%
        dplyr::mutate(ntot=sum(n)) %>%
        dplyr::mutate(perc=100*n/ntot) %>%
        dplyr::mutate(localness=factor(localness,levels=c("URL_and_language","language","URL","none")))


      tib_cityriver_localness %>%
        ggplot2::ggplot(mapping = ggplot2::aes(x = localness,
                                               y = perc,
                                               fill = localness)) +
        ggplot2::geom_col() +
        ggplot2::scale_fill_manual(values = c("#abdda4","#abdda4","#abdda4","#41b6c4")) +
        ggplot2::labs(title = paste0("Web pages about ", input$city, " et ", input$river,"."),
                      x = "",
                      y = "%") +
        ggplot2::scale_y_continuous(limits = c(0,100)) +
        ggplot2::theme_bw(base_family = "CenturySch", base_size = 14) +
        ggplot2::theme(legend.title = ggplot2::element_blank(), legend.position = "bottom")
    })

    output$topics_map=leaflet::renderLeaflet({

      txt_topics=DBI::dbReadTable(conn=conn,name="txt_topics") %>%
        tibble::as_tibble()
      txt_topics=txt_topics %>%
        dplyr::mutate(spec=dplyr::case_when(spec==Inf~1000,
                                            TRUE~spec))
      txt_topics_summary=txt_topics %>%
        dplyr::arrange(citycode,desc(spec),desc(n)) %>%
        dplyr::group_by(citycode) %>%
        dplyr::slice(1) %>%
        dplyr::select(citycode,cluster_name,river_en,spec,n,couleur,prop,npages) %>%
        dplyr::left_join(glourbi::all_cities %>% dplyr::select(Urban.Aggl,citycode=ID,Latitude,Longitude),by="citycode") %>%
        na.omit() %>%
        sf::st_as_sf(coords=c("Longitude","Latitude"))
      #Carte Leaflet
      colors_and_labels=txt_topics_summary %>%
        dplyr::select(cluster_name,couleur) %>%
        sf::st_drop_geometry() %>% unique()
      leaflet::leaflet() %>%
        leaflet::addProviderTiles("CartoDB.Positron") %>%
        # Ajouter les polygones du monde
        # Ajouter les points des villes avec la palette de couleurs
        leaflet::addCircleMarkers(data = txt_topics_summary,
                                  radius = 5,
                                  color = ~ couleur,
                                  fillOpacity = 0.8,
                                  popup = ~glue::glue("<p>City: {Urban.Aggl}</p>
                                            <p>River: {river_en}</p>
                                            <p>Topic: {cluster_name}</p>
                                            <p>Specificity score: {spec}</p>"))  %>%
        # Ajouter une légende pour la couleur
        leaflet::addLegend(values = txt_topics_summary,
                           colors=colors_and_labels$couleur,
                           labels=colors_and_labels$cluster_name,
                           title = "Topics",
                           position = "bottomright")


    })

    output$city_topics_n=renderPlot({
      df=glourbi::get_city_tib("txt_topics",
                               thisCityCode=glourbi::to_citycode(input$city),
                               conn=conn) %>%
        dplyr::filter(river_en==input$river) %>%
        dplyr::mutate(prop=dplyr::case_when(is.na(prop)~0,
                                            !is.na(prop)~prop))
      # plot results
      df_colors=df %>% dplyr::select(couleur,cluster_name) %>% unique()
      df %>%
        ggplot2::ggplot(ggplot2::aes(x = forcats::fct_reorder(cluster_name, prop),
                                     y = prop,
                                     fill = cluster_name)) +
        ggplot2::geom_bar(stat = "identity",
                          position = ggplot2::position_dodge(width = 0.9), width = 0.8) +
        ggplot2::scale_fill_manual(values = setNames(df_colors$couleur, df_colors$cluster_name)) +
        ggplot2::labs(title = paste0("Topics distribution for ", input$city, " and the ", input$river),
                      subtitle = paste0("Number of segments: ", sum(df$n,na.rm=TRUE),", number of pages: ", unique(df$npages)),
                      y = "%",
                      x = "") +
        ggplot2::coord_flip() +
        ggplot2::theme_bw() +
        ggplot2::theme(legend.position = "none")
    })


    output$city_topics_spec=renderPlot({
      df=glourbi::get_city_tib("txt_topics",
                               thisCityCode=glourbi::to_citycode(input$city),
                               conn=conn)
      # plot results
      df_colors=df %>% dplyr::select(couleur,cluster_name) %>% unique()
      df %>%
        ggplot2::ggplot(ggplot2::aes(x = forcats::fct_reorder(cluster_name, spec),
                                     y = spec,
                                     fill = cluster_name)) +
        ggplot2::geom_bar(stat = "identity",
                          position = ggplot2::position_dodge(width = 0.9), width = 0.8) +
        ggplot2::scale_fill_manual(values = setNames(df_colors$couleur, df_colors$cluster_name)) +
        ggplot2::labs(title = paste0("Topics specificity for ", input$city, " and the ", input$river),
                      subtitle = paste0("Number of segments: ", sum(df$n,na.rm=T),", number of pages: ", unique(df$npages)),
                      y = "specificity score",
                      x = "") +
        ggplot2::coord_flip() +
        ggplot2::theme_bw() +
        ggplot2::theme(legend.position = "none")
    })

    output$word_map=leaflet::renderLeaflet({
      searched_lines=r_get_searched_lines()
      print(colnames(searched_lines))
      result=all_cities %>%
        dplyr::filter(selection1_Discourses==TRUE) %>%
        dplyr::select(ID,Urban.Aggl,Latitude,Longitude,selection1_Discourses) %>%
        dplyr::left_join(searched_lines, by=c("ID"="citycode")) %>%
        dplyr::group_by(Urban.Aggl,Latitude,Longitude,selection1_Discourses) %>%
        dplyr::summarise(nocc=dplyr::n(),
                         void=all(is.na(link)),
                         .groups="drop") %>%
        dplyr::mutate(nocc=dplyr::case_when(void~0,
                                     !void~nocc)) %>%
        dplyr::mutate(void=dplyr::case_when(void~"grey",
                                            TRUE~"coral")) %>%
        dplyr::mutate(logn=3*log10(nocc))
      leaflet::leaflet() %>%
        leaflet::addProviderTiles("CartoDB.Positron") %>%
        leaflet::addCircleMarkers(data = result,
                                  label = paste0(result$Urban.Aggl,", ", result$nocc),
                                  color = ~void,
                                  opacity = 0.8,
                                  radius =~3*logn)
    })

    output$download_btn <- downloadHandler(
      filename = function() {
        searched_word=stringr::str_replace_all(input$searched_word," OR ","_OR_")
        glue::glue("corpus_{searched_word}.csv")
      },
      content = function(file) {
        write.csv(r_get_searched_lines(), file)
      }
    )

    observeEvent(input$sub_menu_tab, {
      r_val$sub_menu_tab = input$sub_menu_tab
    })
  }

  )}
