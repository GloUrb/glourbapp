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
         tabPanel("global word search",
                  fluidRow(
                    column(width=3,
                           textInput(ns("searched_word"),
                                     "Search this word:",
                                     value="drought"),
                           p("Consider either roughly equally-sized text segments  or complete web pages."),
                           radioButtons(ns("searched_column"),
                                        "Consider :",
                                        choices=c("text_en","lemmatext"),
                                        selected="text_en"),
                           actionButton(ns("search_btn"), "Search")
                    ),
                    column(width=9,
                           leaflet::leafletOutput(ns("word_map"))),
                    downloadButton(ns("download_btn"), "Download this data"),
                    DT::dataTableOutput(ns("searched_lines"))
                  ) #fluidRow
          ),# tabPanel word search
          tabPanel("by city",
                   div(id=ns("city_river"),
                   fluidRow(column(width=3,
                                   selectInput(ns("city"),
                                               "Choose city",
                                               choices=selection1,
                                               selected=selection1[2])
                                   ),
                            column(width=3,
                                   uiOutput(ns("ui_river"))),
                            column(width=3,
                                   uiOutput(ns("ui_query_language")))
                   )),#fluidRow #div
                   tabsetPanel(id=ns("by_river_menu"),
                     tabPanel("pages table",
                              DT::dataTableOutput(ns("txt_page"))),
                     tabPanel("words & topics",
                              fluidRow(
                                column(width=6,
                                       plotOutput(ns("city_words_freq"))
                                       ),
                                column(width=6,
                                       plotOutput(ns("city_topics_n")),
                                       plotOutput(ns("city_topics_spec")))
                              ))
                   )
          ),# by city tabPanel,
         tabPanel("method",
                  tags$img(src="www/methode_appli.png", width = "1200px"))
        )#tabsetPanel
  )
}

#' mod_discourses Server Functions
#'
#' @noRd
mod_discourses_server <- function(id,conn, r_val){
  moduleServer(id, function(input, output, session){
    ns <- session$ns

    r_get_txt_city_river_language=reactive({
      glourbi::get_city_tib(name="txt_city_river_language",
                            thisCityCode=glourbi::to_citycode(input$city),
                            conn=conn)
    })
    r_get_txt_page=reactive({
      tib_page=glourbi::get_txt_page(thisCityCode=glourbi::to_citycode(input$city),
                                     thisRiver=input$river,
                                     conn=conn)
      if(input$query_language!="all"){
        if(input$query_language=="english"){
            query_language="en"
          }else{
            query_language=stringr::str_replace(input$query_language,"local: ","")
          }
        tib_page=tib_page %>%
          dplyr::filter(hl==query_language)
      }
      tib_page
    })
    r_get_txt_segment=reactive({
      tib_segment=glourbi::get_txt_segment(thisCityCode=glourbi::to_citycode(input$city),
                                           thisRiver=input$river,
                                           conn=conn)
    })
    r_result <- reactiveVal({
      # Requête par défaut
      query_condition <- process_query("climate change","text_en")
      query <- glue::glue("SELECT * FROM txt_page WHERE {query_condition};")
      DBI::dbGetQuery(conn = conn, query)
    })
    observeEvent(input$search_btn, {
      query_condition <- process_query(input$searched_word, input$searched_column)
      query <- glue::glue("SELECT * FROM txt_page WHERE {query_condition};")
      result=DBI::dbGetQuery(conn=conn,
                             query)
      r_result(result)
    })
    output$searched_lines=DT::renderDT({
      searched_lines=r_result()
      searched_lines %>%
        dplyr::select(urban_aggl,country_en,position,link,trans_snippet,text_en,lemmatext) %>%
        format_table()
    })
    output$ui_river=renderUI({
      rivers=r_get_txt_city_river_language() %>%
        dplyr::pull(river_en) %>%
        unique()
      selectInput(ns("river"),"river",choices=rivers, selected=rivers[1])
    })
    output$ui_query_language=renderUI({
      query_languages=r_get_txt_city_river_language() %>%
        dplyr::pull(query_language) %>%
        unique()
      selectInput(ns("query_language"),
                  "query language",
                  choices=c("all",query_languages),
                  selected=query_languages[2])
    })
    output$city_words_freq=renderPlot({
      tib_page=r_get_txt_page()
      tib_word=tib_page %>%
        tidytext::unnest_tokens(input=lemmatext,output=word)
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
        dplyr::select(hl,position, link, trans_snippet,text_en,lemmatext) %>%
        dplyr::arrange(hl, position) %>%
        format_table()
    },escape=FALSE)

    output$city_topics_n=renderPlot({
      df=glourbi::get_city_tib("txt_topics",
                               thisCityCode=glourbi::to_citycode(input$city),
                               conn=conn) %>%
        dplyr::filter(river_en==input$river) %>%
        dplyr::mutate(prop=dplyr::case_when(is.na(prop)~0,
                                            !is.na(prop)~prop))
      # plot results
      df_colors=df %>% dplyr::select(couleur,topic_name) %>% unique()
      df %>%
        ggplot2::ggplot(ggplot2::aes(x = forcats::fct_reorder(topic_name, prop),
                                     y = prop,
                                     fill = topic_name)) +
        ggplot2::geom_bar(stat = "identity",
                          position = ggplot2::position_dodge(width = 0.9), width = 0.8) +
        ggplot2::scale_fill_manual(values = setNames(df_colors$couleur, df_colors$topic_name)) +
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
      df_colors=df %>% dplyr::select(couleur,topic_name) %>% unique()
      df %>%
        ggplot2::ggplot(ggplot2::aes(x = forcats::fct_reorder(topic_name, spec),
                                     y = spec,
                                     fill = topic_name)) +
        ggplot2::geom_bar(stat = "identity",
                          position = ggplot2::position_dodge(width = 0.9), width = 0.8) +
        ggplot2::scale_fill_manual(values = setNames(df_colors$couleur, df_colors$topic_name)) +
        ggplot2::labs(title = paste0("Topics specificity for ", input$city, " and the ", input$river),
                      subtitle = paste0("Number of segments: ", sum(df$n,na.rm=T),", number of pages: ", unique(df$npages)),
                      y = "specificity score",
                      x = "") +
        ggplot2::coord_flip() +
        ggplot2::theme_bw() +
        ggplot2::theme(legend.position = "none")
    })

    output$word_map=leaflet::renderLeaflet({
      searched_lines=r_result()
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
