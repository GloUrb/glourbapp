#' help_guide UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @import shiny
#' @importFrom cicerone use_cicerone
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_help_ui <- function(id) {
  ns <- NS(id)
  tagList(
    actionButton(ns("help_btn"), " Help",
                 icon = icon("circle-question")),
    cicerone::use_cicerone() # Load the cicerone dependencies
  )
}

#' help_guide Server Functions
#'
#' @importFrom cicerone Cicerone
#'
#' @noRd
mod_help_server <- function(id, r_val){
  moduleServer(id, function(input, output, session){
    ns <- session$ns



    # Set up a dynamic observer based on the active tab
    observeEvent(input$help_btn, {
      # Reset tour steps for each new tab
      # tour$reset()

      # Define the cicerone tours for each tab
      tour <- cicerone::Cicerone$new(
        done_btn_text = "Done",
        close_btn_text = "Close",
        next_btn_text = "Next",
        prev_btn_text = "Prev",
      )

      # browser()

      #| notes:
      #| <p> for paragraph, <br/> for line break
      #| <strong> for bold, <em> for italics, and <ul> for bullet points
      #| <a href="https://www.google.com">Google</a> for links
      #| <img src="www/logos_mapdo_evs_ofb.png" width="100px"> for images
      #| <code> for code snippets
      #|
      print(r_val$main_menu_tab)
      if (r_val$main_menu_tab == "Global") {
        tour$step("main_menu_tab",
                  "Global",
                  description="Explore the general characteristics of the GloUrb study sites.
                  <p><small>Exit and choose another tab before pressing help if you want information about the other tabs.</small></p>",
                  position="bottom")

        tour$step("mod_global_1-selection", "Study sites selection",
                  description = "You can display all originally considered cities or only those in sub-selections (selection1 for now).",
                  position = "right")


        tour$step("mod_global_1-global_map", "Global map",
                  description = "This global map displays <b>GloUrb study sites</b>. You can: <ul>
                  <li>click on each circle to display popup information, and</li>
                  <li> choose the descriptor displayed as color.</li></ul>",
                  position = "left")

        tour$step("mod_global_1-select_var_help", "Main descriptor",
                  description = "<p>Choose the <b>main descriptor</b> according to which you want to display cities in the map and plots.</p>
                                 <p>By default the descriptor is <b>cluster</b> i.e. a class obtained through a clustering of all available descriptors. You can choose interactively the number of clusters you wish to consider.</p>
                                 <p>You can also display any other available <b>quantitative or categorical descriptor of cities</b></p>",
                  position = "right")
        tour$step("mod_global_1-plot_palette","Palette",
                  description="The palette of colors according to which the main descriptor is displayed.")

        tour$step("mod_global_1-mod_global_menu_help","Analysis of city descriptors",
                  description=HTML("This menu allows you to display
                                   <ul><li><b>city flash card</b>: the characteristics of <b>one city</b></li>
                                   <li><b>cities univar</b> the distribution of <b>one descriptor</b> (the one selected for the global map visualization) across <b>all cities</b></li>
                                   <li><b>cities multivar</b> the <b>multivariate</b> description of data across <b>all cities</b></li>
                                   <li><b>cities omnivar</b> the univariate description of <b>all descriptors</b> across <b>all cities</b></li>."))
      }
      if (r_val$main_menu_tab == "Surface Water") {
        tour$step("main_menu_tab",
                  "Surface Water",
                  description=HTML("Visualize the <b>extent and type of changes in surface water</b>.
                  <p><small>Exit and choose another tab before pressing help if you want information about the other tabs.</small></p>"),
                  position="bottom")

        tour$step("mod_GSW_1-city_help", "Choose city",
                  description = HTML("<p>Results in this module are available for <b>all cities in #selection1</b>.</p>
                  <p><small> Delete and start typing to select a particular city without scrolling through them all.</small></p>."),
                  position = "bottom")

        tour$step("mod_GSW_1-map_city", "Map",
                  description = HTML("<p>This map displays the study sites and their sub-areas defined through <ul>
                                      <li><b>longitudinal delineation</b>(upstream-urban-downstream)</li>
                                      <li><b>lateral zonation</b> (corridor-plain).</li></ul></p>
                                     <p>The colors of layer GSW change correspond to the <b>intensity of change in surface water</b> over the years <b>2000-2020</b> compared to years <b>1980-2000</b>.</p>"),
                  position = "right")

        tour$step("mod_GSW_1-GSWdescription", "Surface water change intensity.",
                  description = HTML("Number of pixels with <ul>
                                     <li><b><span style='color: #00ff00'>positive</span></b> change in surface water</li>
                                     <li><b>no change</b> in surface water, and</li>
                                     <li><b><span style='color: #ff0000'>negative</span></b> change in surface water</li></ul>
                                     per longitudinal and lateral zone."),
                  position = "right")
      }
      if (r_val$main_menu_tab == "Discourses") {

        if(r_val$sub_menu_tab=="global"){
        tour$step("main_menu_tab",
                  "Discourses",
                  description="Visualize the <b>main topics</b> surrounding all <b>associations of city and river names</b> (assessed using web-scraped textual contents).
                  <p><small>Exit and choose another tab before pressing help if you want information about the other tabs.</small></p>",
                  position="bottom")
        tour$step("mod_discourses_1-sub_menu_tab",
                  "Global/city",
                  description="Discourses results can be analysed <b>globally</b> or <b>city by city</b>",
                  position="top")
        tour$step("mod_discourses_1-global_menu_tab",
                  "Global features",
                  description="The following global web corpus features are displayed:
                  <ul><li><b>topics</b> (content topic modelling)</li>,
                  <li><b>localness</b> (quantifying the proportion of web contents produced locally) and </li>
                  <li><b>word search</b> (enabling users to search for a particular word in the corpus and see it in context).</li></ul>",
                  position="bottom")
        tour$step("mod_discourses_1-topics_map",
                  "Map of topics",
                  description="This map displays the <b>topic most specific to each city</b> according to web discourses associating cities to their rivers.",
                  position="left")
        tour$step("mod_discourses_1-clusters",
                  "Topics",
                  description="This displays how the <b>topics are defined</b>, based the automatic Reinert classification carried out on text segments.",
                  position="top")
        }
        if(r_val$sub_menu_tab=="by city"){
          tour$step("mod_discourses_1-city_river",
                    "River(s)",
                    description="For a city several rivers can be considered. Search engine results pages are retrieved for each of them.",
                    position="bottom")
          tour$step("mod_discourses_1-by_river_menu",
                    "Results by city",
                    description="Display either <ul><li>the <b>pages table</b> with descriptors related to complete web pages</li>
                    <li><b>the <segments table</b> with descriptors related to short segments of text (considered for topic modelling)</li>
                    <li><b>words</b> the most frequent words used in this part of the corpus</li>
                    <li><b>topics</b> the most frequent (left) and most specific (right) topics in this part of the corpus</li>
                    <li><b>localness</b> the proportion of local or non-local pages in this part of the corpus according to various criteria.</li>",
                    position="bottom")

        }


      }
      if (r_val$main_menu_tab == "OpenStreetMap") {
        tour$step("main_menu_tab",
                  "OpenStreetMap",
                  description="Explore and analyse landuse and equipments related to the association of city and river (assessed using OpenStreetMap data).
                  <p><small>Exit and choose another tab before pressing help if you want information about the other tabs.</small></p>",
                  position="bottom")
        tour$step("mod_OSM_1-city_help", "Choose city",
                  description = HTML("<p>Results in this module are available for <b>all cities in #selection1</b>.</p>
                  <p><small> Delete and start typing to select a particular city without scrolling through them all.</small></p>."),
                  position = "right")

      }

      tour$init()$start() # Start the tour
    })
  })
}
