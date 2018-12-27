################################################################################
##
## Title: Appendix S4. Shiny app to visualize the weekly and day/night 3D-UD 
##        contours of common dentex individuals and explore their behaviour 
##        during the spawning period.
## Author: Eneko Aspillaga (University of Barcelona)
## E-mail: eneko.aspillaga@gmail.com
## Date: December 2018
## Link: https://aspillaga.shinyapps.io/3DUD_weeks_spawning/
##
################################################################################

library(shiny)
library(plotly)


# Load and prepare data --------------------------------------------------------

# Load topography
load("./data/topography.rda")

# Load table with dates
load("./data/dates.rda")

# Load contours of the first week group
load("./data/contours_spawning_dentex_1.rda")

# List of individuals and colors
ind <- names(contours.spawn[[1]])
names(ind) <- paste0("Dentex #", substr(ind, start = 7, stop = 9))

col1 <- c("#C22618", "#F08300", "#FFDD06", "#CECE00", "#15892E",
          "#6ABFAA", "#00AEEA", "#005D99", "#D94E96", "#664882",
          "#DFB2A2", "#936037", "#4C4C47")
col2 <- c("#E3817D", "#F7B66F", "#FFE97B", "#E8E697", "#9ACCA3",
          "#A8D7CB", "#96D4EC", "#7FB2C9", "#E5ACCD", "#B39FCC",
          "#F5E4DF", "#BFADA1", "#A8A7A7")

# Day/Night periods
diel <- c("Day" = "day", "Night" = "night")

# List of UD probability contours
prob <- names(contours.spawn[[1]][[1]][[1]])
names(prob) <- paste0(as.numeric(prob) * 100, "%")


# Define UI for the app tha shows 3D UD for dentex -----------------------------

ui <- fluidPage(

  # Align checkboxes
  tags$head(
    tags$style(
      HTML(
        ".checkbox-inline {
        margin-left: 0px;
        margin-right: 10px;
        }
        .checkbox-inline+.checkbox-inline {
        margin-left: 0px;
        margin-right: 10px;
        }"
      ))),

  # App title -----------
  p("Aspillaga", em ("et al."), "2018.", 
    em("Methods in Ecology and Evolution."), "Appendix S4.",
    style = "font-size:10pt; color: #B6B6B4; line-heigh:20px; 
    margin-bottom:-10pt"),
  headerPanel("Weekly day/night 3D-UD contours"),

  # Sidebar panel for inputs ----
  sidebarPanel(
    width = 3,
    
    helpText("This app allows to visualize weekly day/night 3D-UDs 
             of common dentex individuals before and during the spawning
             season."),
    br(),

    # Button to update plot ----
    actionButton("button", "Refresh plot", width = "100%"),
    br(),
    br(),
    
    # Date ----
    h4("Time period (weeks)"),
    helpText("The spawning season beggins in May. The spawning behavior is
             specially observed at night."),
    sliderInput("date", label = NULL,
                min = dates$day[1], max = dates$day[nrow(dates)], step = 7, 
                value = dates$day[1],
                timeFormat = "%b %d"),
    
    # Day-night period ----
    checkboxGroupInput("diel", h4("Day/night period"),
                       choices = diel, selected = c("day", "night"), 
                       inline = TRUE),
    
    # Individuals
    checkboxGroupInput("ind", h4("Individuals"),
                       choices = ind, inline = TRUE),

    # Contour levels
    selectInput("level", h4("UD contour"),
                choices = prob, selected = prob[1]),

    # Show topographic features
    checkboxGroupInput("topofeat",
                       h4("Topographic features"),
                       choices = list("Show topography" = "topo",
                                      "Show sea surface" = "surface"),
                       selected = "topo"),

    # Opacity
    sliderInput("opac", h4("UD contour opacity"),
                min = 0, max = 1, value = 1),
    
    br(),
    helpText("This app is part of the online supporting information of the
             following research paper:"),
    helpText("Aspillaga E., Safi K., Hereu B. & Bartumeus F. (2018).",
             strong("Accounting for topography in the 3D space use modelling
                    of Eulerian telemetry data."), "Under review in",
             em("Methods in Ecology and Evolution."))
    
  ),

  # Main panel for displaying outputs ----
  mainPanel(
    plotlyOutput("plot", height = "600px", width = "900px")
  )
)


# Define server logic ----------------------------------------------------------

server <- function(input, output) {

  # Define scene layout for the plot
  scene.react <- reactive({event_data("plotly_relayout", source = "link")})

  scene.ini <- list(aspectratio = list(x = 1, y = 1, z = 0.5),
                    xaxis = list(range = range(topo$x),
                                 visible = FALSE, title = "Lat (m N)",
                                 showspikes = FALSE),
                    yaxis = list(range = range(topo$y),
                                 visible = FALSE, title = "Long (m E)",
                                 showspikes = FALSE),
                    zaxis = list(range = range(topo$z), type = "linear",
                                 title = "", showspikes = FALSE,
                                 tickvals = seq(-60, 0, 20)))

  # Create the base plot with and without topography
  p.notopo <- plot_ly(type = "mesh3d", hoverinfo = "skip", source = "link")
  p <- add_surface(p.notopo, x = topo$x, y = topo$y, z = topo$z,
                   cmax = 0, cmin = -70, name = "Depth (m)",
                   colorscale = colorscale, showscale = FALSE,
                   contours = list(x = list(highlight = FALSE),
                                   y = list (highlight = FALSE),
                                   z = list(highlight = FALSE)))

  
  # Reactive button to make the plot
  plot <- eventReactive(input$button, {
    
    
    # Topography
    if (!"topo" %in% input$topofeat) p <- p.notopo
    
    # Update scene layout
    scene <- isolate(c(scene.ini, camera = list(scene.react()$scene.camera)))
    p <-  layout(p, margin = list(l = 0, r = 0, t = 0, b = 0), scene = scene)
    
    # Add sea surface
    if ("surface" %in%input$topofeat) {
      p <- add_mesh(p, x = range(topo$x)[c(1, 1, 2, 2)],
                    y = range(topo$y)[c(1, 2, 2, 1)],
                    z = c(0,0,0,0), opacity = 0.3,
                    facecolor = rep("#9BD3F4", 3))
    }
    
    # Add individual UD contours
    w <- dates$weeks[dates$day == input$date]
    date.indx <- dates$indx[dates$day == input$date]
    load(paste0("./data/contours_spawning_dentex_", date.indx, ".rda"))
    
    for (i in input$ind) {
      for (d in input$diel) {
        c <- contours.spawn[[w]][[i]][[d]][[input$level]]
        
        col.i <- ifelse(d == "day", col2[ind == i], col1[ind == i])
        
        name =  paste0(names(ind)[ind == i], "\n", names(diel)[diel == d])
        
        p <- add_mesh(p, x = c$contour[, 1], y = c$contour[, 2],
                      z = -c$contour[, 3],
                      i = c$indx[, 1], j = c$indx[, 2], k = c$indx[, 3],
                      facecolor = rep(col.i, nrow(c$indx)),
                      opacity = input$opac,
                      showlegend = TRUE, name = name, hoverinfo = "name",
                      flatshading = TRUE)
      }
    }
    
    # Plot
    return(p)
    
  })
  
  
  # Render the plot
  output$plot <- renderPlotly({
    if (input$button == 0) {
      scene <- isolate(c(scene.ini, camera = list(scene.react()$scene.camera)))
      layout(p, margin = list(l = 0, r = 0, t = 0, b = 0), scene = scene)
    } else {
      plot()
    }
  })
}

# Run the app ------------------------------------------------------------------
shinyApp(ui = ui, server = server)
