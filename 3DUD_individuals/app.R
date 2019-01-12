################################################################################
##
## Title: Appendix S2. Shiny app to visualize and compare the 3D-UD contours of
##        common dentex individuals in the Medes Islands (NW Mediterranean Sea).
## Author: Eneko Aspillaga (University of Barcelona)
## E-mail: eneko.aspillaga@gmail.com
## Date: December 2018
## Link: https://aspillaga.shinyapps.io/3DUD_individuals/
##
################################################################################

library(shiny)
library(plotly)


# Load and prepare data --------------------------------------------------------

# Load topography
load("./data/topography.rda")

# Contour files
files <- list.files(path = "./data/", pattern = "(cont_)", full.names = TRUE)
ind.files <- unlist(strsplit(files, "_"))[1:4 == 3]
cont.files <- substr(unlist(strsplit(files, "_"))[1:4 == 4], 1, 2)

# List of individuals and colors to plot the 3D-UDs
ind <- unique(ind.files)
names(ind) <- paste0("Dentex #", substr(ind, start = 7, stop = 9))
col <- c("#C22618", "#F08300", "#FFDD06", "#CECE00", "#15892E",
         "#6ABFAA", "#00AEEA", "#005D99", "#D94E96", "#664882",
         "#DFB2A2", "#936037", "#4C4C47")

# List of UD probability contours
# List of UD probability contours
prob <- unique(cont.files)
names(prob) <- paste0(prob, "%")


# Define UI for the app --------------------------------------------------------

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

  # App title ----
  p("Aspillaga", em ("et al."), "2018.", 
     em("Methods in Ecology and Evolution."), "Appendix S2.",
     style = "font-size:10pt; color: #B6B6B4; line-heigh:20px; 
              margin-bottom:-10pt"),
  headerPanel("3D-UD contours of common dentex individuals"),

  # Sidebar panel for inputs ----
  sidebarPanel(
    width = 3,

    helpText("This app allows to visualize the 3D-UDs of different common 
             dentex individuals (", em("Dentex dentex"),") tracked in the Medes
             Islands marine protected area (NW Mediterranean Sea) between 
             Dec-12-2007 and May-31-2008."),
    br(),
    
    # Button to update plot ----
    actionButton("button", "Refresh plot", width = "100%"),
    br(),
    br(),
    
    # Individuals ----
    checkboxGroupInput("ind", h4("Individuals"),
                       choices = ind, inline = TRUE),

    # Contour levels ----
    selectInput("level", h4("UD contour"),
                choices = prob, selected = prob[2]),

    # Show topographic features ----
    checkboxGroupInput("topofeat",
                       h4("Topographic features"),
                       choices = list("Show topography" = "topo",
                                      "Show sea surface" = "surface"),
                       selected = "topo"),

    # Contour opacity ----
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
  scene.react <- reactive({ event_data("plotly_relayout", source = "link") })

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
                    z = c(0, 0, 0, 0), opacity = 0.3,
                    facecolor = rep("#9BD3F4", 3))
    }
    
    # Add individual UD contours
    for (i in input$ind) {
      
      indx <- which(ind.files == i & cont.files == input$level)
      load(files[indx])
      
      col.cont <- col[ind == i]
      name <- names(ind)[ind == i]
      
      p <- add_mesh(p, x = contour$contour[, 1], y = contour$contour[, 2],
                    z = -contour$contour[, 3],
                    i = contour$indx[, 1], j = contour$indx[, 2], 
                    k = contour$indx[, 3],
                    facecolor = rep(col.cont, nrow(contour$indx)),
                    opacity = input$opac, name = name, hoverinfo = "name",
                    flatshading = TRUE)
      
    }
    
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
