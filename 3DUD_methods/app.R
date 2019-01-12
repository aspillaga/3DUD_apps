################################################################################
##
## Title: Appendix S3. Shiny app to compare the 3D-UD contours of common dentex 
##        individuals obtained applying different methods.
## Author: Eneko Aspillaga (University of Barcelona)
## E-mail: eneko.aspillaga@gmail.com
## Date: December 2018
## Link: https://aspillaga.shinyapps.io/3DUD_methods/
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
method.files <- unlist(strsplit(files, "_"))[1:4 == 2]

# List of individuals and colors to plot the 3D-UDs
ind <- unique(ind.files)
names(ind) <- paste0("Dentex #", substr(ind, start = 7, stop = 9))

col1 <- c("#C22618", "#F08300", "#FFDD06", "#CECE00", "#15892E",
          "#6ABFAA", "#00AEEA", "#005D99", "#D94E96", "#664882",
          "#DFB2A2", "#936037", "#4C4C47")
col2 <- c("#E3817D", "#F7B66F", "#FFE97B", "#E8E697", "#9ACCA3",
          "#A8D7CB", "#96D4EC", "#7FB2C9", "#E5ACCD", "#B39FCC",
          "#F5E4DF", "#BFADA1", "#A8A7A7")
col3 <- c("#9B6161", "#AF8255", "#B2A054", "#B2B077", "#779E7D",
          "#7DA097", "#6B96A4", "#567884", "#9B7B8F", "#6E667F",
          "#C1B5B2", "#93857D", "#717070")
colors <- list(topo = col1, notopo = col2, kde = col3)

labels <- list(topo = "3D-UD - Topography", notopo = "3D-UD - Null Topography",
               kde = "3D Kernel")

# List of UD probability contours
prob <- unique(cont.files)
names(prob) <- paste0(prob, "%")


# Define UI for the app --------------------------------------------------------

ui <- fluidPage(

  # Align radio buttons
  tags$head(
    tags$style(
      HTML(
        ".radio-inline {
        margin-left: 0px;
        margin-right: 10px;
        }
        .radio-inline+.radio-inline {
        margin-left: 0px;
        margin-right: 10px;
        }"
      ))),

  # App title ----
  p("Aspillaga", em ("et al."), "2018.", 
    em("Methods in Ecology and Evolution."), "Appendix S3.",
    style = "font-size:10pt; color: #B6B6B4; line-heigh:20px; 
    margin-bottom:-10pt"),
  headerPanel("Comparison between methods to obtain 3D-UD contours"),

  # Sidebar panel for inputs ----
  sidebarPanel(
    width = 3,

    helpText("This app allows to compare the 3D-UDs of common dentex 
             individuals obtained by applying the new method incorporating
             the topography, by applying the method but using a null topography,
             and by applying a previous method based only on 3D kernel density
             estimations."),
    br(),
    
    # Button to update plot ----
    actionButton("button", "Refresh plot", width = "100%"),
    br(),
    br(),
    
    # Method used to compute 3D-UDs
    checkboxGroupInput("method",
                       h4("Method used to compute 3D-UDs"),
                       choices = list("3D-UD including topography" = "topo",
                                      "3D-UD using a null topography" = "notopo",
                                      "3D kernel estimation" = "kde"),
                       selected = "topo"),
    
    # Individuals
    radioButtons("ind", h4("Individuals"),
                 choices = ind, inline = TRUE),

    # Contour levels
    selectInput("level", h4("UD contour"),
                choices = prob, selected = prob[2]),

    # Show topographic features
    checkboxGroupInput("topofeat",
                       h4("Topographic features"),
                       choices = list("Show topography" = "topo",
                                      "Show sea surface" = "surface"),
                       selected = "topo"),

    # Opacity
    sliderInput("opac", h4("UD contour opacity"),
                min = 0, max = 1, value = 0.6),

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


# Define server logic to plot 3D UD --------------------------------------------

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

  # Create the basic plot with and without topography
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
    
    # Load and add 3D UDs
    for (m in input$method) {
      
      i <- input$ind
    
      indx <- which(ind.files == i & method.files == m & cont.files == input$level)
      load(files[indx])
      
      col.cont <- colors[[m]][ind == i]
      name <- paste0(names(ind)[ind == i], "\n", labels[[m]])
      
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
