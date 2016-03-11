library(shiny)
library(ggplot2)
library('RSQLite')
library(ggvis)

genesDbPath <- 'lollipop.db'
genesDbConn <- dbConnect(SQLite(), genesDbPath)
genesDbTable <- dbReadTable(genesDbConn, 'genes')
print(genesDbTable)

geneNames <- genesDbTable$name

twoHitSlider <- function(inputId, labelText, min = 0, max = 1000) {
  tagList(
    singleton(tags$head(tags$script(src = 'twoHitSlider.js'))),
    tags$div(style = 'margin:25px 0',
      tags$label(class='control-label', labelText),
      tags$input(id = inputId,
                 class = 'two-hit-slider',
                 'data-min' = min,
                 'data-max' = max)
    )
    
  )
}

hiddenDataOutput <- function(inputId, dataTarget, dataProp) {
  tagList(
    singleton(tags$head(tags$script(src = 'hiddenDataOutput.js'))),
    tags$div(id = inputId,
             class = 'hidden-input-output',
             'data-target' = dataTarget,
             'data-prop' = dataProp)
  )
}


fluidPage(
  includeCSS('www/bootstrap-slider/bootstrap-slider.min.css'),
  includeScript('www/bootstrap-slider/bootstrap-slider.min.js'),
  includeScript('www/lollipop.js'),
  includeCSS('www/lollipop.css'),
  
  titlePanel("Two-Hit Project"),
  
  sidebarPanel(
    selectInput('gene_name', 'Gene Name', geneNames),
    selectInput('cancer_type', 'Cancer Type', c('BRCA', 'PAAD', 'And', 'many', 'other', 'items', 'in', 'this', 'list')),
    checkboxInput('two_hit', 'Only Two-hits'),
    checkboxInput('clinvar', 'Only Clinvar'),
    
    checkboxGroupInput('variant_type', 'Variant Type', choices=c('SNV', 'Indel'), selected=c('SNV', 'Indel')),
    checkboxGroupInput('consequence', 'Consequence', 
                       choices=c('Missense', 'Stop Gain', 'Others'),
                       selected=c('Missense', 'Stop Gain', 'Others')),
    
    twoHitSlider('zoomslider', 'Plot Zoom (in bp)'),
    sliderInput('cadd_score', 'CADD Score', min=1, max=1400, value=50),
    sliderInput('minor_allele_freq', 'Minor Allele Frequency', min=0, max=1, value=0.5, step=c(0.001, 0.01, 0.1)),
    tags$button(tags$span(class='glyphicon glyphicon-save'), 'Download Data', class='btn bn-primary'),
    tags$button(tags$span(class='glyphicon glyphicon-picture'), 'Download Plot', class='btn btn-primary', 
                style='float:right', onclick='window.open($(\'#plot img\').attr(\'src\'))'),
    #htmlOutput('zoommin', container = putTextInHiddenInput, type='hidden'),
    hiddenDataOutput('zoom_max', '#zoomslider', 'max'),
    hiddenDataOutput('zoom_min', '#zoomslider', 'min')
    
  ),
  
  mainPanel(style='height:738px', 
#     plotOutput('plot', brush=brushOpts(id='plot_brush', resetOnNew = TRUE),
#                dblclick='plot_dblclick')
    ggvisOutput('plot')
  )
)