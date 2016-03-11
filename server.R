library(shiny)
library(ggplot2)
library('plyr')

function(input, output) {
  
      reMapExons <- function(exons, pos, buffer){
        
        reMappedExons <- exons
        # Assume 1-based for... need to make input file is 1-based
        for(i in 1:nrow(exons)){
          exon <- exons[i,]
          len <- exon[3] - exon[2] + 1 # make sure + 1 is necessary
          reMappedExons[i,2] <- pos
          reMappedExons[i,3] <- pos + len
          pos <- pos + len + buffer
        }
        return(reMappedExons)
      }
  
  reMapVariants <- function(exons, remappedExons, variants){
    # Need to handle condition when variant is neve in coords
    #!# Check thoroughly for off by one errors
    
    reMappedVariants <- variants
    for(i in 1:nrow(variants)){
      pos <- variants[i, 5]
      
      # exons and remapped exons should have same nrow
      for(j in 1:nrow(exons)){
        x1 <- exons[j, 2]
        x2 <- exons[j, 3]
        
        if(inCoords(pos, x1, x2)){
          newPos <- remappedExons[j, 2] + (pos - x1) # map to the remapped exon
          reMappedVariants[i, 5] <- newPos
          break 
        }
        # Should do something here if a variant isn't remapped
      }
    }
    
    return(reMappedVariants)
  }
  
  inCoords <- function(pos, x1, x2){
    
    return(pos >= x1 && pos <= x2)
  }
  
  
  
  gene = 'PIP4K2B'
  allExons <- read.table('/home/dfitzgerald/workspace/RProjects/shiny_demo/lollipop/gene_models/CCDS.20110907.exons.txt', sep = '\t', header = F)
  allVariants <- read.table('/home/dfitzgerald/workspace/RProjects/shiny_demo/lollipop/variants/PIP4K2B_two-hit_variants.txt', sep = '\t', header = F)
  exons <- subset(allExons, V4 == gene)
  reMappedExons <- reMapExons(exons, 0, 25)
  variants <- subset(allVariants, V1 == gene)
  reMappedVariants <- reMapVariants(exons, reMappedExons, variants)
  
  #######################################
  # define plot dimensions & initialize
  #######################################
  # Will get first position of first exon and last position of last exon
  xmin <- 0
  xmax <- reMappedExons[nrow(reMappedExons), 3]
  # perhaps should always be 0
  ymin <- -0.25
  ymax <- -log10(min(reMappedVariants[, 8])) * 1.15 # 15% higher than max MAF value 
  yend = ymin - ((ymax - ymin) * .20) # 20% of y range below ymin
  
  ball_pos_x = numeric(nrow(reMappedVariants))
  ball_pos_y = numeric(nrow(reMappedVariants))
  for (i in 1:nrow(reMappedVariants)) {
    pos <- reMappedVariants[i, 5]
    MAF <- reMappedVariants[i, 8]
    # Height will need to be values in our cohort
    # WIll break with NA if we use ExAC consistently
    #p <- addStick(p, pos, ymin, -log10(MAF))
    #p <- addBall(p, pos, ymin, -log10(MAF))
    ball_pos_x[i] <- pos
    ball_pos_y[i] <- ymin + -log10(MAF)
  }
  ball_df <- data.frame(ball_pos_x, ball_pos_y)

  output$zoom_min <- renderText({'0'})
  output$zoom_max <- renderText({'1400'})
  
  vis <- reactive({
    zoom_range <- as.numeric(unlist(strsplit(input$zoomslider, ',')))
    ggvis(data = NULL) %>%
    #add_axis('y', values = 1:6) %>%
    scale_numeric('x', domain = zoom_range, clamp = TRUE) %>%
    set_options(width = '100%', height = '100%') %>%
    layer_rects(x = ~V2, y = 0, x2 = ~V3, height := 12, data = reMappedExons) %>%
    layer_points(~ball_pos_x, ~ball_pos_y, size := 150, data = ball_df) %>%
    (function(plot) {
      add_stick <- function(plot, pos_x, pos_y, n){
        stick_df = data.frame(pt_x=c(pos_x[n], pos_x[n]),
                              pt_y=c(pos_y[n], 0))
        if(n == 1){
          plot %>% layer_paths(~pt_x, ~pt_y, data = stick_df)
        } else if(pos_x[n] < zoom_range[1] || pos_x[n] > zoom_range[2]) {
          plot %>% add_stick(pos_x, pos_y, n - 1)
        } else {
          plot %>% layer_paths(~pt_x, ~pt_y, data = stick_df) %>% add_stick(pos_x, pos_y, n - 1)
        }
      }
      plot %>% add_stick(ball_pos_x, ball_pos_y, length(ball_pos_x))
    })
  })
  vis %>% bind_shiny('plot')
  
  # ggvis
  #########################################################################################################
  # ggplot2
#   
#   ranges <- reactiveValues(x = NULL, y = NULL)
#   
#   output$plot <- renderPlot({
#     
#     # Perhaps could make a geom_segment call for each exon present
#     # The model exon model itself needs to serve as the baseplot, not the variants
#     # i.e. any gene we should accept should have a gene model, but not necessarily variants
#     
#     addStick <- function(p, xpos, ymin, height){
#       # sticks could either be fixed height or have variable scaling
#       # However, numbers would need to be positive
#       # would have to think about how to handle MAF since it is a decimal
#       
#       p <- p + geom_segment(aes_string(x = xpos, y = ymin, xend = xpos, yend = ymin + height), colour="blue", size = 1, linetype="solid", lineend = 'round')
#       
#       return(p)
#       
#     }
#     
#     addBall <- function(p, xpos, ymin, height){
#       # Color based on the mutation type
#       # Maybe handle that outside of this function
#       # Size could also be variable
#       DF <- data.frame()
#       p <- p + geom_point(data=DF, aes_string(xpos, ymin + height), colour="blue",size=10)
#       return(p)
#     }
#     
#     addExon <- function(p, exonStart, exonStop, ymin, ymax){
#       # Height could be a fixed percentage of ymax - ymin
#       
#       #yend = ymin - ((ymax - ymin) * .20) # 20% of y range below ymin
#       p <- p + geom_segment(aes_string(x = exonStart, y = ymin, xend = exonStop, yend = ymin), size = 40, linetype="solid", lineend = 'butt')
#       return(p)
#     }
#     
#     connectExons <- function(p, exonStart, exonStop, ymin){
#       # Height could be a fixed percentage of ymax - ymin
#       p <- p + geom_segment(aes_string(x = exonStart, y = ymin, xend = exonStop, yend = ymin), size = 10, linetype="solid", color = 'red')
#       return(p)
#     }
#     
#     # may need to calculate line breaks early and pass them in here
#     # be sure that all of these values have a buffer to them
#     initializePlot <- function(xmin, xmax, ymin, ymax, gene){
#       DF <- data.frame()
#       p <- ggplot(DF) + geom_point() + xlim(xmin, xmax) + ylim(ymin, ymax) +
#         theme(legend.position = "none", 
#               axis.text.x = element_text(size = 22), 
#               axis.title.x = element_text(size = 22), 
#               axis.title.y = element_text(size = 22), 
#               axis.text.y = element_text(size = 22),
#               plot.title = element_text(lineheight=1.5, face="bold", size = 32, vjust = 1.5), 
#               legend.text = element_text(size = 22, face = "bold"), 
#               legend.title = element_text(size = 22, face = "bold")) +
#         xlab("\nRelative nucleotide position") +
#         ylab("-log10( Allele frequency )\n") +
#         ggtitle(gene) + 
#         coord_cartesian(xlim = ranges$x)
#       return(p)
#     }
#     
#     reMapExons <- function(exons, pos, buffer){
#       reMappedExons <- exons
#       # Assume 1-based for... need to make input file is 1-based
#       for(i in 1:nrow(exons)){
#         exon <- exons[i,]
#         len <- exon[3] - exon[2] + 1 # make sure + 1 is necessary
#         reMappedExons[i,2] <- pos
#         reMappedExons[i,3] <- pos + len
#         
#         
#         pos <- pos + len + buffer
#       }
#       return(reMappedExons)
#     }
#     
#     reMapVariants <- function(exons, remappedExons, variants){
#       # Need to handle condition when variant is neve in coords
#       #!# Check thoroughly for off by one errors
#       
#       reMappedVariants <- variants
#       for(i in 1:nrow(variants)){
#         pos <- variants[i, 5]
#         
#         # exons and remapped exons should have same nrow
#         for(j in 1:nrow(exons)){
#           x1 <- exons[j, 2]
#           x2 <- exons[j, 3]
#           
#           if(inCoords(pos, x1, x2)){
#             newPos <- remappedExons[j, 2] + (pos - x1) # map to the remapped exon
#             reMappedVariants[i, 5] <- newPos
#             break 
#           }
#           # Should do something here if a variant isn't remapped
#         }
#       }
#       
#       return(reMappedVariants)
#     }
#     
#     inCoords <- function(pos, x1, x2){
#       return(pos >= x1 && pos <= x2)
#     }
#     
#     
#     initializeLegend <- function(){
#       1 + 1
#     }
#     
#     ###################################
#     ## MAIN
#     ###################################
#     # Read in all exons from a file
#     # Then can subset by the gene
#     # Still need to iterate through this file and find all unique exons
#     gene = 'PIP4K2B'
#     allExons <- read.table('/home/dfitzgerald/workspace/RProjects/shiny_demo/lollipop/gene_models/CCDS.20110907.exons.txt', sep = '\t', header = F)
#     allVariants <- read.table('/home/dfitzgerald/workspace/RProjects/shiny_demo/lollipop/variants/PIP4K2B_two-hit_variants.txt', sep = '\t', header = F)
#     #exons <- subset(allExons, V4 == gene)
#     exons <- subset(allExons, V4 == input$gene_name)
#     variants <- subset(allVariants, 
#                        V1 == gene)
#     
#     # Generate a list to serve as a map for variant types
#     # We need to keep this fixed for our purposes - consistency across plots
#     # vartype mapping to a hex color 
#     #color.map <- list('' = '')
#     
#     # Re-map exons and variants
#     pos <- 0
#     buffer <- 25
#     reMappedExons <- reMapExons(exons, pos, buffer)
#     reMappedVariants <- reMapVariants(exons, reMappedExons, variants)
#     
#     
#     #######################################
#     # define plot dimensions & initialize
#     #######################################
#     # Will get first position of first exon and last position of last exon
#     xmin <- 0
#     xmax <- reMappedExons[nrow(reMappedExons), 3]
#     print(xmin)
#     print(xmax)
#     # perhaps should always be 0
#     ymin <- -0.25
#     ymax <- -log10(min(reMappedVariants[, 8])) * 1.15 # 15% higher than max MAF value 
#     yend = ymin - ((ymax - ymin) * .20) # 20% of y range below ymin
#     
#     
#     # Could limit plot to certain # of nucleotide positions/exons
#     # in shiny one could shuffle through the different plots
#     # p <- initializePlot(xmin, xmax, ymin, ymax, gene)
#     p <- initializePlot(xmin, xmax, ymin, ymax, input$gene_name)
#     
#     # Plotting order.... exon connectors, sticks, exons, pops
#     # for loop... for exon in exons
#     # Exon connectors plotted first
#     # Lollipop sticks plotted second
#     #p <- connectExons(p, exon1[2], exon2[1], 0)
#     
#     #p <- addStick(p, 150, ymin, 2)
#     #p <- addBall(p, 150, ymin, 2)
#     #p<- addStick(p, 175, ymin, 1)
#     #p <- addBall(p, 175, ymin, 1)
#     #p <- addStick(p, 350, ymin, 2)
#     #p <- addBall(p, 350, ymin, 2)
#     
#     ###################################
#     # Plot variants
#     ###################################
#     for(i in 1:nrow(reMappedVariants)){
#       pos <- reMappedVariants[i, 5]
#       MAF <- reMappedVariants[i, 8]
#       # Height will need to be values in our cohort
#       # WIll break with NA if we use ExAC consistently
#       p <- addStick(p, pos, ymin, -log10(MAF))
#       p <- addBall(p, pos, ymin, -log10(MAF))
#     }
#     
#     ###################################
#     # Plot exons
#     ###################################
#     # Can just connect all exons using xmin and xmax
#     p <- connectExons(p, xmin, xmax, 0)
#     for(i in 1:nrow(reMappedExons)){
#       exon <- reMappedExons[i,]
#       p <- addExon(p, exon[2], exon[3], 0, ymax)
#     }
#     
#     print(p)
#     
#     
#   }, height=700)
#   
#   observeEvent(input$plot_dblclick, {
#     brush <- input$plot_brush
#     if(!is.null(brush)){
#       ranges$x <- c(brush$xmin*1500, brush$xmax*1500)
#       print(ranges$x)
#     }else{
#       ranges$x <- NULL
#     }
#   })
  
}