library(ggvis)
library(shiny)

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


ggvis(data = NULL) %>%
  #add_axis('y', values = 1:6) %>%
  #scale_logical('y', domain = c(ymin, ymax)) %>%
  layer_rects(x = ~V2, y = 0, x2 = ~V3, height := 12, data = reMappedExons) %>%
  layer_points(~ball_pos_x, ~ball_pos_y, size := 150, data = ball_df) %>%
  (function(pl) {
    add_stick <- function(pl, pos_x, pos_y, n){
      stick_df = data.frame(one=c(pos_x[n], pos_x[n]),
                          two=c(pos_y[n], 0))
      if(n == 1){
        pl %>% layer_paths(~one, ~two, data = stick_df)
      } else {
        pl %>% layer_paths(~one, ~two, data = stick_df) %>% add_stick(pos_x, pos_y, n - 1)
      }
    }
    pl %>% add_stick(ball_pos_x, ball_pos_y, length(ball_pos_x))
  })

















