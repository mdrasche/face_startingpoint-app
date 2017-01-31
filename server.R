library(shiny)
#library(imager)
library(jpeg)
library(tidyr)
library(dplyr)


#load('pca_centered1180')
#rot2 <- pca_alt$rotation
#rot2 <- rot2[,1:300]
load('rot_outliersRem')
rot <- as.matrix(rot)
#meanface <- readJPEG('./images/meanface.jpg')
#mface <- meanface %>% as.vector() #meanface as a vector
load('mface_outliersRem')

comp1 <- rot[,1]
comp2 <- rot[,2]
comp3 <- rot[,3]
comp4 <- rot[,4]
comp5 <- rot[,5]
comp6 <- rot[,6]
comp7 <- rot[,7]
comp8 <- rot[,8]
comp9 <- rot[,9]
comp10 <- rot[,10]
comp11 <- rot[,11]
comp12 <- rot[,12]
comp13 <- rot[,13]
comp14 <- rot[,14]
comp15 <- rot[,15]
comp16 <- rot[,16]

xdim <- 179 #image width
ydim <- 251 # image height
rgbdim <- 3 

#Set range of slider
load('eigenvals_min010917')
eigenvals_min <- floor(eigenvals_min)
load('eigenvals_max010917')
eigenvals_max <- ceiling(eigenvals_max)
#stval <- 2.772881 # mean of ratings

#For Skin tone slider:
#load('regress_weights1180')
#load('eigenvals_1180')
#load('skintone_intercept')

#For Gender button
#load('gender_weights')
#gender_intercept <- gender_coef[1,1]
#gender_coef <- fit_coef[2:301,] %>% as.data.frame() %>%
 # tibble::rownames_to_column('PC')  %>%
  #select(Estimate)

#For Skintone/gender/trustworthiness
load('pc_regweights_st_gender_trust_SCALED_1-31-2017')
starting_point_ev <- function(gender, st_rate, trust_rate, rot, dat_allcoefs, threshold) {
  eigenvals_calc <- c()
  for (i in 1:300) {
    df <- dat_allcoefs %>% filter(pc==i)
    intercept <- df %>% filter(predictor == 'intercept')
    intercept <- intercept$Estimate
    gender_weight <- df %>% filter(predictor == 'gender')
    if(gender_weight$p < threshold){
      gender_weight <- gender_weight$Estimate * gender
    }
    else{
      gender_weight <- 0
    }
    st_weight <- df %>% filter(predictor == 'skintone')
    if(st_weight$p < threshold){
      st_weight <- st_weight$Estimate * st_rate
    }
    else{
      st_weight <- 0 
    }
    trust_weight <- df %>% filter(predictor == 'trustworthiness')
    if(trust_weight$p < threshold){
      trust_weight <- trust_weight$Estimate * trust_rate
    }
    else{
      trust_weight <- 0 
    }
    ev <- intercept + st_weight + gender_weight + trust_weight
    eigenvals_calc <- c(eigenvals_calc, ev)
  }
  
  eigenvals_calc <- as.matrix(eigenvals_calc, ncol = 1)
}

shinyServer(function(input, output, session) {
  
  observe({
    input$reset_input
    #updateSliderInput(session, "skintone", value = skintone_intercept)
    updateSliderInput(session, "pc1", value = 0)
    updateSliderInput(session, "pc2", value = 0)
    updateSliderInput(session, "pc3", value = 0)
    updateSliderInput(session, "pc4", value = 0)
    updateSliderInput(session, "pc5", value = 0)
    updateSliderInput(session, "pc6", value = 0)
    updateSliderInput(session, "pc7", value = 0)
    updateSliderInput(session, "pc8", value = 0)
    updateSliderInput(session, "pc10", value = 0)
    updateSliderInput(session, "pc11", value = 0)
    updateSliderInput(session, "pc12", value = 0)
    updateSliderInput(session, "pc13", value = 0)
    updateSliderInput(session, "pc14", value = 0)
    updateSliderInput(session, "pc15", value = 0)
    updateSliderInput(session, "pc16", value = 0)
  })
  

  
  eigen <- reactive({
    gender <- input$gender %>% as.numeric()
    starting_point_ev(gender, input$skintone, input$trust, rot, dat_allcoefs, as.numeric(input$weights))
  })
  
  reconstruction <- reactive({
    recon <- rot %*% eigen() + mface
    #Put in jpg format    251 X 179 X 3
    recon_3d <- array(recon, dim=c(ydim,xdim,rgbdim)) 
  })
  
  output$reconFace <- renderImage({
    # A temp file to save the output.
    # This file will be removed later by renderImage
    outfile <- tempfile(fileext='.jpg')
    
    writeJPEG(reconstruction(), target = outfile, quality =1)
    
    # Return a list containing the filename and alt text
    list(src = outfile)
  }, deleteFile = TRUE)
  
  regressionweight <- reactive({
    if (input$weights == 1){
      dat_coef$Estimate
    }
    else{
      coef <- mutate(dat_coef, est = ifelse(p < input$weights, Estimate, 0))
      coef$est
    }
    
  })
  
  ###User Input
  
  ##PC Sliders
  output$pc1slider <- renderUI({
    sliderInput("pc1", "PC1", 
                min=eigenvals_min[1], max=eigenvals_max[1], value=eigen()[1,1])
  })
  output$pc2slider <- renderUI({
    sliderInput("pc2", "PC2", 
                min=eigenvals_min[2], max=eigenvals_max[2], value=eigen()[2,1])
  })
  output$pc3slider <- renderUI({
    sliderInput("pc3", "PC3", 
                min=eigenvals_min[3], max=eigenvals_max[3], value=eigen()[3,1])
  })
  output$pc4slider <- renderUI({
    sliderInput("pc4", "PC4", 
                min=eigenvals_min[4], max=eigenvals_max[4], value=eigen()[4,1])
  })
  output$pc5slider <- renderUI({
    sliderInput("pc5", "PC5", 
                min=eigenvals_min[5], max=eigenvals_max[5], value=eigen()[5,1])
  })
  output$pc6slider <- renderUI({
    sliderInput("pc6", "PC6", 
                min=eigenvals_min[6], max=eigenvals_max[6], value=eigen()[6,1])
  })
  output$pc7slider <- renderUI({
    sliderInput("pc7", "PC7", 
                min=eigenvals_min[7], max=eigenvals_max[7], value=eigen()[7,1])
  })
  output$pc8slider <- renderUI({
    sliderInput("pc8", "PC8", 
                min=eigenvals_min[8], max=eigenvals_max[8], value=eigen()[8,1])
  })
  output$pc9slider <- renderUI({
    sliderInput("pc9", "PC9", 
                min=eigenvals_min[9], max=eigenvals_max[9], value=eigen()[9,1])
  })
  output$pc10slider <- renderUI({
    sliderInput("pc10", "PC10", 
                min=eigenvals_min[10], max=eigenvals_max[10], value=eigen()[10,1])
  })
  output$pc11slider <- renderUI({
    sliderInput("pc11", "PC11", 
                min=eigenvals_min[11], max=eigenvals_max[11], value=eigen()[11,1])
  })
  output$pc12slider <- renderUI({
    sliderInput("pc12", "PC12", 
                min=eigenvals_min[12], max=eigenvals_max[12], value=eigen()[12,1])
  })
  output$pc13slider <- renderUI({
    sliderInput("pc13", "PC13", 
                min=eigenvals_min[13], max=eigenvals_max[13], value=eigen()[13,1])
  })
  output$pc14slider <- renderUI({
    sliderInput("pc14", "PC14", 
                min=eigenvals_min[14], max=eigenvals_max[14], value=eigen()[14,1])
  })
  output$pc15slider <- renderUI({
    sliderInput("pc15", "PC15", 
                min=eigenvals_min[15], max=eigenvals_max[15], value=eigen()[15,1])
  })
  output$pc16slider <- renderUI({
    sliderInput("pc16", "PC16", 
                min=eigenvals_min[16], max=eigenvals_max[16], value=eigen()[16,1])
  })
  
  
  
  output$myImage <- renderImage({
    # A temp file to save the output.
    # This file will be removed later by renderImage
    outfile <- tempfile(fileext='.jpg')
    
    #Reconstruct image w/ first 5 pc set to 0 in order to not double count
    eigen_meaned <- eigen()
    eigen_meaned[4:8,1] <- 0
    eigen_meaned[10:16,1] <- 0
    recon <- rot %*% eigen_meaned + mface
    reconstruction <- array(recon, dim=c(ydim,xdim,rgbdim)) 
    
    #Principal Components
    comp1_3d <- array(comp1, dim=c(251,179,3))
    comp2_3d <- array(comp2, dim=c(251,179,3))
    comp3_3d <- array(comp3, dim=c(251,179,3))
    comp4_3d <- array(comp4, dim=c(251,179,3))
    comp5_3d <- array(comp5, dim=c(251,179,3))
    comp6_3d <- array(comp6, dim=c(251,179,3))
    comp7_3d <- array(comp7, dim=c(251,179,3))
    comp8_3d <- array(comp8, dim=c(251,179,3))
    comp10_3d <- array(comp10, dim=c(251,179,3))
    comp11_3d <- array(comp11, dim=c(251,179,3))
    comp12_3d <- array(comp12, dim=c(251,179,3))
    comp13_3d <- array(comp13, dim=c(251,179,3))
    comp14_3d <- array(comp14, dim=c(251,179,3))
    comp15_3d <- array(comp15, dim=c(251,179,3))
    comp16_3d <- array(comp16, dim=c(251,179,3))
    
    #Combine components
    #TODO cap the values?? Explore the blue/greens coming up??
    generated_face <- comp1_3d * input$pc1 + comp2_3d * input$pc2 + comp3_3d * input$pc3 +
                      comp4_3d * input$pc4 +  comp5_3d * input$pc5 + comp6_3d * input$pc6 +
                      comp7_3d * input$pc7 +  comp8_3d * input$pc8 + comp10_3d * input$pc10 +  reconstruction +
                      comp11_3d * input$pc11 +  comp12_3d * input$pc12 + comp13_3d * input$pc13 #+
                      #comp14_3d * input$pc14 +  comp15_3d * input$pc15 + comp16_3d * input$pc26 +
                     
    
    writeJPEG(generated_face,target = outfile, quality =1) #Write to temporary file
    
    # Return a list containing the filename
    list(src = outfile,
         contentType = 'image/jpg')
  }, deleteFile = TRUE)
  
  
})