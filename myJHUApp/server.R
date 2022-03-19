library(shiny)

# https://stackoverflow.com/questions/43123462/how-to-obtain-rmse-out-of-lm-result

df <- airquality
df$Temp2 <- df$Temp^2

#df <- airquality
#df$Temp2 <- df$Temp^2   

# Sort dataframe by Temp
df <- df[order(df$Temp), ]
# Drop Solar.R columns (has NA)
df <- subset(df, select = -c(2))
# Drop NA rows
df <- na.omit(df)
set.seed(1)


shinyServer(function(input, output) {
     
     # Get noise setting
     modelNoise <- reactive({
          input$sliderNoise    
     })     

     model1pred <- reactive({
          predict(model1, newdata = data.frame(Temp = 10))
     })     
     
     model2pred <- reactive({
          predict(model2, newdata = data.frame(Temp = 70, Temp2 = 70^2))
     })     
     
     
     output$plot1 <- renderPlot({

          sigma = modelNoise()
          df$Noise <- rnorm(nrow(df), mean = 0, sd=sigma) 
          df$OzNoise <- df$Ozone + df$Noise  
          
          model1 <- lm(OzNoise ~ Temp, data = df)
          model2 <- lm(OzNoise ~ Temp + Temp2, data = df)          
          
          plot(df$Temp, df$Ozone, 
               main="New York Air Quality Measurements",
               ylab="Ozone (ppb)",
               xlab = "Temperature (degrees F)",
               col="red", pch=3)
          points(df$Temp, df$OzNoise, col = "blue", pch=1, cex = 1)
          legend(x="topleft", c("Original Data" , "With Noise"),
                 pch = c(3, 1), 
                 col = c("red", "blue"), 
                 cex = 1.2, 
                 bty = "n" )              

          showModel1 <- FALSE
          showModel2 <- FALSE

          if(input$showModel1){
               predict1 <- predict(model1, newdata = data.frame(Temp = df$Temp))
               lines(df$Temp, predict1, col="black")
               showModel1 <- TRUE
          }
          
          if(input$showModel2){
               predict2 <- predict(model2, newdata = data.frame(Temp = df$Temp, Temp2=df$Temp2))
               lines(df$Temp, predict2, col="black")
               showModel2 <- TRUE
          }
          
          output$pred1 <- renderText({
               if (showModel1) {
                    paste("Ozone  =",
                          sprintf(model1$coefficients[1] , fmt = '%#.2f'),
                         "+",
                         sprintf(model1$coefficients[2] , fmt = '%#.2f'),
                         "* Temp")
               } else {
                    paste("N/A")
               }
          })
          
          # sprintf("%g", t[1,4])
          # sprintf("%.3f", pi)
          # r[,1:3] <- sprintf("%.3f", t[,1:3])
          
          if (showModel1) {
               model1_summary <- summary(model1)
               t1 <- model1_summary$coefficients[,1:4]
               t1[,4] <- sprintf(" %g", t1[,4])
               t1[,1:3] <- sprintf("%8.3f",model1_summary$coefficients[,1:3])
               output$tbl1 <- renderTable({ as.data.frame(t1) },  
                                          striped = TRUE)  
          } else {
               output$tbl1 <- renderTable({NULL})
          }
          output$pred2 <- renderText({
               if (showModel2) {               
                    paste("Ozone  =",
                         sprintf(model2$coefficients[1] , fmt = '%#.2f'),
                         "+",
                         sprintf(model2$coefficients[2] , fmt = '%#.2f'),
                         "* Temp +",
                         sprintf(model2$coefficients[3] , fmt = '%#.2f'), 
                         "* Temp^2")
               } else {
                    paste("N/A")                    
               }
          })
          if (showModel2) {
               model2_summary <- summary(model2)
               t2 <- model2_summary$coefficients[,1:4]
               t2[,4] <- sprintf(" %g", t2[,4])
               t2[,1:3] <- sprintf("%8.3f",model2_summary$coefficients[,1:3])
               output$tbl2 <- renderTable({ as.data.frame(t2) },  
                                          striped = TRUE,
                                          digits = 3)  
          } else {
               output$tbl2 <- renderTable({NULL})
          }               
          
     })
     

})
