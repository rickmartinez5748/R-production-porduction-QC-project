

morning<-c(10,70,5,15)
afternoon<-c(15,50,10,25)
night<-c(25,25,20,30)

defects_table<-rbind(morning, afternoon,night)
colnames(defects_table)<-c("Color Issue", "No Defect", "Strength Failure", "Surface Defect")

#Performing Chi Square Test
chi_result<-chisq.test(defects_table)
chi_result

#Standardized Residuals
chi_result$residuals

# Association Plot
assocplot(defects_table, xlab = "Production Shift", ylab = "Defect Type",
          main = "Association Plot: Defect Type by Production Shift")

# Mosaic Plot with shading
mosaicplot(defects_table, shade = TRUE,
           xlab = "Production Shift", ylab = "Defect Type",
           main = "Mosaic Plot: Defect Type by Production Shift")

# Grouped Bar Plot 
library(reshape2)

expected_table <- chi_result$expected

observed_df <- data.frame(melt(defects_table, value.name = "Count"),
                          Distribution = "Observed")
expected_df <- data.frame(melt(expected_table, value.name = "Count"),
                          Distribution = "Expected")

plot_df <- rbind(observed_df, expected_df)
colnames(plot_df) <- c("Shift", "DefectType", "Count", "Distribution")

ggplot(plot_df, aes(x = Distribution, y = Count, fill = Shift)) +
  geom_bar(stat = "identity", position = "stack") +
  facet_grid(~DefectType) +
  ggtitle("Observed vs Expected Counts for Defect Type by Shift")
