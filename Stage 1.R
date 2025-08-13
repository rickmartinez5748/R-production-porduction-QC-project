library(ggplot2)
library(Hmisc)

polymer_data<-read.csv(choose.files())


# Two-Way ANOVA
yield_anova<-aov(Yield_pct~Catalyst*Temperature,data=polymer_data)
summary(yield_anova)

# Tukey's HSD
TukeyHSD(yield_anova, conf.level = 0.95)

#Interaction Plot

interaction.plot(polymer_data$Catalyst, polymer_data$Temperature, polymer_data$Yield,
                 xlab = "Catalyst", ylab = "Yield (%)",
                 main = "Interaction Plot: Catalyst vs Temperature",
                 trace.label = "Temperature")


#Mean yield by Catalyst
ggplot(polymer_data, aes(Catalyst, Yield_pct)) +
  stat_summary(fun=mean, geom="bar", fill="skyblue") +
  stat_summary(fun.data=mean_cl_normal, geom="errorbar", width=0.2) +
  labs(x="Catalyst", y="Yield (%)", title="Effect of Catalyst on Yield")

#Mean yield by Temperature
ggplot(polymer_data, aes(Temperature, Yield_pct)) +
  stat_summary(fun=mean, geom="bar", fill="lightgreen") +
  stat_summary(fun.data=mean_cl_normal, geom="errorbar", width=0.2) +
  labs(x="Temperature (°C)", y="Yield (%)", title="Effect of Temperature on Yield")

#Grouped bar plot (Catalyst × Temperature)
ggplot(polymer_data, aes(Temperature, Yield_pct, fill=Catalyst)) +
  stat_summary(fun=mean, geom="bar", position="dodge") +
  stat_summary(fun.data=mean_cl_normal, geom="errorbar", position="dodge") +
  labs(x="Temperature (°C)", y="Yield (%)", title="Catalyst–Temperature Interaction on Yield")
