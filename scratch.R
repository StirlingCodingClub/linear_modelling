plant_data <- read.csv(file = "two_discrete_x_values.csv");

species_1 <- plant_data$height[plant_data$species_ID == "species_1"];
species_2 <- plant_data$height[plant_data$species_ID == "species_2"];
hist(species_1, breaks = 10, col = "blue", xlim = c(0, 300), 
     main = "", xlab = "Plant height", cex.lab = 1.25, 
     ylim = c(0, 15));
hist(species_2, breaks = 10, col = "red", add = TRUE)
#legend(x = 0, y = 15, fill = c("red", "blue"), 
#       legend = c("species_2", "species_1"));


n1     <- length(species_1);
n2     <- length(species_2);
var_y1 <- var(species_1);
var_y2 <- var(species_2);
s_p    <- sqrt(((n1 - 1)*var_y1 + (n2 - 1)*var_y2)/(n1 + n2 - 2));
SEybar <- s_p * sqrt((n1 + n2) / (n1*n2));
tval   <- (mean(species_1) - mean(species_2)) / SEybar;

#################
#################  Using the t-test function in R and lm
#################
t.test(species_1, species_2, var.equal = TRUE);

# Use a linear model instead to get the same numbers
lmod1 <- lm(height ~ 1 + species_ID, data = plant_data);
summary(lmod1);


#################
#################  How R sees the linear model and t-test
#################
is_species_2 <- as.numeric(plant_data$species_ID == "species_2");
plant_height <- plant_data$height;
lm_table_eg  <- data.frame(plant_height, is_species_2);


#################
#################  Thinking about this in a plot
#################
plot(x = lm_table_eg$is_species_2, y = lm_table_eg$plant_height, 
     ylim = c(0, 275), pch = 20, cex.axis = 1.25, cex.lab = 1.25, 
     ylab = "Plant height", xlab = "Is species 2: Yes (1) or no (0)", 
     xlim = c(0, 1));
lines(x = c(0, 1), y = c(summary(lmod1)$coefficients[1,1], 
                         summary(lmod1)$coefficients[1,1] + 
                           summary(lmod1)$coefficients[2,1]),
      col = "red", lwd = 4);
points(x = 0, y = summary(lmod1)$coefficients[1,1], pch = 17, 
       col = "blue", cex = 3);
points(x = 1, y = summary(lmod1)$coefficients[1,1] + 
         summary(lmod1)$coefficients[2,1], pch = 18, col = "orange", cex = 3);


#################
#################  Analysis of variance with two categories
#################
aov_1 <- aov(height ~ species_ID, data = plant_data);
summary(aov_1);


#################
#################  What happens when we have three categories? ANOVA
#################
plant_data <- read.csv(file = "three_discrete_x_values.csv");
aov_2 <- aov(height ~ species_ID, data = plant_data);
summary(aov_2);



#################
#################  Three categories with a linear model
#################
lmod2 <- lm(height ~ 1 + species_ID, data = plant_data);
summary(lmod2);

# Can we check out the mean values?
tapply(X = plant_data$height, INDEX = plant_data$species_ID, 
       FUN = mean);


#################
#################  What's really going on with 3 groups?
#################
the_intercept <- rep(x = 1, length = length(plant_data$height));
is_species_2  <- as.numeric(plant_data$species_ID == "species_2");
is_species_3  <- as.numeric(plant_data$species_ID == "species_3");
lm_table_eg   <- data.frame(plant_data$height, the_intercept, 
                            is_species_2, is_species_3);
head(lm_table_eg);







#################
#################  Even more elegant, using matrices!
#################
Y <- as.matrix(lm_table_eg[,1]);
X <- as.matrix(lm_table_eg[,2:4]);

# Now solve for Beta in:  Y = X * Beta
betas <- solve( t(X) %*% X ) %*% t(X) %*% Y;
print(betas);

# Compare back to this
summary(lmod2);












