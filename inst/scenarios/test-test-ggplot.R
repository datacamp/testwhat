library(RAutomatedTesting)

scen <- list(
  list(
    type = "NormalExercise",
    pre_exercise_code = "library(ggplot2)",
    student = '
# base layers
cyl.am <- ggplot(mtcars, aes(x = factor(cyl), fill = factor(am)))

# Add geom
cyl.am + 
  geom_bar()

# Stack - default
cyl.am + 
  geom_bar(position = "stack")

# Fill - show proportion
cyl.am + 
  geom_bar(position = "fill")  

# Dodging - principles of similarity and proximity
cyl.am +
  geom_bar(position = "dodge") 

# Clean up the axes with Scale functions:
cyl.am +
  geom_bar(position = "dodge") +
  scale_fill_manual("Transmission", 
                    values = c("#E41A1C", "#377EB8"),
                    labels = c("Manual", "Automatic")) +
  scale_y_continuous("Number") +
  scale_x_discrete("Cylinders")
        ',
    solution = '
# base layers
    cyl.am <- ggplot(mtcars, aes(x = factor(cyl), fill = factor(am)))
    
    # Add geom
    cyl.am + 
    geom_bar()
    
    # Stack - default
    cyl.am + 
    geom_bar(position = "stack")
    
    # Fill - show proportion
    cyl.am + 
    geom_bar(position = "fill")  
    
    # Dodging - principles of similarity and proximity
    cyl.am +
    geom_bar(position = "dodge") 
    
    # Clean up the axes with Scale functions:
    cyl.am +
    geom_bar(position = "dodge") +
    scale_fill_manual("Transmission", 
    values = c("#E41A1C", "#377EB8"),
    labels = c("Manual", "Automatic")) +
    scale_y_continuous("Number") +
    scale_x_discrete("Cylinders")
    ',
    pass = list(
      test_correct_data = list(
        long = "test succeeds if command has the correct data",
        sct = '
test_error()

test_ggplot(1)
test_ggplot(2, check = "geom")
test_ggplot(3, check = "geom")
test_ggplot(4, check = "geom")
test_ggplot(5, check = "geom")
test_ggplot(6, check = c("geom", "scale"))
success_msg("Great")
        '
      )
    )
  )
)

test_all_scenarios(scen)