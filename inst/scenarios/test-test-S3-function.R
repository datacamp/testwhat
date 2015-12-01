scen <- list(
  list(
    type = "NormalExercise",
    pre_exercise_code = "
set.seed(1)
library(rpart)
titanic <- read.csv(url('http://s3.amazonaws.com/assets.datacamp.com/course/intro_to_ml/titanic.csv'))
titanic$Survived <- factor(titanic$Survived, levels=c('1','0'))
titanic$Pclass <- factor(titanic$Pclass)
shuffled <- titanic[sample(nrow(titanic)),]
train <- shuffled[1:round(0.7*nrow(shuffled)),]
test <- shuffled[(round(0.7*nrow(shuffled))+1):nrow(shuffled),]
tree <- rpart(Survived ~ ., train, method = 'class')
    ",
    student = "

predict(type = 'class', newdata = test, lm(c(1,2,3) ~ c(4,5,6)))

predict(object = tree, type = 'class', train)    

    ",
    solution = "
predict(object = tree, type = 'class', train)    

predict(type = 'class', newdata = test, tree)

    ",
    pass = list(
      test_simple_pass = list(
        long = "tests whether a simple S3 function can pass",
        sct = "

test_function('predict')

        "
      )  
    ),
    fail = list(
      test_simple_fail = list(
        long = "tests whether a simple S3 function can fail",
        sct = "",
        message = ""
      )
    )
  )
)