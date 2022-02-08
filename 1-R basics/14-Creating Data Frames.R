
grades<- data.frame(names=c("John", "Juan","Jean","Yao"),
                    exame_1=c(95,80,90,85),
                    exame_2=c(90,85,85,90))
grades
class(grades$names)

grades<- data.frame(names=c("John", "Juan","Jean","Yao"),
                    exame_1=c(95,80,90,85),
                    exame_2=c(90,85,85,90), stringsAsFactors = TRUE)
grades
class(grades$names)

grades<- data.frame(names=c("John", "Juan","Jean","Yao"),
                    exame_1=c(95,80,90,85),
                    exame_2=c(90,85,85,90), stringsAsFactors = FALSE)
grades
class(grades$names)
