

#Round 1 - no reference

height = c(150,
           162,
           160,
           175,
           162,
           157,
           190)
shoes=c(15,
        20,
        25,
        32,
        39,
        23,
        25)
dep = c(rep("Biology", 2), rep("WEC", 3), rep("SNRE", 2))
gender = c(rep("Female", 7), rep("Male", 1))
repetition = c(rep("A",8))

hist(height, breaks = 10, main = "Histogram of Guess - A")

data1 <- data.frame(shoes, height, dep, gender, repetition)

mm1 <- lm(shoes~dep+height, data = data1)
summary(mm1)

#Round 2 - reference of the meter alone

height = c(155,
           162,
           155,
           170,
           170,
           155,
           210)
shoes=c(22,
        21,
        20,
        22,
        25,
        20,
        25)
repetition = c(rep("B",8))

hist(height, breaks = 10, main = "Histogram of Guess - B")

data2 <- data.frame(shoes, height, dep, gender, repetition)

mm2 <- lm(shoes~dep+height, data = data2)
summary(mm2)

#Round 2 - reference of the meter alone

height = c(152,
           160,
           158,
           170,
           170,
           153,
           191)
shoes=c(20,
        20,
        29,
        27,
        25,
        21,
        36)
repetition = c(rep("C",8))

hist(height, breaks = 10, main = "Histogram of Guess - C")

data3 <- data.frame(shoes, height, dep, gender, repetition)

mm3 <- lm(shoes~dep+height, data = data3)
summary(mm3)

#In a single dataset

data <- rbind(data1,data2,data3)
names(data)

model <- lm(shoes~dep+height, data = data)
summary(model)

ggplot(data, aes(x = height, y = shoes, colour = dep))+
  geom_point()+
  geom_smooth(method = lm, se = T)+
  labs(colour = "Department", y = "Your guess of shoes", 
       x = "Your guess of height", title = "Three rounds combined")+
  facet_wrap(~repetition)+
  theme_bw()
