# Load libraries----
library(ggplot2)
library(plotrix)
library(gridExtra)

# Read in Raw .csv Data ----

langloc_random_letters <- read.csv('/Users/bradyrobinson/Desktop/pwm_langloc_asl_vsl/langloc_random_letters.csv')
langloc_structured_letters <- read.csv('/Users/bradyrobinson/Desktop/pwm_langloc_asl_vsl/langloc_structured_letters.csv')
langloc_random_speech <- read.csv('/Users/bradyrobinson/Desktop/pwm_langloc_asl_vsl/langloc_random_speech.csv')
langloc_structured_speech <- read.csv('/Users/bradyrobinson/Desktop/pwm_langloc_asl_vsl/langloc_structured_speech.csv')
pwm_structured_speech <- read.csv('/Users/bradyrobinson/Desktop/pwm_langloc_asl_vsl/pwm_structured_speech.csv')
pwm_random_speech <- read.csv('/Users/bradyrobinson/Desktop/pwm_langloc_asl_vsl/pwm_random_speech.csv')
pwm_structured_letters <- read.csv('/Users/bradyrobinson/Desktop/pwm_langloc_asl_vsl/pwm_structured_letters.csv')
pwm_random_letters <- read.csv('/Users/bradyrobinson/Desktop/pwm_langloc_asl_vsl/pwm_random_letters.csv')

# Langloc Left Superior Anterior Temporal Gyrus ----
random_letters <- langloc_random_letters[,1]
structured_letters <- langloc_structured_letters[,1]
random_speech <- langloc_random_speech[,1]
structured_speech <- langloc_structured_speech[,1]

colnames(langloc_random_letters)[1]

sig_result1 <- t.test(random_letters, structured_letters, paired = FALSE)
significance1 <- sig_result1[3]
significance1

sig_result2 <- t.test(random_speech, structured_speech, paired = FALSE)
significance2 <- sig_result2[3]
significance2

mean1 <- mean(random_letters)
mean2 <- mean(structured_letters)
mean3 <- mean(random_speech)
mean4 <- mean(structured_speech)

mean_bars_df <- data.frame(name=c('random', 'structured', 'random', 'structured'),
                           value=c(mean1, mean2,mean3,mean4), 
                           group=c('VSL','VSL','ASL','ASL'))
mean_bars_df
ggplot(mean_bars_df, aes(x=group, y=value, fill=name)) + 
  geom_bar(position="dodge",color = 'black',stat='identity', width=.9) +
  theme_classic() +
  scale_fill_brewer(type = 'seq',palette = 'Purples') +
  theme(aspect.ratio = 2/1, legend.title = element_blank(),axis.ticks.x = element_blank(), axis.text.x = element_blank()) + 
  annotate("text", x=1:2, y=35:35, label=c('*',"ns"), size = 4) +
  xlab('ASL                 VSL') +
  ylab('Mean Activation (Beta)') +
  scale_y_continuous(n.breaks = c(10)) +
  ggtitle("Left Superior Anterior \n    Temporal Gyrus")

# Langloc Left Middle Temporal Gyrus----

random_letters <- langloc_random_letters[,2]
structured_letters <- langloc_structured_letters[,2]
random_speech <- langloc_random_speech[,2]
structured_speech <- langloc_structured_speech[,2]

colnames(langloc_random_letters)[2]

sig_result1 <- t.test(random_letters, structured_letters, paired = FALSE)
significance1 <- sig_result1[3]

sig_result2 <- t.test(random_speech, structured_speech, paired = FALSE)
significance2 <- sig_result2[3]

mean1 <- mean(random_letters)
mean2 <- mean(structured_letters)
mean3 <- mean(random_speech)
mean4 <- mean(structured_speech)

mean_bars_df <- data.frame(name=c('random', 'structured', 'random', 'structured'),
                           value=c(mean1, mean2,mean3,mean4), 
                           group=c('VSL','VSL','ASL','ASL'))
mean_bars_df
ggplot(mean_bars_df, aes(x=group, y=value, fill=name)) + 
  geom_bar(position="dodge",color = 'black',stat='identity', width=.9) +
  theme_classic() +
  scale_fill_brewer(type = 'seq',palette = 'Purples') +
  theme(aspect.ratio = 2/1, legend.title = element_blank(),axis.ticks.x = element_blank(), axis.text.x = element_blank()) + 
  annotate("text", x=1:2, y=5:5, label=c('ns',"ns"), size = 4) +
  xlab('ASL                 VSL') +
  ylab('Mean Activation (Beta)') +
  scale_y_continuous(n.breaks = c(10)) +
  ggtitle("Langloc Left Middle \n  Temporal Gyrus")







# Langloc Left Temporal Pole----

random_letters <- langloc_random_letters[,3]
structured_letters <- langloc_structured_letters[,3]
random_speech <- langloc_random_speech[,3]
structured_speech <- langloc_structured_speech[,3]

colnames(langloc_random_letters)[3]

sig_result1 <- t.test(random_letters, structured_letters, paired = FALSE)
significance1 <- sig_result1[3]
significance1

sig_result2 <- t.test(random_speech, structured_speech, paired = FALSE)
significance2 <- sig_result2[3]
significance2

mean1 <- mean(random_letters)
mean2 <- mean(structured_letters)
mean3 <- mean(random_speech)
mean4 <- mean(structured_speech)

mean_bars_df <- data.frame(name=c('random', 'structured', 'random', 'structured'),
                           value=c(mean1, mean2,mean3,mean4), 
                           group=c('VSL','VSL','ASL','ASL'))
mean_bars_df
ggplot(mean_bars_df, aes(x=group, y=value, fill=name)) + 
  geom_bar(position="dodge",color = 'black',stat='identity', width=.9) +
  theme_classic() +
  scale_fill_brewer(type = 'seq',palette = 'Purples') +
  theme(aspect.ratio = 2/1, legend.title = element_blank(),axis.ticks.x = element_blank(), axis.text.x = element_blank()) + 
  annotate("text", x=1:2, y=5:5, label=c('ns',"ns"), size = 4) +
  xlab('ASL                 VSL') +
  ylab('Mean Activation (Beta)') +
  scale_y_continuous(n.breaks = c(10)) +
  ggtitle("Left Temoral Pole")
# Langloc Left Superior Posterior Temporal Gyrus----

random_letters <- langloc_random_letters[,4]
structured_letters <- langloc_structured_letters[,4]
random_speech <- langloc_random_speech[,4]
structured_speech <- langloc_structured_speech[,4]

colnames(langloc_random_letters)[4]

sig_result1 <- t.test(random_letters, structured_letters, paired = FALSE)
significance1 <- sig_result1[3]
significance1

sig_result2 <- t.test(random_speech, structured_speech, paired = FALSE)
significance2 <- sig_result2[3]
significance2

mean1 <- mean(random_letters)
mean2 <- mean(structured_letters)
mean3 <- mean(random_speech)
mean4 <- mean(structured_speech)

mean_bars_df <- data.frame(name=c('random', 'structured', 'random', 'structured'),
                           value=c(mean1, mean2,mean3,mean4), 
                           group=c('VSL','VSL','ASL','ASL'))
mean_bars_df
ggplot(mean_bars_df, aes(x=group, y=value, fill=name)) + 
  geom_bar(position="dodge",color = 'black',stat='identity', width=.9) +
  theme_classic() +
  scale_fill_brewer(type = 'seq',palette = 'Purples') +
  theme(aspect.ratio = 2/1, legend.title = element_blank(),axis.ticks.x = element_blank(), axis.text.x = element_blank()) + 
  annotate("text", x=1:2, y=70:70, label=c('*',"ns"), size = 4) +
  xlab('ASL                 VSL') +
  ylab('Mean Activation (Beta)') +
  scale_y_continuous(n.breaks = c(10)) +
  ggtitle("Left Superior Posterior \n    Temporal Gyrus")

# Langloc Right Superior Posterior Temporal Gyrus----

random_letters <- langloc_random_letters[,5]
structured_letters <- langloc_structured_letters[,5]
random_speech <- langloc_random_speech[,5]
structured_speech <- langloc_structured_speech[,5]

colnames(langloc_random_letters)[5]

sig_result1 <- t.test(random_letters, structured_letters, paired = FALSE)
significance1 <- sig_result1[3]
significance1

sig_result2 <- t.test(random_speech, structured_speech, paired = FALSE)
significance2 <- sig_result2[3]
significance2

mean1 <- mean(random_letters)
mean2 <- mean(structured_letters)
mean3 <- mean(random_speech)
mean4 <- mean(structured_speech)

mean_bars_df <- data.frame(name=c('random', 'structured', 'random', 'structured'),
                           value=c(mean1, mean2,mean3,mean4), 
                           group=c('VSL','VSL','ASL','ASL'))
mean_bars_df
ggplot(mean_bars_df, aes(x=group, y=value, fill=name)) + 
  geom_bar(position="dodge",color = 'black',stat='identity', width=.9) +
  theme_classic() +
  scale_fill_brewer(type = 'seq',palette = 'Purples') +
  theme(aspect.ratio = 2/1, legend.title = element_blank(),axis.ticks.x = element_blank(), axis.text.x = element_blank()) + 
  annotate("text", x=1:2, y=70:70, label=c('*',"ns"), size = 4) +
  xlab('ASL                 VSL') +
  ylab('Mean Activation (Beta)') +
  scale_y_continuous(n.breaks = c(10)) +
  ggtitle("Right Superior Posterior \n   Temporal Gyrus")

# Langloc Right Superior Anterior Temporal Gyrus----

random_letters <- langloc_random_letters[,6]
structured_letters <- langloc_structured_letters[,6]
random_speech <- langloc_random_speech[,6]
structured_speech <- langloc_structured_speech[,6]

colnames(langloc_random_letters)[6]

sig_result1 <- t.test(random_letters, structured_letters, paired = FALSE)
significance1 <- sig_result1[3]
significance1

sig_result2 <- t.test(random_speech, structured_speech, paired = FALSE)
significance2 <- sig_result2[3]
significance2

mean1 <- mean(random_letters)
mean2 <- mean(structured_letters)
mean3 <- mean(random_speech)
mean4 <- mean(structured_speech)

mean_bars_df <- data.frame(name=c('random', 'structured', 'random', 'structured'),
                           value=c(mean1, mean2,mean3,mean4), 
                           group=c('VSL','VSL','ASL','ASL'))
mean_bars_df
ggplot(mean_bars_df, aes(x=group, y=value, fill=name)) + 
  geom_bar(position="dodge",color = 'black',stat='identity', width=.9) +
  theme_classic() +
  scale_fill_brewer(type = 'seq',palette = 'Purples') +
  theme(aspect.ratio = 2/1, legend.title = element_blank(),axis.ticks.x = element_blank(), axis.text.x = element_blank()) + 
  annotate("text", x=1:2, y=70:70, label=c('*',"ns"), size = 4) +
  xlab('ASL                 VSL') +
  ylab('Mean Activation (Beta)') +
  scale_y_continuous(n.breaks = c(10)) +
  ggtitle("Right Superior Anterior \n   Temporal Gyrus")

# Langloc Right Middle Temporal Gyrus----

random_letters <- langloc_random_letters[,7]
structured_letters <- langloc_structured_letters[,7]
random_speech <- langloc_random_speech[,7]
structured_speech <- langloc_structured_speech[,7]

colnames(langloc_random_letters)[7]

sig_result1 <- t.test(random_letters, structured_letters, paired = FALSE)
significance1 <- sig_result1[3]
significance1

sig_result2 <- t.test(random_speech, structured_speech, paired = FALSE)
significance2 <- sig_result2[3]
significance2

mean1 <- mean(random_letters)
mean2 <- mean(structured_letters)
mean3 <- mean(random_speech)
mean4 <- mean(structured_speech)

mean_bars_df <- data.frame(name=c('random', 'structured', 'random', 'structured'),
                           value=c(mean1, mean2,mean3,mean4), 
                           group=c('VSL','VSL','ASL','ASL'))
mean_bars_df
ggplot(mean_bars_df, aes(x=group, y=value, fill=name)) + 
  geom_bar(position="dodge",color = 'black',stat='identity', width=.9) +
  theme_classic() +
  scale_fill_brewer(type = 'seq',palette = 'Purples') +
  theme(aspect.ratio = 2/1, legend.title = element_blank(),axis.ticks.x = element_blank(), axis.text.x = element_blank()) + 
  annotate("text", x=1:2, y=5:5, label=c('ns',"ns"), size = 4) +
  xlab('ASL                 VSL') +
  ylab('Mean Activation (Beta)') +
  scale_y_continuous(n.breaks = c(10)) +
  ggtitle("Right Middle Temporal Gyrus")




# Langloc Right Superior Temporal Gyrus----
random_letters <- langloc_random_letters[,8]
structured_letters <- langloc_structured_letters[,8]
random_speech <- langloc_random_speech[,8]
structured_speech <- langloc_structured_speech[,8]

colnames(langloc_random_letters)[8]

sig_result1 <- t.test(random_letters, structured_letters, paired = FALSE)
significance1 <- sig_result1[3]
significance1

sig_result2 <- t.test(random_speech, structured_speech, paired = FALSE)
significance2 <- sig_result2[3]
significance2

mean1 <- mean(random_letters)
mean2 <- mean(structured_letters)
mean3 <- mean(random_speech)
mean4 <- mean(structured_speech)

mean_bars_df <- data.frame(name=c('random', 'structured', 'random', 'structured'),
                           value=c(mean1, mean2,mean3,mean4), 
                           group=c('VSL','VSL','ASL','ASL'))
mean_bars_df
ggplot(mean_bars_df, aes(x=group, y=value, fill=name)) + 
  geom_bar(position="dodge",color = 'black',stat='identity', width=.9) +
  theme_classic() +
  scale_fill_brewer(type = 'seq',palette = 'Purples') +
  theme(aspect.ratio = 2/1, legend.title = element_blank(),axis.ticks.x = element_blank(), axis.text.x = element_blank()) + 
  annotate("text", x=1:2, y=10:10, label=c('ns',"ns"), size = 4) +
  xlab('ASL                 VSL') +
  ylab('Mean Activation (Beta)') +
  scale_y_continuous(n.breaks = c(10)) +
  ggtitle("Right Superior Temporal Gyrus")




# Langloc Right Temporal Pole----
random_letters <- langloc_random_letters[,9]
structured_letters <- langloc_structured_letters[,9]
random_speech <- langloc_random_speech[,9]
structured_speech <- langloc_structured_speech[,9]

colnames(langloc_random_letters)[9]

sig_result1 <- t.test(random_letters, structured_letters, paired = FALSE)
significance1 <- sig_result1[3]
significance1

sig_result2 <- t.test(random_speech, structured_speech, paired = FALSE)
significance2 <- sig_result2[3]
significance2

mean1 <- mean(random_letters)
mean2 <- mean(structured_letters)
mean3 <- mean(random_speech)
mean4 <- mean(structured_speech)

mean_bars_df <- data.frame(name=c('random', 'structured', 'random', 'structured'),
                           value=c(mean1, mean2,mean3,mean4), 
                           group=c('VSL','VSL','ASL','ASL'))
mean_bars_df
ggplot(mean_bars_df, aes(x=group, y=value, fill=name)) + 
  geom_bar(position="dodge",color = 'black',stat='identity', width=.9) +
  theme_classic() +
  scale_fill_brewer(type = 'seq',palette = 'Purples') +
  theme(aspect.ratio = 2/1, legend.title = element_blank(),axis.ticks.x = element_blank(), axis.text.x = element_blank()) + 
  annotate("text", x=1:2, y=5:5, label=c('ns',"ns"), size = 4) +
  xlab('ASL                 VSL') +
  ylab('Mean Activation (Beta)') +
  scale_y_continuous(n.breaks = c(10)) +
  ggtitle("Right Temporal Pole")



# Langloc Left Precentral Gyrus----
random_letters <- langloc_random_letters[,10]
structured_letters <- langloc_structured_letters[,10]
random_speech <- langloc_random_speech[,10]
structured_speech <- langloc_structured_speech[,10]

colnames(langloc_random_letters)[10]

sig_result1 <- t.test(random_letters, structured_letters, paired = FALSE)
significance1 <- sig_result1[3]
significance1

sig_result2 <- t.test(random_speech, structured_speech, paired = FALSE)
significance2 <- sig_result2[3]
significance2

mean1 <- mean(random_letters)
mean2 <- mean(structured_letters)
mean3 <- mean(random_speech)
mean4 <- mean(structured_speech)

mean_bars_df <- data.frame(name=c('random', 'structured', 'random', 'structured'),
                           value=c(mean1, mean2,mean3,mean4), 
                           group=c('VSL','VSL','ASL','ASL'))
mean_bars_df
ggplot(mean_bars_df, aes(x=group, y=value, fill=name)) + 
  geom_bar(position="dodge",color = 'black',stat='identity', width=.9) +
  theme_classic() +
  scale_fill_brewer(type = 'seq',palette = 'Purples') +
  theme(aspect.ratio = 2/1, legend.title = element_blank(),axis.ticks.x = element_blank(), axis.text.x = element_blank()) + 
  annotate("text", x=1:2, y=30:30, label=c('ns',"ns"), size = 4) +
  xlab('ASL                 VSL') +
  ylab('Mean Activation (Beta)') +
  scale_y_continuous(n.breaks = c(10)) +
  ggtitle("Left Precentral Gyrus")

# Langloc Left Inferior Frontal Gyrus----
random_letters <- langloc_random_letters[,11]
structured_letters <- langloc_structured_letters[,11]
random_speech <- langloc_random_speech[,11]
structured_speech <- langloc_structured_speech[,11]

colnames(langloc_random_letters)[11]

sig_result1 <- t.test(random_letters, structured_letters, paired = FALSE)
significance1 <- sig_result1[3]
significance1

sig_result2 <- t.test(random_speech, structured_speech, paired = FALSE)
significance2 <- sig_result2[3]
significance2

mean1 <- mean(random_letters)
mean2 <- mean(structured_letters)
mean3 <- mean(random_speech)
mean4 <- mean(structured_speech)

mean_bars_df <- data.frame(name=c('random', 'structured', 'random', 'structured'),
                           value=c(mean1, mean2,mean3,mean4), 
                           group=c('VSL','VSL','ASL','ASL'))
mean_bars_df
ggplot(mean_bars_df, aes(x=group, y=value, fill=name)) + 
  geom_bar(position="dodge",color = 'black',stat='identity', width=.9) +
  theme_classic() +
  scale_fill_brewer(type = 'seq',palette = 'Purples') +
  theme(aspect.ratio = 2/1, legend.title = element_blank(),axis.ticks.x = element_blank(), axis.text.x = element_blank()) + 
  annotate("text", x=1:2, y=10:10, label=c('*',"ns"), size = 4) +
  xlab('ASL                 VSL') +
  ylab('Mean Activation (Beta)') +
  scale_y_continuous(n.breaks = c(10)) +
  ggtitle("Left Inferior Frontal Gyrus")

# PWM Left Superior Anterior Temporal Gyrus----
random_letters <- pwm_random_letters[,1]
structured_letters <- pwm_structured_letters[,1]
random_speech <- pwm_random_speech[,1]
structured_speech <- pwm_structured_speech[,1]

colnames(langloc_random_letters)[1]

sig_result1 <- t.test(random_letters, structured_letters, paired = FALSE)
significance1 <- sig_result1[3]
significance1

sig_result2 <- t.test(random_speech, structured_speech, paired = FALSE)
significance2 <- sig_result2[3]
significance2

mean1 <- mean(random_letters)
mean2 <- mean(structured_letters)
mean3 <- mean(random_speech)
mean4 <- mean(structured_speech)

mean_bars_df <- data.frame(name=c('random', 'structured', 'random', 'structured'),
                           value=c(mean1, mean2,mean3,mean4), 
                           group=c('VSL','VSL','ASL','ASL'))
mean_bars_df
ggplot(mean_bars_df, aes(x=group, y=value, fill=name)) + 
  geom_bar(position="dodge",color = 'black',stat='identity', width=.9) +
  theme_classic() +
  scale_fill_brewer(type = 'seq',palette = 'Purples') +
  theme(aspect.ratio = 2/1, legend.title = element_blank(),axis.ticks.x = element_blank(), axis.text.x = element_blank()) + 
  annotate("text", x=1:2, y=60:60, label=c('*',"ns"), size = 4) +
  xlab('ASL                 VSL') +
  ylab('Mean Activation (Beta)') +
  scale_y_continuous(n.breaks = c(10)) +
  ggtitle("Left Superior Anterior \n    Temporal Gyrus")

# PMW Left Middle Temporal Gyrus----
random_letters <- pwm_random_letters[,2]
structured_letters <- pwm_structured_letters[,2]
random_speech <- pwm_random_speech[,2]
structured_speech <- pwm_structured_speech[,2]

colnames(langloc_random_letters)[2]

sig_result1 <- t.test(random_letters, structured_letters, paired = FALSE)
significance1 <- sig_result1[3]
significance1

sig_result2 <- t.test(random_speech, structured_speech, paired = FALSE)
significance2 <- sig_result2[3]
significance2

mean1 <- mean(random_letters)
mean2 <- mean(structured_letters)
mean3 <- mean(random_speech)
mean4 <- mean(structured_speech)

mean_bars_df <- data.frame(name=c('random', 'structured', 'random', 'structured'),
                           value=c(mean1, mean2,mean3,mean4), 
                           group=c('VSL','VSL','ASL','ASL'))
mean_bars_df
ggplot(mean_bars_df, aes(x=group, y=value, fill=name)) + 
  geom_bar(position="dodge",color = 'black',stat='identity', width=.9) +
  theme_classic() +
  scale_fill_brewer(type = 'seq',palette = 'Purples') +
  theme(aspect.ratio = 2/1, legend.title = element_blank(),axis.ticks.x = element_blank(), axis.text.x = element_blank()) + 
  annotate("text", x=1:2, y=60:60, label=c('*',"ns"), size = 4) +
  xlab('ASL                 VSL') +
  ylab('Mean Activation (Beta)') +
  scale_y_continuous(n.breaks = c(10)) +
  ggtitle("Left Middle Temporal Gyrus")
# PWM Left Temporal Pole----

random_letters <- pwm_random_letters[,3]
structured_letters <- pwm_structured_letters[,3]
random_speech <- pwm_random_speech[,3]
structured_speech <- pwm_structured_speech[,3]

colnames(langloc_random_letters)[3]

sig_result1 <- t.test(random_letters, structured_letters, paired = FALSE)
significance1 <- sig_result1[3]
significance1

sig_result2 <- t.test(random_speech, structured_speech, paired = FALSE)
significance2 <- sig_result2[3]
significance2

mean1 <- mean(random_letters)
mean2 <- mean(structured_letters)
mean3 <- mean(random_speech)
mean4 <- mean(structured_speech)

mean_bars_df <- data.frame(name=c('random', 'structured', 'random', 'structured'),
                           value=c(mean1, mean2,mean3,mean4), 
                           group=c('VSL','VSL','ASL','ASL'))
mean_bars_df
ggplot(mean_bars_df, aes(x=group, y=value, fill=name)) + 
  geom_bar(position="dodge",color = 'black',stat='identity', width=.9) +
  theme_classic() +
  scale_fill_brewer(type = 'seq',palette = 'Purples') +
  theme(aspect.ratio = 2/1, legend.title = element_blank(),axis.ticks.x = element_blank(), axis.text.x = element_blank()) + 
  annotate("text", x=1:2, y=25:25, label=c('ns',"ns"), size = 4) +
  xlab('ASL                 VSL') +
  ylab('Mean Activation (Beta)') +
  scale_y_continuous(n.breaks = c(10)) +
  ggtitle("Left Temporal Pole")

# PWM Left Superior Posterior Temporal Gyrus----

random_letters <- pwm_random_letters[,4]
structured_letters <- pwm_structured_letters[,4]
random_speech <- pwm_random_speech[,4]
structured_speech <- pwm_structured_speech[,4]

colnames(langloc_random_letters)[4]

sig_result1 <- t.test(random_letters, structured_letters, paired = FALSE)
significance1 <- sig_result1[3]
significance1

sig_result2 <- t.test(random_speech, structured_speech, paired = FALSE)
significance2 <- sig_result2[3]
significance2

mean1 <- mean(random_letters)
mean2 <- mean(structured_letters)
mean3 <- mean(random_speech)
mean4 <- mean(structured_speech)

mean_bars_df <- data.frame(name=c('random', 'structured', 'random', 'structured'),
                           value=c(mean1, mean2,mean3,mean4), 
                           group=c('VSL','VSL','ASL','ASL'))
mean_bars_df
ggplot(mean_bars_df, aes(x=group, y=value, fill=name)) + 
  geom_bar(position="dodge",color = 'black',stat='identity', width=.9) +
  theme_classic() +
  scale_fill_brewer(type = 'seq',palette = 'Purples') +
  theme(aspect.ratio = 2/1, legend.title = element_blank(),axis.ticks.x = element_blank(), axis.text.x = element_blank()) + 
  annotate("text", x=1:2, y=45:45, label=c('*',"ns"), size = 4) +
  xlab('ASL                 VSL') +
  ylab('Mean Activation (Beta)') +
  scale_y_continuous(n.breaks = c(10)) +
  ggtitle("Left Superior Posterior \n    Temporal Gyrus")

# PWM Right Superior Posterior Temporal Gyrus----

random_letters <- pwm_random_letters[,5]
structured_letters <- pwm_structured_letters[,5]
random_speech <- pwm_random_speech[,5]
structured_speech <- pwm_structured_speech[,5]

colnames(langloc_random_letters)[5]

sig_result1 <- t.test(random_letters, structured_letters, paired = FALSE)
significance1 <- sig_result1[3]
significance1

sig_result2 <- t.test(random_speech, structured_speech, paired = FALSE)
significance2 <- sig_result2[3]
significance2

mean1 <- mean(random_letters)
mean2 <- mean(structured_letters)
mean3 <- mean(random_speech)
mean4 <- mean(structured_speech)

mean_bars_df <- data.frame(name=c('random', 'structured', 'random', 'structured'),
                           value=c(mean1, mean2,mean3,mean4), 
                           group=c('VSL','VSL','ASL','ASL'))
mean_bars_df
ggplot(mean_bars_df, aes(x=group, y=value, fill=name)) + 
  geom_bar(position="dodge",color = 'black',stat='identity', width=.9) +
  theme_classic() +
  scale_fill_brewer(type = 'seq',palette = 'Purples') +
  theme(aspect.ratio = 2/1, legend.title = element_blank(),axis.ticks.x = element_blank(), axis.text.x = element_blank()) + 
  annotate("text", x=1:2, y=10:10, label=c('ns',"ns"), size = 4) +
  xlab('ASL                 VSL') +
  ylab('Mean Activation (Beta)') +
  scale_y_continuous(n.breaks = c(10)) +
  ggtitle("Right Superior Posterior \n      Temporal Gyrus")



# Compiled Visualization

# Langloc Left Inferior Frontal Gyrus----

random_speech <- langloc_random_speech[,11]
structured_speech <- langloc_structured_speech[,11]

colnames(langloc_random_letters)[11]

sig_result1 <- t.test(random_speech, structured_speech, paired = FALSE)
significance1 <- sig_result1[3]
significance1

mean1 <- mean(random_speech)
mean2 <- mean(structured_speech)
stderr1 <- std.error(random_speech)
stderr2 <- std.error(structured_speech)
ymin1 <- (mean1 - (stderr1))
ymax1 <- (mean1 + (stderr1))
ymin2 <- (mean2 - (stderr2))
ymax2 <- (mean2 + (stderr2))

# Langloc Left Superior Anterior Temporal Gyrus ----

random_speech <- langloc_random_speech[,1]
structured_speech <- langloc_structured_speech[,1]

colnames(langloc_random_letters)[1]

sig_result2 <- t.test(random_speech, structured_speech, paired = FALSE)
significance2 <- sig_result2[3]
significance2

mean3 <- mean(random_speech)
mean4 <- mean(structured_speech)
stderr3 <- std.error(random_speech)
stderr4 <- std.error(structured_speech)
ymin3 <- (mean3 - (stderr3))
ymax3 <- (mean3 + (stderr3))
ymin4 <- (mean4 - (stderr4))
ymax4 <- (mean4 + (stderr4))


# Langloc Left Superior Posterior Temporal Gyrus----

random_speech <- langloc_random_speech[,4]
structured_speech <- langloc_structured_speech[,4]

colnames(langloc_random_letters)[4]

sig_result3 <- t.test(random_speech, structured_speech, paired = FALSE)
significance3 <- sig_result3[3]
significance3

mean5 <- mean(random_speech)
mean6 <- mean(structured_speech)
stderr5 <- std.error(random_speech)
stderr6 <- std.error(structured_speech)
ymin5 <- (mean5 - (stderr5))
ymax5 <- (mean5 + (stderr5))
ymin6 <- (mean6 - (stderr6))
ymax6 <- (mean6 + (stderr6))


# Langloc Right Superior Posterior Temporal Gyrus----

random_speech <- langloc_random_speech[,5]
structured_speech <- langloc_structured_speech[,5]

colnames(langloc_random_letters)[5]

sig_result4 <- t.test(random_speech, structured_speech, paired = FALSE)
significance4 <- sig_result4[3]
significance4

mean7 <- mean(random_speech)
mean8 <- mean(structured_speech)
stderr7 <- std.error(random_speech)
stderr8 <- std.error(structured_speech)
ymin7 <- (mean7 - (stderr7))
ymax7 <- (mean7 + (stderr7))
ymin8 <- (mean8 - (stderr8))
ymax8 <- (mean8 + (stderr8))

# Langloc Right Superior Anterior Temporal Gyrus----

random_speech <- langloc_random_speech[,6]
structured_speech <- langloc_structured_speech[,6]

colnames(langloc_random_letters)[6]

sig_result5 <- t.test(random_speech, structured_speech, paired = FALSE)
significance5 <- sig_result5[3]
significance5

mean9 <- mean(random_speech)
mean10 <- mean(structured_speech)
stderr9 <- std.error(random_speech)
stderr10 <- std.error(structured_speech)
ymin9 <- (mean9 - (stderr9))
ymax9 <- (mean9 + (stderr9))
ymin10 <- (mean10 - (stderr10))
ymax10 <- (mean10 + (stderr10))

# Langloc Visualization----

mean_bars_df <- data.frame(name=c('Random', 'Structured', 'Random', 'Structured', 'Random', 'Structured', 'Random', 'Structured', 'Random', 'Structured'),
                           value=c(mean1, mean2,mean3,mean4, mean5, mean6, mean9, mean10, mean7, mean8), 
                           group=c('  LIFG','  LIFG','LSTGA','LSTGA','LSTGP','LSTGP','RSTGA','RSTGA','RSTGP','RSTGP'),
                           lower=c(ymin1,ymin2,ymin3,ymin4,ymin5,ymin6,ymin9,ymin10,ymin7,ymin8),
                           upper=c(ymax1,ymax2,ymax3,ymax4,ymax5,ymax6,ymax9,ymax10,ymax7,ymax8))

mean_bars_df
plot1 <- ggplot(mean_bars_df, aes(x=group, y=value, fill=name)) + 
  geom_bar(position=position_dodge(),color = 'black',stat='identity', width=.7) +
  geom_errorbar(aes(ymin=lower, ymax=upper), width = 0.2, position = position_dodge(.735)) +
  theme_classic() +
  scale_fill_brewer(type = 'seq',palette = 'Purples') +
  ylab('Mean Activation (Beta)') +
  ggtitle("               Language Localizer Parcels") +
  theme(aspect.ratio = 2/2.1, legend.title = element_blank(),axis.ticks.x = element_blank(), 
        axis.text.x = element_text(color="black", size=12),
        axis.text.y = element_text(color="black", size=12), 
        axis.title.y=element_text(size=12), 
        plot.title = element_text(size=14)) + 
  annotate("text", x=1:2:3:4:5, y=67:67:67:67:67, label=c('p = .041',"p = .002", 'p < .001','p = .016','p = .004'), size = 4) +
  xlab(NULL) +
  scale_y_continuous(n.breaks = c(10))
  


# PWM Left Superior Anterior Temporal Gyrus----
random_speech <- pwm_random_speech[,1]
structured_speech <- pwm_structured_speech[,1]

colnames(langloc_random_letters)[1]

sig_result1 <- t.test(random_speech, structured_speech, paired = FALSE)
significance1 <- sig_result1[3]
significance1

mean1 <- mean(random_speech)
mean2 <- mean(structured_speech)
stderr1 <- std.error(random_speech)
stderr2 <- std.error(structured_speech)
ymin1 <- (mean1 - (stderr1))
ymax1 <- (mean1 + (stderr1))
ymin2 <- (mean2 - (stderr2))
ymax2 <- (mean2 + (stderr2))



# PMW Left Middle Temporal Gyrus----
random_speech <- pwm_random_speech[,2]
structured_speech <- pwm_structured_speech[,2]

colnames(langloc_random_letters)[2]

sig_result2 <- t.test(random_speech, structured_speech, paired = FALSE)
significance2 <- sig_result2[3]
significance2

mean3 <- mean(random_speech)
mean4 <- mean(structured_speech)
stderr3 <- std.error(random_speech)
stderr4 <- std.error(structured_speech)
ymin3 <- (mean3 - (stderr3))
ymax3 <- (mean3 + (stderr3))
ymin4 <- (mean4 - (stderr4))
ymax4 <- (mean4 + (stderr4))

# PWM Left Superior Posterior Temporal Gyrus----

random_speech <- pwm_random_speech[,4]
structured_speech <- pwm_structured_speech[,4]

colnames(langloc_random_letters)[4]

sig_result3 <- t.test(random_speech, structured_speech, paired = FALSE)
significance3 <- sig_result3[3]
significance3

mean5 <- mean(random_speech)
mean6 <- mean(structured_speech)
stderr5 <- std.error(random_speech)
stderr6 <- std.error(structured_speech)
ymin5 <- (mean5 - (stderr5))
ymax5 <- (mean5 + (stderr5))
ymin6 <- (mean6 - (stderr6))
ymax6 <- (mean6 + (stderr6))

# Compiled PWM----
mean_bars_df <- data.frame(name=c('Random', 'Structured', 'Random', 'Structured', 'Random', 'Structured'),
                           value=c(mean1, mean2,mean3,mean4,mean5,mean6), 
                           group=c('LSTGA','LSTGA','LMTG','LMTG','LSTGP','LSTGP'),
                           lower=c(ymin1, ymin2, ymin3, ymin4, ymin5, ymin6),
                           upper=c(ymax1, ymax2, ymax3, ymax4, ymax5, ymax6))
mean_bars_df
plot2 <- ggplot(mean_bars_df, aes(x=group, y=value, fill=name)) + 
  geom_bar(position=position_dodge(),color = 'black',stat='identity', width=.7) +
  geom_errorbar(aes(ymin=lower, ymax=upper), width = 0.2, position = position_dodge(.735)) +
  theme_classic() +
  scale_fill_brewer(type = 'seq',palette = 'Oranges') +
  ylab('Mean Activation (Beta)') +
  ggtitle("       Phonological Working Memory Parcels") +
  theme(aspect.ratio = 2/2.1, legend.title = element_blank(),axis.ticks.x = element_blank(), 
        axis.text.x = element_text(color="black", size=12),
        axis.text.y = element_text(color="black", size=12), 
        axis.title.y=element_text(size=12), 
        plot.title = element_text(size=14)) + 
  annotate("text", x=1:2:3, y=53:53:53, label=c("p = .002",'p < .001', 'p = .014'), size = 4) +
  xlab(NULL) +
  scale_y_continuous(n.breaks = c(7.5))

#Combine plots----

grid.arrange(plot1, plot2, ncol=2)
  
