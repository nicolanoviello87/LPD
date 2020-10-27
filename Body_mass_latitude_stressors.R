rm(list = ls())

#read in Living Planet and Body Mass data files
lpd <- read_xlsx("LPI_Data/LPI.xlsx")
bs <- read_xlsx("Body_Mass_means.xlsx")

###################################################################################
#format data for model compatibility
###################################################################################

#keep only those lpi entries with threat data
lpi <- lpd[ which(lpd$Threat_status=="Threatened"),]
lpi2 <- lpd[ which(lpd$Threat_status== "No threats"),]
lpi <- rbind(lpi, lpi2)

#replace NULL values with NA
lpi = as.matrix(lpi)
lpi[lpi=="NULL"] <- NA
lpi = as.data.frame(lpi)

#change names of Classes to be more friendly
lpi$Class <- as.character(lpi$Class)
lpi$Class[lpi$Class == 'Actinopterygii'] <- "Bony Fish"
lpi$Class[lpi$Class == 'Sarcopterygii'] <- "Bony Fish"
lpi$Class[lpi$Class == 'Osteichthyes'] <- "Bony Fish"
lpi$Class[lpi$Class == 'Myxini'] <- "Bony Fish"
lpi$Class[lpi$Class == 'Cephalaspidomorphi'] <- "Bony Fish"
lpi$Class[lpi$Class == 'Holocephali'] <- "Cartilaginous Fish"
lpi$Class[lpi$Class == 'Elasmobranchii'] <- "Cartilaginous Fish"
lpi$Class[lpi$Class == 'Aves'] <- "Birds"
lpi$Class[lpi$Class == 'Reptilia'] <- "Reptiles"
lpi$Class[lpi$Class == 'Amphibia'] <- "Amphibians"
lpi$Class[lpi$Class == 'Mammalia'] <- "Mammals"

#count stressors
lpi$threat_number <- apply(lpi[145:147], 1, function(x) sum(!is.na(x)))

#add 1 to threat number to ensure model convergence (will be removed again 
#prior to interpretation)
lpi$nplus1 = lpi$threat_number+1

#merge LPD and body mass data
all <- merge(bs, lpi, by="Binomial")

#ln body size data
all <- all[all$lognat_bs >= 0.00, ]
all <- all %>% distinct(ID, .keep_all = TRUE)

#convert latitiude to numeric
all$Latitude = as.character(all$Latitude)
all$Latitude = as.numeric(all$Latitude)

###################################################################################
#Subset groups by system and class
###################################################################################
marine <- all[ which(all$System=="Marine"),]
fresh <- all[ which(all$System=="Freshwater"),]
terr <- all[ which(all$System=="Terrestrial"),]

amph <- all[ which(all$Class=="Amphibians"),]
terr.amph <- terr[which(terr$Class=="Amphibians"),]
fresh.amph <- fresh[which(fresh$Class=="Amphibians"),]
terr.rept <- terr[which(terr$Class=="Reptiles"),]
marine.rept <- marine[which(marine$Class=="Reptiles"),]
fresh.rept <- fresh[which(fresh$Class=="Reptiles"),]
terr.mamm <- terr[which(terr$Class=="Mammals"),]
marine.mamm <- marine[which(marine$Class=="Mammals"),]
fresh.mamm <- fresh[which(fresh$Class=="Mammals"),]
terr.bird <- terr[which(terr$Class=="Birds"),]
marine.bird <- marine[which(marine$Class=="Birds"),]
fresh.bird <- fresh[which(fresh$Class=="Birds"),]
marine.carti <- marine[which(marine$Class=="Cartilaginous Fish"),]
fresh.carti <- fresh[which(fresh$Class=="Cartilaginous Fish"),]
marine.bony <- marine[which(marine$Class=="Bony Fish"),]
fresh.bony <- fresh[which(fresh$Class=="Bony Fish"),]

###################################################################################
#generate global models for use in dredge
###################################################################################
ta = glmmTMB(nplus1 ~ (lognat_bs + ns(Latitude, df = 4))^3 + 
               (1|Genus/Binomial), data = terr.amph, 
             na.action = "na.fail", family = "truncated_compois")

fa = glmmTMB(nplus1 ~ (lognat_bs + ns(Latitude, df = 4))^3 + 
               (1|Genus/Binomial), data = fresh.amph, 
             na.action = "na.fail", family = "truncated_compois")

tr = glmmTMB(nplus1 ~ (lognat_bs + ns(Latitude, df = 4))^3 + 
               (1|Genus/Binomial), data = terr.rept, 
             na.action = "na.fail", family = "truncated_compois")

mr = glmmTMB(nplus1 ~ (lognat_bs + ns(Latitude, df = 4))^3 + 
               (1|Genus/Binomial), data = marine.rept, 
             na.action = "na.fail", family = "truncated_compois")

fr = glmmTMB(nplus1 ~ (lognat_bs + ns(Latitude, df = 4))^3 + 
               (1|Genus/Binomial), data = fresh.rept, 
             na.action = "na.fail", family = "truncated_compois")

tm = glmmTMB(nplus1 ~ (lognat_bs + ns(Latitude, df = 4))^3 + 
               (1|Genus/Binomial), data = terr.mamm, 
             na.action = "na.fail", family = "truncated_compois")

mm = glmmTMB(nplus1 ~ (lognat_bs + ns(Latitude, df = 4))^3 + 
               (1|Genus/Binomial), data = marine.mamm, 
             na.action = "na.fail", family = "truncated_compois")

fm = glmmTMB(nplus1 ~ (lognat_bs + ns(Latitude, df = 4))^3 + 
               (1|Genus/Binomial), data = fresh.mamm, 
             na.action = "na.fail", family = "truncated_compois")

tbi = glmmTMB(nplus1 ~ (lognat_bs + ns(Latitude, df = 4))^3 + 
                (1|Genus/Binomial), data = terr.bird, 
              na.action = "na.fail", family = "truncated_compois")

mbi = glmmTMB(nplus1 ~ (lognat_bs + ns(Latitude, df = 4))^3 + 
                (1|Genus/Binomial), data = marine.bird, 
              na.action = "na.fail", family = "truncated_compois")

fbi = glmmTMB(nplus1 ~ (lognat_bs + ns(Latitude, df = 4))^3 + 
                (1|Genus/Binomial), data = fresh.bird, 
              na.action = "na.fail", family = "truncated_compois")

mcf = glmmTMB(nplus1 ~ (lognat_bs + ns(Latitude, df = 4))^3 + 
                (1|Genus/Binomial), data = marine.carti, 
              na.action = "na.fail", family = "truncated_compois")

mbf = glmmTMB(nplus1 ~ (lognat_bs + ns(Latitude, df = 4))^3 + 
                (1|Genus/Binomial), data = marine.bony, 
              na.action = "na.fail", family = "truncated_compois")

fbf = glmmTMB(nplus1 ~ (lognat_bs + ns(Latitude, df = 4))^3 + 
                (1|Genus/Binomial), data = fresh.bony, 
              na.action = "na.fail", family = "truncated_compois")

#start working on parallising dredge function
numCores <- detectCores()
registerDoParallel(numCores)
getDoParWorkers()

#put models in list
list <- list(ta, fa, tr, mr, fr, tm, mm, fm, tbi, mbi, fbi, mcf, mbf, fbf)

group.dredge <- foreach(i = list) %dopar% dredge(i)

ta.d <- group.dredge[[1]]
fa.d <- group.dredge[[2]]
tr.d <- group.dredge[[3]]
mr.d <- group.dredge[[4]]
fr.d <- group.dredge[[5]]
tm.d <- group.dredge[[6]]
mm.d <- group.dredge[[7]]
fm.d <- group.dredge[[8]]
tbi.d <- group.dredge[[9]]
mbi.d <- group.dredge[[10]]
fbi.d <- group.dredge[[11]]
mcf.d <- group.dredge[[12]]
mbf.d <- group.dredge[[13]]
fbf.d <- group.dredge[[14]]

###################################################################################
#run best models for each system/class group and generate predictions for plotting
###################################################################################

terr.amph.best <- glmmTMB(nplus1 ~ lognat_bs +
                            (1|Genus/Binomial), data = terr.amph, 
                          family = "truncated_compois")

bs.terr.amph.best <- ggpredict(terr.amph.best, 
                               terms = c("lognat_bs"),
                               interval = "confidence")

fresh.amph.best <- glmmTMB(nplus1 ~ lognat_bs * ns(Latitude, df = 4) + 
                             (1|Genus/Binomial), data = fresh.amph, 
                           family = "truncated_compois")

bs.lat.fresh.amph <- ggpredict(fresh.amph.best, 
                               terms = c("Latitude",
                                         "lognat_bs [quart2]"), 
                               interval = "confidence")

bs.fresh.amph.best <- ggpredict(fresh.amph.best, 
                                terms = c("lognat_bs"),
                                interval = "confidence")

lat.fresh.amph.best <- ggpredict(fresh.amph.best, 
                                 terms = c("Latitude"), 
                                 interval = "confidence")

terr.rept.best <- glmmTMB(nplus1 ~ lognat_bs + ns(Latitude, df = 4) + 
                            (1|Genus/Binomial), data = terr.rept, 
                          family = "truncated_compois")

bs.terr.rept.best <- ggpredict(terr.rept.best, 
                               terms = c("lognat_bs"), 
                               interval = "confidence")

lat.terr.rept.best <- ggpredict(terr.rept.best, 
                                terms = c("Latitude"), 
                                interval = "confidence")

marine.rept.best <- glmmTMB(nplus1 ~ lognat_bs + ns(Latitude, df = 4) + 
                              (1|Genus/Binomial), data = marine.rept, 
                            family = "truncated_compois")

bs.marine.rept.best <- ggpredict(marine.rept.best, 
                                 terms = c("lognat_bs"), 
                                 interval = "confidence")

lat.marine.rept.best <- ggpredict(marine.rept.best, 
                                  terms = c("Latitude"), 
                                  interval = "confidence")

fresh.rept.best <- glmmTMB(nplus1 ~ lognat_bs + ns(Latitude, df = 4) + 
                             (1|Genus/Binomial), data = fresh.rept, 
                           family = "truncated_compois")

bs.fresh.rept.best <- ggpredict(fresh.rept.best, 
                                terms = c("lognat_bs"), 
                                interval = "confidence")

lat.fresh.rept.best <- ggpredict(fresh.rept.best, 
                                 terms = c("Latitude"), 
                                 interval = "confidence")

terr.mamm.best <- glmmTMB(nplus1 ~ lognat_bs + ns(Latitude, df = 4) + 
                            (1|Genus/Binomial), data = terr.mamm, 
                          family = "truncated_compois")

bs.terr.mamm.best <- ggpredict(terr.mamm.best, 
                               terms = c("lognat_bs"), 
                               interval = "confidence")

lat.terr.mamm.best <- ggpredict(terr.mamm.best, 
                                terms = c("Latitude"), 
                                interval = "confidence")

marine.mamm.best <- glmmTMB(nplus1^3 ~ lognat_bs +
                              (1|Genus/Binomial), data = marine.mamm, 
                            family = "truncated_compois")

bs.marine.mamm.best <- ggpredict(marine.mamm.best, 
                                 terms = c("lognat_bs"), 
                                 interval = "confidence")

fresh.mamm.best <- glmmTMB(nplus1 ~ lognat_bs + ns(Latitude, df = 4) + 
                             (1|Genus/Binomial), data = fresh.mamm, 
                           family = "truncated_compois")

bs.fresh.mamm.best <- ggpredict(fresh.mamm.best, 
                                terms = c("lognat_bs"), 
                                interval = "confidence")

lat.fresh.mamm.best <- ggpredict(fresh.mamm.best, 
                                 terms = c("Latitude"), 
                                 interval = "confidence")

terr.bird.best <- glmmTMB(nplus1 ~ lognat_bs *  ns(Latitude, df = 4) + 
                            (1|Genus/Binomial), data = terr.bird, 
                          family = "truncated_compois")

bs.lat.terr.bird <- ggpredict(terr.bird.best, 
                              terms = c("Latitude",
                                        "lognat_bs [quart2]"), 
                              interval = "confidence")

bs.terr.bird.best <- ggpredict(terr.bird.best, 
                               terms = c("lognat_bs"), 
                               interval = "confidence")

lat.terr.bird.best <- ggpredict(terr.bird.best, 
                                terms = c("Latitude"), 
                                interval = "confidence")

marine.bird.best <- glmmTMB(nplus1 ~ lognat_bs * ns(Latitude, df = 4) + 
                              (1|Genus/Binomial), data = marine.bird, 
                            family = "truncated_compois")

bs.lat.marine.bird <- ggpredict(marine.bird.best, 
                                terms = c("Latitude",
                                          "lognat_bs [quart2]"), 
                                interval = "confidence")

bs.marine.bird.best <- ggpredict(marine.bird.best, 
                                 terms = c("lognat_bs"), 
                                 interval = "confidence")

lat.marine.bird.best <- ggpredict(marine.bird.best, 
                                  terms = c("Latitude"), 
                                  interval = "confidence")

fresh.bird.best <- glmmTMB(nplus1 ~ lognat_bs + ns(Latitude, df = 4) + 
                             (1|Genus/Binomial), data = fresh.bird, 
                           family = "truncated_compois")

bs.fresh.bird.best <- ggpredict(fresh.bird.best, 
                                terms = c("lognat_bs"),
                                interval = "confidence")

lat.fresh.bird.best <- ggpredict(fresh.bird.best, 
                                 terms = c("Latitude"), 
                                 interval = "confidence")

marine.carti.best <- glmmTMB(nplus1 ~ lognat_bs + ns(Latitude, df = 4) + 
                               (1|Genus/Binomial), data = marine.carti, 
                             family = "truncated_compois")

bs.marine.carti.best <- ggpredict(marine.carti.best, 
                                  terms = c("lognat_bs"), 
                                  interval = "confidence")

lat.marine.carti.best <- ggpredict(marine.carti.best, 
                                   terms = c("Latitude"), 
                                   interval = "confidence")

marine.bony.best <- glmmTMB(nplus1 ~ lognat_bs * ns(Latitude, df = 4) + 
                              (1|Genus/Binomial), data = marine.bony, 
                            family = "truncated_compois")

bs.lat.marine.bony <- ggpredict(marine.bony.best, 
                                terms = c("Latitude",
                                          "lognat_bs [quart2]"), 
                                interval = "confidence")

bs.marine.bony.best <- ggpredict(marine.bony.best, 
                                 terms = c("lognat_bs"), 
                                 interval = "confidence")

lat.marine.bony.best <- ggpredict(marine.bony.best, 
                                  terms = c("Latitude"), 
                                  interval = "confidence")

fresh.bony.best <- glmmTMB(nplus1 ~ lognat_bs * ns(Latitude, df = 4) + 
                             (1|Genus/Binomial), data = fresh.bony, 
                           family = "truncated_compois")

bs.lat.fresh.bony <- ggpredict(fresh.bony.best, 
                               terms = c("Latitude",
                                         "lognat_bs [quart2]"), 
                               interval = "confidence")

bs.fresh.bony.best <- ggpredict(fresh.bony.best, 
                                terms = c("lognat_bs"), 
                                interval = "confidence")

lat.fresh.bony.best <- ggpredict(fresh.bony.best, 
                                 terms = c("Latitude"), 
                                 interval = "confidence")

###################################################################################
#create function appending ggpredict objects with index column specifying original
#data frame
###################################################################################
append <- function(...){
  args <- list(...)
  result <- do.call(rbind,args)
  result$source <- rep(as.character(match.call()[-1]),times = sapply(args,nrow))
  result
}

###################################################################################
#combine latitude estimate predictions
###################################################################################
lat.marine <- append(lat.marine.bird.best, lat.marine.bony.best, 
                     lat.marine.carti.best,lat.marine.rept.best, 
                     lat.marine.rept.best)

lat.terr <- append(lat.terr.mamm.best, lat.terr.rept.best, lat.terr.bird.best)

lat.fresh <- append(lat.fresh.amph.best, lat.fresh.bird.best, lat.fresh.bony.best,
                    lat.fresh.mamm.best,
                    lat.fresh.rept.best)

###################################################################################
#combine body mass estimate predictions
###################################################################################
bs.marine <- append(bs.marine.bird.best, bs.marine.bony.best, bs.marine.carti.best, 
                    bs.marine.mamm.best, bs.marine.rept.best)

bs.terr <- append(bs.terr.amph.best, bs.terr.bird.best, bs.terr.mamm.best, 
                  bs.terr.rept.best)

bs.fresh <- append(bs.fresh.amph.best, bs.fresh.bird.best, bs.fresh.bony.best,
                   bs.fresh.mamm.best, bs.fresh.rept.best)

###################################################################################
#combine interactive estimate predictions
###################################################################################
bs.lat.fresh <- append(bs.lat.fresh.amph, bs.lat.fresh.bony)

bs.lat.terr <- append(bs.lat.terr.bird)

bs.lat.marine <- append(bs.lat.marine.bird, bs.lat.marine.bony)

###################################################################################
#find confidence intervals for all model estimates
###################################################################################
confint(marine.mamm.best)
confint(marine.bird.best)
confint(marine.bony.best)
confint(marine.carti.best)
confint(marine.rept.best)
confint(terr.mamm.best)
confint(terr.bird.best)
confint(terr.rept.best)
confint(terr.amph.best)
confint(fresh.mamm.best)
confint(fresh.bird.best)
confint(fresh.bony.best)
confint(fresh.amph.best)
confint(fresh.rept.best)

###################################################################################
#specify significant results for plotting different line styles
###################################################################################
bs.marine$sig <- rep(NA, nrow(bs.marine))
bs.marine[bs.marine$source == "bs.marine.mamm.best", ][, "sig"] <- "Non-significant"
bs.marine[bs.marine$source == "bs.marine.bird.best", ][, "sig"] <- "Non-significant"
bs.marine[bs.marine$source == "bs.marine.bony.best", ][, "sig"] <- "Non-significant"
bs.marine[bs.marine$source == "bs.marine.carti.best", ][, "sig"] <- "Significant"
bs.marine[bs.marine$source == "bs.marine.rept.best", ][, "sig"] <- "Non-significant"

bs.terr$sig <- rep(NA, nrow(bs.terr))
bs.terr[bs.terr$source == "bs.terr.mamm.best", ][, "sig"] <- "Significant"
bs.terr[bs.terr$source == "bs.terr.bird.best", ][, "sig"] <- "Significant"
bs.terr[bs.terr$source == "bs.terr.rept.best", ][, "sig"] <- "Non-significant"
bs.terr[bs.terr$source == "bs.terr.amph.best", ][, "sig"] <- "Non-significant"

bs.fresh$sig <- rep(NA, nrow(bs.fresh))
bs.fresh[bs.fresh$source == "bs.fresh.mamm.best", ][, "sig"] <- "Significant"
bs.fresh[bs.fresh$source == "bs.fresh.bird.best", ][, "sig"] <- "Non-significant"
bs.fresh[bs.fresh$source == "bs.fresh.bony.best", ][, "sig"] <- "Non-significant"
bs.fresh[bs.fresh$source == "bs.fresh.amph.best", ][, "sig"] <- "Non-significant"
bs.fresh[bs.fresh$source == "bs.fresh.rept.best", ][, "sig"] <- "Non-significant"

bs.terr$source <- as.character(bs.terr$source)
bs.terr$source[bs.terr$source == 'bs.terr.bony.best'] <- "Bony Fish"
bs.terr$source[bs.terr$source == 'bs.terr.carti.best'] <- "Cartilaginous Fish"
bs.terr$source[bs.terr$source == 'bs.terr.bird.best'] <- "Birds"
bs.terr$source[bs.terr$source == 'bs.terr.rept.best'] <- "Reptiles"
bs.terr$source[bs.terr$source == 'bs.terr.amph.best'] <- "Amphibians"
bs.terr$source[bs.terr$source == 'bs.terr.mamm.best'] <- "Mammals"

###################################################################################
#plot latitudinal estimates
###################################################################################

lt <- ggplot(lat.terr, aes(x, predicted-1)) +
  geom_line(aes(color=source), size = 1) + 
  geom_ribbon(aes(ymin=conf.low-1, ymax=conf.high-1, group = source, 
                  fill = source, colour = source),
              linetype =3, alpha = 0.1) +
  scale_colour_manual(values = c("#FDE725FF",  "#404788FF", "#20A387FF")) +
  scale_fill_manual(values = c("#FDE725FF",  "#404788FF", "#20A387FF")) +
  ggtitle("Terrestrial") +
  theme_bw() +
  ylim(-1,4) +
  scale_x_continuous(breaks=pretty_breaks(n=10)) +   
  ylab("Number of Threats") + 
  xlab("Latitude") +
  theme(legend.position = "none") +
  theme(legend.title = element_blank()) +
  theme(strip.text.x = element_text(size = 20, colour = "black")) +
  theme(axis.text.x=element_text(size = 20)) +
  theme(axis.text.y=element_text(size = 20)) +
  theme(axis.title=element_text(size = 20)) +
  theme(plot.title = element_text(size=20))

lm <- ggplot(lat.marine, aes(x, predicted-1)) +
  geom_line(aes(color=source), size = 1) + 
  geom_ribbon(aes(ymin=conf.low-1, ymax=conf.high-1, group = source,
                  fill = source, colour = source),
              linetype =3, alpha = 0.1) +
  scale_colour_manual(values = c("#FDE725FF", "#440154FF", "#2D708EFF", 
                                 "#20A387FF")) +
  scale_fill_manual(values = c("#FDE725FF", "#440154FF", "#2D708EFF", 
                               "#20A387FF")) +
  ggtitle("Marine") +
  theme_bw() +
  ylim(-1,4) +
  theme(legend.position = "none") +
  scale_x_continuous(breaks=pretty_breaks(n=10)) +   
  ylab("Number of Threats") + 
  xlab("Latitude") +
  theme(axis.title.y = element_blank()) +
  theme(legend.title = element_blank()) +
  theme(strip.text.x = element_text(size = 20, colour = "black")) +
  theme(axis.text.x=element_text(size = 20)) +
  theme(axis.text.y=element_text(size = 20)) +
  theme(axis.title=element_text(size = 20)) +
  theme(plot.title = element_text(size=20))

lf <- ggplot(lat.fresh, aes(x, predicted-1)) +
  geom_line(aes(color=source), size = 1) + 
  geom_ribbon(aes(ymin=conf.low-1, ymax=conf.high-1, group = source, 
                  fill = source, colour = source, alpha = 0.2),
              linetype =3, alpha = 0.1) +
  scale_colour_manual(values = c("#73D055FF", "#FDE725FF", "#440154FF", 
                                 "#404788FF", "#20A387FF")) +
  scale_fill_manual(values = c("#73D055FF", "#FDE725FF", "#440154FF", 
                               "#404788FF", "#20A387FF")) +
  ggtitle("Freshwater") +
  theme_bw() +
  ylim(-1,4) +
  theme(legend.position = "none") +
  scale_x_continuous(breaks=pretty_breaks(n=10)) +   
  ylab("Number of Threats") + 
  xlab("Latitude") +
  theme(axis.title.y = element_blank()) +
  theme(legend.title = element_blank()) +
  theme(strip.text.x = element_text(size = 20, colour = "black")) +
  theme(axis.text.x=element_text(size = 20)) +
  theme(axis.text.y=element_text(size = 20)) +
  theme(axis.title=element_text(size = 20)) +
  theme(plot.title = element_text(size=20))

grid.arrange(lt, lm, lf, nrow = 1)

###################################################################################
#plot body mass estimates
###################################################################################
bst <- ggplot(bs.terr, aes(x, predicted-1)) +
  geom_line(aes(linetype = sig, colour = source), size = 1) +
  scale_linetype_manual(values=c(4, 1)) +
  geom_ribbon(aes(ymin=conf.low-1, ymax=conf.high-1, group = source, 
                  fill = source, colour = source),
              linetype =3, alpha = 0.1) +
  scale_colour_manual(values = c("#73D055FF","#FDE725FF",  "#404788FF", "#20A387FF")) +
  scale_fill_manual(values = c("#73D055FF","#FDE725FF",  "#404788FF", "#20A387FF")) +
  ggtitle("Terrestrial") +
  theme_bw() +
  ylim(0,3) +
  scale_x_continuous(breaks=pretty_breaks(n=10)) +   
  ylab("Number of Threats") + 
  xlab("ln body mass") +
  theme(legend.position = "none") +
  theme(legend.title = element_blank()) +
  theme(strip.text.x = element_text(size = 20, colour = "black")) +
  theme(axis.text.x=element_text(size = 20)) +
  theme(axis.text.y=element_text(size = 20)) +
  theme(axis.title=element_text(size = 20)) +
  theme(plot.title = element_text(size=20))

bsm <- ggplot(bs.marine, aes(x, predicted-1)) +
  geom_line(aes(linetype = sig, colour = source), size = 1) +
  scale_linetype_manual(values=c(4, 1)) +
  geom_ribbon(aes(ymin=conf.low-1, ymax=conf.high-1, group = source, 
                  fill = source, colour = source),
              linetype =3, alpha = 0.1) +
  scale_colour_manual(values = c("#FDE725FF", "#440154FF", "#2D708EFF", "#404788FF",
                                 "#20A387FF")) +
  scale_fill_manual(values = c("#FDE725FF", "#440154FF", "#2D708EFF", "#404788FF",
                               "#20A387FF")) +
  ggtitle("Marine") +
  theme_bw() +
  ylim(0,3) +
  theme(legend.position = "none") +
  scale_x_continuous(breaks=pretty_breaks(n=10)) +   
  ylab("Number of Threats") + 
  xlab("ln body mass") +
  theme(axis.title.y = element_blank()) +
  theme(legend.title = element_blank()) +
  theme(strip.text.x = element_text(size = 20, colour = "black")) +
  theme(axis.text.x=element_text(size = 20)) +
  theme(axis.text.y=element_text(size = 20)) +
  theme(axis.title=element_text(size = 20)) +
  theme(plot.title = element_text(size=20))

bsf <- ggplot(bs.fresh, aes(x, predicted-1)) +
  geom_line(aes(linetype = sig, colour = source), size = 1) +
  scale_linetype_manual(values=c(4, 1)) +
  geom_ribbon(aes(ymin=conf.low-1, ymax=conf.high-1, group = source, 
                  fill = source, colour = source),
              linetype =3, alpha = 0.1) +
  scale_colour_manual(values = c("#73D055FF","#FDE725FF", "#440154FF", "#404788FF",
                                 "#20A387FF")) +
  scale_fill_manual(values = c("#73D055FF","#FDE725FF", "#440154FF", "#404788FF",
                               "#20A387FF")) +
  ggtitle("Freshwater") +
  theme_bw() +
  ylim(0,3) +
  theme(legend.position = "none") +
  scale_x_continuous(breaks=pretty_breaks(n=10)) +   
  ylab("Number of Threats") + 
  xlab("ln body mass") +
  theme(legend.title = element_blank()) +
  theme(axis.title.y = element_blank()) +
  theme(strip.text.x = element_text(size = 20, colour = "black")) +
  theme(axis.text.x=element_text(size = 20)) +
  theme(axis.text.y=element_text(size = 20)) +
  theme(axis.title=element_text(size = 20)) +
  theme(plot.title = element_text(size=20))

grid.arrange(bst, bsm, bsf, nrow = 1)

###################################################################################
#plot inetractive latitude and body mass estimates
###################################################################################
bs.l.t <- ggplot(bs.lat.terr, aes(x, predicted-1)) +
  geom_line(aes(colour = group), size = 1) +
  scale_linetype_manual(values=c(4, 1)) +
  geom_ribbon(aes(ymin=conf.low-1, ymax=conf.high-1, group = group, fill = group, 
                  colour = group),
              linetype =3, alpha = 0.1) +
  scale_colour_manual(labels = c("First", "Median", "Third"),
                      values = c("goldenrod1", "forestgreen", "hotpink4")) +
  scale_fill_manual(values = c("goldenrod1", "forestgreen", "hotpink4")) +
  ggtitle("Terrestrial") +
  theme_bw() +
  ylim(-1, 4) +
  scale_x_continuous(breaks=pretty_breaks(n=10)) +   
  ylab("Number of Threats") + 
  xlab("Latitude") +
  theme(legend.position = "none") +
  theme(legend.title = element_blank()) +
  theme(strip.text.x = element_text(size = 20, colour = "black")) +
  theme(axis.text.x=element_text(size = 20)) +
  theme(axis.text.y=element_text(size = 20)) +
  theme(axis.title=element_text(size = 20)) +
  theme(plot.title = element_text(size=20))+ 
  facet_wrap(~ source, scales = "free") +
  guides(color = guide_legend(override.aes = list(size=20))) +
  theme(legend.key.size = unit(2, "cm"),
        legend.key.width = unit(2,"cm")) +
  guides(color=guide_legend(override.aes=list(fill=c("goldenrod1", 
                                                     "forestgreen", "hotpink4")))) 



bs.l.m <- ggplot(bs.lat.marine, aes(x, predicted-1)) +
  geom_line(aes(colour = group), size = 1) +
  scale_linetype_manual(values=c(4, 1)) +
  geom_ribbon(aes(ymin=conf.low-1, ymax=conf.high-1, group = group, fill = group, 
                  colour = group),
              linetype =3, alpha = 0.1) +
  scale_colour_manual(values = c("goldenrod1", "forestgreen", "hotpink4",
                                 "goldenrod1", "forestgreen", "hotpink4")) +
  scale_fill_manual(values = c("goldenrod1", "forestgreen", "hotpink4",
                               "goldenrod1", "forestgreen", "hotpink4")) +
  ggtitle("Marine") +
  theme_bw() +
  ylim(-1, 4) +
  scale_x_continuous(breaks=pretty_breaks(n=10)) +   
  ylab("Number of Threats") + 
  xlab("Latitude") +
  theme(legend.position = "none") +
  theme(axis.title.y = element_blank()) +
  theme(legend.title = element_blank()) +
  theme(strip.text.x = element_text(size = 20, colour = "black")) +
  theme(axis.text.x=element_text(size = 20)) +
  theme(axis.text.y=element_text(size = 20)) +
  theme(axis.title=element_text(size = 20)) +
  theme(plot.title = element_text(size=20)) + 
  facet_wrap(~ source, scales = "free") 

bs.l.f <- ggplot(bs.lat.fresh, aes(x, predicted-1)) +
  geom_line(aes(colour = group), size = 1) +
  scale_linetype_manual(values=c(4, 1)) +
  geom_ribbon(aes(ymin=conf.low-1, ymax=conf.high-1, group = group, fill = group, 
                  colour = group),
              linetype =3, alpha = 0.1) +
  scale_colour_manual(values = c("goldenrod1", "forestgreen", "hotpink4",
                                 "goldenrod1", "forestgreen", "hotpink4")) +
  scale_fill_manual(values = c("goldenrod1", "forestgreen", "hotpink4",
                               "goldenrod1", "forestgreen", "hotpink4")) +
  ggtitle("Freshwater") +
  theme_bw() +
  ylim(-1, 4) +
  scale_x_continuous(breaks=pretty_breaks(n=10)) +   
  ylab("Number of Threats") + 
  xlab("Latitude") +
  theme(legend.position = "none") +
  theme(legend.title = element_blank()) +
  theme(strip.text.x = element_text(size = 20, colour = "black")) +
  theme(axis.text.x=element_text(size = 20)) +
  theme(axis.text.y=element_text(size = 20)) +
  theme(axis.title=element_text(size = 20)) +
  theme(plot.title = element_text(size=20)) +
  facet_wrap(~ source) 

grid.arrange(bs.l.t, bs.l.m, bs.l.f, nrow = 2)
