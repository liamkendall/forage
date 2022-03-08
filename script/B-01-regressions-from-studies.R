#This code consists of regressions to determine typical (50%) and maximum (90%) foraging ranges from return rates in homing studies
#Primarily these are logistic regressions, however, where authors reported other regression types - and equations - these are used.

####libraries
library(MASS)

########################
## Costa et al.  2021 ##
########################

costa.2021 <- data.frame(ntotal=30,
           col=c(rep("h1",9),
                 rep("h2",9),
                 rep("h3",9)),
           dist=c(100,500,1000,1500,2000,
                  2500,3000,4000,5000,
                  100,500,1000,1500,2000,
                  2500,3000,4000,5000,
                  100,500,1000,1500,2000,
                  2500,3000,4000,5000),
           return=c(28,21,18,15,6,3,3,3,1,
                    23,25,24,13,6,1,0,1,0,
                    10,9,9,7,1,0,0,0,0))


costa.glm <- glm(cbind(return,ntotal-return)~dist*col,
    family="binomial",
    data=costa.2021)


costa.glm1 <- glm(cbind(return,ntotal-return)~dist,
                 family="binomial",
                 data=costa.2021 %>% 
                   filter(col%in%"h1"))
LD50.1 <- dose.p(costa.glm1, p = 0.5)
LD90.1 <- dose.p(costa.glm1, p = 0.1)

costa.glm2 <- glm(cbind(return,ntotal-return)~dist,
                 family="binomial",
                 data=costa.2021 %>% 
                   filter(col%in%"h2"))

LD50.2 <- dose.p(costa.glm2, p = 0.5)
LD90.2 <- dose.p(costa.glm2, p = 0.1)


costa.glm3 <- glm(cbind(return,ntotal-return)~dist,
                 family="binomial",
                 data=costa.2021 %>% 
                   filter(col%in%"h3"))

LD50.3 <- dose.p(costa.glm3, p = 0.5)
LD90.3 <- dose.p(costa.glm3, p = 0.1)

#################
## Geudot 2009 ##
#################

geudot.09 <- data.frame(ntotal=10,
                         dist=c(600,800,1000,1200,1400,
                                1600),
                         return=c(9,8,7,7,0,0))

geudot.glm <- glm(cbind(return,ntotal-return)~dist,
                  family="binomial",
                  data=geudot.09)

LD50.4 <- dose.p(geudot.glm, p = 0.5)
LD90.4 <- dose.p(geudot.glm, p = 0.1)

######################
## Nunes-Silva 2020 ##
######################

nunes.2020 <- data.frame(n.return=c(36,32,28,22,
                                  22,20,19,
                                  22,21,20,
                                  15,8,1),
                         col=c(rep("h1",7),
                               rep("h2",6)),
                         dist=c(100,600,1100,1600,2100,
                                2500,3000,
                                1500,3000,
                                4000,5000,7500,10000),
                         return.perc=c(0.80,0.711,0.622,
                                  0.489,0.489,
                                  0.444,0.422,
                                  0.733,
                                  0.7,
                                  0.667,
                                  0.5,
                                  0.267,
                                  0.033))

nunes.2020$n.total <- round(nunes.2020$n.return/nunes.2020$return.perc)

nunes.glm1 <- glm(cbind(n.return,n.total-n.return)~dist,
                  family="binomial",
                  data=nunes.2020 %>% 
                    filter(col%in%"h1"))
LD50.5 <- dose.p(nunes.glm1, p = 0.5)
LD90.5 <- dose.p(nunes.glm1, p = 0.1)

LD50.5
LD90.5

nunes.glm2 <- glm(cbind(n.return,n.total-n.return)~dist,
                  family="binomial",
                  data=nunes.2020 %>% 
                    filter(col%in%"h2"))

LD50.6 <- dose.p(nunes.glm2, p = 0.5)
LD90.6 <- dose.p(nunes.glm2, p = 0.1)
LD50.6
LD90.6

##########################
## Goulson & Stout 2001 ##
##########################


#50%
0.56 - 0.048 *(1.25)
#90%
0.56 - 0.048 *(9.58)

#####################
## Pahl & Zhu 2011 ##
#####################

pahl <- structure(list(group = c("East", "East", "East", "East", "East", 
                         "East", "East", "East", "East", "East", "East", "East", "East", 
                         "West", "West", "West", "West", "West", "West", "North", "North", 
                         "North", "North", "North", "North", "South", "South", "South", 
                         "South", "South", "South", "South", "South", "South", "South"),
                       dist = c(292.4268277, 939.2218184, 1964.607537, 2931.971342, 
                                2935.003689, 3296.101851, 4442.84932, 4923.544134, 5961.964417, 
                                7021.430091, 7767.930446, 10951.30595, 11066.76141, 1381.356047, 
                                2496.17336, 3030.952564, 4038.279981, 7051.663038, 274.5948197, 
                                274.2780073, 1948.359591, 2976.008255, 4019.180723, 4992.835516, 
                                7015.320139, 379.9122882, 448.2984915, 770.5871438, 1086.494291, 
                                1440.780988, 1844.490407, 3007.689488, 4001.258198, 5022.208544, 
                                5996.315925), 
                       proportion = c(78.60521111, 99.56877317, 80.21606601,
                                      84.5149377, 70.73595503, 69.90590674, 59.08540808, 74.80817014, 
                                      36.22640314, 22.01438328, 29.91676888, 4.658498943, 0, 90.51083724, 
                                      64.78096954, 34.74426457, 17.44848405, 4.635869491, 79.63385547, 
                                      81.07345067, 74.04673434, 44.41120429, 24.23550923, 0, 9.778005078, 
                                      81.07127825, 90.32437056, 65.84473481, 70.36265959, 60.4839082, 
                                      66.02830492, 60.45168386, 25.67546651, 26.47709221, 0), 
                       ntotal = c(20L,  20L, 20L, 20L, 20L, 20L, 20L, 20L, 20L, 20L, 20L, 20L, 20L, 20L, 
                                  20L, 20L, 20L, 20L, 20L, 20L, 20L, 20L, 20L, 20L, 20L, 20L, 20L, 
                                  20L, 20L, 20L, 20L, 20L, 20L, 20L, 20L), 
                       nreturn = c(16L, 20L, 16L, 17L, 14L, 14L, 12L, 15L, 7L, 4L, 6L, 1L, 0L, 18L, 13L, 7L, 
                                   3L, 1L, 16L, 16L, 15L, 9L, 5L, 0L, 2L, 16L, 18L, 13L, 14L, 12L, 
                                   13L, 12L, 5L, 5L, 0L)), 
                  class = "data.frame", 
                  row.names = c(NA, -35L))

pahl.glm1 <- glm(cbind(nreturn,ntotal-nreturn)~dist,
                   family="binomial",
                   data=pahl %>% 
                   filter(group%in%"West"))

dose.p(pahl.glm1, p = 0.1)
dose.p(pahl.glm1, p = 0.5)

##############
## Rao 2019 ##
##############

rao19 <- data.frame(dist=c(1500,3000,5000,7500,10000,13000,16000,
       1500,3000,5000,7500,10000,13000,16000),
group=c("east","east","east","east","east","east","east",
        "west","west","west","west","west","west","west"),
ntotal=15,
nreturn=c(5,8,7,2,0,0,0,
          7,6,7,3,5,3,1))

rao.glm1 <- glm(cbind(nreturn,ntotal-nreturn)~dist,
                 family="binomial",
                 data=rao19 %>% 
                   filter(group%in%"west"))

dose.p(rao.glm1, p = 0.1)
dose.p(rao.glm1, p = 0.5)
15*7


####################
## Rodrigues 2014 ##
####################

rodri <- data.frame(dist=c(500,1000,1500,2000,2500,
                           500,1000,1500,2000,2500),
                    group=c("mix","mix","mix","mix","mix",
                            "exp","exp","exp","exp","exp"),
                    ntotal=c(125,125,125,125,100,
                             12,13,35,33,20),
                    nreturn=c(65,37,19,22,2,
                              12,10,10,4,4))


rodr.glm1 <- glm(cbind(nreturn,ntotal-nreturn)~dist,
                family="binomial",
                data=rodri %>% 
                  filter(group%in%"mix"))

dose.p(rodr.glm1, p = 0.1)
dose.p(rodr.glm1, p = 0.5)


################
## Silva 2014 ##
################

silva <- data.frame(dist=c(200,400,600,800,1000,
                           1200,1400,1600,1800,2000,
                           2400,2800,3200,2600,4000,
                           200,400,600,800,1000,
                           1200,1400,1600,1800,2000,
                           2400,2800,3200,2600,4000,
                           200,400,600,800,1000,
                           1200,1400,1600,1800,2000,
                           2400,2800,3200,2600,4000),
                    group=c("one","one","one","one","one",
                            "one","one","one","one","one",
                            "one","one","one","one","one",
                            "two","two","two","two","two",
                            "two","two","two","two","two",
                            "two","two","two","two","two",
                            "three","three","three","three","three",
                            "three","three","three","three","three",
                            "three","three","three","three","three"),
                    ntotal=10,
                    nreturn=c(10,10,9,8,7,7,5,5,6,5,5,3,3,3,1,
                              10,9,7,8,6,9,4,9,7,5,4,4,1,2,0,
                              10,10,8,9,10,7,10,5,9,8,6,7,6,1,0))

silva.glm1 <- glm(cbind(nreturn,ntotal-nreturn)~dist,
                 family="binomial",
                 data=silva %>% 
                   filter(group%in%"three"))

dose.p(silva.glm1, p = 0.1)
dose.p(silva.glm1, p = 0.5)

###############################
## Southwick & Buchmann 1995 ##
###############################

0.419-0.595 * log10(0.73) 
0.419 - 0.595 * log10(3.43) 

#typical
#730m
#max
#3430m

#arizona
0.398-0.517 * log10(0.63)
#630m
0.398-0.517 * log10(3.77)
#3770m

#mountain
0.930-0.100*4.3
#4300m
0.930-0.100*8.3
#8300m

#####################################
## Van Nieuwstadt and Iraheta 1996 ##
#####################################

#Nannotrigona_perilampoides
-.1*(123)+62.3
#typ=123

-.1*(523)+62.3
#max = 523

#Partamona_cupira
-0.075 * (134) +60.1
#134m
-0.075 * (668) +60.1
#668m

#Tetragonisca_angustula
#no typical as intercept between 50
-0.051 * (656.5) +43.5
#656m

#Trigona_corvina
-0.094* (227) +71.4
#227m
-0.094* (653) +71.4
#653

#########################
## Vicens & Bosch 2000 ##
#########################

vicen <- data.frame(dist=c(800,1400,1600,1800,
                           2000,2200,2400,2600),
                    ntotal=c(5,5,5,10,
                             5,5,5,10),
                    nreturn=c(5,5,4,9,
                              1,1,0,0))

vicen.glm1 <- glm(cbind(nreturn,ntotal-nreturn)~dist,
                  family="binomial",
                  data=vicen)

dose.p(vicen.glm1, p = 0.1)
dose.p(vicen.glm1, p = 0.5)

