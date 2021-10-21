
# This is the original R script from EA (now converted into package):

MacroP.metrics.f <- readRDS('Data_Input/MAC_OPEN_DATA_METRICS_F.rds')
MacroP.metrics.f <- MacroP.metrics.f %>% mutate(SITECODE = paste0(SITE_ID))
#  transform(MacroP.metrics.f, SITE_ID = as.character(SITE_ID))
MacroP.metrics.f <- MacroP.metrics.f %>% filter(SITE_ID %in%MPsitelist1()) %>%
  mutate(SAMPLE_DATE = as.Date(paste(substr(SAMPLE_DATE, start = 1, stop = 10)),format="%d/%m/%Y")) %>% mutate(YEAR = as.integer(year(SAMPLE_DATE))) %>% mutate(MONTH = as.integer(month(SAMPLE_DATE)))
MacroP.metrics.f2a <- inner_join(MacroP.metrics.f , BioSiteListMP2(),by = "SITECODE")
MacroP.metrics.f2 <- subset (MacroP.metrics.f2a ,select =-c(REPLICATE_CODE))
MacroP.metrics.f2 <-MacroP.metrics.f2  %>% filter(complete.cases(.))

MacroP.metrics.f2$logSlope <- log10(MacroP.metrics.f2$SLOPE)
########### calculate reference RMNI ######

MacroP.metrics.f2 <- MacroP.metrics.f2 %>% mutate(REF_RMNI = (5.239+(1.384*log10(ALKALINITY+1))+(-0.68*log10(SLOPE+1))+(0.711*log10(DIST_FROM_SOURCE+1))+(-1.074*log10(SOURCE_ALTITUDE+1))))



########### calculate reference TAxA ###########
MacroP.metrics.f2 <- MacroP.metrics.f2 %>% mutate(REF_TAXA=(10.026*exp(log10(SLOPE+1)*-0.426)))
########### calculate reference Fuctional groups ###########
MacroP.metrics.f2 <- MacroP.metrics.f2 %>% mutate(REF_NFG=(6.304*exp(log10(SLOPE+1)*-0.377))) %>% mutate(REF_ALGAE = 0.05)

################### RMNI EQR ###########################
MacroP.metrics.f2 <- MacroP.metrics.f2 %>% mutate(RMNI_EQR = (RMNI-10)/(REF_RMNI-10))
################### NTAXA EQR ##########################
MacroP.metrics.f2 <- MacroP.metrics.f2 %>%mutate(NTAXA_EQR = (RN_A_TAXA / REF_TAXA)) %>%  mutate(RMNI_EQR_ADJ = RMNI_EQR)

################### NFG EQR ############################
MacroP.metrics.f2 <- MacroP.metrics.f2 %>% mutate(NFG_EQR = (N_RFG / REF_NFG))

################### ALGal EQR ##########################
MacroP.metrics.f2 <- MacroP.metrics.f2 %>% mutate(ALG_EQR = (RFA_PC-100)/(REF_ALGAE-100)) #%>%  mutate(RDiversity_EQR_ADJ = min(MacroP.metrics.f2$NTAXA_EQR,MacroP.metrics.f2$NFG_EQR))

######################### add adjustments to RMNI EQR #########

for (n in 1:nrow(MacroP.metrics.f2)) {
  if( MacroP.metrics.f2$RMNI_EQR[n] > 1) {MacroP.metrics.f2[n,"RMNI_EQR_ADJ"] <-1}}

for (n in 1:nrow(MacroP.metrics.f2)) {
  if ( MacroP.metrics.f2$RMNI_EQR[n] >= 0.85 &  MacroP.metrics.f2$RMNI_EQR[n] < 1) {MacroP.metrics.f2[n,"RMNI_EQR_ADJ"] <-((( MacroP.metrics.f2$RMNI_EQR[n]-0.85)/(1-0.85))*0.2+0.8)}}

for (n in 1:nrow(MacroP.metrics.f2)) {
  if( MacroP.metrics.f2$RMNI_EQR[n] >= 0.70 &  MacroP.metrics.f2$RMNI_EQR[n] < 0.85) {MacroP.metrics.f2[n,"RMNI_EQR_ADJ"] <-((( MacroP.metrics.f2$RMNI_EQR[n]-0.7)/(0.85-0.7))*0.2+0.6)}}

for (n in 1:nrow(MacroP.metrics.f2)) {
  if( MacroP.metrics.f2$RMNI_EQR[n] >= 0.52 &  MacroP.metrics.f2$RMNI_EQR[n] < 0.7) {MacroP.metrics.f2[n,"RMNI_EQR_ADJ"] <-((( MacroP.metrics.f2$RMNI_EQR[n]-0.52)/(0.7-0.52))*0.2+0.4)}}

for (n in 1:nrow(MacroP.metrics.f2)) {
  if( MacroP.metrics.f2$RMNI_EQR[n] >= 0.34 &  MacroP.metrics.f2$RMNI_EQR[n] < 0.52) {MacroP.metrics.f2[n,"RMNI_EQR_ADJ"] <-((( MacroP.metrics.f2$RMNI_EQR[n]-0.34)/(0.52-0.34))*0.2+0.2)}}

for (n in 1:nrow(MacroP.metrics.f2)) {
  if( MacroP.metrics.f2$RMNI_EQR[n] >= 0.16 &  MacroP.metrics.f2$RMNI_EQR[n] < 0.34) {MacroP.metrics.f2[n,"RMNI_EQR_ADJ"] <-((( MacroP.metrics.f2$RMNI_EQR[n]-0.16)/(0.34-0.16))*0.2)}}

for (n in 1:nrow(MacroP.metrics.f2)) {
  if(MacroP.metrics.f2$RMNI_EQR[n] <0.16) {MacroP.metrics.f2[n,"RMNI_EQR_ADJ"] <-0} }

################# adjustments to Ntaxa or NFG EQR ##############

for (n in 1:nrow(MacroP.metrics.f2)) {
  if(min(MacroP.metrics.f2$NTAXA_EQR[n],MacroP.metrics.f2$NFG_EQR[n]) >= 0.83 ) {MacroP.metrics.f2[n,"Diversity_EQR_ADJ"] <-((( min(MacroP.metrics.f2$NTAXA_EQR[n],MacroP.metrics.f2$NFG_EQR[n])-0.83)/(1-0.83))*0.2+0.8)} }

for (n in 1:nrow(MacroP.metrics.f2)) {
  if(min(MacroP.metrics.f2$NTAXA_EQR[n],MacroP.metrics.f2$NFG_EQR[n]) >= 0.66 & min(MacroP.metrics.f2$NTAXA_EQR[n],MacroP.metrics.f2$NFG_EQR[n]) <0.83) {MacroP.metrics.f2[n,"Diversity_EQR_ADJ"] <-((( min(MacroP.metrics.f2$NTAXA_EQR[n],MacroP.metrics.f2$NFG_EQR[n])-0.66)/(0.83-0.66))*0.2+0.6)} }

for (n in 1:nrow(MacroP.metrics.f2)) {
  if(min(MacroP.metrics.f2$NTAXA_EQR[n],MacroP.metrics.f2$NFG_EQR[n]) >= 0.49 & min(MacroP.metrics.f2$NTAXA_EQR[n],MacroP.metrics.f2$NFG_EQR[n]) <0.66) {MacroP.metrics.f2[n,"Diversity_EQR_ADJ"] <-((( min(MacroP.metrics.f2$NTAXA_EQR[n],MacroP.metrics.f2$NFG_EQR[n])-0.49)/(0.66-0.49))*0.2+0.4)} }

for (n in 1:nrow(MacroP.metrics.f2)) {
  if(min(MacroP.metrics.f2$NTAXA_EQR[n],MacroP.metrics.f2$NFG_EQR[n]) >= 0.32 & min(MacroP.metrics.f2$NTAXA_EQR[n],MacroP.metrics.f2$NFG_EQR[n]) <0.49) {MacroP.metrics.f2[n,"Diversity_EQR_ADJ"] <-((( min(MacroP.metrics.f2$NTAXA_EQR[n],MacroP.metrics.f2$NFG_EQR[n])-0.32)/(0.49-0.32))*0.2+0.2)} }

for (n in 1:nrow(MacroP.metrics.f2)) {
  # if(min(MacroP.metrics.f2$NTAXA_EQR[n],MacroP.metrics.f2$NFG_EQR[n]) <0.32 & min(MacroP.metrics.f2$NTAXA_EQR[n],MacroP.metrics.f2$NFG_EQR[n]) >0.15) {MacroP.metrics.f2[n,"Diversity_EQR_ADJ"] <-((( min(MacroP.metrics.f2$NTAXA_EQR[n],MacroP.metrics.f2$NFG_EQR[n])-0.15)/(0.32-0.15))*0.2)} }
  if(min(MacroP.metrics.f2$NTAXA_EQR[n],MacroP.metrics.f2$NFG_EQR[n]) <0.32) {MacroP.metrics.f2[n,"Diversity_EQR_ADJ"] <-((( min(MacroP.metrics.f2$NTAXA_EQR[n],MacroP.metrics.f2$NFG_EQR[n])-0.15)/(0.32-0.15))*0.2)} }
################# adjustments to ALGAL EQR ##############

for (n in 1:nrow(MacroP.metrics.f2)) {
  if( MacroP.metrics.f2$ALG_EQR[n] >= 0.975) {MacroP.metrics.f2[n,"ALG_EQR_ADJ"] <-((( MacroP.metrics.f2$ALG_EQR[n]-0.975)/(1-0.975))*0.2+0.8)}}

for (n in 1:nrow(MacroP.metrics.f2)) {
  if( MacroP.metrics.f2$ALG_EQR[n] >= 0.925 & MacroP.metrics.f2$ALG_EQR[n] <0.975 ) {MacroP.metrics.f2[n,"ALG_EQR_ADJ"] <-((( MacroP.metrics.f2$ALG_EQR[n]-0.925)/(0.975-0.925))*0.2+0.6)}}

for (n in 1:nrow(MacroP.metrics.f2)) {
  if( MacroP.metrics.f2$ALG_EQR[n] >= 0.825 & MacroP.metrics.f2$ALG_EQR[n] <0.925 ) {MacroP.metrics.f2[n,"ALG_EQR_ADJ"] <-((( MacroP.metrics.f2$ALG_EQR[n]-0.825)/(0.925-0.825))*0.2+0.4)}}

for (n in 1:nrow(MacroP.metrics.f2)) {
  if( MacroP.metrics.f2$ALG_EQR[n] >= 0.625 & MacroP.metrics.f2$ALG_EQR[n] <0.825 ) {MacroP.metrics.f2[n,"ALG_EQR_ADJ"] <-((( MacroP.metrics.f2$ALG_EQR[n]-0.625)/(0.825-0.625))*0.2+0.2)}}

for (n in 1:nrow(MacroP.metrics.f2)) {
  if( MacroP.metrics.f2$ALG_EQR[n] <0.625) {MacroP.metrics.f2[n,"ALG_EQR_ADJ"] <-(( MacroP.metrics.f2$ALG_EQR[n]-0.625)*0.2+0.8)}}

##################### Combining the ecological ratios for each metric ############


for (n in 1:nrow(MacroP.metrics.f2)) {
  if( MacroP.metrics.f2$Diversity_EQR_ADJ[n] < MacroP.metrics.f2$RMNI_EQR_ADJ[n]) {MacroP.metrics.f2[n,"CompositionDiveristy"] <-((0.5* MacroP.metrics.f2$Diversity_EQR_ADJ[n]+MacroP.metrics.f2$RMNI_EQR_ADJ[n]))/1.5}
  else {MacroP.metrics.f2[n,"CompositionDiveristy"] <-MacroP.metrics.f2$RMNI_EQR_ADJ[n]}}
for (n in 1:nrow(MacroP.metrics.f2)) {MacroP.metrics.f2[n,"Z"] <-(2*(1/(exp(log (2600000000)+MacroP.metrics.f2$REF_RMNI[n]*log(0.0166))+1/0.5)))}

for (n in 1:nrow(MacroP.metrics.f2)) {
  if( MacroP.metrics.f2$CompositionDiveristy[n] < MacroP.metrics.f2$ALG_EQR_ADJ[n]) {MacroP.metrics.f2[n,"EQR_LEAFPACS"] <-MacroP.metrics.f2$CompositionDiveristy[n]}
  else {MacroP.metrics.f2[n,"EQR_LEAFPACS"] <-(MacroP.metrics.f2$Z[n]*MacroP.metrics.f2$ALG_EQR_ADJ[n]+MacroP.metrics.f2$CompositionDiveristy[n])/(MacroP.metrics.f2$Z[n]+1)}}
######### cap final EQR #########
MacroP.metrics.f2$EQR_LEAFPACS_Capped <- MacroP.metrics.f2$EQR_LEAFPACS
for (n in 1:nrow(MacroP.metrics.f2)) {
  if( MacroP.metrics.f2$EQR_LEAFPACS[n] >1) { MacroP.metrics.f2[n,EQR_LEAFPACS_Capped ] <- 1}}

################ add columns for confidence and classification results ###########
MacroP.metrics.f2$CLASS <-  c("BLANK")
MacroP.metrics.f2$SE <-  0
MacroP.metrics.f2$trsfdMean <-  0
MacroP.metrics.f2$trsfdError <-  0
MacroP.metrics.f2$Normdist1 <-  0
MacroP.metrics.f2$Normdist2 <-  0
MacroP.metrics.f2$Normdist3 <-  0
MacroP.metrics.f2$Normdist4 <-  0
MacroP.metrics.f2$Bad <-  0
MacroP.metrics.f2$Poor <-  0
MacroP.metrics.f2$Moderate <-  0
MacroP.metrics.f2$Good <-  0
MacroP.metrics.f2$High <-  0
########  calculate CLASS #######################
for (n in 1:nrow(MacroP.metrics.f2)) {
  # if(numeric(MacroP.metrics.f2$EQR_LEAFPACS[n])) {
  if( MacroP.metrics.f2$EQR_LEAFPACS[n] <0.2) { MacroP.metrics.f2$CLASS[n] <- "BAD"} else
    if( MacroP.metrics.f2$EQR_LEAFPACS[n] <0.4) { MacroP.metrics.f2$CLASS[n] <- "POOR"} else
      if( MacroP.metrics.f2$EQR_LEAFPACS[n] <0.6) { MacroP.metrics.f2$CLASS[n] <- "MODERATE"} else
        if( MacroP.metrics.f2$EQR_LEAFPACS[n] <0.8) { MacroP.metrics.f2$CLASS[n] <- "GOOD"} else
          if( MacroP.metrics.f2$EQR_LEAFPACS[n] >=0.8) { MacroP.metrics.f2$CLASS[n] <- "HIGH"}}# else {MacroP.metrics.f2[n,CLASS ] <- "UNCLASSIFIABLE"}}

################ SE value ##########################
for (n in 1:nrow(MacroP.metrics.f2)) {
  MacroP.metrics.f2$SE[n]  <-(( 0.04+-2.98*MacroP.metrics.f2$EQR_LEAFPACS_Capped[n]+2.96*MacroP.metrics.f2$EQR_LEAFPACS_Capped[n]^0.95)/sqrt(1))}
for (n in 1:nrow(MacroP.metrics.f2)) {
  MacroP.metrics.f2$trsfdMean[n]  <- log(MacroP.metrics.f2$EQR_LEAFPACS_Capped[n]/(1-MacroP.metrics.f2$EQR_LEAFPACS_Capped[n]))}
for (n in 1:nrow(MacroP.metrics.f2)) {
  MacroP.metrics.f2$trsfdError[n]  <-( MacroP.metrics.f2$SE[n])/(MacroP.metrics.f2$EQR_LEAFPACS_Capped[n]*(1-MacroP.metrics.f2$EQR_LEAFPACS_Capped[n]))}

for (n in 1:nrow(MacroP.metrics.f2)) {
  MacroP.metrics.f2$Normdist1[n]  <-pnorm((-1.386-MacroP.metrics.f2$trsfdMean[n])/(MacroP.metrics.f2$trsfdError[n]))}
for (n in 1:nrow(MacroP.metrics.f2)) {
  MacroP.metrics.f2$Normdist2[n]  <-pnorm((-0.405-MacroP.metrics.f2$trsfdMean[n])/(MacroP.metrics.f2$trsfdError[n]))}
for (n in 1:nrow(MacroP.metrics.f2)) {
  MacroP.metrics.f2$Normdist3[n]  <-pnorm((0.405-MacroP.metrics.f2$trsfdMean[n])/(MacroP.metrics.f2$trsfdError[n]))}
for (n in 1:nrow(MacroP.metrics.f2)) {
  MacroP.metrics.f2$Normdist4[n]  <-pnorm((1.386-MacroP.metrics.f2$trsfdMean[n])/(MacroP.metrics.f2$trsfdError[n]))}

for (n in 1:nrow(MacroP.metrics.f2)) {
  if( MacroP.metrics.f2$EQR_LEAFPACS_Capped[n]>0.95) {MacroP.metrics.f2$Bad[n]<-0} else
    if(MacroP.metrics.f2$EQR_LEAFPACS_Capped[n] <= 0.056){MacroP.metrics.f2$Bad[n]  <-88.1} else {MacroP.metrics.f2$Bad[n]  <-round((100*MacroP.metrics.f2$Normdist1[n]),2)}}

for (n in 1:nrow(MacroP.metrics.f2)) {
  if( MacroP.metrics.f2$EQR_LEAFPACS_Capped[n]>0.95) {MacroP.metrics.f2$Poor[n]<-0} else
    if(MacroP.metrics.f2$EQR_LEAFPACS_Capped[n] <= 0.056){MacroP.metrics.f2$Poor[n]  <-9.6} else {MacroP.metrics.f2$Poor[n]  <-round( (100*(MacroP.metrics.f2$Normdist2[n]-MacroP.metrics.f2$Normdist1[n])),1)}}

for (n in 1:nrow(MacroP.metrics.f2)) {
  if( MacroP.metrics.f2$EQR_LEAFPACS_Capped[n]>0.95) {MacroP.metrics.f2$Moderate[n]<-0} else
    if(MacroP.metrics.f2$EQR_LEAFPACS_Capped[n] <= 0.056){MacroP.metrics.f2$Moderate[n]  <-2} else {MacroP.metrics.f2$Moderate[n]  <-round((100*(MacroP.metrics.f2$Normdist3[n]-MacroP.metrics.f2$Normdist2[n])),2)}}

for (n in 1:nrow(MacroP.metrics.f2)) {
  if( MacroP.metrics.f2$EQR_LEAFPACS_Capped[n]>0.95) {MacroP.metrics.f2$Good[n]<-0} else
    if(MacroP.metrics.f2$EQR_LEAFPACS_Capped[n] <= 0.056){MacroP.metrics.f2$Good[n]  <-0.4} else {MacroP.metrics.f2$Good[n]  <-round((100*(MacroP.metrics.f2$Normdist4[n]-MacroP.metrics.f2$Normdist3[n])),2)}}

for (n in 1:nrow(MacroP.metrics.f2)) {
  if( MacroP.metrics.f2$EQR_LEAFPACS_Capped[n]>0.95) {MacroP.metrics.f2$High[n]<-100} else
    if(MacroP.metrics.f2$EQR_LEAFPACS_Capped[n] <= 0.056){MacroP.metrics.f2$High[n]  <-0} else {MacroP.metrics.f2$High[n]  <-round((100*(1-MacroP.metrics.f2$Normdist4[n])),2)}}

LEAFPACS_summary<-  MacroP.metrics.f2 %>% select(SITECODE,SAMPLE_DATE,WATER_BODY,EQR_LEAFPACS,CLASS,Bad,Poor,Moderate,Good,High) %>%
  dplyr::arrange(SITECODE,SAMPLE_DATE)

LEAFmap <- left_join ( LEAFPACS_summary,BioSiteListMP2(), by = "SITECODE")
LEAFmap <- LEAFmap %>% group_by(SITECODE) %>%
  filter(SAMPLE_DATE==max(SAMPLE_DATE)) %>% ungroup()

assign('MacroP.metrics.f2',MacroP.metrics.f2,envir=.GlobalEnv)
assign('LEAFmap',LEAFmap,envir=.GlobalEnv)
output$LEAFPACS_Summary <- renderDataTable(LEAFPACS_summary,rownames= FALSE,options =
                                             list(scrollX = TRUE ,bInfo=F,bPaginate=F,sScrollY='25vh', scrollCollapse =
                                                    TRUE, aoColumnDefs = list(list(sClass="alignright"))))
# DARLEQ_list <- list("INPUTDATA"=diatomres$Job_Summary$MissingTaxa, "EQR"=diatomEQR$EQR,"Uncertainty"=diatomEQR$Uncertainty)
file_name <- c("LEAFPACS_RESULTS")
output$LEAFPACS1 <- downloadHandler(

  ## Copyright (c) 2019, Steve Juggins
  ##
  ## License GPL-2
  ##
  ## Permission is hereby granted to use, copy, modify and distribute the software in accordance with
  ## the GPL-2 license and subject to the following condition:
  ##

  filename = function() {
    paste("LEAFPACS", ".xlsx", sep = "")
  },


  content = function(file) {
    write.xlsx(MacroP.metrics.f2, file, row.names = FALSE)
  }
)

LEAFmapMhigh <- LEAFmap %>% filter(CLASS=="HIGH")
LEAFmapMgood <- LEAFmap %>% filter(CLASS=="GOOD")
LEAFmapMmoderate <- LEAFmap %>% filter(CLASS=="MODERATE")
LEAFmapMpoor <- LEAFmap %>% filter(CLASS=="POOR")
LEAFmapMbad <- LEAFmap %>% filter(CLASS=="BAD")
#################################### RICT Leaflet map output ######################
leafpacscoords <- reactive({SpatialPointsDataFrame(LEAFmap[,c('Longitude', 'Latitude')] , LEAFmap)})
# if(exists("LEAFmapMhigh")) {
assign('LEAFmapMhigh1',LEAFmapMhigh,envir=.GlobalEnv)#}
# if(exists("LEAFmapMgood")) {
assign('LEAFmapMgood1',LEAFmapMgood,envir=.GlobalEnv)#}
# if(exists("LEAFmapMmoderate")) {
assign('LEAFmapMmoderate1',LEAFmapMmoderate,envir=.GlobalEnv)#}
# if(exists("LEAFmapMpoor")) {
assign('LEAFmapMpoor1',LEAFmapMpoor,envir=.GlobalEnv)#}
# if(exists("LEAFmapMbad")) {
assign('LEAFmapMbad1',LEAFmapMbad,envir=.GlobalEnv)#}
if(exists("LEAFmap")) {
  assign('leafpacscoords1',leafpacscoords(),envir=.GlobalEnv)}

output$MacroPmap <- renderLeaflet({

  leaflet(leafpacscoords()) %>%
    addTiles() %>%
    addCircles(data = LEAFmapMhigh,
               radius = 250,
               lat = LEAFmapMhigh$Latitude,
               lng = LEAFmapMhigh$Longitude,
               fillColor = "navy",
               fillOpacity = 1,
               color = "navy",
               weight = 2,
               stroke = T,
               group = "LEAFPAC2 High (Navy)",
               popup = ~paste0(LEAFmapMhigh$SITECODE," ",LEAFmapMhigh$WATER_BODY.x," ",LEAFmapMhigh$SAMPLE_DATE),
               popupOptions = popupOptions(minWidth = 100, closeOnClick = TRUE),
               layerId = as.character(LEAFmapMhigh$SITECODE),
               highlightOptions = highlightOptions(color = "mediumseagreen",
                                                   opacity = 1.0,
                                                   weight = 2,
                                                   bringToFront = TRUE)) %>%
    addCircles(data = LEAFmapMgood,
               radius = 250,
               lat = LEAFmapMgood$Latitude,
               lng = LEAFmapMgood$Longitude,
               fillColor = "green",
               fillOpacity = 1,
               color = "green",
               weight = 2,
               stroke = T,
               group = "LEAFPAC2 Good (Green)",
               popup = ~paste0(LEAFmapMgood$SITECODE," ",LEAFmapMgood$WATER_BODY.x," ",LEAFmapMgood$SAMPLE_DATE),
               popupOptions = popupOptions(minWidth = 100, closeOnClick = TRUE),
               layerId = as.character(LEAFmapMgood$SITECODE),
               highlightOptions = highlightOptions(color = "mediumseagreen",
                                                   opacity = 1.0,
                                                   weight = 2,
                                                   bringToFront = TRUE)) %>%
    addCircles(data = LEAFmapMmoderate,
               radius = 250,
               lat = LEAFmapMmoderate$Latitude,
               lng = LEAFmapMmoderate$Longitude,
               fillColor = "yellow",
               fillOpacity = 1,
               color = "yellow",
               weight = 2,
               stroke = T,
               group = "LEAFPAC2 Moderate (Yellow)",
               popup = ~paste0(LEAFmapMmoderate$SITECODE," ",LEAFmapMmoderate$WATER_BODY.x," ",LEAFmapMmoderate$SAMPLE_DATE),
               popupOptions = popupOptions(minWidth = 100, closeOnClick = TRUE),
               layerId = as.character(LEAFmapMmoderate$SITECODE),
               highlightOptions = highlightOptions(color = "mediumseagreen",
                                                   opacity = 1.0,
                                                   weight = 2,
                                                   bringToFront = TRUE)) %>%
    addCircles(data = LEAFmapMpoor,
               radius = 250,
               lat = LEAFmapMpoor$Latitude,
               lng = LEAFmapMpoor$Longitude,
               fillColor = "orange",
               fillOpacity = 1,
               color = "orange",
               weight = 2,
               stroke = T,
               group = "LEAFPAC2 Poor (Orange)",
               popup = ~paste0(LEAFmapMpoor$SITECODE," ",LEAFmapMpoor$WATER_BODY.x," ",LEAFmapMpoor$SAMPLE_DATE),
               popupOptions = popupOptions(minWidth = 100, closeOnClick = TRUE),
               layerId = as.character(LEAFmapMpoor$SITECODE),
               highlightOptions = highlightOptions(color = "mediumseagreen",
                                                   opacity = 1.0,
                                                   weight = 2,
                                                   bringToFront = TRUE)) %>%
    addCircles(data = LEAFmapMbad,
               radius = 250,
               lat = LEAFmapMbad$Latitude,
               lng = LEAFmapMbad$Longitude,
               fillColor = "red",
               fillOpacity = 1,
               color = "red",
               weight = 2,
               stroke = T,
               group = "LEAFPAC2 Bad (Red)",
               popup = ~paste0(LEAFmapMbad$SITECODE," ",LEAFmapMbad$WATER_BODY.x," ",LEAFmapMbad$SAMPLE_DATE),
               popupOptions = popupOptions(minWidth = 100, closeOnClick = TRUE),
               layerId = as.character(LEAFmapMbad$SITECODE),
               highlightOptions = highlightOptions(color = "mediumseagreen",
                                                   opacity = 1.0,
                                                   weight = 2,
                                                   bringToFront = TRUE)) %>%
    addLayersControl(

      overlayGroups = c("LEAFPAC2 High (Navy)","LEAFPAC2 Good (Green)","LEAFPAC2 Moderate (Yellow)","LEAFPAC2 Poor (Orange)", "LEAFPAC2 Bad (Red)"),
      options = layersControlOptions(collapsed = FALSE)
    )
})


################rainbow plots #################
MacroPrainbowsites <- LEAFmap %>% select(SITECODE,WATER_BODY.x)
output$MPsites2 = DT::renderDataTable(MacroPrainbowsites,rownames= FALSE,colnames = c('SITE_CODE' = 1,'WATERBODY' =2), options =
                                        list(scrollX = TRUE ,bInfo=F,bPaginate=F,sScrollY='25vh', scrollCollapse =
                                               TRUE, aoColumnDefs = list(list(sClass="alignright"))),selection =
                                        list(mode='single', selected=1))


LEAFPACS_Summary2 <- LEAFPACS_summary[,c(1:5)]
MPsitelist2a <- reactive({input$MPsites2_rows_selected })
MPsitelist2 <- reactive({MacroPrainbowsites[MPsitelist2a(),1]})
MacroPPlotData1 <-  LEAFPACS_Summary2 %>% mutate(Bad =0.20) %>% mutate(Poor = 0.40) %>% mutate(Moderate = 0.60) %>% mutate(Good=0.80) %>% mutate(High=1.5)
MacroPPlotData <- reactive({MacroPPlotData1 %>% filter (SITECODE %in% MPsitelist2())})

output$MacroPtest4 <- renderDataTable(MacroPPlotData(), options =
                                        list(scrollX = TRUE ,bInfo=F,bPaginate=F,sScrollY='25vh', scrollCollapse =
                                               TRUE, aoColumnDefs = list(list(sClass="alignright"))))

output$MacroPPlot1 <- renderPlot(
  #  gather(key,value, Bad, Poor,Moderate,Good,High) %>%


  plot( MacroPPlotData()$SAMPLE_DATE , MacroPPlotData()$EQR_LEAFPACS, col="black" , type="b" ,lty = 1, lwd = 1 , xlab="date" , cex = 2.5, ylab="EQI value" , pch=20, ylim = c(0, 1.5),
        main =MPsitelist2() ) +
    # Fill the area
    polygon(
      c(min(MacroPPlotData()$SAMPLE_DATE), MacroPPlotData()$SAMPLE_DATE , max(MacroPPlotData()$SAMPLE_DATE)) ,
      c( 0 , MacroPPlotData()$Bad , 0) ,
      col=adjustcolor("red",alpha.f=0.6) , border=F ) +

    polygon(
      c(min(MacroPPlotData()$SAMPLE_DATE), MacroPPlotData()$SAMPLE_DATE , max(MacroPPlotData()$SAMPLE_DATE)) ,
      c( 0.2 , MacroPPlotData()$Poor , 0.2) ,
      col=adjustcolor("orange",alpha.f=0.6) , border=F ) +

    polygon(
      c(min(MacroPPlotData()$SAMPLE_DATE), MacroPPlotData()$SAMPLE_DATE , max(MacroPPlotData()$SAMPLE_DATE)) ,
      c( 0.4 , MacroPPlotData()$Moderate , 0.4) ,
      col=adjustcolor("yellow",alpha.f=0.6) , border=F ) +

    polygon(
      c(min(MacroPPlotData()$SAMPLE_DATE), MacroPPlotData()$SAMPLE_DATE , max(MacroPPlotData()$SAMPLE_DATE)) ,
      c( 0.6 , MacroPPlotData()$Good , 0.6) ,
      col=adjustcolor("green",alpha.f=0.6) , border=F ) +

    polygon(
      c(min(MacroPPlotData()$SAMPLE_DATE), MacroPPlotData()$SAMPLE_DATE , max(MacroPPlotData()$SAMPLE_DATE)) ,
      c( 0.8, MacroPPlotData()$High , 0.8) ,
      col=adjustcolor("blue",alpha.f=0.6) , border=F )

)


})
