###===================================================
### Make data with 2017 predictor information in it
# Started June 2018, based on analogous script that incorporates
# 2016 data.  Incorporates updated VDEM data covering 2017
# then tries to get the rest of it caught up. 
###===================================================

### AS the prior script worked to fill in missingness in 2016 by any means necessary,
### the same is done here for 2016 and extended to 2017.

### Biggest shortcoming of 2017 data is that 
### popsize, tradeshare, and gdppcgrowth are missing from both 
### VDEM and WDI. 
### So carrying-forward for now from 2016  (which itself is sometimes filled in by carryovers
### or by numbers filled in from other sources, as shown below.

### XXX Check if anything that had been missing in 2016 is now filled in, 
### in the 2017 data. 

### TO SEE ISSUES THAT ARE YET TO BE RESOLVED, FIND "XXX" 

###================================================================================
# Script for construction of new data for 2017/2018 (predictors up through 2016).
# Data sources: 
# 1. Old EWP data are used to seed variable that aren't expected to change over time, from which 
# we carried forward as needed. 
#
# 2. VDEM is the largest data source.
# 
# 3. Polity related data from SystemicPeace - polity2 and pol.durable
#
# 4. WDI, using the R package to grab them from online.  Not available until 1960, and sometimes later. 
# On tradeshare, VDEM often has better good early coverage but misses later years;
# so created a rescaled version of the wdi.tradeshare.ln to match the VDEM one, then 
# used that to fill in where possible when VDEM one is missing.
#
# Likewise with population size -- filled in (mostly later years) of missingness in VDEM version
# with adjusted version of the WDI one.
#
# Likewise with gdppcgrowth.
#
# A note on ELF: Some countries permanently  missing ELF.
# -Yemen.  But it's 93% arab, so 1-.93^2=.135 ought to be very close, if "Arab" is a group at the right level.
# -South Sudan. .35 dinka, .16 Nuer and I don't know the rest, but this bounds it between 0.61 and 0.85;
# the middle of which is .73, which I'll use. 
#
# - For Montenegro, CIA factbook says:  45%, Serbian 28.7%, Bosniak 8.7%, Albanian 4.9%, Muslim 3.3%, Romani 1%, Croat 1%, other 2.6%, unspecified 4.9% 
# So upper bound at 1 - (.45^2 + .29^2 + .09^2 + .05^2 + .03^2 + .01^2) = 0.70
#
# Cutting mev.regac for 2016-based predictions onward -- it's the only variable
# that requires a whole additonal data source, and regenerating it is a many-layered process
# and depends upon things like definitions of regions. 

library(dplyr)
library(readxl)

# 1. Read in VDEM and deal with naming
vdem=read.csv("../data/vdem/V-Dem-CY+Others-v8.csv")

# get SFTGCODE (PITF) to add to VDEM data to allow pulling in the EWP data
source("cowtopitf2018.R")
vdem$sftgcode=cowtopitfit(cowcodedata = vdem$COWcode, yeardata=vdem$year)

# 2. Read in polity, and make some changes
# Fomerly: As I understand (5 June 2018), still only available through 2016.
# Now (15 Oct 2018) --use Monty's shared data for 2017.
polity = read_excel("../data/p4v2017_montyshared.xls")
names(polity)[names(polity)=="scode"]="sftgcode"
polity$sftgcode[polity$sftgcode=="SER"] <- "SRB"
polity$sftgcode[polity$sftgcode=="MNT"] <- "MNE"
polity$sftgcode[polity$sftgcode=="GMY"] <- "GER"
polity$sftgcode[polity$sftgcode=="SSU"] <- "SSD"
polity$sftgcode[polity$sftgcode=="SDN"] <- "SUD"
polity$sftgcode[polity$sftgcode=="USR"] <- "USS"

# 3. Load old EWP data, which will be used for some variables that don't change and 
# as a frame to merge onto.
load("../data/EWP_final_data_31Oct2016.RData")
dat.ewp=dat.retro.isr
rm(dat.retro.isr)

###==================================
#Build up dataset
### Merge EWP and VDEM
dat2=merge(x = vdem, y = dat.ewp, by =c("sftgcode","year"), all.x=T)
rownames(dat2)=paste0(dat2$country_name,dat2$year)

checkcountries=c("Serbia","Eritrea", "North Korea")

#Countries that don't yet exist will have a bunch of NAs, including on reg.afr, for exmample. 
# Remove them:
#View(dat2[dat2$year==1990,c("year","country_name","reg.afr","sftgcode")])

### Merge polity onto that.
dat3=merge(x=dat2, y=polity, by=c("sftgcode","year"), all.x=T)

# Remove small countries that got sftgcode==0
table(dat3$country_name[dat3$sftgcode==0])

#removes Maldives, Palestin/British, Palestine/Gaza, Palestin/west Bank,
# Sao Tome and Principe, Seychelles, Somaliland, Vanuatu, Zanzibar
dat3=dat3[dat3$sftgcode!=0,]

#Write over the old pol.durable and pol.durable.ln with new values from polity,
# Also adding 1 to ln(durable).
dat3$pol.durable = dat3$durable
dat3$pol.durable.ln = log(1+dat3$durable)

table(dat3$country_name[dat3$year==2016])

###=========================================================
### World Development Indicators, at least for now
### Based on Jay's code.
###========================================================
# Load required package
library(WDI)

wdilist <- c("NE.TRD.GNFS.ZS",     # Trade (% of GDP)
             "NY.GDP.PCAP.PP.KD",  # GDP per capita, PPP (constant 2005 intl $)
             "NY.GDP.PCAP.KD",     # GDP per capita (constant 2000 US$)
             "NY.GDP.MKTP.KD.ZG",  # GDP growth (annual %)
             "FP.CPI.TOTL.ZG",     # Inflation, consumer prices (annual %)
            # "FP.CPI.TOTL",        # Consumer price index (2005 = 100) 
             "SP.POP.TOTL"        # Population, total
            # "SP.URB.TOTL.IN.ZS",  # Urban population (% of total)
            # "SP.POP.GROW",        # Population growth (annual %)
            # "EN.POP.DNST",        # Population density (people per sq. km of land area)
            # "SP.POP.0014.TO.ZS",  # Population ages 0-14 (% of total)
            # "MS.MIL.TOTL.P1",     # Armed forces personnel, total
            # "MS.MIL.TOTL.TF.ZS",  # Armed forces personnel (% of total labor force)
            # "NY.ADJ.DFOR.GN.ZS",  # Adjusted savings: forest depletion (% of GNI)
            # "NY.ADJ.DMIN.GN.ZS",  # Adjusted savings: mineral depletion (% of GNI)
            # "NY.ADJ.DNGY.GN.ZS",  # Adjusted savings: energy depletion (% of GNI)
            # "IT.CEL.SETS.P2",     # Mobile cellular subscriptions (per 100 people)                                        
            # "IT.NET.USER.P2",     # Internet users (per 100 people)                                              
            # "SP.DYN.IMRT.IN"     # Infant mortality rate
    )
# Extract latest version of desired variables from WDI from 1945 (from web)
wdi <- WDI(country="all", indicator=wdilist, extra=FALSE, start=1945, end=as.numeric(substr(Sys.Date(),1,4)))

# Add PITF country codes for merging
source("f.pitfcodeit.R")
wdi <- pitfcodeit(wdi, "country")

# In summer 2018, name of Swaziland was updated to eSwatini
wdi$country[wdi$country=="Eswatini"]="Swaziland"
wdi$sftgcode[wdi$country=="Swaziland"]="SWA"

# Subset to drop cases with missing PITF codes, cut extra id vars
wdi <- subset(wdi, !is.na(sftgcode), select=c(length(names(wdi)), 3:(length(names(wdi)) - 1)))

# Rename variables-- add a "new" to indicate these are newly brought in from wdi 
# to avoid reusing names already in the old EWP data
names(wdi) <- c("sftgcode", "year",
                "wdi.trade.new",
                "wdi.gdppcppp.new",
                "wdi.gdppc.new",
                "wdi.gdppcgrow.new",
                "wdi.inflation.new",
                "wdi.popsize.new"
                #"wdi.cpi.new","wdi.popsize.new",
                #"wdi.popurb.new","wdi.popgrow.new","wdi.popdens.new","wdi.pop014.new","wdi.miltot.new","wdi.milpct.new","wdi.forest.new","wdi.minerals.new", "wdi.energy.new","wdi.mobiles.new","wdi.netusers.new","wdi.imrate.new"
                )

# Reorder for easier review
wdi <- wdi[order(wdi$sftgcode, wdi$year),]

# Write it out
write.csv(wdi, file = "wdi_18Oct2018.csv", row.names = FALSE)
#View(wdi)

# Merge it in:
dat4=merge(x=dat3, y=wdi, by=c("sftgcode","year"), all.x=T)

# Rename for official version, dat
dat=dat4
rm(dat2,dat3, dat4)

table(dat$country_name[dat$year==2017])
dat %>% filter(country_name %in% checkcountries, year==2017) %>% select(country_name)

###================================
### PRIO/UCDP 
### -- start with PRIO battle death data to get up  to 2008.
### -- then fill in up to 2017 using UCDP.
###  ...note that using advanced copy of UCDP data for 2017.
###+=============================

prio=read_excel("../data/PRIO Battle Deaths Dataset 3.1.xls")

#Prio type 3 and 4 are what we are concerned with - internal and "internationalized internal" conflicts
prio = filter(prio, type>=4)
table(prio$location)
setdiff(prio$location, dat$country_name)

# Fix county names to match.
prio$location[prio$location=="Democratic Republic of Congo (Zaire)"] = "Democratic Republic of Congo"
prio$location[prio$location=="Congo"] = "Democratic Republic of Congo"
prio$location[prio$location=="Yugoslavia (Serbia)"] = "Serbia"

# Not sure what to do about events coded to North Yemen and South Vietnam.
#Turn into country-year battle deaths
dat$battledeaths = 0
for (j in  1:nrow(prio)){
  (thisyear=prio$year[j] )
  (thiscountry=prio$location[j])
  (battledeaths=prio$bdeadbes[j])
  
  if(battledeaths==-999) battledeaths=prio$bdeadlow[j]
  
  getrows=which(dat$country_name==thiscountry & dat$year==thisyear)
  
  if (length(getrows)!=0) dat$battledeaths[getrows]=battledeaths
}

# Now UCDP battle-related deaths for 2008 onward
# load("../data/ucdp-brd-conf-172.RData") ## From 2016 data version
# The original xlsx seems to behave oddly; so have resaved it as CSV
#ucdp = as.data.frame(read_excel("../data/BattleRelatedDeathsDataset18.11989-01-01-2017-12-31.xlsx"))
ucdp = read.csv("../data/BattleRelatedDeathsDataset18.11989-01-01-2017-12-31.csv")
ucdp = ucdp %>% filter(Year>2008)

# make country names workable; 
# If occured in multiple countries, I take it to be an international conflict
# and thus leave out.  The USA ones are all elsewhere and thus international.

setdiff(unique(ucdp$LocationInc), unique(dat$country_name))
#unique(dat$country_name)[grep(pattern="Russia", unique(dat$country_name))]

ucdp$LocationInc = as.character(ucdp$LocationInc)
ucdp$LocationInc[ucdp$LocationInc=="Myanmar (Burma)"] = "Burma/Myanmar"
ucdp$LocationInc[ucdp$LocationInc=="Yemen (North Yemen)"] = "Yemen"
ucdp$LocationInc[ucdp$LocationInc=="DR Congo (Zaire)"] = "Democratic Republic of Congo"
ucdp$LocationInc[ucdp$LocationInc=="Russia (Soviet Union)"] = "Russia"
ucdp$LocationInc[ucdp$LocationInc=="Congo"] = "Republic of the Congo"

# Now fill into the main dataset:
for (j in  1:nrow(ucdp)){
  (thisyear=ucdp$Year[j] )
  (thiscountry=ucdp$LocationInc[j])
  (battledeaths=ucdp$BdBest[j])
  
  getrows=which(dat$country_name==thiscountry & dat$year==thisyear)
  
  if (length(getrows)!=0) dat$battledeaths[getrows]=battledeaths
}


#dat %>% filter(country_name %in% checkcountries, year==2017) %>% select(country_name)
table(dat$country_name[dat$year==2017])
dat$battledeaths[dat$year==2017]

###========================================
### Wrap-up
###========================================
dat=dat[dat$year>=1945,]
table(dat$year)
table(dat$country_name)

### Select which countries we're really using.  Mostly its the EWP countries, minus 4. 
ewpcountries = unique(dat.ewp$sftgcode)
countriestodrop = unique(dat.ewp$sftgcode[is.na(dat.ewp$mkl.start)])
unique(dat.ewp$country_name[is.na(dat.ewp$mkl.start)])
countriestokeep = setdiff(ewpcountries, countriestodrop)
dat = dat%>%filter(sftgcode %in% countriestokeep)
table(dat$year)

# Weirdly two Sudan 2011's. 
badsudan=min(which(dat$country_name=="Sudan" & dat$year==2011))
dat=dat[-badsudan,]

###===============================
### ADD NEW ONSETS  -- STATE LED
# This is built on code written for predictions made from 2016 predictors. 
# Thus assuming neither new onsets or offsets for 2017 
# This is true for now but check in each future year. XXX
###===============================

table(dat$mkl.start, dat$year)
table(is.na(dat$mkl.start), dat$year)

# For starters, make mkl.ongoing the same in 2016 and 2017 as it was in 2015.
# We will make exceptions from there. Likewise for mkl.ever.
dat = dat %>% group_by(country_name) %>% mutate(
  mkl.ongoing.lag1 = lag(mkl.ongoing, 1, order_by=country_name),
  mkl.ongoing.lag2 = lag(mkl.ongoing, 2, order_by=country_name),
  mkl.ever.lag1 = lag(mkl.ever, 1, order_by=country_name),
  mkl.ever.lag2 = lag(mkl.ever, 2, order_by=country_name))

dat$mkl.ongoing[dat$year==2016] = dat$mkl.ongoing.lag1[dat$year==2016]
dat$mkl.ongoing[dat$year==2017] = dat$mkl.ongoing.lag2[dat$year==2017]
dat$mkl.ever[dat$year==2016] = dat$mkl.ever.lag1[dat$year==2016]
dat$mkl.ever[dat$year==2017] = dat$mkl.ever.lag2[dat$year==2017]

dat$mkl.start[dat$year==2017]=0
dat$mkl.end[dat$year==2017]=0

dat$mkl.start[dat$year==2016]=0
dat$mkl.end[dat$year==2016]=0
dat$mkl.start.1[dat$year==2016]=0
dat$mkl.start.1[dat$year==2015]=0

dat$mkl.start[dat$sftgcode=="IRQ" & dat$year==2014] = 1
dat$mkl.start.1[dat$sftgcode=="IRQ" & dat$year==2013] = 1
dat$mkl.ongoing[dat$sftgcode=="IRQ" & dat$year>=2014] = 1

dat$mkl.start[dat$country_name=="Ethiopia" & dat$year==2015] = 1
dat$mkl.start.1[dat$country_name=="Ethiopia" & dat$year==2014] = 1
dat$mkl.ongoing[dat$country_name=="Ethiopia" & dat$year>=2015] = 1

dat$mkl.start[dat$sftgcode=="PHI" & dat$year==2016] = 1
dat$mkl.start.1[dat$sftgcode=="PHI" & dat$year==2015] = 1
dat$mkl.ongoing[dat$sftgcode=="PHI" & dat$year>=2016] = 1

#Burma starting in 2016
dat$mkl.start[dat$sftgcode=="MYA" & dat$year==2016] = 1
dat$mkl.start.1[dat$sftgcode=="MYA" & dat$year==2015] = 1
dat$mkl.ongoing[dat$sftgcode=="MYA" & dat$year>=2016] = 1

#Egypt 2013 and 2014 then off
dat$mkl.start[dat$sftgcode=="EGY" & dat$year==2013] = 1
dat$mkl.start.1[dat$sftgcode=="EGY" & dat$year==2012] = 1
dat$mkl.end[dat$sftgcode=="EGY" & dat$year==2014] = 1  #we coded as ending jan 1 2015, so I'm coding that to 2014 as its not really ongoing in 2015.
dat$mkl.ongoing[dat$sftgcode=="EGY" & (dat$year==2013 | dat$year==2014)] = 1
dat$mkl.ongoing[dat$sftgcode=="EGY" & dat$year>=2015] = 0  #code as off in 2015.

#check
table(dat$mkl.ongoing[dat$sftgcode=="EGY"], dat$year[dat$sftgcode=="EGY"])

### Update: mkl.ever -- only Egypt is new
dat$mkl.ever[dat$sftgcode=="EGY" & dat$year>=2013] = 1

# Update: mkl.type
# Not going to update as no longer in use.
dat$mkl.type[dat$year==2016] = NA


###========================================
### Create non-state-led MA!
### AGAIN ASSUMING NO NEW INFORMATION REQUIRED FOR PREDICTIONS FROM 
### 2017 DATA. True for now but check in future years. XXX
###========================================
#Initialize to zero
dat$nonstatemk.start=0
dat$nonstatemk.start.1=0
table(dat$nonstatemk.start)
dat$nonstatemk.ongoing=0
dat$nonstatemk.end=0
dat$nonstatemk.ever=0

newcountry=c("Liberia","India","Sierra Leone", "Algeria", "Somalia",
 "Afghanistan", "Georgia", "Republic of the Congo","Rwanda", "Democratic Republic of Congo", 
 "Russia","Nepal","Democratic Republic of Congo", "Democratic Republic of Congo", "Pakistan", 
 "Afghanistan","Iraq", "Thailand", "India", "Somalia", 
 "Democratic Republic of Congo","Nigeria", "Syria", "Central African Republic", "South Sudan")
newyear=c(1989, 1990, 1991, 1991, 1991, 
          1992, 1992, 1992, 1993, 1993, 
          1995, 1996, 1996, 1998, 2001, 
          2001, 2003, 2004, 2004, 2007, 
          2008, 2010, 2012, 2013, 2013)

#endyear, 999 for censoring
endyear = c(1990,2008,2002,2005,1992,
            1996,1993,2003,1994,1993,
            2005,2006,1997,9999,9999,
            9999,9999,9999,9999,9999,
            2012,9999,9999,9999,9999)

length(newcountry)
length(newyear)
length(endyear)

for(j in 1:length(newcountry)){
  thiscountry=newcountry[j]
  print(thiscountry)
  thisyear=newyear[j]
  
  if (endyear[j]<= max(dat$year)) {  
    dat$nonstatemk.end[dat$country_name==thiscountry & dat$year==endyear[j]] = 1
  }
  
  thisend2=min(endyear[j], max(dat$year))  #handles censoring
  dat$nonstatemk.start[dat$country_name== thiscountry & dat$year== thisyear] = 1
  dat$nonstatemk.start.1[dat$country_name==thiscountry & dat$year==(thisyear-1)] = 1
  dat$nonstatemk.ongoing[dat$country_name==thiscountry & dat$year>=thisyear & dat$year<=thisend2] = 1
  dat$nonstatemk.ever[dat$country_name == thiscountry & dat$year>=thisyear] = 1
  
}

table(dat$nonstatemk.start, dat$mkl.start)
table(dat$nonstatemk.ongoing)
table(dat$nonstatemk.ever)

###===========================================================================
### Create combined onset variable

dat$anymk.start=0
dat$anymk.start[dat$mkl.start==1]=1
dat$anymk.start[dat$nonstatemk.start==1]=1
table(dat$mkl.start)
table(dat$anymk.start) # adds 16 onsets.

dat$anymk.start.1=0
dat$anymk.start.1[dat$mkl.start.1==1]=1
dat$anymk.start.1[dat$nonstatemk.start.1==1]=1
table(dat$mkl.start.1)
table(dat$anymk.start.1) # adds 16 predictable onsets too.

dat$anymk.ongoing=0
dat$anymk.ongoing[dat$mkl.ongoing==1]=1
dat$anymk.ongoing[dat$nonstatemk.ongoing==1]=1
table(dat$mkl.ongoing)
table(dat$anymk.ongoing) # adds 63 country years of ongoing

dat$anymk.ever=0
dat$anymk.ever[dat$mkl.ever==1]=1
dat$anymk.ever[dat$nonstatemk.ever==1]=1
table(dat$mkl.ever)
table(dat$anymk.ever) 

### Add extra lead:
# Lead from start.1, because it has some info brought 
# in from 2017. 
dat = dat %>% group_by(country_name) %>% mutate(
  anymk.start.2 = lead(anymk.start.1,1, order_by=country_name),
  anymk.start.3 = lead(anymk.start.1,2, order_by=country_name))

dat=as.data.frame(dat)
dat$anymk.start.2[dat$year==2016] = NA
dat$anymk.start.3[dat$year==2016] = NA
dat$anymk.start.3[dat$year==2015] = NA

dat$anymk.start.1[dat$year==2017] = NA
dat$anymk.start.2[dat$year==2017] = NA
dat$anymk.start.3[dat$year==2017] = NA

dat = dat %>% mutate(anymk.start.2window = as.numeric(anymk.start.1 | anymk.start.2),
                     anymk.start.3window = as.numeric(anymk.start.1 | anymk.start.2 | anymk.start.3))

table(dat$anymk.start.2, dat$year)
table(dat$anymk.start.3, dat$year)

# Make these windows also NA where there is not enough time left
# to code the whole window
dat$anymk.start.2window[dat$year==2016] = NA
dat$anymk.start.3window[dat$year==2016] = NA
dat$anymk.start.3window[dat$year==2015] = NA

# Again adding 2017 analog assuming no new information
dat$anymk.start.2window[dat$year==2017] = NA
dat$anymk.start.3window[dat$year==2017] = NA
dat=as.data.frame(dat)
table(dat$country_name[dat$year==2017])
#View(dat[dat$country_name=="Sudan",c("year", "country_name","anymk.start.2","anymk.start.2window","anymk.start.1", "anymk.start")])

###==============================================================================
#  Some data preparation
###==============================================================================

# Instead of postcw we'll want to know if we're on 1989 or after since the definition
# of the outcome will include non-state MK
dat$includesnonstate=0
dat$includesnonstate[dat$year>=1989]=1

# political killings approved by authorities, created from whether 
# vdem::v2clkill_ord=0 or not.
dat$pol_killing_approved = as.numeric(dat$v2clkill_ord==0)

#free movement or not, for men and women
dat$freemove_men4 = as.numeric(dat$v2cldmovem_ord==4)
dat$freemove_women4 = as.numeric(dat$v2cldmovew_ord==4)

dat$social_inequality = as.numeric(dat$v2clsocgrp_ord==0)
dat$even_civilrights = as.numeric(dat$v2clrgunev_ord==2)

dat$repress_civilsoc = as.numeric(dat$v2csreprss_ord==0)

dat$social_power_dist = as.numeric(is.element(el=dat$v2pepwrsoc_ord, set=c(0,1,2)))
# a "1" on this means power is "monopolized" by groups

# What we had planned to use:
usefulvars=c("anymk.start","anymk.start.1","mkl.start","mkl.start.1","anymk.ongoing","anymk.ever",
             "reg.afr", "reg.eap", "reg.eur", "reg.mna", "reg.sca",
             "countryage.ln", "wdi.popsize.ln", "imr.normed.ln", "gdppcgrow.sr",
             "wdi.trade.plus1ln", "ios.iccpr1",
             "postcw","pol.cat.fl1","pol.cat.fl2","pol.cat.fl3","pol.cat.fl7",
             "pol.durable.ln","elf.ethnic","cou.tries5d", "elc.eleth2",
             "v2elrstrct", "v2psparban_ord","v2psoppaut_ord","v2jureform_ord",
             "v2clrelig_ord", "pol_killing_approved",
             "freemove_men4","freemove_women4", "v2xcl_disc",
             "social_inequality","even_civilrights","repress_civilsoc",
             "social_power_dist","battledeaths",
             #"mev.regac.ln",
             "cou.tries.5d")  #these two right now only until 2015 (from EWP)...fine for training set,

# All of the variables from EWP have to be replaced, carried forward, or dropped. 
usefulvars[usefulvars %in% colnames(dat.ewp)]
#"reg.afr"        "reg.eap"       
#"reg.eur"        "reg.mna"        "reg.sca"       
# "countryage.ln"  "wdi.popsize.ln" "imr.normed.ln" 
# "gdppcgrow.sr"   "ios.iccpr1"     "postcw"        
# "pol.cat.fl1"    "pol.cat.fl2"    "pol.cat.fl3"   
# "pol.cat.fl7"    "pol.durable.ln" "elf.ethnic"    
# "cou.tries5d"    "elc.eleth2" 
# Also: wdi.trade.plus1ln -- stops in 2014 and has good bit of missingness.

### Deal with coup attempts in prior 5 years
### Check Powell and Thyne for latest updates which tend to be current.
# In 2017, any coup attempts? Zimbabwe. 
# Re-read Powell and Thyne in futue. 
dat$cou.s.d[dat$year==2017]=0
dat$cou.f.d[dat$year==2017]=0

dat$cou.s.d[dat$year==2017 & dat$country_name=="Zimbabwe"]=1
dat$cou.f.d[dat$year==2017]=0

# In 2016 only known coup attempt is Turkey (failed)
dat$cou.s.d[dat$year==2016]=0
dat$cou.f.d[dat$year==2016]=0
dat$cou.f.d[dat$year==2016 & dat$country_name=="Turkey"]=1

dat$cou.any = ifelse(dat$cou.s.d>0 | dat$cou.f.d>0, 1, 0)

# Construct indicator for coup attempt in prior five years.
# Construct separately from cou.tries5d
dat = dat %>% group_by(country_name) %>% mutate(
  cou.lag1 = lag(cou.any,1, order_by=country_name),
  cou.lag2 = lag(cou.any,2, order_by=country_name),
  cou.lag3 = lag(cou.any,3, order_by=country_name),
  cou.lag4 = lag(cou.any,4, order_by=country_name))

dat$coup.try.5yr = 0
dat$coup.try.5yr[dat$cou.any | dat$cou.lag1==1 | dat$cou.lag2==1 | dat$cou.lag3==1 | dat$cou.lag4==1 ] = 1

### First the carry-forwards:
###===============================================================================
### Carry forward the variables that don't change over time or change predicatbly
###===============================================================================
# stay the same: "reg.afr", "reg.eap", "reg.eur", "reg.mna", "reg.sca",
# advance in simple ways = "countryage", "countryage_ln"
# usually carry forward, and we can make exceptions later:  mkl.ongoing, mkl.ever
# Also carry-forward:
# infant mortality -- e_peinfmor -- stops in 2015, so could use then carry forward last year.
# ios.iccpr1 -- first optional protocol signatory -- can be carried forward pretty safely -- 
# "elf.ethnic" -- constant over time, so just copy forward.

### This loop just carries forward values to 2016 for select variables,
### and select years.
uniquecountries = unique(dat$country_name)

for (j in 1: length(uniquecountries)){
  thiscountry=uniquecountries[j]
  
  #These get carried-forward "permanently"
  dat$reg.afr[dat$country_name==thiscountry & dat$year>2015] = dat$reg.afr[dat$country_name==thiscountry & dat$year==2015]
  dat$reg.eap[dat$country_name==thiscountry & dat$year>2015] = dat$reg.eap[dat$country_name==thiscountry & dat$year==2015]
  dat$reg.eur[dat$country_name==thiscountry & dat$year>2015] = dat$reg.eur[dat$country_name==thiscountry & dat$year==2015]
  dat$reg.mna[dat$country_name==thiscountry & dat$year>2015] = dat$reg.mna[dat$country_name==thiscountry & dat$year==2015]
  dat$reg.sca[dat$country_name==thiscountry & dat$year>2015] = dat$reg.sca[dat$country_name==thiscountry & dat$year==2015]
  
  # ios.iccpr1 -- first optional protocol signatory -- can be carried forward unless someone leaves
  dat$ios.iccpr1[dat$country_name==thiscountry & dat$year>2015] = dat$ios.iccpr1[dat$country_name==thiscountry & dat$year==2015]
  
  # "elf.ethnic" -- constant over time, so just copy forward.
  dat$elf.ethnic[dat$country_name==thiscountry & dat$year>2015] = dat$elf.ethnic[dat$country_name==thiscountry & dat$year==2015]
  
  # countryage grows by one each year, except where there are new countries.
  dat$countryage[dat$country_name==thiscountry & dat$year==2016] = dat$countryage[dat$country_name==thiscountry & dat$year==2015]+1
  dat$countryage[dat$country_name==thiscountry & dat$year==2017] = dat$countryage[dat$country_name==thiscountry & dat$year==2016]+1
  
  #infant mortality using wdi.imrate -- as of now (June 2018) still stops in 2015. 
  # So carry through to future from there.
  dat$wdi.imrate[dat$country_name==thiscountry & dat$year>2015] = dat$wdi.imrate[dat$country_name==thiscountry & dat$year==2015]

  # pol.durable not available in 2017 yet because in polity, which only goes to 2016
  # DISABLING THIS 15 Oct 2017 given new polity data. 
  # dat$pol.durable[dat$country_name==thiscountry & dat$year==2017] = dat$pol.durable[dat$country_name==thiscountry & dat$year==2016]
  
  # Likewise, polity2 not available in 2017, so carrying forward
  # DISABLING THIS 15 Oct 2017 given new polity data. 
  # dat$polity2[dat$country_name==thiscountry & dat$year==2017] = dat$polity2[dat$country_name==thiscountry & dat$year==2016]
  
  # Cutting mev.regac for 2016-based predictions onward -- it's the only variable
  # that requires a whole additonal data source, and regenerating it is a many-layered process
  # and depends upon things like definitions of regions. 
  # dat$mev.regac.ln[dat$country_name==thiscountry & dat$year==2016] = dat$mev.regac.ln[dat$country_name==thiscountry & dat$year==2015]
}

#
# Remake countryage.ln
dat$countryage.ln=log(dat$countryage+1)
# Remake pol.durable.ln
dat$pol.durable.ln=log(dat$pol.durable+1)
dat$durable.ln = dat$pol.durable.ln  # for no good reason, we have this with a different name.

dat=as.data.frame(dat)  #get rid of tidyverse classing which causes problems. 

# Check in:
setdiff(usefulvars, names(dat))
View(dat[dat$year==2017, usefulvars[usefulvars %in% names(dat)]])

### Any endings to adjust (because carried ongoing in 2015 over into 2016)
## No. Egypt ended but was not ongoing in 2015 as we coded, so not carried over.

### Replacements =====================================
### What's left to deal with:
# "wdi.popsize.ln" 
# "gdppcgrow.sr"         
# "pol.cat.fl1"    "pol.cat.fl2"    "pol.cat.fl3"   
# "pol.cat.fl7"    "pol.durable.ln"    
# "cou.tries5d"    "elc.eleth2" 


# Infant mortality: previously used VDEM version, e_peinfmor.
# However WDI is the same where both are present, and has less missingness. 
dat$imr.sqrt = sqrt(dat$wdi.imrate)


# That leaves:  "wdi.popsize.ln" ,"gdppcgrow.sr"  and elc.eleth2.
# For gdppcgrow, replace with VDEM e_migdpgro
# For wdi.popsize.ln, replace with VDEM e_mipopula and its log.
dat$gdppcgrowth = dat$e_migdpgro
dat$popsize = dat$e_mipopula * 1000   #apparently now e_mipopula is in 1000s.
dat$popsize.ln = log(dat$popsize)

#
# For the polity related variables --  "pol.cat.fl1"    "pol.cat.fl2"    "pol.cat.fl3"   
# "pol.cat.fl7"    "pol.durable.ln"  -- we can get from latest polity update, or we could 
# replace with e_v2x_regime in VDEM which is a 0/1/2/3 measure of autocracy to democracy, 
# but we'd still need durable from polity IV.  I propose getting from polity IV, but using
# polity2 score rather than cutting into the 1/2/3/7 categories.

# Next, recreate trade as portion of gdp from data in VDEM. 

# the following is missing in 2017 entirely:
dat$tradeshare = 10^6*(dat$e_cow_exports+dat$e_cow_imports)/(dat$e_migdppc*dat$popsize)

# worse, 2017 also missing from WDI.
dat$tradeshare.ln=log(dat$tradeshare)  # has reasonable distribution actually.
#plot(density(dat$tradeshare.ln, na.rm=TRUE), ylim=c(0,1), xlim=c(-10,10))
dat$wdi.trade.ln.new = log(dat$wdi.trade.new)
#lines(density(dat$wdi.trade.ln.new, na.rm=TRUE))
cor(dat$tradeshare.ln, dat$wdi.trade.ln.new, use="complete.obs")

#Unfortunately a lot of missingness. Let's look, and compare to wdi.trade.new
#View(dat[,c("country_name","sftgcode","year","wdi.trade.ln.new","tradeshare.ln")])

# Make and adjusted version of the wdi one to fit the tradeshare (VDEM) one:
lm.adjust.trade = lm(tradeshare.ln~wdi.trade.ln.new, data=dat)

dat$wdi.trade.ln.new.adj=lm.adjust.trade$coefficients[1] + lm.adjust.trade$coefficients[2]*dat$wdi.trade.ln.new

#Where tradeshare is missing, replace with the adjusted wdi.trade.ln.new
dat$tradeshare.ln.combined = dat$tradeshare.ln
dat$tradeshare.ln.combined[is.na(dat$tradeshare.ln)]=dat$wdi.trade.ln.new.adj[is.na(dat$tradeshare.ln)]
sum(is.na(dat$tradeshare.ln))
sum(is.na(dat$tradeshare.ln.combined)) #Cuts missingness a good bit


# Some missingness on popsize.ln -- let's take a look and again consider filling in with WDI. 
#View(dat[,c("country_name","sftgcode","year","wdi.popsize.new","popsize")])

# Similarly, popsize is missing from both VDEM and WDI.

# Make and adjusted version of the wdi one to fit the tradeshare (VDEM) one:
lm.adjust.popsize = lm(popsize~wdi.popsize.new, data=dat)
dat$wdi.popsize.new.adj=lm.adjust.popsize$coefficients[1] + lm.adjust.popsize$coefficients[2]*dat$wdi.popsize.new

#Where popsize is missing, replace with the adjusted wdi.trade.ln.new
dat$popsize.combined = dat$popsize
dat$popsize.combined[is.na(dat$popsize)]=dat$wdi.popsize.new.adj[is.na(dat$popsize)]
sum(is.na(dat$popsize))
sum(is.na(dat$popsize.combined)) #Cuts most of missingness

dat$popsize.ln.combined = log(dat$popsize.combined)

# And gdpgrowth is problematic too:
#View(dat[,c("country_name","sftgcode","year","wdi.gdppcgrow.new","gdppcgrowth")])

# The gdp growth figures from VDEM and WDI aren't as correlated as you'd like, but
# again WDI is missing before 1960 while the VDEM ones typically stop early.
with(dat, cor(wdi.gdppcgrow.new, gdppcgrowth, use="complete.obs"))
# Make and adjusted version of the wdi one to fit the tradeshare (VDEM) one:
lm.adjust.gdppcgrowth = lm(gdppcgrowth~wdi.gdppcgrow.new, data=dat)
dat$wdi.gdppcgrow.new.adj=lm.adjust.gdppcgrowth$coefficients[1] + lm.adjust.gdppcgrowth$coefficients[2]*dat$wdi.gdppcgrow.new

#Where gdp missing from vdem, replace with WDI.
dat$gdppcgrowth.combined = dat$gdppcgrowth
dat$gdppcgrowth.combined[is.na(dat$gdppcgrowth)]=dat$wdi.gdppcgrow.new.adj[is.na(dat$gdppcgrowth)]
sum(is.na(dat$gdppcgrowth))
sum(is.na(dat$gdppcgrowth.combined)) #Cuts most of missingness

# Replace elc_eleth2 with a minority rule variable from VDEM. Though see note at top of the document. 
# -- we may want something else here. 

dat$minorityrule = 0
dat$minorityrule[dat$v2pepwrses_ord<=1]=1

### Some renaming to make things easier to remember:
names(dat)[names(dat) == 'v2elrstrct'] <- 'candidaterestriction'
names(dat)[names(dat) == 'v2psparban_ord'] <- 'partyban'
names(dat)[names(dat) == 'v2psoppaut_ord'] <- 'barrierstoparties'
names(dat)[names(dat) == 'v2jureform_ord'] <- 'judicialreform'
names(dat)[names(dat) == 'v2clrelig_ord'] <- 'religiousfreedom'
names(dat)[names(dat) == 'v2xcl_disc'] <- 'freediscussion'

## 
dat$battledeaths.ln=log(dat$battledeaths+1)

###===================================================
### Final adjustments
###====================================================

# ELF is missing permanently for South Sudan and Yemen.  See notes at top
# for approximations added here. 
dat[dat$country_name=="South Sudan","elf.ethnic"] = 0.73
dat[dat$country_name=="Yemen","elf.ethnic"] = 0.135


# Fearon & Laitin regime type (autocracy, anocracy, democracy)
dat$polity2.fl=NA
dat$polity2.fl[!is.na(dat$polity2)]=0
dat$polity2.fl.1=dat$polity2.fl.2=dat$polity2.fl.3=dat$polity2.fl.7=dat$polity2.fl

dat$polity2.fl[dat$polity2 >= -10 & dat$polity2 < -5] <- 1  # Autocracy
dat$polity2.fl[dat$polity2 >= -5 & dat$polity2 <= 5] <- 2  # Anocracy
dat$polity2.fl[dat$polity2 > 5] <- 3  # Democracy
#dat$polity2.fl[dat$polity2 == -66 | dat$polity2 == -77 | dat$polity2 == -88 ] <- 7  # Other
# There are no -66, -77, -88 in polity2.

dat$polity2.fl.1[dat$polity2.fl==1]=1
dat$polity2.fl.2[dat$polity2.fl==2]=1
dat$polity2.fl.3[dat$polity2.fl==3]=1
#dat$polity2.fl.7[dat$polity2.fl==7]=1

dat$polity2_sq = dat$polity2^2

###===============================
### That leaves us with:
###================================

woulduse = c("anymk.start","anymk.start.1","mkl.start","mkl.start.1","mkl.ongoing","mkl.ever",
             "anymk.ongoing", "anymk.ever",
                        "reg.afr", "reg.eap", "reg.eur", "reg.mna", "reg.sca",
                        "countryage.ln", "popsize.ln.combined", "imr.sqrt", "gdppcgrowth.combined",
                        "tradeshare.ln.combined", "ios.iccpr1",
                        "includesnonstate","polity2", "battledeaths.ln",
                        "durable.ln","minorityrule", "elf.ethnic",
                        "candidaterestriction", "partyban","barrierstoparties","judicialreform",
                        "religiousfreedom", "pol_killing_approved",
                        "freemove_men4","freemove_women4", "freediscussion",
                        "social_inequality","even_civilrights","repress_civilsoc",
                        "social_power_dist", "coup.try.5yr")
dropfornow=c()
#dropfornow = c("tradeshare.ln","gdpgrowth")
actualuse = setdiff(woulduse, dropfornow)
setdiff(actualuse, names(dat))




###======================================================================
### Examine missingness
###====================================================================
View(dat[dat$year>=2016, woulduse])
dim(dat)
dat.old=dat #just so we can get back to here if needed without re-running.

dat.check = dat[dat$year==2017,c("country_name",actualuse)]
par(las=2) # make label text perpendicular to axis
par(mar=c(5,8,4,2)) # increase y-axis margin.
barplot(horiz=TRUE, sapply(X = names(dat.check), function(x) sum(is.na(dat.check[,x]))))

#check missingness on elf:
dat.check$country_name[is.na(dat.check$elf.ethnic)]

# Tradeshare.ln.combined is still the biggest trouble maker.

###================================
### Fix missingness in 2016 and 2017
### Idea is that we can't fix all missingness, but at least 
### want to fill in where we are making predictions from.
###================================

# First, we are only looking at countries over .5 million in pop
# (Changed from 1 million, 25 May 2018)

dat$country_name[is.na(dat$popsize.combined) & dat$year==2016]

### We need population data for Eritrea, North Korea

### For now, let those with population = NA thgrough. 
dat = filter(dat, popsize.combined>.5e6 | is.na(popsize.combined))
#  dat %>% filter(country_name %in% checkcountries, year>=2011) %>% select(country_name, year, popsize.combined) %>% View()


# Look at key variables to hunt for missingness
outcomename="anymk.start.1"
predictornames = c("mkl.ongoing","mkl.ever",
                   "reg.afr", "reg.eap", "reg.eur", "reg.mna", "reg.sca", #americas, "reg.amr" left out as baseline.
                   "countryage.ln", "popsize.ln.combined", "imr.sqrt", "gdppcgrowth.combined",
                   "ios.iccpr1","includesnonstate",
                   #"polity2", #"polity2_sq",
                   "durable.ln","minorityrule", "elf.ethnic", "battledeaths.ln",
                   "candidaterestriction", "partyban","judicialreform",
                   "religiousfreedom", "pol_killing_approved",
                   "freemove_men4","freemove_women4", "freediscussion",
                   "social_inequality","even_civilrights","repress_civilsoc","social_power_dist", 
                   #"elc.eleth2",  #testing to see if it helps given "minorityrule" from vdem
                   #"barrierstoparties",
                   "tradeshare.ln.combined",   #consider dropping due to missingness
                   "coup.try.5yr",
                   #"mev.regac.ln",
                   #"polity2.fl.1",
                   "polity2.fl.2","polity2.fl.3")  #these lasts two currently as carry-forwards

yX=dat[,c(outcomename,predictornames, "country_name", "year")]
yX2016 = filter(yX, year==2016)
yX2017 = filter(yX, year==2017)
View(yX2017)
(missing = apply(yX2016, 2, function(x) sum(is.na(x))))
(somemissing = names(missing[missing>0]))

#See variables with missingness in 2016, and 2015 data to see if we can carry forward
#View(yX %>% filter(year>=2015) %>% select(country_name, year, somemissing))

# Afhganistan missingness in 2017
dat[dat$country_name=="Afghanistan" & (dat$year==2017), "tradeshare.ln.combined"]=
dat[dat$country_name=="Afghanistan" & (dat$year==2016), "tradeshare.ln.combined"]

#Austria missing on repress civilsoc in all years — fill with "no". 
dat[dat$country_name=="Austria", "repress_civilsoc"] = 0

#Cuba  — missing gdppcgrowth in 2016 and 2017, present in 2015. 
#dat[dat$country_name=="Cuba" & dat$year==2015, "gdppcgrowth.combined"]
# CIA factbook says -0.9% in 2016. Take that, carry it for 2017. 
dat[dat$country_name=="Cuba" & (dat$year==2016 | dat$year==2017), "gdppcgrowth.combined"]= -0.009
  
#Cuba missing tradeshare.ln.combined in 2016 and 2017, present in 2015. 
# Carry forward to both.
dat[dat$country_name=="Cuba" & (dat$year==2016 | dat$year==2017), "tradeshare.ln.combined"]= 
  dat[dat$country_name=="Cuba" & dat$year==2015, "tradeshare.ln.combined"]

#Burkina Faso missing tradeshare.ln.combined in 2016 and 2017, present in 2015
#Carry forward to both.
dat[dat$country_name=="Burkina Faso" & (dat$year==2016 | dat$year==2017), "tradeshare.ln.combined"]= 
dat[dat$country_name=="Burkina Faso" & dat$year==2015, "tradeshare.ln.combined"]

#Bosnia and Herzegovina missing polity in both years - last available showed
# polity2.fl.2 = 1 in 1992-1994.  Let's keep it there (throughout all years), but make note. 
# Oct 15: NOTE THAT THIS IS STILL MISSING Polity in Monty's shared 2017 polity data.
dat[dat$country_name=="Bosnia and Herzegovina", c("polity2.fl.2")] = 1
dat[dat$country_name=="Bosnia and Herzegovina", c("polity2.fl.3") ] = 0
dat[dat$country_name=="Bosnia and Herzegovina", c("polity2.fl.2") ] = 0

#Djibouti missing gdppcgrowth.combined in 2016 and 2017. 
# CIA factbook has estimates for both. 
#, present in 2015;  
dat[dat$country_name=="Djibouti" & dat$year==2016, 
    "gdppcgrowth.combined"]= 0.065
dat[dat$country_name=="Djibouti" & dat$year==2017, 
    "gdppcgrowth.combined"]= 0.07

#Timor-leste missing elf.ethnic in both years.  
# Haven't found a good source elsewhere.  The largest ethnic group 
# I can find evidence of is 100,000, out of a population of about 1 million
# That bounds the ELF to be above 0.9.  So I'll take that. 
dat[dat$country_name=="Timor-Leste", "elf.ethnic" ] = .9

#Timor-Leste missing tradeshare in 2017 (no longer missing in 2016 though!)
dat[dat$country_name=="Timor-Leste" & dat$year==2017, "tradeshare.ln.combined"]= 
dat[dat$country_name=="Timor-Leste" & dat$year==2016, "tradeshare.ln.combined"]

#Haiti missing polity in 2016 and 2017
# Now filled in with updated polity data.
#dat[dat$country_name=="Haiti" & (dat$year==2016 | dat$year==2017), "polity2.fl.2"] = 1  # Value in 2015
#dat[dat$country_name=="Haiti" & (dat$year==2016 | dat$year==2017), "polity2.fl.1"] = 0
#dat[dat$country_name=="Haiti" & (dat$year==2016 | dat$year==2017), "polity2.fl.3"] = 0

#Jordan missing tradeshare since 2010. But stable enough before then
# so let's carry forward.
dat[dat$country_name=="Jordan" & dat$year>=2010, "tradeshare.ln.combined"] = 
dat[dat$country_name=="Jordan" & dat$year==2009, "tradeshare.ln.combined"] 

#Nigeria missing tradeshare in 2017, but now available in 2016.
dat[dat$country_name=="Nigeria" & dat$year==2017, "tradeshare.ln.combined"]= 
dat[dat$country_name=="Nigeria" & dat$year==2016, "tradeshare.ln.combined"]

# South Sudan missing tradeshare in 2017 but not available in 2016.

dat[dat$country_name=="South Sudan" & dat$year==2017, "tradeshare.ln.combined"]= 
dat[dat$country_name=="South Sudan" & dat$year==2016, "tradeshare.ln.combined"]

# South Sudan also missing gdppcgrowth in 2017. CIA factbook shows contradiction
  # between growth rate (in overall real GDP) and the change in GDPpc. 
  # Going with the more conservative value. 
  
dat[dat$country_name=="South Sudan" & dat$year==2017, "gdppcgrowth.combined"]= -.0065
  
#Oman missing Gdppcgrowth in 2017. CIA factbook says 0.
dat[dat$country_name=="Oman" & dat$year==2017, "gdppcgrowth.combined"]= 0

#Yemen missing Gdppcgrowth in 2017. CIA factbook:
dat[dat$country_name=="Yemen" & dat$year==2017, "gdppcgrowth.combined"]= (2300-2400)/2400

# Trinidad and Tobago missing tradeshare in 2016 and 2017.
dat[dat$country_name=="Trinidad and Tobago" & (dat$year==2016), "tradeshare.ln.combined"]= 
  log((8.71+9.49)/20.3)

dat[dat$country_name=="Trinidad and Tobago" & (dat$year==2017), "tradeshare.ln.combined"]= 
  log((10.2+9.67)/20.3)

# Serbia missing elf.ethnic in both. Missing for all of time actually. 
# Fearon does not give, but Alesina gives a 0.57 on "ethnic fractionalization", so let's go with that.
dat[dat$country_name == "Serbia", "elf.ethnic"] = 0.57

#Libya — missing in 2016 and 2017 but now available up to 2015.
dat[dat$country_name=="Libya" & (dat$year==2016 | dat$year==2017), "tradeshare.ln.combined"] = 
  dat[dat$country_name=="Libya" & (dat$year==2015), "tradeshare.ln.combined"] 
  
  
#PNG — missing tradeshare since at least 2005 now (used to be available later).
# Fill in up to 2015 from the 2005 value
# Then fill in 2016 and 2017 from CIA factbook based estimates.
dat[dat$country_name=="Papua New Guinea" & dat$year>=2006, "tradeshare.ln.combined"] =
  dat[dat$country_name=="Papua New Guinea" & dat$year==2005, "tradeshare.ln.combined"] 

dat[dat$country_name=="Papua New Guinea" & dat$year==2016, "tradeshare.ln.combined"] = 
  log((9.22 + 2.27)/29.92)
dat[dat$country_name=="Papua New Guinea" & dat$year==2017, "tradeshare.ln.combined"] = log((9.52 + 1.88)/21.8)

#Somalia — missing gdppcgrowth since at least 2012
# no per capita estimates available; using gdp growth rate (i.e. assuming roughly
# stable population)
dat[dat$country_name=="Somalia" & dat$year==2016, "gdppcgrowth.combined"] = 0.032
dat[dat$country_name=="Somalia" & dat$year==2017, "gdppcgrowth.combined"] = 0.024

#Syria — missing both gdppcgrowth and tradeshare since at least 2012
dat[dat$country_name=="Syria" & (dat$year==2016 | dat$year==2017), "tradeshare.ln.combined"] = 
  log((1.71 + 5.50)/24.6)
#gdppcgrowth data from 2014 vs. 2015 is last available. 
#I will assume continued deterioration at that rate. 
dat[dat$country_name=="Syria" & dat$year==2016, "gdppcgrowth.combined"] = (2900-3300)/2900
dat[dat$country_name=="Syria" & dat$year==2017, "gdppcgrowth.combined"] = (2900-3300)/2900

#Turkmenistan — missing tradeshare since 2013
dat[dat$country_name=="Turkmenistan" & (dat$year==2016 | dat$year==2017), "tradeshare.ln.combined"] = 
  log((6.99 + 5.00)/41.67)

#Venezuela — missing gdppcgrowth and tradeshare in 2015, 2016, but both present in 2014.
dat[dat$country_name=="Venezuela" & (dat$year==2016 | dat$year==2017), "tradeshare.ln.combined"] = 
  log((27.2 + 20.19)/215.3)

# fill in gdppcgrowth from CIA factbook
dat[dat$country_name=="Venezuela" & dat$year==2016, "gdppcgrowth.combined"] = (14300-17300)/17300
dat[dat$country_name=="Venezuela" & dat$year==2017, "gdppcgrowth.combined"] = (12400-14300)/14300

# North Korea 
# population, from UNDESA
dat[dat$country_name=="North Korea" & (dat$year==2016 | dat$year==2017), "popsize.ln.combined"] = log(25.37e6)

# gdppcgrowth.combined -- not available from factbook. So carrying forward. 
dat[dat$country_name=="North Korea" & (dat$year==2017 | dat$year==2016), "gdppcgrowth.combined"] = 
dat[dat$country_name=="North Korea" & dat$year==2015, "gdppcgrowth.combined"]

# tradeshare.ln.combined -- can get for 2015 and 2016 from factbook. Carry forward for 2017.
# (except that denominator comes from 2013 only!)

dat[dat$country_name=="North Korea" & dat$year==2016, "tradeshare.ln.combined"] = log((3.75+2.99)/28)

dat[dat$country_name=="North Korea" & dat$year==2015, "tradeshare.ln.combined"] = log((3.71+2.91)/28)

dat[dat$country_name=="North Korea" & dat$year==2017, "tradeshare.ln.combined"] = 
  dat[dat$country_name=="North Korea" & dat$year==2016, "tradeshare.ln.combined"]


# Eritrea
# population, from CIA factbook, for 2017 actually, used in both 2016 and 2017.
dat %>% filter(country_name=="Eritrea" & year==2016) %>% select(predictornames)

dat[dat$country_name=="Eritrea" & (dat$year==2016 | dat$year==2017), "popsize.combined"] = 5.92e6
dat[dat$country_name=="Eritrea" & (dat$year==2016 | dat$year==2017), "popsize.ln.combined"] = log(5.92e6)

# gdppcgrowth.ln.combined, using CIA Factbook, which just gives flat estimates in 2015-2017
dat[dat$country_name=="Eritrea" & dat$year==2016, "gdppcgrowth.combined"] = 0
dat[dat$country_name=="Eritrea" & dat$year==2017, "gdppcgrowth.combined"] = 0

dat[dat$country_name=="Eritrea" & dat$year==2016, "tradeshare.ln.combined"] = 
  log((485 + 1049 )/6050)

dat[dat$country_name=="Eritrea" & dat$year==2017, "tradeshare.ln.combined"] = 
  log((636 + 1127 )/6050)

# Montenegro -- just missing ELF; see note at top for approximation
dat[dat$country_name=="Montenegro", "elf.ethnic"] = 0.70
  
# Solomon Islands -- needs tradeshare.ln.combined
# Using CIA factbook data but appears to be buggy -- import and export numbers the same,
# and lists 2015 twice rather than 2016 and 2015 (for both, with identical numbers.)
dat %>% filter(country_name=="Solomon Islands" & year==2016) %>% select(predictornames)
dat %>% filter(country_name=="Solomon Islands" & year==2017) %>% select(predictornames)

dat[dat$country_name=="Solomon Islands" & (dat$year==2016 | dat$year==2017), "tradeshare.ln.combined"] = 
  log((.4199 + .4199 )/1.273)

# RE-view:
yX=dat[,c(outcomename,predictornames, "country_name", "year")]
yX2016 = filter(yX, year==2016)
yX2017 = filter(yX, year==2017)

(missing = apply(yX2017, 2, function(x) sum(is.na(x))))
(somemissing = names(missing[missing>0]))
View(dat[dat$year>=2016, c("year","country_name",somemissing)])

checkvars=somemissing[-1]
View(yX2017[!complete.cases(yX2017[,c("country_name",checkvars)]),])

View(yX2016[!complete.cases(yX2016[,c("country_name",checkvars)]),])

### North Korea is missing infant mortality in all years.  Factbook puts it at
# 22 per 1000. Use that in all years
#dat[dat$country_name=="North Korea","imr.sqrt"]=22/1000  #MISTAKENLY DID THIS PRIOR TO Sept 3 2018,-- should be just 22
dat[dat$country_name=="North Korea","imr.sqrt"]=22  #MISTAKENLY DID THIS PRIOR TO Sept 3 2018,-- should be just 22


# Biggest problem is missing on  tradeshare.ln.combined in 23 countries. 
# Carry forward. 
uniquecountries=unique(dat[dat$year==2017, "country_name"])
for (j in 1: length(uniquecountries)){
  thiscountry=uniquecountries[j]
  if (is.na(dat$tradeshare.ln.combined[dat$country_name==thiscountry & dat$year==2017])){
    dat$tradeshare.ln.combined[dat$country_name==thiscountry & dat$year==2017]=dat$tradeshare.ln.combined[dat$country_name==thiscountry & dat$year==2016]
  }
}

### NOTES: Taiwan missing tradeshare all years. But it is missing a lot, because
### not in some datasets. So let Taiwan be missing.

# Candidaterestriction missing from two
dat$candidaterestriction[dat$year==2017 & dat$country_name=="Timor-Leste"] = dat$candidaterestriction[dat$year==2016 & dat$country_name=="Timor-Leste"]
dat$candidaterestriction[dat$year==2017 & dat$country_name=="Sudan"] = dat$candidaterestriction[dat$year==2016 & dat$country_name=="Sudan"]

# Bahrain was missing Candidaterestriction 2013-2016.  Was 1 before that and 0 after. 
# For 2016 I will go with the 2017 value, 0.
dat$candidaterestriction[dat$year==2016 & dat$country_name=="Bahrain"] =0

#UAE is missing candidaterestriction in 2013-2016 but is otherwise "1".
dat$candidaterestriction[dat$year>=2001 & dat$country_name=="United Arab Emirates"]=1

a=dat[dat$year==2016, c("country_name",predictornames)]
a.clean=na.omit(a)
(stillmissing=setdiff(a$country_name, a.clean$country_name))

#save(dat, file = "prepared2017predictors_20July2018.RData")
save(dat, file = "prepared2017predictors_15Oct2018.RData")


### FOR FUTURE REFERENCE, pol.cat.fl variables come from:
### Polity IV (pol)
# Fearon & Laitin regime type (autocracy, anocracy, democracy)
#dat$pol.cat.fl[dat$pol.polity >= -10 & dat$pol.polity < -5] <- 1  # Autocracy
#dat$pol.cat.fl[dat$pol.polity >= -5 & dat$pol.polity <= 5] <- 2  # Anocracy
#dat$pol.cat.fl[dat$pol.polity > 5] <- 3  # Democracy
#dat$pol.cat.fl[dat$pol.polity == -66 | dat$pol.polity == -77 | dat$pol.polity == -88 ] <- 7  # Other

#"cou.tries5d" -- see data.transformation.R -- original was a real pain. vdem coup
# data doesn't have "attempts", so that's not the fix.

# "elc.eleth2", salient elite ethnicity (minority rule).  comes from data.transformation.R, i.e.
# "dat$elc.eleth2 <- ifelse(dat$elc.eleth==2, 1, ifelse(is.na(dat$elc.eleth)==FALSE, 0, NA) )"
# comes from elc.csv, which comes from elcV2015.csv, 
# which comes from " Source: Center for Systemic Peace via PITF.

### note for later,  good way to do lags (from Jay)
#ld1 <- function(x)c(x[2:(length(x))], NA) # Function for 1-year lead; gets 2nd thru last obs, appends NA at end
#dat <- ddply(dat, ~country, transform, mkl.start.1 = ld1(mkl.start))

###===========================================================
#Notes:
###===========================================================
# elf.ethniccX -- replaced with the actual number, elf.ethnic
# elc.elethX -- using only elc.eleth2 due to risk of retrospective bias as-is so using “2 vs not 2”.  
#(CHECK FIRST THAT THIS IS HOW ITS CODED -WE JUST WANT MINORITY ELITE RULE WITHOUT SALIENCE)
# v2clkill_ord (poltiical killings) replaced with pol_killing_approved

# Excluded overall freedom of movement (continuous), v2xcl_dmove, 
# but included binary forms for clearly free moevement for men and women
# v2cldmovem==4 and v2cldmovew==4, called freemove_men4
# and freemove_women4.

# Included freedom of political discussion, v2xcl_disc, as is (continuous);
# may want to reconsider.

# To do:
# cou.tries5d -- aceptable risk, but let’s look at Powell and Thyne, and check if updated past 2014/ how regularly.
# pit.sftpuhvl2.10.ln -- risk of retrospective bias if used as-is, but use just count of civil wars (adverse regime change is risky and is part of this)
# "conflict" -- use PRIO 1000
# Mev.regac.ln: -- risk of retrospective bias, requires separate dataset not used for anything
# else, complciated to reconstruct -- cut.
# v2xcl_dmove -- see notes about versions of this, but what I saw
# was not ordinal and didn't allow what we discussed; Including it as is for now.

# 11.5 State authority over population (C) (v2svstpop) -- did not find this in the V-DEM data!

# There were electio-related things we said we'd put into the data 
# that aren't useful for the main EWP analysis but could be useful for
# other things later.

#elf.ethniccX, X={1: low, 2: medium, 3: high, 9: missing}
#v2psparban, *_osp, *_ord)
#v2psoppaut, *_osp, *_ord)
#v2jureform, *_osp, *_ord)
#v2clrelig, *_osp, *_ord
#v2svstpop


# - v2xcl_dmove. Acceptable (and if needed collapse to the more serious categories); try men and women separately.  Use 4-vs-other.  Try one collapsed to “all citizens = 4 or not) and one that is “women =4 or not”.
# -(v2xcl_disc).  option to restrict to more serious categories, try men and women separately
# -(v2clsocgrp, *_osp, *_ord). Use only “0” vs “not 0”
# 
# -(v2clrgunev, *_osp, *_ord). I propose instead 0-1 vs. 2 -- so it’s “any uneveness vs. eveness”.
# 
# -v2csreprss, *_osp, *_ord. Acceptable when using only 0 vs. not-0.
# 
# -(v2pepwrsoc, *_osp, *_ord.  I propose using 0-1-2 vs 3-4 because that gets “institutionalized monopolies” vs. everything else. 
#   
# v2x_clphy. The scale is interval, there is also an ordinal option (16.43). Acceptable risk. Split into the torture and the killings scales, and do bother as 4-vs-all
#   
# -(v2clkill, *_osp, *_ord). Do as 4 vs. all?




