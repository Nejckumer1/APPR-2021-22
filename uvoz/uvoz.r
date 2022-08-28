# 2. faza: Uvoz podatkov


#sl <- locale("sl", decimal_mark=",", grouping_mark=".")

###############################################################################
# Prvi del podatkov ki vsebujejo indeks telesne teze za evropo
###############################################################################
podatki_1 = read.csv("podatki/kartoni_sveta.csv")

#ITT= indeks telesne teze
podatki_1 = podatki_1 %>% select(drzava=Country, splo=Sex, leto=Year, starostna_skupina=Age.group,povprecen_ITT=Mean.BMI)
write.csv(podatki_1,"podatki/podatki_ITT")





###############################################################################
# Drugi del podatkov ki vsebujejo sportne kartone v sloveniji za osnovno solo
###############################################################################

podatki_2 = read_excel("podatki/sportno-vzgojni-karton.xlsx")

#Dodamo stolpcem imena
colnames(podatki_2) = podatki_2[2,]

#Najprej razdelimo podatke
podatki_2_mean = podatki_2[,1:13]
podatki_2_stevilo = podatki_2[,c(1:3,14:23)]
podatki_2_std = podatki_2[,c(1:3,24:33)]


#funkcija za ureditiev podatkov
ureditev_podatkov = function(podatki, znak){
  #najprej se znebimo prvih dveh vrstic in nato dodamo stolpec vrsta
  podatki = podatki[-c(1,2),] %>%  mutate(vrsta = znak) 
  
  #zdopolnimo vrednosti NA v prvih dveh stolpcih
  for(j in 1:2){
    for( i in 1:420){
      nas_vrednost = podatki[i,j]
      if(is.na(nas_vrednost)==TRUE){
        podatki[i,j] = vrednost 
      }
      else{
        vrednost = podatki[i,j]
      }
    }
  }
  podatki = podatki %>% select(spol=Gender, razred=RAZRED, leto=LETO_MERITEV, visina=Height,
                               teza=Weight, kozna_guba="Triceps skinfold", dotikanje_plosce_z_roko="Arm plate tapping",
                               skos_z_mesta="Standing broad jump", premagovanje_ovir_nazaj="Polygon backwards",trebusnjaki="Sit ups", 
                               predklon_naprej="Stand and reach",vesa_v_zgibi="Bent arm hang", tek_na_60m = "60 m sprint", vrsta=vrsta)
  
  podatki$spol[podatki$spol == "boys"] = "fantje"
  podatki$spol[podatki$spol == "girls"] = "punce"
  podatki$spol[podatki$spol == "Total"] = "fantje_in_punce"
  podatki$razred[podatki$razred == "Total"] = "vsi_razredi_skupaj"
  podatki$leto[podatki$leto == "Total"] = "vsa_leta"
  podatki = podatki %>% mutate(across(.cols=4:13, .fns=as.numeric)) #spremenimo tipe stolpcev v numeric
  
}

podatki_povprecje = ureditev_podatkov(podatki_2_mean, znak = "povprecje")
write_csv(podatki_povprecje,"podatki/podatki_2_povprecje")

podatki_stevilo = ureditev_podatkov(podatki_2_stevilo, znak="stevilo")
write_csv(podatki_stevilo,"podatki/podatki_2_stevilo")

podatki_stdev = ureditev_podatkov(podatki_2_std,znak="stdev")
write_csv(podatki_stdev,"podatki/podatki_2_stdev")

sportni_karton = rbind(podatki_povprecje,podatki_stevilo,podatki_stdev)
write_csv(sportni_karton,"podatki/sportni_karton_urejen")



##############################################################################
# Tretji tip podatkov, ki ni povezan s prvma dvema
#############################################################################
# funkcija ki uvozi podatke


podatki = function(interval, startTime, endTime){
  zahtevek = content(
    GET(
      url = "https://api.binance.com",
      path = "api/v3/klines",
      query = list (
        symbol = "BTCUSDT",
        interval = interval,
        startTime = startTime,
        endTime = endTime
      )
    )
  )
  
  
  zahtevek_df <- as.data.frame(
    foreach(i = 1:length(zahtevek), .combine = rbind) %do% {
      foreach(j = 1:12, .combine = c) %do% {
        zahtevek[[i]][j]
      }
    },
    stringsAsFactors = FALSE, row.names = FALSE)
  
  
  zahtevek_df <- zahtevek_df[,-12]
  cols_numeric <- c(1,2,3,4,5,6,7,8,9,10,11)
  zahtevek_df[, cols_numeric] = apply(zahtevek_df[, cols_numeric], 2, function(x) as.numeric(x))
  colnames(zahtevek_df) <-c("Open_time",
                            "Open",
                            "High",
                            "Low",
                            "Close",
                            "Volume",
                            "Close_time",
                            "Quote_asset_volume",
                            "Number_of_trades",
                            "Taker_buy_base_asset_volume", 
                            "Taker_buy_quote_asset_volume")
  
  zahtevek_df$Open_time = zahtevek_df$Open_time / 1000
  zahtevek_df$Open_time = as_datetime(zahtevek_df$Open_time) 
  
  zahtevek_df$Close_time = zahtevek_df$Close_time/ 1000
  zahtevek_df$Close_time = anytime(zahtevek_df$Close_time)
  
  colnames(zahtevek_df)[1] <- "Date"
  zahtevek_df
}

podatki_svecniki = podatki("1d",1640959200000,1654977267000)
write_csv(podatki_svecniki,"podatki/podatki_svecniki")











