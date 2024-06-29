ep_listak <- read_csv(
  paste0("/home/mkoltai/Desktop/mas/EP2024/",
  "jelolo-szervezetek-listak_EP_kepviselok_valasztasa_2024_2024069.csv")) %>%
  filter(!grepl("visszaut",`Lista státusza`))
megye_telep_lista <- read_csv("/home/mkoltai/Desktop/mas/EP2024/megye_telepszam.csv") %>%
  mutate(megye.id=paste0(ifelse(megye.id<10,"0",""),as.character(megye.id)))

l_EP_megye_telep <- list()
# Define the URL of the JSON data
for (k_maz in 1:length(megye_telep_lista$megye)) {
  for (k_taz in 1:megye_telep_lista$telep.szama[k_maz]) {
    
    maz <- megye_telep_lista$megye.id[k_maz]
    taz <- ifelse(k_taz<10,paste0("00",k_taz),ifelse(k_taz<100,paste0("0",k_taz),k_taz))
    url_code <- paste0("TelepEredm-",maz,"-",taz,".json")
    # https://vtr.valasztas.hu/ep2024/data/06141000/szavossz/01/TelepEredm-01-012.json
    
    if (!is.data.frame(l_EP_megye_telep[[megye_telep_lista$megye[k_maz] ]][[k_taz]])) {
      # Download the JSON file
      response <- GET(paste0("https://vtr.valasztas.hu/ep2024/data/06141200/szavossz/",maz,"/",url_code))
      
      # Check if the request was successful
      if (status_code(response) == 200) {
        # Write the content to a file
        writeBin(content(response, "raw"), url_code)
        cat("File downloaded successfully.\n")
        print(paste0(url_code," - ",megye_telep_lista$megye[k_maz], "/", k_taz ))
        
        # Read the JSON file
        # Convert the JSON data to a data frame
        ep_data_frame <- as.data.frame(fromJSON(url_code))
        colnames(ep_data_frame) = gsub("data.|OvEpHeader.|tetelek.","",colnames(ep_data_frame))
        ep_data_frame["partlista"] = ep_listak$`Rövid név`[ep_data_frame$szavlap_sorsz]
        ep_data_frame["megye"] = unlist(lapply(ep_data_frame$maz, function(x) 
          megye_telep_lista$megye[megye_telep_lista$megye.id %in% x]))
        
        l_EP_megye_telep[[megye_telep_lista$megye[k_maz]]][[k_taz]] <- ep_data_frame[,
          c("megye","taz","vp_osszes","szavazott_osszesen","szl_ervenyes",
          "szavazat","szavazat_szaz","partlista")]
      } else {
        # stop("Failed to download file. Status code:", status_code(response))
        print(paste0("Failed to download file. Status code:", status_code(response)))
        l_EP_megye_telep[[megye_telep_lista$megye[k_maz]]][[k_taz]] <- NA
      }
      
    } # if NA
  } # k_taz
} # k_maz

# missing?
unlist(lapply(l_EP_megye_telep, function(x) sum(unlist(lapply(x, is.na)))))
# which ones are NAs?
which(!sapply(l_EP_megye_telep$BARANYA, is.data.frame))
lapply(l_EP_megye_telep, function(x) which(!sapply(x, is.data.frame)) ) 

# non NA towns
l_non_NA <- lapply(l_EP_megye_telep, function(x) which(unlist(lapply(x,is.data.frame))) )

df_EP_megye_telep <- bind_rows(lapply(names(l_EP_megye_telep), function(x) 
  lapply(l_non_NA[[x]], function(n) l_EP_megye_telep[[x]][[n]]) ))

write_csv(df_EP_megye_telep,file = "2024_EP/EP2024_megye_telep_BPker.csv")
