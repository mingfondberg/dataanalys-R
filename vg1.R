
 
library(httr)
library(jsonlite)

# Definiera URL till API:et
url <- "https://api.scb.se/OV0104/v1/doris/sv/ssd/BE/BE0101/BE0101A/BefolkningNy"

# Skapa en JSON-sträng för att specificera parametrarna för din förfrågan
body <- '{
  "query": [
    {
      "code": "ContentsCode",
      "selection": {
        "filter": "item",
        "values": ["BE0101N1"]
      }
    },
    {
      "code": "Tid",
      "selection": {
        "filter": "item",
        "values": ["2010", "2011"]
      }
    }
  ],
  "response": {
    "format": "json"
  }
}'

# Skicka POST-förfrågan till API:et
response <- POST(url, body = body, encode = "json")

# Kontrollera statuskod för att se om förfrågan var framgångsrik
if (status_code(response) == 200) {
  print("Data hämtad framgångsrikt")
  # Extrahera innehållet från svaret
  data <- content(response, "parsed", encoding = "UTF-8")
  # Visa en sammanfattning av datan
  print(data)
} else {
  print(paste("Fel uppstod: statuskod", status_code(response)))
}
head(data)
install.packages("dplyr")
library(dplyr)

# Skapa en data.frame från nedladdad data
data_frame <- data.frame(
  År = c(data$data[[1]]$key[[1]], data$data[[2]]$key[[1]]),
  Folkmängd = as.numeric(c(data$data[[1]]$values[[1]], data$data[[2]]$values[[1]]))
)

# Visa den skapade data.frame
print(data_frame)
install.packages("openxlsx")
library(openxlsx)

# Ange sökväg och filnamn för den nya Excel-filen
file_path <- "Folkmängd_2010_2011.xlsx"

# Skriv data.frame till en Excel-fil
write.xlsx(data_frame, file = file_path)







library(httr)

# URL till SCB:s API
url <- "https://api.scb.se/OV0104/v1/doris/sv/ssd/BE/BE0101/BE0101A/BefolkningNy"



# Skicka en enklare POST-förfrågan
simple_response <- POST(url, body = simple_body, encode = "json", verbose = TRUE)

# Kontrollera responsen
if (status_code(simple_response) == 200) {
  print(content(simple_response, "text"))
} else {
  print(paste("Fel uppstod: statuskod", status_code(simple_response)))
  print(content(simple_response, "text"))  # Skriv ut felet för att se detaljer
}
library(httr)

# URL till SCB:s API
url <- "https://api.scb.se/OV0104/v1/doris/sv/ssd/BE/BE0101/BE0101A/BefolkningNy"



# Skicka en enklare POST-förfrågan
simple_response <- POST(url, body = simple_body, encode = "json", verbose = TRUE)

# Kontrollera responsen
if (status_code(simple_response) == 200) {
  print(content(simple_response, "text"))
} else {
  print(paste("Fel uppstod: statuskod", status_code(simple_response)))
  print(content(simple_response, "text"))  # Skriv ut felet för att se detaljer
}
library(jsonlite)

# Anta att `api_response` är variabeln som håller ditt API-svar som en textsträng
api_response <- content(simple_response, "text")  # Uppdatera variabelnamn efter ditt behov
data_list <- fromJSON(api_response)

# Konvertera till data.frame
data_df <- do.call(rbind, lapply(data_list$data, function(item) {
  data.frame(Region = item$key[1], År = item$key[2], Folkmängd = as.numeric(item$values[1]), Folkökning = as.numeric(item$values[2]))
}))
# Ange filnamnet för din Excel-fil
file_name <- "SCB_Data.xlsx"

# Skriv data.frame till en Excel-fil
write.xlsx(data_df, file = file_name)

# Kontrollera att filen har skapats i din arbetskatalog
list.files(pattern = "*.xlsx")
# Antag att du har ditt JSON data laddat i en variabel som heter json_data
json_data <- fromJSON(content(simple_response, "text"))

# Omvandla JSON-data till en data.frame
final_data <- data.frame()
for (i in seq_along(json_data$data)) {
  # För varje datapunkt, skapa en rad i data.frame
  row_data <- json_data$data[[i]]
  final_data <- rbind(final_data, c(row_data$key, row_data$values))
}


# Nu när vi har en korrekt formaterad data.frame kan vi exportera till Excel
write.xlsx(final_data, file = "SCB_Data_Corrected.xlsx")
final_data <- data.frame()

# Fyll sedan `final_data` med din loop (som nedan).
json_data <- fromJSON(content(simple_response, "text"))
for (i in seq_along(json_data$data)) {
  # För varje datapunkt, skapa en rad i data.frame
  row_data <- json_data$data[[i]]
  # Här ska du se till att varje insättning är i rätt format
  new_row <- data.frame(Region = row_data$key[1], 
                        År = row_data$key[2], 
                        Folkmängd = as.numeric(row_data$values[1]), 
                        Folkökning = as.numeric(row_data$values[2]))
  final_data <- rbind(final_data, new_row)
}

# Nu när du har en korrekt formaterad data.frame kan du exportera till Excel
write.xlsx(final_data, file = "SCB_Data_Corrected.xlsx")
# Detta antar att `data_df` redan är korrekt skapad och innehåller datan du vill exportera.
write.xlsx(data_df, file = "SCB_Data_Corrected.xlsx")
# Detta antar att `data_df` redan är korrekt skapad och innehåller datan du vill exportera.
write.xlsx(data_df, file = "SCB_Data_Corrected.xlsx")
# Detta kommer att lista alla filer som slutar med .xlsx i din aktuella arbetskatalog.
list.files(pattern = "*.xlsx")
# Detta kommer att visa de första raderna i din data.frame
head(data_df)
library(jsonlite)

# Antag att 'simple_response' är din respons som erhållits från API:et.
# Använd fromJSON för att konvertera responsen till en lista i R.
api_data <- fromJSON(content(simple_response, "text", encoding = "UTF-8"))
# Detta kommer att visa strukturen på din lista.

# Skapa en tom data.frame med korrekta kolumnnamn.
data_df <- data.frame(RegionKod = character(),
                      År = integer(),
                      Folkmängd = numeric(),
                      Folkökning = numeric(),
                      stringsAsFactors = FALSE)

# Loopa över varje element i listan och fyll data.frame
for(item in api_data$data) {
  # Skapa en temporär data.frame för denna iteration
  temp_df <- data.frame(RegionKod = item$key[1],
                        År = as.integer(item$key[2]),
                        Folkmängd = as.numeric(item$values[1]),
                        Folkökning = as.numeric(item$values[2]),
                        stringsAsFactors = FALSE)
  
  # Kombinera den temporära data.frame med den huvudsakliga
  data_df <- rbind(data_df, temp_df)
}
head(data_df)

str(api_data$data)
data_df <- data.frame(RegionKod = character(),
                      År = integer(),
                      Folkmängd = numeric(),
                      Folkökning = numeric(),
                      stringsAsFactors = FALSE)

# Loop igenom 'api_data$data' för att bygga 'data_df'
for(item in api_data$data) {
  # Kontrollera att 'item' har 'key' och 'values'
  if(!is.null(item$key) && !is.null(item$values)) {
    # Anta att 'key' innehåller två element: RegionKod och År
    # och 'values' innehåller två element: Folkmängd och Folkökning
    temp_df <- data.frame(RegionKod = item$key[1],
                          År = as.integer(item$key[2]),
                          Folkmängd = as.numeric(item$values[1]),
                          Folkökning = as.numeric(item$values[2]),
                          stringsAsFactors = FALSE)
    
    # Lägg till 'temp_df' till 'data_df'
    data_df <- rbind(data_df, temp_df)
  }
}



# Loopa genom varje rad i 'api_data$data' och fyll 'data_df'
for(i in 1:nrow(api_data$data)) {
  key_values <- api_data$data$key[[i]]
  values_values <- api_data$data$values[[i]]
  
  # Omvänt till nummer, hantera '..' som NA
  folkmangd <- ifelse(values_values[1] == "..", NA, as.numeric(values_values[1]))
  folkokning <- ifelse(values_values[2] == "..", NA, as.numeric(values_values[2]))
  
  # Skapa en temporär data.frame för denna raden
  temp_df <- data.frame(RegionKod = as.integer(key_values[1]),
                        År = as.integer(key_values[2]),
                        Folkmängd = folkmangd,
                        Folkökning = folkokning,
                        stringsAsFactors = FALSE)
  
  # Lägg till den temporära data.frame till 'data_df'
  data_df <- rbind(data_df, temp_df)
}

# Nu kan vi använda write.xlsx för att skapa en Excel-fil
write.xlsx(data_df, file = "/mnt/data/SCB_Data_Corrected.xlsx")

# Ge länken till användaren att ladda ner den skapade filen
cat("Din fil är redo att laddas ner: ", "/mnt/data/SCB_Data_Corrected.xlsx", "\n")
write.xlsx(data_df, file = "SCB_Data_Corrected.xlsx")
region_namn <- c("1" = "Stockholms län)
# Antag att 'data_df' är din nuvarande data.frame och 'region_namn' är din uppslagslista

data_df$RegionText <- region_namn[data_df$RegionKod]

# Nu när din data.frame har en ny kolumn 'RegionText', kan du skriva den till en Excel-fil igen
write.xlsx(data_df, file = "/mnt/data/SCB_Data_Corrected_with_Region.xlsx")


# Använd denna vektor för att skapa en ny kolumn i data.frame
data_df$RegionText <- region_namn[data_df$RegionKod]

# Visa de första raderna för att kontrollera att allt ser rätt ut
head(data_df)
# Antag att din data.frame heter data_df och du har redan skapat en kolumn RegionText

# Du kan välja ordningen på kolumnerna manuellt genom att specificera kolumnnamnen i den ordning du önskar
data_df <- data_df[c("RegionKod", "RegionText", "År", "Folkmängd", "Folkökning")]

# Nu kommer RegionText att visas direkt efter RegionKod.
# Visa de första raderna för att kontrollera att kolumnordningen är korrekt
head(data_df)

# Skriv sedan om data.frame till en Excel-fil med den nya ordningen
write.xlsx(data_df, file = "SCB_Data_Reordered.xlsx")




