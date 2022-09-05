# Auto-Report Testing tool
library(datatoys)
library(rmarkdown)
library(magrittr)

test <- function(data, extension = "html"){
  data <- data %>% as.data.frame()

  rmarkdown::render(
    params = list(
      inputData = data
    ),
    input = "inst/report.Rmd",
    output_format = switch(extension,
                           "pdf" = pdf_document(),
                           "html" = html_document(),
                           "docx" = word_document()
    ),
  )
}

extension <-  "html"

example <- readxl::read_xlsx("example_g1e.xlsx")
test(example, extension)
test(karaoke, extension)
test(tuition, extension)
test(scholarship, extension)
test(elevator, extension)
test(necessariesPrice, extension)
test(foodBank, extension)
test(odaNews, extension)
test(odaKR, extension)
test(odaIndex, extension)
test(carInspection, extension)
test(foodNutrients, extension)
test(fireStation, extension)
test(fire, extension)
test(seoulER, extension)
test(postOffice, extension)
test(gasStation, extension)
test(childAbuse, extension)
test(petNames, extension)
test(accident, extension)
test(globalBusiness, extension)
# weather2020 ### Ignore
test(legalDong, extension)
test(housingPrice, extension)
test(busStation, extension)
test(airport, extension)
test(nationalPension, extension)
test(bloodTest, extension) #Err
test(medicalCheckup, extension)
test(pollution, extension)
test(openData, extension)
test(crimePlace, extension)
test(crime, extension)
test(restaurant, extension)
test(gyeonggiER, extension)
test(pharmacyInfo, extension)
test(medicine, extension)
test(hospitalInfo, extension)
