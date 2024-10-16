library(shiny)


ui <- fluidPage(
  
)

server <- function(input, output, session) {
  
}

shinyApp(ui, server)

pscomm <- "powershell -command \"java -jar "
currentPH <- getwd()
jarPH <- "\\mithril-standalone-mithril-2\\Built\\MITHrIL2.jar"
jarPATH <- paste(pscomm, currentPH, jarPH, sep = "")
typeof(jarPATH)
print(jarPATH)
system(jarPATH)


# Showing Options
system("powershell -command \"java -jar C:/Users/maria/Documents/'Data Science'/Bioinformatica/Progetto/mithril-standalone-mithril-2/Built/MITHrIL2.jar\"")
system("powershell -command \"java -jar .\\mithril-standalone-mithril-2\\Built\\MITHrIL2.jar")

# Showing available organisms
system("powershell -command \"java -jar ./mithril-standalone-mithril-2/Built/MITHrIL2.jar organisms\"")

# Pathway Categories
system("powershell -command \"java -jar ./mithril-standalone-mithril-2/Built/MITHrIL2.jar pathway-categories\"")

# Exporting Graph
system("powershell -command \"java -jar ./mithril-standalone-mithril-2/Built/MITHrIL2.jar exportgraph -eo graph.txt\"")




# Testing
#system("powershell -command \"java -jar .\\mithril-standalone-mithril-2\\Built\\MITHrIL2.jar batch-mithril -i input.txt -o . -adjuster Bonferroni -combiner Mean")

#system("powershell -command \"java -jar .\\mithril-standalone-mithril-2\\Built\\MITHrIL2.jar batch-mithril -i input2.txt -o . -adjuster Bonferroni -combiner Mean")


