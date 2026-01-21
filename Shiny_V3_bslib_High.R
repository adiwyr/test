library(dplyr)
library(stringr)
library(shiny)
library(bslib)
library(highcharter)
library(DT)
library(readxl)
library(shinyWidgets)

# Assume the working directory and data loading remains the same
gefis <- readxl::read_xlsx("~/lustat-daten/work/text/_user/00380392/R/ShinyR/Gefi-Dashboard/gefis-lu-jr.xlsx")


format(object.size(gefis), units = "auto")
dim(gefis)

# Format function
format <- function(x) {
   prettyNum(round(x, digits = 0), big.mark = "'", scientific = FALSE)
}

# Definition High-Theme
high_theme <-
   hc_theme(
      chart = list(
         plotBorderWidth = 0,
         marginLeft = 80,
         marginRight = 50,
         spacingLeft = 80,
         spacingRight = 50,
         spacingBottom = 80,
         backgroundColor = "white",
         style = list(
            color = "#8d8689",
            fontFamily = "Source Sans Pro",
            fontWeight = "normal"
         )
      ),
      title = list(align = "left"),
      subtitle = list(
         margin = 0,
         align = "left"
      ),
      legend = list(
         enabled = TRUE,
         reversed = TRUE,
         align = "left",
         verticalAlign = "bottom",
         layout = "vertical",
         margin = 0,
         floating = FALSE,
         itemStyle = list(
            fontSize = "1.0em",
            fontWeight = "normal",
            color = "#8d8689"
         ),
         x = -10,
         y = 50
      ),
      plotOptions = list(line = list(
         marker = FALSE
      )),
      xAxis = list(
         title = list(enabled = FALSE),
         labels = list(rotation = FALSE)
      ),
      yAxis = list(
         title = list(align = "high"),
         labels = list(format = "{value:,f}"),
         style = list(color = "#8d8689")
      ),
      tooltip = list(
         crosshairs = FALSE,
         shared = FALSE,
         sort = TRUE,
         table = TRUE
      ),
      caption = list(
         margin = 20,
         align = "right"
      )
   )


# Highcharter Optionen definieren
hcoptslang <- getOption("highcharter.lang")

hcoptslang$months <- c("Januar", "Februar", "März", "April", "Mai", "Juni", "Juli", "August", "September", "Oktober", "November", "Dezember")
hcoptslang$shortMonths <- c("Jan", "Feb", "Mär", "Apr", "Mai", "Jun", "Jul", "Aug", "Sep", "Okt", "Nov", "Dez")
hcoptslang$printChart <- "Grafik drucken"
hcoptslang$downloadPNG <- "Download PNG Bild"
hcoptslang$downloadJPEG <- "Download JPEG Bild"
hcoptslang$downloadPDF <- "Download PDF Dokument"
hcoptslang$downloadSVG <- "Download SVG Vektor Bild"
hcoptslang$downloadCSV <- "Download CSV"
hcoptslang$downloadXLS <- "Download XSV"
hcoptslang$thousandsSep <- "'"

hcoptslang$contextButtonTitle <- "Optionen"
options(highcharter.lang = hcoptslang)


css <- "
.vscomp-dropbox-container.pop-comp-wrapper.position-top,
.vscomp-dropbox-container.pop-comp-wrapper.position-bottom
  {
    width: fit-content !important;
    max-width: unset !important;
  }"


# UI
ui <- page_sidebar(
   theme = bs_theme(
      version = 5,
      base_font = font_google("Source Sans Pro"),
      code_font = font_google("Source Sans Pro")
   ),

   title = "LUSTAT Dashboard zu den Gemeindefinanzen",

   # Sidebar with navigation and controls
   sidebar = sidebar(
     title = "Steuerung",

      # Controls (previously in controlbar)
      card(
         virtualSelectInput("year", "Jahr auswählen:",
                     choices = unique(gefis$jahr),
                     selected = "2023",
                     dropboxWrapper = "body"),
         virtualSelectInput("gemeinde", "Gemeinde auswählen:",
                     choices = unique(gefis$gemeinde),
                     selected = "Doppleschwand",
                     dropboxWrapper = "body"),
         virtualSelectInput("benchmark", "Vergleichsgemeinde auswählen:",
                     choices = unique(gefis$gemeinde),
                     selected = "Romoos",
                     dropboxWrapper = "body"),
         virtualSelectInput("art", "Artstufe auswählen:",
                     choices = c(2,3),
                     selected = "2",
                     dropboxWrapper = "body")
      )
   ),

   # Main content
   nav_panel("Gefi Dashboard",
             h2(textOutput("titel"), class = "fw-bold mb-4"),

             # Value boxes in a layout grid
             layout_columns(
                value_box(
                   title = textOutput("gemeindea"),
                   value = textOutput("aktiven"),
                   showcase = bsicons::bs_icon("wallet-fill")
                ),
                value_box(
                   title = textOutput("gemeindeab"),
                   value = textOutput("abschluss"),
                   showcase = bsicons::bs_icon("coin")
                ),
                value_box(
                   title = textOutput("gemeindenet"),
                   value = textOutput("nettoinv"),
                   showcase = bsicons::bs_icon("tools")
                )
             ),

             # Charts and tables
             card(
                height = 500,
                full_screen = TRUE,
                card_header("Aufwand nach Arten: Interaktive Grafik"),
                highchartOutput("expensesByType")
             ),

             card(
                height = 500,
                full_screen = TRUE,
                card_header("Aufwand nach Arten: Interaktive Tabelle"),
                dataTableOutput("tabelle_artausgaben_gemeinde")
             )
   ),

   nav_panel("uebersicht",
             card(
                card_header("Luzerner Gemeindefinanzen: Datendownload"),
                card_body(
                   actionButton(
                      inputId = "download",
                      label = "Download der Basisdaten",
                      onclick = "window.open('https://www.data.lustat.ch/gefis-lu-jr.csv')"
                   ),
                   p("Datendownload von Opendata.Swiss")
                )
             )
   )
)

# Server
server <- function(input, output) {

   output$gemeindea <- renderText({
      paste0("Aktiven/Passiven ", "(", input$gemeinde, ")")
   })

   output$gemeindeab <- renderText({
      paste0("Ertrags-/Aufwandüberschuss ", "(", input$gemeinde, ")")
   })

   output$gemeindenet <- renderText({
      paste0("Nettoinvestitionen ", "(", input$gemeinde, ")")
   })

   output$titel <- renderText({
      paste0("Jahresrechnung von ", input$gemeinde, " im Vergleich mit ",
             input$benchmark, " im Jahr ", input$year)
   })

   # Value box contents
   output$aktiven <- renderText({
      gefis |>
         filter(jahr == input$year,
                gemeinde == input$gemeinde,
                str_detect(art_nr, "^1")) |>
         select(saldo) |>
         mutate(saldo = as.numeric(saldo)) |>
         summarise(aktiven = sum(saldo, na.rm = TRUE)) |>
         pull() |>
         format() |>
         paste0(" CHF")
   })

   output$abschluss <- renderText({
      gefis |>
         filter(jahr == input$year,
                gemeinde == input$gemeinde,
                str_detect(art_nr, "^900")) |>
         select(saldo) |>
         mutate(saldo = as.numeric(saldo)) |>
         summarise(rechnungsabschluss = sum(saldo, na.rm = TRUE)) |>
         pull() |>
         format() |>
         paste0(" CHF")
   })

   output$nettoinv <- renderText({
      gefis |>
         mutate(saldo = as.numeric(saldo)) |>
         summarise(difference = sum(saldo[jahr == input$year &
                                          gemeinde == input$gemeinde &
                                          art_nr == "6900"],
                                    na.rm = TRUE) -
                      sum(saldo[jahr == input$year &
                                   gemeinde == input$gemeinde &
                                   art_nr == "5900"],
                          na.rm = TRUE)) |>
         pull() |>
         format() |>
         paste0(" CHF")
   })

   # Nettoausgaben calculation
   nettoausgaben <- reactive({
      gefis |>
         filter(jahr == input$year,
                gemeinde %in% c(input$gemeinde, input$benchmark),
                str_detect(art_nr, "^3")) |>
         mutate(art_nr = substr(art_nr, 1, input$art),
                saldo = as.numeric(saldo)) |>
         select(jahr, gemeinde, art_nr, saldo) |>
         group_by(jahr, gemeinde, art_nr) |>
         summarise(saldo = sum(saldo, na.rm = TRUE),
                   .groups = "drop") |>
         mutate(saldo = round(saldo, 0)) |>
         mutate(art_bez = case_when(art_nr == "30" ~ "Personalaufwand",
                                    art_nr == "31" ~ "Sach- und übriger Betriebsaufwand",
                                    art_nr == "33" ~ "Abschreibungen Verwaltungsvermögen",
                                    art_nr == "34" ~ "Finanzaufwand",
                                    art_nr == "35" ~ "Einlagen in Spezialfinanzierungen und Fonds",
                                    art_nr == "36" ~ "Transferaufwand",
                                    art_nr == "37" ~ "Durchlaufende Beiträge",
                                    art_nr == "38" ~ "Ausserordentlicher Aufwand",
                                    art_nr == "39" ~ "Interne Verrechnungen und Umlagen",

                                    art_nr == "300"	~ "Behörden und Kommissionen"  ,
                                    art_nr == "301"	~ "Löhne des Verwaltungs- und Betriebspersonals",
                                    art_nr == "302"	~ "Löhne der Lehrpersonen",
                                    art_nr == "303"	~ "Temporäre Arbeitskräfte",
                                    art_nr == "304"	~ "Zulagen",
                                    art_nr == "305"	~ "Arbeitgebendenbeiträge",
                                    art_nr == "306"	~ "Arbeitgebendenleistungen",
                                    art_nr == "309"	~ "Übriger Personalaufwand",
                                    art_nr == "310"	~ "Material- und Warenaufwand",
                                    art_nr == "311"	~ "Nicht aktivierbare Anlagen",
                                    art_nr == "312"	~ "Ver- und Entsorgung Liegenschaften VV",
                                    art_nr == "313"	~ "Dienstleistungen und Honorare",
                                    art_nr == "314"	~ "Baulicher und betrieblicher Unterhalt",
                                    art_nr == "315"	~ "Unterhalt Mobilien und immaterielle Anlagen",
                                    art_nr == "316"	~ "Mieten, Leasing, Pachten, Benützungskosten",
                                    art_nr == "317"	~ "Spesenentschädigungen",
                                    art_nr == "318"	~ "Wertberichtigungen auf Forderungen",
                                    art_nr == "319"	~ "Übriger Betriebsaufwand",
                                    art_nr == "330"	~ "Abschreibungen Sachanlagen VV",
                                    art_nr == "332"	~ "Abschreibungen Immaterielle Anlagen",
                                    art_nr == "340"	~ "Zinsaufwand",
                                    art_nr == "341"	~ "Realisierte Verluste FV",
                                    art_nr == "342"	~ "Kapitalbeschaffungs- und Verwaltungskosten",
                                    art_nr == "343"	~ "Liegenschaftsaufwand Finanzvermögen",
                                    art_nr == "344"	~ "Wertberichtigungen Anlagen FV",
                                    art_nr == "349"	~ "Übriger Finanzaufwand",
                                    art_nr == "350"	~ "Einlagen in Spezialfinanzierungen und Fonds des Fremdkapitals",
                                    art_nr == "351"	~ "Einlagen in Spezialfinanzierungen und Fonds des Eigenkapitals",
                                    art_nr == "360"	~ "Ertragsanteile an Dritte",
                                    art_nr == "361"	~ "Entschädigungen an öffentlichen Gemeinwesen",
                                    art_nr == "362"	~ "Finanzausgleich",
                                    art_nr == "363"	~ "Beiträge an öffentliche Gemeinwesen und Dritte inkl. Förderbeiträge",
                                    art_nr == "364" ~ "Wertberichtigungen Darlehen VV",
                                    art_nr == "365"	~ "Wertberichtigungen Beteiligungen VV",
                                    art_nr == "366"	~ "Abschreibungen Investitionsbeiträge",
                                    art_nr == "369"	~ "Übriger Transferaufwand",
                                    art_nr == "370"	~ "Durchlaufende Beiträge",
                                    art_nr == "380"	~ "Ausserordentlicher Personalaufwand",
                                    art_nr == "381"	~ "Ausserordentlicher Sach- und Betriebsaufwand",
                                    art_nr == "384"	~ "Ausserordentlicher Finanzaufwand",
                                    art_nr == "386"	~ "Ausserordentlicher Transferaufwand",
                                    art_nr == "387"	~ "Ausserplanmässige Wertberichtigungen",
                                    art_nr == "389"	~ "Zins- und Amortisation LUPK-Darlehen",
                                    art_nr == "390"	~ "Material- und Warenbezüge",
                                    art_nr == "391"	~ "Dienstleistungen",
                                    art_nr == "392"	~ "Pacht, Mieten, Benützungskosten",
                                    art_nr == "393"	~ "Betriebs- und Verwaltungskosten",
                                    art_nr == "394"	~ "Zinsen und Finanzaufwand",
                                    art_nr == "397"	~" Umlagen ",
                                    art_nr == "398"	~ "Übertragungen",
                                    art_nr == "399"	~ "Übrige interne Verrechnungen",
                                    TRUE ~ "Andere"
                                    )) |>
         select(-art_nr) |>
         relocate(jahr, gemeinde, art_bez, saldo)
   })

   # Highcharts
   output$expensesByType <- renderHighchart({
      data <- nettoausgaben()

      hc <- highchart() |>
         hc_chart(type = "column") |>
         hc_title(
            text = paste0("Aufwand nach Arten der Gemeinde ",
                          input$gemeinde, " ", input$year)
         ) |>
         hc_subtitle(text = paste0("Ausgaben nach Art auf ", input$art, ". Stufe")) |>
         hc_xAxis(categories = unique(data$art_bez)) |>
         hc_yAxis(title = list(text = "Saldo (CHF)")) |>
         hc_plotOptions(column = list(
            dataLabels = list(enabled = FALSE),
            enableMouseTracking = TRUE
         ))

      # Add series for each gemeinde
      for(gem in unique(data$gemeinde)) {
         gem_data <- data |>
            filter(gemeinde == gem) |>
            pull(saldo)

         hc <- hc |>
            hc_add_series(
               name = gem,
               data = gem_data
            )
      }

      hc |>
         hc_tooltip(
            formatter = JS(
               "function() {
            return '<b>' + this.series.name + '</b><br/>' +
                   'Art: ' + this.x + '<br/>' +
                   'Saldo: ' + Highcharts.numberFormat(this.y, 0, '.', '\\'') + ' CHF';
          }"
            )
         ) |>
         hc_colors(list('#406ab2', '#f1873d')) |>
         hc_exporting(
            enabled = TRUE,
            filename = paste0("Ausgaben und Einnahmen nach Arten ", "auf Stufe ", input$art, " der Gemeinden ", input$gemeinde, " und ", input$benchmark)
         ) |>
         hc_add_theme(high_theme)
   })

   # Tabellen ####
   output$tabelle_artausgaben_gemeinde <- renderDataTable({
      datatable(nettoausgaben() |>
                   mutate (saldo = paste0(format(saldo), " CHF")),
                colnames=c("Jahr", "Gemeinde", "Ausgabeart", "Betrag"),
                caption = "LUSTAT: Gemeindefinanzen ",
                rownames = FALSE,
                extensions = "Buttons",
                options = list(
                   dom = 'Bfrtip',
                   buttons = c('copy', 'excel', 'pdf'),
                   language = list(
                     search = "Suche"),
                   pageLength = 20
                ),
                class="display",
                callback=JS('$("button.buttons-copy").css("background","lightgray");
                    $("button.buttons-excel").css("background","lightgray");
                    $("button.buttons-pdf").css("background","lightgray");
                    return table;'),
      )

   },
   server = FALSE)
}

shinyApp(ui, server)
