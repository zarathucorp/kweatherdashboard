
#' @title 'Shinydashboard' for 'K-WEATHER'
#' @description RStudio addin- 'Shinydashboard' for 'K-WEATHER' 
#' @param max.filesize Maximum file size to upload (MB), Default: 2048 (2 GB)
#' @return RStudio addin- 'Shinydashboard' for 'K-WEATHER'
#' @details RStudio addin- 'Shinydashboard' for 'K-WEATHER'
#' @examples 
#' if(interactive()){
#' kweather()
#'  }
#' @seealso 
#'  \code{\link[data.table]{fread}}
#'  \code{\link[zip]{unzip}}
#'  \code{\link[shinyBS]{bsModal}}
#'  \code{\link[shinyWidgets]{downloadBttn}}
#'  \code{\link[lubridate]{hour}},\code{\link[lubridate]{day}}
#'  \code{\link[rmarkdown]{render}},\code{\link[rmarkdown]{html_document}}
#' @rdname kweather
#' @export 
#' @importFrom zip unzip
#' @importFrom shinyBS bsModal
#' @importFrom shinyWidgets downloadBttn
#' @importFrom lubridate hour day ymd_hm as_date
#' @importFrom data.table fread := .SD rbindlist
#' @importFrom stats IQR median quantile sd
#' @importFrom DT datatable %>% formatStyle styleInterval renderDT
#' @importFrom rmarkdown render html_document
#' @importFrom shinycustomloader withLoader
#' @importFrom shinydashboard box
#' @import shiny 
#' @import shinydashboard
#' @import highcharter


kweather <- function(max.filesize = 2048){
  options(shiny.maxRequestSize = max.filesize * 1024^2)
  . <- DATETIME <- ID <- V1 <- NULL
  
  
  ref <- data.table::fread("inst/ref.csv")
  names(ref) <- c("Type", "ID", "SERIAL")

  
  ui <- dashboardPage(
    dashboardHeader(
      title = "K-WEATHER Dashboard",
      dropdownMenu(type = "messages",
                   messageItem(
                     from = "Sales Dept",
                     message = "Sales are steady this month."
                   ),
                   messageItem(
                     from = "New User",
                     message = "How do I register?",
                     icon = icon("question"),
                     time = "13:45"
                   ),
                   messageItem(
                     from = "Support",
                     message = "The new server is ready.",
                     icon = icon("life-ring"),
                     time = "2014-12-01"
                   )
      )),
    
    dashboardSidebar(
      sidebarMenu(
        fileInput("file", "Upload zip file"),
        helpText(a(h4("Example data", class = "btn btn-default action-button" , 
                      style = "fontweight:600"), href="https://zarathu.s3.ap-northeast-2.amazonaws.com/exampledata/example_skkudashboard.zip")),
        uiOutput("range_month"),
        uiOutput("placeid"),
        menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),
        menuItem("Per place", icon = icon("th"), tabName = "perplace",
                 badgeLabel = "new", badgeColor = "green"),
        uiOutput("downreport")
      )
    ),
    dashboardBody(
      tabItems(
        # First tab content
        tabItem(tabName = "dashboard",
                uiOutput("show"),
                fluidRow(
                  do.call(tagList, lapply(c("PM10", "PM25", "CO2", "VOCS", "NOISE", "TEMP", "HUMI"), function(i){
                    box(title = i, status = "primary", solidHeader = TRUE,
                        withLoader(highchartOutput(paste0("chart", i)), type="html", loader="loader6"))
                  }))
                )
                
        ),
        
        # Second tab content
        tabItem(tabName = "perplace",
                selectInput("vars", "Select variables", c("PM10", "PM25", "CO2", "VOCS", "NOISE", "TEMP", "HUMI")),
                fluidRow(
                  uiOutput("perplace")
                )
                
        )
      )
    )
  )
  
  
  
  server <- function(input, output, session){
    
    userFile <- eventReactive(input$file, {
      # If no file is selected, don't do anything
      #validate(need(input$file, message = FALSE))
      input$file
    })
    
    data <- eventReactive(input$file, {
      validate(need((grepl("zip", userFile()$name) == T), message = "Please upload zip file"))
      #validate(need((grepl("csv", userFile()$name) == T), message = "Please upload csv/xlsx/sav/sas7bdat file"))
      #files <- unzip(userFile()$datapath, exdir = "ex")
      tmp <- tempfile()
      zip::unzip(userFile()$datapath, exdir = tmp)
      out <- rbindlist(
        lapply(paste0(tmp,"/", list.files(tmp)), fread, na.string = c("-999", "-9999", "-1099.9")) 
      )
      out[, DATETIME := ymd_hm(DATETIME)]
      return(merge(out, ref, by = "SERIAL"))
    })
    
    
    
    output$show <- renderUI({
      req(data())
      tagList(
        actionButton("showdata", "Show data"),
        shinyBS::bsModal("data_modal", "Data", "showdata", size = "large", dataTableOutput("data")),
        actionButton("summarydata", "Summary table"),
        shinyBS::bsModal("summary_modal", "Summary table", "summarydata", size = "large", dataTableOutput("summary")),
        actionButton("outlierdata", "Outlier data"),
        shinyBS::bsModal("outlier_modal", "Outlier(yellow background) data", "outlierdata", size = "large", dataTableOutput("outlier"))
      )
      
    })
    
    output$placeid <- renderUI({
      req(data())
      choice.list <- sort(unique(data()$ID))
      tagList(
        selectInput("place", "Place ID", choices = choice.list, selected = choice.list[1], multiple = T),
        radioButtons("group_place", "Place choice with group", c("No", "노원 EZ하우스", "일반아파트", "All"), inline = T)
      )
    })
    
    output$downreport <- renderUI({
      req(data())
      shinyWidgets::downloadBttn(
        outputId = "report_kweather",
        label = "Download report",
        style = "bordered",
        color = "default"
      )
      #downloadButton("report_kweather", "Download report")
      
    })
    
    
    
    IDList <- reactive({
      req(data())
      req(input$group_place)
      idlist <- sort(unique(data()$ID))
      switch(input$group_place,
             "No" = idlist[1],
             "노원 EZ하우스" = grep("EZ", idlist, value = T),
             "일반아파트" = grep("GP", idlist, value = T),
             "All" = idlist)
    })
    
    observeEvent(IDList(), {
      updateRadioButtons(session, "place", selected = IDList())
    })
    
    output$perplace <- renderUI({
      req(input$place)
      do.call(tagList, lapply(input$place, function(i){
        box(title = i, status = "primary", solidHeader = TRUE,
            withLoader(highchartOutput(paste0("chart", i)), type="html", loader="loader6"))
      }))
      
    })
    
    
    
    output$range_month <- renderUI({
      tagList(
        dateRangeInput("dateRange", label = "Date range", start = as_date(min(data()$DATETIME)), end = as_date(max(data()$DATETIME)), 
                       min = as_date(min(data()$DATETIME)), max = as_date(max(data()$DATETIME)), language = "kr", width = '100%'),
        radioButtons("xaxis", "X-axis", c("Hour", "All Day", "Weekday", "Weekend"), inline = T)
      )
      
    })
    
    
    
    
    
    output$data <- renderDT({
      datatable(data()[, c(12:13, 2, 3, 11,  4:10)], rownames = F, filter = 'top', options = list(scrollX =T))
    })
    
    
    table.summary <- reactive({
      req(data.hcp())
      dd <- data.hcp()[, as.list(unlist(lapply(.SD, function(x) list(`Mean ± SD` = paste(round(mean(x, na.rm = T), 1), "±", round(sd(x, na.rm = T), 1)), Min = round(min(x, na.rm = T), 1), Median=round(median(x, na.rm = T), 1), Max = round(max(x, na.rm = T), 1))))), 
                       .SDcols = c("PM10", "PM25", "CO2", "VOCS", "NOISE", "TEMP", "HUMI"), by = .(substr(ID, 1, 2))]
      
      tb <- t(dd[, -1])
      
      tb.final <- cbind(sapply(strsplit(rownames(tb), "\\."), `[[`, 2), tb)
      colnames(tb.final) <- c(" ", dd[[1]])
      colnames(tb.final) <- gsub("EZ", "노원 EZ하우스", gsub("GP", "일반아파트", colnames(tb.final), ignore.case = T),  ignore.case = T) 
      #colnames(tb.final) <- gsub("EZ", "노원 EZ하우스", colnames(tb.final)) %>% gsub("GP", "일반아파트", colnames(tb.final))
      
      rownames(tb.final) <- unlist(lapply(c("PM10", "PM25", "CO2", "VOCS", "NOISE", "TEMP", "HUMI"), function(x){c(x, "", "", "")})) 
      return(tb.final)
    })
    
    
    output$summary <- renderDT({
      req(table.summary())
      fname <- "summary"
      datatable(table.summary(), rownames = T, extensions = "Buttons", caption = "Summary table",
                options = c(list(dom = '<lf<rt>Bip>', lengthMenu = list(c(10, 25, -1), c('10', '25', 'All')), pageLength = -1,
                                 buttons = list('copy', 
                                                'print', 
                                                list(extend = 'collection', 
                                                     buttons = list(list(extend = 'csv', filename= fname),
                                                                    list(extend = 'excel', filename= fname), 
                                                                    list(extend = 'pdf', filename= fname)
                                                     ), 
                                                     text = 'Download')
                                 )), list(scrollX = TRUE)))
    })
    
    output$outlier <- renderDT({
      req(data())
      req(input$dateRange)
      zz <- data()[as_date(DATETIME) >= input$dateRange[1] & as_date(DATETIME) <= input$dateRange[2], ][ID %in% input$place]
      kk <- zz[, lapply(.SD, function(x){x > quantile(x, 0.75, na.rm = T) + 1.5 * IQR(x, na.rm = T) | x < quantile(x, 0.25, na.rm = T) - 1.5 * IQR(x, na.rm = T)}), .SDcols = c("PM10", "PM25", "CO2", "VOCS", "NOISE", "TEMP", "HUMI")]
      datatable(zz[rowSums(kk[, -c(1, 2)]) > 0][, c(12:13, 2, 3, 11,  4:10)], rownames = F, filter = 'top', options = list(scrollX =T)) %>% 
        formatStyle("PM10", backgroundColor = styleInterval(c(quantile(zz$PM10, 0.25, na.rm = T) - 1.5 * IQR(zz$PM10, na.rm = T), quantile(zz$PM10, 0.75, na.rm = T) + 1.5 * IQR(zz$PM10, na.rm = T)), c("yellow", "NA", "yellow"))) %>% 
        formatStyle("PM25", backgroundColor = styleInterval(c(quantile(zz$PM25, 0.25, na.rm = T) - 1.5 * IQR(zz$PM25, na.rm = T), quantile(zz$PM25, 0.75, na.rm = T) + 1.5 * IQR(zz$PM25, na.rm = T)), c("yellow", "NA", "yellow"))) %>% 
        formatStyle("CO2", backgroundColor = styleInterval(c(quantile(zz$CO2, 0.25, na.rm = T) - 1.5 * IQR(zz$CO2, na.rm = T), quantile(zz$CO2, 0.75, na.rm = T) + 1.5 * IQR(zz$CO2, na.rm = T)), c("yellow", "NA", "yellow"))) %>% 
        formatStyle("VOCS", backgroundColor = styleInterval(c(quantile(zz$VOCS, 0.25, na.rm = T) - 1.5 * IQR(zz$VOCS, na.rm = T), quantile(zz$VOCS, 0.75, na.rm = T) + 1.5 * IQR(zz$VOCS, na.rm = T)), c("yellow", "NA", "yellow"))) %>% 
        formatStyle("NOISE", backgroundColor = styleInterval(c(quantile(zz$NOISE, 0.25, na.rm = T) - 1.5 * IQR(zz$NOISE, na.rm = T), quantile(zz$NOISE, 0.75, na.rm = T) + 1.5 * IQR(zz$NOISE, na.rm = T)), c("yellow", "NA", "yellow"))) %>% 
        formatStyle("TEMP", backgroundColor = styleInterval(c(quantile(zz$TEMP, 0.25, na.rm = T) - 1.5 * IQR(zz$TEMP, na.rm = T), quantile(zz$TEMP, 0.75, na.rm = T) + 1.5 * IQR(zz$TEMP, na.rm = T)), c("yellow", "NA", "yellow"))) %>% 
        formatStyle("HUMI", backgroundColor = styleInterval(c(quantile(zz$HUMI, 0.25, na.rm = T) - 1.5 * IQR(zz$HUMI, na.rm = T), quantile(zz$HUMI, 0.75, na.rm = T) + 1.5 * IQR(zz$HUMI, na.rm = T)), c("yellow", "NA", "yellow")))  
    })
    
    
    data.hc <- eventReactive(c(data(), input$dateRange, input$xaxis), {
      out <- data()[as_date(DATETIME) >= input$dateRange[1] & as_date(DATETIME) <= input$dateRange[2], ]
      #out <- data()[, .SD]
      req(input$xaxis)
      if (input$xaxis == "Hour"){
        out[, DATETIME := lubridate::hour(DATETIME)] 
      } else if (input$xaxis == "All Day"){
        out[, DATETIME := lubridate::day(DATETIME)]
      } else if (input$xaxis == "Weekday"){
        out <- out[weekdays(DATETIME) %in% c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday")]
        out[, DATETIME := lubridate::day(DATETIME)]
      } else if (input$xaxis == "Weekend"){
        out <- out[weekdays(DATETIME) %in% c("Saturday", "Sunday")]
        out[, DATETIME := lubridate::day(DATETIME)]
      }
      
      return(out[])
    })
    
    data.hcp <- eventReactive(c(data.hc(), input$place), {
      validate(need(nrow(data.hc()) > 0, "No data in the date ranges"))
      return(data.hc()[ID %in% input$place])
    })
    
    
    list.chart <- eventReactive(c(data.hcp(), input$place, input$xaxis), {
      req(data.hcp())
      req(input$place)
      validate(need(nrow(data.hcp()) > 0, "No data in the place"))
      txt <- ifelse(input$xaxis == "Hour", "Hour", "Day")
      idlist <- unique(data()$ID)
      
      out <- lapply(c("PM10", "PM25", "CO2", "VOCS", "NOISE", "TEMP", "HUMI"), function(my_i){
        return(
          hcboxplot(x = data.hcp()[[my_i]], var = data.hcp()[["DATETIME"]], outliers = F, color = "#2980b9", name = my_i) %>%  
            #hc_title(text = my_i) %>% 
            hc_xAxis(title = list(text = txt)) %>% 
            hc_chart(type = "column") %>% 
            hc_add_series(data.hcp()[, round(mean(get(my_i), na.rm = T), 1), by = .(DATETIME)], "line", hcaes(y = V1), name = "Place mean") %>% 
            hc_add_series(data.hc()[ID %in% grep("GP", idlist, value = T), round(mean(get(my_i), na.rm = T), 1), by = .(DATETIME)], "line", hcaes(y = V1), name = "GP mean") %>%
            hc_add_series(data.hc()[ID %in% grep("EZ", idlist, value = T), round(mean(get(my_i), na.rm = T), 1), by = .(DATETIME)], "line", hcaes(y = V1), name = "EZ mean") %>%
            hc_add_series(data.hc()[, round(mean(get(my_i), na.rm = T), 1), by = .(DATETIME)], "line", hcaes(y = V1), name = "All mean") %>%
            hc_exporting(enabled = T) %>% 
            hc_tooltip(valueDecimals = 1)
        )
      })
      
      names(out) <- c("PM10", "PM25", "CO2", "VOCS", "NOISE", "TEMP", "HUMI")
      return(out)
      
    })
    
    
    
    
    for (i in c("PM10", "PM25", "CO2", "VOCS", "NOISE", "TEMP", "HUMI")) {
      local({
        my_i <- i
        output[[paste0("chart",my_i)]] <- renderHighchart({
          req(list.chart())
          list.chart()[[my_i]]
        })
        
      })
    }
    
    
    list.chartperplace <- eventReactive(c(input$place, data.hcp(), input$xaxis, input$vars), {
      req(data.hcp())
      req(input$place)
      validate(need(nrow(data.hcp()) > 0, "No data in the place"))
      txt <- ifelse(input$xaxis == "Hour", "Hour", "Day")
      idlist <- unique(data()$ID)
      out <- lapply(input$place, function(my_i){
        data.perplace <- data.hcp()[ID == my_i]
        return(
          hcboxplot(x = data.perplace[[input$vars]], var = data.perplace[["DATETIME"]], outliers = F, color = "#2980b9", name = my_i) %>%  
            #hc_title(text = my_i) %>% 
            hc_xAxis(title = list(text = txt)) %>% 
            hc_chart(type = "column") %>% 
            hc_add_series(data.perplace[, round(mean(get(input$vars), na.rm = T), 1), by = .(DATETIME)], "line", hcaes(y = V1), name = "Place mean") %>% 
            hc_add_series(data.hc()[ID %in% grep("GP", idlist, value = T), round(mean(get(input$vars), na.rm = T), 1), by = .(DATETIME)], "line", hcaes(y = V1), name = "GP mean") %>%
            hc_add_series(data.hc()[ID %in% grep("EZ", idlist, value = T), round(mean(get(input$vars), na.rm = T), 1), by = .(DATETIME)], "line", hcaes(y = V1), name = "EZ mean") %>%
            hc_add_series(data.hc()[, round(mean(get(input$vars), na.rm = T), 1), by = .(DATETIME)], "line", hcaes(y = V1), name = "All mean") %>%
            hc_exporting(enabled = T) %>% 
            hc_tooltip(valueDecimals = 1)
        )
      })
      names(out) <- input$place
      return(out)
      
    })
    
    
    observeEvent(c(input$place, input$vars), {
      for (i in input$place) {
        local({
          my_i <- i
          output[[paste0("chart",my_i)]] <- renderHighchart({
            req(list.chartperplace())
            list.chartperplace()[[my_i]]
          })
          
        })
      }
    })
    
    
    output$report_kweather <- downloadHandler(
      filename = function() {
        paste(paste('Report_kweather_', input$dateRange[1], "_", input$dateRange[2], sep=""), sep = '.', 'html')
      },
      content = function(file) {
        withProgress(message = 'Download in progress',
                     detail = 'This may take a while...', value = 0, {
                       for (i in 1:60) {
                         incProgress(1/60)
                         Sys.sleep(0.2)
                       }
                       
                       src <- normalizePath("inst/report_kweather.Rmd")
                       
                       # temporarily switch to the temp dir, in case you do not have write
                       # permission to the current working directory
                       owd <- setwd(tempdir())
                       on.exit(setwd(owd))
                       file.copy(src, 'report_stress.Rmd', overwrite = TRUE)
                       
                       out <- rmarkdown::render('report_stress.Rmd', 
                                     rmarkdown::html_document(toc=T, highlight="textmate", theme="cosmo", toc_float = T),
                                     params=list(place = input$place,
                                                 dateRange1 = input$dateRange[1],
                                                 dateRange2 = input$dateRange[2],
                                                 xaxis = input$xaxis,
                                                 vars = input$vars,
                                                 chart = list.chart(),
                                                 chart.perplace = list.chartperplace(),
                                                 tbsummary = table.summary()),
                                     envir = new.env()
                       )
                       file.rename(out, file)
                       
                     })
        
      })
    
    session$onSessionEnded(function() {
      stopApp()
    })
    
    
    
  }
  
  
  
  viewer <- browserViewer(browser = getOption("browser"))
  runGadget(ui, server, viewer = viewer)
}

