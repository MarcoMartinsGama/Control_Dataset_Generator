library(shiny)

if (!requireNamespace("shinyjs", quietly = TRUE)) install.packages("shinyjs")
if (!requireNamespace("dplyr", quietly = TRUE)) install.packages("dplyr")
if (!requireNamespace("tidyr", quietly = TRUE)) install.packages("tidyr")
if (!requireNamespace("DT", quietly = TRUE)) install.packages("DT")

library(shinyjs)
library(dplyr)
library(tidyr)
library(DT)

function(input, output, session) {
  observeEvent(input$generate,{
    output$output_text <- renderText({"Working... Please Wait."}) # Message for user patience 
    delay(100, { # Delay to display message correctly 
      
    combined_data <- reactiveVal(NULL)
    msstats_data <- reactiveVal(NULL)
    ttest_data <- reactiveVal(NULL)
    data <- list()
    
      # Merge SAINT lists
    
      # Iterate over each file path in the input
      for (i in seq_along(input$saintList$datapath)) {
        filepath <- input$saintList$datapath[i]
        # Read the file into a data frame
        df <- read.table(filepath, header = TRUE, sep = "\t")
        # Append the data frame to the list
        data[[i]] <- df
      }
      
      # Combine all data frames into one
      combined_df <- do.call(rbind, data)
      
      # Assign the combined data frame to combined_data
      combined_data(combined_df)
       # Process Msstats
      
      msstats_df <- read.table(input$msstatsFile$datapath, header = TRUE, sep = "\t")
      msstats_df$log2FC <- replace(msstats_df$log2FC, is.infinite(msstats_df$log2FC) & msstats_df$log2FC < 0, -5)
      msstats_df$log2FC <- replace(msstats_df$log2FC, is.infinite(msstats_df$log2FC) & msstats_df$log2FC > 0, 5)
      msstats_df <- msstats_df %>%
        separate(Label, into = c("Bait", "Control"), sep = "-") %>%
        rename(Prey = Protein)
      msstats_data(msstats_df)

      
        # Process Ttest
   
      ttest_df <- read.table(input$ttestFile$datapath, header = TRUE, sep = "\t")
      ttest_df <- ttest_df %>%
        filter(!grepl(";", Prey))%>%
      filter(!grepl("CON_", Prey))
      ttest_data(ttest_df)
      
      # Merge all tables
      merged_df <- combined_data() %>%
        full_join(msstats_data(), by = c("Bait", "Prey")) %>%
        full_join(ttest_data(), by = c("Bait", "Prey"))
      
    
      final_results <-merged_df %>% mutate(
        BFDR_col = if ("BFDR" %in% names(.)) {BFDR} else if ("BFDR.x" %in% names(.) && "BFDR.y" %in% names(.) ) {c(BFDR.x,BFDR.y)},
        
        BFDR_col_name = if ("BFDR" %in% names(.)) {paste(Bait, "BFDR", sep = "-")} else if ("BFDR.x" %in% names(.)
                                                                                            && "BFDR.y" %in% names(.)) {
          c(paste(Bait, "BFDR.spc", sep = "-"),paste(Bait, "BFDR.int", sep = "-"))},
        Ttest_pvalue_col_name = paste(Bait, "Ttest_pvalue", sep = "-"),
        MSstats_adj_pvalue_col_name = paste(Bait, "MSstats_adj.pvalue", sep = "-"),
        log2FC_col_name = paste(Bait, "log2FC", sep = "-")
      )%>%
        pivot_wider(names_from = BFDR_col_name, values_from = BFDR_col) %>%
        pivot_wider(names_from = Ttest_pvalue_col_name, values_from = p.value) %>%
        pivot_wider(names_from = MSstats_adj_pvalue_col_name, values_from = adj.pvalue) %>%
        pivot_wider(names_from = log2FC_col_name, values_from = log2FC) %>%
        # Select only Prey and the newly made columns
        select(Prey,PreyGene, ends_with("-BFDR"), ends_with("-Ttest_pvalue"), ends_with("-MSstats_adj.pvalue"), ends_with("-log2FC")) %>%
        select(Prey,PreyGene, sort(setdiff(names(.), c("Prey","PreyGene")))) %>%
      group_by(Prey) %>%
        summarize(across(everything(), ~ paste(na.omit(unique(unlist(.))), collapse = ", ")), .groups = 'drop')
        
      
      
      output$mergedTable <- renderDT({final_results})
      output$output_text <- renderText({"Done."})
      
    })
 
    output$download_mergedTable <- downloadHandler(
      filename = function() { "RawDataset.txt" },
      content = function(file) {
        write.table(merged_df, file, sep = "\t", row.names = FALSE, quote = FALSE)})
    
    output$download_final_results <- downloadHandler(
      filename = function() { "FullDataset.txt" },
      content = function(file) {
        write.table(final_results, file, sep = "\t", row.names = FALSE, quote = FALSE)})
  })
}