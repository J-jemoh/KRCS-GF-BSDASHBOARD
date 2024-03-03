source("dependencies.R")
server = function(input, output, session) {
    #agregation by age
    output$age_group<-renderDT({
        selected_outreach <- mutate(AYP, Age_Group = case_when(
            age >= 14 & age <= 17 ~ "14-17",
            age >= 18 & age <= 24 ~ "18-24",
            TRUE ~ "Other"
        ))
        
        # Group by region and age group, then count occurrences
        table_outreach <- selected_outreach %>%
            group_by(region, Age_Group) %>%
            summarize(Count = n(), .groups = 'drop')
        
        # Reshape data for table
        table_outreach <- table_outreach %>%
            pivot_wider(names_from = Age_Group, values_from = Count, names_prefix = "Count_")
        # Rename columns
        colnames(table_outreach) <- gsub("Count_", "", colnames(table_outreach))
        colnames(table_outreach) <- gsub("-", " to ", colnames(table_outreach))
        
        # Convert back to data frame
        table_outreach <- as.data.frame(table_outreach)
        
        # Display as DT table
        datatable(table_outreach, options = list(searching = FALSE), 
                  caption = "Count of Outreach by Region and Age Group")%>%
            formatStyle(names(table_outreach), textAlign = "center",
                        background = styleColorBar( c(0, max(c(table_outreach$`14to17`,table_outreach$`18to24`))*2, na.rm=T) , 'lightblue'),
                        backgroundSize = '100% 90%',
                        backgroundRepeat = 'no-repeat',
                        backgroundPosition = 'center',
            )
    })
    output$gender_county<-renderDT({
        table_outreach <- AYP %>%
            group_by(region, sex) %>%
            summarize(Count = n(), .groups = 'drop')
        
        # Reshape data for table
        table_outreach <- table_outreach %>%
            pivot_wider(names_from = sex, values_from = Count, names_prefix = "Count_")
        # Rename columns
        colnames(table_outreach) <- gsub("Count_", "", colnames(table_outreach))
        colnames(table_outreach) <- gsub("-", " to ", colnames(table_outreach))
        # Convert back to data frame
        table_outreach <- as.data.frame(table_outreach)
        
        # Display as DT table
        datatable(table_outreach, options = list(searching = FALSE), 
                  caption = "Count of Outreach by Region and Gender") %>%
            formatStyle(names(table_outreach),
                        background = styleColorBar( c(0, max(c(table_outreach$Male,table_outreach$Female))*2, na.rm=T) , 'lightblue'),
                        backgroundSize = '100% 90%',
                        backgroundRepeat = 'no-repeat',
                        backgroundPosition = 'center',
                        textAlign = 'center'
            )
    })
    #bar charts
    output$age1_barchartt<-renderPlotly({
        # Define age groups
        AYP <- mutate(AYP, Age_Group = case_when(
            age >= 14 & age <= 17 ~ "14-17",
            age >= 18 & age <= 24 ~ "18-24",
            TRUE ~ "Other"
        ))
        
        # Group by region and age group, then count occurrences
        table_outreach <- AYP %>%
            group_by(region, Age_Group) %>%
            summarize(Count = n(), .groups = 'drop')
        
        # Reshape data for plot
        table_outreach <- table_outreach %>%
            pivot_wider(names_from = Age_Group, values_from = Count)
        
        # Rename columns to remove hyphens
        names(table_outreach) <- gsub("-", "to", names(table_outreach))
        
        # Create bar chart with Plotly
        plot_ly(table_outreach, x = ~region, y = ~`14to17`, type = 'bar', name = '14-17') %>%
            add_trace(y = ~`18to24`, name = '18-24') %>%
            layout(title = "Count of Outreach by Region and Age Group",
                   xaxis = list(title = "Region"),
                   yaxis = list(title = "Count"))
    })
    output$gender1_county1<-renderPlotly({
        table_outreach <- AYP %>%
            group_by(region, sex) %>%
            summarize(Count = n(), .groups = 'drop')
        
        # Reshape data for table
        table_outreach <- table_outreach %>%
            pivot_wider(names_from = sex, values_from = Count, names_prefix = "Count_")
        
        # Rename columns
        colnames(table_outreach) <- gsub("Count_", "", colnames(table_outreach))
        colnames(table_outreach) <- gsub("-", " to ", colnames(table_outreach))
        
        # Convert back to data frame
        table_outreach <- as.data.frame(table_outreach)
        
        # Create bar chart
        bar_chart <- plot_ly(data = table_outreach, x = ~region, y = ~Female, 
                             type = 'bar', 
                             name = 'Female', 
                             marker = list(color = 'rgb(255, 51, 102)')) %>%
            add_trace(x = ~region, y = ~Male, 
                      name = 'Male', 
                      marker = list(color = 'rgb(51, 102, 255)')) %>%
            layout(title = "Count of Outreach by Region and Gender", 
                   xaxis = list(title = "Region"),
                   yaxis = list(title = "Count"),
                   barmode = 'group')
        
        # Print the bar chart
        bar_chart
        
    })
    #hcbf ebi
    output$EBI_individual<-renderUI({
        tags$table(class = "table table-striped table-bordered table-hover",
                   tags$thead(
                       tags$tr(
                           tags$th("coverage Indicator"),
                           tags$td(style="font-weight:bold;","Quater 9" ),
                           # tags$td(style="font-weight:bold;","Data as at today(",Sys.Date(),")"),
                           tags$th(style="text-align:center","Quater 10"),
                           tags$th(style="text-align:center","Sem"),
                           tags$th(style="text-align:center","Quater 11"),
                           tags$th(style="text-align:center","Quater 12"),
                           tags$th(style="text-align:center","Sem")
                           
                       )),
                   tags$tbody(
                       tags$tr(
                           tags$td(style="font-weight:bold;", "#HBCF"),
                           tags$td(style="text-align:center",hbcf_complete_q9),
                           # tags$td(style="text-align:center",),
                           tags$td(style="text-align:center",hbcf_complete_10),
                           tags$td(style="text-align:center",hbcf_complete_sem),
                           tags$td(style="text-align:center",),
                           tags$td(style="text-align:center",),
                           tags$td(style="text-align:center",)
                           
                       ),   
                       tags$tr(
                           tags$td(style="font-weight:bold;", "MHMC"),
                           tags$td(style="text-align:center",mhmc_complete_q9),
                           # tags$td(style="text-align:center",),
                           tags$td(style="text-align:center",mhmc_complete_q10),
                           tags$td(style="text-align:center",mhmc_complete_sem),
                           tags$td(style="text-align:center",),
                           tags$td(style="text-align:center",),
                           tags$td(style="text-align:center",)
                           
                       ),
                       tags$tr(
                           tags$td(style="font-weight:bold;", "Mentorship"),
                           tags$td(style="text-align:center",mentorship_complete_q9),
                           # tags$td(style="text-align:center",),
                           tags$td(style="text-align:center",mentorship_complete_q10),
                           tags$td(style="text-align:center",mentorship_complete_sem),
                           tags$td(style="text-align:center",),
                           tags$td(style="text-align:center",),
                           tags$td(style="text-align:center",)
                           
                       ),
                       tags$tr(style="background-color:grey",
                           tags$td(style="font-weight:bold;", "Total"),
                           tags$td(style="text-align:center",mentorship_complete_q9+mhmc_complete_q9+hbcf_complete_q9),
                           # tags$td(style="text-align:center",),
                           tags$td(style="text-align:center",mentorship_complete_q10+mhmc_complete_q10+hbcf_complete_10),
                           tags$td(style="text-align:center",mentorship_complete_sem + hbcf_complete_sem+mhmc_complete_sem),
                           tags$td(style="text-align:center",),
                           tags$td(style="text-align:center",),
                           tags$td(style="text-align:center",)
                           
                       )
                       
                   )
        ) 
    })
    output$ayp_individual<-renderUI({
        tags$table(class = "table table-striped table-bordered table-hover",
                   tags$thead(
                              tags$tr(
                                  tags$th("coverage Indicator"),
                                  tags$td(style="font-weight:bold;","Quater 9" ),
                                  # tags$td(style="font-weight:bold;","Data as at today(",Sys.Date(),")"),
                                  tags$th(style="text-align:center","Quater 10"),
                                  tags$th(style="text-align:center","Sem"),
                                  tags$th(style="text-align:center","Quater 11"),
                                  tags$th(style="text-align:center","Quater 12"),
                                  tags$th(style="text-align:center","Sem")
                                  
                              )),
                   tags$tbody(
                       tags$tr(
                           tags$td(style="font-weight:bold;", "#of adolescent girls and young women reached with HIV prevention programs - defined package"),
                           tags$td(style="text-align:center",ayp_defined_9),
                           # tags$td(style="text-align:center",),
                           tags$td(style="text-align:center",ayp_defined_10),
                           tags$td(style="text-align:center",ayp_defined_sem),
                           tags$td(style="text-align:center",),
                           tags$td(style="text-align:center",),
                           tags$td(style="text-align:center",)
                           
                       ),   
                       tags$tr(
                           tags$td(style="font-weight:bold;", "# of young people aged 10–24 years reached by comprehensive sexuality education and/or life skills–based HIV education out of schools"),
                           tags$td(style="text-align:center",mentorship_complete_q9+mhmc_complete_q9+hbcf_complete_q9),
                           # tags$td(style="text-align:center",),
                           tags$td(style="text-align:center",mentorship_complete_q10+mhmc_complete_q10+hbcf_complete_10),
                           tags$td(style="text-align:center",mentorship_complete_sem + hbcf_complete_sem+mhmc_complete_sem),
                           tags$td(style="text-align:center",),
                           tags$td(style="text-align:center",),
                           tags$td(style="text-align:center",)
                           
                       )
                       
                   )
        ) 
    })
    #FSW hiv status at enrollment
    output$enrol_status<-renderDataTable({
        if(input$hivRegionInput=="All"){
            fsw_data<-fsw_consolidated
        }
        else{
            fsw_data<-subset(fsw_consolidated,region %in% input$hivRegionInput )  
        }
        table_outreach <- fsw_data %>%
            group_by(hiv_status_at_enrollment) %>%
            summarize(Count = n())
        total_count <- nrow(filter(ebi_MHMC))
        # table_outreach <- table_outreach %>%
        #     mutate(Percentage = round((Count / total_count) * 100,2))
        table_outreach$hiv_status_at_enrollment[is.na(table_outreach$hiv_status_at_enrollment) | table_outreach$hiv_status_at_enrollment == ""] <- "Missing"
        colnames(table_outreach) <- c("HIV Status at enrolment","COUNT")
        datatable(table_outreach, options =list(searching = FALSE))
        table_outreach
    })
    output$enrol_status_chart<-renderPlotly({
        # Create the table_outreach data frame
        if(input$hivRegionInput=="All"){
            fsw_data<-fsw_consolidated
        }
        else{
            fsw_data<-subset(fsw_consolidated,region %in% input$hivRegionInput )  
        }
        table_outreach <- fsw_data %>%
            count(hiv_status_at_enrollment, name = "Count") %>%
            mutate(Percentage = round(Count / nrow(fsw_consolidated) * 100, 2),
                   `HIV Status at Enrolment` = coalesce(hiv_status_at_enrollment, "Missing")) %>%
            select(`HIV Status at Enrolment`, Count, Percentage)
        
        # Create plotly pie chart
        pie_chart <- plot_ly(table_outreach, 
                             labels = ~`HIV Status at Enrolment`,
                             values = ~Count, 
                             type = 'pie',
                             marker = list(colors = c('#1f77b4', '#ff7f0e', '#2ca02c', '#d62728', '#9467bd', '#8c564b', '#e377c2', '#7f7f7f', '#bcbd22', '#17becf')),
                             hoverinfo = 'text')
        
        # Show the pie chart
        pie_chart
        
    })
    output$fsw_indicators<-renderUI({
        
        filtered_data<-if(input$indicatorRegionInput=="All"){
            fsw_consolidated
        }
        else{
            filtered_data<-subset(fsw_consolidated,region %in% input$indicatorRegionInput)
                peer_education_counts <- filtered_data %>%
                    summarise(across(starts_with("received_peer_education"), ~ sum(. == "Yes", na.rm = TRUE)))
                peer_education_counts <- peer_education_counts %>%
                    select(where(~ . > 0))
                
                # Count risk education services
                risk_services_counts <- filtered_data %>%
                    summarise(across(starts_with("provided_with_risk_reduction_couselling"), ~ sum(. == "Yes", na.rm = TRUE)))
                risk_services_counts <- risk_services_counts %>%
                    select(where(~ . > 0))
                
                #tested for hiv
                hiv_test_counts <- filtered_data %>%
                    summarise(across(starts_with("tested_for_hiv"), ~ sum(. == "Yes", na.rm = TRUE)))
                hiv_test_counts <- hiv_test_counts %>%
                    select(where(~ . > 0))
                #screened for TB
                Screened_tb_counts <- filtered_data %>%
                    summarise(across(starts_with("screened_for_tb"), ~ sum(. == "Yes", na.rm = TRUE)))
                Screened_tb_counts <- Screened_tb_counts %>%
                    select(where(~ . > 0))
                # count all those who received condoms
                received_condom_counts <- filtered_data %>%
                    summarise(across(starts_with("condoms_distributed_nmbr"), ~ sum(. > 0, na.rm = TRUE)))
                received_condom_counts <- received_condom_counts %>%
                    select(where(~ . > 0))
                #count those who were screened for STI
                sti_counts <- filtered_data %>%
                    summarise(across(starts_with("screened_for_stis"), ~ sum(. == "Yes", na.rm = TRUE)))
                sti_counts <- sti_counts %>%
                    select(where(~ . > 0))
                
                # Combine the counts into one table
                combined_counts <- bind_rows(
                    peer_education = peer_education_counts,
                    risk_services = risk_services_counts,
                    hiv_tests=hiv_test_counts,
                    Screened_tb=Screened_tb_counts,
                    received_condom=received_condom_counts,
                    sti_counts_r=sti_counts,
                    .id = "Service_Type"
                ) 
        }
        pe_july<- combined_counts$received_peer_education22[!is.na(combined_counts$received_peer_education22)][1]
        pe_august<- combined_counts$received_peer_education75[!is.na(combined_counts$received_peer_education75)][1]
        pe_sep<- combined_counts$received_peer_education128[!is.na(combined_counts$received_peer_education128)][1]
        pe_oct<- combined_counts$received_peer_education181[!is.na(combined_counts$received_peer_education181)][1]
        pe_nov<- combined_counts$received_peer_education234[!is.na(combined_counts$received_peer_education234)][1]
        pe_dec<- combined_counts$received_peer_education287[!is.na(combined_counts$received_peer_education287)][1]
        #risk reduction 
        rrc_july<- combined_counts$provided_with_risk_reduction_couselling68[!is.na(combined_counts$provided_with_risk_reduction_couselling68)][1]
        rrc_august<- combined_counts$provided_with_risk_reduction_couselling121[!is.na(combined_counts$provided_with_risk_reduction_couselling121)][1]
        rrc_sep<- combined_counts$provided_with_risk_reduction_couselling174[!is.na(combined_counts$provided_with_risk_reduction_couselling174)][1]
        rrc_oct<- combined_counts$provided_with_risk_reduction_couselling227[!is.na(combined_counts$provided_with_risk_reduction_couselling227)][1]
        rrc_nov<- combined_counts$provided_with_risk_reduction_couselling280[!is.na(combined_counts$provided_with_risk_reduction_couselling280)][1]
        rrc_dec<- combined_counts$provided_with_risk_reduction_couselling333[!is.na(combined_counts$provided_with_risk_reduction_couselling333)][1]
        #tested for hiv
        hiv_july<- combined_counts$tested_for_hiv24[!is.na(combined_counts$tested_for_hiv24)][1]
        hiv_august<- combined_counts$tested_for_hiv77[!is.na(combined_counts$tested_for_hiv77)][1]
        hiv_sep<- combined_counts$tested_for_hiv130[!is.na(combined_counts$tested_for_hiv130)][1]
        hiv_oct<- combined_counts$tested_for_hiv183[!is.na(combined_counts$tested_for_hiv183)][1]
        hiv_nov<- combined_counts$tested_for_hiv236[!is.na(combined_counts$tested_for_hiv236)][1]
        hiv_dec<- combined_counts$tested_for_hiv289[!is.na(combined_counts$tested_for_hiv289)][1]
        #Screened for TB
        stb_july<- combined_counts$screened_for_tb39[!is.na(combined_counts$screened_for_tb39)][1]
        stb_august<- combined_counts$screened_for_tb92[!is.na(combined_counts$screened_for_tb92)][1]
        stb_sep<- combined_counts$screened_for_tb145[!is.na(combined_counts$screened_for_tb145)][1]
        stb_oct<- combined_counts$screened_for_tb198[!is.na(combined_counts$screened_for_tb198)][1]
        stb_nov<- combined_counts$screened_for_tb251[!is.na(combined_counts$screened_for_tb251)][1]
        stb_dec<- combined_counts$screened_for_tb304[!is.na(combined_counts$screened_for_tb304)][1]
        #Received Condoms
        cnd_july<- combined_counts$condoms_distributed_nmbr46[!is.na(combined_counts$condoms_distributed_nmbr46)][1]
        cnd_august<- combined_counts$condoms_distributed_nmbr99[!is.na(combined_counts$condoms_distributed_nmbr99)][1]
        cnd_sep<- combined_counts$condoms_distributed_nmbr152[!is.na(combined_counts$condoms_distributed_nmbr152)][1]
        cnd_oct<- combined_counts$condoms_distributed_nmbr205[!is.na(combined_counts$condoms_distributed_nmbr205)][1]
        cnd_nov<- combined_counts$condoms_distributed_nmbr258[!is.na(combined_counts$condoms_distributed_nmbr258)][1]
        cnd_dec<- combined_counts$condoms_distributed_nmbr311[!is.na(combined_counts$condoms_distributed_nmbr311)][1]
        #screened for STIS
        sti_july<- combined_counts$screened_for_stis61[!is.na(combined_counts$screened_for_stis61)][1]
        sti_august<- combined_counts$screened_for_stis114[!is.na(combined_counts$screened_for_stis114)][1]
        sti_sep<- combined_counts$screened_for_stis167[!is.na(combined_counts$screened_for_stis167)][1]
        sti_oct<- combined_counts$screened_for_stis220[!is.na(combined_counts$screened_for_stis220)][1]
        sti_nov<- combined_counts$screened_for_stis273[!is.na(combined_counts$screened_for_stis273)][1]
        sti_dec<- combined_counts$screened_for_stis326[!is.na(combined_counts$screened_for_stis326)][1]
        
        tags$table(class = "table table-striped table-bordered table-hover",
                   tags$thead(
                              tags$tr(
                                  tags$th("Type"),
                                  tags$td(style="font-weight:bold;","July" ),
                                  tags$th(style="text-align:center","August"),
                                  tags$th(style="text-align:center","September"),
                                  tags$th(style="text-align:center","October"),
                                  tags$th(style="text-align:center","November"),
                                  tags$th(style="text-align:center","December")
                                  
                              )),
                   tags$tbody(
                       tags$tr(
                           tags$td(style="font-weight:bold;", "Received Peer Education"),
                           tags$td(style="text-align:center",pe_july),
                           tags$td(style="text-align:center",pe_august),
                           tags$td(style="text-align:center",pe_sep),
                           tags$td(style="text-align:center",pe_oct),
                           tags$td(style="text-align:center",pe_nov),
                           tags$td(style="text-align:center",pe_dec)
                           
                       ),   
                       tags$tr(
                           tags$td(style="font-weight:bold;", "Provided with Risk reduction Counselling"),
                           tags$td(style="text-align:center",rrc_july),
                           # tags$td(style="text-align:center",),
                           tags$td(style="text-align:center",rrc_august),
                           tags$td(style="text-align:center",rrc_sep),
                           tags$td(style="text-align:center",rrc_oct),
                           tags$td(style="text-align:center",rrc_nov),
                           tags$td(style="text-align:center",rrc_dec)
                           
                       ),  
                       tags$tr(
                           tags$td(style="font-weight:bold;", "Tested for HIV"),
                           tags$td(style="text-align:center",hiv_july),
                           # tags$td(style="text-align:center",),
                           tags$td(style="text-align:center",hiv_august),
                           tags$td(style="text-align:center",hiv_sep),
                           tags$td(style="text-align:center",hiv_oct),
                           tags$td(style="text-align:center",hiv_nov),
                           tags$td(style="text-align:center",hiv_dec)
                           
                           
                       ), 
                       tags$tr(
                           tags$td(style="font-weight:bold;","Screened for TB"),
                           tags$td(style="text-align:center",stb_july),
                           # tags$td(style="text-align:center",),
                           tags$td(style="text-align:center",stb_august),
                           tags$td(style="text-align:center",stb_sep),
                           tags$td(style="text-align:center",stb_oct),
                           tags$td(style="text-align:center",stb_nov),
                           tags$td(style="text-align:center",stb_dec)
                           
                       ), 
                       tags$tr(
                           tags$td(style="font-weight:bold;","Received Condoms"),
                           tags$td(style="text-align:center",cnd_july),
                           # tags$td(style="text-align:center",),
                           tags$td(style="text-align:center",cnd_august),
                           tags$td(style="text-align:center",cnd_sep),
                           tags$td(style="text-align:center",cnd_oct),
                           tags$td(style="text-align:center",cnd_nov),
                           tags$td(style="text-align:center",cnd_dec)
                          
                       ),
                       tags$tr(
                           tags$td(style="font-weight:bold;","Screened for STI"),
                           tags$td(style="text-align:center",sti_july),
                           # tags$td(style="text-align:center",),
                           tags$td(style="text-align:center",sti_august),
                           tags$td(style="text-align:center",sti_sep),
                           tags$td(style="text-align:center",sti_oct),
                           tags$td(style="text-align:center",sti_nov),
                           tags$td(style="text-align:center",sti_dec)
                       )
                       
                   )
        )
    })
    output$fsw_indicators_age<-renderUI({
        
        filtered_data<-if(input$ageRegionInput=="All"){
            fsw_consolidated
        }
        else{
            filtered_data<-subset(fsw_consolidated,region %in% input$ageRegionInput)
        }
        peer_education_counts <- filtered_data %>%
            filter(age >= 10 & age <= 24) %>%
            summarise(across(starts_with("received_peer_education"), ~ sum(. == "Yes", na.rm = TRUE)))
        peer_education_counts <- peer_education_counts %>%
            select(where(~ . > 0))
        peer_education_counts_25 <- filtered_data %>%
            filter(age >= 25 & age <= 50) %>%
            summarise(across(starts_with("received_peer_education"), ~ sum(. == "Yes", na.rm = TRUE)))
        peer_education_counts_25 <- peer_education_counts_25 %>%
            select(where(~ . > 0))
        peer_education_counts_50 <- filtered_data %>%
            filter(age >= 51 & age <= 100) %>%
            summarise(across(starts_with("received_peer_education"), ~ sum(. == "Yes", na.rm = TRUE)))
        peer_education_counts_50 <- peer_education_counts_50 %>%
            select(where(~ . > 0))
        risk_services_counts <- filtered_data %>%
            filter(age >= 10 & age <= 24) %>%
            summarise(across(starts_with("provided_with_risk_reduction_couselling"), ~ sum(. == "Yes", na.rm = TRUE)))
        risk_services_counts <- risk_services_counts %>%
            select(where(~ . > 0))
        risk_services_counts_25 <- filtered_data %>%
            filter(age >= 25 & age <= 50) %>%
            summarise(across(starts_with("provided_with_risk_reduction_couselling"), ~ sum(. == "Yes", na.rm = TRUE)))
        risk_services_counts_25 <- risk_services_counts_25 %>%
            select(where(~ . > 0))
        risk_services_counts_50 <- filtered_data %>%
            filter(age >= 51 & age <= 100) %>%
            summarise(across(starts_with("provided_with_risk_reduction_couselling"), ~ sum(. == "Yes", na.rm = TRUE)))
        risk_services_counts_50 <- risk_services_counts_50 %>%
            select(where(~ . > 0))
        #tested for hiv
        hiv_test_counts <- filtered_data %>%
            filter(age >= 10 & age <= 24) %>%
            summarise(across(starts_with("tested_for_hiv"), ~ sum(. == "Yes", na.rm = TRUE)))
        hiv_test_counts <- hiv_test_counts %>%
            select(where(~ . > 0))
        hiv_test_counts_25 <- filtered_data %>%
            filter(age >= 25 & age <= 50) %>%
            summarise(across(starts_with("tested_for_hiv"), ~ sum(. == "Yes", na.rm = TRUE)))
        hiv_test_counts_25 <- hiv_test_counts_25 %>%
            select(where(~ . > 0))
        hiv_test_counts_50 <- filtered_data %>%
            filter(age >= 51 & age <= 100) %>%
            summarise(across(starts_with("tested_for_hiv"), ~ sum(. == "Yes", na.rm = TRUE)))
        hiv_test_counts_50 <- hiv_test_counts_50 %>%
            select(where(~ . > 0))
        Screened_tb_counts <- filtered_data %>%
            filter(age >= 10 & age <= 24) %>%
            summarise(across(starts_with("screened_for_tb"), ~ sum(. == "Yes", na.rm = TRUE)))
        Screened_tb_counts <- Screened_tb_counts %>%
            select(where(~ . > 0))
        Screened_tb_counts_25 <- filtered_data %>%
            filter(age >= 25 & age <= 50) %>%
            summarise(across(starts_with("screened_for_tb"), ~ sum(. == "Yes", na.rm = TRUE)))
        Screened_tb_counts_25 <- Screened_tb_counts_25 %>%
            select(where(~ . > 0))
        Screened_tb_counts_50 <- filtered_data %>%
            filter(age >= 51 & age <= 100) %>%
            summarise(across(starts_with("screened_for_tb"), ~ sum(. == "Yes", na.rm = TRUE)))
        Screened_tb_counts_50 <- Screened_tb_counts_50 %>%
            select(where(~ . > 0))
        # count all those who received condoms
        received_condom_counts <- filtered_data %>%
            filter(age >= 10 & age <= 24) %>%
            summarise(across(starts_with("condoms_distributed_nmbr"), ~ sum(. > 0, na.rm = TRUE)))
        received_condom_counts <- received_condom_counts %>%
            select(where(~ . > 0))
        received_condom_counts_25 <- filtered_data %>%
            filter(age >= 25 & age <= 50) %>%
            summarise(across(starts_with("condoms_distributed_nmbr"), ~ sum(. > 0, na.rm = TRUE)))
        received_condom_counts_25 <- received_condom_counts_25 %>%
            select(where(~ . > 0))
        received_condom_counts_50 <- filtered_data %>%
            filter(age >= 51 & age <= 100) %>%
            summarise(across(starts_with("condoms_distributed_nmbr"), ~ sum(. > 0, na.rm = TRUE)))
        received_condom_counts_50 <- received_condom_counts_50 %>%
            select(where(~ . > 0))
        #count those who were screened for STI
        sti_counts <- filtered_data %>%
            filter(age >= 10 & age <= 24) %>%
            summarise(across(starts_with("screened_for_stis"), ~ sum(. == "Yes", na.rm = TRUE)))
        sti_counts <- sti_counts %>%
            select(where(~ . > 0))
        sti_counts_24 <- filtered_data %>%
            filter(age >= 25 & age <= 50) %>%
            summarise(across(starts_with("screened_for_stis"), ~ sum(. == "Yes", na.rm = TRUE)))
        sti_counts_24 <- sti_counts_24 %>%
            select(where(~ . > 0))
        sti_counts_50 <- filtered_data %>%
            filter(age >= 51 & age <= 100) %>%
            summarise(across(starts_with("screened_for_stis"), ~ sum(. == "Yes", na.rm = TRUE)))
        sti_counts_50 <- sti_counts_50 %>%
            select(where(~ . > 0))
        
        tags$table(class = "table table-striped table-bordered table-hover",
                   tags$thead(
                       tags$tr(
                           tags$th("Type"),
                           tags$td(style="font-weight:bold;text-align:center","10-24" ),
                           tags$th(style="text-align:center","25-50"),
                           tags$th(style="text-align:center","> 50")
                           
                       )),
                   tags$tbody(
                       tags$tr(
                           tags$td(style="font-weight:bold;", "Received Peer Education"),
                           tags$td(style="text-align:center",max(peer_education_counts)),
                           tags$td(style="text-align:center",max(peer_education_counts_25)),
                           tags$td(style="text-align:center",max(peer_education_counts_50))

                           
                       ),   
                       tags$tr(
                           tags$td(style="font-weight:bold;", "Provided with Risk reduction Counselling"),
                           tags$td(style="text-align:center",max(risk_services_counts)),
                           tags$td(style="text-align:center",max(risk_services_counts_25)),
                           tags$td(style="text-align:center",max(risk_services_counts_50))

                           
                       ),  
                       tags$tr(
                           tags$td(style="font-weight:bold;", "Tested for HIV"),
                           tags$td(style="text-align:center",max(hiv_test_counts)),
                           # tags$td(style="text-align:center",),
                           tags$td(style="text-align:center",max(hiv_test_counts_25)),
                           tags$td(style="text-align:center",max(hiv_test_counts_50))
                           
                           
                       ), 
                       tags$tr(
                           tags$td(style="font-weight:bold;","Screened for TB"),
                           tags$td(style="text-align:center",max(Screened_tb_counts)),
                           # tags$td(style="text-align:center",),
                           tags$td(style="text-align:center",max(Screened_tb_counts_25)),
                           tags$td(style="text-align:center",max(Screened_tb_counts_50))
                           
                       ), 
                       tags$tr(
                           tags$td(style="font-weight:bold;","Received Condoms"),
                           tags$td(style="text-align:center",max(received_condom_counts)),
                           tags$td(style="text-align:center",max(received_condom_counts_25)),
                           tags$td(style="text-align:center",max(received_condom_counts_50))
                           
                       ),
                       tags$tr(
                           tags$td(style="font-weight:bold;","Screened for STI"),
                           tags$td(style="text-align:center",max(sti_counts)),
                           # tags$td(style="text-align:center",),
                           tags$td(style="text-align:center",max(sti_counts_24)),
                           tags$td(style="text-align:center",max(sti_counts_50))

                       )
                       
                   )
        )
    })
    #fsw key indicator summaary
    output$fsw_defined<-renderUI({
        filtered_data<-if(input$definedRegionInput=="All"){
            fsw_consolidated
        }else{
            filtered_data<-subset(fsw_consolidated,region %in% input$definedRegionInput)
        }
            #defined packages indcators
            q9_test_hiv <- filtered_data %>%
                summarise(across(starts_with("tested_for_hiv")[1:3], ~ sum(. == "Yes", na.rm = TRUE))) %>%
                summarise(max_value = max(c_across(everything()))) %>%
                pull()
            #q10 hiv
            q10_test_hiv <- filtered_data %>%
                summarise(across(starts_with("tested_for_hiv")[3:6], ~ sum(. == "Yes", na.rm = TRUE))) %>%
                summarise(max_value = max(c_across(everything()))) %>%
                pull()
            #q9 initiated on prep
            q9_prep_initiated <- filtered_data %>%
                summarise(across(starts_with("initiated_on_prep")[1:3], ~ sum(. == "Yes", na.rm = TRUE))) %>%
                summarise(max_value = max(c_across(everything()))) %>%
                pull()
            #q9 initiated on prep
            q10_prep_initiated <- filtered_data %>%
                summarise(across(starts_with("initiated_on_prep")[3:6], ~ sum(. == "Yes", na.rm = TRUE))) %>%
                summarise(max_value = max(c_across(everything()))) %>%
                pull()
            sem_hiv_test<-sum(
                nrow(filter(
                    filtered_data,(tested_for_hiv24=="Yes"|tested_for_hiv77=="Yes"|tested_for_hiv130=="Yes") & (tested_for_hiv183=="Yes"|tested_for_hiv236=="Yes"|
                                                                                                                       tested_for_hiv289=="Yes")
                ))
            )
            
            #sem intiated on prep
            sem_prep<-sum(
                nrow(filter(
                    filtered_data,initiated_on_prep65=="Yes"|initiated_on_prep118=="Yes"|initiated_on_prep171=="Yes" & initiated_on_prep224=="Yes"|initiated_on_prep277=="Yes"|
                        initiated_on_prep330=="Yes"
                ))
            )
            
            # combine package distribution
            combo_july<-nrow(filter(filtered_data,screened_for_stis61=="Yes",provided_with_risk_reduction_couselling68=="Yes"|received_peer_education22=="Yes",condoms_distributed_nmbr46>0))
            combo_august<-nrow(filter(filtered_data,screened_for_stis114=="Yes",provided_with_risk_reduction_couselling121=="Yes"|received_peer_education75=="Yes",condoms_distributed_nmbr99>0))
            combo_sep<-nrow(filter(filtered_data,screened_for_stis167=="Yes",provided_with_risk_reduction_couselling174=="Yes"|received_peer_education128=="Yes",condoms_distributed_nmbr152>0))
            
            combo_oct<-nrow(filter(filtered_data,screened_for_stis220=="Yes",provided_with_risk_reduction_couselling227=="Yes"|received_peer_education181=="Yes",condoms_distributed_nmbr205>0))
            combo_nov<-nrow(filter(filtered_data,screened_for_stis273=="Yes",provided_with_risk_reduction_couselling280=="Yes"|received_peer_education234=="Yes",condoms_distributed_nmbr258>0))
            combo_dec<-nrow(filter(filtered_data,screened_for_stis326=="Yes",provided_with_risk_reduction_couselling333=="Yes"|received_peer_education287=="Yes",condoms_distributed_nmbr311>0))
            
            #check those with defined packages
            
            fsw_consolidated <- filtered_data %>%
                mutate(
                    combo_condition = (screened_for_stis61 == "Yes" & 
                                           (provided_with_risk_reduction_couselling68 == "Yes" | received_peer_education22 == "Yes") &
                                           condoms_distributed_nmbr46 > 0) |
                        (screened_for_stis114 == "Yes" & 
                             (provided_with_risk_reduction_couselling121 == "Yes" | received_peer_education75 == "Yes") &
                             condoms_distributed_nmbr99 > 0) |
                        (screened_for_stis167 == "Yes" & 
                             (provided_with_risk_reduction_couselling174 == "Yes" | received_peer_education128 == "Yes") &
                             condoms_distributed_nmbr152 > 0)
                )
            
            # Count the number of rows meeting the combined condition
            combo_count <- fsw_consolidated %>%
                summarise(combo_july_to_sep = sum(combo_condition, na.rm = TRUE))
            
            #
            fsw_consolidated <- filtered_data %>%
                mutate(
                    combo_condition_oct_to_dec = (screened_for_stis220 == "Yes" & 
                                                      (provided_with_risk_reduction_couselling227 == "Yes" | received_peer_education181 == "Yes") &
                                                      condoms_distributed_nmbr205 > 0) |
                        (screened_for_stis273 == "Yes" & 
                             (provided_with_risk_reduction_couselling280 == "Yes" | received_peer_education234 == "Yes") &
                             condoms_distributed_nmbr258 > 0) |
                        (screened_for_stis326 == "Yes" & 
                             (provided_with_risk_reduction_couselling333 == "Yes" | received_peer_education287 == "Yes") &
                             condoms_distributed_nmbr311 > 0)
                )
            
            # Count the number of rows meeting the combined conditions for October to December
            combo_count_oct_to_dec <- fsw_consolidated %>%
                summarise(combo_oct_to_dec = sum(combo_condition_oct_to_dec, na.rm = TRUE))
            
            #comb  juy to dec
            combo_july_to_dec <- sum(
                nrow(filter(filtered_data, screened_for_stis61 == "Yes" & (provided_with_risk_reduction_couselling68 == "Yes" | received_peer_education22 == "Yes") & condoms_distributed_nmbr46 > 0|
                                screened_for_stis114 == "Yes" & (provided_with_risk_reduction_couselling121 == "Yes" | received_peer_education75 == "Yes") & condoms_distributed_nmbr99 > 0|
                                screened_for_stis167 == "Yes"& (provided_with_risk_reduction_couselling174 == "Yes" | received_peer_education128 == "Yes") & condoms_distributed_nmbr152 > 0|
                                screened_for_stis220 == "Yes"& (provided_with_risk_reduction_couselling227 == "Yes" | received_peer_education181 == "Yes") & condoms_distributed_nmbr205 > 0|
                                screened_for_stis273 == "Yes" &(provided_with_risk_reduction_couselling280 == "Yes" | received_peer_education234 == "Yes") & condoms_distributed_nmbr258 > 0|
                                screened_for_stis326 == "Yes"& (provided_with_risk_reduction_couselling333 == "Yes" | received_peer_education287 == "Yes") & condoms_distributed_nmbr311 > 0))
                
            )
        
        tags$table(class = "table table-striped table-bordered table-hover",
                   tags$thead(
                              tags$tr(
                                  tags$th("Type"),
                                  tags$td(style="font-weight:bold;","Quater 9" ),
                                  # tags$td(style="font-weight:bold;","Data as at today(",Sys.Date(),")"),
                                  tags$th(style="text-align:center","Quater 10"),
                                  tags$th(style="text-align:center","Semester"),
                                  
                              )),
                   tags$tbody(
                       tags$tr(
                           tags$td(style="font-weight:bold;", "#of sex workers that have received an HIV test during the reporting period and know their results"),
                           tags$td(style="text-align:center",q9_test_hiv),
                           # tags$td(style="text-align:center",),
                           tags$td(style="text-align:center",q10_test_hiv),
                           tags$td(style="text-align:center",sem_hiv_test),
                           
                       ),   
                       tags$tr(
                           tags$td(style="font-weight:bold;", "#of sex workers reached with HIV prevention programs - defined package"),
                           tags$td(style="text-align:center",combo_count$combo_july_to_sep),
                           # tags$td(style="text-align:center",),
                           tags$td(style="text-align:center",combo_count_oct_to_dec$combo_oct_to_dec),
                           tags$td(style="text-align:center",combo_july_to_dec),
                       ),  
                       tags$tr(
                           tags$td(style="font-weight:bold;", "#of eligible sex workers who initiated oral antiretroviral PrEP during the reporting period"),
                           tags$td(style="text-align:center",q9_prep_initiated),
                           # tags$td(style="text-align:center",),
                           tags$td(style="text-align:center",q10_prep_initiated),
                           tags$td(style="text-align:center",sem_prep),
                       ),
                       
                   )
        )
    })
    #msm hiv status
    output$enrol_status_msm<-renderDT({
        filtered_data <- if (input$msmhivRegionInput == "All") {
            msm_consolidated
        } else {
            subset(msm_consolidated, region %in% input$msmhivRegionInput)
        }
        
        table_outreach <- filtered_data %>%
            group_by(hiv_status_at_enrollment) %>%
            summarize(Count = n())
        table_outreach$hiv_status_at_enrollment[is.na(table_outreach$hiv_status_at_enrollment) | table_outreach$hiv_status_at_enrollment == ""] <- "Missing"
        colnames(table_outreach) <- c("HIV Status at enrolment","COUNT")
        datatable(table_outreach, options = list(config = list(searching = FALSE)))
        table_outreach
    })
    #MSM ENROLMENT BAR CHART
    output$enrol_status_chart_msm<-renderPlotly({
        filtered_data <- if (input$msmhivRegionInput == "All") {
            msm_consolidated
        } else {
            subset(msm_consolidated, region %in% input$msmhivRegionInput)
        }
        
        table_outreach <- filtered_data %>%
            group_by(hiv_status_at_enrollment) %>%
            summarize(Count = n())
        
        table_outreach$hiv_status_at_enrollment[is.na(table_outreach$hiv_status_at_enrollment) | table_outreach$hiv_status_at_enrollment == ""] <- "Missing"
        colnames(table_outreach) <- c("HIV Status at enrolment","COUNT")
        
        pie_chart <- plot_ly(table_outreach, labels = ~`HIV Status at enrolment`, values = ~COUNT, type = 'pie')
        
        pie_chart %>%
            layout(title = "HIV Status Distribution",
                   xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                   yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
    })
    #msm indicators
    output$msm_indicators<-renderUI({
        
        filtered_data_msm<-if(input$indicator_msm_RegionInput=="All"){
            msm_consolidated
        }
        else{
            filtered_data_msm<-subset(msm_consolidated,region %in% input$indicator_msm_RegionInput)
        }
            peer_education_counts_msm <- filtered_data_msm %>%
                summarise(across(starts_with("received_peer_education"), ~ sum(. == "Yes", na.rm = TRUE)))
            peer_education_counts_msm <- peer_education_counts_msm %>%
                select(where(~ . > 0))
            
            # Count risk education services
            risk_services_counts_msm <- filtered_data_msm %>%
                summarise(across(starts_with("provided_with_risk_reduction_couselling"), ~ sum(. == "Yes", na.rm = TRUE)))
            risk_services_counts_msm <- risk_services_counts_msm %>%
                select(where(~ . > 0))
            
            #tested for hiv
            hiv_test_counts_msm <- filtered_data_msm %>%
                summarise(across(starts_with("tested_for_hiv"), ~ sum(. == "Yes", na.rm = TRUE)))
            hiv_test_counts_msm <- hiv_test_counts_msm %>%
                select(where(~ . > 0))
            #screened for TB
            Screened_tb_counts_msm <- filtered_data_msm %>%
                summarise(across(starts_with("screened_for_tb"), ~ sum(. == "Yes", na.rm = TRUE)))
            Screened_tb_counts_msm <- Screened_tb_counts_msm %>%
                select(where(~ . > 0))
            # count all those who received condoms
            received_condom_counts_msm <- filtered_data_msm %>%
                summarise(across(starts_with("condoms_distributed_nmbr"), ~ sum(. > 0, na.rm = TRUE)))
            received_condom_counts_msm <- received_condom_counts_msm %>%
                select(where(~ . > 0))
            #count those who were screened for STI
            sti_counts_msm <- filtered_data_msm %>%
                summarise(across(starts_with("screened_for_stis"), ~ sum(. == "Yes", na.rm = TRUE)))
            sti_counts_msm <- sti_counts_msm %>%
                select(where(~ . > 0))
            
            # Combine the counts into one table
            combined_counts_msm<- bind_rows(
                peer_education = peer_education_counts_msm,
                risk_services = risk_services_counts_msm,
                hiv_tests=hiv_test_counts_msm,
                Screened_tb=Screened_tb_counts_msm,
                received_condom=received_condom_counts_msm,
                sti_counts_r=sti_counts_msm,
                .id = "Service_Type"
            ) 
        
        
        pe_july<- combined_counts_msm$received_peer_education22[!is.na(combined_counts_msm$received_peer_education22)][1]
        pe_august<- combined_counts_msm$received_peer_education75[!is.na(combined_counts_msm$received_peer_education75)][1]
        pe_sep<- combined_counts_msm$received_peer_education128[!is.na(combined_counts_msm$received_peer_education128)][1]
        pe_oct<- combined_counts_msm$received_peer_education181[!is.na(combined_counts_msm$received_peer_education181)][1]
        pe_nov<- combined_counts_msm$received_peer_education234[!is.na(combined_counts_msm$received_peer_education234)][1]
        pe_dec<- combined_counts_msm$received_peer_education287[!is.na(combined_counts_msm$received_peer_education287)][1]
        #risk reduction 
        rrc_july<- combined_counts_msm$provided_with_risk_reduction_couselling68[!is.na(combined_counts_msm$provided_with_risk_reduction_couselling68)][1]
        rrc_august<- combined_counts_msm$provided_with_risk_reduction_couselling121[!is.na(combined_counts_msm$provided_with_risk_reduction_couselling121)][1]
        rrc_sep<- combined_counts_msm$provided_with_risk_reduction_couselling174[!is.na(combined_counts_msm$provided_with_risk_reduction_couselling174)][1]
        rrc_oct<- combined_counts_msm$provided_with_risk_reduction_couselling227[!is.na(combined_counts_msm$provided_with_risk_reduction_couselling227)][1]
        rrc_nov<- combined_counts_msm$provided_with_risk_reduction_couselling280[!is.na(combined_counts_msm$provided_with_risk_reduction_couselling280)][1]
        rrc_dec<- combined_counts_msm$provided_with_risk_reduction_couselling333[!is.na(combined_counts_msm$provided_with_risk_reduction_couselling333)][1]
        #tested for hiv
        hiv_july<- combined_counts_msm$tested_for_hiv24[!is.na(combined_counts_msm$tested_for_hiv24)][1]
        hiv_august<- combined_counts_msm$tested_for_hiv77[!is.na(combined_counts_msm$tested_for_hiv77)][1]
        hiv_sep<- combined_counts_msm$tested_for_hiv130[!is.na(combined_counts_msm$tested_for_hiv130)][1]
        hiv_oct<- combined_counts_msm$tested_for_hiv183[!is.na(combined_counts_msm$tested_for_hiv183)][1]
        hiv_nov<- combined_counts_msm$tested_for_hiv236[!is.na(combined_counts_msm$tested_for_hiv236)][1]
        hiv_dec<- combined_counts_msm$tested_for_hiv289[!is.na(combined_counts_msm$tested_for_hiv289)][1]
        #Screened for TB
        stb_july<- combined_counts_msm$screened_for_tb39[!is.na(combined_counts_msm$screened_for_tb39)][1]
        stb_august<- combined_counts_msm$screened_for_tb92[!is.na(combined_counts_msm$screened_for_tb92)][1]
        stb_sep<- combined_counts_msm$screened_for_tb145[!is.na(combined_counts_msm$screened_for_tb145)][1]
        stb_oct<- combined_counts_msm$screened_for_tb198[!is.na(combined_counts_msm$screened_for_tb198)][1]
        stb_nov<- combined_counts_msm$screened_for_tb251[!is.na(combined_counts_msm$screened_for_tb251)][1]
        stb_dec<- combined_counts_msm$screened_for_tb304[!is.na(combined_counts_msm$screened_for_tb304)][1]
        #Received Condoms
        cnd_july<- combined_counts_msm$condoms_distributed_nmbr46[!is.na(combined_counts_msm$condoms_distributed_nmbr46)][1]
        cnd_august<- combined_counts_msm$condoms_distributed_nmbr99[!is.na(combined_counts_msm$condoms_distributed_nmbr99)][1]
        cnd_sep<- combined_counts_msm$condoms_distributed_nmbr152[!is.na(combined_counts_msm$condoms_distributed_nmbr152)][1]
        cnd_oct<- combined_counts_msm$condoms_distributed_nmbr205[!is.na(combined_counts_msm$condoms_distributed_nmbr205)][1]
        cnd_nov<- combined_counts_msm$condoms_distributed_nmbr258[!is.na(combined_counts_msm$condoms_distributed_nmbr258)][1]
        cnd_dec<- combined_counts_msm$condoms_distributed_nmbr311[!is.na(combined_counts_msm$condoms_distributed_nmbr311)][1]
        #screened for STIS
        sti_july<- combined_counts_msm$screened_for_stis61[!is.na(combined_counts_msm$screened_for_stis61)][1]
        sti_august<- combined_counts_msm$screened_for_stis114[!is.na(combined_counts_msm$screened_for_stis114)][1]
        sti_sep<- combined_counts_msm$screened_for_stis167[!is.na(combined_counts_msm$screened_for_stis167)][1]
        sti_oct<- combined_counts_msm$screened_for_stis220[!is.na(combined_counts_msm$screened_for_stis220)][1]
        sti_nov<- combined_counts_msm$screened_for_stis273[!is.na(combined_counts_msm$screened_for_stis273)][1]
        sti_dec<- combined_counts_msm$screened_for_stis326[!is.na(combined_counts_msm$screened_for_stis326)][1]
        
        tags$table(class = "table table-striped table-bordered table-hover",
                   tags$thead(
                       tags$tr(
                           tags$th("Type"),
                           tags$td(style="font-weight:bold;","July" ),
                           tags$th(style="text-align:center","August"),
                           tags$th(style="text-align:center","September"),
                           tags$th(style="text-align:center","October"),
                           tags$th(style="text-align:center","November"),
                           tags$th(style="text-align:center","December")
                           
                       )),
                   tags$tbody(
                       tags$tr(
                           tags$td(style="font-weight:bold;", "Received Peer Education"),
                           tags$td(style="text-align:center",pe_july),
                           tags$td(style="text-align:center",pe_august),
                           tags$td(style="text-align:center",pe_sep),
                           tags$td(style="text-align:center",pe_oct),
                           tags$td(style="text-align:center",pe_nov),
                           tags$td(style="text-align:center",pe_dec)
                           
                       ),   
                       tags$tr(
                           tags$td(style="font-weight:bold;", "Provided with Risk reduction Counselling"),
                           tags$td(style="text-align:center",rrc_july),
                           # tags$td(style="text-align:center",),
                           tags$td(style="text-align:center",rrc_august),
                           tags$td(style="text-align:center",rrc_sep),
                           tags$td(style="text-align:center",rrc_oct),
                           tags$td(style="text-align:center",rrc_nov),
                           tags$td(style="text-align:center",rrc_dec)
                           
                       ),  
                       tags$tr(
                           tags$td(style="font-weight:bold;", "Tested for HIV"),
                           tags$td(style="text-align:center",hiv_july),
                           # tags$td(style="text-align:center",),
                           tags$td(style="text-align:center",hiv_august),
                           tags$td(style="text-align:center",hiv_sep),
                           tags$td(style="text-align:center",hiv_oct),
                           tags$td(style="text-align:center",hiv_nov),
                           tags$td(style="text-align:center",hiv_dec)
                           
                           
                       ), 
                       tags$tr(
                           tags$td(style="font-weight:bold;","Screened for TB"),
                           tags$td(style="text-align:center",stb_july),
                           # tags$td(style="text-align:center",),
                           tags$td(style="text-align:center",stb_august),
                           tags$td(style="text-align:center",stb_sep),
                           tags$td(style="text-align:center",stb_oct),
                           tags$td(style="text-align:center",stb_nov),
                           tags$td(style="text-align:center",stb_dec)
                           
                       ), 
                       tags$tr(
                           tags$td(style="font-weight:bold;","Received Condoms"),
                           tags$td(style="text-align:center",cnd_july),
                           # tags$td(style="text-align:center",),
                           tags$td(style="text-align:center",cnd_august),
                           tags$td(style="text-align:center",cnd_sep),
                           tags$td(style="text-align:center",cnd_oct),
                           tags$td(style="text-align:center",cnd_nov),
                           tags$td(style="text-align:center",cnd_dec)
                           
                       ),
                       tags$tr(
                           tags$td(style="font-weight:bold;","Screened for STI"),
                           tags$td(style="text-align:center",sti_july),
                           # tags$td(style="text-align:center",),
                           tags$td(style="text-align:center",sti_august),
                           tags$td(style="text-align:center",sti_sep),
                           tags$td(style="text-align:center",sti_oct),
                           tags$td(style="text-align:center",sti_nov),
                           tags$td(style="text-align:center",sti_dec)
                       )
                       
                   )
        )
    })
    output$msm_defined<-renderUI({
        filtered_data<-if(input$defined_msm_RegionInput=="All"){
            msm_consolidated
        }
        else{
        filtered_data<-subset(msm_consolidated,region %in% input$defined_msm_RegionInput)
        }
            q9_test_hiv_msm <- filtered_data %>%
                summarise(across(starts_with("tested_for_hiv")[1:3], ~ sum(. == "Yes", na.rm = TRUE))) %>%
                summarise(max_value = max(c_across(everything()))) %>%
                pull()
            #q10 hiv
            q10_test_hiv_msm <- filtered_data %>%
                summarise(across(starts_with("tested_for_hiv")[3:6], ~ sum(. == "Yes", na.rm = TRUE))) %>%
                summarise(max_value = max(c_across(everything()))) %>%
                pull()
            #q9 initiated on prep
            q9_prep_initiated_msm <- filtered_data %>%
                summarise(across(starts_with("initiated_on_prep")[1:3], ~ sum(. == "Yes", na.rm = TRUE))) %>%
                summarise(max_value = max(c_across(everything()))) %>%
                pull()
            #q9 initiated on prep
            q10_prep_initiated_msm <- filtered_data %>%
                summarise(across(starts_with("initiated_on_prep")[3:6], ~ sum(. == "Yes", na.rm = TRUE))) %>%
                summarise(max_value = max(c_across(everything()))) %>%
                pull()
            sem_hiv_test_msm<-sum(
                nrow(filter(
                    filtered_data,(tested_for_hiv24=="Yes"|tested_for_hiv77=="Yes"|tested_for_hiv130=="Yes") & (tested_for_hiv183=="Yes"|tested_for_hiv236=="Yes"|
                                                                                                                    tested_for_hiv289=="Yes")
                ))
            )
            
            #sem intiated on prep
            sem_prep_msm<-sum(
                nrow(filter(
                    filtered_data,initiated_on_prep65=="Yes"|initiated_on_prep118=="Yes"|initiated_on_prep171=="Yes" & initiated_on_prep224=="Yes"|initiated_on_prep277=="Yes"|
                        initiated_on_prep330=="Yes"
                ))
            )
            msm_consolidated <- filtered_data %>%
                mutate(
                    combo_condition = (screened_for_stis61 == "Yes" & 
                                           (provided_with_risk_reduction_couselling68 == "Yes" | received_peer_education22 == "Yes") &
                                           condoms_distributed_nmbr46 > 0) |
                        (screened_for_stis114 == "Yes" & 
                             (provided_with_risk_reduction_couselling121 == "Yes" | received_peer_education75 == "Yes") &
                             condoms_distributed_nmbr99 > 0) |
                        (screened_for_stis167 == "Yes" & 
                             (provided_with_risk_reduction_couselling174 == "Yes" | received_peer_education128 == "Yes") &
                             condoms_distributed_nmbr152 > 0)
                )
            
            # Count the number of rows meeting the combined condition
            combo_count_msm <- msm_consolidated %>%
                summarise(combo_july_to_sep = sum(combo_condition, na.rm = TRUE))
            #msm july to dece
            msm_consolidated <- filtered_data %>%
                mutate(
                    combo_condition_oct_to_dec = (screened_for_stis220 == "Yes" & 
                                                      (provided_with_risk_reduction_couselling227 == "Yes" | received_peer_education181 == "Yes") &
                                                      condoms_distributed_nmbr205 > 0) |
                        (screened_for_stis273 == "Yes" & 
                             (provided_with_risk_reduction_couselling280 == "Yes" | received_peer_education234 == "Yes") &
                             condoms_distributed_nmbr258 > 0) |
                        (screened_for_stis326 == "Yes" & 
                             (provided_with_risk_reduction_couselling333 == "Yes" | received_peer_education287 == "Yes") &
                             condoms_distributed_nmbr311 > 0)
                )
            
            # Count the number of rows meeting the combined conditions for October to December
            combo_count_oct_to_dec_msm <- msm_consolidated %>%
                summarise(combo_oct_to_dec = sum(combo_condition_oct_to_dec, na.rm = TRUE))
            
            #comb  juy to dec
            combo_july_to_dec_msm <- sum(
                nrow(filter(filtered_data, screened_for_stis61 == "Yes" & (provided_with_risk_reduction_couselling68 == "Yes" | received_peer_education22 == "Yes") & condoms_distributed_nmbr46 > 0|
                                screened_for_stis114 == "Yes" & (provided_with_risk_reduction_couselling121 == "Yes" | received_peer_education75 == "Yes") & condoms_distributed_nmbr99 > 0|
                                screened_for_stis167 == "Yes"& (provided_with_risk_reduction_couselling174 == "Yes" | received_peer_education128 == "Yes") & condoms_distributed_nmbr152 > 0|
                                screened_for_stis220 == "Yes"& (provided_with_risk_reduction_couselling227 == "Yes" | received_peer_education181 == "Yes") & condoms_distributed_nmbr205 > 0|
                                screened_for_stis273 == "Yes" &(provided_with_risk_reduction_couselling280 == "Yes" | received_peer_education234 == "Yes") & condoms_distributed_nmbr258 > 0|
                                screened_for_stis326 == "Yes"& (provided_with_risk_reduction_couselling333 == "Yes" | received_peer_education287 == "Yes") & condoms_distributed_nmbr311 > 0))
                
            )
        
        
        tags$table(class = "table table-striped table-bordered table-hover",
                   tags$thead(h4(strong("Quaterly progress of Key Indicators-defined packages"),style="color:red"),
                              tags$tr(
                                  tags$th("Type"),
                                  tags$td(style="font-weight:bold;","Quater 9" ),
                                  # tags$td(style="font-weight:bold;","Data as at today(",Sys.Date(),")"),
                                  tags$th(style="text-align:center","Quater 10"),
                                  tags$th(style="text-align:center","Semester"),
                                  
                              )),
                   tags$tbody(
                       tags$tr(
                           tags$td(style="font-weight:bold;", "#of sex workers that have received an HIV test during the reporting period and know their results"),
                           tags$td(style="text-align:center",q9_test_hiv_msm),
                           # tags$td(style="text-align:center",),
                           tags$td(style="text-align:center",q10_test_hiv_msm),
                           tags$td(style="text-align:center",sem_hiv_test_msm),
                           
                       ),   
                       tags$tr(
                           tags$td(style="font-weight:bold;", "#of sex workers reached with HIV prevention programs - defined package"),
                           tags$td(style="text-align:center",combo_count_msm$combo_july_to_sep),
                           # tags$td(style="text-align:center",),
                           tags$td(style="text-align:center",combo_count_oct_to_dec_msm$combo_oct_to_dec),
                           tags$td(style="text-align:center",combo_july_to_dec_msm),
                       ),  
                       tags$tr(
                           tags$td(style="font-weight:bold;", "#of eligible sex workers who initiated oral antiretroviral PrEP during the reporting period"),
                           tags$td(style="text-align:center",q9_prep_initiated_msm),
                           # tags$td(style="text-align:center",),
                           tags$td(style="text-align:center",q10_prep_initiated_msm),
                           tags$td(style="text-align:center",sem_prep_msm),
                       ),
                       
                   )
        )
    })
    #PWID
    output$enrol_status_pwid<-renderDataTable({
        filtered_data <- if (input$PWIDRegionInput == "All") {
            pwid_consolidated
        } else {
            subset(pwid_consolidated, region %in% input$PWIDRegionInput)
        }
        
        table_outreach <- filtered_data %>%
            group_by(hiv_status_at_enrollment) %>%
            summarize(Count = n())
        table_outreach$hiv_status_at_enrollment[is.na(table_outreach$hiv_status_at_enrollment) | table_outreach$hiv_status_at_enrollment == ""] <- "Missing"
        colnames(table_outreach) <- c("HIV Status at enrolment","COUNT")
        datatable(table_outreach, options = list(config = list(searching = FALSE)))
        table_outreach
    })
    #PWID HIV ENROLLMENT STATUS
    output$enrol_status_chart_pwid<-renderPlotly({
        filtered_data <- if (input$PWIDRegionInput == "All") {
            pwid_consolidated
        } else {
            subset(pwid_consolidated, region %in% input$PWIDRegionInput)
        }
        
        table_outreach <- filtered_data %>%
            group_by(hiv_status_at_enrollment) %>%
            summarize(Count = n())
        
        table_outreach$hiv_status_at_enrollment[is.na(table_outreach$hiv_status_at_enrollment) | table_outreach$hiv_status_at_enrollment == ""] <- "Missing"
        colnames(table_outreach) <- c("HIV Status at enrolment","COUNT")
        
        pie_chart <- plot_ly(table_outreach, labels = ~`HIV Status at enrolment`, values = ~COUNT, type = 'pie')
        
        pie_chart %>%
            layout(title = "HIV Status Distribution",
                   xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                   yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
    })
    #PWID indicators
    output$pwid_indicators<-renderUI({
        filtered_data <- if (input$PWIDIndicatorRegionInput == "All") {
            pwid_consolidated
        } else {
            filtered_data<-subset(pwid_consolidated,region %in% input$PWIDIndicatorRegionInput)
        }
            peer_education_counts_pwid <- filtered_data %>%
                summarise(across(starts_with("received_peer_education"), ~ sum(. == "Yes", na.rm = TRUE)))
            peer_education_counts_pwid <- peer_education_counts_pwid %>%
                select(where(~ . > 0))
            
            # Count risk education services
            risk_services_counts_pwid <- filtered_data %>%
                summarise(across(starts_with("provided_with_risk_reduction_couselling"), ~ sum(. == "Yes", na.rm = TRUE)))
            risk_services_counts_pwid <- risk_services_counts_pwid %>%
                select(where(~ . > 0))
            
            #tested for hiv
            hiv_test_counts_pwid <- filtered_data %>%
                summarise(across(starts_with("tested_for_hiv"), ~ sum(. == "Yes", na.rm = TRUE)))
            hiv_test_counts_pwid <- hiv_test_counts_pwid %>%
                select(where(~ . > 0))
            #screened for TB
            Screened_tb_counts_pwid <- filtered_data %>%
                summarise(across(starts_with("screened_for_tb"), ~ sum(. == "Yes", na.rm = TRUE)))
            Screened_tb_counts_pwid <- Screened_tb_counts_pwid %>%
                select(where(~ . > 0))
            # count all those who received condoms
            received_condom_counts_pwid <- filtered_data %>%
                summarise(across(starts_with("condoms_distributed_nmbr"), ~ sum(. > 0, na.rm = TRUE)))
            received_condom_counts_pwid <- received_condom_counts_pwid %>%
                select(where(~ . > 0))
            #count those who were screened for STI
            sti_counts_pwid<- filtered_data %>%
                summarise(across(starts_with("screened_for_stis"), ~ sum(. == "Yes", na.rm = TRUE)))
            sti_counts_pwid <- sti_counts_pwid %>%
                select(where(~ . > 0))
            # count all those who received condoms
            received_niddle_syring_pwid <- filtered_data %>%
                summarise(across(starts_with("needles_and_syringes_distributed_nmbr"), ~ sum(. > 0, na.rm = TRUE)))
            received_niddle_syring_pwid <- received_niddle_syring_pwid %>%
                select(where(~ . > 0))
            
            # Combine the counts into one table
            combined_counts_pwid <- bind_rows(
                peer_education = peer_education_counts_pwid,
                risk_services = risk_services_counts_pwid,
                hiv_tests=hiv_test_counts_pwid,
                Screened_tb=Screened_tb_counts_pwid,
                received_condom=received_condom_counts_pwid,
                sti_counts_r=sti_counts_pwid,
                syringe_counts=received_niddle_syring_pwid,
                .id = "Service_Type"
            )
        
        
        pe_july<- combined_counts_pwid$received_peer_education22[!is.na(combined_counts_pwid$received_peer_education22)][1]
        pe_august<- combined_counts_pwid$received_peer_education75[!is.na(combined_counts_pwid$received_peer_education75)][1]
        pe_sep<- combined_counts_pwid$received_peer_education128[!is.na(combined_counts_pwid$received_peer_education128)][1]
        pe_oct<- combined_counts_pwid$received_peer_education181[!is.na(combined_counts_pwid$received_peer_education181)][1]
        pe_nov<- combined_counts_pwid$received_peer_education234[!is.na(combined_counts_pwid$received_peer_education234)][1]
        pe_dec<- combined_counts_pwid$received_peer_education287[!is.na(combined_counts_pwid$received_peer_education287)][1]
        #risk reduction 
        rrc_july<- combined_counts_pwid$provided_with_risk_reduction_couselling68[!is.na(combined_counts_pwid$provided_with_risk_reduction_couselling68)][1]
        rrc_august<- combined_counts_pwid$provided_with_risk_reduction_couselling121[!is.na(combined_counts_pwid$provided_with_risk_reduction_couselling121)][1]
        rrc_sep<- combined_counts_pwid$provided_with_risk_reduction_couselling174[!is.na(combined_counts_pwid$provided_with_risk_reduction_couselling174)][1]
        rrc_oct<- combined_counts_pwid$provided_with_risk_reduction_couselling227[!is.na(combined_counts_pwid$provided_with_risk_reduction_couselling227)][1]
        rrc_nov<- combined_counts_pwid$provided_with_risk_reduction_couselling280[!is.na(combined_counts_pwid$provided_with_risk_reduction_couselling280)][1]
        rrc_dec<- combined_counts_pwid$provided_with_risk_reduction_couselling333[!is.na(combined_counts_pwid$provided_with_risk_reduction_couselling333)][1]
        #tested for hiv
        hiv_july<- combined_counts_pwid$tested_for_hiv24[!is.na(combined_counts_pwid$tested_for_hiv24)][1]
        hiv_august<- combined_counts_pwid$tested_for_hiv77[!is.na(combined_counts_pwid$tested_for_hiv77)][1]
        hiv_sep<- combined_counts_pwid$tested_for_hiv130[!is.na(combined_counts_pwid$tested_for_hiv130)][1]
        hiv_oct<- combined_counts_pwid$tested_for_hiv183[!is.na(combined_counts_pwid$tested_for_hiv183)][1]
        hiv_nov<- combined_counts_pwid$tested_for_hiv236[!is.na(combined_counts_pwid$tested_for_hiv236)][1]
        hiv_dec<- combined_counts_pwid$tested_for_hiv289[!is.na(combined_counts_pwid$tested_for_hiv289)][1]
        #Screened for TB
        stb_july<- combined_counts_pwid$screened_for_tb39[!is.na(combined_counts_pwid$screened_for_tb39)][1]
        stb_august<- combined_counts_pwid$screened_for_tb92[!is.na(combined_counts_pwid$screened_for_tb92)][1]
        stb_sep<- combined_counts_pwid$screened_for_tb145[!is.na(combined_counts_pwid$screened_for_tb145)][1]
        stb_oct<- combined_counts_pwid$screened_for_tb198[!is.na(combined_counts_pwid$screened_for_tb198)][1]
        stb_nov<- combined_counts_pwid$screened_for_tb251[!is.na(combined_counts_pwid$screened_for_tb251)][1]
        stb_dec<- combined_counts_pwid$screened_for_tb304[!is.na(combined_counts_pwid$screened_for_tb304)][1]
        #Received Condoms
        cnd_july<- combined_counts_pwid$condoms_distributed_nmbr46[!is.na(combined_counts_pwid$condoms_distributed_nmbr46)][1]
        cnd_august<- combined_counts_pwid$condoms_distributed_nmbr99[!is.na(combined_counts_pwid$condoms_distributed_nmbr99)][1]
        cnd_sep<- combined_counts_pwid$condoms_distributed_nmbr152[!is.na(combined_counts_pwid$condoms_distributed_nmbr152)][1]
        cnd_oct<- combined_counts_pwid$condoms_distributed_nmbr205[!is.na(combined_counts_pwid$condoms_distributed_nmbr205)][1]
        cnd_nov<- combined_counts_pwid$condoms_distributed_nmbr258[!is.na(combined_counts_pwid$condoms_distributed_nmbr258)][1]
        cnd_dec<- combined_counts_pwid$condoms_distributed_nmbr311[!is.na(combined_counts_pwid$condoms_distributed_nmbr311)][1]
        #screened for STIS
        sti_july<- combined_counts_pwid$screened_for_stis61[!is.na(combined_counts_pwid$screened_for_stis61)][1]
        sti_august<- combined_counts_pwid$screened_for_stis114[!is.na(combined_counts_pwid$screened_for_stis114)][1]
        sti_sep<- combined_counts_pwid$screened_for_stis167[!is.na(combined_counts_pwid$screened_for_stis167)][1]
        sti_oct<- combined_counts_pwid$screened_for_stis220[!is.na(combined_counts_pwid$screened_for_stis220)][1]
        sti_nov<- combined_counts_pwid$screened_for_stis273[!is.na(combined_counts_pwid$screened_for_stis273)][1]
        sti_dec<- combined_counts_pwid$screened_for_stis326[!is.na(combined_counts_pwid$screened_for_stis326)][1]
        #Received niddles and syringes
        niddle_july<- combined_counts_pwid$needles_and_syringes_distributed_nmbr52[!is.na(combined_counts_pwid$needles_and_syringes_distributed_nmbr52)][1]
        niddle_august<- combined_counts_pwid$needles_and_syringes_distributed_nmbr105[!is.na(combined_counts_pwid$needles_and_syringes_distributed_nmbr105)][1]
        niddle_sep<- combined_counts_pwid$needles_and_syringes_distributed_nmbr158[!is.na(combined_counts_pwid$needles_and_syringes_distributed_nmbr158)][1]
        niddle_oct<- combined_counts_pwid$needles_and_syringes_distributed_nmbr211[!is.na(combined_counts_pwid$needles_and_syringes_distributed_nmbr211)][1]
        niddle_nov<- combined_counts_pwid$needles_and_syringes_distributed_nmbr264[!is.na(combined_counts_pwid$needles_and_syringes_distributed_nmbr264)][1]
        niddle_dec<- combined_counts_pwid$needles_and_syringes_distributed_nmbr317[!is.na(combined_counts_pwid$needles_and_syringes_distributed_nmbr317)][1]
        
        tags$table(class = "table table-striped table-bordered table-hover",
                   tags$thead(h4(strong("Monthly progress of Key Indicators-Individual packages"),style="color:red"),
                              tags$tr(
                                  tags$th("Type"),
                                  tags$td(style="font-weight:bold;","July" ),
                                  # tags$td(style="font-weight:bold;","Data as at today(",Sys.Date(),")"),
                                  tags$th(style="text-align:center","August"),
                                  tags$th(style="text-align:center","September"),
                                  tags$th(style="text-align:center","October"),
                                  tags$th(style="text-align:center","November"),
                                  tags$th(style="text-align:center","December"),
                                  
                              )),
                   tags$tbody(
                       tags$tr(
                           tags$td(style="font-weight:bold;", "Received Peer Education"),
                           tags$td(style="text-align:center",pe_july),
                           # tags$td(style="text-align:center",),
                           tags$td(style="text-align:center",pe_august),
                           tags$td(style="text-align:center",pe_sep),
                           tags$td(style="text-align:center",pe_oct),
                           tags$td(style="text-align:center",pe_nov),
                           tags$td(style="text-align:center",pe_dec),
                           
                       ),   
                       tags$tr(
                           tags$td(style="font-weight:bold;", "Provided with Risk reduction Counselling"),
                           tags$td(style="text-align:center",rrc_july),
                           # tags$td(style="text-align:center",),
                           tags$td(style="text-align:center",rrc_august),
                           tags$td(style="text-align:center",rrc_sep),
                           tags$td(style="text-align:center",rrc_oct),
                           tags$td(style="text-align:center",rrc_nov),
                           tags$td(style="text-align:center",rrc_dec),
                           
                       ),  
                       tags$tr(
                           tags$td(style="font-weight:bold;", "Tested for HIV"),
                           tags$td(style="text-align:center",hiv_july),
                           # tags$td(style="text-align:center",),
                           tags$td(style="text-align:center",hiv_august),
                           tags$td(style="text-align:center",hiv_sep),
                           tags$td(style="text-align:center",hiv_oct),
                           tags$td(style="text-align:center",hiv_nov),
                           tags$td(style="text-align:center",hiv_dec),
                           
                           
                       ), 
                       tags$tr(
                           tags$td(style="font-weight:bold;","Screened for TB"),
                           tags$td(style="text-align:center",stb_july),
                           # tags$td(style="text-align:center",),
                           tags$td(style="text-align:center",stb_august),
                           tags$td(style="text-align:center",stb_sep),
                           tags$td(style="text-align:center",stb_oct),
                           tags$td(style="text-align:center",stb_nov),
                           tags$td(style="text-align:center",stb_dec),
                           
                       ), 
                       tags$tr(
                           tags$td(style="font-weight:bold;","Received Condoms"),
                           tags$td(style="text-align:center",cnd_july),
                           # tags$td(style="text-align:center",),
                           tags$td(style="text-align:center",cnd_august),
                           tags$td(style="text-align:center",cnd_sep),
                           tags$td(style="text-align:center",cnd_oct),
                           tags$td(style="text-align:center",cnd_nov),
                           tags$td(style="text-align:center",cnd_dec),
                           
                       ),
                       tags$tr(
                           tags$td(style="font-weight:bold;","Screened for STI"),
                           tags$td(style="text-align:center",sti_july),
                           # tags$td(style="text-align:center",),
                           tags$td(style="text-align:center",sti_august),
                           tags$td(style="text-align:center",sti_sep),
                           tags$td(style="text-align:center",sti_oct),
                           tags$td(style="text-align:center",sti_nov),
                           tags$td(style="text-align:center",sti_dec),
                       ),
                       tags$tr(
                           tags$td(style="font-weight:bold;","Needles & Syringes Distributed"),
                           tags$td(style="text-align:center",niddle_july),
                           # tags$td(style="text-align:center",),
                           tags$td(style="text-align:center",niddle_august),
                           tags$td(style="text-align:center",niddle_sep),
                           tags$td(style="text-align:center",niddle_oct),
                           tags$td(style="text-align:center",niddle_nov),
                           tags$td(style="text-align:center",niddle_dec),
                       ),
                       
                   )
        )
    })
    #PWID key indicator summaary
    output$pwid_defined<-renderUI({
        filtered_data <- if (input$pwiddefinedRegionInput == "All") {
            pwid_consolidated
        } else {
            filtered_data<-subset(pwid_consolidated,region %in% input$pwiddefinedRegionInput)
        }
            q9_niddle_pwid <- filtered_data %>%
                summarise(across(starts_with("needles_and_syringes_distributed_nmbr")[1:3], ~ sum(. > 0, na.rm = TRUE))) %>%
                summarise(max_value = max(c_across(everything()))) %>%
                pull()
            #q10 hiv
            q10_niddle_pwid <- filtered_data %>%
                summarise(across(starts_with("needles_and_syringes_distributed_nmbr")[3:6], ~ sum(. > 0, na.rm = TRUE))) %>%
                summarise(max_value = max(c_across(everything()))) %>%
                pull()
            sem_niddle_pwid<-sum(
                nrow(filter(
                    filtered_data,(needles_and_syringes_distributed_nmbr52 > 0|needles_and_syringes_distributed_nmbr105 > 0|needles_and_syringes_distributed_nmbr158 > 0)
                    & (needles_and_syringes_distributed_nmbr211 > 0|needles_and_syringes_distributed_nmbr264 > 0|needles_and_syringes_distributed_nmbr317 > 0)
                ))
            )
            pwid_consolidated <- filtered_data %>%
                mutate(
                    combo_condition = (screened_for_stis61 == "Yes" & 
                                           (provided_with_risk_reduction_couselling68 == "Yes" | received_peer_education22 == "Yes") &
                                           condoms_distributed_nmbr46 > 0) |
                        (screened_for_stis114 == "Yes" & 
                             (provided_with_risk_reduction_couselling121 == "Yes" | received_peer_education75 == "Yes") &
                             condoms_distributed_nmbr99 > 0) |
                        (screened_for_stis167 == "Yes" & 
                             (provided_with_risk_reduction_couselling174 == "Yes" | received_peer_education128 == "Yes") &
                             condoms_distributed_nmbr152 > 0)
                )
            
            # Count the number of rows meeting the combined condition
            combo_count_pwid <- pwid_consolidated %>%
                summarise(combo_july_to_sep = sum(combo_condition, na.rm = TRUE))
            #msm july to dece
            pwid_consolidated <- filtered_data %>%
                mutate(
                    combo_condition_oct_to_dec = (screened_for_stis220 == "Yes" & 
                                                      (provided_with_risk_reduction_couselling227 == "Yes" | received_peer_education181 == "Yes") &
                                                      condoms_distributed_nmbr205 > 0) |
                        (screened_for_stis273 == "Yes" & 
                             (provided_with_risk_reduction_couselling280 == "Yes" | received_peer_education234 == "Yes") &
                             condoms_distributed_nmbr258 > 0) |
                        (screened_for_stis326 == "Yes" & 
                             (provided_with_risk_reduction_couselling333 == "Yes" | received_peer_education287 == "Yes") &
                             condoms_distributed_nmbr311 > 0)
                )
            
            # Count the number of rows meeting the combined conditions for October to December
            combo_count_oct_to_dec_pwid <- pwid_consolidated %>%
                summarise(combo_oct_to_dec = sum(combo_condition_oct_to_dec, na.rm = TRUE))
            
            #comb  juy to dec
            combo_july_to_dec_pwid <- sum(
                nrow(filter(filtered_data, screened_for_stis61 == "Yes" & (provided_with_risk_reduction_couselling68 == "Yes" | received_peer_education22 == "Yes") & condoms_distributed_nmbr46 > 0|
                                screened_for_stis114 == "Yes" & (provided_with_risk_reduction_couselling121 == "Yes" | received_peer_education75 == "Yes") & condoms_distributed_nmbr99 > 0|
                                screened_for_stis167 == "Yes"& (provided_with_risk_reduction_couselling174 == "Yes" | received_peer_education128 == "Yes") & condoms_distributed_nmbr152 > 0|
                                screened_for_stis220 == "Yes"& (provided_with_risk_reduction_couselling227 == "Yes" | received_peer_education181 == "Yes") & condoms_distributed_nmbr205 > 0|
                                screened_for_stis273 == "Yes" &(provided_with_risk_reduction_couselling280 == "Yes" | received_peer_education234 == "Yes") & condoms_distributed_nmbr258 > 0|
                                screened_for_stis326 == "Yes"& (provided_with_risk_reduction_couselling333 == "Yes" | received_peer_education287 == "Yes") & condoms_distributed_nmbr311 > 0))
                
            )
        
        
        tags$table(class = "table table-striped table-bordered table-hover",
                   tags$thead(h4(strong("Quaterly progress of Key Indicators-defined packages"),style="color:red"),
                              tags$tr(
                                  tags$th("Type"),
                                  tags$td(style="font-weight:bold;","Quater 9" ),
                                  # tags$td(style="font-weight:bold;","Data as at today(",Sys.Date(),")"),
                                  tags$th(style="text-align:center","Quater 10"),
                                  tags$th(style="text-align:center","Semester"),
                                  
                              )),
                   tags$tbody(
                       tags$tr(
                           tags$td(style="font-weight:bold;", "#of needles and syringes distributed per person who injects drugs per year by needle and syringe programs"),
                           tags$td(style="text-align:center",q9_niddle_pwid),
                           # tags$td(style="text-align:center",),
                           tags$td(style="text-align:center",q10_niddle_pwid),
                           tags$td(style="text-align:center",sem_niddle_pwid),
                           
                       ),   
                       tags$tr(
                           tags$td(style="font-weight:bold;", "#of people who inject drugs reached with HIV prevention programs - defined package"),
                           tags$td(style="text-align:center",combo_count_pwid$combo_july_to_sep),
                           # tags$td(style="text-align:center",),
                           tags$td(style="text-align:center",combo_count_oct_to_dec_pwid$combo_oct_to_dec),
                           tags$td(style="text-align:center",combo_july_to_dec_pwid),
                       ),  
                       tags$tr(
                           tags$td(style="font-weight:bold;", "#of individuals receiving Opioid Substitution Therapy who received treatment for at least 6 months"),
                           tags$td(style="text-align:center",0),
                           # tags$td(style="text-align:center",),
                           tags$td(style="text-align:center",0),
                           tags$td(style="text-align:center",0),
                       ), 
                       
                   )
        )
    })
    #tg data
    output$enrol_status_tg<-renderDT({
        filtered_data <- if (input$TGRegionInput == "All") {
            tg_consolidated
        } else {
            subset(tg_consolidated, region %in% input$TGRegionInput)
        }
        
        table_outreach <- filtered_data %>%
            group_by(hiv_status_at_enrollment) %>%
            summarize(Count = n())
        table_outreach$hiv_status_at_enrollment[is.na(table_outreach$hiv_status_at_enrollment) | table_outreach$hiv_status_at_enrollment == ""] <- "Missing"
        colnames(table_outreach) <- c("HIV Status at enrolment","COUNT")
        datatable(table_outreach, options = list(config = list(searching = FALSE)))
        table_outreach
    })
    #TG HIV ENROLLMENT STATUS
    output$enrol_status_chart_tg<-renderPlotly({
        filtered_data <- if (input$TGRegionInput == "All") {
            tg_consolidated
        } else {
            subset(tg_consolidated, region %in% input$TGRegionInput)
        }
        
        table_outreach <- filtered_data %>%
            group_by(hiv_status_at_enrollment) %>%
            summarize(Count = n())
        
        table_outreach$hiv_status_at_enrollment[is.na(table_outreach$hiv_status_at_enrollment) | table_outreach$hiv_status_at_enrollment == ""] <- "Missing"
        colnames(table_outreach) <- c("HIV Status at enrolment","COUNT")
        
        pie_chart <- plot_ly(table_outreach, labels = ~`HIV Status at enrolment`, values = ~COUNT, type = 'pie')
        
        pie_chart %>%
            layout(title = "HIV Status Distribution",
                   xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                   yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
    })
    #TG indicators
    output$tg_indicators<-renderUI({
        filtered_data <- if (input$TGIndicatorRegionInput == "All") {
            tg_consolidated
        } else {
            filtered_data<-subset(tg_consolidated,region %in% input$TGIndicatorRegionInput)
        }
            peer_education_counts_msm <- filtered_data %>%
                summarise(across(starts_with("received_peer_education"), ~ sum(. == "Yes", na.rm = TRUE)))
            peer_education_counts_msm <- peer_education_counts_msm %>%
                select(where(~ . > 0))
            
            # Count risk education services
            risk_services_counts_msm <- filtered_data %>%
                summarise(across(starts_with("provided_with_risk_reduction_couselling"), ~ sum(. == "Yes", na.rm = TRUE)))
            risk_services_counts_msm <- risk_services_counts_msm %>%
                select(where(~ . > 0))
            
            #tested for hiv
            hiv_test_counts_msm <- filtered_data %>%
                summarise(across(starts_with("tested_for_hiv"), ~ sum(. == "Yes", na.rm = TRUE)))
            hiv_test_counts_msm <- hiv_test_counts_msm %>%
                select(where(~ . > 0))
            #screened for TB
            Screened_tb_counts_msm <- filtered_data %>%
                summarise(across(starts_with("screened_for_tb"), ~ sum(. == "Yes", na.rm = TRUE)))
            Screened_tb_counts_msm <- Screened_tb_counts_msm %>%
                select(where(~ . > 0))
            # count all those who received condoms
            received_condom_counts_msm <- filtered_data %>%
                summarise(across(starts_with("condoms_distributed_nmbr"), ~ sum(. > 0, na.rm = TRUE)))
            received_condom_counts_msm <- received_condom_counts_msm %>%
                select(where(~ . > 0))
            #count those who were screened for STI
            sti_counts_msm <- filtered_data %>%
                summarise(across(starts_with("screened_for_stis"), ~ sum(. == "Yes", na.rm = TRUE)))
            sti_counts_msm <- sti_counts_msm %>%
                select(where(~ . > 0))
            
            # Combine the counts into one table
            combined_counts_tg <- bind_rows(
                peer_education = peer_education_counts_msm,
                risk_services = risk_services_counts_msm,
                hiv_tests=hiv_test_counts_msm,
                Screened_tb=Screened_tb_counts_msm,
                received_condom=received_condom_counts_msm,
                sti_counts_r=sti_counts_msm,
                .id = "Service_Type"
            )
        
        pe_july<- combined_counts_tg$received_peer_education22[!is.na(combined_counts_tg$received_peer_education22)][1]
        pe_august<- combined_counts_tg$received_peer_education75[!is.na(combined_counts_tg$received_peer_education75)][1]
        pe_sep<- combined_counts_tg$received_peer_education128[!is.na(combined_counts_tg$received_peer_education128)][1]
        pe_oct<- combined_counts_tg$received_peer_education181[!is.na(combined_counts_tg$received_peer_education181)][1]
        pe_nov<- combined_counts_tg$received_peer_education234[!is.na(combined_counts_tg$received_peer_education234)][1]
        pe_dec<- combined_counts_tg$received_peer_education287[!is.na(combined_counts_tg$received_peer_education287)][1]
        #risk reduction 
        rrc_july<- combined_counts_tg$provided_with_risk_reduction_couselling68[!is.na(combined_counts_tg$provided_with_risk_reduction_couselling68)][1]
        rrc_august<- combined_counts_tg$provided_with_risk_reduction_couselling121[!is.na(combined_counts_tg$provided_with_risk_reduction_couselling121)][1]
        rrc_sep<- combined_counts_tg$provided_with_risk_reduction_couselling174[!is.na(combined_counts_tg$provided_with_risk_reduction_couselling174)][1]
        rrc_oct<- combined_counts_tg$provided_with_risk_reduction_couselling227[!is.na(combined_counts_tg$provided_with_risk_reduction_couselling227)][1]
        rrc_nov<- combined_counts_tg$provided_with_risk_reduction_couselling280[!is.na(combined_counts_tg$provided_with_risk_reduction_couselling280)][1]
        rrc_dec<- combined_counts_tg$provided_with_risk_reduction_couselling333[!is.na(combined_counts_tg$provided_with_risk_reduction_couselling333)][1]
        #tested for hiv
        hiv_july<- combined_counts_tg$tested_for_hiv24[!is.na(combined_counts_tg$tested_for_hiv24)][1]
        hiv_august<- combined_counts_tg$tested_for_hiv77[!is.na(combined_counts_tg$tested_for_hiv77)][1]
        hiv_sep<- combined_counts_tg$tested_for_hiv130[!is.na(combined_counts_tg$tested_for_hiv130)][1]
        hiv_oct<- combined_counts_tg$tested_for_hiv183[!is.na(combined_counts_tg$tested_for_hiv183)][1]
        hiv_nov<- combined_counts_tg$tested_for_hiv236[!is.na(combined_counts_tg$tested_for_hiv236)][1]
        hiv_dec<- combined_counts_tg$tested_for_hiv289[!is.na(combined_counts_tg$tested_for_hiv289)][1]
        #Screened for TB
        stb_july<- combined_counts_tg$screened_for_tb39[!is.na(combined_counts_tg$screened_for_tb39)][1]
        stb_august<- combined_counts_tg$screened_for_tb92[!is.na(combined_counts_tg$screened_for_tb92)][1]
        stb_sep<- combined_counts_tg$screened_for_tb145[!is.na(combined_counts_tg$screened_for_tb145)][1]
        stb_oct<- combined_counts_tg$screened_for_tb198[!is.na(combined_counts_tg$screened_for_tb198)][1]
        stb_nov<- combined_counts_tg$screened_for_tb251[!is.na(combined_counts_tg$screened_for_tb251)][1]
        stb_dec<- combined_counts_tg$screened_for_tb304[!is.na(combined_counts_tg$screened_for_tb304)][1]
        #Received Condoms
        cnd_july<- combined_counts_tg$condoms_distributed_nmbr46[!is.na(combined_counts_tg$condoms_distributed_nmbr46)][1]
        cnd_august<- combined_counts_tg$condoms_distributed_nmbr99[!is.na(combined_counts_tg$condoms_distributed_nmbr99)][1]
        cnd_sep<- combined_counts_tg$condoms_distributed_nmbr152[!is.na(combined_counts_tg$condoms_distributed_nmbr152)][1]
        cnd_oct<- combined_counts_tg$condoms_distributed_nmbr205[!is.na(combined_counts_tg$condoms_distributed_nmbr205)][1]
        cnd_nov<- combined_counts_tg$condoms_distributed_nmbr258[!is.na(combined_counts_tg$condoms_distributed_nmbr258)][1]
        cnd_dec<- combined_counts_tg$condoms_distributed_nmbr311[!is.na(combined_counts_tg$condoms_distributed_nmbr311)][1]
        #screened for STIS
        sti_july<- combined_counts_tg$screened_for_stis61[!is.na(combined_counts_tg$screened_for_stis61)][1]
        sti_august<- combined_counts_tg$screened_for_stis114[!is.na(combined_counts_tg$screened_for_stis114)][1]
        sti_sep<- combined_counts_tg$screened_for_stis167[!is.na(combined_counts_tg$screened_for_stis167)][1]
        sti_oct<- combined_counts_tg$screened_for_stis220[!is.na(combined_counts_tg$screened_for_stis220)][1]
        sti_nov<- combined_counts_tg$screened_for_stis273[!is.na(combined_counts_tg$screened_for_stis273)][1]
        sti_dec<- combined_counts_tg$screened_for_stis326[!is.na(combined_counts_tg$screened_for_stis326)][1]
        
        tags$table(class = "table table-striped table-bordered table-hover",
                   tags$thead(
                              tags$tr(
                                  tags$th("Type"),
                                  tags$td(style="font-weight:bold;","July" ),
                                  # tags$td(style="font-weight:bold;","Data as at today(",Sys.Date(),")"),
                                  tags$th(style="text-align:center","August"),
                                  tags$th(style="text-align:center","September"),
                                  tags$th(style="text-align:center","October"),
                                  tags$th(style="text-align:center","November"),
                                  tags$th(style="text-align:center","December")
                                  
                              )),
                   tags$tbody(
                       tags$tr(
                           tags$td(style="font-weight:bold;", "Received Peer Education"),
                           tags$td(style="text-align:center",pe_july),
                           # tags$td(style="text-align:center",),
                           tags$td(style="text-align:center",pe_august),
                           tags$td(style="text-align:center",pe_sep),
                           tags$td(style="text-align:center",pe_oct),
                           tags$td(style="text-align:center",pe_nov),
                           tags$td(style="text-align:center",pe_dec)
                           
                       ),   
                       tags$tr(
                           tags$td(style="font-weight:bold;", "Provided with Risk reduction Counselling"),
                           tags$td(style="text-align:center",rrc_july),
                           # tags$td(style="text-align:center",),
                           tags$td(style="text-align:center",rrc_august),
                           tags$td(style="text-align:center",rrc_sep),
                           tags$td(style="text-align:center",rrc_oct),
                           tags$td(style="text-align:center",rrc_nov),
                           tags$td(style="text-align:center",rrc_dec)
                           
                       ),  
                       tags$tr(
                           tags$td(style="font-weight:bold;", "Tested for HIV"),
                           tags$td(style="text-align:center",hiv_july),
                           # tags$td(style="text-align:center",),
                           tags$td(style="text-align:center",hiv_august),
                           tags$td(style="text-align:center",hiv_sep),
                           tags$td(style="text-align:center",hiv_oct),
                           tags$td(style="text-align:center",hiv_nov),
                           tags$td(style="text-align:center",hiv_dec)
                           
                           
                       ), 
                       tags$tr(
                           tags$td(style="font-weight:bold;","Screened for TB"),
                           tags$td(style="text-align:center",stb_july),
                           # tags$td(style="text-align:center",),
                           tags$td(style="text-align:center",stb_august),
                           tags$td(style="text-align:center",stb_sep),
                           tags$td(style="text-align:center",stb_oct),
                           tags$td(style="text-align:center",stb_nov),
                           tags$td(style="text-align:center",stb_dec)
                           
                       ), 
                       tags$tr(
                           tags$td(style="font-weight:bold;","Received Condoms"),
                           tags$td(style="text-align:center",cnd_july),
                           # tags$td(style="text-align:center",),
                           tags$td(style="text-align:center",cnd_august),
                           tags$td(style="text-align:center",cnd_sep),
                           tags$td(style="text-align:center",cnd_oct),
                           tags$td(style="text-align:center",cnd_nov),
                           tags$td(style="text-align:center",cnd_dec)
                           
                       ),
                       tags$tr(
                           tags$td(style="font-weight:bold;","Screened for STI"),
                           tags$td(style="text-align:center",sti_july),
                           # tags$td(style="text-align:center",),
                           tags$td(style="text-align:center",sti_august),
                           tags$td(style="text-align:center",sti_sep),
                           tags$td(style="text-align:center",sti_oct),
                           tags$td(style="text-align:center",sti_nov),
                           tags$td(style="text-align:center",sti_dec)
                       ),
                       
                   )
        )
    })
    #tg key indicator summaary
    output$tg_defined<-renderUI({
        filtered_data <- if (input$tgdefinedRegionInput == "All") {
            tg_consolidated
        } else {
            filtered_data<-subset(tg_consolidated,region %in% input$tgdefinedRegionInput)
        }
            q9_test_hiv_tg <- filtered_data %>%
                summarise(across(starts_with("tested_for_hiv")[1:3], ~ sum(. == "Yes", na.rm = TRUE))) %>%
                summarise(max_value = max(c_across(everything()))) %>%
                pull()
            #q10 hiv
            q10_test_hiv_tg <- filtered_data %>%
                summarise(across(starts_with("tested_for_hiv")[3:6], ~ sum(. == "Yes", na.rm = TRUE))) %>%
                summarise(max_value = max(c_across(everything()))) %>%
                pull()
            sem_hiv_test_tg<-sum(
                nrow(filter(
                    filtered_data,(tested_for_hiv24=="Yes"|tested_for_hiv77=="Yes"|tested_for_hiv130=="Yes") & (tested_for_hiv183=="Yes"|tested_for_hiv236=="Yes"|
                                                                                                                    tested_for_hiv289=="Yes")
                ))
            )
            tg_consolidated <- filtered_data %>%
                mutate(
                    combo_condition = (screened_for_stis61 == "Yes" & 
                                           (provided_with_risk_reduction_couselling68 == "Yes" | received_peer_education22 == "Yes") &
                                           condoms_distributed_nmbr46 > 0) |
                        (screened_for_stis114 == "Yes" & 
                             (provided_with_risk_reduction_couselling121 == "Yes" | received_peer_education75 == "Yes") &
                             condoms_distributed_nmbr99 > 0) |
                        (screened_for_stis167 == "Yes" & 
                             (provided_with_risk_reduction_couselling174 == "Yes" | received_peer_education128 == "Yes") &
                             condoms_distributed_nmbr152 > 0)
                )
            
            # Count the number of rows meeting the combined condition
            combo_count_tg <- tg_consolidated %>%
                summarise(combo_july_to_sep = sum(combo_condition, na.rm = TRUE))
            #msm july to dece
            tg_consolidated <- filtered_data %>%
                mutate(
                    combo_condition_oct_to_dec = (screened_for_stis220 == "Yes" & 
                                                      (provided_with_risk_reduction_couselling227 == "Yes" | received_peer_education181 == "Yes") &
                                                      condoms_distributed_nmbr205 > 0) |
                        (screened_for_stis273 == "Yes" & 
                             (provided_with_risk_reduction_couselling280 == "Yes" | received_peer_education234 == "Yes") &
                             condoms_distributed_nmbr258 > 0) |
                        (screened_for_stis326 == "Yes" & 
                             (provided_with_risk_reduction_couselling333 == "Yes" | received_peer_education287 == "Yes") &
                             condoms_distributed_nmbr311 > 0)
                )
            
            # Count the number of rows meeting the combined conditions for October to December
            combo_count_oct_to_dec_tg <- tg_consolidated %>%
                summarise(combo_oct_to_dec = sum(combo_condition_oct_to_dec, na.rm = TRUE))
            
            #comb  juy to dec
            combo_july_to_dec_tg <- sum(
                nrow(filter(filtered_data, screened_for_stis61 == "Yes" & (provided_with_risk_reduction_couselling68 == "Yes" | received_peer_education22 == "Yes") & condoms_distributed_nmbr46 > 0|
                                screened_for_stis114 == "Yes" & (provided_with_risk_reduction_couselling121 == "Yes" | received_peer_education75 == "Yes") & condoms_distributed_nmbr99 > 0|
                                screened_for_stis167 == "Yes"& (provided_with_risk_reduction_couselling174 == "Yes" | received_peer_education128 == "Yes") & condoms_distributed_nmbr152 > 0|
                                screened_for_stis220 == "Yes"& (provided_with_risk_reduction_couselling227 == "Yes" | received_peer_education181 == "Yes") & condoms_distributed_nmbr205 > 0|
                                screened_for_stis273 == "Yes" &(provided_with_risk_reduction_couselling280 == "Yes" | received_peer_education234 == "Yes") & condoms_distributed_nmbr258 > 0|
                                screened_for_stis326 == "Yes"& (provided_with_risk_reduction_couselling333 == "Yes" | received_peer_education287 == "Yes") & condoms_distributed_nmbr311 > 0))
                
            )
        
        
        tags$table(class = "table table-striped table-bordered table-hover",
                   tags$thead(
                              tags$tr(
                                  tags$th("Type"),
                                  tags$td(style="font-weight:bold;","Quater 9" ),
                                  # tags$td(style="font-weight:bold;","Data as at today(",Sys.Date(),")"),
                                  tags$th(style="text-align:center","Quater 10"),
                                  tags$th(style="text-align:center","Semester")
                                  
                              )),
                   tags$tbody(
                       tags$tr(
                           tags$td(style="font-weight:bold;", "#of transgender people that have received an HIV test during the reporting period and know their results"),
                           tags$td(style="text-align:center",q9_test_hiv_tg),
                           # tags$td(style="text-align:center",),
                           tags$td(style="text-align:center",q10_test_hiv_tg),
                           tags$td(style="text-align:center",sem_hiv_test_tg)
                           
                       ),   
                       tags$tr(
                           tags$td(style="font-weight:bold;", "#of transgender people reached with HIV prevention programs - defined package of services"),
                           tags$td(style="text-align:center",combo_count_tg$combo_july_to_sep),
                           # tags$td(style="text-align:center",),
                           tags$td(style="text-align:center",combo_count_oct_to_dec_tg$combo_oct_to_dec),
                           tags$td(style="text-align:center",combo_july_to_dec_tg)
                       ),  
                       
                   )
        )
    })
    
}