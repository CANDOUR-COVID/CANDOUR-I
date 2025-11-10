#### Function ####

wtp_figures <- function(data, country) {
  
  data <- data %>%
    mutate_at(vars(wtpVal, taxesExtra, ticketExtra),
              as.numeric)
  
  if(country == "UK") {
    
    data_UK <- data %>%
      filter(country == "UK")
    
    ggplot(drop_na(data_UK, wtp_amount_1), aes(x=wtp_amount_1)) +
      facet_wrap(~factor(wtpVal,
                         levels = c(30, 60, 115, 230, 575),
                         labels = c(30, 60, 115, 230, 575))) +
      geom_bar(aes(y = (..count..)/sum(..count..)), stat = "count") +
      scale_y_continuous(labels = scales::percent, breaks = seq(0, 1, by = 0.05)) +
      geom_text(aes(label = ..count.., y = ((..count..)/sum(..count..))), 
                stat = "count", vjust = -0.5, size = 6) +
      labs(x = NULL,
           y = "Proportion") +
      expand_limits(y=c(0, 0.25))
    
    ggsave("figures/wtp_amount_1_uk.pdf", width = 12, height = 8, dpi = 300)
    
    ggplot(drop_na(data_UK, wtp_amount_2a), aes(x=wtp_amount_2a)) +
      facet_wrap(~factor(wtpVal,
                         levels = c(30, 60, 115, 230, 575),
                         labels = c(60, 120, 230, 460, 1150))) +
      geom_bar(aes(y = (..count..)/sum(..count..)), stat = "count") +
      scale_y_continuous(labels = scales::percent, breaks = seq(0, 1, by = 0.05)) +
      geom_text(aes(label = ..count.., y = ((..count..)/sum(..count..))), 
                stat = "count", vjust = -0.5, size = 6) +
      labs(x = NULL,
           y = "Proportion") +
      expand_limits(y=c(0, 0.55))
    
    ggsave("figures/wtp_amount_2a_uk.pdf", width = 12, height = 8, dpi = 300)
    
    ggplot(drop_na(data_UK, wtp_amount_2b), aes(x=wtp_amount_2b)) +
      facet_wrap(~factor(wtpVal,
                         levels = c(30, 60, 115, 230, 575),
                         labels = c(15, 30, 57.5, 115, 287.5))) +
      geom_bar(aes(y = (..count..)/sum(..count..)), stat = "count") +
      scale_y_continuous(labels = scales::percent, breaks = seq(0, 1, by = 0.05)) +
      geom_text(aes(label = ..count.., y = ((..count..)/sum(..count..))), 
                stat = "count", vjust = -0.5, size = 6) +
      labs(x = NULL,
           y = "Proportion") +
      expand_limits(y=c(0, 0.4))
    
    ggsave("figures/wtp_amount_2b_uk.pdf", width = 12, height = 8, dpi = 300)
    
    ggplot(drop_na(data_UK, geq_taxes_1), aes(x=geq_taxes_1)) +
      facet_wrap(~factor(taxesExtra,
                         levels = c(5, 30, 60, 290, 575, 2880),
                         labels = c(5, 30, 60, 290, 575, 2880))) +
      geom_bar(aes(y = (..count..)/sum(..count..)), stat = "count") +
      scale_y_continuous(labels = scales::percent, breaks = seq(0, 1, by = 0.05)) +
      geom_text(aes(label = ..count.., y = ((..count..)/sum(..count..))), 
                stat = "count", vjust = -0.5, size = 6) +
      labs(x = NULL,
           y = "Proportion") +
      expand_limits(y=c(0, 0.25))
    
    ggsave("figures/geq_taxes_1_uk.pdf", width = 12, height = 8, dpi = 300)
    
    ggplot(drop_na(data_UK, geq_taxes_2a), aes(x=geq_taxes_2a)) +
      facet_wrap(~factor(taxesExtra,
                         levels = c(5, 30, 60, 290, 575, 2880),
                         labels = c(10, 60, 120, 580, 1150, 5760))) +
      geom_bar(aes(y = (..count..)/sum(..count..)), stat = "count") +
      scale_y_continuous(labels = scales::percent, breaks = seq(0, 1, by = 0.05)) +
      geom_text(aes(label = ..count.., y = ((..count..)/sum(..count..))), 
                stat = "count", vjust = -0.5, size = 6) +
      labs(x = NULL,
           y = "Proportion") +
      expand_limits(y=c(0, 0.55))
    
    ggsave("figures/geq_taxes_2a_uk.pdf", width = 12, height = 8, dpi = 300)
    
    ggplot(drop_na(data_UK, geq_taxes_2b), aes(x=geq_taxes_2b)) +
      facet_wrap(~factor(taxesExtra,
                         levels = c(5, 30, 60, 290, 575, 2880),
                         labels = c(2.5, 15, 30, 145, 287.5, 1440))) +
      geom_bar(aes(y = (..count..)/sum(..count..)), stat = "count") +
      scale_y_continuous(labels = scales::percent, breaks = seq(0, 1, by = 0.05)) +
      geom_text(aes(label = ..count.., y = ((..count..)/sum(..count..))), 
                stat = "count", vjust = -0.5, size = 6) +
      labs(x = NULL,
           y = "Proportion") +
      expand_limits(y=c(0, 0.4))
    
    ggsave("figures/geq_taxes_2b_uk.pdf", width = 12, height = 8, dpi = 300)
    
    ggplot(drop_na(data_UK, geq_ticket_1), aes(x=geq_ticket_1)) +
      facet_wrap(~factor(ticketExtra,
                         levels = c(5, 10, 30, 60, 115),
                         labels = c(5, 10, 30, 60, 115))) +
      geom_bar(aes(y = (..count..)/sum(..count..)), stat = "count") +
      scale_y_continuous(labels = scales::percent, breaks = seq(0, 1, by = 0.05)) +
      geom_text(aes(label = ..count.., y = ((..count..)/sum(..count..))), 
                stat = "count", vjust = -0.5, size = 6) +
      labs(x = NULL,
           y = "Proportion") +
      expand_limits(y=c(0, 0.25))
    
    ggsave("figures/geq_ticket_1_uk.pdf", width = 12, height = 8, dpi = 300)
    
    ggplot(drop_na(data_UK, geq_ticket_2a), aes(x=geq_ticket_2a)) +
      facet_wrap(~factor(ticketExtra,
                         levels = c(5, 10, 30, 60, 115),
                         labels = c(10, 20, 60, 120, 230))) +
      geom_bar(aes(y = (..count..)/sum(..count..)), stat = "count") +
      scale_y_continuous(labels = scales::percent, breaks = seq(0, 1, by = 0.05)) +
      geom_text(aes(label = ..count.., y = ((..count..)/sum(..count..))), 
                stat = "count", vjust = -0.5, size = 6) +
      labs(x = NULL,
           y = "Proportion") +
      expand_limits(y=c(0, 0.55))
    
    ggsave("figures/geq_ticket_2a_uk.pdf", width = 12, height = 8, dpi = 300)
    
    ggplot(drop_na(data_UK, geq_ticket_2b), aes(x=geq_ticket_2b)) +
      facet_wrap(~factor(ticketExtra,
                         levels = c(5, 10, 30, 60, 115),
                         labels = c(2.5, 5, 15, 30, 57.5))) +
      geom_bar(aes(y = (..count..)/sum(..count..)), stat = "count") +
      scale_y_continuous(labels = scales::percent, breaks = seq(0, 1, by = 0.05)) +
      geom_text(aes(label = ..count.., y = ((..count..)/sum(..count..))), 
                stat = "count", vjust = -0.5, size = 6) +
      labs(x = NULL,
           y = "Proportion") +
      expand_limits(y=c(0, 0.4))
    
    ggsave("figures/geq_ticket_2b_uk.pdf", width = 12, height = 8, dpi = 300)
    
    rm(data_UK)
    
  }
  
  if (country == "Australia") {
    
    data_Australia <- data %>%
      filter(country == "Australia")
    
    ggplot(drop_na(data_Australia, wtp_amount_1), aes(x=wtp_amount_1)) +
      facet_wrap(~factor(wtpVal,
                         levels = c(55, 115, 230, 460, 1145),
                         labels = c(55, 115, 230, 460, 1145))) +
      geom_bar(aes(y = (..count..)/sum(..count..)), stat = "count") +
      scale_y_continuous(labels = scales::percent, breaks = seq(0, 1, by = 0.05)) +
      geom_text(aes(label = ..count.., y = ((..count..)/sum(..count..))), 
                stat = "count", vjust = -0.5, size = 6) +
      labs(x = NULL,
           y = "Proportion") +
      expand_limits(y=c(0, 0.25))
    
    ggsave("figures/wtp_amount_1_australia.pdf", width = 12, height = 8, dpi = 300)
    
    ggplot(drop_na(data_Australia, wtp_amount_2a), aes(x=wtp_amount_2a)) +
      facet_wrap(~factor(wtpVal,
                         levels = c(55, 115, 230, 460, 1145),
                         labels = c(110, 230, 460, 920, 2290))) +
      geom_bar(aes(y = (..count..)/sum(..count..)), stat = "count") +
      scale_y_continuous(labels = scales::percent, breaks = seq(0, 1, by = 0.05)) +
      geom_text(aes(label = ..count.., y = ((..count..)/sum(..count..))), 
                stat = "count", vjust = -0.5, size = 6) +
      labs(x = NULL,
           y = "Proportion") +
      expand_limits(y=c(0, 0.55))
    
    ggsave("figures/wtp_amount_2a_australia.pdf", width = 12, height = 8, dpi = 300)
    
    ggplot(drop_na(data_Australia, wtp_amount_2b), aes(x=wtp_amount_2b)) +
      facet_wrap(~factor(wtpVal,
                         levels = c(55, 115, 230, 460, 1145),
                         labels = c(27.5, 57.5, 115, 230, 572.5))) +
      geom_bar(aes(y = (..count..)/sum(..count..)), stat = "count") +
      scale_y_continuous(labels = scales::percent, breaks = seq(0, 1, by = 0.05)) +
      geom_text(aes(label = ..count.., y = ((..count..)/sum(..count..))), 
                stat = "count", vjust = -0.5, size = 6) +
      labs(x = NULL,
           y = "Proportion") +
      expand_limits(y=c(0, 0.4))
    
    ggsave("figures/wtp_amount_2b_australia.pdf", width = 12, height = 8, dpi = 300)
    
    ggplot(drop_na(data_Australia, geq_taxes_1), aes(x=geq_taxes_1)) +
      facet_wrap(~factor(taxesExtra,
                         levels = c(10, 55, 115, 575, 1145, 5730),
                         labels = c(10, 55, 115, 575, 1145, 5730))) +
      geom_bar(aes(y = (..count..)/sum(..count..)), stat = "count") +
      scale_y_continuous(labels = scales::percent, breaks = seq(0, 1, by = 0.05)) +
      geom_text(aes(label = ..count.., y = ((..count..)/sum(..count..))), 
                stat = "count", vjust = -0.5, size = 6) +
      labs(x = NULL,
           y = "Proportion") +
      expand_limits(y=c(0, 0.25))
    
    ggsave("figures/geq_taxes_1_australia.pdf", width = 12, height = 8, dpi = 300)
    
    ggplot(drop_na(data_Australia, geq_taxes_2a), aes(x=geq_taxes_2a)) +
      facet_wrap(~factor(taxesExtra,
                         levels = c(10, 55, 115, 575, 1145, 5730),
                         labels = c(20, 110, 230, 1150, 2290, 11460))) +
      geom_bar(aes(y = (..count..)/sum(..count..)), stat = "count") +
      scale_y_continuous(labels = scales::percent, breaks = seq(0, 1, by = 0.05)) +
      geom_text(aes(label = ..count.., y = ((..count..)/sum(..count..))), 
                stat = "count", vjust = -0.5, size = 6) +
      labs(x = NULL,
           y = "Proportion") +
      expand_limits(y=c(0, 0.55))
    
    ggsave("figures/geq_taxes_2a_australia.pdf", width = 12, height = 8, dpi = 300)
    
    ggplot(drop_na(data_Australia, geq_taxes_2b), aes(x=geq_taxes_2b)) +
      facet_wrap(~factor(taxesExtra,
                         levels = c(10, 55, 115, 575, 1145, 5730),
                         labels = c(5, 27.5, 57.5, 287.5, 572.5, 2865))) +
      geom_bar(aes(y = (..count..)/sum(..count..)), stat = "count") +
      scale_y_continuous(labels = scales::percent, breaks = seq(0, 1, by = 0.05)) +
      geom_text(aes(label = ..count.., y = ((..count..)/sum(..count..))), 
                stat = "count", vjust = -0.5, size = 6) +
      labs(x = NULL,
           y = "Proportion") +
      expand_limits(y=c(0, 0.4))
    
    ggsave("figures/geq_taxes_2b_australia.pdf", width = 12, height = 8, dpi = 300)
    
    ggplot(drop_na(data_Australia, geq_ticket_1), aes(x=geq_ticket_1)) +
      facet_wrap(~factor(ticketExtra,
                         levels = c(10, 25, 55, 115, 230),
                         labels = c(10, 25, 55, 115, 230))) +
      geom_bar(aes(y = (..count..)/sum(..count..)), stat = "count") +
      scale_y_continuous(labels = scales::percent, breaks = seq(0, 1, by = 0.05)) +
      geom_text(aes(label = ..count.., y = ((..count..)/sum(..count..))), 
                stat = "count", vjust = -0.5, size = 6) +
      labs(x = NULL,
           y = "Proportion") +
      expand_limits(y=c(0, 0.25))
    
    ggsave("figures/geq_ticket_1_australia.pdf", width = 12, height = 8, dpi = 300)
    
    ggplot(drop_na(data_Australia, geq_ticket_2a), aes(x=geq_ticket_2a)) +
      facet_wrap(~factor(ticketExtra,
                         levels = c(10, 25, 55, 115, 230),
                         labels = c(20, 50, 110, 230, 460))) +
      geom_bar(aes(y = (..count..)/sum(..count..)), stat = "count") +
      scale_y_continuous(labels = scales::percent, breaks = seq(0, 1, by = 0.05)) +
      geom_text(aes(label = ..count.., y = ((..count..)/sum(..count..))), 
                stat = "count", vjust = -0.5, size = 6) +
      labs(x = NULL,
           y = "Proportion") +
      expand_limits(y=c(0, 0.55))
    
    ggsave("figures/geq_ticket_2a_australia.pdf", width = 12, height = 8, dpi = 300)
    
    ggplot(drop_na(data_Australia, geq_ticket_2b), aes(x=geq_ticket_2b)) +
      facet_wrap(~factor(ticketExtra,
                         levels = c(10, 25, 55, 115, 230),
                         labels = c(5, 12.5, 27.5, 57.5, 115))) +
      geom_bar(aes(y = (..count..)/sum(..count..)), stat = "count") +
      scale_y_continuous(labels = scales::percent, breaks = seq(0, 1, by = 0.05)) +
      geom_text(aes(label = ..count.., y = ((..count..)/sum(..count..))), 
                stat = "count", vjust = -0.5, size = 6) +
      labs(x = NULL,
           y = "Proportion") +
      expand_limits(y=c(0, 0.4))
    
    ggsave("figures/geq_ticket_2b_australia.pdf", width = 12, height = 8, dpi = 300)
    
    rm(data_Australia)
    
  }
  
  if (country == "Brazil") {
    
    data_Brazil <- data %>%
      filter(country == "Brazil") 
    
    ggplot(drop_na(data_Brazil, wtp_amount_1), aes(x=wtp_amount_1)) +
      facet_wrap(~factor(wtpVal,
                         levels = c(65, 135, 270, 535, 1340),
                         labels = c(65, 135, 270, 535, 1340))) +
      geom_bar(aes(y = (..count..)/sum(..count..)), stat = "count") +
      scale_y_continuous(labels = scales::percent, breaks = seq(0, 1, by = 0.05)) +
      geom_text(aes(label = ..count.., y = ((..count..)/sum(..count..))), 
                stat = "count", vjust = -0.5, size = 6) +
      labs(x = NULL,
           y = "Proportion") +
      expand_limits(y=c(0, 0.25))
    
    ggsave("figures/wtp_amount_1_brazil.pdf", width = 12, height = 8, dpi = 300)
    
    ggplot(drop_na(data_Brazil, wtp_amount_2a), aes(x=wtp_amount_2a)) +
      facet_wrap(~factor(wtpVal,
                         levels = c(65, 135, 270, 535, 1340),
                         labels = c(130, 270, 540, 1070, 2680))) +
      geom_bar(aes(y = (..count..)/sum(..count..)), stat = "count") +
      scale_y_continuous(labels = scales::percent, breaks = seq(0, 1, by = 0.05)) +
      geom_text(aes(label = ..count.., y = ((..count..)/sum(..count..))), 
                stat = "count", vjust = -0.5, size = 6) +
      labs(x = NULL,
           y = "Proportion") +
      expand_limits(y=c(0, 0.55))
    
    ggsave("figures/wtp_amount_2a_brazil.pdf", width = 12, height = 8, dpi = 300)
    
    ggplot(drop_na(data_Brazil, wtp_amount_2b), aes(x=wtp_amount_2b)) +
      facet_wrap(~factor(wtpVal,
                         levels = c(65, 135, 270, 535, 1340),
                         labels = c(32.5, 67.5, 135, 267.5, 670))) +
      geom_bar(aes(y = (..count..)/sum(..count..)), stat = "count") +
      scale_y_continuous(labels = scales::percent, breaks = seq(0, 1, by = 0.05)) +
      geom_text(aes(label = ..count.., y = ((..count..)/sum(..count..))), 
                stat = "count", vjust = -0.5, size = 6) +
      labs(x = NULL,
           y = "Proportion") +
      expand_limits(y=c(0, 0.4))
    
    ggsave("figures/wtp_amount_2b_brazil.pdf", width = 12, height = 8, dpi = 300)

    ggplot(drop_na(data_Brazil, geq_taxes_1), aes(x=geq_taxes_1)) +
      facet_wrap(~factor(taxesExtra,
                         levels = c(15, 65, 135, 670, 1340, 6690),
                         labels = c(15, 65, 135, 670, 1340, 6690))) +
      geom_bar(aes(y = (..count..)/sum(..count..)), stat = "count") +
      scale_y_continuous(labels = scales::percent, breaks = seq(0, 1, by = 0.05)) +
      geom_text(aes(label = ..count.., y = ((..count..)/sum(..count..))), 
                stat = "count", vjust = -0.5, size = 6) +
      labs(x = NULL,
           y = "Proportion") +
      expand_limits(y=c(0, 0.25))
    
    ggsave("figures/geq_taxes_1_brazil.pdf", width = 12, height = 8, dpi = 300)
    
    ggplot(drop_na(data_Brazil, geq_taxes_2a), aes(x=geq_taxes_2a)) +
      facet_wrap(~factor(taxesExtra,
                         levels = c(15, 65, 135, 670, 1340, 6690),
                         labels = c(30, 130, 270, 1340, 2680, 13380))) +
      geom_bar(aes(y = (..count..)/sum(..count..)), stat = "count") +
      scale_y_continuous(labels = scales::percent, breaks = seq(0, 1, by = 0.05)) +
      geom_text(aes(label = ..count.., y = ((..count..)/sum(..count..))), 
                stat = "count", vjust = -0.5, size = 6) +
      labs(x = NULL,
           y = "Proportion") +
      expand_limits(y=c(0, 0.55))
    
    ggsave("figures/geq_taxes_2a_brazil.pdf", width = 12, height = 8, dpi = 300)
    
    ggplot(drop_na(data_Brazil, geq_taxes_2b), aes(x=geq_taxes_2b)) +
      facet_wrap(~factor(taxesExtra,
                         levels = c(15, 65, 135, 670, 1340, 6690),
                         labels = c(7.5, 32.5, 67.5, 335, 670, 3345))) +
      geom_bar(aes(y = (..count..)/sum(..count..)), stat = "count") +
      scale_y_continuous(labels = scales::percent, breaks = seq(0, 1, by = 0.05)) +
      geom_text(aes(label = ..count.., y = ((..count..)/sum(..count..))), 
                stat = "count", vjust = -0.5, size = 6) +
      labs(x = NULL,
           y = "Proportion") +
      expand_limits(y=c(0, 0.4))
    
    ggsave("figures/geq_taxes_2b_brazil.pdf", width = 12, height = 8, dpi = 300)
    
    ggplot(drop_na(data_Brazil, geq_ticket_1), aes(x=geq_ticket_1)) +
      facet_wrap(~factor(ticketExtra,
                         levels = c(15, 25, 65, 135, 270),
                         labels = c(15, 25, 65, 135, 270))) +
      geom_bar(aes(y = (..count..)/sum(..count..)), stat = "count") +
      scale_y_continuous(labels = scales::percent, breaks = seq(0, 1, by = 0.05)) +
      geom_text(aes(label = ..count.., y = ((..count..)/sum(..count..))), 
                stat = "count", vjust = -0.5, size = 6) +
      labs(x = NULL,
           y = "Proportion") +
      expand_limits(y=c(0, 0.25))
    
    ggsave("figures/geq_ticket_1_brazil.pdf", width = 12, height = 8, dpi = 300)
    
    ggplot(drop_na(data_Brazil, geq_ticket_2a), aes(x=geq_ticket_2a)) +
      facet_wrap(~factor(ticketExtra,
                         levels = c(15, 25, 65, 135, 270),
                         labels = c(30, 50, 130, 270, 540))) +
      geom_bar(aes(y = (..count..)/sum(..count..)), stat = "count") +
      scale_y_continuous(labels = scales::percent, breaks = seq(0, 1, by = 0.05)) +
      geom_text(aes(label = ..count.., y = ((..count..)/sum(..count..))), 
                stat = "count", vjust = -0.5, size = 6) +
      labs(x = NULL,
           y = "Proportion") +
      expand_limits(y=c(0, 0.55))
    
    ggsave("figures/geq_ticket_2a_brazil.pdf", width = 12, height = 8, dpi = 300)
    
    ggplot(drop_na(data_Brazil, geq_ticket_2b), aes(x=geq_ticket_2b)) +
      facet_wrap(~factor(ticketExtra,
                         levels = c(15, 25, 65, 135, 270),
                         labels = c(7.5, 12.5, 32.5, 67.5, 135))) +
      geom_bar(aes(y = (..count..)/sum(..count..)), stat = "count") +
      scale_y_continuous(labels = scales::percent, breaks = seq(0, 1, by = 0.05)) +
      geom_text(aes(label = ..count.., y = ((..count..)/sum(..count..))), 
                stat = "count", vjust = -0.5, size = 6) +
      labs(x = NULL,
           y = "Proportion") +
      expand_limits(y=c(0, 0.4))
    
    ggsave("figures/geq_ticket_2b_brazil.pdf", width = 12, height = 8, dpi = 300)
    
    rm(data_Brazil)
        
  }
  
  if (country == "Canada") {
    
    data_Canada <- data %>%
      filter(country == "Canada")
    
    ggplot(drop_na(data_Canada, wtp_amount_1), aes(x=wtp_amount_1)) +
      facet_wrap(~factor(wtpVal,
                         levels = c(50, 105, 210, 415, 1040),
                         labels = c(50, 105, 210, 415, 1040))) +
      geom_bar(aes(y = (..count..)/sum(..count..)), stat = "count") +
      scale_y_continuous(labels = scales::percent, breaks = seq(0, 1, by = 0.05)) +
      geom_text(aes(label = ..count.., y = ((..count..)/sum(..count..))), 
                stat = "count", vjust = -0.5, size = 6) +
      labs(x = NULL,
           y = "Proportion") +
      expand_limits(y=c(0, 0.25))
    
    ggsave("figures/wtp_amount_1_canada.pdf", width = 12, height = 8, dpi = 300)
    
    ggplot(drop_na(data_Canada, wtp_amount_2a), aes(x=wtp_amount_2a)) +
      facet_wrap(~factor(wtpVal,
                         levels = c(50, 105, 210, 415, 1040),
                         labels = c(100, 210, 420, 830, 2080))) +
      geom_bar(aes(y = (..count..)/sum(..count..)), stat = "count") +
      scale_y_continuous(labels = scales::percent, breaks = seq(0, 1, by = 0.05)) +
      geom_text(aes(label = ..count.., y = ((..count..)/sum(..count..))), 
                stat = "count", vjust = -0.5, size = 6) +
      labs(x = NULL,
           y = "Proportion") +
      expand_limits(y=c(0, 0.55))
    
    ggsave("figures/wtp_amount_2a_canada.pdf", width = 12, height = 8, dpi = 300)
    
    ggplot(drop_na(data_Canada, wtp_amount_2b), aes(x=wtp_amount_2b)) +
      facet_wrap(~factor(wtpVal,
                         levels = c(50, 105, 210, 415, 1040),
                         labels = c(25, 52.5, 105, 207.5, 520))) +
      geom_bar(aes(y = (..count..)/sum(..count..)), stat = "count") +
      scale_y_continuous(labels = scales::percent, breaks = seq(0, 1, by = 0.05)) +
      geom_text(aes(label = ..count.., y = ((..count..)/sum(..count..))), 
                stat = "count", vjust = -0.5, size = 6) +
      labs(x = NULL,
           y = "Proportion") +
      expand_limits(y=c(0, 0.4))
    
    ggsave("figures/wtp_amount_2b_canada.pdf", width = 12, height = 8, dpi = 300)

    ggplot(drop_na(data_Canada, geq_taxes_1), aes(x=geq_taxes_1)) +
      facet_wrap(~factor(taxesExtra,
                         levels = c(10, 50, 105, 520, 1040, 5205),
                         labels = c(10, 50, 105, 520, 1040, 5205))) +
      geom_bar(aes(y = (..count..)/sum(..count..)), stat = "count") +
      scale_y_continuous(labels = scales::percent, breaks = seq(0, 1, by = 0.05)) +
      geom_text(aes(label = ..count.., y = ((..count..)/sum(..count..))), 
                stat = "count", vjust = -0.5, size = 6) +
      labs(x = NULL,
           y = "Proportion") +
      expand_limits(y=c(0, 0.25))
    
    ggsave("figures/geq_taxes_1_canada.pdf", width = 12, height = 8, dpi = 300)
    
    ggplot(drop_na(data_Canada, geq_taxes_2a), aes(x=geq_taxes_2a)) +
      facet_wrap(~factor(taxesExtra,
                         levels = c(10, 50, 105, 520, 1040, 5205),
                         labels = c(20, 100, 210, 1040, 2080, 10410))) +
      geom_bar(aes(y = (..count..)/sum(..count..)), stat = "count") +
      scale_y_continuous(labels = scales::percent, breaks = seq(0, 1, by = 0.05)) +
      geom_text(aes(label = ..count.., y = ((..count..)/sum(..count..))), 
                stat = "count", vjust = -0.5, size = 6) +
      labs(x = NULL,
           y = "Proportion") +
      expand_limits(y=c(0, 0.55))
    
    ggsave("figures/geq_taxes_2a_canada.pdf", width = 12, height = 8, dpi = 300)
    
    ggplot(drop_na(data_Canada, geq_taxes_2b), aes(x=geq_taxes_2b)) +
      facet_wrap(~factor(taxesExtra,
                         levels = c(10, 50, 105, 520, 1040, 5205),
                         labels = c(5, 25, 52.5, 260, 520, 2602.5))) +
      geom_bar(aes(y = (..count..)/sum(..count..)), stat = "count") +
      scale_y_continuous(labels = scales::percent, breaks = seq(0, 1, by = 0.05)) +
      geom_text(aes(label = ..count.., y = ((..count..)/sum(..count..))), 
                stat = "count", vjust = -0.5, size = 6) +
      labs(x = NULL,
           y = "Proportion") +
      expand_limits(y=c(0, 0.4))
    
    ggsave("figures/geq_taxes_2b_canada.pdf", width = 12, height = 8, dpi = 300)
    
    ggplot(drop_na(data_Canada, geq_ticket_1), aes(x=geq_ticket_1)) +
      facet_wrap(~factor(ticketExtra,
                         levels = c(10, 20, 50, 105, 210),
                         labels = c(10, 20, 50, 105, 210))) +
      geom_bar(aes(y = (..count..)/sum(..count..)), stat = "count") +
      scale_y_continuous(labels = scales::percent, breaks = seq(0, 1, by = 0.05)) +
      geom_text(aes(label = ..count.., y = ((..count..)/sum(..count..))), 
                stat = "count", vjust = -0.5, size = 6) +
      labs(x = NULL,
           y = "Proportion") +
      expand_limits(y=c(0, 0.25))
    
    ggsave("figures/geq_ticket_1_canada.pdf", width = 12, height = 8, dpi = 300)
    
    ggplot(drop_na(data_Canada, geq_ticket_2a), aes(x=geq_ticket_2a)) +
      facet_wrap(~factor(ticketExtra,
                         levels = c(10, 20, 50, 105, 210),
                         labels = c(20, 40, 100, 210, 420))) +
      geom_bar(aes(y = (..count..)/sum(..count..)), stat = "count") +
      scale_y_continuous(labels = scales::percent, breaks = seq(0, 1, by = 0.05)) +
      geom_text(aes(label = ..count.., y = ((..count..)/sum(..count..))), 
                stat = "count", vjust = -0.5, size = 6) +
      labs(x = NULL,
           y = "Proportion") +
      expand_limits(y=c(0, 0.55))
    
    ggsave("figures/geq_ticket_2a_canada.pdf", width = 12, height = 8, dpi = 300)
    
    ggplot(drop_na(data_Canada, geq_ticket_2b), aes(x=geq_ticket_2b)) +
      facet_wrap(~factor(ticketExtra,
                         levels = c(10, 20, 50, 105, 210),
                         labels = c(5, 10, 25, 52.5, 105))) +
      geom_bar(aes(y = (..count..)/sum(..count..)), stat = "count") +
      scale_y_continuous(labels = scales::percent, breaks = seq(0, 1, by = 0.05)) +
      geom_text(aes(label = ..count.., y = ((..count..)/sum(..count..))), 
                stat = "count", vjust = -0.5, size = 6) +
      labs(x = NULL,
           y = "Proportion") +
      expand_limits(y=c(0, 0.4))
    
    ggsave("figures/geq_ticket_2b_canada.pdf", width = 12, height = 8, dpi = 300)
    
    rm(data_Canada)
        
  }
  
  if (country == "Colombia") {
    
    data_Colombia <- data %>%
      filter(country == "Colombia")
    
    ggplot(drop_na(data_Colombia, wtp_amount_1), aes(x=wtp_amount_1)) +
      facet_wrap(~factor(wtpVal,
                         levels = c(44000, 87000, 175000, 349000, 874000),
                         labels = c(44000, 87000, 175000, 349000, 874000))) +
      geom_bar(aes(y = (..count..)/sum(..count..)), stat = "count") +
      scale_y_continuous(labels = scales::percent, breaks = seq(0, 1, by = 0.05)) +
      geom_text(aes(label = ..count.., y = ((..count..)/sum(..count..))), 
                stat = "count", vjust = -0.5, size = 6) +
      labs(x = NULL,
           y = "Proportion") +
      expand_limits(y=c(0, 0.25))
    
    ggsave("figures/wtp_amount_1_colombia.pdf", width = 12, height = 8, dpi = 300)
    
    ggplot(drop_na(data_Colombia, wtp_amount_2a), aes(x=wtp_amount_2a)) +
      facet_wrap(~factor(wtpVal,
                         levels = c(44000, 87000, 175000, 349000, 874000),
                         labels = c(88000, 174000, 350000, 698000, 1748000))) +
      geom_bar(aes(y = (..count..)/sum(..count..)), stat = "count") +
      scale_y_continuous(labels = scales::percent, breaks = seq(0, 1, by = 0.05)) +
      geom_text(aes(label = ..count.., y = ((..count..)/sum(..count..))), 
                stat = "count", vjust = -0.5, size = 6) +
      labs(x = NULL,
           y = "Proportion") +
      expand_limits(y=c(0, 0.55))
    
    ggsave("figures/wtp_amount_2a_colombia.pdf", width = 12, height = 8, dpi = 300)
    
    ggplot(drop_na(data_Colombia, wtp_amount_2b), aes(x=wtp_amount_2b)) +
      facet_wrap(~factor(wtpVal,
                         levels = c(44000, 87000, 175000, 349000, 874000),
                         labels = c(22000, 43500, 87500, 174500, 437000))) +
      geom_bar(aes(y = (..count..)/sum(..count..)), stat = "count") +
      scale_y_continuous(labels = scales::percent, breaks = seq(0, 1, by = 0.05)) +
      geom_text(aes(label = ..count.., y = ((..count..)/sum(..count..))), 
                stat = "count", vjust = -0.5, size = 6) +
      labs(x = NULL,
           y = "Proportion") +
      expand_limits(y=c(0, 0.4))
    
    ggsave("figures/wtp_amount_2b_colombia.pdf", width = 12, height = 8, dpi = 300)
    
    ggplot(drop_na(data_Colombia, geq_taxes_1), aes(x=geq_taxes_1)) +
      facet_wrap(~factor(taxesExtra,
                         levels = c(9000, 44000, 87000, 437000, 874000, 4328000),
                         labels = c(9000, 44000, 87000, 437000, 874000, 4328000))) +
      geom_bar(aes(y = (..count..)/sum(..count..)), stat = "count") +
      scale_y_continuous(labels = scales::percent, breaks = seq(0, 1, by = 0.05)) +
      geom_text(aes(label = ..count.., y = ((..count..)/sum(..count..))), 
                stat = "count", vjust = -0.5, size = 6) +
      labs(x = NULL,
           y = "Proportion") +
      expand_limits(y=c(0, 0.25))
    
    ggsave("figures/geq_taxes_1_colombia.pdf", width = 12, height = 8, dpi = 300)
    
    ggplot(drop_na(data_Colombia, geq_taxes_2a), aes(x=geq_taxes_2a)) +
      facet_wrap(~factor(taxesExtra,
                         levels = c(9000, 44000, 87000, 437000, 874000, 4328000),
                         labels = c(18000, 88000, 174000, 874000, 1748000, 8656000))) +
      geom_bar(aes(y = (..count..)/sum(..count..)), stat = "count") +
      scale_y_continuous(labels = scales::percent, breaks = seq(0, 1, by = 0.05)) +
      geom_text(aes(label = ..count.., y = ((..count..)/sum(..count..))), 
                stat = "count", vjust = -0.5, size = 6) +
      labs(x = NULL,
           y = "Proportion") +
      expand_limits(y=c(0, 0.55))
    
    ggsave("figures/geq_taxes_2a_colombia.pdf", width = 12, height = 8, dpi = 300)
    
    ggplot(drop_na(data_Colombia, geq_taxes_2b), aes(x=geq_taxes_2b)) +
      facet_wrap(~factor(taxesExtra,
                         levels = c(9000, 44000, 87000, 437000, 874000, 4328000),
                         labels = c(4500, 22000, 43500, 218500, 437000, 2164000))) +
      geom_bar(aes(y = (..count..)/sum(..count..)), stat = "count") +
      scale_y_continuous(labels = scales::percent, breaks = seq(0, 1, by = 0.05)) +
      geom_text(aes(label = ..count.., y = ((..count..)/sum(..count..))), 
                stat = "count", vjust = -0.5, size = 6) +
      labs(x = NULL,
           y = "Proportion") +
      expand_limits(y=c(0, 0.4))
    
    ggsave("figures/geq_taxes_2b_colombia.pdf", width = 12, height = 8, dpi = 300)
    
    ggplot(drop_na(data_Colombia, geq_ticket_1), aes(x=geq_ticket_1)) +
      facet_wrap(~factor(ticketExtra,
                         levels = c(9000, 17000, 44000, 87000, 175000),
                         labels = c(9000, 17000, 44000, 87000, 175000))) +
      geom_bar(aes(y = (..count..)/sum(..count..)), stat = "count") +
      scale_y_continuous(labels = scales::percent, breaks = seq(0, 1, by = 0.05)) +
      geom_text(aes(label = ..count.., y = ((..count..)/sum(..count..))), 
                stat = "count", vjust = -0.5, size = 6) +
      labs(x = NULL,
           y = "Proportion") +
      expand_limits(y=c(0, 0.25))
    
    ggsave("figures/geq_ticket_1_colombia.pdf", width = 12, height = 8, dpi = 300)
    
    ggplot(drop_na(data_Colombia, geq_ticket_2a), aes(x=geq_ticket_2a)) +
      facet_wrap(~factor(ticketExtra,
                         levels = c(9000, 17000, 44000, 87000, 175000),
                         labels = c(18000, 34000, 88000, 174000, 350000))) +
      geom_bar(aes(y = (..count..)/sum(..count..)), stat = "count") +
      scale_y_continuous(labels = scales::percent, breaks = seq(0, 1, by = 0.05)) +
      geom_text(aes(label = ..count.., y = ((..count..)/sum(..count..))), 
                stat = "count", vjust = -0.5, size = 6) +
      labs(x = NULL,
           y = "Proportion") +
      expand_limits(y=c(0, 0.55))
    
    ggsave("figures/geq_ticket_2a_colombia.pdf", width = 12, height = 8, dpi = 300)
    
    ggplot(drop_na(data_Colombia, geq_ticket_2b), aes(x=geq_ticket_2b)) +
      facet_wrap(~factor(ticketExtra,
                         levels = c(9000, 17000, 44000, 87000, 175000),
                         labels = c(4500, 8500, 22000, 43500, 87500))) +
      geom_bar(aes(y = (..count..)/sum(..count..)), stat = "count") +
      scale_y_continuous(labels = scales::percent, breaks = seq(0, 1, by = 0.05)) +
      geom_text(aes(label = ..count.., y = ((..count..)/sum(..count..))), 
                stat = "count", vjust = -0.5, size = 6) +
      labs(x = NULL,
           y = "Proportion") +
      expand_limits(y=c(0, 0.4))
    
    ggsave("figures/geq_ticket_2b_colombia.pdf", width = 12, height = 8, dpi = 300)
    
    rm(data_Colombia)
    
  }
  
  if (country == "Chile") {
    
    data_Chile <- data %>%
      filter(country == "Chile")
    
    ggplot(drop_na(data_Chile, wtp_amount_1), aes(x=wtp_amount_1)) +
      facet_wrap(~factor(wtpVal,
                         levels = c(15000, 30000, 60000, 119000, 299000),
                         labels = c(15000, 30000, 60000, 119000, 299000))) +
      geom_bar(aes(y = (..count..)/sum(..count..)), stat = "count") +
      scale_y_continuous(labels = scales::percent, breaks = seq(0, 1, by = 0.05)) +
      geom_text(aes(label = ..count.., y = ((..count..)/sum(..count..))), 
                stat = "count", vjust = -0.5, size = 6) +
      labs(x = NULL,
           y = "Proportion") +
      expand_limits(y=c(0, 0.25))
    
    ggsave("figures/wtp_amount_1_chile.pdf", width = 12, height = 8, dpi = 300)
    
    ggplot(drop_na(data_Chile, wtp_amount_2a), aes(x=wtp_amount_2a)) +
      facet_wrap(~factor(wtpVal,
                         levels = c(15000, 30000, 60000, 119000, 299000),
                         labels = c(30000, 60000, 120000, 238000, 598000))) +
      geom_bar(aes(y = (..count..)/sum(..count..)), stat = "count") +
      scale_y_continuous(labels = scales::percent, breaks = seq(0, 1, by = 0.05)) +
      geom_text(aes(label = ..count.., y = ((..count..)/sum(..count..))), 
                stat = "count", vjust = -0.5, size = 6) +
      labs(x = NULL,
           y = "Proportion") +
      expand_limits(y=c(0, 0.55))
    
    ggsave("figures/wtp_amount_2a_chile.pdf", width = 12, height = 8, dpi = 300)
    
    ggplot(drop_na(data_Chile, wtp_amount_2b), aes(x=wtp_amount_2b)) +
      facet_wrap(~factor(wtpVal,
                         levels = c(15000, 30000, 60000, 119000, 299000),
                         labels = c(7500, 15000, 30000, 59500, 149500))) +
      geom_bar(aes(y = (..count..)/sum(..count..)), stat = "count") +
      scale_y_continuous(labels = scales::percent, breaks = seq(0, 1, by = 0.05)) +
      geom_text(aes(label = ..count.., y = ((..count..)/sum(..count..))), 
                stat = "count", vjust = -0.5, size = 6) +
      labs(x = NULL,
           y = "Proportion") +
      expand_limits(y=c(0, 0.4))
    
    ggsave("figures/wtp_amount_2b_chile.pdf", width = 12, height = 8, dpi = 300)
    
    ggplot(drop_na(data_Chile, geq_taxes_1), aes(x=geq_taxes_1)) +
      facet_wrap(~factor(taxesExtra,
                         levels = c(3000, 15000, 30000, 149000, 299000, 1493000),
                         labels = c(3000, 15000, 30000, 149000, 299000, 1493000))) +
      geom_bar(aes(y = (..count..)/sum(..count..)), stat = "count") +
      scale_y_continuous(labels = scales::percent, breaks = seq(0, 1, by = 0.05)) +
      geom_text(aes(label = ..count.., y = ((..count..)/sum(..count..))), 
                stat = "count", vjust = -0.5, size = 6) +
      labs(x = NULL,
           y = "Proportion") +
      expand_limits(y=c(0, 0.25))
    
    ggsave("figures/geq_taxes_1_chile.pdf", width = 12, height = 8, dpi = 300)
    
    ggplot(drop_na(data_Chile, geq_taxes_2a), aes(x=geq_taxes_2a)) +
      facet_wrap(~factor(taxesExtra,
                         levels = c(3000, 15000, 30000, 149000, 299000, 1493000),
                         labels = c(6000, 30000, 60000, 298000, 598000, 2986000))) +
      geom_bar(aes(y = (..count..)/sum(..count..)), stat = "count") +
      scale_y_continuous(labels = scales::percent, breaks = seq(0, 1, by = 0.05)) +
      geom_text(aes(label = ..count.., y = ((..count..)/sum(..count..))), 
                stat = "count", vjust = -0.5, size = 6) +
      labs(x = NULL,
           y = "Proportion") +
      expand_limits(y=c(0, 0.55))
    
    ggsave("figures/geq_taxes_2a_chile.pdf", width = 12, height = 8, dpi = 300)
    
    ggplot(drop_na(data_Chile, geq_taxes_2b), aes(x=geq_taxes_2b)) +
      facet_wrap(~factor(taxesExtra,
                         levels = c(3000, 15000, 30000, 149000, 299000, 1493000),
                         labels = c(1500, 7500, 15000, 74500, 149500, 746500))) +
      geom_bar(aes(y = (..count..)/sum(..count..)), stat = "count") +
      scale_y_continuous(labels = scales::percent, breaks = seq(0, 1, by = 0.05)) +
      geom_text(aes(label = ..count.., y = ((..count..)/sum(..count..))), 
                stat = "count", vjust = -0.5, size = 6) +
      labs(x = NULL,
           y = "Proportion") +
      expand_limits(y=c(0, 0.4))
    
    ggsave("figures/geq_taxes_2b_chile.pdf", width = 12, height = 8, dpi = 300)
    
    ggplot(drop_na(data_Chile, geq_ticket_1), aes(x=geq_ticket_1)) +
      facet_wrap(~factor(ticketExtra,
                         levels = c(3000, 6000, 15000, 30000, 60000),
                         labels = c(3000, 6000, 15000, 30000, 60000))) +
      geom_bar(aes(y = (..count..)/sum(..count..)), stat = "count") +
      scale_y_continuous(labels = scales::percent, breaks = seq(0, 1, by = 0.05)) +
      geom_text(aes(label = ..count.., y = ((..count..)/sum(..count..))), 
                stat = "count", vjust = -0.5, size = 6) +
      labs(x = NULL,
           y = "Proportion") +
      expand_limits(y=c(0, 0.25))
    
    ggsave("figures/geq_ticket_1_chile.pdf", width = 12, height = 8, dpi = 300)
    
    ggplot(drop_na(data_Chile, geq_ticket_2a), aes(x=geq_ticket_2a)) +
      facet_wrap(~factor(ticketExtra,
                         levels = c(3000, 6000, 15000, 30000, 60000),
                         labels = c(6000, 12000, 30000, 60000, 120000))) +
      geom_bar(aes(y = (..count..)/sum(..count..)), stat = "count") +
      scale_y_continuous(labels = scales::percent, breaks = seq(0, 1, by = 0.05)) +
      geom_text(aes(label = ..count.., y = ((..count..)/sum(..count..))), 
                stat = "count", vjust = -0.5, size = 6) +
      labs(x = NULL,
           y = "Proportion") +
      expand_limits(y=c(0, 0.55))
    
    ggsave("figures/geq_ticket_2a_chile.pdf", width = 12, height = 8, dpi = 300)
    
    ggplot(drop_na(data_Chile, geq_ticket_2b), aes(x=geq_ticket_2b)) +
      facet_wrap(~factor(ticketExtra,
                         levels = c(3000, 6000, 15000, 30000, 60000),
                         labels = c(1500, 3000, 7500, 15000, 30000))) +
      geom_bar(aes(y = (..count..)/sum(..count..)), stat = "count") +
      scale_y_continuous(labels = scales::percent, breaks = seq(0, 1, by = 0.05)) +
      geom_text(aes(label = ..count.., y = ((..count..)/sum(..count..))), 
                stat = "count", vjust = -0.5, size = 6) +
      labs(x = NULL,
           y = "Proportion") +
      expand_limits(y=c(0, 0.4))
    
    ggsave("figures/geq_ticket_2b_chile.pdf", width = 12, height = 8, dpi = 300)
    
    rm(data_Chile)
    
  }
  
  if (country == "China") {
    
    data_China <- data %>%
      filter(country == "China") 
    
    ggplot(drop_na(data_China, wtp_amount_1), aes(x=wtp_amount_1)) +
      facet_wrap(~factor(wtpVal,
                         levels = c(85, 175, 345, 690, 1730),
                         labels = c(85, 175, 345, 690, 1730))) +
      geom_bar(aes(y = (..count..)/sum(..count..)), stat = "count") +
      scale_y_continuous(labels = scales::percent, breaks = seq(0, 1, by = 0.05)) +
      geom_text(aes(label = ..count.., y = ((..count..)/sum(..count..))), 
                stat = "count", vjust = -0.5, size = 6) +
      labs(x = NULL,
           y = "Proportion") +
      expand_limits(y=c(0, 0.25))
    
    ggsave("figures/wtp_amount_1_china.pdf", width = 12, height = 8, dpi = 300)
    
    ggplot(drop_na(data_China, wtp_amount_2a), aes(x=wtp_amount_2a)) +
      facet_wrap(~factor(wtpVal,
                         levels = c(85, 175, 345, 690, 1730),
                         labels = c(170, 350, 690, 1380, 3460))) +
      geom_bar(aes(y = (..count..)/sum(..count..)), stat = "count") +
      scale_y_continuous(labels = scales::percent, breaks = seq(0, 1, by = 0.05)) +
      geom_text(aes(label = ..count.., y = ((..count..)/sum(..count..))), 
                stat = "count", vjust = -0.5, size = 6) +
      labs(x = NULL,
           y = "Proportion") +
      expand_limits(y=c(0, 0.55))
    
    ggsave("figures/wtp_amount_2a_china.pdf", width = 12, height = 8, dpi = 300)
    
    ggplot(drop_na(data_China, wtp_amount_2b), aes(x=wtp_amount_2b)) +
      facet_wrap(~factor(wtpVal,
                         levels = c(85, 175, 345, 690, 1730),
                         labels = c(42.5, 87.5, 172.5, 345, 865))) +
      geom_bar(aes(y = (..count..)/sum(..count..)), stat = "count") +
      scale_y_continuous(labels = scales::percent, breaks = seq(0, 1, by = 0.05)) +
      geom_text(aes(label = ..count.., y = ((..count..)/sum(..count..))), 
                stat = "count", vjust = -0.5, size = 6) +
      labs(x = NULL,
           y = "Proportion") +
      expand_limits(y=c(0, 0.4))
    
    ggsave("figures/wtp_amount_2b_china.pdf", width = 12, height = 8, dpi = 300)

    ggplot(drop_na(data_China, geq_taxes_1), aes(x=geq_taxes_1)) +
      facet_wrap(~factor(taxesExtra,
                         levels = c(15, 85, 175, 865, 1730, 8650),
                         labels = c(15, 85, 175, 865, 1730, 8650))) +
      geom_bar(aes(y = (..count..)/sum(..count..)), stat = "count") +
      scale_y_continuous(labels = scales::percent, breaks = seq(0, 1, by = 0.05)) +
      geom_text(aes(label = ..count.., y = ((..count..)/sum(..count..))), 
                stat = "count", vjust = -0.5, size = 6) +
      labs(x = NULL,
           y = "Proportion") +
      expand_limits(y=c(0, 0.25))
    
    ggsave("figures/geq_taxes_1_china.pdf", width = 12, height = 8, dpi = 300)
    
    ggplot(drop_na(data_China, geq_taxes_2a), aes(x=geq_taxes_2a)) +
      facet_wrap(~factor(taxesExtra,
                         levels = c(15, 85, 175, 865, 1730, 8650),
                         labels = c(30, 170, 350, 1730, 3460, 17300))) +
      geom_bar(aes(y = (..count..)/sum(..count..)), stat = "count") +
      scale_y_continuous(labels = scales::percent, breaks = seq(0, 1, by = 0.05)) +
      geom_text(aes(label = ..count.., y = ((..count..)/sum(..count..))), 
                stat = "count", vjust = -0.5, size = 6) +
      labs(x = NULL,
           y = "Proportion") +
      expand_limits(y=c(0, 0.55))
    
    ggsave("figures/geq_taxes_2a_china.pdf", width = 12, height = 8, dpi = 300)
    
    ggplot(drop_na(data_China, geq_taxes_2b), aes(x=geq_taxes_2b)) +
      facet_wrap(~factor(taxesExtra,
                         levels = c(15, 85, 175, 865, 1730, 8650),
                         labels = c(7.5, 42.5, 87.5, 432.5, 865, 4325))) +
      geom_bar(aes(y = (..count..)/sum(..count..)), stat = "count") +
      scale_y_continuous(labels = scales::percent, breaks = seq(0, 1, by = 0.05)) +
      geom_text(aes(label = ..count.., y = ((..count..)/sum(..count..))), 
                stat = "count", vjust = -0.5, size = 6) +
      labs(x = NULL,
           y = "Proportion") +
      expand_limits(y=c(0, 0.4))
    
    ggsave("figures/geq_taxes_2b_china.pdf", width = 12, height = 8, dpi = 300)
    
    ggplot(drop_na(data_China, geq_ticket_1), aes(x=geq_ticket_1)) +
      facet_wrap(~factor(ticketExtra,
                         levels = c(15, 35, 85, 175, 345),
                         labels = c(15, 35, 85, 175, 345))) +
      geom_bar(aes(y = (..count..)/sum(..count..)), stat = "count") +
      scale_y_continuous(labels = scales::percent, breaks = seq(0, 1, by = 0.05)) +
      geom_text(aes(label = ..count.., y = ((..count..)/sum(..count..))), 
                stat = "count", vjust = -0.5, size = 6) +
      labs(x = NULL,
           y = "Proportion") +
      expand_limits(y=c(0, 0.25))
    
    ggsave("figures/geq_ticket_1_china.pdf", width = 12, height = 8, dpi = 300)
    
    ggplot(drop_na(data_China, geq_ticket_2a), aes(x=geq_ticket_2a)) +
      facet_wrap(~factor(ticketExtra,
                         levels = c(15, 35, 85, 175, 345),
                         labels = c(30, 70, 170, 350, 690))) +
      geom_bar(aes(y = (..count..)/sum(..count..)), stat = "count") +
      scale_y_continuous(labels = scales::percent, breaks = seq(0, 1, by = 0.05)) +
      geom_text(aes(label = ..count.., y = ((..count..)/sum(..count..))), 
                stat = "count", vjust = -0.5, size = 6) +
      labs(x = NULL,
           y = "Proportion") +
      expand_limits(y=c(0, 0.55))
    
    ggsave("figures/geq_ticket_2a_china.pdf", width = 12, height = 8, dpi = 300)
    
    ggplot(drop_na(data_China, geq_ticket_2b), aes(x=geq_ticket_2b)) +
      facet_wrap(~factor(ticketExtra,
                         levels = c(15, 35, 85, 175, 345),
                         labels = c(7.5, 17.5, 42.5, 87.5, 172.5))) +
      geom_bar(aes(y = (..count..)/sum(..count..)), stat = "count") +
      scale_y_continuous(labels = scales::percent, breaks = seq(0, 1, by = 0.05)) +
      geom_text(aes(label = ..count.., y = ((..count..)/sum(..count..))), 
                stat = "count", vjust = -0.5, size = 6) +
      labs(x = NULL,
           y = "Proportion") +
      expand_limits(y=c(0, 0.4))
    
    ggsave("figures/geq_ticket_2b_china.pdf", width = 12, height = 8, dpi = 300)
    
    rm(data_China)
        
  }
  
  if (country == "France") {
    
    data_France <- data %>%
      filter(country == "France") 
    
    ggplot(drop_na(data_France, wtp_amount_1), aes(x=wtp_amount_1)) +
      facet_wrap(~factor(wtpVal,
                         levels = c(30, 65, 130, 260, 645),
                         labels = c(30, 65, 130, 260, 645))) +
      geom_bar(aes(y = (..count..)/sum(..count..)), stat = "count") +
      scale_y_continuous(labels = scales::percent, breaks = seq(0, 1, by = 0.05)) +
      geom_text(aes(label = ..count.., y = ((..count..)/sum(..count..))), 
                stat = "count", vjust = -0.5, size = 6) +
      labs(x = NULL,
           y = "Proportion") +
      expand_limits(y=c(0, 0.25))
    
    ggsave("figures/wtp_amount_1_france.pdf", width = 12, height = 8, dpi = 300)
    
    ggplot(drop_na(data_France, wtp_amount_2a), aes(x=wtp_amount_2a)) +
      facet_wrap(~factor(wtpVal,
                         levels = c(30, 65, 130, 260, 645),
                         labels = c(60, 130, 260, 520, 1290))) +
      geom_bar(aes(y = (..count..)/sum(..count..)), stat = "count") +
      scale_y_continuous(labels = scales::percent, breaks = seq(0, 1, by = 0.05)) +
      geom_text(aes(label = ..count.., y = ((..count..)/sum(..count..))), 
                stat = "count", vjust = -0.5, size = 6) +
      labs(x = NULL,
           y = "Proportion") +
      expand_limits(y=c(0, 0.55))
    
    ggsave("figures/wtp_amount_2a_france.pdf", width = 12, height = 8, dpi = 300)
    
    ggplot(drop_na(data_France, wtp_amount_2b), aes(x=wtp_amount_2b)) +
      facet_wrap(~factor(wtpVal,
                         levels = c(30, 65, 130, 260, 645),
                         labels = c(15, 32.5, 65, 130, 322.5))) +
      geom_bar(aes(y = (..count..)/sum(..count..)), stat = "count") +
      scale_y_continuous(labels = scales::percent, breaks = seq(0, 1, by = 0.05)) +
      geom_text(aes(label = ..count.., y = ((..count..)/sum(..count..))), 
                stat = "count", vjust = -0.5, size = 6) +
      labs(x = NULL,
           y = "Proportion") +
      expand_limits(y=c(0, 0.4))
    
    ggsave("figures/wtp_amount_2b_france.pdf", width = 12, height = 8, dpi = 300)
    
    ggplot(drop_na(data_France, geq_taxes_1), aes(x=geq_taxes_1)) +
      facet_wrap(~factor(taxesExtra,
                         levels = c(5, 30, 65, 325, 645, 3225),
                         labels = c(5, 30, 65, 325, 645, 3225))) +
      geom_bar(aes(y = (..count..)/sum(..count..)), stat = "count") +
      scale_y_continuous(labels = scales::percent, breaks = seq(0, 1, by = 0.05)) +
      geom_text(aes(label = ..count.., y = ((..count..)/sum(..count..))), 
                stat = "count", vjust = -0.5, size = 6) +
      labs(x = NULL,
           y = "Proportion") +
      expand_limits(y=c(0, 0.25))
    
    ggsave("figures/geq_taxes_1_france.pdf", width = 12, height = 8, dpi = 300)
    
    ggplot(drop_na(data_France, geq_taxes_2a), aes(x=geq_taxes_2a)) +
      facet_wrap(~factor(taxesExtra,
                         levels = c(5, 30, 65, 325, 645, 3225),
                         labels = c(10, 60, 130, 650, 1290, 6450))) +
      geom_bar(aes(y = (..count..)/sum(..count..)), stat = "count") +
      scale_y_continuous(labels = scales::percent, breaks = seq(0, 1, by = 0.05)) +
      geom_text(aes(label = ..count.., y = ((..count..)/sum(..count..))), 
                stat = "count", vjust = -0.5, size = 6) +
      labs(x = NULL,
           y = "Proportion") +
      expand_limits(y=c(0, 0.55))
    
    ggsave("figures/geq_taxes_2a_france.pdf", width = 12, height = 8, dpi = 300)
    
    ggplot(drop_na(data_France, geq_taxes_2b), aes(x=geq_taxes_2b)) +
      facet_wrap(~factor(taxesExtra,
                         levels = c(5, 30, 65, 325, 645, 3225),
                         labels = c(2.5, 15, 32.5, 162.5, 322.5, 1612.5))) +
      geom_bar(aes(y = (..count..)/sum(..count..)), stat = "count") +
      scale_y_continuous(labels = scales::percent, breaks = seq(0, 1, by = 0.05)) +
      geom_text(aes(label = ..count.., y = ((..count..)/sum(..count..))), 
                stat = "count", vjust = -0.5, size = 6) +
      labs(x = NULL,
           y = "Proportion") +
      expand_limits(y=c(0, 0.4))
    
    ggsave("figures/geq_taxes_2b_france.pdf", width = 12, height = 8, dpi = 300)
    
    ggplot(drop_na(data_France, geq_ticket_1), aes(x=geq_ticket_1)) +
      facet_wrap(~factor(ticketExtra,
                         levels = c(5, 15, 30, 65, 130),
                         labels = c(5, 15, 30, 65, 130))) +
      geom_bar(aes(y = (..count..)/sum(..count..)), stat = "count") +
      scale_y_continuous(labels = scales::percent, breaks = seq(0, 1, by = 0.05)) +
      geom_text(aes(label = ..count.., y = ((..count..)/sum(..count..))), 
                stat = "count", vjust = -0.5, size = 6) +
      labs(x = NULL,
           y = "Proportion") +
      expand_limits(y=c(0, 0.25))
    
    ggsave("figures/geq_ticket_1_france.pdf", width = 12, height = 8, dpi = 300)
    
    ggplot(drop_na(data_France, geq_ticket_2a), aes(x=geq_ticket_2a)) +
      facet_wrap(~factor(ticketExtra,
                         levels = c(5, 15, 30, 65, 130),
                         labels = c(10, 30, 60, 130, 260))) +
      geom_bar(aes(y = (..count..)/sum(..count..)), stat = "count") +
      scale_y_continuous(labels = scales::percent, breaks = seq(0, 1, by = 0.05)) +
      geom_text(aes(label = ..count.., y = ((..count..)/sum(..count..))), 
                stat = "count", vjust = -0.5, size = 6) +
      labs(x = NULL,
           y = "Proportion") +
      expand_limits(y=c(0, 0.55))
    
    ggsave("figures/geq_ticket_2a_france.pdf", width = 12, height = 8, dpi = 300)
    
    ggplot(drop_na(data_France, geq_ticket_2b), aes(x=geq_ticket_2b)) +
      facet_wrap(~factor(ticketExtra,
                         levels = c(5, 15, 30, 65, 130),
                         labels = c(2.5, 7.5, 15, 32.5, 65))) +
      geom_bar(aes(y = (..count..)/sum(..count..)), stat = "count") +
      scale_y_continuous(labels = scales::percent, breaks = seq(0, 1, by = 0.05)) +
      geom_text(aes(label = ..count.., y = ((..count..)/sum(..count..))), 
                stat = "count", vjust = -0.5, size = 6) +
      labs(x = NULL,
           y = "Proportion") +
      expand_limits(y=c(0, 0.4))
    
    ggsave("figures/geq_ticket_2b_france.pdf", width = 12, height = 8, dpi = 300)
    
    rm(data_France)
    
  }
  
  if (country == "India") {
    
    data_India <- data %>%
      filter(country == "India") 
    
    ggplot(drop_na(data_India, wtp_amount_1), aes(x=wtp_amount_1)) +
      facet_wrap(~factor(wtpVal,
                         levels = c(400, 795, 1595, 3190, 7975),
                         labels = c(400, 795, 1595, 3190, 7975))) +
      geom_bar(aes(y = (..count..)/sum(..count..)), stat = "count") +
      scale_y_continuous(labels = scales::percent, breaks = seq(0, 1, by = 0.05)) +
      geom_text(aes(label = ..count.., y = ((..count..)/sum(..count..))), 
                stat = "count", vjust = -0.5, size = 6) +
      labs(x = NULL,
           y = "Proportion") +
      expand_limits(y=c(0, 0.25))
    
    ggsave("figures/wtp_amount_1_india.pdf", width = 12, height = 8, dpi = 300)
    
    ggplot(drop_na(data_India, wtp_amount_2a), aes(x=wtp_amount_2a)) +
      facet_wrap(~factor(wtpVal,
                         levels = c(400, 795, 1595, 3190, 7975),
                         labels = c(800, 1590, 3190, 6380, 15950))) +
      geom_bar(aes(y = (..count..)/sum(..count..)), stat = "count") +
      scale_y_continuous(labels = scales::percent, breaks = seq(0, 1, by = 0.05)) +
      geom_text(aes(label = ..count.., y = ((..count..)/sum(..count..))), 
                stat = "count", vjust = -0.5, size = 6) +
      labs(x = NULL,
           y = "Proportion") +
      expand_limits(y=c(0, 0.55))
    
    ggsave("figures/wtp_amount_2a_india.pdf", width = 12, height = 8, dpi = 300)
    
    ggplot(drop_na(data_India, wtp_amount_2b), aes(x=wtp_amount_2b)) +
      facet_wrap(~factor(wtpVal,
                         levels = c(400, 795, 1595, 3190, 7975),
                         labels = c(200, 397.5, 797.5, 1595, 3987.5))) +
      geom_bar(aes(y = (..count..)/sum(..count..)), stat = "count") +
      scale_y_continuous(labels = scales::percent, breaks = seq(0, 1, by = 0.05)) +
      geom_text(aes(label = ..count.., y = ((..count..)/sum(..count..))), 
                stat = "count", vjust = -0.5, size = 6) +
      labs(x = NULL,
           y = "Proportion") +
      expand_limits(y=c(0, 0.4))
    
    ggsave("figures/wtp_amount_2b_india.pdf", width = 12, height = 8, dpi = 300)
    
    ggplot(drop_na(data_India, geq_taxes_1), aes(x=geq_taxes_1)) +
      facet_wrap(~factor(taxesExtra,
                         levels = c(80, 400, 795, 3985, 7975, 39865),
                         labels = c(80, 400, 795, 3985, 7975, 39865))) +
      geom_bar(aes(y = (..count..)/sum(..count..)), stat = "count") +
      scale_y_continuous(labels = scales::percent, breaks = seq(0, 1, by = 0.05)) +
      geom_text(aes(label = ..count.., y = ((..count..)/sum(..count..))), 
                stat = "count", vjust = -0.5, size = 6) +
      labs(x = NULL,
           y = "Proportion") +
      expand_limits(y=c(0, 0.25))
    
    ggsave("figures/geq_taxes_1_india.pdf", width = 12, height = 8, dpi = 300)
    
    ggplot(drop_na(data_India, geq_taxes_2a), aes(x=geq_taxes_2a)) +
      facet_wrap(~factor(taxesExtra,
                         levels = c(80, 400, 795, 3985, 7975, 39865),
                         labels = c(160, 800, 1590, 7970, 15950, 79730))) +
      geom_bar(aes(y = (..count..)/sum(..count..)), stat = "count") +
      scale_y_continuous(labels = scales::percent, breaks = seq(0, 1, by = 0.05)) +
      geom_text(aes(label = ..count.., y = ((..count..)/sum(..count..))), 
                stat = "count", vjust = -0.5, size = 6) +
      labs(x = NULL,
           y = "Proportion") +
      expand_limits(y=c(0, 0.55))
    
    ggsave("figures/geq_taxes_2a_india.pdf", width = 12, height = 8, dpi = 300)
    
    ggplot(drop_na(data_India, geq_taxes_2b), aes(x=geq_taxes_2b)) +
      facet_wrap(~factor(taxesExtra,
                         levels = c(80, 400, 795, 3985, 7975, 39865),
                         labels = c(40, 200, 397.5, 1992.5, 3987.5, 19932.5))) +
      geom_bar(aes(y = (..count..)/sum(..count..)), stat = "count") +
      scale_y_continuous(labels = scales::percent, breaks = seq(0, 1, by = 0.05)) +
      geom_text(aes(label = ..count.., y = ((..count..)/sum(..count..))), 
                stat = "count", vjust = -0.5, size = 6) +
      labs(x = NULL,
           y = "Proportion") +
      expand_limits(y=c(0, 0.4))
    
    ggsave("figures/geq_taxes_2b_india.pdf", width = 12, height = 8, dpi = 300)
    
    ggplot(drop_na(data_India, geq_ticket_1), aes(x=geq_ticket_1)) +
      facet_wrap(~factor(ticketExtra,
                         levels = c(80, 160, 400, 795, 1595),
                         labels = c(80, 160, 400, 795, 1595))) +
      geom_bar(aes(y = (..count..)/sum(..count..)), stat = "count") +
      scale_y_continuous(labels = scales::percent, breaks = seq(0, 1, by = 0.05)) +
      geom_text(aes(label = ..count.., y = ((..count..)/sum(..count..))), 
                stat = "count", vjust = -0.5, size = 6) +
      labs(x = NULL,
           y = "Proportion") +
      expand_limits(y=c(0, 0.25))
    
    ggsave("figures/geq_ticket_1_india.pdf", width = 12, height = 8, dpi = 300)
    
    ggplot(drop_na(data_India, geq_ticket_2a), aes(x=geq_ticket_2a)) +
      facet_wrap(~factor(ticketExtra,
                         levels = c(80, 160, 400, 795, 1595),
                         labels = c(160, 320, 800, 1590, 3190))) +
      geom_bar(aes(y = (..count..)/sum(..count..)), stat = "count") +
      scale_y_continuous(labels = scales::percent, breaks = seq(0, 1, by = 0.05)) +
      geom_text(aes(label = ..count.., y = ((..count..)/sum(..count..))), 
                stat = "count", vjust = -0.5, size = 6) +
      labs(x = NULL,
           y = "Proportion") +
      expand_limits(y=c(0, 0.55))
    
    ggsave("figures/geq_ticket_2a_india.pdf", width = 12, height = 8, dpi = 300)
    
    ggplot(drop_na(data_India, geq_ticket_2b), aes(x=geq_ticket_2b)) +
      facet_wrap(~factor(ticketExtra,
                         levels = c(80, 160, 400, 795, 1595),
                         labels = c(40, 80, 200, 397.5, 797.5))) +
      geom_bar(aes(y = (..count..)/sum(..count..)), stat = "count") +
      scale_y_continuous(labels = scales::percent, breaks = seq(0, 1, by = 0.05)) +
      geom_text(aes(label = ..count.., y = ((..count..)/sum(..count..))), 
                stat = "count", vjust = -0.5, size = 6) +
      labs(x = NULL,
           y = "Proportion") +
      expand_limits(y=c(0, 0.4))
    
    ggsave("figures/geq_ticket_2b_india.pdf", width = 12, height = 8, dpi = 300)
    
    rm(data_India)
    
  }
  
  if (country == "Italy") {
    
    data_Italy <- data %>%
      filter(country == "Italy") 
    
    ggplot(drop_na(data_Italy, wtp_amount_1), aes(x=wtp_amount_1)) +
      facet_wrap(~factor(wtpVal,
                         levels = c(30, 60, 115, 230, 575),
                         labels = c(30, 60, 115, 230, 575))) +
      geom_bar(aes(y = (..count..)/sum(..count..)), stat = "count") +
      scale_y_continuous(labels = scales::percent, breaks = seq(0, 1, by = 0.05)) +
      geom_text(aes(label = ..count.., y = ((..count..)/sum(..count..))), 
                stat = "count", vjust = -0.5, size = 6) +
      labs(x = NULL,
           y = "Proportion") +
      expand_limits(y=c(0, 0.25))
    
    ggsave("figures/wtp_amount_1_italy.pdf", width = 12, height = 8, dpi = 300)
    
    ggplot(drop_na(data_Italy, wtp_amount_2a), aes(x=wtp_amount_2a)) +
      facet_wrap(~factor(wtpVal,
                         levels = c(30, 60, 115, 230, 575),
                         labels = c(60, 120, 230, 460, 1150))) +
      geom_bar(aes(y = (..count..)/sum(..count..)), stat = "count") +
      scale_y_continuous(labels = scales::percent, breaks = seq(0, 1, by = 0.05)) +
      geom_text(aes(label = ..count.., y = ((..count..)/sum(..count..))), 
                stat = "count", vjust = -0.5, size = 6) +
      labs(x = NULL,
           y = "Proportion") +
      expand_limits(y=c(0, 0.55))
    
    ggsave("figures/wtp_amount_2a_italy.pdf", width = 12, height = 8, dpi = 300)
    
    ggplot(drop_na(data_Italy, wtp_amount_2b), aes(x=wtp_amount_2b)) +
      facet_wrap(~factor(wtpVal,
                         levels = c(30, 60, 115, 230, 575),
                         labels = c(15, 30, 57.5, 115, 287.5))) +
      geom_bar(aes(y = (..count..)/sum(..count..)), stat = "count") +
      scale_y_continuous(labels = scales::percent, breaks = seq(0, 1, by = 0.05)) +
      geom_text(aes(label = ..count.., y = ((..count..)/sum(..count..))), 
                stat = "count", vjust = -0.5, size = 6) +
      labs(x = NULL,
           y = "Proportion") +
      expand_limits(y=c(0, 0.4))
    
    ggsave("figures/wtp_amount_2b_italy.pdf", width = 12, height = 8, dpi = 300)
    
    ggplot(drop_na(data_Italy, geq_taxes_1), aes(x=geq_taxes_1)) +
      facet_wrap(~factor(taxesExtra,
                         levels = c(5, 30, 60, 290, 575, 2885),
                         labels = c(5, 30, 60, 290, 575, 2885))) +
      geom_bar(aes(y = (..count..)/sum(..count..)), stat = "count") +
      scale_y_continuous(labels = scales::percent, breaks = seq(0, 1, by = 0.05)) +
      geom_text(aes(label = ..count.., y = ((..count..)/sum(..count..))), 
                stat = "count", vjust = -0.5, size = 6) +
      labs(x = NULL,
           y = "Proportion") +
      expand_limits(y=c(0, 0.25))
    
    ggsave("figures/geq_taxes_1_italy.pdf", width = 12, height = 8, dpi = 300)
    
    ggplot(drop_na(data_Italy, geq_taxes_2a), aes(x=geq_taxes_2a)) +
      facet_wrap(~factor(taxesExtra,
                         levels = c(5, 30, 60, 290, 575, 2885),
                         labels = c(10, 60, 120, 580, 1150, 5770))) +
      geom_bar(aes(y = (..count..)/sum(..count..)), stat = "count") +
      scale_y_continuous(labels = scales::percent, breaks = seq(0, 1, by = 0.05)) +
      geom_text(aes(label = ..count.., y = ((..count..)/sum(..count..))), 
                stat = "count", vjust = -0.5, size = 6) +
      labs(x = NULL,
           y = "Proportion") +
      expand_limits(y=c(0, 0.55))
    
    ggsave("figures/geq_taxes_2a_italy.pdf", width = 12, height = 8, dpi = 300)
    
    ggplot(drop_na(data_Italy, geq_taxes_2b), aes(x=geq_taxes_2b)) +
      facet_wrap(~factor(taxesExtra,
                         levels = c(5, 30, 60, 290, 575, 2885),
                         labels = c(2.5, 15, 30, 145, 287.5, 1442.5))) +
      geom_bar(aes(y = (..count..)/sum(..count..)), stat = "count") +
      scale_y_continuous(labels = scales::percent, breaks = seq(0, 1, by = 0.05)) +
      geom_text(aes(label = ..count.., y = ((..count..)/sum(..count..))), 
                stat = "count", vjust = -0.5, size = 6) +
      labs(x = NULL,
           y = "Proportion") +
      expand_limits(y=c(0, 0.4))
    
    ggsave("figures/geq_taxes_2b_italy.pdf", width = 12, height = 8, dpi = 300)
    
    ggplot(drop_na(data_Italy, geq_ticket_1), aes(x=geq_ticket_1)) +
      facet_wrap(~factor(ticketExtra,
                         levels = c(5, 10, 30, 60, 115),
                         labels = c(5, 10, 30, 60, 115))) +
      geom_bar(aes(y = (..count..)/sum(..count..)), stat = "count") +
      scale_y_continuous(labels = scales::percent, breaks = seq(0, 1, by = 0.05)) +
      geom_text(aes(label = ..count.., y = ((..count..)/sum(..count..))), 
                stat = "count", vjust = -0.5, size = 6) +
      labs(x = NULL,
           y = "Proportion") +
      expand_limits(y=c(0, 0.25))
    
    ggsave("figures/geq_ticket_1_italy.pdf", width = 12, height = 8, dpi = 300)
    
    ggplot(drop_na(data_Italy, geq_ticket_2a), aes(x=geq_ticket_2a)) +
      facet_wrap(~factor(ticketExtra,
                         levels = c(5, 10, 30, 60, 115),
                         labels = c(10, 20, 60, 120, 230))) +
      geom_bar(aes(y = (..count..)/sum(..count..)), stat = "count") +
      scale_y_continuous(labels = scales::percent, breaks = seq(0, 1, by = 0.05)) +
      geom_text(aes(label = ..count.., y = ((..count..)/sum(..count..))), 
                stat = "count", vjust = -0.5, size = 6) +
      labs(x = NULL,
           y = "Proportion") +
      expand_limits(y=c(0, 0.55))
    
    ggsave("figures/geq_ticket_2a_italy.pdf", width = 12, height = 8, dpi = 300)
    
    ggplot(drop_na(data_Italy, geq_ticket_2b), aes(x=geq_ticket_2b)) +
      facet_wrap(~factor(ticketExtra,
                         levels = c(5, 10, 30, 60, 115),
                         labels = c(2.5, 5, 15, 30, 57.5))) +
      geom_bar(aes(y = (..count..)/sum(..count..)), stat = "count") +
      scale_y_continuous(labels = scales::percent, breaks = seq(0, 1, by = 0.05)) +
      geom_text(aes(label = ..count.., y = ((..count..)/sum(..count..))), 
                stat = "count", vjust = -0.5, size = 6) +
      labs(x = NULL,
           y = "Proportion") +
      expand_limits(y=c(0, 0.4))
    
    ggsave("figures/geq_ticket_2b_italy.pdf", width = 12, height = 8, dpi = 300)
    
    rm(data_Italy)
    
  }
  
  if (country == "Spain") {
    
    data_Spain <- data %>%
      filter(country == "Spain") 
    
    ggplot(drop_na(data_Spain, wtp_amount_1), aes(x=wtp_amount_1)) +
      facet_wrap(~factor(wtpVal,
                         levels = c(30, 55, 110, 220, 550),
                         labels = c(30, 55, 110, 220, 550))) +
      geom_bar(aes(y = (..count..)/sum(..count..)), stat = "count") +
      scale_y_continuous(labels = scales::percent, breaks = seq(0, 1, by = 0.05)) +
      geom_text(aes(label = ..count.., y = ((..count..)/sum(..count..))), 
                stat = "count", vjust = -0.5, size = 6) +
      labs(x = NULL,
           y = "Proportion") +
      expand_limits(y=c(0, 0.25))
    
    ggsave("figures/wtp_amount_1_spain.pdf", width = 12, height = 8, dpi = 300)
    
    ggplot(drop_na(data_Spain, wtp_amount_2a), aes(x=wtp_amount_2a)) +
      facet_wrap(~factor(wtpVal,
                         levels = c(30, 55, 110, 220, 550),
                         labels = c(60, 110, 220, 440, 1100))) +
      geom_bar(aes(y = (..count..)/sum(..count..)), stat = "count") +
      scale_y_continuous(labels = scales::percent, breaks = seq(0, 1, by = 0.05)) +
      geom_text(aes(label = ..count.., y = ((..count..)/sum(..count..))), 
                stat = "count", vjust = -0.5, size = 6) +
      labs(x = NULL,
           y = "Proportion") +
      expand_limits(y=c(0, 0.55))
    
    ggsave("figures/wtp_amount_2a_spain.pdf", width = 12, height = 8, dpi = 300)
    
    ggplot(drop_na(data_Spain, wtp_amount_2b), aes(x=wtp_amount_2b)) +
      facet_wrap(~factor(wtpVal,
                         levels = c(30, 55, 110, 220, 550),
                         labels = c(15, 27.5, 55, 110, 275))) +
      geom_bar(aes(y = (..count..)/sum(..count..)), stat = "count") +
      scale_y_continuous(labels = scales::percent, breaks = seq(0, 1, by = 0.05)) +
      geom_text(aes(label = ..count.., y = ((..count..)/sum(..count..))), 
                stat = "count", vjust = -0.5, size = 6) +
      labs(x = NULL,
           y = "Proportion") +
      expand_limits(y=c(0, 0.4))
    
    ggsave("figures/wtp_amount_2b_spain.pdf", width = 12, height = 8, dpi = 300)
    
    ggplot(drop_na(data_Spain, geq_taxes_1), aes(x=geq_taxes_1)) +
      facet_wrap(~factor(taxesExtra,
                         levels = c(5, 30, 55, 275, 550, 2755),
                         labels = c(5, 30, 55, 275, 550, 2755))) +
      geom_bar(aes(y = (..count..)/sum(..count..)), stat = "count") +
      scale_y_continuous(labels = scales::percent, breaks = seq(0, 1, by = 0.05)) +
      geom_text(aes(label = ..count.., y = ((..count..)/sum(..count..))), 
                stat = "count", vjust = -0.5, size = 6) +
      labs(x = NULL,
           y = "Proportion") +
      expand_limits(y=c(0, 0.25))
    
    ggsave("figures/geq_taxes_1_spain.pdf", width = 12, height = 8, dpi = 300)
    
    ggplot(drop_na(data_Spain, geq_taxes_2a), aes(x=geq_taxes_2a)) +
      facet_wrap(~factor(taxesExtra,
                         levels = c(5, 30, 55, 275, 550, 2755),
                         labels = c(10, 60, 110, 550, 1100, 5510))) +
      geom_bar(aes(y = (..count..)/sum(..count..)), stat = "count") +
      scale_y_continuous(labels = scales::percent, breaks = seq(0, 1, by = 0.05)) +
      geom_text(aes(label = ..count.., y = ((..count..)/sum(..count..))), 
                stat = "count", vjust = -0.5, size = 6) +
      labs(x = NULL,
           y = "Proportion") +
      expand_limits(y=c(0, 0.55))
    
    ggsave("figures/geq_taxes_2a_spain.pdf", width = 12, height = 8, dpi = 300)
    
    ggplot(drop_na(data_Spain, geq_taxes_2b), aes(x=geq_taxes_2b)) +
      facet_wrap(~factor(taxesExtra,
                         levels = c(5, 30, 55, 275, 550, 2755),
                         labels = c(2.5, 15, 27.5, 137.5, 275, 1377.5))) +
      geom_bar(aes(y = (..count..)/sum(..count..)), stat = "count") +
      scale_y_continuous(labels = scales::percent, breaks = seq(0, 1, by = 0.05)) +
      geom_text(aes(label = ..count.., y = ((..count..)/sum(..count..))), 
                stat = "count", vjust = -0.5, size = 6) +
      labs(x = NULL,
           y = "Proportion") +
      expand_limits(y=c(0, 0.4))
    
    ggsave("figures/geq_taxes_2b_spain.pdf", width = 12, height = 8, dpi = 300)
    
    ggplot(drop_na(data_Spain, geq_ticket_1), aes(x=geq_ticket_1)) +
      facet_wrap(~factor(ticketExtra,
                         levels = c(5, 10, 30, 55, 110),
                         labels = c(5, 10, 30, 55, 110))) +
      geom_bar(aes(y = (..count..)/sum(..count..)), stat = "count") +
      scale_y_continuous(labels = scales::percent, breaks = seq(0, 1, by = 0.05)) +
      geom_text(aes(label = ..count.., y = ((..count..)/sum(..count..))), 
                stat = "count", vjust = -0.5, size = 6) +
      labs(x = NULL,
           y = "Proportion") +
      expand_limits(y=c(0, 0.25))
    
    ggsave("figures/geq_ticket_1_spain.pdf", width = 12, height = 8, dpi = 300)
    
    ggplot(drop_na(data_Spain, geq_ticket_2a), aes(x=geq_ticket_2a)) +
      facet_wrap(~factor(ticketExtra,
                         levels = c(5, 10, 30, 55, 110),
                         labels = c(10, 20, 60, 110, 220))) +
      geom_bar(aes(y = (..count..)/sum(..count..)), stat = "count") +
      scale_y_continuous(labels = scales::percent, breaks = seq(0, 1, by = 0.05)) +
      geom_text(aes(label = ..count.., y = ((..count..)/sum(..count..))), 
                stat = "count", vjust = -0.5, size = 6) +
      labs(x = NULL,
           y = "Proportion") +
      expand_limits(y=c(0, 0.55))
    
    ggsave("figures/geq_ticket_2a_spain.pdf", width = 12, height = 8, dpi = 300)
    
    ggplot(drop_na(data_Spain, geq_ticket_2b), aes(x=geq_ticket_2b)) +
      facet_wrap(~factor(ticketExtra,
                         levels = c(5, 10, 30, 55, 110),
                         labels = c(2.5, 5, 15, 27.5, 55))) +
      geom_bar(aes(y = (..count..)/sum(..count..)), stat = "count") +
      scale_y_continuous(labels = scales::percent, breaks = seq(0, 1, by = 0.05)) +
      geom_text(aes(label = ..count.., y = ((..count..)/sum(..count..))), 
                stat = "count", vjust = -0.5, size = 6) +
      labs(x = NULL,
           y = "Proportion") +
      expand_limits(y=c(0, 0.4))
    
    ggsave("figures/geq_ticket_2b_spain.pdf", width = 12, height = 8, dpi = 300)
    
    rm(data_Spain)
    
  }
  
  if (country == "Uganda") {
    
    data_Uganda <- data %>%
      filter(country == "Uganda") 
    
    ggplot(drop_na(data_Uganda, wtp_amount_1), aes(x=wtp_amount_1)) +
      facet_wrap(~factor(wtpVal,
                         levels = c(6000, 13000, 26000, 51000, 128000),
                         labels = c(6000, 13000, 26000, 51000, 128000))) +
      geom_bar(aes(y = (..count..)/sum(..count..)), stat = "count") +
      scale_y_continuous(labels = scales::percent, breaks = seq(0, 1, by = 0.05)) +
      geom_text(aes(label = ..count.., y = ((..count..)/sum(..count..))), 
                stat = "count", vjust = -0.5, size = 6) +
      labs(x = NULL,
           y = "Proportion") +
      expand_limits(y=c(0, 0.25))
    
    ggsave("figures/wtp_amount_1_uganda.pdf", width = 12, height = 8, dpi = 300)
    
    ggplot(drop_na(data_Uganda, wtp_amount_2a), aes(x=wtp_amount_2a)) +
      facet_wrap(~factor(wtpVal,
                         levels = c(6000, 13000, 26000, 51000, 128000),
                         labels = c(12000, 26000, 52000, 102000, 256000))) +
      geom_bar(aes(y = (..count..)/sum(..count..)), stat = "count") +
      scale_y_continuous(labels = scales::percent, breaks = seq(0, 1, by = 0.05)) +
      geom_text(aes(label = ..count.., y = ((..count..)/sum(..count..))), 
                stat = "count", vjust = -0.5, size = 6) +
      labs(x = NULL,
           y = "Proportion") +
      expand_limits(y=c(0, 0.55))
    
    ggsave("figures/wtp_amount_2a_uganda.pdf", width = 12, height = 8, dpi = 300)
    
    ggplot(drop_na(data_Uganda, wtp_amount_2b), aes(x=wtp_amount_2b)) +
      facet_wrap(~factor(wtpVal,
                         levels = c(6000, 13000, 26000, 51000, 128000),
                         labels = c(3000, 6500, 13000, 25500, 64000))) +
      geom_bar(aes(y = (..count..)/sum(..count..)), stat = "count") +
      scale_y_continuous(labels = scales::percent, breaks = seq(0, 1, by = 0.05)) +
      geom_text(aes(label = ..count.., y = ((..count..)/sum(..count..))), 
                stat = "count", vjust = -0.5, size = 6) +
      labs(x = NULL,
           y = "Proportion") +
      expand_limits(y=c(0, 0.4))
    
    ggsave("figures/wtp_amount_2b_uganda.pdf", width = 12, height = 8, dpi = 300)
    
    ggplot(drop_na(data_Uganda, geq_taxes_1), aes(x=geq_taxes_1)) +
      facet_wrap(~factor(taxesExtra,
                         levels = c(1000, 6000, 13000, 64000, 128000, 642000),
                         labels = c(1000, 6000, 13000, 64000, 128000, 642000))) +
      geom_bar(aes(y = (..count..)/sum(..count..)), stat = "count") +
      scale_y_continuous(labels = scales::percent, breaks = seq(0, 1, by = 0.05)) +
      geom_text(aes(label = ..count.., y = ((..count..)/sum(..count..))), 
                stat = "count", vjust = -0.5, size = 6) +
      labs(x = NULL,
           y = "Proportion") +
      expand_limits(y=c(0, 0.25))
    
    ggsave("figures/geq_taxes_1_uganda.pdf", width = 12, height = 8, dpi = 300)
    
    ggplot(drop_na(data_Uganda, geq_taxes_2a), aes(x=geq_taxes_2a)) +
      facet_wrap(~factor(taxesExtra,
                         levels = c(1000, 6000, 13000, 64000, 128000, 642000),
                         labels = c(2000, 12000, 26000, 128000, 256000, 1284000))) +
      geom_bar(aes(y = (..count..)/sum(..count..)), stat = "count") +
      scale_y_continuous(labels = scales::percent, breaks = seq(0, 1, by = 0.05)) +
      geom_text(aes(label = ..count.., y = ((..count..)/sum(..count..))), 
                stat = "count", vjust = -0.5, size = 6) +
      labs(x = NULL,
           y = "Proportion") +
      expand_limits(y=c(0, 0.55))
    
    ggsave("figures/geq_taxes_2a_uganda.pdf", width = 12, height = 8, dpi = 300)
    
    ggplot(drop_na(data_Uganda, geq_taxes_2b), aes(x=geq_taxes_2b)) +
      facet_wrap(~factor(taxesExtra,
                         levels = c(1000, 6000, 13000, 64000, 128000, 642000),
                         labels = c(500, 3000, 6500, 32000, 64000, 321000))) +
      geom_bar(aes(y = (..count..)/sum(..count..)), stat = "count") +
      scale_y_continuous(labels = scales::percent, breaks = seq(0, 1, by = 0.05)) +
      geom_text(aes(label = ..count.., y = ((..count..)/sum(..count..))), 
                stat = "count", vjust = -0.5, size = 6) +
      labs(x = NULL,
           y = "Proportion") +
      expand_limits(y=c(0, 0.4))
    
    ggsave("figures/geq_taxes_2b_uganda.pdf", width = 12, height = 8, dpi = 300)
    
    ggplot(drop_na(data_Uganda, geq_ticket_1), aes(x=geq_ticket_1)) +
      facet_wrap(~factor(ticketExtra,
                         levels = c(1000, 3000, 6000, 13000, 26000),
                         labels = c(1000, 3000, 6000, 13000, 26000))) +
      geom_bar(aes(y = (..count..)/sum(..count..)), stat = "count") +
      scale_y_continuous(labels = scales::percent, breaks = seq(0, 1, by = 0.05)) +
      geom_text(aes(label = ..count.., y = ((..count..)/sum(..count..))), 
                stat = "count", vjust = -0.5, size = 6) +
      labs(x = NULL,
           y = "Proportion") +
      expand_limits(y=c(0, 0.25))
    
    ggsave("figures/geq_ticket_1_uganda.pdf", width = 12, height = 8, dpi = 300)
    
    ggplot(drop_na(data_Uganda, geq_ticket_2a), aes(x=geq_ticket_2a)) +
      facet_wrap(~factor(ticketExtra,
                         levels = c(1000, 3000, 6000, 13000, 26000),
                         labels = c(2000, 6000, 12000, 26000, 52000))) +
      geom_bar(aes(y = (..count..)/sum(..count..)), stat = "count") +
      scale_y_continuous(labels = scales::percent, breaks = seq(0, 1, by = 0.05)) +
      geom_text(aes(label = ..count.., y = ((..count..)/sum(..count..))), 
                stat = "count", vjust = -0.5, size = 6) +
      labs(x = NULL,
           y = "Proportion") +
      expand_limits(y=c(0, 0.55))
    
    ggsave("figures/geq_ticket_2a_uganda.pdf", width = 12, height = 8, dpi = 300)
    
    ggplot(drop_na(data_Uganda, geq_ticket_2b), aes(x=geq_ticket_2b)) +
      facet_wrap(~factor(ticketExtra,
                         levels = c(1000, 3000, 6000, 13000, 26000),
                         labels = c(500, 1500, 3000, 6500, 13000))) +
      geom_bar(aes(y = (..count..)/sum(..count..)), stat = "count") +
      scale_y_continuous(labels = scales::percent, breaks = seq(0, 1, by = 0.05)) +
      geom_text(aes(label = ..count.., y = ((..count..)/sum(..count..))), 
                stat = "count", vjust = -0.5, size = 6) +
      labs(x = NULL,
           y = "Proportion") +
      expand_limits(y=c(0, 0.4))
    
    ggsave("figures/geq_ticket_2b_uganda.pdf", width = 12, height = 8, dpi = 300)
    
    rm(data_Uganda)
    
  }
  
  if (country == "US") {
    
    data_UnitedStates <- data %>%
      filter(country == "US") 
    
    ggplot(drop_na(data_UnitedStates, wtp_amount_1), aes(x=wtp_amount_1)) +
      facet_wrap(~factor(wtpVal,
                         levels = c(50, 100, 200, 400, 1000),
                         labels = c(50, 100, 200, 400, 1000))) +
      geom_bar(aes(y = (..count..)/sum(..count..)), stat = "count") +
      scale_y_continuous(labels = scales::percent, breaks = seq(0, 1, by = 0.05)) +
      geom_text(aes(label = ..count.., y = ((..count..)/sum(..count..))), 
                stat = "count", vjust = -0.5, size = 6) +
      labs(x = NULL,
           y = "Proportion") +
      expand_limits(y=c(0, 0.25))
    
    ggsave("figures/wtp_amount_1_unitedstates.pdf", width = 12, height = 8, dpi = 300)
    
    ggplot(drop_na(data_UnitedStates, wtp_amount_2a), aes(x=wtp_amount_2a)) +
      facet_wrap(~factor(wtpVal,
                         levels = c(50, 100, 200, 400, 1000),
                         labels = c(100, 200, 400, 800, 2000))) +
      geom_bar(aes(y = (..count..)/sum(..count..)), stat = "count") +
      scale_y_continuous(labels = scales::percent, breaks = seq(0, 1, by = 0.05)) +
      geom_text(aes(label = ..count.., y = ((..count..)/sum(..count..))), 
                stat = "count", vjust = -0.5, size = 6) +
      labs(x = NULL,
           y = "Proportion") +
      expand_limits(y=c(0, 0.55))
    
    ggsave("figures/wtp_amount_2a_unitedstates.pdf", width = 12, height = 8, dpi = 300)
    
    ggplot(drop_na(data_UnitedStates, wtp_amount_2b), aes(x=wtp_amount_2b)) +
      facet_wrap(~factor(wtpVal,
                         levels = c(50, 100, 200, 400, 1000),
                         labels = c(25, 50, 100, 200, 500))) +
      geom_bar(aes(y = (..count..)/sum(..count..)), stat = "count") +
      scale_y_continuous(labels = scales::percent, breaks = seq(0, 1, by = 0.05)) +
      geom_text(aes(label = ..count.., y = ((..count..)/sum(..count..))), 
                stat = "count", vjust = -0.5, size = 6) +
      labs(x = NULL,
           y = "Proportion") +
      expand_limits(y=c(0, 0.4))
    
    ggsave("figures/wtp_amount_2b_unitedstates.pdf", width = 12, height = 8, dpi = 300)
    
    ggplot(drop_na(data_UnitedStates, geq_taxes_1), aes(x=geq_taxes_1)) +
      facet_wrap(~factor(taxesExtra,
                         levels = c(10, 50, 100, 500, 1000, 5000),
                         labels = c(10, 50, 100, 500, 1000, 5000))) +
      geom_bar(aes(y = (..count..)/sum(..count..)), stat = "count") +
      scale_y_continuous(labels = scales::percent, breaks = seq(0, 1, by = 0.05)) +
      geom_text(aes(label = ..count.., y = ((..count..)/sum(..count..))), 
                stat = "count", vjust = -0.5, size = 6) +
      labs(x = NULL,
           y = "Proportion") +
      expand_limits(y=c(0, 0.25))
    
    ggsave("figures/geq_taxes_1_unitedstates.pdf", width = 12, height = 8, dpi = 300)
    
    ggplot(drop_na(data_UnitedStates, geq_taxes_2a), aes(x=geq_taxes_2a)) +
      facet_wrap(~factor(taxesExtra,
                         levels = c(10, 50, 100, 500, 1000, 5000),
                         labels = c(20, 100, 200, 1000, 2000, 10000))) +
      geom_bar(aes(y = (..count..)/sum(..count..)), stat = "count") +
      scale_y_continuous(labels = scales::percent, breaks = seq(0, 1, by = 0.05)) +
      geom_text(aes(label = ..count.., y = ((..count..)/sum(..count..))), 
                stat = "count", vjust = -0.5, size = 6) +
      labs(x = NULL,
           y = "Proportion") +
      expand_limits(y=c(0, 0.55))
    
    ggsave("figures/geq_taxes_2a_unitedstates.pdf", width = 12, height = 8, dpi = 300)
    
    ggplot(drop_na(data_UnitedStates, geq_taxes_2b), aes(x=geq_taxes_2b)) +
      facet_wrap(~factor(taxesExtra,
                         levels = c(10, 50, 100, 500, 1000, 5000),
                         labels = c(5, 25, 50, 250, 500, 2500))) +
      geom_bar(aes(y = (..count..)/sum(..count..)), stat = "count") +
      scale_y_continuous(labels = scales::percent, breaks = seq(0, 1, by = 0.05)) +
      geom_text(aes(label = ..count.., y = ((..count..)/sum(..count..))), 
                stat = "count", vjust = -0.5, size = 6) +
      labs(x = NULL,
           y = "Proportion") +
      expand_limits(y=c(0, 0.4))
    
    ggsave("figures/geq_taxes_2b_unitedstates.pdf", width = 12, height = 8, dpi = 300)
    
    ggplot(drop_na(data_UnitedStates, geq_ticket_1), aes(x=geq_ticket_1)) +
      facet_wrap(~factor(ticketExtra,
                         levels = c(10, 20, 50, 100, 200),
                         labels = c(10, 20, 50, 100, 200))) +
      geom_bar(aes(y = (..count..)/sum(..count..)), stat = "count") +
      scale_y_continuous(labels = scales::percent, breaks = seq(0, 1, by = 0.05)) +
      geom_text(aes(label = ..count.., y = ((..count..)/sum(..count..))), 
                stat = "count", vjust = -0.5, size = 6) +
      labs(x = NULL,
           y = "Proportion") +
      expand_limits(y=c(0, 0.25))
    
    ggsave("figures/geq_ticket_1_unitedstates.pdf", width = 12, height = 8, dpi = 300)
    
    ggplot(drop_na(data_UnitedStates, geq_ticket_2a), aes(x=geq_ticket_2a)) +
      facet_wrap(~factor(ticketExtra,
                         levels = c(10, 20, 50, 100, 200),
                         labels = c(20, 40, 100, 200, 400))) +
      geom_bar(aes(y = (..count..)/sum(..count..)), stat = "count") +
      scale_y_continuous(labels = scales::percent, breaks = seq(0, 1, by = 0.05)) +
      geom_text(aes(label = ..count.., y = ((..count..)/sum(..count..))), 
                stat = "count", vjust = -0.5, size = 6) +
      labs(x = NULL,
           y = "Proportion") +
      expand_limits(y=c(0, 0.55))
    
    ggsave("figures/geq_ticket_2a_unitedstates.pdf", width = 12, height = 8, dpi = 300)
    
    ggplot(drop_na(data_UnitedStates, geq_ticket_2b), aes(x=geq_ticket_2b)) +
      facet_wrap(~factor(ticketExtra,
                         levels = c(10, 20, 50, 100, 200),
                         labels = c(5, 10, 25, 50, 100))) +
      geom_bar(aes(y = (..count..)/sum(..count..)), stat = "count") +
      scale_y_continuous(labels = scales::percent, breaks = seq(0, 1, by = 0.05)) +
      geom_text(aes(label = ..count.., y = ((..count..)/sum(..count..))), 
                stat = "count", vjust = -0.5, size = 6) +
      labs(x = NULL,
           y = "Proportion") +
      expand_limits(y=c(0, 0.4))
    
    ggsave("figures/geq_ticket_2b_unitedstates.pdf", width = 12, height = 8, dpi = 300)
    
    rm(data_UnitedStates)
    
  }
  
}

recode_for_wtp <- function(data) {
  
  data <- data %>%
    select(id, country, age, gender, EDUCATION_LEVEL,
           wtp_access, wtp_private, wtp_amount_1, wtp_amount_2a, wtp_amount_2b,
           geq_taxes_0, geq_taxes_1, geq_taxes_2a, geq_taxes_2b,
           geq_ticket_0, geq_ticket_1, geq_ticket_2a, geq_ticket_2b,
           wtpVal, taxesExtra, ticketExtra)
  
  data <- data %>%
    mutate_at(vars(wtpVal, taxesExtra, ticketExtra),
              as.numeric)
  
  data <- data %>%
    mutate(wtpVal_PPP = if_else(wtpVal %in% c(50, 44000, 15000, 85, 30, 30, 30, 30, 6000, 50, 1650), 50,
                                if_else(wtpVal %in% c(135, 105, 87000, 30000, 175, 60, 795, 60, 13000, 100, 3300), 100,
                                        if_else(wtpVal %in% c(270, 210, 175000, 60000, 345, 130, 1595, 110, 26000, 200, 6600), 200,
                                                if_else(wtpVal %in% c(460, 535, 415, 349000, 119000, 690, 260, 3190, 220, 51000, 13200), 400,
                                                        if_else(wtpVal %in% c(1145, 1340, 1040, 874000, 299000, 1730, 645, 7975, 575, 550, 128000, 1000, 33000), 1000,
                                                                if_else(wtpVal == 55 & country == "Australia", 50,
                                                                        if_else(wtpVal == 55 & country == "Spain", 100,
                                                                                if_else(wtpVal == 65 & country == "Brazil", 50,
                                                                                        if_else(wtpVal == 65 & country == "France", 100,
                                                                                                if_else(wtpVal == 115 & country == "Australia", 100,
                                                                                                        if_else(wtpVal == 115 & country %in% c("UK", "Italy"), 200,
                                                                                                                if_else(wtpVal == 230 & country == "Australia", 200,
                                                                                                                        if_else(wtpVal == 230 & country %in% c("UK", "Italy"), 400,
                                                                                                                                if_else(wtpVal == 400 & country == "India", 50,
                                                                                                                                        if_else(wtpVal == 400 & country == "US", 400, NA_real_))))))))))))))),
           taxesExtra_PPP = if_else(taxesExtra %in% c(10, 15, 10, 9000, 3000, 5, 80, 330), 10,
                                    if_else(taxesExtra %in% c(30, 50, 65, 85, 400, 6000, 15000, 44000, 1650), 50,
                                            if_else(taxesExtra %in% c(60, 65, 100, 105, 115, 135, 175, 795, 13000, 30000, 87000, 3300), 100,
                                                    if_else(taxesExtra %in% c(275, 290, 325, 500, 520, 670, 865, 3985, 64000, 149000, 437000, 16500), 500,
                                                            if_else(taxesExtra %in% c(550, 645, 1040, 1145, 1340, 1730, 7975, 128000, 299000, 874000, 33000), 1000,
                                                                    if_else(taxesExtra %in% c(2755, 2880, 2885, 3225, 5000, 5205, 5730, 6690, 8650, 39865, 642000, 1493000, 4368000, 165000), 5000,
                                                                            if_else(taxesExtra == 55 & country == "Australia", 50,
                                                                                    if_else(taxesExtra == 55 & country == "Spain", 100,
                                                                                            if_else(taxesExtra == 575 & country == "Australia", 500,
                                                                                                    if_else(taxesExtra == 575 & country %in% c("UK", "Italy"), 1000,
                                                                                                            if_else(taxesExtra == 1000 & country == "Uganda", 10,
                                                                                                                    if_else(taxesExtra == 1000 & country == "US", 1000, NA_real_)))))))))))),
           ticketExtra_PPP = if_else(ticketExtra %in% c(5, 80, 1000, 9000), 10,
                                    if_else(ticketExtra %in% c(20, 25, 35, 160, 17000), 20,
                                            if_else(ticketExtra %in% c(30, 50, 85, 400, 15000, 44000), 50,
                                                    if_else(ticketExtra %in% c(60, 100, 105, 135, 175, 795, 13000, 30000, 87000), 100,
                                                            if_else(ticketExtra %in% c(110, 130, 200, 210, 230, 270, 345, 1595, 26000, 60000, 175000), 200,
                                                                    if_else(ticketExtra == 10 & country %in% c("Australia", "Canada", "US"), 10,
                                                                            if_else(ticketExtra == 10 & country %in% c("UK", "Italy", "Spain"), 20,
                                                                                    if_else(ticketExtra == 15 & country %in% c("Brazil", "China"), 10,
                                                                                            if_else(ticketExtra == 15 & country == "France", 20,
                                                                                                    if_else(ticketExtra == 55 & country == "Australia", 50,
                                                                                                            if_else(ticketExtra == 55 & country == "Spain", 100,
                                                                                                                    if_else(ticketExtra == 65 & country == "Brazil", 50,
                                                                                                                            if_else(ticketExtra == 65 & country == "France", 100,
                                                                                                                                    if_else(ticketExtra == 115 & country == "Australia", 100,
                                                                                                                                            if_else(ticketExtra == 115 & country %in% c("UK", "Italy"), 200,
                                                                                                                                                    if_else(ticketExtra == 3000 & country == "Chile", 10,
                                                                                                                                                            if_else(ticketExtra == 3000 & country == "Uganda", 20,
                                                                                                                                                                    if_else(ticketExtra == 6000 & country == "Chile", 20,
                                                                                                                                                                            if_else(ticketExtra == 6000 & country == "Uganda", 50, NA_real_))))))))))))))))))))
  
    data_Australia <- data %>%
    filter(country == "Australia") %>%
    mutate_at(vars(wtp_amount_1, wtp_amount_2a, wtp_amount_2b,
                   geq_taxes_0, geq_taxes_1, geq_taxes_2a, geq_taxes_2b,
                   geq_ticket_0, geq_ticket_1, geq_ticket_2a, geq_ticket_2b),
              list(~factor(., levels = c("Yes",
                                         "No",
                                         "Do not know",
                                         "Prefer not to say"), 
                           labels = c("Yes",
                                      "No",
                                      "DK",
                                      "RS")))) 
  
  data_Brazil <- data %>%
    filter(country == "Brazil") %>%
    mutate_at(vars(wtp_amount_1, wtp_amount_2a, wtp_amount_2b,
                   geq_taxes_0, geq_taxes_1, geq_taxes_2a, geq_taxes_2b,
                   geq_ticket_0, geq_ticket_1, geq_ticket_2a, geq_ticket_2b),
              list(~factor(., levels = c("Sim",
                                         "Não",
                                         "Não sei",
                                         "Prefiro não informar"), 
                           labels = c("Yes",
                                      "No",
                                      "DK",
                                      "RS")))) 
  
  data_Canada <- data %>%
    filter(country == "Canada") %>%
    mutate_at(vars(wtp_amount_1, wtp_amount_2a, wtp_amount_2b,
                   geq_taxes_0, geq_taxes_1, geq_taxes_2a, geq_taxes_2b,
                   geq_ticket_0, geq_ticket_1, geq_ticket_2a, geq_ticket_2b),
              list(~factor(., levels = c("Yes",
                                         "Oui",
                                         "No",
                                         "Non",
                                         "Do not know",
                                         "Ne sait pas",
                                         "Prefer not to say",
                                         "Préferè ne pas dire"), 
                           labels = c("Yes",
                                      "Yes",
                                      "No",
                                      "No",
                                      "DK",
                                      "DK",
                                      "RS",
                                      "RS")))) 
  
  data_Colombia <- data %>%
    filter(country == "Colombia") %>%
    mutate_at(vars(wtp_amount_1, wtp_amount_2a, wtp_amount_2b,
                   geq_taxes_0, geq_taxes_1, geq_taxes_2a, geq_taxes_2b,
                   geq_ticket_0, geq_ticket_1, geq_ticket_2a, geq_ticket_2b),
              list(~factor(., levels = c("Sí",
                                         "No",
                                         "No lo sé",
                                         "Prefiero no contestar"), 
                           labels = c("Yes",
                                      "No",
                                      "DK",
                                      "RS")))) 
  
  data_Chile <- data %>%
    filter(country == "Chile") %>%
    mutate_at(vars(wtp_amount_1, wtp_amount_2a, wtp_amount_2b,
                   geq_taxes_0, geq_taxes_1, geq_taxes_2a, geq_taxes_2b,
                   geq_ticket_0, geq_ticket_1, geq_ticket_2a, geq_ticket_2b),
              list(~factor(., levels = c("Sí",
                                         "No",
                                         "No lo sé",
                                         "Prefiero no contestar"), 
                           labels = c("Yes",
                                      "No",
                                      "DK",
                                      "RS")))) 
  
  data_China <- data %>%
    filter(country == "China") %>%
    mutate_at(vars(wtp_amount_1, wtp_amount_2a, wtp_amount_2b,
                   geq_taxes_0, geq_taxes_1, geq_taxes_2a, geq_taxes_2b,
                   geq_ticket_0, geq_ticket_1, geq_ticket_2a, geq_ticket_2b),
              list(~factor(., levels = c("愿意",
                                         "不愿意",
                                         "不知道",
                                         "不想说"), 
                           labels = c("Yes",
                                      "No",
                                      "DK",
                                      "RS")))) 
  
  data_France <- data %>%
    filter(country == "France") %>%
    mutate_at(vars(wtp_amount_1, wtp_amount_2a, wtp_amount_2b,
                   geq_taxes_0, geq_taxes_1, geq_taxes_2a, geq_taxes_2b,
                   geq_ticket_0, geq_ticket_1, geq_ticket_2a, geq_ticket_2b),
              list(~factor(., levels = c("Oui",
                                         "Non",
                                         "Ne sait pas",
                                         "Préfère ne pas dire"), 
                           labels = c("Yes",
                                      "No",
                                      "DK",
                                      "RS")))) 
  
  data_India <- data %>%
    filter(country == "India") %>%
    mutate_at(vars(wtp_amount_1, wtp_amount_2a, wtp_amount_2b,
                   geq_taxes_0, geq_taxes_1, geq_taxes_2a, geq_taxes_2b,
                   geq_ticket_0, geq_ticket_1, geq_ticket_2a, geq_ticket_2b),
              list(~factor(., levels = c("Yes",
                                         "No",
                                         "Do not know",
                                         "Prefer not to say"), 
                           labels = c("Yes",
                                      "No",
                                      "DK",
                                      "RS")))) 
  
  data_Italy <- data %>%
    filter(country == "Italy") %>%
    mutate_at(vars(wtp_amount_1, wtp_amount_2a, wtp_amount_2b,
                   geq_taxes_0, geq_taxes_1, geq_taxes_2a, geq_taxes_2b,
                   geq_ticket_0, geq_ticket_1, geq_ticket_2a, geq_ticket_2b),
              list(~factor(., levels = c("Sì",
                                         "No",
                                         "Non saprei",
                                         "Preferisco non rispondere"), 
                           labels = c("Yes",
                                      "No",
                                      "DK",
                                      "RS")))) 
  
  data_Russia <- data %>%
    filter(country == "Russia") %>%
    mutate_at(vars(wtp_amount_1, wtp_amount_2a, wtp_amount_2b,
                   geq_taxes_0, geq_taxes_1, geq_taxes_2a, geq_taxes_2b,
                   geq_ticket_0, geq_ticket_1, geq_ticket_2a, geq_ticket_2b),
              list(~factor(., levels = c("Да",
                                         "Нет",
                                         "Не знаю",
                                         "Предпочту не отвечать"), 
                           labels = c("Yes",
                                      "No",
                                      "DK",
                                      "RS")))) 
  
  data_Spain <- data %>%
    filter(country == "Spain") %>%
    mutate_at(vars(wtp_amount_1, wtp_amount_2a, wtp_amount_2b,
                   geq_taxes_0, geq_taxes_1, geq_taxes_2a, geq_taxes_2b,
                   geq_ticket_0, geq_ticket_1, geq_ticket_2a, geq_ticket_2b),
              list(~factor(., levels = c("Sí",
                                         "No",
                                         "No lo sé",
                                         "Prefiero no contestar"), 
                           labels = c("Yes",
                                      "No",
                                      "DK",
                                      "RS")))) 
  
  data_Uganda <- data %>%
    filter(country == "Uganda") %>%
    mutate_at(vars(wtp_amount_1, wtp_amount_2a, wtp_amount_2b,
                   geq_taxes_0, geq_taxes_1, geq_taxes_2a, geq_taxes_2b,
                   geq_ticket_0, geq_ticket_1, geq_ticket_2a, geq_ticket_2b),
              list(~factor(., levels = c("Yes",
                                         "No",
                                         "Do not know",
                                         "Prefer not to say"), 
                           labels = c("Yes",
                                      "No",
                                      "DK",
                                      "RS")))) 
  
  data_UK <- data %>%
    filter(country == "UK") %>%
    mutate_at(vars(wtp_amount_1, wtp_amount_2a, wtp_amount_2b,
                   geq_taxes_0, geq_taxes_1, geq_taxes_2a, geq_taxes_2b,
                   geq_ticket_0, geq_ticket_1, geq_ticket_2a, geq_ticket_2b),
              list(~factor(., levels = c("Yes",
                                         "No",
                                         "Do not know",
                                         "Prefer not to say"), 
                           labels = c("Yes",
                                      "No",
                                      "DK",
                                      "RS"))))
  
  data_UnitedStates <- data %>%
    filter(country == "US") %>%
    mutate_at(vars(wtp_amount_1, wtp_amount_2a, wtp_amount_2b,
                   geq_taxes_0, geq_taxes_1, geq_taxes_2a, geq_taxes_2b,
                   geq_ticket_0, geq_ticket_1, geq_ticket_2a, geq_ticket_2b),
              list(~factor(., levels = c("Yes",
                                         "No",
                                         "Do not know",
                                         "Prefer not to say"), 
                           labels = c("Yes",
                                      "No",
                                      "DK",
                                      "RS")))) 
  
  data <- rbind(data_UK, data_Australia, data_Brazil, data_Canada, data_Colombia, 
                data_Chile, data_China, data_France, data_India, data_Italy,
                data_Spain, data_Uganda, data_UnitedStates)
  
  data$wtp_access[data$country == "China"] <- substr(data$wtp_access[data$country == "China"], 1, 3)
  
  wtp_access_text1 <- c("Vacinas disponibilizadas somente pelo governo a um preço baixo ou gratuitamente?",
                        "Vacunas disponibles solo a través del gobierno a bajo costo o sin costo alguno?",
                        "仅由政",
                        "Vacunas disponibles solo a través del gobierno a bajo costo o sin costo alguno?",
                        "Vaccins uniquement mis à disposition par le gouvernement à faible coût ou gratuitement ?",
                        "Vaccini forniti solo dal governo a basso costo o a costo zero",
                        "Vaccines only made available by government at low or no cost?",
                        "Vaccines only made available by government at low or no cost",
                        "Вакцины, предоставляемые только государством по льготной цене или бесплатно?")
  
  wtp_access_text2 <- c("Vacinas disponibilizadas somente para aquisição particular?",
                        "Vacunas disponibles solo a través de distribuidores privados?",
                        "​仅供",
                        "Vacunas disponibles solo a través de distribuidores privados?",
                        "Les vaccins ne sont disponibles que par achat privé ?",
                        "Vaccini solo disponibili attraverso acquisto privato",
                        "Vaccines are only available for private purchase?",
                        "Vaccines are only available for private purchase",
                        "Вакцины, только приобретаются индивидуально за деньги?")
  
  wtp_access_text3 <- c("Vacinas disponibilizadas pelo governo, mas que os cidadãos possam pagar por conta própria para obterem acesso?",
                        "Vacunas disponibles a través del gobierno, pero que ciudadanos puedan pagar de forma privada para obtener acceso?",
                        "​由政",
                        "Vacunas disponibles a través del gobierno, pero que ciudadanos puedan pagar de forma privada para obtener acceso?",
                        "Vaccins mis à disposition par le gouvernement mais les citoyens peuvent y avoir accès rapidement par achat privé ?",
                        "Vaccini forniti dal governo, ma i cittadini possono pagare privatamente per ottenere accesso anticipato",
                        "Vaccines made available by government but citizens can pay privately to gain access?",
                        "Vaccines made available by government but citizens can pay privately to gain access",
                        "Вакцины, предоставляемые государством с возможностью платной вакцинации за деньги?")
  
  wtp_access_text4 <- c("Não sei",
                        "No lo sé",
                        "不知道",
                        "No lo sé",
                        "Ne sait pas",
                        "Non saprei",
                        "Do not know",
                        "Не знаю")
  
  
  
  data$wtp_access <- case_when(data$wtp_access %in% wtp_access_text1
                               ~ "Vaccines only made available by government at low or no cost",
                               data$wtp_access %in% wtp_access_text2
                               ~ "Vaccines are only available for private purchase",
                               data$wtp_access %in% wtp_access_text3
                               ~ "Vaccines made available by government but citizens can pay privately to gain access",
                               data$wtp_access %in% wtp_access_text4 ~ "Do not know",
                               data$wtp_access == "Preferisco non rispondere" ~ "Prefer not to say")
  
  wtp_private_text1 <- c("Sim",
                         "Sí",
                         "会",
                         "Sí",
                         "Oui",
                         "Sì",
                         "Yes",
                         "Да")
  
  wtp_private_text2 <- c("Não",
                         "No",
                         "不会",
                         "Non",
                         "Нет")
  
  wtp_private_text3 <- c("Não sei",
                         "No lo sé",
                         "不知道",
                         "No lo sé",
                         "Ne sait pas",
                         "Non saprei",
                         "Do not know",
                         "Не знаю")
  
  data$wtp_private <- case_when(data$wtp_private %in% wtp_private_text1 ~ "Yes",
                               data$wtp_private %in% wtp_private_text2  ~ "No",
                               data$wtp_private %in% wtp_private_text3  ~ "Do not know",
                               data$wtp_private %in% c("Preferisco non rispondere",
                                                       "Prefer not to say") ~ "Prefer not to say")
  
  data <- data %>% 
    mutate(WTP_R1 = if_else(wtp_amount_1 == "Yes", 1, 
                            if_else(wtp_amount_1 == "No", 0, NA_real_)),
           WTP_R2_aux = coalesce(wtp_amount_2a, wtp_amount_2b),
           geq_taxes_R1 = if_else(geq_taxes_1 == "Yes", 1, 
                                  if_else(geq_taxes_1 == "No", 0, NA_real_)),
           geq_taxes_R2_aux = coalesce(geq_taxes_2a, geq_taxes_2b),
           geq_ticket_R1 = if_else(geq_ticket_1 == "Yes", 1, 
                                   if_else(geq_ticket_1 == "No", 0, NA_real_)),
           geq_ticket_R2_aux = coalesce(geq_ticket_2a, geq_ticket_2b)) %>%
    mutate(WTP_R2 = if_else(WTP_R2_aux == "Yes", 1, 
                            if_else(WTP_R2_aux == "No", 0, NA_real_)),
           geq_taxes_R2 = if_else(geq_taxes_R2_aux == "Yes", 1, 
                                  if_else(geq_taxes_R2_aux == "No", 0, NA_real_)),
           geq_ticket_R2 = if_else(geq_ticket_R2_aux == "Yes", 1, 
                                   if_else(geq_ticket_R2_aux == "No", 0, NA_real_))) %>%
    mutate(WTP_BD1 = wtpVal,
           WTP_BD2 = if_else(WTP_R1 == 1, wtpVal*2,
                             if_else(WTP_R1 == 0, wtpVal/2, NA_real_)),
           geq_taxes_BD1 = taxesExtra,
           geq_taxes_BD2 = if_else(geq_taxes_R1 == 1, taxesExtra*2,
                                   if_else(geq_taxes_R1 == 0, taxesExtra/2, NA_real_)),
           geq_ticket_BD1 = ticketExtra,
           geq_ticket_BD2 = if_else(geq_ticket_R1 == 1, ticketExtra*2,
                                    if_else(geq_ticket_R1 == 0, ticketExtra/2, NA_real_)),
           WTP_BD1_PPP = wtpVal_PPP,
           WTP_BD2_PPP = if_else(WTP_R1 == 1, wtpVal_PPP*2,
                             if_else(WTP_R1 == 0, wtpVal_PPP/2, NA_real_)),
           geq_taxes_BD1_PPP = taxesExtra_PPP,
           geq_taxes_BD2_PPP = if_else(geq_taxes_R1 == 1, taxesExtra_PPP*2,
                                   if_else(geq_taxes_R1 == 0, taxesExtra_PPP/2, NA_real_)),
           geq_ticket_BD1_PPP = ticketExtra_PPP,
           geq_ticket_BD2_PPP = if_else(geq_ticket_R1 == 1, ticketExtra_PPP*2,
                                    if_else(geq_ticket_R1 == 0, ticketExtra_PPP/2, NA_real_)))
  
  return(data)
}

wtp_regression <- function(data, country_name, PPP) {
  require(DCchoice)
  
  WTP <- list()
  
  if (PPP == TRUE) {
    if(missing(country_name)) {
      WTPdb <- summary(dbchoice(WTP_R1 + WTP_R2 ~ 1 | log(WTP_BD1_PPP) + log(WTP_BD2_PPP),
                                dist = "logistic", data = data))
      
      WTP[["vaccine"]] <- tibble(N = nrow(data), 
                                 "N(WTP>0)" = nrow(data[data$wtp_private == "Yes",]), 
                                 median = exp(WTPdb$medianWTP), 
                                 mean = exp(WTPdb$meanWTP))
      
      taxesdb <- summary(dbchoice(geq_taxes_R1 + geq_taxes_R2 ~ 1 | log(geq_taxes_BD1_PPP) + log(geq_taxes_BD2_PPP), 
                                  dist = "logistic", data = data))
      
      WTP[["taxes"]] <- tibble(N = nrow(data), 
                               "N(WTP>0)" = nrow(data[data$geq_taxes_0 == "Yes",]), 
                               median = exp(taxesdb$medianWTP), 
                               mean = exp(taxesdb$meanWTP))
      
      ticketdb <- summary(dbchoice(geq_ticket_R1 + geq_ticket_R2 ~ 1 | log(geq_ticket_BD1_PPP) + log(geq_ticket_BD2_PPP), 
                                   dist = "logistic", data = data))
      
      WTP[["ticket"]] <- tibble(N = nrow(data), 
                                "N(WTP>0)" = nrow(data[data$geq_ticket_0 == "Yes",]), 
                                median = exp(ticketdb$medianWTP), 
                                mean = exp(ticketdb$meanWTP))
      
      WTP$country <- "global"
    }
    else {
      data <- data %>%
        filter(country == country_name)
      
      WTPdb <- summary(dbchoice(WTP_R1 + WTP_R2 ~ 1 | log(WTP_BD1_PPP) + log(WTP_BD2_PPP),
                                dist = "logistic", data = data))
      
      WTP[["vaccine"]] <- tibble(N = nrow(data), 
                                 "N(WTP>0)" = nrow(data[data$wtp_private == "Yes",]), 
                                 median = exp(WTPdb$medianWTP), 
                                 mean = exp(WTPdb$meanWTP))
      
      taxesdb <- summary(dbchoice(geq_taxes_R1 + geq_taxes_R2 ~ 1 | log(geq_taxes_BD1_PPP) + log(geq_taxes_BD2_PPP), 
                                  dist = "logistic", data = data))
      
      WTP[["taxes"]] <- tibble(N = nrow(data), 
                               "N(WTP>0)" = nrow(data[data$geq_taxes_0 == "Yes",]), 
                               median = exp(taxesdb$medianWTP), 
                               mean = exp(taxesdb$meanWTP))
      
      ticketdb <- summary(dbchoice(geq_ticket_R1 + geq_ticket_R2 ~ 1 | log(geq_ticket_BD1_PPP) + log(geq_ticket_BD2_PPP), 
                                   dist = "logistic", data = data))
      
      WTP[["ticket"]] <- tibble(N = nrow(data), 
                                "N(WTP>0)" = nrow(data[data$geq_ticket_0 == "Yes",]), 
                                median = exp(ticketdb$medianWTP), 
                                mean = exp(ticketdb$meanWTP))
      
      WTP$country <- country_name
      
    }
  }
  else{
    if(missing(country_name)) {
      WTPdb <- summary(dbchoice(WTP_R1 + WTP_R2 ~ 1 | log(WTP_BD1) + log(WTP_BD2),
                                dist = "logistic", data = data))
      
      WTP[["vaccine"]] <- tibble(N = nrow(data), 
                                 "N(WTP>0)" = nrow(data[data$wtp_private == "Yes",]), 
                                 median = exp(WTPdb$medianWTP), 
                                 mean = exp(WTPdb$meanWTP))
      
      taxesdb <- summary(dbchoice(geq_taxes_R1 + geq_taxes_R2 ~ 1 | log(geq_taxes_BD1) + log(geq_taxes_BD2), 
                                  dist = "logistic", data = data))
      
      WTP[["taxes"]] <- tibble(N = nrow(data), 
                               "N(WTP>0)" = nrow(data[data$geq_taxes_0 == "Yes",]), 
                               median = exp(taxesdb$medianWTP), 
                               mean = exp(taxesdb$meanWTP))
      
      ticketdb <- summary(dbchoice(geq_ticket_R1 + geq_ticket_R2 ~ 1 | log(geq_ticket_BD1) + log(geq_ticket_BD2), 
                                   dist = "logistic", data = data))
      
      WTP[["ticket"]] <- tibble(N = nrow(data), 
                                "N(WTP>0)" = nrow(data[data$geq_ticket_0 == "Yes",]), 
                                median = exp(ticketdb$medianWTP), 
                                mean = exp(ticketdb$meanWTP))
      
      WTP$country <- "global"
    }
    else {
      data <- data %>%
        filter(country == country_name)
      
      WTPdb <- summary(dbchoice(WTP_R1 + WTP_R2 ~ 1 | log(WTP_BD1) + log(WTP_BD2),
                                dist = "logistic", data = data))
      
      WTP[["vaccine"]] <- tibble(N = nrow(data), 
                                 "N(WTP>0)" = nrow(data[data$wtp_private == "Yes",]), 
                                 median = exp(WTPdb$medianWTP), 
                                 mean = exp(WTPdb$meanWTP))
      
      taxesdb <- summary(dbchoice(geq_taxes_R1 + geq_taxes_R2 ~ 1 | log(geq_taxes_BD1) + log(geq_taxes_BD2), 
                                  dist = "logistic", data = data))
      
      WTP[["taxes"]] <- tibble(N = nrow(data), 
                               "N(WTP>0)" = nrow(data[data$geq_taxes_0 == "Yes",]), 
                               median = exp(taxesdb$medianWTP), 
                               mean = exp(taxesdb$meanWTP))
      
      ticketdb <- summary(dbchoice(geq_ticket_R1 + geq_ticket_R2 ~ 1 | log(geq_ticket_BD1) + log(geq_ticket_BD2), 
                                   dist = "logistic", data = data))
      
      WTP[["ticket"]] <- tibble(N = nrow(data), 
                                "N(WTP>0)" = nrow(data[data$geq_ticket_0 == "Yes",]), 
                                median = exp(ticketdb$medianWTP), 
                                mean = exp(ticketdb$meanWTP))
      
      WTP$country <- country_name
      
    }
  }
  
  
    
    
  return(WTP)
  
}