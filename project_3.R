library(janitor)
library(skimr)
library(devEMF)
library(officer)
library(patchwork)
library(tidyverse)
library(tidygraph)
library(ggraph)
library(ggforce)
library(treemapify)



options(scipen = 999)

# setwd
setwd("C:/Users/Stephen/Desktop/University of Wisconsin/classes/DS745/project_3")


#//////////////////////////////////////////////////////////////////////////////////////////////////////


# create custom color_palette ####
color_palette <- tibble(hex = c("#083D7F", "#2474B6", "#8BBFD0",
                                "#CBCBCB", "#7D7D7D",
                                "#99ba78", "#35B779FF", "#006629", 
                                "#E4DC68", "#FDA159", "#EF6712", "#CE1B1E",
                                "#8B008B", "#DA70D6"))
color_palette
color_palette %>% pull(hex) %>% show_col()

# color_palette supports 11 colors, plus possible extensions via fill/line type
show_col(color_palette %>% slice(1, 3) %>% pull(hex)) # 2 colors
show_col(color_palette %>% slice(1, 2, 3) %>% pull(hex)) # 3 colors
show_col(color_palette %>% slice(1, 2, 3, 4) %>% pull(hex)) # 4 colors
show_col(color_palette %>% slice(1, 2, 3, 4, 5) %>% pull(hex)) # 5 colors
show_col(color_palette %>% slice(1, 2, 3, 4, 5, 6) %>% pull(hex)) # 6 colors
show_col(color_palette %>% slice(1, 2, 3, 4, 5, 6, 7) %>% pull(hex)) # 7 colors
show_col(color_palette %>% slice(1, 2, 3, 4, 5, 6, 7, 8) %>% pull(hex)) # 8 colors
show_col(color_palette %>% slice(1, 2, 3, 4, 5, 6, 7, 8, 9) %>% pull(hex)) # 9 colors
show_col(color_palette %>% slice(1, 2, 3, 4, 5, 6, 7, 8, 9, 10) %>% pull(hex)) # 10 colors
show_col(color_palette %>% slice(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11) %>% pull(hex)) # 11 colors


#////////////////////////////////////////////////////////////////////////////////////////////
#////////////////////////////////////////////////////////////////////////////////////////////
#////////////////////////////////////////////////////////////////////////////////////////////


# get country_crosswalk ####
country_crosswalk <- read_csv("country_crosswalk.csv", lazy = FALSE)


#/////////////////


# inspect
country_crosswalk
country_crosswalk %>% glimpse()
country_crosswalk %>% nrow() # 219
country_crosswalk %>% ncol() # 14
country_crosswalk %>% count(country) %>% arrange(desc(n))


#////////////////////////////////////////////////////////////////////////////////////////////
#////////////////////////////////////////////////////////////////////////////////////////////
#////////////////////////////////////////////////////////////////////////////////////////////


# get imports ####

imports <- read_csv(file = "DOT_03-27-2024 22-19-22-24_timeSeries.csv") %>%
        rename(country_name = `Country Name`,
               counterpart = `Counterpart Country Name`,
               category = `Indicator Name`) %>%
        filter(category %in% c("Goods, Value of Imports, Cost, Insurance, Freight (CIF), US Dollars"),
               Attribute == "Value") %>%
        select(-c(`Country Code`, `Counterpart Country Code`, `Indicator Code`, Attribute, ...83)) %>%
        mutate(across(.cols = `1948`:`2022`, .fns = ~ as.numeric(.x))) %>%
        pivot_longer(cols = -c(country_name, counterpart, category), names_to = "year", values_to = "official_imports") %>%
        mutate(country_name = case_when(country_name == "Armenia, Rep. of" ~ "Armenia",
                                        country_name == "Azerbaijan, Rep. of" ~ "Azerbaijan",
                                        country_name == "Belarus, Rep. of" ~ "Belarus",
                                        country_name == "Bosnia and Herzegovina" ~ "BiH",
                                        country_name == "Croatia, Rep. of" ~ "Croatia",
                                        country_name == "Czech Rep." ~ "Czechia",
                                        country_name == "Estonia, Rep. of" ~ "Estonia",
                                        country_name == "Kazakhstan, Rep. of" ~ "Kazakhstan",
                                        country_name == "Kosovo, Rep. of" ~ "Kosovo",
                                        country_name == "Kyrgyz Rep." ~ "Kyrgyzstan",
                                        country_name == "Moldova, Rep. of" ~ "Moldova",
                                        country_name == "Netherlands, The" ~ "Netherlands",
                                        country_name == "North Macedonia, Republic of" ~ "N. Macedonia",
                                        country_name == "Poland, Rep. of" ~ "Poland",
                                        country_name == "Russian Federation" ~ "Russia",
                                        country_name == "Serbia, Rep. of" ~ "Serbia",
                                        country_name == "Slovak Rep." ~ "Slovakia",
                                        country_name == "Slovenia, Rep. of" ~ "Slovenia",
                                        country_name == "Tajikistan, Rep. of" ~ "Tajikistan",
                                        country_name == "United Kingdom" ~ "U.K.",
                                        country_name == "United States" ~ "U.S.",
                                        country_name == "Uzbekistan, Rep. of" ~ "Uzbekistan",
                                        TRUE ~ country_name),
               counterpart = case_when(counterpart == "Armenia, Rep. of" ~ "Armenia",
                                       counterpart == "Azerbaijan, Rep. of" ~ "Azerbaijan",
                                       counterpart == "Belarus, Rep. of" ~ "Belarus",
                                       counterpart == "Bosnia and Herzegovina" ~ "BiH",
                                       counterpart == "Croatia, Rep. of" ~ "Croatia",
                                       counterpart == "Czech Rep." ~ "Czechia",
                                       counterpart == "Estonia, Rep. of" ~ "Estonia",
                                       counterpart == "Kazakhstan, Rep. of" ~ "Kazakhstan",
                                       counterpart == "Kosovo, Rep. of" ~ "Kosovo",
                                       counterpart == "Kyrgyz Rep." ~ "Kyrgyzstan",
                                       counterpart == "Moldova, Rep. of" ~ "Moldova",
                                       counterpart == "Netherlands, The" ~ "Netherlands",
                                       counterpart == "North Macedonia, Republic of" ~ "N. Macedonia",
                                       counterpart == "Poland, Rep. of" ~ "Poland",
                                       counterpart == "Russian Federation" ~ "Russia",
                                       counterpart == "Serbia, Rep. of" ~ "Serbia",
                                       counterpart == "Slovak Rep." ~ "Slovakia",
                                       counterpart == "Slovenia, Rep. of" ~ "Slovenia",
                                       counterpart == "Tajikistan, Rep. of" ~ "Tajikistan",
                                       counterpart == "United Kingdom" ~ "U.K.",
                                       counterpart == "United States" ~ "U.S.",
                                       counterpart == "Uzbekistan, Rep. of" ~ "Uzbekistan",
                                       TRUE ~ counterpart),
               official_imports = as.numeric(official_imports),
               year = as.numeric(year)) %>%
        filter(year > 2000) %>%
        mutate(type = "imports") %>%
        select(-category)


#/////////////////////


# inspect
imports
imports %>% glimpse()
imports %>% nrow() # 1095930
imports %>% ncol() # 6

# check
imports %>% count(type)
imports %>% count(year) %>% print(n = nrow(.))
imports %>% count(country_name) %>% print(n = nrow(.))

# inspect country names
sub_obj_4_1_dots_imports_from_russia_as_share_of_total_imports %>% anti_join(., country_crosswalk, by = c("country_name" = "country")) %>%
        distinct(country_name) %>% arrange(country_name) %>% print(n = nrow(.))
country_crosswalk %>% filter(ee_region_flag == 1 | country == "U.S.") %>%
        anti_join(., sub_obj_4_1_dots_imports_from_russia_as_share_of_total_imports, by = c("country" = "country_name")) %>% select(country)

sub_obj_4_1_dots_imports_from_russia_as_share_of_total_imports %>%
        filter(str_detect(string = country_name, pattern = regex("yemen", ignore_case = TRUE))) %>%
        distinct(country_name)
country_crosswalk %>% filter(str_detect(string = country, pattern = regex("gamb", ignore_case = TRUE))) %>% select(country)


#/////////////////////////////////////////////////////////////////////////////////////////////


# join imports to country_crosswalk ####
imports <- imports %>%
        inner_join(., country_crosswalk %>% select(country, ee_region_flag, mcp_grouping), 
                   by = c("country_name" = "country")) %>%
        rename(country_ee_region_flag = ee_region_flag, 
               country_mcp_grouping = mcp_grouping) %>%
        inner_join(., country_crosswalk %>% select(country, ee_region_flag, mcp_grouping), 
                   by = c("counterpart" = "country")) %>%
        rename(counterpart_ee_region_flag = ee_region_flag, 
               counterpart_mcp_grouping = mcp_grouping) %>%
        filter(country_ee_region_flag == 1,
               counterpart_ee_region_flag == 1,
               !is.na(official_imports))
        

#///////////////////////////


# inspect
imports
imports %>% glimpse()
imports %>% nrow() # 945
imports %>% ncol() # 37

# check country/year
imports %>% distinct(country_name) %>% nrow() # 45
imports %>% distinct(counterpart) %>% nrow() # 45


#/////////////////////////////////////////////////////////////////////////////////////////////
#/////////////////////////////////////////////////////////////////////////////////////////////
#/////////////////////////////////////////////////////////////////////////////////////////////


# get imports_2022 ####

imports_2022 <- imports %>% filter(year == 2022)


#/////////////////////


# inspect
imports_2022
imports_2022 %>% glimpse()


#////////////////////////////////////////////////////////////////////////////////////////////


# get exports_2022_graph ####

imports_2022 <- imports_2022 %>% 
        rename(importer = country_name, 
               exporter = counterpart,
               importer_mcp_grouping = country_mcp_grouping,
               exporter_mcp_grouping = counterpart_mcp_grouping) %>%
        mutate(from = exporter, to = importer,
               importer_mcp_grouping = case_when(importer_mcp_grouping == "E&E graduates" ~ "Eastern Europe",
                                                 importer_mcp_grouping == "E&E Balkans" ~ "Balkans",
                                                 importer_mcp_grouping == "E&E Eurasia" ~ "Eurasia",
                                                 TRUE ~ importer_mcp_grouping),
               exporter_mcp_grouping = case_when(exporter_mcp_grouping == "E&E graduates" ~ "Eastern Europe",
                                                 exporter_mcp_grouping == "E&E Balkans" ~ "Balkans",
                                                 exporter_mcp_grouping == "E&E Eurasia" ~ "Eurasia",
                                                 TRUE ~ exporter_mcp_grouping)) %>% 
        arrange(exporter)

imports_2022


#///////////////


# get numeric id for each country
country_id <- imports_2022 %>% 
        distinct(exporter) %>%
        rename(country = exporter) %>%
        bind_rows(., imports_2022 %>% 
                          distinct(importer) %>%
                          rename(country = importer)) %>%
        distinct(country) %>%
        arrange(country) %>%
        mutate(id = row_number())

country_id


#///////////////


imports_2022 <- imports_2022 %>% 
        left_join(., country_id %>% rename(exporter_id = id), by = c("exporter" = "country")) %>%
        left_join(., country_id %>% rename(importer_id = id), by = c("importer" = "country")) 

imports_2022


#///////////////


# note that i opted to to use exports to match the tidygraph from/to syntax used in as_tbl_graph, where from is set as 
exports_2022_edges <- imports_2022 %>% 
        select(exporter_id, importer_id, exporter, importer, official_imports, exporter_mcp_grouping) %>%
        rename(from = exporter_id, 
               to = importer_id,
               exports = official_imports) %>%
        distinct()

exports_2022_edges


#///////////////


exports_2022_nodes <- imports_2022 %>% 
        mutate(name = exporter_id) %>%
        select(name, exporter, year, exporter_mcp_grouping) %>%
        distinct()
   
exports_2022_nodes     
        

#///////////////


exports_2022_graph <- graph_from_data_frame(d = exports_2022_edges, v = exports_2022_nodes) %>%
        as_tbl_graph()

exports_2022_graph

# inspect nodes
exports_2022_graph %>% 
        activate(nodes) %>%
        as_tibble()

# inspect edges
exports_2022_graph %>% 
        activate(edges) %>%
        as_tibble()


#////////////////////////////////////////////////////////////////////////////////////////////


# balkans_eurasia_russia_exports_network_chart ####

set_graph_style()
balkans_eurasia_russia_exports_network_chart <- exports_2022_graph %>%
        activate(nodes) %>%
        mutate(centrality_degree = centrality_degree(weights = exports)) %>%
        filter(exporter_mcp_grouping %in% c("Balkans", "Eurasia", "Russia")) %>%
        ggraph(layout = "stress") +
        geom_edge_fan(width = .2, color = "#CBCBCB") +
        geom_node_point(mapping = aes(size = centrality_degree, color = exporter_mcp_grouping)) +
        geom_node_text(mapping = aes(label = exporter), repel = TRUE, size = 7, max.overlaps = Inf) +
        scale_color_viridis_d() +
        scale_size(guide = "none") +
        # coord_fixed(ratio =  1 / 1, clip = "off") +
        labs(size = NULL, color = NULL) +
        theme( 
                legend.position = "bottom",
                legend.text = element_text(size = 20, family = "Calibri", margin(t = 0, r = 0, b = 0, l = 0, unit = "pt"), 
                                           hjust = .5, color = "#333333") 
        ) + 
        guides(color = guide_legend(nrow = 1, byrow = TRUE, label.hjust = 0, color = "#333333",
                                    override.aes = list(size = 5)))

# inspect
balkans_eurasia_russia_exports_network_chart 


#//////////////////////////////////


# save chart as emf
filename <- tempfile(fileext = ".emf")
emf(file = filename)
print(balkans_eurasia_russia_exports_network_chart )
dev.off()

# add emf to word doc  
read_docx() %>% 
        body_add_img(src = filename, width = 6, height = 6) %>% 
        print(target = "balkans_eurasia_russia_exports_network_chart .docx")


#////////////////////////////////////////////////////////////////////////////////////////////


# ee_region_exports_network_chart ####

set_graph_style()
ee_region_exports_network_chart <- exports_2022_graph %>%
        activate(nodes) %>%
        mutate(centrality_degree = centrality_degree(weights = exports)) %>%
        filter(exporter_mcp_grouping %in% c("EU-15", "Balkans", "Eurasia", "Eastern Europe", "CARs", "Russia")) %>%
        ggraph(layout = "stress") +
        geom_edge_fan(width = .2, color = "#CBCBCB") +
        geom_node_point(mapping = aes(size = centrality_degree, color = exporter_mcp_grouping)) +
        geom_node_text(mapping = aes(label = exporter), size = 7, repel = TRUE, max.overlaps = Inf) +
        scale_color_viridis_d() +
        scale_size(guide = "none") +
        # coord_fixed(ratio =  1 / 1, clip = "off") +
        labs(size = NULL, color = NULL) +
        theme( 
                legend.position = "bottom",
                legend.text = element_text(size = 20, family = "Calibri", margin(t = 0, r = 0, b = 0, l = 0, unit = "pt"), 
                                           hjust = .5, color = "#333333") 
        ) + 
        guides(color = guide_legend(nrow = 2, byrow = TRUE, label.hjust = 0, color = "#333333",
                                    override.aes = list(size = 5)))

# inspect
ee_region_exports_network_chart


#//////////////////////////////////


# save chart as emf
filename <- tempfile(fileext = ".emf")
emf(file = filename)
print(ee_region_exports_network_chart)
dev.off()

# add emf to word doc  
read_docx() %>% 
        body_add_img(src = filename, width = 6, height = 6) %>% 
        print(target = "ee_region_exports_network_chart.docx")


#////////////////////////////////////////////////////////////////////////////////////////////
#////////////////////////////////////////////////////////////////////////////////////////////
#////////////////////////////////////////////////////////////////////////////////////////////


# get treemap of eurasia imports

eurasian_exports_treemap <- imports_2022 %>% 
        filter(exporter_mcp_grouping == "Eurasia") %>%
        rename(exports = official_imports) %>%
        select(exporter, importer, exports, importer_mcp_grouping) %>%
        group_by(importer, importer_mcp_grouping) %>%
        summarize(exports_sum = sum(exports, na.rm = TRUE), .groups = "drop") %>%
        ungroup() %>%
        ggplot(mapping = aes(area = exports_sum, fill = importer_mcp_grouping,
                       label = importer, subgroup = importer_mcp_grouping)) +
        geom_treemap() +
        geom_treemap_subgroup_border(colour = "white", size = 5) +
        geom_treemap_text(colour = "white", place = "centre",
                          size = 15, grow = TRUE) +
        scale_fill_viridis_d() +
        # coord_fixed(ratio =  1 / 1, clip = "off") +
        labs(fill = NULL) +
        theme( 
                legend.position = "bottom",
                legend.text = element_text(size = 20, family = "Calibri", margin(t = 0, r = 0, b = 0, l = 0, unit = "pt"), 
                                           hjust = .5, color = "#333333")
        ) + 
        guides(fill = guide_legend(nrow = 2, byrow = TRUE, label.hjust = 0, color = "#333333"))

# inspect
eurasian_exports_treemap


#//////////////////////////////////


# save chart as emf
filename <- tempfile(fileext = ".emf")
emf(file = filename)
print(eurasian_exports_treemap)
dev.off()

# add emf to word doc  
read_docx() %>% 
        body_add_img(src = filename, width = 6, height = 6) %>% 
        print(target = "eurasian_exports_treemap.docx")


#////////////////////////////////////////////////////////////////////////////////////////////
#////////////////////////////////////////////////////////////////////////////////////////////
#////////////////////////////////////////////////////////////////////////////////////////////


# get treemap of balkans imports

balkan_exports_treemap <- imports_2022 %>% 
        filter(exporter_mcp_grouping == "Balkans") %>%
        rename(exports = official_imports) %>%
        select(exporter, importer, exports, importer_mcp_grouping) %>%
        group_by(importer, importer_mcp_grouping) %>%
        summarize(exports_sum = sum(exports, na.rm = TRUE), .groups = "drop") %>%
        ungroup() %>%
        ggplot(mapping = aes(area = exports_sum, fill = importer_mcp_grouping,
                             label = importer, subgroup = importer_mcp_grouping)) +
        geom_treemap() +
        geom_treemap_subgroup_border(colour = "white", size = 5) +
        geom_treemap_text(colour = "white", place = "centre",
                          size = 15, grow = TRUE) +
        scale_fill_viridis_d() +
        # coord_fixed(ratio =  1 / 1, clip = "off") +
        labs(fill = NULL) +
        theme( 
                legend.position = "bottom",
                legend.text = element_text(size = 20, family = "Calibri", margin(t = 0, r = 0, b = 0, l = 0, unit = "pt"), 
                                           hjust = .5, color = "#333333")
        ) + 
        guides(fill = guide_legend(nrow = 2, byrow = TRUE, label.hjust = 0, color = "#333333"))

# inspect
balkan_exports_treemap


#//////////////////////////////////


# save chart as emf
filename <- tempfile(fileext = ".emf")
emf(file = filename)
print(balkan_exports_treemap)
dev.off()

# add emf to word doc  
read_docx() %>% 
        body_add_img(src = filename, width = 6, height = 6) %>% 
        print(target = "balkan_exports_treemap.docx")



