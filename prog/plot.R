# library -----------------------------------------------------------------

library(dplyr)
library(tidyr)
library(purrr)
library(readr)
library(stringr)
library(magrittr)
library(ggplot2)
library(cowplot)
library(readxl)
library(lemon)
library(patchwork)
library(gdxrrw)
library(ggh4x)
library(ggrepel)


# input data --------------------------------------------------------------

timeslice<-c('2C_Monthly','2C_Quarterly','2C_Half-yearly',
             '1.5C_Monthly','1.5C_Quarterly','1.5C_Half-yearly',
             '1.5Cdec_Monthly','1.5Cdec_Quarterly','1.5Cdec_Half-yearly')
Month<-c('2C_Monthly','1.5C_Monthly','1.5Cdec_Monthly')
Season<-c('2C_Half-yearly','1.5C_Half-yearly','1.5Cdec_Half-yearly')

df.var <- read_csv('define/variable.csv',col_names=c('Variable2','Variable'))
map_R10 <- read_csv('define/AIMTech_R31toR10.csv')
process_csv <- function(path) {
  read_csv(path, show_col_types = FALSE) %>%
    gather(-Model, -Scenario, -Region, -Variable, -Unit, key = "Year", value = "Value") %>%
    filter(Value != "N/A") %>%
    inner_join(df.var, by = "Variable") %>%
    select(-Variable) %>%
    rename(Variable = "Variable2") %>%
    spread(key = Year, value = "Value", fill = 0) %>%
    gather(-c("Model", "Scenario", "Region", "Variable", "Unit"), key = "Year", value = "Value") %>%
    spread(key = Scenario, value = "Value", fill = 0) %>%
    gather(-c("Model", "Region", "Variable", "Year", "Unit"), key = "Scenario", value = "Value") %>%
    mutate(Year = as.integer(as.character(Year)))
}

df.m <- process_csv('data/monthly/scenario_data.csv')
df.q <- process_csv('data/quarterly/scenario_data.csv')
df.h <- process_csv('data/half-yearly/scenario_data.csv')
map_R10 <- read_csv('define/AIMTech_R31toR10.csv')

df_all <- full_join(df.m,df.q) %>%
  full_join(df.h) %>%
  select(-Model,-Unit) %>% 
  mutate(Scenario=recode(Scenario,
                   NPi1000m="2C_Monthly",
                   NPi1000q="2C_Quarterly",
                   NPi1000s="2C_Half-yearly",
                   NPi500m="1.5C_Monthly",
                   NPi500q="1.5C_Quarterly",
                   NPi500s="1.5C_Half-yearly",
                   NPi500m_DEC="1.5Cdec_Monthly",
                   NPi500q_DEC="1.5Cdec_Quarterly",
                   NPi500s_DEC="1.5Cdec_Half-yearly")) %>%
  filter(Scenario %in% timeslice) %>%
  mutate(
    Fam = sub("_.*$", "", Scenario),
    Res = sub("^.*_", "", Scenario) %>% str_replace_all("\\s+", "") %>% na_if("")
  )

df_r <- df_all %>% 
  filter(Region %in% map_R10$R31) %>%
  left_join(map_R10 %>% select(R31, R10), by = c("Region" = "R31")) %>%
  group_by(R10, Scenario, Year, Variable,Fam,Res) %>%
  summarise(Value = sum(Value, na.rm = TRUE), .groups = "drop") %>% 
  mutate(R10=recode(R10,MIDDLE_EAST="MID_EAST")) 

df <- df %>% 
  filter(Region=="World")

year_all <- seq(2020,2050,5)
dir.create(path='output',showWarnings=F)

# Theme & Function --------------------------------------------------------

MyTheme <- theme_bw() +
  theme(
    panel.border=element_blank(),
    panel.grid.minor = element_line(color = NA), 
    axis.line=element_line(colour="black"),
    panel.background=element_rect(fill = "white"),
    panel.grid.major=element_blank(),
    strip.background=element_rect(fill="white", colour="white"),
    strip.text.x = element_text(size=8, colour = "black", angle = 0,face="bold"),
    strip.text.y = element_text(size=8, colour = "black", angle = 270,face="bold"),
    axis.text.x=element_text(size = 8, angle=45, vjust=0.9, hjust=1, margin = margin(t = 0.1, r = 0, b = 0, l = 0, unit = "cm")),
    axis.text.y=element_text(size = 8, margin = margin(t = 0, r = 0.3, b = 0, l = 0, unit = "cm")),
    axis.title.x = element_blank(),
    legend.text = element_text(size=9),
    legend.title = element_text(size=8),
    axis.ticks.length=unit(0.15,"cm"),
    plot.tag=element_text(face='bold')
  )

set_plot <- function(var){
  plt <- list()
  plt$Color <- as.character(var$Color); names(plt$Color) <- as.character(var$Variable)
  plt$Legend <- as.character(var$Legend); names(plt$Legend) <- as.character(var$Variable)
  return(plt)
}

source('prog/dataframe.R')

year_all <- seq(2020,2050,5)

# 1-a ------------------------------------------------------------------------

df_secele <- df %>% 
  filter(Scenario != "Baseline") %>% 
  filter(Variable %in% secele$Variable, Year %in% year_all) %>% 
  mutate(Variable = factor(Variable, levels = rev(secele$Variable)))

plt <- set_plot(secele)

g_secele2050 <- df_secele %>% 
  filter(Year == 2050) %>%
  mutate(Scenario = factor(Scenario, levels = timeslice)) %>% 
  ggplot() +
  geom_bar(aes(x = Scenario, y = Value, fill = Variable), stat = "identity") +
  scale_fill_manual(values = plt$Color, labels = plt$Legend, name = "") +
  coord_cartesian(ylim = c(0, 600)) +
  guides(fill = guide_legend(reverse = FALSE)) +
  labs(y = expression(paste("Power generation (EJ ", {yr^-1}, ")"))) +
  MyTheme

vre_keys <- c("Sec_Ene_Ele_Solar", "Sec_Ene_Ele_Win", "Sec_Ene_Ele")

df_vre_2050 <- df %>%
  filter(Variable %in% vre_keys) %>%
  group_by(Scenario, Year, Variable) %>%
  summarise(Value = sum(Value, na.rm = TRUE), .groups = "drop") %>%
  pivot_wider(
    names_from  = Variable,
    values_from = Value,
    values_fill = list(Value = 0)
  ) %>%
  mutate(
    VRE           = coalesce(Sec_Ene_Ele_Solar, 0) +
      coalesce(Sec_Ene_Ele_Win,   0),
    VRE_share_pct = if_else(Sec_Ene_Ele > 0, 100 * VRE / Sec_Ene_Ele, NA_real_)
  ) %>%
  filter(Year == 2050) %>%
  mutate(Scenario = factor(Scenario, levels = timeslice))

y_upper <- 600
vre_min <- 75
vre_max <- 100
scale_fac_zoom <- y_upper / (vre_max - vre_min)

df_scen_label <- tibble(
  Scenario = factor(
    c("2C_Quarterly", "1.5C_Quarterly", "1.5Cdec_Quarterly"), 
    levels = timeslice
  ),
  label = c("2C", "1.5C", "1.5Cdec"),
  y     = c(580, 580, 580) 
)

g_core <- df_secele %>% 
  filter(Year == 2050) %>%
  mutate(Scenario = factor(Scenario, levels = timeslice)) %>% 
  ggplot() +
  geom_bar(aes(x = Scenario, y = Value, fill = Variable), stat = "identity") +
  scale_x_discrete(
    labels = function(x) sub(".*_", "", x)  # "2C_Monthly" → "Monthly"
  ) +
  scale_fill_manual(values = plt$Color, labels = plt$Legend, name = "") +
  geom_point(
    data = df_vre_2050,
    aes(
      x     = Scenario,
      y     = (VRE_share_pct - vre_min) * scale_fac_zoom,
      shape = "VRE share"
    ),
    inherit.aes = FALSE,
    size        = 2.5,
    stroke      = 1,
    fill        = "white",
    color       = "black"
  ) +
  scale_shape_manual(
    name   = "VRE share",
    values = c("VRE share" = 21)
  ) +
  scale_y_continuous(
    limits = c(0, y_upper),
    expand = expansion(mult = c(0, 0.02)),
    name   = expression(paste("Power generation (EJ ", {yr^-1}, ")")),
    sec.axis = sec_axis(
      ~ . / scale_fac_zoom + vre_min,
      breaks = seq(vre_min, vre_max, by = 5),
      labels = function(x) sprintf("%d%%", x),
      name   = "VRE share"
    )
  ) +
  guides(
    fill  = guide_legend(reverse = FALSE),
    shape = guide_legend(
      override.aes = list(size = 3, fill = "white", color = "black")
    )
  ) +
  MyTheme +
  theme(
    legend.key.size = unit(5.5, "mm"),
    legend.text     = element_text(size = 8)
  ) +
  geom_text(
    data = df_scen_label,
    aes(x = Scenario, y = y, label = label),
    vjust = 0,
    size  = 3,
    fontface = "bold" 
  )

l_secele <- plot_grid(
  get_legend(
    g_secele2050 +
      guides(fill = guide_legend(nrow = 2)) +
      theme(legend.text = element_text(size = 8))
  ),
  nrow = 1
)

g_secele2050_main <- g_core +
  guides(fill = "none") +
  theme(
    legend.title        = element_blank(),
    legend.position     = c(0.02, 0.9),
    legend.justification = c(0, 1)
  )

g_secele2050_VREshare <- plot_grid(
  g_secele2050_main,
  l_secele,
  ncol        = 1,
  rel_heights = c(7.5, 1.5)
)

# 1-b -------------------------------------------------------------------------
res_order <- c("Monthly", "Quarterly", "Half-yearly")
fam_order <- c("2C", "1.5C", "1.5Cdec")

normalize_res <- function(x) {
  case_when(
    grepl("Monthly", x) ~ "Monthly",
    grepl("Quarterly", x) ~ "Quarterly",
    grepl("Half-yearly",   x) ~ "Half-yearly",
    TRUE ~ NA_character_
  )
}

vars_map <- c(
  "Sec_Ene_Hyd_Ele"     = "Electrolysis",
  "Sec_Ene_Ele_Sto_Dis" = "Storage",
  "Sec_Ene_Ele_Cur"     = "Curtailment"
)

df_pts2 <- df %>%
  filter(
    Scenario != "Baseline",
    Variable %in% names(vars_map),
    Year == 2050
  ) %>%
  mutate(
    Res = normalize_res(Scenario),
    Fam = case_when(
      grepl("^2C",        Scenario) ~ "2C",
      grepl("^1\\.5Cdec", Scenario) ~ "1.5Cdec",
      grepl("^1\\.5C",    Scenario) ~ "1.5C",
      TRUE ~ NA_character_
    ),
    VariableLab = recode(Variable, !!!vars_map)
  ) %>%
  filter(!is.na(Fam), !is.na(Res)) %>%
  group_by(VariableLab, Fam, Res) %>%
  summarise(Value = sum(Value, na.rm = TRUE), .groups = "drop") %>%
  mutate(
    Fam         = factor(Fam, levels = fam_order),
    Res         = factor(Res, levels = res_order),
    VariableLab = factor(VariableLab,
                         levels = c("Electrolysis", "Storage", "Curtailment"))
  )

color_pal <- c(
  "Monthly" = "indianred1",
  "Quarterly" = "deepskyblue1",
  "Half-yearly"   = "darkgoldenrod2")

shape_pal <- c(
  "Monthly" = 16,  
  "Quarterly" = 24, 
  "Half-yearly"   = 4 )

p_VRE_responce <- df_pts2 %>%
  ggplot(aes(x = Fam, y = Value, color = Res, shape = Res)) +
  geom_line(
    aes(group = interaction(VariableLab, Fam)),
    color        = "grey70",
    linewidth    = 0.4,
    show.legend  = FALSE
  ) +
  geom_point(size = 3, stroke = 1) +
  facet_wrap(~ VariableLab, scales = "free_y", nrow = 1) +
  scale_shape_manual(values = shape_pal, guide = "none") +
  scale_color_manual(values = color_pal, name = "Resolution") +
  scale_y_continuous(limits = c(0, NA), expand = expansion(mult = c(0, 0.05))) +
  labs(
    x = "Emission constraint (Fam)",
    y = expression(paste("Value (EJ ", {yr^-1}, ")")),
    color = "Resolution",      
    shape = "Resolution"
  ) +
  MyTheme +
  guides(
    color = guide_legend(
      nrow = 1,      
      override.aes = list(
        shape = shape_pal,     
        size  = 2.5,
        stroke = 1),
      label.theme = element_text(
        size   = 9,
        colour = "black")
    )
  ) +
  theme(
    axis.text.x = element_text(size = 8.5),
    legend.position = "bottom",
    
    legend.text  = element_text(size = 8),
    legend.title = element_blank(),
    legend.key.width  = unit(1, "lines"),  
    legend.spacing.x  = unit(0.4, "cm"),      
  )

g_VRE_responce <- p_VRE_responce +
  guides(fill = "none")

l_VRE <- plot_grid(
  get_legend(
    p_VRE_responce +                
      theme(legend.position    = "bottom",
            legend.justification = "left",  
            legend.box.just      = "left",   
            legend.text = element_text(size = 8))
  ),
  nrow = 1
)

VRE_responce <- plot_grid(
  g_VRE_responce,
  l_VRE,
  ncol        = 1,
  rel_heights = c(80, 1)
)


# Fig1 合成------------------------------------------------------------------------

ele_plot <- plot_grid(g_secele2050_VREshare,VRE_responce,
                      nrow=1,rel_widths=c(1.1,1),labels=c('a','b'),label_size=12)
ggsave(filename='output/figure/Fig1.png',plot=ele_plot,width=180,height=120,units='mm',dpi=300,bg='white')



# 2-a -------------------------------------------------------------------------

df_finene <- df %>% 
  filter(Variable %in% finene$Variable, Year%in%year_all) %>% 
  mutate(Sb=case_when(
    str_detect(Variable,'Bio') ~ 'Biomass',
    TRUE ~ Variable
  )) %>% 
  group_by(Scenario,Sb,Year) %>% 
  summarise(Value=sum(Value)) %>% 
  ungroup() %>% 
  rename(Variable=Sb) %>% 
  mutate(Variable=factor(Variable,levels=rev(c('Fin_Ene_Liq_Oil',
                                               'Fin_Ene_SolidsCoa',
                                               'Fin_Ene_Gas',
                                               'Biomass',
                                               'Fin_Ene_Solar',
                                               'Fin_Ene_Ele',
                                               'Fin_Ene_Heat',
                                               'Fin_Ene_Hyd',
                                               'Fin_Ene_Liq_Hyd_syn'))))

plt <- set_plot(finene %>% 
                  filter(!str_detect(Variable,'Bio')) %>%
                  bind_rows(data.frame(Variable='Biomass',Legend='Biomass',Color='darkolivegreen2')) %>%
                  mutate(Variable=as.character(Variable)) %>% 
                  mutate(Legend=factor(Legend,levels=c('Oil',
                                                       'Coal',
                                                       'Gas',
                                                       'Biomass',
                                                       'Solar',
                                                       'Electricity',
                                                       'Heat',
                                                       'Hydrogen',
                                                       'Synfuel'))) %>% 
                  arrange(Legend))

df_scen_label <- tibble(
  Scenario = factor(
    c("2C_Quarterly", "1.5C_Quarterly", "1.5Cdec_Quarterly"), 
    levels = timeslice
  ),
  label = c("2C", "1.5C", "1.5Cdec"),
  y     = c(550, 550, 550)  
)

g_finene2050<-df_finene %>% 
  filter(Scenario%in%timeslice) %>% 
  mutate(Scenario=factor(Scenario,levels=c(timeslice))) %>% 
  filter(Year==2050) %>% 
  ggplot() +
  geom_bar(aes(x=Scenario,y=Value,fill=Variable),stat='identity') +
  scale_x_discrete(
    labels = function(x) sub(".*_", "", x) 
  ) +
  scale_y_continuous(breaks = function(lim) seq(0, lim[2], by = 100)) +
  scale_fill_manual(values=plt$Color,labels=plt$Legend,name='') +
  guides(fill=guide_legend(reverse=FALSE)) +
  labs(y=expression(paste('Final energy (EJ ',{yr^-1},')'))) +
  MyTheme +
  theme(
    legend.title = element_text())+
  geom_text(
    data = df_scen_label,
    aes(x = Scenario, y = y, label = label),
    vjust = 0,
    size  = 3,
    fontface = "bold" 
  )

# 2-b -----------------------------------------------------

diff<-c('2C_Monthly','2C_Quarterly',
        '1.5C_Monthly','1.5C_Quarterly',
        '1.5Cdec_Monthly','1.5Cdec_Quarterly')

df_sector <- list(name1=c('Ind','Res_and_Com','Tra'),name2=list(finind,finbui,fintra))

df_finsec <- map2(df_sector$name1, df_sector$name2,~{
  df %>% filter(Variable%in%..2$Variable) %>% 
    mutate(Sb=case_when(
      str_detect(Variable,'Oil') ~ 'Fin_Ene_Liq_Oil',
      str_detect(Variable,'Coa') ~ 'Fin_Ene_SolidsCoa',
      str_detect(Variable,'Gas') ~ 'Fin_Ene_Gas',
      str_detect(Variable,'Bio') ~ 'Biomass',
      str_detect(Variable,'Solar') ~ 'Fin_Ene_Solar',
      str_detect(Variable,'Ele') ~ 'Fin_Ene_Ele',
      str_detect(Variable,'Heat') ~ 'Fin_Ene_Heat',
      str_detect(Variable,'Hyd')&!str_detect(Variable,'syn') ~ 'Fin_Ene_Hyd',
      str_detect(Variable,'syn') ~ 'Fin_Ene_Liq_Hyd_syn'
    )) %>% 
    group_by(Scenario,Sb,Year) %>% 
    summarise(Value=sum(Value)) %>% 
    rename(Variable=Sb) %>%
    mutate(Variable=factor(Variable,levels=rev(c('Fin_Ene_Liq_Oil',
                                                 'Fin_Ene_SolidsCoa',
                                                 'Fin_Ene_Gas',
                                                 'Biomass',
                                                 'Fin_Ene_Solar',
                                                 'Fin_Ene_Ele',
                                                 'Fin_Ene_Heat',
                                                 'Fin_Ene_Hyd',
                                                 'Fin_Ene_Liq_Hyd_syn')))) %>% 
    mutate(Se=..1)
}) %>%
  bind_rows() %>% 
  mutate(Se=case_when(
    Se=='Ind'~'Industry',
    Se=='Tra'~'Transport',
    TRUE~'Buildings'
  )) %>%
  mutate(Se=factor(Se,levels=c('Industry','Buildings','Transport')))

plt <- set_plot(finene %>% 
                  filter(!str_detect(Variable,'Bio')) %>%
                  bind_rows(data.frame(Variable='Biomass',Legend='Biomass',Color='darkolivegreen2')) %>%
                  mutate(Variable=as.character(Variable)) %>% 
                  mutate(Legend=factor(Legend,levels=c('Oil',
                                                       'Coal',
                                                       'Gas',
                                                       'Biomass',
                                                       'Solar',
                                                       'Electricity',
                                                       'Heat',
                                                       'Hydrogen',
                                                       'Synfuel'))) %>% 
                  arrange(Legend))

df_finsec_diff <- df_finsec %>%
  filter(Scenario %in% timeslice) %>%
  mutate(Scenario = factor(Scenario, levels = c(timeslice))) %>%
  filter(Year == 2050) %>%
  group_by(Se, Variable) %>%
  mutate(Value = case_when(
    Scenario %in% c('2C_Monthly', '2C_Quarterly', '2C_Half-yearly') ~ 
      Value - ifelse(any(Scenario == '2C_Half-yearly'), Value[Scenario == '2C_Half-yearly'], 0),
    Scenario %in% c('1.5C_Monthly', '1.5C_Quarterly', '1.5C_Half-yearly') ~ 
      Value - ifelse(any(Scenario == '1.5C_Half-yearly'), Value[Scenario == '1.5C_Half-yearly'], 0),
    Scenario %in% c('1.5Cdec_Monthly', '1.5Cdec_Quarterly', '1.5Cdec_Half-yearly') ~ 
      Value - ifelse(any(Scenario == '1.5Cdec_Half-yearly'), Value[Scenario == '1.5Cdec_Half-yearly'], 0),
  )) %>%
  ungroup() %>%
  filter(Scenario %in% diff)

g_finbar<-df_finsec_diff %>% 
  ggplot() +
  geom_bar(aes(x=Scenario,y=Value,fill=Variable),stat='identity') +
  guides(fill = guide_legend(reverse = FALSE, nrow = 3, ncol = 3)) +  
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
  scale_fill_manual(values=plt$Color,labels=plt$Legend,name='') +
  labs(y=expression(paste('Final energy difference (EJ ',{yr^-1},')'))) +
  facet_wrap(vars(Se),nrow=1)+
  MyTheme +
  theme(    legend.position      = c(0.98, 0.98),   
            legend.justification = c(1, 1),        
            legend.title = element_text())

# 2-c 世界全体の部門別CO2排出量-------------------------------------------------------------------------

emisec <- tribble(~Variable,~Legend,~Color,
                  'Emi_CO2_Oth','DACCS','thistle2',
                  'Emi_CO2_Ene_Sup','Energy\nsupply','moccasin',
                  'Emi_CO2_Ene_Dem_Ind','Industry','salmon',
                  'Emi_CO2_Ene_Dem_Res_and_Com','Buildings','lightskyblue3',
                  'Emi_CO2_Ene_Dem_Tra','Transport','darkolivegreen2',
                  'Emi_CO2_Ind_Pro','Industrial\nprocess','sandybrown')

res_order <- c("Monthly", "Quarterly", "Half-yearly")
fam_order <- c("2C", "1.5C", "1.5Cdec")

normalize_res <- function(x) {
  case_when(
    grepl("Monthly", x, ignore.case = TRUE) ~ "Monthly",
    grepl("Quarterly", x, ignore.case = TRUE, perl = TRUE)  ~ "Quarterly",
    grepl("Half-yearly", x, ignore.case = TRUE)  ~ "Half-yearly",
    TRUE ~ NA_character_
  )
}

df_emi <- df %>%
  filter(Scenario != "Baseline",
         Variable %in% emisec$Variable,
         Year == 2050) %>%
  mutate(
    Res = normalize_res(as.character(Scenario)),
    Fam = dplyr::case_when(
      grepl("^2C",        Scenario) ~ "2C",
      grepl("^1\\.5Cdec", Scenario) ~ "1.5Cdec",
      grepl("^1\\.5C",    Scenario) ~ "1.5C",
      TRUE ~ NA_character_
    ),
    Variable=factor(Variable,levels=rev(emisec$Variable)))%>%
  filter(!is.na(Res), !is.na(Fam)) %>%
  mutate(
    Res         = factor(Res, levels = res_order),
    Fam         = factor(Fam, levels = fam_order))

plt <- set_plot(emisec)

g_emi_2050 <- df_emi %>%
  ggplot() +
  geom_bar(aes(x = Res, y = Value / 1000, fill = Variable), stat = 'identity') +
  scale_fill_manual(values = plt$Color, labels = plt$Legend, name ='Variable') +  
  guides(fill = guide_legend(nrow = 1)) + 
  facet_wrap(vars(Fam),scales='free') +
  labs(y = expression(paste({CO[2]}, ' emissions (Gt', {CO[2]}, ' ', {yr^-1}, ')'))
  ) +
  geom_hline(yintercept = 0, linetype = 'dashed', color = 'grey10', linewidth = 1) +
  MyTheme +
  theme(legend.position = "right",
        legend.title=element_blank())

# 2-d  -----------------------------------------------------------
df_totcos2050 <- df %>% 
  filter(Variable == "Pol_Cos_Add_Tot_Ene_Sys_Cos",
         Scenario != "Baseline") %>%
  filter(Year %in% 2025:2050) %>%
  mutate(
    discount = 0.95^(Year - 2023),
    Value    = Value * discount * 5 / 1000  
  ) %>% 
  group_by(Fam, Res) %>% 
  summarise(Value = sum(Value), .groups = "drop") %>%
  mutate(
    Fam = factor(Fam, levels = c("2C", "1.5C", "1.5Cdec")),
    Res = factor(Res, levels = c("Monthly", "Quarterly", "Half-yearly"))
  )

g_totcos2050 <- df_totcos2050 %>%   
  ggplot(aes(x = Fam, y = Value, shape = Res, group = Res)) +
  geom_point(size = 3) +                    
  labs(
    x = "Emission constraint (Fam)",
    y = "Cumulative energy system cost\n(trillion US$2010)",
    shape = "Resolution (Res)"
  ) +
  scale_shape_manual(values = c(
      "Monthly" = 16,   
      "Quarterly" = 24,   
      "Half-yearly"   = 4   
    )) +
  coord_cartesian(ylim = c(0, 25)) +
  MyTheme +
  theme(
    legend.position      = c(0.95, 0.05),  
    legend.justification = c(1, 0),     
    legend.background    = element_rect(fill = "white", color = NA),
    legend.key.size      = unit(4, "mm"),
    legend.text          = element_text(size = 10),
    legend.title         = element_blank()
  )


# Fig2 -------------------------------------------------------------------------
l_finene <- plot_grid(get_legend(
  g_finene2050+theme(legend.text = element_text(size = 8))+guides(fill = guide_legend(ncol=1))))
finene_2050 <- plot_grid(g_finene2050+theme(legend.position='none'),
                         labels='a',label_size=12)
finene_sec <- plot_grid(g_finbar+theme(legend.position='none'),
                        labels='b',label_size=12)
finene_plot <- plot_grid(finene_2050,finene_sec,l_finene,nrow=1,rel_widths=c(5,7,1.6))

l_emi <- plot_grid(get_legend(
  g_emi_2050+theme(legend.text = element_text(size = 8))+guides(fill = guide_legend(ncol=1))))
emi <- plot_grid(g_emi_2050+theme(legend.position='none'),
                 nrow=1,labels='c',label_size=12)
cost <- plot_grid(g_totcos2050,
                 nrow=1,labels='d',label_size=12)

emi_cost_plot <- plot_grid(emi,l_emi,cost,nrow=1,rel_widths=c(6,1.5,3))
 
Fig2_plot <- plot_grid(finene_plot,emi_cost_plot,ncol=1,rel_heights=c(1,1))
ggsave(filename='output/figure/Fig2.png',plot=Fig2_plot,width=180,height=160,units='mm',dpi=300,bg='white')



# Fig3a ----------------------------------------------------------
fam_order <- c("2C", "1.5C", "1.5Cdec")
r10_order <- c("EUROPE","REF_ECON", "NORTH_AM","LATIN_AM","PAC_OECD",
               "AFRICA","MID_EAST","CHINA+","INDIA+","REST_ASIA")
labeller_R10 <- c(
  "REF_ECON"    = "REF_\nECON",
  "NORTH_AM" = "NORTH_\nAM",
  "LATIN_AM" = "LATIN_\nAM",
  "PAC_OECD" = "PAC_\nOECD",
  "MID_EAST"   = "MID_\nEAST",
  "REST_ASIA" = "REST_\nASIA")

df_secele <- df_r %>%
  filter(Scenario != "Baseline",
         Variable %in% secele$Variable,
         Year == 2050) %>%
  mutate(Variable=factor(Variable,levels=rev(secele$Variable)))%>%
  filter(!is.na(Res), !is.na(Fam)) %>%
  mutate(
    R10         = factor(R10, levels = r10_order),
    Res         = factor(Res, levels = res_order),
    Fam         = factor(Fam, levels = fam_order))

plt <- set_plot(secele)

g_R10secele2050 <- ggplot(df_secele) +
  geom_bar(aes(x = Res, y = Value, fill = Variable), stat = "identity") +
  scale_fill_manual(values = plt$Color, labels = plt$Legend, name = "Variable") +
  guides(fill = guide_legend(nrow = 1)) +
  ggh4x::facet_grid2(
    rows   = vars(Fam),
    cols   = vars(R10),
    scales = "free_y",
    independent = "y",
    axes   = "x",
    labeller = labeller(R10 = labeller_R10)
  ) +
  scale_x_discrete(limits = c("Monthly","Quarterly","Half-yearly"), drop = FALSE) +
  labs(y=expression(paste('Power generation (EJ ',{yr^-1},')'))) +
  MyTheme +
  theme(
    legend.position = "bottom",
    legend.title = element_blank(),
    legend.text = element_text(size=8),
    strip.text.y = element_text(size = 8),
    strip.placement = "outside",
    strip.text.y.right = element_text(size = 8),
    axis.text.x=element_text(size = 8,angle=45, vjust=0.9, hjust=1, margin = margin(t = 0.1, r = 0.1, b = 0, l = 0, unit = "cm")),
    axis.text.y=element_text(size = 8,margin = unit(c(t = 0, r = 0.1, b = 0, l = 0), "cm"))
  )
# Fig3b ------------------------------------------------------------------------
r10_order <- c("EUROPE","REF_ECON", "NORTH_AM","LATIN_AM","PAC_OECD",
               "AFRICA","MID_EAST","CHINA+","INDIA+","REST_ASIA")

PW_list <- tribble(~Variable,~Legend,~Color,
                   'Sec_Ene_Ele_Cur','Curtailment','gray',
                   'PHES+CAES','PHES+CAES','lightseagreen',
                   'Sec_Ene_Ele_Sto_Dis_Bat','Battery','lightblue1',
                   'Sec_Ene_Hyd_Ele','Electrolysis','thistle2')

res_order <- c("Monthly", "Quarterly", "Half-yearly")
fam_order <- c("2C", "1.5C", "1.5Cdec")

vars_map <- c(
  "Sec_Ene_Hyd_Ele"         = "H2 electrolysis",
  "Sec_Ene_Ele_Sto_Dis_Bat" = "Battery charge",
  "Sec_Ene_Ele_Sto_Dis_PHS" = "PHES",
  "Sec_Ene_Ele_Sto_Dis_CAE" = "CAES",
  "PHES+CAES"                = "PHES+CAES",
  "Sec_Ene_Ele_Cur"         = "Curtailment")

df_VRE <- df_r %>%
  filter(Scenario != "Baseline",
         Variable %in% names(vars_map),
         Year == 2050) 

df_phes_caes <- df_VRE %>%
  filter(Variable %in% c("Sec_Ene_Ele_Sto_Dis_PHS", "Sec_Ene_Ele_Sto_Dis_CAE")) %>%
  group_by(Scenario, Fam, Res, R10, Year) %>%
  summarise(Value = sum(Value, na.rm = TRUE), .groups = "drop") %>%
  mutate(Variable = "PHES+CAES")

df_others <- df_VRE %>%
  filter(!Variable %in% c("Sec_Ene_Ele_Sto_Dis_PHS", "Sec_Ene_Ele_Sto_Dis_CAE")) 

df_combined <- bind_rows(df_others, df_phes_caes)%>%
  mutate(Variable=factor(Variable,levels=names(vars_map)))%>%
  filter(!is.na(Res), !is.na(Fam)) %>%
  mutate(
    R10         = factor(R10, levels = r10_order),
    Res         = factor(Res, levels = res_order),
    Fam         = factor(Fam, levels = fam_order))

plt <- set_plot(PW_list)

g_R10VRE_2050 <- ggplot(df_combined) +
  geom_bar(aes(x = Res, y = Value, fill = Variable), stat = "identity") +
  scale_fill_manual(values = plt$Color, labels = plt$Legend, name = "Variable") +
  guides(fill = guide_legend(nrow = 1)) +
  ggh4x::facet_grid2(
    rows   = vars(Fam),
    cols   = vars(R10),
    scales = "free_y",
    independent = "y",
    axes   = "x",
    labeller = labeller(R10 = labeller_R10)
  ) +
  scale_x_discrete(limits = c("Monthly","Quarterly","Half-yearly"), drop = FALSE) +
  labs(y=expression(paste('Usage of VRE response measures (',{yr^-1},')')),
       x = NULL) +
  MyTheme +
  theme(
    legend.position = "bottom",
    legend.title = element_blank(),
    legend.text = element_text(size=8),
    strip.text.y = element_text(size = 8),
    strip.placement = "outside",
    strip.text.y.right = element_text(size = 8),
    axis.text.x=element_text(size = 8,angle=45, vjust=0.9, hjust=1, margin = margin(t = 0.1, r = 0.1, b = 0, l = 0, unit = "cm")),
    axis.text.y=element_text(size = 8,margin = unit(c(t = 0, r = 0.1, b = 0, l = 0), "cm"))
  )

# Fig3 -------------------------------------------------------------------------

R10power <- plot_grid(g_R10secele2050,g_R10VRE_2050+theme(legend.position='bottom'),
                 ncol=1,labels=c('a','b'),rel_heights=c(1,1),label_size=12)

ggsave(filename='output/figure/Fig3.png',plot=R10power,width=210,height=310,units='mm',dpi=300,bg='white')


# Fig4 -------------------------------------------------------------------------
r10_order <- c("EUROPE","REF_ECON", "NORTH_AM","LATIN_AM","PAC_OECD",
               "AFRICA","MID_EAST","CHINA+","INDIA+","REST_ASIA")

emisec <- tribble(~Variable,~Legend,~Color,
                  'Emi_CO2_Oth','DACCS','thistle2',
                  'Emi_CO2_Ene_Sup','Energy\nsupply','moccasin',
                  'Emi_CO2_Ene_Dem_Ind','Industry','salmon',
                  'Emi_CO2_Ene_Dem_Res_and_Com','Buildings','lightskyblue3',
                  'Emi_CO2_Ene_Dem_Tra','Transport','darkolivegreen2',
                  'Emi_CO2_Ind_Pro','Industrial\nprocess','sandybrown')

res_order <- c("Monthly", "Quarterly", "Half-yearly")
fam_order <- c("2C", "1.5C", "1.5Cdec")

df_emi <- df_r %>%
  filter(Scenario != "Baseline",
         Variable %in% emisec$Variable,
         Year == 2050) %>%
  mutate(Variable=factor(Variable,levels=rev(emisec$Variable)))%>%
  filter(!is.na(Res), !is.na(Fam)) %>%
  mutate(
    R10         = factor(R10, levels = r10_order),
    Res         = factor(Res, levels = res_order),
    Fam         = factor(Fam, levels = fam_order))

df_emitot <- df_r %>%
  filter(Scenario != "Baseline",
         Variable == "Emi_CO2",,
         Year == 2050) %>%
  filter(!is.na(Res), !is.na(Fam)) %>%
  mutate(
    R10         = factor(R10, levels = r10_order),
    Res         = factor(Res, levels = res_order),
    Fam         = factor(Fam, levels = fam_order))

plt <- set_plot(emisec)

g_emi_2050 <- ggplot() +
  geom_bar(data = df_emi,aes(x = Res, y = Value/1000, fill = Variable), stat = "identity") +
  scale_fill_manual(values = plt$Color, labels = plt$Legend, name = "Variable") +
  guides(fill = guide_legend(nrow = 1)) +
  ggh4x::facet_grid2(
    rows   = vars(Fam),
    cols   = vars(R10),
    scales = "free_y",  
    independent = "y",  
    axes   = "x",       
    labeller = labeller(R10 = labeller_R10)
  ) +
  scale_x_discrete(limits = c("Monthly","Quarterly","Half-yearly"), drop = FALSE) +
  labs(y = expression(paste(CO[2], " emissions (Gt ", CO[2], " ", yr^-1, ")")), x = NULL) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey10", linewidth = 0.5) +
  MyTheme +
  theme(
    legend.position = "bottom",
    legend.title = element_blank(),
    legend.text = element_text(size=8),
    strip.text.y = element_text(size = 8),
    strip.placement = "outside",  
    strip.text.y.right = element_text(size = 8),  
    axis.text.x=element_text(size = 8,angle=45, vjust=0.9, hjust=1, margin = margin(t = 0.1, r = 0.1, b = 0, l = 0, unit = "cm")),
    axis.text.y=element_text(size = 8,margin = unit(c(t = 0, r = 0.1, b = 0, l = 0), "cm"))
  )

ggsave('output/figure/Fig4.png', 
       plot=g_emi_2050,width=220,height=150,units='mm',dpi=300,bg='white')


# A1 Cost assumptions of technologies-------------------------------------------------------------------------

capcos <- tribble(~Variable,~Device,
                  'Cap_Cos_Ele_SolarPV','Solar PV',
                  'Cap_Cos_Ele_Win_Ons','Wind onshore',
                  'Cap_Cos_Hyd_Ele','Electrolysis')

line_color <- c('indianred1', 'deepskyblue1', 'darkgoldenrod1')
shape <- c(16,4,17)

Month<-c('2C_Monthly','1.5C_Monthly','1.5Cdec_Monthly')
Fam_level<-c('2C','1.5C','1.5Cdec')

g_capcos <- df_all %>%
  filter(Region=='XE15',
         Variable %in% capcos$Variable,
         Scenario %in% Month,
         Year %in% 2020:2050) %>% 
  inner_join(capcos, by = "Variable") %>%  
  mutate(
    Fam    = factor(Fam, levels = Fam_level),
    Device = factor(Device, levels = unique(capcos$Device))
  ) %>% 
  ggplot() +
  geom_path(aes(x=Year, y=Value, color=Fam, group=Fam), size=0.4) +
  geom_point(aes(x=Year, y=Value, color=Fam, shape=Fam),
             size=2.5, stroke=0.7) +
  scale_color_manual(
    values = line_color,
    breaks = Fam_level,
    labels = Fam_level,
    name   = NULL
  ) +
  scale_shape_manual(
    values = shape,
    breaks = Fam_level,
    labels = Fam_level,
    name   = NULL
  ) +
  scale_x_continuous(limits=c(2020,2050), breaks=seq(2020,2050,by=10)) +
  scale_y_continuous(limits=c(0, NA)) +
  facet_wrap(vars(Device), nrow=1, scales='free_y') +
  labs(x=NULL, y='Capital cost') +
  MyTheme +
  theme(
    legend.position='right',
    strip.background=element_blank(),
    axis.text.x=element_text(angle=45, hjust=1)
  ) +
  guides(
    color = guide_legend(override.aes = list(linetype = 1)) 
  )

ggsave(filename='output/SI/FigA1.png',plot=g_capcos,width=180,height=85,units='mm',dpi=300)


# A2 power generation -----------------------------------------------------

df_secele <- df %>% 
  filter(Scenario != "Baseline") %>% 
  filter(Variable %in% secele$Variable, Year %in% year_all) %>% 
  mutate(Variable = factor(Variable, levels = rev(secele$Variable)))

plt <- set_plot(secele)

g_secele_all <- df_secele %>% 
  mutate(Scenario = factor(Scenario, levels = timeslice)) %>% 
  ggplot() +
  geom_bar(aes(x = Year, y = Value, fill = Variable), stat = "identity") +
  scale_fill_manual(values = plt$Color, labels = plt$Legend, name = "") +
  facet_wrap(vars(Scenario),nrow=3) +
  coord_cartesian(ylim = c(0, 600)) +
  guides(fill = guide_legend(reverse = FALSE)) +
  labs(y = expression(paste("Power generation (EJ ", {yr^-1}, ")"))) +
  MyTheme +
  theme(legend.position    = "bottom")


ggsave(filename='output/SI/FigA2.png',plot=g_secele_all,width=180,height=180,units='mm',dpi=300,bg='white')


# A3 Final energy demand-----------------------------------------------------

df_finene <- df %>% 
  filter(Scenario != "Baseline") %>% 
  filter(Variable %in% finene$Variable, Year %in% year_all) %>% 
  mutate(Variable = factor(Variable, levels = rev(finene$Variable)))

plt <- set_plot(finene)

g_finene_all <- df_finene %>% 
  mutate(Scenario = factor(Scenario, levels = timeslice)) %>% 
  ggplot() +
  geom_bar(aes(x = Year, y = Value, fill = Variable), stat = "identity") +
  scale_fill_manual(values = plt$Color, labels = plt$Legend, name = "") +
  facet_wrap(vars(Scenario),nrow=3) +
  coord_cartesian(ylim = c(0, 600)) +
  guides(fill = guide_legend(reverse = FALSE)) +
  labs(y = expression(paste("Final energy (EJ ", {yr^-1}, ")"))) +
  MyTheme +
  theme(legend.position    = "bottom")


ggsave(filename='output/SI/FigA3.png',plot=g_finene_all,width=180,height=200,units='mm',dpi=300,bg='white')


# A4 Sectoral final energy demand-----------------------------------------------------

df_sector <- list(name1=c('Ind','Res_and_Com','Tra'),name2=list(finind,finbui,fintra))

df_finsec <- map2(df_sector$name1, df_sector$name2,~{
  df %>% filter(Variable%in%..2$Variable) %>% 
    mutate(Sb=case_when(
      str_detect(Variable,'Oil') ~ 'Fin_Ene_Liq_Oil',
      str_detect(Variable,'Coa') ~ 'Fin_Ene_SolidsCoa',
      str_detect(Variable,'Gas') ~ 'Fin_Ene_Gas',
      str_detect(Variable,'Bio') ~ 'Biomass',
      str_detect(Variable,'Solar') ~ 'Fin_Ene_Solar',
      str_detect(Variable,'Ele') ~ 'Fin_Ene_Ele',
      str_detect(Variable,'Heat') ~ 'Fin_Ene_Heat',
      str_detect(Variable,'Hyd')&!str_detect(Variable,'syn') ~ 'Fin_Ene_Hyd',
      str_detect(Variable,'syn') ~ 'Fin_Ene_Liq_Hyd_syn'
    )) %>% 
    group_by(Scenario,Sb,Year) %>% 
    summarise(Value=sum(Value)) %>% 
    rename(Variable=Sb) %>%
    mutate(Variable=factor(Variable,levels=rev(c('Fin_Ene_Liq_Oil',
                                                 'Fin_Ene_SolidsCoa',
                                                 'Fin_Ene_Gas',
                                                 'Biomass',
                                                 'Fin_Ene_Solar',
                                                 'Fin_Ene_Ele',
                                                 'Fin_Ene_Heat',
                                                 'Fin_Ene_Hyd',
                                                 'Fin_Ene_Liq_Hyd_syn')))) %>% 
    mutate(Se=..1)
}) %>%
  bind_rows() %>% 
  mutate(Se=case_when(
    Se=='Ind'~'Industry',
    Se=='Tra'~'Transport',
    TRUE~'Buildings'
  )) %>%
  mutate(Se=factor(Se,levels=c('Industry','Buildings','Transport')))

plt <- set_plot(finene %>% 
                  filter(!str_detect(Variable,'Bio')) %>%
                  bind_rows(data.frame(Variable='Biomass',Legend='Biomass',Color='darkolivegreen2')) %>%
                  mutate(Variable=as.character(Variable)) %>% 
                  mutate(Legend=factor(Legend,levels=c('Oil',
                                                       'Coal',
                                                       'Gas',
                                                       'Biomass',
                                                       'Solar',
                                                       'Electricity',
                                                       'Heat',
                                                       'Hydrogen',
                                                       'Synfuel'))) %>% 
                  arrange(Legend))

df_finsec <- df_finsec %>%
  filter(Scenario %in% timeslice) %>%
  mutate(Scenario = factor(Scenario, levels = c(timeslice))) %>%
  filter(Year == 2050) 

g_finbar<-df_finsec %>% 
  ggplot() +
  geom_bar(aes(x=Scenario,y=Value,fill=Variable),stat='identity') +
  guides(fill = guide_legend(reverse = FALSE, nrow =1)) +  
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
  scale_fill_manual(values=plt$Color,labels=plt$Legend,name='') +
  labs(y=expression(paste('Final energy difference (EJ ',{yr^-1},')'))) +
  facet_wrap(vars(Se),nrow=1)+
  MyTheme +
  theme(    legend.position      = 'bottom')

ggsave(
  filename = "output/SI/FigA4.png",
  plot = g_finbar,
  width = 180, height = 120, units = "mm", dpi = 300, bg = "white"
)


# A5 Hydrogen generation-----------------------------------------------------

df_sechyd <- df %>% 
  filter(Scenario != "Baseline") %>% 
  filter(Variable %in% sechyd$Variable, Year %in% year_all) %>% 
  mutate(Variable = factor(Variable, levels = rev(sechyd$Variable)))

plt <- set_plot(sechyd)

g_sechyd_all <- df_sechyd %>% 
  mutate(Scenario = factor(Scenario, levels = timeslice)) %>% 
  ggplot() +
  geom_bar(aes(x = Year, y = Value, fill = Variable), stat = "identity") +
  scale_fill_manual(values = plt$Color, labels = plt$Legend, name = "") +
  facet_wrap(vars(Scenario),nrow=3) +
  coord_cartesian(ylim = c(0, 150)) +
  guides(fill = guide_legend(reverse = FALSE)) +
  labs(y = expression(paste("Hydrogen generation (EJ ", {yr^-1}, ")"))) +
  MyTheme +
  theme(legend.position    = "bottom")


ggsave(filename='output/SI/FigA5.png',plot=g_sechyd_all,width=180,height=200,units='mm',dpi=300,bg='white')


# A6 CO2 emission ---------------------------------------------------------

df_emisec <- df %>% 
  filter(Scenario != "Baseline") %>% 
  filter(Variable %in% emisec$Variable, Year %in% year_all) %>% 
  mutate(Variable = factor(Variable, levels = rev(emisec$Variable)))

plt <- set_plot(emisec)

g_emisec_all <- df_emisec %>% 
  mutate(Scenario = factor(Scenario, levels = timeslice)) %>% 
  ggplot() +
  geom_bar(aes(x = Year, y = Value/1000, fill = Variable), stat = "identity") +
  scale_fill_manual(values = plt$Color, labels = plt$Legend, name = "") +
  facet_wrap(vars(Scenario),nrow=3) +
  coord_cartesian(ylim = c(-10, 40)) +
  geom_hline(yintercept = 0, linetype = 'dashed', color = 'grey10', linewidth = 1) +
  guides(fill = guide_legend(reverse = FALSE,nrow=1)) +
  labs(y = expression(paste(CO[2], " emissions (Gt ", CO[2], " ", yr^-1, ")")), x = NULL) +
  MyTheme +
  theme(legend.position    = "bottom")

ggsave(filename='output/SI/FigA6.png',plot=g_emisec_all,width=180,height=200,units='mm',dpi=300,bg='white')

# A7 Additional Investment-------------------------------------------------------------------------

df_investment<- df %>%
  filter(Variable %in% invadd$Variable,
         Scenario %in% timeslice,
         Year > 2020) %>%
  mutate(
    discount = 0.95^(Year - 2023),
    Value = Value * discount * 5 / 1000
  ) %>%
  group_by(Scenario, Variable) %>%
  summarise(Value = sum(Value), .groups = "drop") %>%
  mutate(
    Scenario = factor(Scenario, levels = timeslice),
    Variable = factor(Variable, levels = invadd$Variable)
  ) 

g_totcos_2021_50 <- df_investment%>%
  ggplot() +
  geom_bar(
    aes(x = Scenario, y = Value, fill = Variable),
    stat = "identity",
    position = "stack"
  ) +
  scale_fill_manual(
    values = setNames(invadd$Color, invadd$Variable),
    labels = setNames(invadd$Legend, invadd$Variable),
    name = ""
  ) +
  labs(
    x = NULL,
    y = "Cumulative additional investment (trillion US$)"
  ) +
  MyTheme +
  theme(legend.position = "right")

ggsave(filename='output/SI/FigA7.png',plot=g_totcos_2021_50,width=130,height=130,units='mm',dpi=300,bg='white')

# A8 Carbon price-------------------------------------------------------------------------
line_color <- c('indianred1','indianred2','indianred3',
                'deepskyblue1','deepskyblue2','deepskyblue3', 
                'darkgoldenrod1', 'darkgoldenrod2', 'darkgoldenrod3')
shape <- c(16,4,17,16,4,17,16,4,17)

g_prccar <- df %>%
  filter(Variable=='Prc_Car',Scenario!='Baseline',Year%in%c(2020:2050)) %>% 
  mutate(Scenario = factor(Scenario, levels = timeslice)) %>% 
  ggplot() +
  geom_path(aes(x=Year,y=Value,color=Scenario,group=Scenario),size=0.4) +
  geom_point(size=2.5,aes(x=Year,y=Value,color=Scenario,shape=Scenario),stroke=0.7) +
  scale_color_manual(values=line_color) +
  scale_shape_manual(values=shape) +
  labs(y=expression(paste('Carbon prices (US$2010 t-',{CO[2]^-1},')'))) +
  MyTheme +
  theme(axis.title.y = element_text(size=15),
        legend.title=element_blank(),
        legend.text=element_text(size=10)) 

ggsave(filename='output/SI/FigA8.png',plot=g_prccar,width=150,height=130,units='mm',dpi=300,bg='white')


# A9 R10 Final energy-------------------------------------------------------------------------

fam_order <- c("2C", "1.5C", "1.5Cdec")
r10_order <- c("EUROPE","REF_ECON", "NORTH_AM","LATIN_AM","PAC_OECD",
               "AFRICA","MID_EAST","CHINA+","INDIA+","REST_ASIA")

df_fineneR10<- df_r %>% 
  filter(Variable %in% finene$Variable, Year%in%year_all) %>% 
  mutate(Sb=case_when(
    str_detect(Variable,'Bio') ~ 'Biomass',
    TRUE ~ Variable
  )) %>% 
  group_by(Scenario,Sb,Year,R10,Fam,Res) %>% 
  summarise(Value=sum(Value)) %>% 
  ungroup() %>% 
  rename(Variable=Sb) %>% 
  mutate(Variable=factor(Variable,levels=rev(c('Fin_Ene_Liq_Oil',
                                               'Fin_Ene_SolidsCoa',
                                               'Fin_Ene_Gas',
                                               'Biomass',
                                               'Fin_Ene_Solar',
                                               'Fin_Ene_Ele',
                                               'Fin_Ene_Heat',
                                               'Fin_Ene_Hyd',
                                               'Fin_Ene_Liq_Hyd_syn')))) %>% 
  filter(Scenario != "Baseline", Year == 2050) %>%
  # mutate(Variable=factor(Variable,levels=rev(finene$Variable)))%>%
  filter(!is.na(Res), !is.na(Fam)) %>%
  mutate(
    R10         = factor(R10, levels = r10_order),
    Res         = factor(Res, levels = res_order),
    Fam         = factor(Fam, levels = fam_order))

plt <- set_plot(finene %>% 
                  filter(!str_detect(Variable,'Bio')) %>%
                  bind_rows(data.frame(Variable='Biomass',Legend='Biomass',Color='darkolivegreen2')) %>%
                  mutate(Variable=as.character(Variable)) %>% 
                  mutate(Legend=factor(Legend,levels=c('Oil',
                                                       'Coal',
                                                       'Gas',
                                                       'Biomass',
                                                       'Solar',
                                                       'Electricity',
                                                       'Heat',
                                                       'Hydrogen',
                                                       'Synfuel'))) %>% 
                  arrange(Legend))



g_fineneR10 <- ggplot(df_fineneR10) +
  geom_bar(aes(x = Res, y = Value, fill = Variable), stat = "identity") +
  scale_fill_manual(values = plt$Color, labels = plt$Legend, name = "Variable") +
  guides(fill = guide_legend(nrow = 1)) +
  ggh4x::facet_grid2(
    rows   = vars(Fam),
    cols   = vars(R10),
    scales = "free_y",   
    independent = "y",  
    axes   = "x",    
    labeller = labeller(R10 = labeller_R10)
  ) +
  scale_x_discrete(limits = c("Monthly","Quarterly","Half-yearly"), drop = FALSE) +
  labs(y=expression(paste('Final energy (EJ ',{yr^-1},')'))) +
  MyTheme +
  theme(
    legend.position = "bottom",
    legend.title = element_blank(),
    legend.text = element_text(size=8),
    strip.text.y = element_text(size = 8),
    strip.placement = "outside",  
    strip.text.y.right = element_text(size = 8),  
    axis.text.x=element_text(size = 8,angle=45, vjust=0.9, hjust=1, margin = unit(c(t = 0.1, r = 0.1, b = 0, l = 0), "cm")),
    axis.text.y=element_text(size = 8,margin = unit(c(t = 0, r = 0.1, b = 0, l = 0), "cm"))
  )

ggsave(paste0('output/SI/FigA9.png'),
       plot=g_fineneR10,width=200,height=150,units='mm',dpi=300,bg='white')
