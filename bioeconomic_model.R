# smith 1969 
#devtools::install_github("tylergrantsmith/purrrgress")

library(tidyverse)
theme_set(theme_light(25))

### yearly change in effort
one_step_effort<-
  function(price,
           catchability,
           biomass_now,
           cost,
           effort_now,
           phi_parameter
  ){
    
    
    return(
      phi_parameter * (
        price*catchability*biomass_now-cost
      )*effort_now
    )
    
  }
### yearly change in biomass
one_step_biomass<-function(
  logistic_growth,
           catchability,
           biomass_now,
           effort_now,
           carrying_capacity){
    
  return(
    logistic_growth*biomass_now*
        (1-biomass_now/carrying_capacity)- 
           effort_now * catchability * 
           biomass_now 
    ) 
    
    
  }


### smith model runner
simulate_many_years<-function(
  price,
  catchability,
  cost,
  phi_parameter,
  carrying_capacity,
  logistic_growth,
  effort_start,
  years_to_simulate=100
){
  #### initialize
  biomass<-rep.int(NA,times=years_to_simulate)
  effort<-rep.int(NA,times=years_to_simulate)
  catches<-rep.int(NA,times=years_to_simulate)
  CPUE<-rep.int(NA,times=years_to_simulate)
  profits<-rep.int(NA,times=years_to_simulate)
  depletion<-rep.int(NA,times=years_to_simulate)
  
  biomass[1]<-carrying_capacity
  effort[1]<-effort_start
  
  
  
  ## run
  for(i in 2:years_to_simulate){
    effort[i]=one_step_effort(
      price,
      catchability,
      biomass[i-1],
      cost,
      effort[i-1],
      phi_parameter
    ) + effort[i-1]
    biomass[i] = biomass[i-1] +
      one_step_biomass(
        logistic_growth,
        catchability,
        biomass[i-1],
        effort[i-1],
        carrying_capacity
      )
    
    ## do not let them go negative
    effort[i] = max(effort[i],0)
    biomass[i] = max(biomass[i],0)
    
    ## do other indicators
    catches[i] = catchability*biomass[i]*effort[i]
    CPUE[i] = catches[i]/effort[i]
    profits[i] = price*catches[i]-cost*effort[i]
    depletion[i] = biomass[i]/carrying_capacity
    
  }
  
  return(
    data.frame(
      effort=effort,
         biomass=biomass,
      depletion=depletion,
      profits=profits,
      CPUE=CPUE,
      catches=catches,
      year = 1:years_to_simulate
      )
  )
}


fao_textbook<-
  simulate_many_years(
  price = 60,
  cost = 30000,
  carrying_capacity = 3500000,
  catchability = 0.0004,
  phi_parameter = 0.000005,
  logistic_growth = 0.36,
  effort_start = 1
)

library(tidyverse)

fao_textbook %>%
  gather("variable","value",-year) %>%
  ggplot()+
  geom_line(aes(y=value,x=year,col=variable),lwd=1) +
  facet_wrap(.~variable,scales="free_y")


##### run a bunch

set.seed(42)

RUNS_TO_MAKE<-1000000
SPREAD_FROM_FAO_NUMBERS<-2

parameters<-
  data.frame(
    price = runif(RUNS_TO_MAKE,30,90),
    cost= runif(RUNS_TO_MAKE,30000/SPREAD_FROM_FAO_NUMBERS,SPREAD_FROM_FAO_NUMBERS*30000),
    carrying_capacity = runif(RUNS_TO_MAKE,3500000/SPREAD_FROM_FAO_NUMBERS,SPREAD_FROM_FAO_NUMBERS*3500000),
    catchability= runif(RUNS_TO_MAKE,0.0004/SPREAD_FROM_FAO_NUMBERS,0.0004*SPREAD_FROM_FAO_NUMBERS),
    phi_parameter= runif(RUNS_TO_MAKE,0.000005/SPREAD_FROM_FAO_NUMBERS,0.000005*SPREAD_FROM_FAO_NUMBERS),
    #not spread the same way, because it is too critical
    logistic_growth= runif(RUNS_TO_MAKE,0.1,0.6),
    effort_start= rep.int(1,times=RUNS_TO_MAKE),
    #don't really know how long this fishery has been around
    years_to_simulate= sample(40:65,size=RUNS_TO_MAKE,replace=TRUE)
  ) 
total_data<-
  purrrgress::pro_pmap_dfr(parameters,simulate_many_years,.id="run") 


total_data %>%
  group_by(run) %>%
  filter(year==max(year)) %>% ungroup() %>%
  gather("variable","value",-year,-run) %>%
  ggplot()+
  geom_histogram(aes(x=value,fill=variable),col="grey")+
  facet_wrap(.~variable,scales="free")



#we know three things:
#1 total catches this year are between 10,000t and 15,000t
#2 profits are NEGATIVE
#2 ten years ago it used to be easier to catch fish

filtered<-
  total_data %>%
  group_by(run) %>%
  arrange(run,year) 


to_filter<-
  filtered %>%
  mutate(old_CPUE=lag(CPUE,n=10)) %>%
  filter(year==max(year)) 


pass_1<-
  to_filter %>%
  filter(catches<=15000 & catches>=10000) %>%
  pull(run)
length(pass_1)

pass_2<-
  to_filter %>%
  filter(profits<0) %>%
  pull(run)
length(pass_2)

pass_3<-
  to_filter %>%
  filter(old_CPUE>CPUE) %>%
  pull(run)
length(pass_3)

pass_all<-
  intersect(pass_1,pass_2) %>%
  intersect(pass_3)
length(pass_all)


### massive!!
pryr::object_size(total_data)
pryr::object_size(to_filter)

small_tot_data <- 
  total_data %>%  
  filter(parse_number(run)<20000) %>% group_by(run) %>%
  mutate(year=year-max(year))%>%
  gather("variable","value",-year,-run)

small_tot_data<-
  small_tot_data %>%
  mutate(variable= factor(
    variable,
    levels=c("biomass","catches","CPUE","depletion","effort","profits"),
    labels=c("Biomass(t)","Catches(t)","CPUE (t/boat)","Depletion","Effort (# boats)","Profits")
  ))



recent<-
  small_tot_data  

prior_only<-recent  %>% filter( year>=-10) %>%
  ggplot()+
  geom_line(aes(y=value,x=year,col=variable,
                group=run),alpha=.01) +
  facet_wrap(.~variable,scales="free_y") +
 # theme_light(20) +
  ggtitle("Prior Only") +
  ylab("") +
  scale_color_discrete(guide=FALSE)


ggsave("./smith_prior_only_plot.png",
       prior_only,
       width=16, height=9, dpi=100)

passed_first<-
  total_data %>%filter(run %in% pass_1)%>% group_by(run) %>%
  mutate(year=year-max(year))%>%
  gather("variable","value",-year,-run) %>% filter( year>=-10)  %>%
  mutate(variable= factor(
    variable,
    levels=c("biomass","catches","CPUE","depletion","effort","profits"),
    labels=c("Biomass(t)","Catches(t)","CPUE (t/boat)","Depletion","Effort (# boats)","Profits")
  ))

knowing_catches<-passed_first %>%
  ggplot()+
  geom_line(aes(y=value,x=year,col=variable,
                group=run),alpha=.01) +
  facet_wrap(.~variable,scales="free_y") +
  theme_light(20) +
  ggtitle("Knowing Catches")+
  ylab("") +
  scale_color_discrete(guide=FALSE)

ggsave("./smith_knowing_catches_plot.png",
       knowing_catches,
       width=16, height=9, dpi=100)

pass_12<-intersect(pass_1,pass_2)
passed_12<-passed_first  %>%filter(run %in% pass_12) 

cpue_catches<-passed_first  %>%
  ggplot()+
  geom_line(aes(y=value,x=year,col=variable,
                group=run),alpha=.001) +
  geom_line(aes(y=value,x=year,col=variable,
                group=run),alpha=.1,
            data =  passed_12) +
  facet_wrap(.~variable,scales="free_y") +
  ggtitle("Knowing catches + negative profits")+
  ylab("") +
  scale_fill_discrete(guide=FALSE)

ggsave("./smith_knowing_catches_cpue_plot.png",
       cpue_catches,
       width=16, height=9, dpi=100)

passed_123<-passed_first  %>%filter(run %in% pass_all) 


all_plot<-passed_first  %>%
  ggplot()+
  geom_line(aes(y=value,x=year,col=variable,
                group=run),alpha=.001) +
  geom_line(aes(y=value,x=year,col=variable,
                group=run),alpha=.1,
            data =  passed_123) +
  facet_wrap(.~variable,scales="free_y") +
  ggtitle("Knowing catches + negative profits + CPUE Drop")+
  ylab("") +
  scale_fill_discrete(guide=FALSE)


ggsave("./smith_knowing_all_plot.png",
       all_plot,
       width=16, height=9, dpi=100)


test<-
  total_data %>%
  group_by(run) %>%
  summarise(great=max(na.omit(catches))) %>%
  filter(rank(desc(great))<1000)

test_pass<- test %>% pull(run)

parameters %>% mutate(run=as.character(row_number())) %>%
  filter(run %in% pass_all) %>%
  ggplot()+
  geom_point(aes(y=carrying_capacity,x=logistic_growth)) +
  xlab("Logistic Growth") +
  ylab("Carrying Capacity(t)") +
  ggtitle("Parameter posterior") -> rkplot

  ggsave("./smith_rkplot.png",
       rkplot,
       width=16, height=9, dpi=100)

correct_parameters<-
  parameters %>% mutate(run=as.character(row_number())) %>%
  filter(run %in% pass_all)

write_csv(correct_parameters,"./smith_correct_parameters.csv")
write_csv(passed_123,"./smith_passed_one_two_three.csv")
write_csv(recent  %>% filter( year>=-10),
          "./smith_prior_runs.csv")

######

passed_123 %>%
  filter(year==0)  %>%
  ggplot()+
  geom_histogram(aes(x=value,fill=variable),col="grey")+
  facet_wrap(.~variable,scales="free") +
  scale_fill_discrete(guide=FALSE) +
  xlab("")+ ylab("Count") +
  ggtitle("Marginal posteriors, state variables") -> state_posterior_plot

ggsave("./smith_posterior_plot.png",
       state_posterior_plot,
       width=16, height=9, dpi=100)




library(latex2exp)
correct_parameters  %>%
  gather("variable","value",
         -run,-years_to_simulate,-effort_start) %>% mutate(
           variable=factor(variable,
                           levels=c("price","cost","carrying_capacity",
                                    "catchability","phi_parameter","logistic_growth"),
                           labels = c(TeX("$p$ : unit revenue"),
                                      TeX("$c$ : yearly boat cost"),
                                      TeX("$K$ : carrying capacity"),
                                      TeX("$q$ : catchability"),
                                      TeX("$\\phi$"),
                                      TeX("$r$ : logistic growth")))
         ) %>%
  ggplot()+
  geom_histogram(aes(x=value,fill=variable),col="grey")+
  facet_wrap(.~variable,scales="free",labeller=label_parsed) +
  scale_fill_discrete(guide=FALSE) +
  xlab("")+ ylab("Count") + 
  scale_x_continuous(guide = guide_axis(n.dodge=2),
                     labels = scales::comma) +
  ggtitle("Marginal posteriors, parameters") -> parameters_posterior_plot


ggsave("./smith_posterior_parameter_plot_2.png",
       parameters_posterior_plot,
       width=16, height=9, dpi=100)

#######
p1<- recent %>% filter(variable=="Depletion") %>% 
  filter(year==0) %>% ggplot(aes(x=value,y=..density..)) + 
  geom_histogram(col="grey",fill="red") +
  ggtitle("Prior, Depletion") +
  scale_y_continuous("Density",limits=c(0,10)) +
  scale_x_continuous(name="Depletion",
                     limits = c(0,1))

p2<-passed_first %>% filter(variable=="Depletion") %>% 
  filter(year==0) %>% ggplot(aes(x=value,y=..density..)) + geom_histogram(col="grey",fill="blue") +
  ggtitle("Catches only, Depletion") +
  scale_y_continuous("Density",limits=c(0,10)) +

  scale_x_continuous(name="Depletion",
                     limits = c(0,1))


p3<-passed_12 %>% filter(variable=="Depletion") %>% 
  filter(year==0) %>% ggplot(aes(x=value,y=..density..)) + 
  geom_histogram(col="grey",fill="orange") +
  ggtitle("Catches+profits, Depletion") +
  scale_y_continuous("Density",limits=c(0,10)) +
  
  scale_x_continuous(name="Depletion",
                     limits = c(0,1))


p4<-passed_123 %>% filter(variable=="Depletion") %>% 
  filter(year==0) %>% ggplot(aes(x=value,y=..density..)) + 
  geom_histogram(col="grey",fill="black") +
  ggtitle("Catches+profits+CPUE, Depletion") +
  scale_y_continuous("Density",limits=c(0,10)) +
  
  scale_x_continuous(name="Depletion",
                     limits = c(0,1))

library(patchwork)
p1+p2+p3+p4

ggsave("./smith_depletion_plots_2.png",
       p1+p2+p3+p4 + plot_annotation(tag_levels = "A"),
       width=16, height=9, dpi=100)
