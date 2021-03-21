#  INterventions

int.x.count <- data_all %>% 
  select(aid, Int_cat) %>% 
  filter(Int_cat!="" & !is.na(Int_cat)) %>%  # just in case
  arrange(aid,Int_cat) %>% 
  distinct() %>%
  group_by(aid) %>%
  filter(n() > 1) %>%
  split(.$aid) %>%
  map(., 2) %>%
  map(~combn(.x, m = 2)) %>%
  map(~t(.x)) %>%
  map_dfr(as_tibble) %>% 
  group_by(V1,V2) %>% 
  count() %>% 
  rename(Int1=V1,Int2=V2)
  
  int.x.list <- expand.grid(int_list$int_val,int_list$int_val)  %>% 
    rename(Int1=Var1,Int2=Var2)

  int.x.count.full <- int.x.count %>% 
    full_join(int.x.list) %>%   # inserts missing combinations
  #  mutate(n=replace_na(n,0)) %>%
    ungroup() %>% 
    mutate( Int1=factor( Int1,levels=mixedsort(int_list$int_val))  # set int list in right order
 )
  # ggplot(data=int.x.count.full, aes(x=Int2,y=reorder(Int1, desc(Int1)))) +
  #   geom_tile(aes(fill=n),color="gray90",size=0.1) +
  #   geom_text(aes(label=n),show.legend = F) +
  #   scale_fill_gradient2(low="#f7fbff",high="#2171b5",name="# Cases",na.value="gray90", limits=c(0,max(int.x.count.full$n))) +
  #   coord_equal() +
  #   # theme_tufte(base_family="Helvetica") +	# having issues with font, font colour in windows...
  #   theme(axis.ticks=element_line(size=0.4)) +
  #   theme(axis.text=element_text(size=9)) +
  #   theme(legend.title=element_text(size=10)) +
  #   theme(legend.text=element_text(size=10)) +
  #   theme(legend.title.align=1) +
  #   theme(legend.position="bottom") +
  #   theme(legend.key.size=unit(1, "cm")) +
  #   theme(legend.key.width=unit(1, "cm")) +
  #   #  labs(x="Conservation Intervention", y="Outcome", title ="All ecosystems") +
  #   theme(axis.text.x = element_text(angle=45,hjust=0,vjust=1,size=9)) +
  #   scale_x_discrete(position = "top") +
  #   scale_y_discrete(position = "right") 
  ggsave(paste0(plotdir,today.date,'_multiple_int_map.png'),width = 8,height = 8)

  
  ggplot(data=int.x.count.full, aes(x=Int2,y=reorder(Int1, desc(Int1)))) +
    theme_bw() +
    geom_tile(aes(fill = n), color='white',size=0.1) +
    scale_fill_gradient(low = '#f7fbff', high = '#2171b5', space = 'Lab',na.value="white",limits=c(0,max(int.x.count.full$n))) +
    geom_text(aes(label=n),show.legend = T) +
    theme(axis.text.x=element_text(angle=90),
          axis.ticks=element_blank(),
          axis.line=element_blank(),
          panel.border=element_blank(),
          panel.grid.major=element_line(color='#eeeeee'))+
    theme(axis.text.x = element_text(angle=45,hjust=0,vjust=1,size=9)) +
    scale_x_discrete(position = "top") +
    scale_y_discrete(position = "right") +
    labs(x="Intervention 1", y="Intervention 2")
  ggsave(paste0(plotdir,today.date,'_multiple_int_map.png'),width = 8,height = 8)
  
  
  
# Sub-interventions
sub.int.x.count <- data_all %>% 
    select(aid, Intervention.subcategory) %>% 
    filter(Intervention.subcategory!="" & !is.na(Intervention.subcategory)) %>%  # just in case
    arrange(aid,Intervention.subcategory) %>% 
    distinct() %>%
    group_by(aid) %>%
    filter(n() > 1) %>%
    split(.$aid) %>%
    map(., 2) %>%
    map(~combn(.x, m = 2)) %>%
    map(~t(.x)) %>%
    map_dfr(as_tibble) %>% 
    group_by(V1,V2) %>% 
    count() %>% 
    rename(Int1=V1,Int2=V2)
  
sub.int.x.list <- expand.grid(int_sub_list$int_val,int_sub_list$int_val)  %>% 
    rename(Int1=Var1,Int2=Var2)
  
sub.int.x.count.full <- sub.int.x.count %>% 
    full_join(sub.int.x.list) %>%   # inserts missing combinations
   # mutate(n=replace_na(n,0)) %>%
    ungroup() %>% 
    mutate( Int1=factor( Int1,levels=mixedsort(int_sub_list$int_val))  # set int list in right order
    )
  
ggplot(data=sub.int.x.count.full, aes(x=Int2,y=reorder(Int1, desc(Int1)),fill=n)) +
  theme_bw() +
  geom_tile(aes(fill = n), color='white',size=0.1) +
  scale_fill_gradient(low = '#f7fbff', high = '#2171b5', space = 'Lab',na.value="white",limits=c(0,max(sub.int.x.count.full$n))) +
  geom_text(aes(label=n),show.legend = T) +
  theme(axis.text.x=element_text(angle=90),
        axis.ticks=element_blank(),
        axis.line=element_blank(),
        panel.border=element_blank(),
        panel.grid.major=element_line(color='#eeeeee'))+
  theme(axis.text.x = element_text(angle=45,hjust=0,vjust=1,size=9)) +
  scale_x_discrete(position = "top") +
  scale_y_discrete(position = "right") +
  labs(x="Sub-Intervention 1", y="Sub-Intervention 2")
  ggsave(paste0(plotdir,today.date,'_multiple_sub_int_map.png'),width = 12,height = 12)
  
  
  # Outcomes
  out.x.count <- data_all %>% 
    select(aid, Outcome_cat) %>% 
    filter(Outcome_cat!="" & !is.na(Outcome_cat)) %>%  # just in case
    arrange(aid,Outcome_cat) %>% 
    distinct() %>%
    group_by(aid) %>%
    filter(n() > 1) %>%
    split(.$aid) %>%
    map(., 2) %>%
    map(~combn(.x, m = 2)) %>%
    map(~t(.x)) %>%
    map_dfr(as_tibble) %>% 
    group_by(V1,V2) %>% 
    count() %>% 
    rename(Out1=V1,Out2=V2)
  
  out.x.list <- expand.grid(out_list$out_val,out_list$out_val)  %>% 
    rename(Out1=Var1,Out2=Var2)
  
  out.x.count.full <- out.x.count %>% 
    full_join(out.x.list) %>%   # inserts missing combinations
  #  mutate(n=replace_na(n,0)) %>%
    ungroup() %>% 
    mutate(Out1=factor(Out1,levels=mixedsort(out_list$out_val))  # set Out list in right order
    )
  ggplot(data=out.x.count.full, aes(x=Out2,y=reorder(Out1, desc(Out1)),fill=n)) +
    theme_bw() +
    geom_tile(aes(fill = n), color='white',size=0.1) +
    scale_fill_gradient(low = '#f7fbff', high = '#2171b5', space = 'Lab',na.value="white",limits=c(0,max(sub.int.x.count.full$n))) +
    geom_text(aes(label=n),show.legend = T) +
    theme(axis.text.x=element_text(angle=90),
          axis.ticks=element_blank(),
          axis.line=element_blank(),
          panel.border=element_blank(),
          panel.grid.major=element_line(color='#eeeeee'))+
    theme(axis.text.x = element_text(angle=45,hjust=0,vjust=1,size=9)) +
    scale_x_discrete(position = "top") +
    scale_y_discrete(position = "right") +
    labs(x="Outcome 1", y="Outcome 2")
  
  ggsave(paste0(plotdir,today.date,'_multiple_out_map.png'),width = 8,height = 8)
  
  
  # sub-outcomes
  sub.out.x.count <- data_all %>% 
    select(aid, Outcome.subcategory) %>% 
    filter(Outcome.subcategory!="" & !is.na(Outcome.subcategory)) %>%  # just in case
    arrange(aid,Outcome.subcategory) %>% 
    distinct() %>%
    group_by(aid) %>%
    filter(n() > 1) %>%
    split(.$aid) %>%
    map(., 2) %>%
    map(~combn(.x, m = 2)) %>%
    map(~t(.x)) %>%
    map_dfr(as_tibble) %>% 
    group_by(V1,V2) %>% 
    count() %>% 
    rename(Out1=V1,Out2=V2)
  
  sub.out.x.list <- expand.grid(out_sub_list$out_val,out_sub_list$out_val)  %>% 
    rename(Out1=Var1,Out2=Var2)
  
  sub.out.x.count.full <- sub.out.x.count %>% 
    full_join(sub.out.x.list) %>%   # inserts missing combinations
  #  mutate(n=replace_na(n,0)) %>%
    ungroup() %>% 
    mutate(Out1=factor(Out1,levels=mixedsort(out_sub_list$out_val))  # set Out list in right order
    )
  
  
  ggplot(data=sub.out.x.count.full, aes(x=Out2,y=reorder(Out1, desc(Out1)),fill=n)) +
    theme_bw() +
    geom_tile(aes(fill = n), color='white',size=0.1) +
    scale_fill_gradient(low = '#f7fbff', high = '#2171b5', space = 'Lab',na.value="white",limits=c(0,max(sub.int.x.count.full$n))) +
    geom_text(aes(label=n),show.legend = T) +
    theme(axis.text.x=element_text(angle=90),
          axis.ticks=element_blank(),
          axis.line=element_blank(),
          panel.border=element_blank(),
          panel.grid.major=element_line(color='#eeeeee'))+
    theme(axis.text.x = element_text(angle=45,hjust=0,vjust=1,size=9)) +
    scale_x_discrete(position = "top") +
    scale_y_discrete(position = "right") +
    labs(x="Outcome 1", y="Outcome 2") 
  
  ggsave(paste0(plotdir,today.date,'_multiple_out_map.png'),width = 18,height = 18)
  
# Interdisciplinary sub outcomes
  
  sub.out.x.count.intdisc <- sub.out.x.count.full %>% 
    filter(str_detect(Out1, "^[1234]") & str_detect(Out2, "^[4567]") )
  unique(sub.out.x.count.intdisc$Out1)
  unique(sub.out.x.count.intdisc$Out2)
  
  ggplot(data=sub.out.x.count.intdisc, aes(x=Out1,y=reorder(Out2, desc(Out2)),fill=n)) +
    theme_bw() +
    geom_tile(aes(fill = n), color='white',size=0.1) +
    scale_fill_gradient(low = '#f7fbff', high = '#2171b5', space = 'Lab',na.value="white",limits=c(0,max(sub.int.x.count.full$n))) +
    geom_text(aes(label=n),show.legend = T) +
    theme(axis.text.x=element_text(angle=90),
          axis.ticks=element_blank(),
          axis.line=element_blank(),
          panel.border=element_blank(),
          panel.grid.major=element_line(color='#eeeeee'))+
    theme(axis.text.x = element_text(angle=45,hjust=0,vjust=1,size=9)) +
    scale_x_discrete(position = "top") +
    scale_y_discrete(position = "right") +
    labs(x="Ecological + Ecosystem services", y= "Social + Ecosystem services")
  
ggsave(paste0(plotdir,today.date,'interdisc_multiple_out_map.png'),width = 11,height = 8)
  
  