#  INterventions

test1 <- data_all %>% 
  select(aid, Int_cat) %>% 
  filter(Int_cat!="" & !is.na(Int_cat)) %>%  # just in case
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
  
  test_list <- expand.grid(int_list$Int_cat_orig,int_list$Int_cat_orig)  %>% 
    rename(Int1=Var1,Int2=Var2)

  test2 <- test1 %>% 
    full_join(test_list) %>%   # inserts missing combinations
    mutate(n=replace_na(n,0)) %>%
    ungroup() %>% 
    mutate( Int1=factor( Int1,levels=mixedsort(int_list$Int_cat_orig))  # set int list in right order
 )
  ggplot(data=test2, aes(x=Int1,y=reorder(Int2, desc(Int2)),fill=n)) +
    geom_tile(color="gray90",size=0.1) +
    geom_text(aes(label=n),show.legend = F) +
    scale_fill_gradient2(low="#f7fbff",high="#2171b5",name="# Cases",na.value="gray90", limits=c(0,max(io_counts$n))) +
    coord_equal() +
    # theme_tufte(base_family="Helvetica") +	# having issues with font, font colour in windows...
    theme(axis.ticks=element_line(size=0.4)) +
    theme(axis.text=element_text(size=9)) +
    theme(legend.title=element_text(size=10)) +
    theme(legend.text=element_text(size=10)) +
    theme(legend.title.align=1) +
    theme(legend.position="bottom") +
    theme(legend.key.size=unit(1, "cm")) +
    theme(legend.key.width=unit(1, "cm")) +
    #  labs(x="Conservation Intervention", y="Outcome", title ="All ecosystems") +
    theme(axis.text.x = element_text(angle=45,hjust=0,vjust=1,size=9)) +
    scale_x_discrete(position = "top") +
    scale_y_discrete(position = "right") 
  ggsave(paste0(plotdir,today.date,'_multiple_int_map.png'),width = 8,height = 8)
  
  
  # Outcomes
  test1 <- data_all %>% 
    select(aid, Outcome_cat) %>% 
    filter(Outcome_cat!="" & !is.na(Outcome_cat)) %>%  # just in case
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
  
  test_list <- expand.grid(out_list$Outcome_cat_orig,out_list$Outcome_cat_orig)  %>% 
    rename(Out1=Var1,Out2=Var2)
  
  test2 <- test1 %>% 
    full_join(test_list) %>%   # inserts missing combinations
    mutate(n=replace_na(n,0)) %>%
    ungroup() %>% 
    mutate(Out1=factor(Out1,levels=mixedsort(Out_list$Outcome_cat_orig))  # set Out list in right order
    )
  ggplot(data=test2, aes(x=Out1,y=reorder(Out2, desc(Out2)),fill=n)) +
    geom_tile(color="gray90",size=0.1) +
    geom_text(aes(label=n),show.legend = F) +
    scale_fill_gradient2(low="#f7fbff",high="#2171b5",name="# Cases",na.value="gray90", limits=c(0,max(io_counts$n))) +
    coord_equal() +
    # theme_tufte(base_family="Helvetica") +	# having issues with font, font colour in windows...
    theme(axis.ticks=element_line(size=0.4)) +
    theme(axis.text=element_text(size=9)) +
    theme(legend.title=element_text(size=10)) +
    theme(legend.text=element_text(size=10)) +
    theme(legend.title.align=1) +
    theme(legend.position="bottom") +
    theme(legend.key.size=unit(1, "cm")) +
    theme(legend.key.width=unit(1, "cm")) +
    #  labs(x="Conservation Intervention", y="Outcome", title ="All ecosystems") +
    theme(axis.text.x = element_text(angle=45,hjust=0,vjust=1,size=9)) +
    scale_x_discrete(position = "top") +
    scale_y_discrete(position = "right") 
  
  ggsave(paste0(plotdir,today.date,'_multiple_out_map.png'),width = 8,height = 8)
  