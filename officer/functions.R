
check_template<-function(my_pres, my_pres_layout, tmplt_name, valid_types, output_filename = 'output.pptx') {
  for (i in 1:length(my_pres_layout)) {
    v_layout<-my_pres_layout[i]
    props<-layout_properties(my_pres, layout = v_layout, master=tmplt_name)
    my_pres %<>% 
      add_slide(layout = v_layout, master = tmplt_name)
    props$rnames<-row.names(props)
    props %<>%
      group_by(type) %>% 
      arrange(rnames) %>% 
      mutate(occurences = n(),
             rn = row_number()) %>% 
      ungroup() %>% 
      mutate(idx =  str_extract(pattern = '[0-9]+', string = ph),
             id = if_else(occurences>1, rn, NA_integer_)) %>% 
      select(type, id, ph_label) 
    for (j in 1:nrow(props)){
      v_vals<-props %>% 
        slice(j:j) %>% 
        as.vector
      if(v_vals$type %in% valid_types) {
        if(!is.na(v_vals$id) & v_vals$type=='body') {
          my_pres %<>%  
            ph_with(location = ph_location_type(type = v_vals$type, id = v_vals$id),
                    value = paste('Position: ',v_vals$type,  '\nLayout: ',v_layout,'\nIndex:',v_vals$id))
          
        } else {
          my_pres %<>%  
            ph_with(location = ph_location_type(type = v_vals$type),
                    value = paste('Position: ',v_vals$type,  '\nLayout: ',v_layout))
        }
      }
    }
  }
  print(my_pres, target = output_filename)  
}

insert_slide<-function(my_pres, 
                       layout, 
                       master, 
                       title=NULL, 
                       ctrtitle=NULL,
                       body_text=NULL, 
                       body_index=NULL, 
                       body_object = NULL, 
                       slidenum=NULL){
  my_pres  %<>% 
    add_slide(layout = layout, master = master) 
  
  if(!is.null(title)){
    my_pres %<>% 
      ph_with(location = ph_location_type(type = "title"), value = title) 
  }
  if(!is.null(ctrtitle)){
    my_pres %<>% 
      ph_with(location = ph_location_type(type = "ctrTitle"), value = ctrtitle) 
  }
  if(!is.null(body_text)){
    if(!is.null(body_index)){
      my_pres %<>% 
        ph_with(value = body_text, type = 'cairo', location = ph_location_type(type = "body", index = body_index))      
    } else {
      my_pres %<>% 
        ph_with(value = body_text, type = 'cairo', location = ph_location_type(type = "body"))
    }
  }
  if(!is.null(body_object)){
    if(!is.null(body_index)){
      my_pres %<>% 
        ph_with(value = body_object, type = 'cairo', location = ph_location_type(type = "body", index = body_index))       
    } else {
      my_pres %<>% 
        ph_with(value = body_object, type = 'cairo', location = ph_location_type(type = "body")) 
    }
  }
  if(!is.null(slidenum)){
    my_pres %<>% 
      ph_with(value = slidenum, type = 'cairo', location = ph_location_type(type = "sldNum"))
  }
  return(my_pres)  
}