version
#install.packages("Cairo")
#install.packages('officer') # Install
if (!require("officer")) {
  install.packages('officer')
}
if (!require("magrittr")) {
  install.packages('magrittr')
}
##### For Mac users #####
if (!require("Cairo")) {
  install.packages('Cairo')
}
library('officer') # Load
library('magrittr') 
library('dplyr')
library('stringr')

my_pres <- read_pptx(path = 'template.pptx') 

(my_pres_layout <- layout_summary(my_pres)$layout)
(tmplt_name <- layout_summary(my_pres)$master[1]) 
valid_types<-c('body', 'title', 'ctrTitle', 'subTitle', 'dt', 'ftr', 'sldNum')




text<-"""
Let us choose next layouts and positions:
1. Title Slide, positions:
  a) ctrTitle
  b) ftr
  c) sldNum
2. Title and Content, positions:
  a) body with index = 2
  b) title
  c) sldNum
3. Section Header, positions:
  a) title
  b) sldNum
"""
So, we have 3 different layouts with different position options. Let us create a function.



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

library(ggplot2)
?mtcars


It seems like everything is done now, so we can prepare our short presentation on mtcars dataset. 
Let us create several plots on this, for example:
  1. 1/4 mile time dependence on Gross horse power 
  2. Miles/(US) gallon dependence on Displacement (cubic inches)
  
  plot_1<-ggplot(mtcars)+
    aes(x = hp, y = qsec, size = cyl)+
    geom_point()+
    geom_smooth()+
    theme_minimal()
  
  plot_2<-ggplot(mtcars)+
    aes(x = disp, y = mpg)+
    geom_point()+
    geom_smooth()+
    theme_minimal()
And here we go with our presentation:  
#Step 1, read our template:  
my_pres <- read_pptx(path = 'template.pptx') 
#Step 2, create title slide
my_pres<- insert_slide(my_pres = my_pres, 
                       layout = 'Title Slide', 
                       master = tmplt_name, 
                       ctrtitle = 'Little presentation on Mtcars dataset',
                       slidenum = 1)
#Step 3, create intro slide
my_pres<- insert_slide(my_pres = my_pres, 
                       layout = 'Section Header', 
                       master = tmplt_name, 
                       title = 'Everything is data...',
                       slidenum = 2)
#Step 4, create slide with plot 1
my_pres<- insert_slide(my_pres = my_pres, 
                       layout = 'Title and Content', 
                       master = tmplt_name, 
                       title = '1/4 mile time dependence on Gross horse power',
                       body_object = plot_1,
                       body_index = 2,
                       slidenum = 3)
#Step 5, create slide with plot 2
my_pres<- insert_slide(my_pres = my_pres, 
                       layout = 'Title and Content', 
                       master = tmplt_name, 
                       title = 'Miles/(US) gallon dependence on Displacement (cubic inches)',
                       body_object = plot_2,
                       body_index = 2,
                       slidenum = 4)
print(my_pres, target = 'output.pptx')  


insert_slide_full<-function(my_pres, layout, master, title, object = NULL, txt = NULL){
  my_pres  %<>% 
    add_slide(layout = "CUSTOM_1_2", master = mytmp) %>% 
    ph_with(value = object, type = 'cairo', location = ph_location_type(type = "body")) %>% 
    ph_with(location = ph_location_type(type = "title"), value = title) 
  return(my_pres)  
}

insert_slide_desc<-function(my_pres, layout, master, title, description, object, mytmp="OLX template") {
  my_pres  %<>% 
    add_slide(layout = "CUSTOM_1_1", master = mytmp) %>% 
    ph_with(value = object, type = 'cairo', location = ph_location_type(type = "body")) %>% 
    ph_with(location = ph_location_type(type = "title", id = 1), value = title) %>% 
    ph_with(location = ph_location_type(type = "title", id = 2), value = description) 
  return(my_pres)
}

my_pres <- read_pptx(path = 'presa.pptx') 

#Slide 1
my_pres  %<>% 
  add_slide(layout = "Title <red; 2 pane>", master = mytmp) %>% 
  ph_with(location = ph_location_type(type = "body"), value = ' ') %>% 
  ph_with(location = ph_location_type(type = "title"), value = 'True story')

#Slide 2
my_pres  %<>% 
  add_slide(layout = "Statement / quote <red>", master = mytmp) %>% 
  ph_with(location = ph_location_type(type = "title"), value = 'The info is our oil')

#Slide 3
#my_pres<-insert_slide_desc(my_pres = my_pres, title = 'DUR', description = 'description',object = plot1)

#Slide 4
my_pres<-insert_slide_full(my_pres = my_pres, title = "Динамика DAU в разрезе категорий", object = plot_1)

#Slide 5 
my_pres<-insert_slide_full(my_pres = my_pres, title = "Динамика откликов и активных объявлений в категории 'Одежда/обувь'", object = plot_2)

#Slide 6
my_pres<-insert_slide_full(my_pres = my_pres, title = "Интересы юзеров Одежды/обувь", object = plot_3)

#Slide 7 
my_pres<-insert_slide_full(my_pres = my_pres, title = "Распределение юзеров в разрезе регионов", object = plot_4)

#Slide 8
my_pres<-insert_slide_full(my_pres = my_pres, title = "Распределение юзеров в разрезе ОС", object = plot_5)

#Slide 9
my_pres<-insert_slide_full(my_pres = my_pres, title = "Как долго юзеры с нами", object = plot_6)

#Export
print(my_pres, target = 'ph_with_gg.pptx')  

plot_1 - ситуация в целом
plot_2 - динамика реплаев одежда обувь
plot_3 - интересы в других категориях
plot_4 - В разрезе регионов
plot_5 - В разрезе ОС
plot_6 - как долго с нами
