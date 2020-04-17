source('./packages.R')
source('./functions.R')
my_pres <- read_pptx(path = 'template.pptx') 

(my_pres_layout <- layout_summary(my_pres)$layout)
(tmplt_name <- layout_summary(my_pres)$master[1]) 
valid_types<-c('body', 'title', 'ctrTitle', 'subTitle', 'dt', 'ftr', 'sldNum')

check_template(my_pres = my_pres,
               my_pres_layout = my_pres_layout, 
               tmplt_name = tmplt_name,
               valid_types = valid_types,
               output_filename = 'output.pptx')

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

plot_1
#And here we go with our presentation:  
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
