library(shiny)


initialamount = sliderInput(inputId = "initialamount",label = "Initial Amount", value=1000,min=1000,max=100000,step=500)
returnrate = sliderInput(inputId = "returnrate",label = "Return Rate (in %)", value=5,min=0,max=20,step=0.1)
years = sliderInput(inputId = "years",label = "Years", value=20,min=0,max=50)
annualcontrib = sliderInput(inputId = "annualcontrib",label = "Annual Contribution", value=2000,min=0,max=50000,step=500)
growthrate = sliderInput(inputId = "growthrate",label = "Growth Rate (in %)", value=2,min=0,max=20,step=0.1)
facet = selectInput(inputId="facet",label="Facet?",choices= c("No","Yes")) 


ui = fluidPage(initialamount,returnrate,years,annualcontrib,growthrate,facet,
               mainPanel(
                 plotOutput("freqs_plot"),
                 tableOutput("summary_table")
               )
               
               
)
server = function(input, output){
  #onclick(test %in% input$initialamount)
  #' @title Future Value
  #' @description Find the future amount of money with an interest rate and time
  #' @param Amount = number you start with, rate = rate that it increases, years = the amount of years in the calculation
  #' @return The new value 
  future_value = function(amount, rate,years){
    rate = rate/100
    return(amount*((1+rate)^years)) 
    
  }
  #' @title Annuity
  #' @description Find the annuity of a certain savings rate 
  #' @param Contrbit = how much is being set aside. Rate = What is the rate of interest on the set aside money. Years = How many years is money being set aside and gaining interest. 
  #' @return
  annuity = function(contrib,rate,years){
    rate = rate/100
    return(contrib*((((1+rate)^years)-1)/rate))
  }
  #' @title Growing Annuity
  #' @description Calculate the growing annuity
  #' @param contrib= Amount being set aside, rate = annual rate of return, growth = annial growth rate, years = number of years
  #' @return The amount under growing annuity
  growing_annuity = function(contrib,rate,growth,years){
    rate = rate/100
    growth = growth/100
    return(contrib*(( ((1+rate)^years) - ((1+growth)^years)        )/(rate-growth))    )
    
  }
  
  modalities = reactive({
    modalities = data.frame(1,1000,1000,1000,stringsAsFactors = FALSE)
    
    names(modalities) = c("year","no_contrib","fixed_contrib", "growing_contrib")
    for(val in 1:input$years-1){
      
      modalities[val+1,] = c(val+1, future_value(amount=input$initialamount,rate=input$returnrate,years=val),future_value(amount=input$initialamount,rate=input$returnrate,years=val)+annuity(contrib=input$annualcontrib, rate=input$returnrate,years=val),future_value(amount=input$initialamount,rate=input$returnrate,years=val)+growing_annuity(contrib=input$annualcontrib,rate=input$returnrate,growth=input$growthrate,years=val))
    }
    modalities
  })
  output$freqs_plot <- renderPlot({
    
    library(ggplot2)
    mod = modalities()
    if(input$facet == "No"){
      ggplot(mod)+geom_line(aes(mod$year,mod$no_contrib),color="red")+
        geom_line(aes(mod$year,mod$fixed_contrib),color="blue")+
        geom_line(aes(mod$year,mod$growing_contrib),color="yellow")+
        xlab("Time")+ylab("Amount")+ggtitle("Graph")
    }else{
      newmod = data.frame(1,"type",100, stringsAsFactors = FALSE)
      names(newmod) = c("year","type","amount")
      for(index in 1:nrow(mod)){
        i = index*3
        newmod[i,'year'] = mod[index,"year"]
        newmod[i,'type'] = "no_contrib"
        newmod[i,'amount'] = mod[index,"no_contrib"]
        
        newmod[i+1,'year'] = mod[index,"year"]
        newmod[i+1,'type'] = "fixed_contrib"
        newmod[i+1,'amount'] = mod[index,"fixed_contrib"]
        
        newmod[i+2,'year'] = mod[index,"year"]
        newmod[i+2,'type'] = "growing_contrib"
        newmod[i+2,'amount'] = mod[index,"growing_contrib"]
        
      }
      newmod = newmod[-c(1,2),]
      print(newmod)
      ggplot(newmod,aes(year, amount))+geom_point()+geom_line()+facet_grid(~type)
    }
  })
  
  output$summary_table <- renderTable({
    print(modalities())
  })
  
}
shinyApp(ui = ui,server=server)
#rsconnect::setAccountInfo(name='jhohbein', token='A8058512227DA0090589575981750643', secret='TEz0oCBr28GnFkyWx57C+LZeCYjah/q92Uqj+SpE')
#library(rsconnect)
#rsconnect::deployApp(paste(getwd(),'/workout02-Jameson-Hohbein.rmd',sep=""))





