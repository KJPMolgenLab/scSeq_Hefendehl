#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
##

#install.packages("nloptr")

library(shiny)
library(ggplot2)
library(ggpubr)
library(tidyverse)
library(DT)
library(viridis)


trimmed_scaled <- function(vec){
  m <- max(vec, na.rm=T)
  vec[vec>quantile(vec,.95)] = m
  return(scale(vec))
}

load("Dataset.RData")

metadata=samplemeta

# Define UI for application that draws a histogram
ui <- fluidPage(

  # Application title
  titlePanel("Gene viewer scRNA"),

  # Sidebar with a slider input for number of bins
  fluidRow(
    column(2,
           selectizeInput("gene",
                          "Gene:",
                          options = list(delimiter =" ", create = T),
                          unique(rownames(counts)),
                          selected="Gapdh",
                          multiple = T), p("paste multiple genes with space as delimiter")),

    column(2,
           selectInput("cluster", "Cell cluster:",
                       sort(as.character(unique(metadata$Celltype))),
                       selected=unique(metadata$Celltype),
                       multiple = T)),
    column(2,
           selectInput("treatment", "Condition",
                       sort(as.character(unique(metadata$corrGenotype_Treatment))),
                       selected=c("APPPS1+ Ctrl",
                                  "WT Stroke",
                                  "APPPS1+ Stroke",
                                  "WT Ctrl"),
                       multiple = T)),
    column(1,
           radioButtons("scale",
                        "Scale:",
                        c("yes", "no"), selected="no")),
    column(1,
           radioButtons("spatialfilter",
                        "Spatial:",
                        c("all", "Stroke", "Control"),
                        selected=c("all"))),
    column(2,
           radioButtons("switch",
                        "grid-type",
                        c("bygene", "bycluster"),
                        selected="bygene")
    ),
    column(2,
           radioButtons("clustplot",
                        "cluster-type",
                        c("t-sne", "UMAP", "both"),
                        selected="both"))),
  hr(),
  tabsetPanel(type = "tabs",
              tabPanel("Boxplots",
                       plotOutput("distPlot")),
              tabPanel("Stats",
                       DT::dataTableOutput("distTable")),

              tabPanel(title="Gene Maps",
                       plotOutput("MapPlots")),

              tabPanel(title="Feature Maps",
                       selectInput("condition",
                                   "Feature",multiple=F,
                                   c("nFeature_RNA","Mouse_ID","Genotype",
                                     "Age","Sex","Treatment","Brain_region",
                                     "total_reads","Plate","percent.mito","percent.ribo",
                                     "S.Score","G2M.Score","Phase","Celltype",
                                     "Genotype_corr","methoxy","corrGenotype_Treatment",
                                     "Pseudotime",
                                     "Control_spatial_binary", "Stroke_spatial_binary"),
                                   selected="Celltype"),
                       plotOutput("MapPlots_Feature")
              ),
              tabPanel(title="SpatialPlots",
                       selectInput("spatial",
                                   "Feature",multiple=F,
                                   c("Control_spatial_binary", "Stroke_spatial_binary"),
                                   selected="Stroke_spatial_binary"),
                       plotOutput("SpatialPlots_Feature")),
              tabPanel(title="SpatialStats",
                       DT::dataTableOutput("SpatialTab_Feature")
              )

  )
)



# Define server logic required to draw a histogram
server <- function(input, output) {
  ## gene plot

  observe({
    req(input$gene)
    req(input$cluster)
    req(input$treatment)
    filteridx <-  which(metadata$Celltype %in% input$cluster & metadata$corrGenotype_Treatment %in% input$treatment)

    plotdata <- counts[input$gene, filteridx]
    plotdata <- rbind(plotdata, t(metadata[colnames(plotdata),]))

    plotdata <- t(plotdata) %>%  as.data.frame()
    plotdata <- plotdata %>% mutate_at(input$gene,as.numeric)
    if(input$scale == "yes"){
      plotdata[,input$gene] = scale(plotdata[,input$gene])
    }
    plotdata_all = plotdata
    if(input$spatialfilter=="Stroke"){
      plotdata = plotdata[plotdata$Stroke_spatial_binary, ]
    }

    if(input$spatialfilter=="Control"){
      plotdata = plotdata[plotdata$Control_spatial_binary, ]
    }

    plotdata <- plotdata %>% reshape2::melt(., direction = "long",
                                            value.name = "counts",
                                            variable.name="gene",
                                            measure.vars = input$gene)
    plotdata$counts=plotdata$counts %>% as.numeric()

    plotdata$corrGenotype_Treatment=paste0(plotdata$Genotype_corr," ", plotdata$Treatment)


    if(input$switch == "bygene"){
      if(length(input$treatment)>1){
        ggpubr:::compare_means(counts ~ corrGenotype_Treatment,
                               data = plotdata, method = "wilcox.test",
                               group.by = "gene")

        gp<-ggplot(plotdata, aes(x=corrGenotype_Treatment, y=counts, fill=corrGenotype_Treatment)) +
          geom_boxplot(outlier.shape = NA)+geom_jitter(size=1,width=0.05, aes(colour = methoxy))+
          facet_grid(cols=vars(gene), rows=vars(Celltype), scales="free") +
          scale_color_manual(values = c("MX04+" = "black", "MX04+" = "gray"))+
          theme_bw()+labs(y="log(count +1)")+
          theme(axis.text.x=element_blank(),
                axis.ticks.x=element_blank(), legend.position = "top")

        my_comparisons <- list( c("APPPS1+ Ctrl", "APPPS1+ Stroke"),
                                c("APPPS1+ Ctrl",   "WT Ctrl"),
                                c("APPPS1+ Ctrl", "WT Stroke"),
                                c("APPPS1+ Stroke", "WT Ctrl"),
                                c("APPPS1+ Stroke", "WT Stroke"),
                                c("WT Stroke","WT Ctrl"))
        gp <- gp + ggpubr:::stat_compare_means(comparison=my_comparisons,size = 3,
                                               label = "p.adj")
        n <- sum(filteridx)
        if(length(input$cluster)>1){
          comparison_all <- ggpubr:::compare_means(counts ~ corrGenotype_Treatment,
                                                   data = plotdata, method = "wilcox.test",
                                                   group.by = c("gene","Celltype"))
          restab <- as.data.frame(comparison_all)[,c("gene", "Celltype","group1", "group2","p","p.adj")]
        }else {
          comparison_all <- ggpubr:::compare_means(counts ~ corrGenotype_Treatment,
                                                   data = plotdata, method = "wilcox.test",
                                                   group.by = c("gene"))
          restab <- as.data.frame(comparison_all)[,c("gene","group1", "group2","p","p.adj")]
          restab$Celltype <- input$cluster
          restab <- restab[,c("gene", "Celltype","group1", "group2","p","p.adj")]

        }

        attach(restab)
        for(i in 1:nrow(restab)){
          idxbas=plotdata$gene==gene[i]&
            plotdata$Celltype==Celltype[i]

          idxg1=idxbas& plotdata$corrGenotype_Treatment==group1[i]
          idxg2=idxbas& plotdata$corrGenotype_Treatment==group2[i]
          restab[i,"g1median"]=median(plotdata[idxg1, "counts"], na.rm=T)
          restab[i,"g2median"]=median(plotdata[idxg2, "counts"], na.rm=T)
        }
        detach(restab)
      } else {
        gp<-ggplot(plotdata, aes(x=corrGenotype_Treatment, y=counts, fill=corrGenotype_Treatment)) +
          geom_boxplot(outlier.shape = NA)+geom_jitter(size=1,width=0.05, aes(colour = methoxy))+
          facet_grid(cols=vars(gene), rows=vars(Celltype), scales="free") +
          scale_color_manual(values = c("MX04+" = "black", "MX04+" = "gray"))+
          theme_bw()+labs(y="log(count +1)")+
          theme(axis.text.x=element_blank(),
                axis.ticks.x=element_blank(), legend.position = "top")

        restab=data.frame(Exception="only one Condition Selected")
      }
    }

    if(input$switch == "bycluster"){
      if(length(input$cluster)>1){
        comparison <- ggpubr:::compare_means(counts ~ Celltype,
                                             data = plotdata, method = "wilcox.test",
                                             group.by = c("gene"))

        gp<-ggplot(plotdata, aes(x=Celltype, y=counts, fill=Celltype)) +
          geom_boxplot(outlier.shape = NA)+
          facet_grid(cols=vars(gene), rows=vars(corrGenotype_Treatment)) +
          geom_jitter(size=1,width=0.05, aes(colour = methoxy))+
          scale_color_manual(values = c("MX04+" = "black", "MX04+" = "gray"))+
          theme_bw()+labs(y="log(count +1)")+
          theme(axis.text.x=element_blank(),
                axis.ticks.x=element_blank(), legend.position = "top")

        tmp=comparison[comparison$gene==input$gene[1],c("group1", "group2")]
        tmp=tmp[order(tmp$group1,tmp$group2), ]

        my_comparisons <- apply(tmp,1, c, simplify = F)
        gp <- gp + ggpubr:::stat_compare_means(comparison=my_comparisons,label = "p.adj",
                                               hide.ns = T, size = 3)
        n<-sum(filteridx)

        if(length(input$treatment)>1){

          comparison_all <- ggpubr:::compare_means(counts ~ Celltype,
                                                   data = plotdata, method = "wilcox.test",
                                                   group.by = c("gene","corrGenotype_Treatment"))
          restab <- as.data.frame(comparison_all)[,c("gene", "corrGenotype_Treatment","group1", "group2","p","p.adj")]
        } else {
          comparison_all <- ggpubr:::compare_means(counts ~ Celltype,
                                                   data = plotdata, method = "wilcox.test",
                                                   group.by = c("gene"))
          restab <- as.data.frame(comparison_all)[,c("gene", "group1", "group2","p","p.adj")]
          restab$corrGenotype_Treatment <- input$treatment
          restab <- restab[,c("gene", "corrGenotype_Treatment","group1", "group2","p","p.adj")]


        }
        attach(restab)
        for(i in 1:nrow(restab)){
          idxbas=plotdata$gene==gene[i]&
            plotdata$corrGenotype_Treatment==corrGenotype_Treatment[i]

          idxg1=idxbas& plotdata$Celltype==group1[i]
          idxg2=idxbas& plotdata$Celltype==group2[i]
          restab[i,"g1median"]=median(plotdata[idxg1, "counts"], na.rm=T)
          restab[i,"g2median"]=median(plotdata[idxg2, "counts"], na.rm=T)
        }
        detach(restab)
      } else {
        gp<-ggplot(plotdata, aes(x=Celltype, y=counts, fill=Celltype)) +
          geom_boxplot(outlier.shape = NA)+
          facet_grid(cols=vars(gene), rows=vars(corrGenotype_Treatment)) +
          geom_jitter(size=1,width=0.05, aes(colour = methoxy))+
          scale_color_manual(values = c("MX04+" = "black", "MX04+" = "gray"))+
          theme_bw()+labs(y="log(count +1)")+
          theme(axis.text.x=element_blank(),
                axis.ticks.x=element_blank(), legend.position = "top")

        restab=data.frame(Exception = "Only one Celltype selected")
      }
    }


    output$distPlot <- renderPlot(gp , height = min(c(2400, 500*n), na.rm=T))
    output$distTable <- DT::renderDataTable(
      DT::datatable(restab, extensions = "Buttons",
                    filter="top",
                    options = list(
                      pageLength = 15,
                      info = FALSE,
                      lengthMenu = list(c(15,50, 100, -1),
                                        c("15","50", "100" ,"All")
                      ),dom = 'Blfrtip',
                      buttons = c('copy', 'csv', 'excel', 'pdf')
                    ))
    )

  })
  observe({
    trimmed_scaled <- function(vec){
      m <- quantile(vec[vec!=0],.99, na.rm=T)
      vec[vec>m] = m
      #vec[vec==0]= NA
      #vec[which(vec>=0)]= scale(vec[which(vec>=0)])
      vec= scale(vec)
      return(vec)
    }
    req(input$gene)
    req(input$cluster)

    tmp <- apply(t(counts[input$gene,]), 2, trimmed_scaled)
    rownames(tmp) = colnames(counts)
    plotdata <- tmp %>% as.data.frame()
    plotdata <- cbind(plotdata, metadata[rownames(plotdata),])

    if(input$spatialfilter=="Stroke"){
      plotdata = plotdata[plotdata$Stroke_spatial_binary, ]
    }

    if(input$spatialfilter=="Control"){
      plotdata = plotdata[plotdata$Control_spatial_binary, ]
    }

    plotdata_all = plotdata

    plotdata <- plotdata %>% reshape2::melt(., direction = "long",
                                            value.name = "counts",
                                            variable.name="gene",
                                            measure.vars = input$gene)
    filteridx <-  plotdata$Celltype %in% input$cluster & plotdata$corrGenotype_Treatment %in% input$treatment

    plotdata = plotdata[filteridx, ]

    if(input$clustplot=="both"){
      a<-ggplot(plotdata, aes(x=as.numeric(tSNE_1),
                              y=as.numeric(tSNE_2))) +
        geom_point(aes(colour=scale(counts)))+facet_wrap(vars(gene))+theme_bw()+
        ylab("tSNE_1")+xlab("tSNE_2")+ labs(colour="scaled reads")+scale_color_viridis(direction = -1)
      b<-ggplot(plotdata, aes(x=as.numeric(UMAP_1),
                              y=as.numeric(UMAP_2))) +
        geom_point(aes(colour=scale(counts)))+facet_wrap(vars(gene))+theme_bw()+
        ylab("UMAP_1")+xlab("UMAP_2")+ labs(colour="scaled reads")+scale_color_viridis(direction = -1)
      gp2 <- ggarrange(a,b)
    }
    if(input$clustplot=="UMAP"){
      gp2<-ggplot(plotdata, aes(x=as.numeric(UMAP_1),
                                y=as.numeric(UMAP_2))) +
        geom_point(aes(colour=scale(counts)))+facet_wrap(vars(gene))+theme_bw()+
        ylab("UMAP_1")+xlab("UMAP_2")+ labs(colour="scaled reads")+scale_color_viridis(direction = -1)
    }

    if(input$clustplot=="t-sne"){
      gp2<-ggplot(plotdata, aes(x=as.numeric(tSNE_1),
                                y=as.numeric(tSNE_2))) +
        geom_point(aes(colour=scale(counts)))+facet_wrap(vars(gene))+theme_bw()+
        ylab("tSNE_1")+xlab("tSNE_2")+ labs(colour="scaled reads")+scale_color_viridis(direction = -1)

    }

    output$MapPlots <- renderPlot(gp2, height = min(2400, 500*floor(sqrt(length(input$gene)))))


    if(input$clustplot=="both"){
      a<-ggplot(plotdata, aes(x=as.numeric(tSNE_1),
                              y=as.numeric(tSNE_2))) +
        geom_point(aes(colour=get(input$condition)))+theme_bw()+
        ylab("tSNE_1")+xlab("tSNE_2")+ labs(colour=input$condition)
      b<-ggplot(plotdata, aes(x=as.numeric(UMAP_1),
                              y=as.numeric(UMAP_2))) +
        geom_point(aes(colour=get(input$condition)))+theme_bw()+
        ylab("UMAP_1")+xlab("UMAP_2")+ labs(colour=input$condition)
      p2 <- ggarrange(a,b)
    }
    if(input$clustplot=="UMAP"){
      p2<-ggplot(plotdata, aes(x=as.numeric(UMAP_1),
                               y=as.numeric(UMAP_2))) +
        geom_point(aes(colour=get(input$condition)))+theme_bw()+
        ylab("UMAP_1")+xlab("UMAP_2")+ labs(colour=input$condition)
    }

    if(input$clustplot=="t-sne"){
      p2<-ggplot(plotdata, aes(x=as.numeric(tSNE_1),
                               y=as.numeric(tSNE_2))) +
        geom_point(aes(colour=get(input$condition)))+theme_bw()+
        ylab("tSNE_1")+xlab("tSNE_2")+ labs(colour=input$condition)

    }


    output$MapPlots_Feature <- renderPlot(p2)

    plotdata_all <- plotdata_all %>% reshape2::melt(., direction = "long",
                                                    value.name = "counts",
                                                    variable.name="gene",
                                                    measure.vars = input$gene)

    filteridx <-  plotdata_all$Celltype %in% input$cluster & plotdata_all$corrGenotype_Treatment %in% input$treatment
    plotdata_all= plotdata_all[filteridx,]
    plotdata_all$counts=plotdata_all$counts %>% as.numeric()
    gp_spatial<-ggplot(plotdata_all, aes(x=get(input$spatial), y=counts, fill=get(input$spatial))) +
      geom_boxplot(outlier.shape = NA)+geom_jitter(size=1,width=0.05, aes(colour = methoxy))+
      facet_grid(cols=vars(Celltype), rows=vars(gene), scales="free")+
      scale_color_manual(values = c("MX04+" = "black", "MX04+" = "gray"))+
      theme_bw()+labs(y="log(count +1)", x=input$spatial, fill=input$spatial)+
      theme(axis.text.x=element_blank(),
            axis.ticks.x=element_blank(), legend.position = "top")


    my_comparisons <- list( c("FALSE", "TRUE"))
    gp_spatial <- gp_spatial + ggpubr:::stat_compare_means(comparison=my_comparisons,size = 3,
                                                           label = "p.adj")
    if(length(input$cluster)>1){
      comparison_all <- ggpubr:::compare_means(as.formula(paste0("counts ~", input$spatial)),
                                               data = plotdata_all, method = "wilcox.test",
                                               group.by = c("gene", "Celltype"))
      restab_spatial <- as.data.frame(comparison_all)[,c("gene", "Celltype","group1", "group2","p","p.adj")]
    }else {
      comparison_all <- ggpubr:::compare_means(counts ~ corrGenotype_Treatment,
                                               data = plotdata, method = "wilcox.test",
                                               group.by = c("gene"))
      restab_spatial <- as.data.frame(comparison_all)[,c("gene","group1", "group2","p","p.adj")]
      restab_spatial$Celltype <- input$cluster
      restab_spatial <- restab_spatial[,c("gene", "Celltype","group1", "group2","p","p.adj")]

    }


    output$SpatialPlots_Feature <- renderPlot(gp_spatial, height = min(2400, 500*floor(sqrt(length(input$gene)))))
    output$SpatialTab_Feature <- DT::renderDataTable(
      DT::datatable(restab_spatial, extensions = "Buttons",
                    filter="top",
                    options = list(
                      pageLength = 15,
                      info = FALSE,
                      lengthMenu = list(c(15,50, 100, -1),
                                        c("15","50", "100" ,"All")
                      ),dom = 'Blfrtip',
                      buttons = c('copy', 'csv', 'excel', 'pdf')
                    )))



  })
}

# Run the application
shinyApp(ui = ui, server = server)
