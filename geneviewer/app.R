#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#



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
        column(4,
               selectizeInput("gene",
                              "Gene:",
                              unique(rownames(counts)),
                              selected="Gapdh",
                              multiple = T)),
        column(4,
               selectInput("cluster", "Cell cluster:",
                           sort(as.character(unique(metadata$Celltype))),
                           selected=c("Microglia_0",
                                      "Microglia_1",
                                      "Microglia_2",
                                      "Microglia_3",
                                      "Microglia_4",
                                      "Microglia_5"),
                           multiple = T)),
        column(2,
               radioButtons("scale",
                            "Scale:",
                            c("yes", "no"), selected="no")),
        column(2,
               radioButtons("switch",
                            "plot-type",
                            c("bygene", "bycluster"),
                            selected="bygene")
        )),
    hr(),
    tabsetPanel(type = "tabs",
                tabPanel("Boxplots",
                         plotOutput("distPlot")),
                tabPanel("Stats",
                         DT::dataTableOutput("distTable")),
                tabPanel("ClusterMaps",
                         fluidRow(
                             column(6,
                                    plotOutput("MapPlots_tsne")),
                             column(6,
                                    plotOutput("MapPlots_UMAP")))),
                tabPanel("CelltypeMaps",
                         fluidRow(
                             column(6,
                                    plotOutput("MapPlotsCelltype_tsne")),
                             column(6,
                                    plotOutput("MapPlotsCelltype_UMAP"))))
    )
)


# Define server logic required to draw a histogram
server <- function(input, output) {
    ## gene plot

    observe({
        req(input$gene)
        req(input$cluster)
        plotdata <- counts[input$gene, metadata$Celltype %in% input$cluster]
        plotdata <- rbind(plotdata, t(metadata[metadata$Celltype %in% input$cluster,]))

        plotdata <- t(plotdata) %>%  as.data.frame()
        plotdata <- plotdata %>% mutate_at(input$gene,as.numeric)
        if(input$scale == "yes"){
            plotdata[,input$gene] = scale(plotdata[,input$gene])
        }
        plotdata <- plotdata %>% reshape2::melt(., direction = "long",
                                                value.name = "counts",
                                                variable.name="gene",
                                                measure.vars = input$gene)
        plotdata$counts=plotdata$counts %>% as.numeric()

        plotdata$corrGenotype_Treatment=paste0(plotdata$Genotype_corr," ", plotdata$Treatment)


        if(input$switch == "bygene"){
            compare_means(counts ~ corrGenotype_Treatment,
                          data = plotdata, method = "wilcox.test",
                          group.by = "gene")

            p<-ggplot(plotdata, aes(x=corrGenotype_Treatment, y=counts, fill=corrGenotype_Treatment)) +
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
            p <- p + stat_compare_means(comparison=my_comparisons,size = 3,
                                        label = "p.adj")
            n <- length(input$cluster)

            comparison_all <- compare_means(counts ~ corrGenotype_Treatment,
                                            data = plotdata, method = "wilcox.test",
                                            group.by = c("gene","Celltype"))
            restab <- as.data.frame(comparison_all)[,c("gene", "Celltype","group1", "group2","p","p.adj")]

        }

        if(input$switch == "bycluster"){

            comparison <- compare_means(counts ~ Celltype,
                                        data = plotdata, method = "wilcox.test",
                                        group.by = c("gene"))

            p<-ggplot(plotdata, aes(x=Celltype, y=counts, fill=Celltype)) +
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
            p <- p + stat_compare_means(comparison=my_comparisons,label = "p.adj",
                                        hide.ns = T, size = 3)
            n<-length(input$cluster)

            comparison_all <- compare_means(counts ~ Celltype,
                                            data = plotdata, method = "wilcox.test",
                                            group.by = c("gene","corrGenotype_Treatment"))
            restab <- as.data.frame(comparison_all)[,c("gene", "corrGenotype_Treatment","group1", "group2","p","p.adj")]

        }
        output$distPlot <- renderPlot( p , height = min(c(2400, 500*n), na.rm=T))
        output$distTable <- DT::renderDataTable(
            DT::datatable(restab, extensions = "Buttons",
                          options = list(
                              pageLength = 15,
                              info = FALSE,
                              lengthMenu = list(c(15,50, 100, -1),
                                                c("15","50", "100" ,"All")
                              ), dom = 'Bfrtip',
                              buttons = c('copy', 'csv', 'excel', 'pdf')
                          )))

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

        plotdata <- plotdata %>% reshape2::melt(., direction = "long",
                                                value.name = "counts",
                                                variable.name="gene",
                                                measure.vars = input$gene)

        plotdata = plotdata[plotdata$Celltype %in% input$cluster, ]


        output$MapPlots_tsne <- renderPlot({

            ggplot(plotdata, aes(x=as.numeric(tSNE_1),
                                 y=as.numeric(tSNE_2))) +
                geom_point(aes(colour=scale(counts)))+facet_wrap(vars(gene))+theme_bw()+
                ylab("tSNE_1")+xlab("tSNE_2")+scale_color_viridis(direction = -1)

        },height = min(2400, 300*floor(sqrt(length(input$gene)))))

        output$MapPlots_UMAP <- renderPlot({

            ggplot(plotdata, aes(x=as.numeric(UMAP_1),
                                 y=as.numeric(UMAP_2))) +
                geom_point(aes(colour=scale(counts)))+facet_wrap(vars(gene))+theme_bw()+
                ylab("UMAP_1")+xlab("UMAP_2")+scale_color_viridis(direction = -1)
        },height = min(2400, 300*floor(sqrt(length(input$gene)))))



        output$MapPlotsCelltype_tsne <- renderPlot({ggplot(plotdata, aes(x=as.numeric(tSNE_1),
                                                                         y=as.numeric(tSNE_2))) +
                geom_point(aes(colour=Celltype))+facet_wrap(vars(gene))+theme_bw()+
                ylab("tSNE_1")+xlab("tSNE_2")}, height = min(2400, 300*floor(sqrt(length(input$gene)))))

        output$MapPlotsCelltype_UMAP <- renderPlot({ggplot(plotdata, aes(x=as.numeric(UMAP_1),
                                                                         y=as.numeric(UMAP_2))) +
                geom_point(aes(colour=Celltype))+facet_wrap(vars(gene))+theme_bw()+
                ylab("UMAP_1")+xlab("UMAP_2")},height = min(2400, 300*floor(sqrt(length(input$gene)))))

    })
}

# Run the application
shinyApp(ui = ui, server = server)
