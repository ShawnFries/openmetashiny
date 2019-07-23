#TODO: What exactly should this be doing? rma.lm()? Or rma with mods argument? (Well, looks like the Python version used the latter; no results searching repo for rma.lm)

#############################
##       effect_norm       ##
#############################

#TODO: Add rma.glmm() for exact calculations as an option
#TODO: (lower priority) Support entering proportions as decimal from 0 to 1 (possibly + sample size)
# (Is part of backcalc)

dataModal2_subgroup <- function(failed=F) {
  modalDialog(
    selectInput("type_subgroup", "Type of data", c("One proportion", "One mean", "Two proportions", "Two means"), switch(input$dataType,
                                                                                                                    "proportion" = "One proportion",
                                                                                                                    "mean" = "One mean",
                                                                                                                    "proportions" = "Two proportions",
                                                                                                                    "means" = "Two means"
                                                                                                                   )
    ), selectInput("moderators_subgroup", "Moderators", colnames(hot$data), multiple=T),
    conditionalPanel(
      condition="input.type_subgroup == 'One proportion'",
      selectInput("metric1_subgroup",
                  "Metric",
                  c(`PR - raw proportion`="PR", `PAS - arcsine transformed proportion`="PAS", `PLO - logit transformed proportion`="PLO")
      )
    ),
    conditionalPanel(
      condition="input.type_subgroup == 'One mean'",
      selectInput("metric2_subgroup",
                  "Metric",
                  c(`MN - raw mean`="MN",
                            `MNLN - log transformed mean`="MNLN",
                            `CVLN - log transformed coefficient of variation`="CVLN",
                            `SDLN - log transformed standard deviation`="SDLN")
      )
    ),
    conditionalPanel(
      condition="input.type_subgroup == 'Two proportions'",
      selectInput("metric3_subgroup",
                  "Metric",
                  c(`RR - log risk ratio`="RR",
                            `OR - log odds ratio`="OR",
                            `RD - risk difference`="RD",
                            `AS - arcsine square root transformed risk difference`="AS",
                            `PETO - log odds ratio estimated with Peto's method`="PETO"
                  )
      )
    ),
    conditionalPanel(
      condition="input.type_subgroup == 'Two means'",
      selectInput("metric4_subgroup",
                  "Metric",
                  c(`MD - raw mean difference`="MD",
                            `SMD - standardized mean difference`="SMD",
                            `SMDH - standardized mean difference with heteroscedastic population variances in the two groups`="SMDH",
                            `ROM - log transformed ratio of means`="ROM"
                  )
      )
      ),

      conditionalPanel(
        condition="input.metric4_subgroup == 'MD'",
        checkboxInput("use_homoscedasticity_subgroup",
                      "Assume homoscedasticity of sampling variances? (i.e. true variance of measurements is the same in sample 1 and sample 2)"
        )
      ),
      conditionalPanel(
        condition="input.metric4_subgroup == 'SMD'",
        checkboxInput("variance_is_approximate_subgroup",
                      "Use the large-sample approximation for the sampling variances? If not, the exact unbiased sampling variances will be used.",
                      T
        )
      ),

    footer=tagList(
      modalButton("Cancel"),
      actionButton("oknorm_escalc_subgroup", "OK")                                     ####oknorm_escalc rendered later in this file
    )
  )
}

# Show modal when button is clicked.
observeEvent(input$effect_norm_subgroup, {
  showModal(dataModal2_subgroup())
})

observeEvent(input$oknorm_escalc_subgroup, {                         ####oknorm_escalc
  if (!is.null(hot$data) & input$type_subgroup == "One proportion") {
    vals$dataescalc_subgroup <- tryCatch({
      escalc(
        measure=input$metric1_subgroup,

        xi=if (!is.null(hot$data$count)) count
        else if (!is.null(hot$data$counts)) countsx
        else if (!is.null(hot$data$xi)) xi
        else if (!is.null(hot$data$x)) x
        else if (!is.null(hot$data$x_i)) x_i
        else if (!is.null(hot$data$x_is)) x_is
        else xis,

        ni=if (!is.null(hot$data$ni)) ni
        else if (!is.null(hot$data$nis)) nis
        else if (!is.null(hot$data$n_i)) n_i
        else if (!is.null(hot$data$n_is)) n_is
        else if (!is.null(hot$data$n)) n
        else ns,

        data=hot$data)},
      error=function(err) {
        #error handler picks up where error was generated
        print(paste("ERROR:  There must be at least one column named \"count\" or \"xi\" and one named \"ni\""))
      }
    )#ends tryCatch
    removeModal()

  } else if (!is.null(hot$data) & input$type_subgroup == "One mean") {
    vals$dataescalc_subgroup <- tryCatch({
      escalc(measure=input$metric2_subgroup,

             mi=if (!is.null(hot$data$mi)) mi
             else if (!is.null(hot$data$m)) m
             else if (!is.null(hot$data$m_i)) m_i
             else if (!is.null(hot$data$m_is)) m_is
             else if (!is.null(hot$data$mis)) mis
             else if (!is.null(hot$data$u)) u
             else if (!is.null(hot$data$ui)) ui
             else if (!is.null(hot$data$u_i)) u_i
             else if (!is.null(hot$data$u_is)) u_is
             else if (!is.null(hot$data$uis)) uis
             else if (!is.null(hot$data$mu)) mu
             else if (!is.null(hot$data$mui)) mui
             else if (!is.null(hot$data$mu_i)) mu_i
             else if (!is.null(hot$data$mu_is)) mu_is
             else muis,

             sdi=if (!is.null(hot$data$sdi)) sdi
             else if (!is.null(hot$data$sd)) sd
             else if (!is.null(hot$data$sd_i)) sd_i
             else if (!is.null(hot$data$sd_is)) sd_is
             else if (!is.null(hot$data$sdis)) sdis
             else if (!is.null(hot$data$sigma)) sigma
             else if (!is.null(hot$data$sigmai)) sigmai
             else if (!is.null(hot$data$sigma_i)) sigma_i
             else if (!is.null(hot$data$sigma_is)) sigma_is
             else sigmais,

             ni=if (!is.null(hot$data$ni)) ni
             else if (!is.null(hot$data$nis)) nis
             else if (!is.null(hot$data$n_i)) n_i
             else if (!is.null(hot$data$n_is)) n_is
             else if (!is.null(hot$data$n)) n
             else ns,

             data=hot$data
      )
    },
    error=function(err) {
      print("ERROR:  There must be at least one column each named \"mi\", \"sdi\" and \"ni\"")
    }
    )#ends tryCatch
    removeModal()

  } else if(!is.null(hot$data) & input$type_subgroup == "Two proportions") {
    vals$dataescalc_subgroup <- tryCatch({ # TODO: Add error handling for other column names/check similar names
      escalc(measure=input$metric3_subgroup,
             ai=ai,
             n1i=n1i,
             ci=ci,
             n2i=n2i,
             data=hot$data)},
      error=function(err){
        print("ERROR:  There must be at least one column each named \"ai\", \"ci\", \"n1i\", and \"n2i\"")
      }
    )#ends tryCatch
    removeModal()

  } else if (!is.null(hot$data) & input$type_subgroup == "Two means") {  # TODO: Add error handling for other column names/check similar names
    vals$dataescalc_subgroup <- tryCatch({
      escalc(measure=input$metric4_subgroup,
             m1i=if (!is.null(hot$data$m1i)) hot$data$m1i
             else if (!is.null(hot$data$m1)) hot$data$m1
             else if (!is.null(hot$data$m1_i)) hot$data$m1_i
             else if (!is.null(hot$data$m1_is)) hot$data$m1_is
             else if (!is.null(hot$data$m1is)) hot$data$m1is
             else if (!is.null(hot$data$u1)) hot$data$u1
             else if (!is.null(hot$data$u1i)) hot$data$u1i
             else if (!is.null(hot$data$u1_i)) hot$data$u1_i
             else if (!is.null(hot$data$u1_is)) hot$data$u_is
             else if (!is.null(hot$data$u1is)) hot$data$uis
             else if (!is.null(hot$data$mu1)) hot$data$mu1
             else if (!is.null(hot$data$mu1i)) hot$data$mu1i
             else if (!is.null(hot$data$mu1_i)) hot$data$mu1_i
             else if (!is.null(hot$data$mu1_is)) hot$data$mu1_is
             else hot$data$mu1is,
             sd1i=hot$data$sd1i,
             n1i=hot$data$n1i,
             m2i=if (!is.null(hot$data$m2i)) hot$data$m2i
             else if (!is.null(hot$data$m2)) hot$data$m2
             else if (!is.null(hot$data$m2_i)) hot$data$m2_i
             else if (!is.null(hot$data$m2_is)) hot$data$m2_is
             else if (!is.null(hot$data$m2is)) hot$data$m2is
             else if (!is.null(hot$data$u2)) hot$data$u2
             else if (!is.null(hot$data$u2i)) hot$data$u2i
             else if (!is.null(hot$data$u2_i)) hot$data$u2_i
             else if (!is.null(hot$data$u2_is)) hot$data$u_is
             else if (!is.null(hot$data$u2is)) hot$data$uis
             else if (!is.null(hot$data$mu2)) hot$data$mu2
             else if (!is.null(hot$data$mu2i)) hot$data$mu2i
             else if (!is.null(hot$data$mu2_i)) hot$data$mu2_i
             else if (!is.null(hot$data$mu2_is)) hot$data$mu2_is
             else hot$data$mu2is,
             sd2i=sd2i,
             n2i=n2i,

             # LS is the default (large-sample approximation if using SMD). UB is unbiased (only an option for measure == "SMD")
             vtype=if (input$metric4_subgroup == "SMD" & !input$variance_is_approximate_subgroup) "UB"
             else if (input$metric4_subgroup == "MD" & input$use_homoscedasticity_subgroup) "HO"
             else "LS",
             data=hot$data)
    },
    error=function(err){
      print("ERROR:  There must be at least one column each named \"m1i\", \"m2i\", \"sd1i\", \"sd2i\", \"n1i\", and \"n2i\"")
    }
    )#ends tryCatch
    removeModal()

  } else {
    showModal(dataModal(failed=T))
  }


  output$escalcdat_subgroup <- renderTable({
    if (!is.null(vals$dataescalc_subgroup)) {
      vals$dataescalc_subgroup
    }
  })
})


#################################
##         oknorm_res          ##
#################################

res_subgroup <- eventReactive(input$oknorm_res_subgroup, {
  conflevel <- as.numeric(as.character(input$conflevel_subgroup))
  cc <- as.numeric(as.character(input$cc_subgroup))
  
  tryCatch({
    unique_subgroups <- unique(hot$data[[input$moderators_subgroup]])
    rmas <- list()
    for (subgroup in unique_subgroups) {
      #print(subgroup)
     # print(vals$dataescalc_subgroup)
     # print(input$moderators_subgroup)
     # print(subset(vals$dataescalc_subgroup, vals$dataescalc_subgroup[[input$moderators_subgroup]] == subgroup))
      rmas[[paste("subgroup", subgroup)]] <- rma(yi,
          vi,
          method=input$est_subgroup,
          data=vals$dataescalc_subgroup,
          add=cc,
          to=input$addto_subgroup,
          level=conflevel,
          subset=vals$dataescalc_subgroup[[input$moderators_subgroup]] == subgroup,  # Really awful line actually, but needed to subset by variable column directly in metafor
          digits=input$digits_subgroup)
    }
    rmas
  },
  error=function(err) {
    print(paste("ERROR:  ", err))
  }
  )
}
)

observeEvent(input$oknorm_res_subgroup, {
  # cc<-as.numeric(as.character(input$cc_subgroup))
  #
  # res<-if (input$fixed_norm_subgroup == "FE") {
  #   rma(yi, vi, method=input$fixed_norm_subgroup, data=vals$dataescalc_subgroup, weighted=F, add=cc, to=input$addto_subgroup)
  # } else if(input$fixed_norm=="RE"){
  #   rma(yi, vi, method=input$rand_est_subgroup, data=vals$dataescalc_subgroup, weighted=F, add=cc, to=input$addto_subgroup)
  # }

  res_subgroup <- res_subgroup()

  #####################NEEDS TO BE GENERALIZED############################
  #TODO: Fix this.. support multiple subgroups don't just show the last
  conflevel <- as.numeric(as.character(input$conflevel_subgroup))
  output$forest_plots <- renderUI({
    subgroup_forest_plots <- list()
    i <- 1
    for (subgroup in res_subgroup) {
      plot_name <- paste("forest_norm_subgroup", i, sep="_")
      subgroup_forest_plots[[plot_name]] <- plotOutput(plot_name)
      
      i <- i + 1
    }
  
    do.call(tagList, subgroup_forest_plots)
  })
  
  number_of_plots <- length(res_subgroup)
  #flattened_subgroups <- unlist(res_subgroup, recursive=F, use=F)
  #print(flattened_subgroups)
  #print(res_subgroup[[1]])
  unique_subgroups <- unique(hot$data[[input$moderators_subgroup]])
  for (i in 1:number_of_plots) {
    local({
      local_i <- i
      plot_name <- paste("forest_norm_subgroup", local_i, sep="_")
      output[[plot_name]] <- renderPlot({
        forest(res_subgroup[[local_i]], refline=NA, level=conflevel, digits=input$digits_subgroup,
               atransf=if (input$atransf_subgroup != "none") get(paste0("transf.", input$atransf_subgroup)))
        grid.text(paste("Subgroup:", unique_subgroups[local_i]), .5, .9, gp=gpar(cex=2))
      })
    })
  }

  output$msummary_norm_subgroup <- renderPrint({
    subgroup_index <- 0
    for (subgroup_analysis in res_subgroup) {
      subgroup_index <- subgroup_index + 1
      subgroup_name <- unique_subgroups[subgroup_index]
      print(paste("Subgroup:", subgroup_name))
      print(subgroup_analysis)
    }
  })

})

#################################
##         save_fplot          ##
#################################
dataModal3_subgroup <- function(failed=F) {
  modalDialog(

    textInput("fplot_path_subgroup", "Type a path to save your forest plot:",
              "~/openmeta/plot1.png"),
    textInput("fplot_w_subgroup", "Width of forest plot:", "8"),
    textInput("fplot_h_subgroup", "Height of forest plot:", "6"),
    selectInput("fplot_unit_subgroup", "Unit of saved plot dimensions", c(`pixels`="px", `inches`="in", "cm", "mm"), "in"),
    textInput("fplot_resolution_subgroup", "Resolution of forest plot:", "210"),

    footer = tagList(modalButton("Cancel"), actionButton("ok_save_fplot_subgroup", "OK")
    )
  )
}

# Show modal when button is clicked.
observeEvent(input$save_fplot_subgroup, {
  showModal(dataModal3_subgroup())
})

observeEvent(input$ok_save_fplot_subgroup,{
  conflevel <- as.numeric(as.character(input$conflevel_subgroup))

  res_subgroup <- res_subgroup()

  ##save a png of the plot
  png(filename=input$fplot_path_subgroup,
      width=as.numeric(input$fplot_w_subgroup),
      height=as.numeric(input$fplot_h_subgroup),
      units=input$fplot_unit_subgroup,
      res=as.numeric(input$fplot_resolution_subgroup)
     )

  forest(res, refline=NA, digits=input$digits_subgroup, level=conflevel, atransf=if (input$atransf_subgroup != "none") get(paste0("transf.", input$atransf_subgroup)))

  dev.off()

  removeModal()
})


##########################
##     dynamic UI       ##
##########################
observe({
  fixed_norm <- input$fixed_norm_subgroup

  updateSelectInput(session,
                    "est_subgroup",
                    "Estimation method",

                    if (fixed_norm == "RE") c(`DerSimonian Laird`="DL",
                                              `Hedges`="HE",
                                              `Hunter-Schmidt`="HS",
                                              `Sidik-Jonkman`="SJ",
                                              `Maximum likelihood`="ML",
                                              `Restricted ML`="REML",
                                              `Empirical Bayes`="EB",
                                              `Paule-Mandel`="PM",
                                              `Generalized Q-statistic`="GENQ"
                                             )
                    else c(`Inverse-variance`="FE"),

                    if (fixed_norm == "RE") "REML"  # Default to REML if Random-Effects
  )
})
