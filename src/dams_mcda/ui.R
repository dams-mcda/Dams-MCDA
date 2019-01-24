# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
#base_dir <- "~/Beatrice2/R_ELF/R_NEST/MCDA_App_Shiny/"
#working_dir <- paste("MCDA_11132018/WSM_Tool", "", sep="")
#setwd(working_dir)

base_dir <- "/srv/shiny-server/dams_mcda/"
setwd(base_dir)

library(shiny)
library(ggplot2)
library(dplyr)
library(shinyjs)
library(Cairo)

options(shiny.usecairo=T)

source("server.R")

# default graph dimensions
graph_width <- "100%"
graph_height <- 500


# Define UI for Shiny web application
ui <- shinyUI(fluidPage(
	# used for toggling display of elements
	shinyjs::useShinyjs(),

	# link css
	tags$head(
		tags$link(rel = "stylesheet", type = "text/css", href = "dams_mcda.css")
	),

	titlePanel("A Watershed-Scale Dam Decision Making Tool"),

	navlistPanel(
		# Define layout widths
		widths = c(4,8),

		#Define Instructions tab
		tabPanel("Multi-Criteria Decision Analysis",
				htmlOutput("Introduction"),
			  HTML("This R Shiny app supports decision making about a dam, using a set of decision criteria and decision alternatives identified using data from dam decision-maker interviews. The tool uses a Weighted Sum approach to Multi-Criteria Decision Analysis (MCDA) to compare 
             decision-maker preferences for decision criteria over a set of decision alternatives.<br>\

			       <br>Picture a watershed: rain falls and runs downhill toward tributaries that flow into the river and ultimately to the ocean. This watershed is home to valuable ecosystem services, including pristine natural lakes, clean water sources, and significant 
             biodiversity, including several sea-run fish species (e.g. Atlantic salmon, American eel, Blueback herring, and Alewife). The river and its tributaries are home to many dams that also provide services, including reservoirs for drinking water and recreation, 
             flood protection, and generation of reliable, on-demand renewable hydropower, critical to reducing emissions that contribute to climate change and poor human health. Dams across the U.S. are aging and pose potential safety hazards, increasing the need for 
             regular maintenance or more extensive repair. Dams may interrupt flows and prevent sea-run fish passage, contributing to large population declines. They may also contribute to poor water quality downstream, increased predation, and climate change. Dams have 
             long threatened indigenous cultural heritage in the U.S., while at the same time helping to shape post-industrial community identity over the last two centuries.<br>\
			       
			       <br>For this activity, imagine that the future of a watershed is directly in your hands. You (the decision maker) are personally tasked with using your professional expertise to make sustainable decisions for a set of dams within a given watershed. 
             In this MCDA tool, you will select a numeric score for each decision criterion (e.g., annuitized cost, greenhouse gas emissions reductions, sea-run fish survival, etc.) involved in each decision alternative (e.g., remove dam, keep dam, improve fish 
             passage facilities, etc.). The output from the MCDA tool will be a visual ranking of decision alternatives based on your preferences for decision criteria.<br>\
			       
			       <br>Your Task: What do you envision for the future of this set of dams? Your task is to consider each of the decision alternatives for decisions across the set of dams:<br>\
			       <br>(1) Remove dam <br>\
			       (2) Improve fish passage facilities<br>\
			       (3) Upgrade or replace turbines<br>\
			       (4) Install turbines or expand power capacity<br>\
			       (5) Keep and maintain dam<br>\
			       
			       <br>Please also consider the following decision criteria, to be applied in some combination for the set of dams:<br>\
			       
			       <br>(1) Fish survival (thousands of lbs or metric tonnes per acre): proxy criteria estimated as sea-run fish (Atlantic salmon, Alewife, Blueback herring, American eel) biomass calculated using functional habitat units (Roy et al., 2018).<br>\
			       (2) River recreation area (square miles or kilometers): estimated area of river that may increase or decrease with a dam decision alternative, combines functional area for whitewater and flatwater recreation defined by Roy et al. (2018).<br>\
			       (3) Reservoir storage (cubic miles or kilometers): estimated storage potential of the reservoir, based on its volume (Roy et al., 2018).<br>\
			       (4) Annuitized project costs (2018 $USD): estimated total project costs (capital and operation & maintenance) on an annual basis using a 10% discount rate.<br>\
			       (5) Number of properties impacted: estimated number based on potential changes in viewshed or property value (Roy et al., 2018).<br>\
			       (6) Breach damage potential (unitless): a proxy for safety based on the State hazard rating, which indicates the potential for downstream property damage, injury, and death in the case of dam breach.<br>\
			       (7) Annual electricity generation (MWh/yr): estimate based on nameplate capacity.<br>\
			       (8) Greenhouse gas emissions reduction (lbs or metric tonnes per year): estimated based on avoided fossil fuel-generated electricity, using the State's energy mix.<br>\
			       (9) Indigenous cultural heritage (unitless): to convey the importance of the decision alternative for preserving/restoring the culture of indigenous people.<br>\
			       (10) Town/city identity (unitless): rating provided by decision-maker to convey the importance of the decision alternative for preserving the existing identity of the community of town/city residents.<br>\
			       (11) Industrial historical value (unitless): rating provided by the decision maker to convey the importance of the decision alternative for preserving/restoring the industrial historical value of the infrastructure.<br>\
			       (12) Aesthetic value (unitless): rating provided by the decision maker to convey the importance of the decision alternative for improving or preserving the aesthetics (e.g, appearance, scenic value, smell, sound).<br>\
			       
			       <br> Toggle through the ALTERNATIVE pages at left to compare criteria under a single decision alternative and click UPDATE button to view alternative-specific results and mark the alternative COMPLETE. After you have finished rating criteria under every ALTERNATIVE tab, 
            select the OUTPUT tab and click GENERATE to view the results.<br>\
            <br><b>\Developers\<b><br>\
            <br>Emma Fox- Lead Developer (Ph.D. candidate, University of Maine Ecology and Environmental Science Program). Designed initial user interface and server functionality based on Raymond & Klein (2018). Adjusted WSM function for new dam decision application and advised model-related changes. 
            Designed and wrote app text, and designed accompanying multi-dam decision example fact sheets.<br>\
            <br>Sharon Klein- Development Advisor (Associate Professor, University of Maine School of Economics). Advised user-friendliness enhancements to WSM model and UI/UX, refined criteria definitions, revised app text.<br>\
			      <br>William Winslow - Developer (Software Engineer, GeoSpatial Science Center(GSSC), University of New Hampshire). Deployment (Docker, Apache), server code reorganization, debugging/bug fixes, misc. feature implementations (UI/UX).<br>\
			      <br>Garrett Raymond- Technical Consultant. Built WSM function in R and provided basic web app design. See also: Raymond, G. and Klein, S. (2018). Web App: Multi-Criteria Decision Analysis of Fuel Pathways.https://fuel-production-pathway-comparison-tool.shinyapps.io/gr_ui_sep_models/ <br>\
            <br>Support for the Future of Dams project provided by the National Science Foundation's Research Infrastructure Improvement NSF #IIA-1539071, USDA National Institute of Food and Agriculture, Hatch project 0230040, and Department of the Interior, 
            U.S. Geological Survey Grant No. G16AP00057 through the Senator George J. Mitchell Center at the University of Maine.
						Data Discovery Center at the University of New Hampshire- host for the Shiny web app. https://ddc.unh.edu <br>")),

		tabPanel(
				#"Alternative 1: Remove Dam",
				htmlOutput("Alt1"), # status and title
				HTML("When a dam is removed, water is allowed to flow (somewhat) freely downstream, creating greater connectivity for fish passage and river recreation, bolstering sea-run fish populations, and improving benthic (riverbed) aquatic communities. Dam removal may increase local water quality, 
            regulate water temperature, and provide additional tourism/fishing opportunities. The river and its tributaries will likely return to their \"natural\" flows. However, dam removal may also create temporary mud flats as the reservoir empties, or release toxic or harmful impounded sediments.
            Dam removal eliminates lake-dwelling wildlife habitat and local flatwater recreation opportunities, reduces overall reservoir storage volume, and lowers total annual hydropower generation. Dam removal costs are typically high for dam removals, with no payback in terms of direct market returns. <br>\
            <br>Warning: decision criteria ratings must sum to 1! The tracking indicator will help you keep track of the sum. Be aware that decision criteria are directly compensating (i.e., if the sum of all ratings is 1, then increasing the rating on one criterion requires another crterion rating to decrease
            in order to keep the sum equal to 1). <br>\
            <br> Please consider and rate the given set of decision criteria based on the case of removing the dam. In each case, 0 = not at all important and 1 = extremely important.<br>"),

				htmlOutput("Alt1Progress"),

				#Fish Survival
				sliderInput(inputId = "FishBiomass1", label = "Fish Survival (thousands of lbs or metric tonnes per acre): proxy criteria estimated as sea-run fish (Atlantic salmon, Alewife, Blueback herring, American eel) biomass calculated using functional habitat units (Roy et al., 2018).",
				            value=0, min=0, max=1, step = 0.05),
				#River Recreation
				sliderInput(inputId = "RiverRec1", label = "River Recreation Area (square miles or kilometers): estimated area of river that may increase or decrease with a dam decision alternative, combines functional area for whitewater and flatwater recreation defined by Roy et al. (2018).",
				            value=0, min=0, max=1, step = 0.05),
				#Reservoir Storage
				sliderInput(inputId = "Reservoir1", label = "Reservoir Storage (cubic miles or kilometers): estimated storage potential of the reservoir, based on its volume (Roy et al., 2018).",
				            value=0, min=0, max=1, step = 0.05),
				#Annuitized Project Costs
				sliderInput(inputId = "ProjectCost1", label = "Annuitized Project Costs (2018 $USD): estimated total project costs (capital and operation & maintenance) on an annual basis using a 10% discount rate.",
				            value=0, min=0, max=1, step = 0.05),
				#Breach Damage Potential
				sliderInput(inputId = "Safety1", label = "Breach Damage Potential (unitless): a proxy for safety based on the State hazard rating, which indicates the potential for downstream property damage, injury, and death in the case of dam breach.",
				            value=0, min=0, max=1, step = 0.05),
				#Number of Properties
				sliderInput(inputId = "NumProperties1", label = " Number of Properties Impacted: estimated number based on potential changes in viewshed or property value (Roy et al., 2018).",
				            value=0, min=0, max=1, step = 0.05),
				#Annual Electricity Generation
				sliderInput(inputId = "ElectricityGeneration1", label = "Annual Electricity Generation (MWh/yr): data from FERC licenses; interpolated data estimated based on nameplate capacity and capacity factor assumptions.",
				            value=0, min=0, max=1, step = 0.05),
				#GHGEmissions
				sliderInput(inputId = "AvoidEmissions1", label = "Greenhouse Gas Emissions Reduction (lbs or metric tonnes per year): estimated based on avoided fossil fuel-generated electricity, using the State's energy mix.", 
				            value=0, min=0, max=1, step = 0.05),
				#IndigenousHeritage
				sliderInput(inputId = "IndigenousHeritage1", label = "Indigenous Cultural Heritage (unitless): to convey the importance of the decision alternative for preserving/restoring the culture of indigenous people.", 
				            value=0, min=0, max=1, step = 0.05),
				#IndustrialHistory
				sliderInput(inputId = "IndustrialHistory1", label = " Industrial Historical Value (unitless): rating provided by the decision maker to convey the importance of the decision alternative for preserving/restoring the industrial historical value of the infrastructure.", 
				            value=0, min=0, max=1, step = 0.05),
				#CommunityIdentity
				sliderInput(inputId = "CommunityIdentity1", label = "Town/City Identity (unitless): rating provided by decision-maker to convey the importance of the decision alternative for preserving the existing identity of the community of town/city residents.", 
				            value=0, min=0, max=1, step = 0.05),
				#Aesthetics
				sliderInput(inputId = "Aesthetics1", label = " Aesthetic Value (unitless): rating provided by the decision maker to convey the importance of the decision alternative for improving or preserving the aesthetics (e.g, appearance, scenic value, smell, sound).", 
				            value=0, min=0, max=1, step = 0.05),

				actionButton("updateBtn1", "Update"),

				# output post generate
				div(id="alt-1-output",
					h2("Raw Scores of Alternative 1"),
					plotOutput("SummPlot1", height=graph_height, width=graph_width)
				)
			), # end
		#End Alternative 1: Remove Dam Tab

		tabPanel(
				#"Alternative 2: Improve Fish Passage Facilities",
				htmlOutput("Alt2"), # status and title
				HTML("When improvements are made to a dam's fish passage using state-of-the-art facilities, it may increase survival for one or more sea-run fish species within the watershed and improve angling in the river. Improvements to fish passage may even provide learning opportunities
            for citizens and students. However, annual electricity generation may be diminished (depending on the technology selected to pass fish), and costs for state-of-the-art fish passage facilities are typically high. Fish passage facility improvements may be required by law 
            depending on the species migrating in the waterway, and additional improvements may become required as other species become threatened or endangered. In the case where owners are required to improve passage for sea-run fish species, owners must bear the cost or risk surrendering 
            the dam operation license. <br>\
            <br>Warning: decision criteria ratings must sum to 1! The tracking indicator will help you keep track of the sum. Be aware that decision criteria are directly compensating (i.e., if the sum of all ratings is 1, then increasing the rating on one criterion requires another criterion 
            rating to decrease in order to keep the sum equal to 1). <br>\
            <br>Please consider and rate the given set of criteria based on the case of improvements to fish passage facilities at the dam. In each case, 0 = not at all important and 1 = extremely important.<br>"),

				htmlOutput("Alt2Progress"),

				#Fish Survival
				sliderInput(inputId = "FishBiomass2", label = "Fish Survival (thousands of lbs or metric tonnes per acre): proxy criteria estimated as sea-run fish (Atlantic salmon, Alewife, Blueback herring, American eel) biomass calculated using functional habitat units (Roy et al., 2018).", 
				            value=0, min=0, max=1, step = 0.05),
				#River Recreation
				sliderInput(inputId = "RiverRec2", label = "River Recreation Area (square miles or kilometers): estimated area of river that may increase or decrease with a dam decision alternative, combines functional area for whitewater and flatwater recreation defined by Roy et al. (2018).", 
				            value=0, min=0, max=1, step = 0.05),
				#Reservoir Storage
				sliderInput(inputId = "Reservoir2", label = "Reservoir Storage (cubic miles or kilometers): estimated storage potential of the reservoir, based on its volume (Roy et al., 2018).", 
				            value=0, min=0, max=1, step = 0.05),
				#Annuitized Project Costs
				sliderInput(inputId = "ProjectCost2", label = "Annuitized Project Costs (2018 $USD): estimated total project costs (capital and operation & maintenance) on an annual basis using a 10% discount rate.", 
				            value=0, min=0, max=1, step = 0.05),
				#Breach Damage Potential
				sliderInput(inputId = "Safety2", label = "Breach Damage Potential (unitless): a proxy for safety based on the State hazard rating, which indicates the potential for downstream property damage, injury, and death in the case of dam breach.", 
				            value=0, min=0, max=1, step = 0.05),
				#Number of Properties
				sliderInput(inputId = "NumProperties2", label = " Number of Properties Impacted: estimated number based on potential changes in viewshed or property value (Roy et al., 2018).", 
				            value=0, min=0, max=1, step = 0.05),
				#Annual Electricity Generation
				sliderInput(inputId = "ElectricityGeneration2", label = "Annual Electricity Generation (MWh/yr): data from FERC licenses; interpolated data estimated based on nameplate capacity and capacity factor assumptions.", 
				            value=0, min=0, max=1, step = 0.05),
				#GHGEmissions
				sliderInput(inputId = "AvoidEmissions2", label = "Greenhouse Gas Emissions Reduction (lbs or metric tonnes per year): estimated based on avoided fossil fuel-generated electricity, using the State's energy mix.", 
				            value=0, min=0, max=1, step = 0.05),
				#IndigenousHeritage
				sliderInput(inputId = "IndigenousHeritage2", label = "Indigenous Cultural Heritage (unitless): to convey the importance of the decision alternative for preserving/restoring the culture of indigenous people.", 
				            value=0, min=0, max=1, step = 0.05),
				#IndustrialHistory
				sliderInput(inputId = "IndustrialHistory2", label = " Industrial Historical Value (unitless): rating provided by the decision maker to convey the importance of the decision alternative for preserving/restoring the industrial historical value of the infrastructure.", 
				            value=0, min=0, max=1, step = 0.05),
				#CommunityIdentity
				sliderInput(inputId = "CommunityIdentity2", label = "Town/City Identity (unitless): rating provided by decision-maker to convey the importance of the decision alternative for preserving the existing identity of the community of town/city residents.", 
				            value=0, min=0, max=1, step = 0.05),
				#Aesthetics
				sliderInput(inputId = "Aesthetics2", label = " Aesthetic Value (unitless): rating provided by the decision maker to convey the importance of the decision alternative for improving or preserving the aesthetics (e.g, appearance, scenic value, smell, sound).", 
				            value=0, min=0, max=1, step = 0.05),
				actionButton("updateBtn2", "Update"),

				div(id="alt-2-output",
					h2("Raw Scores of Alternative 2"),
					plotOutput("SummPlot2", height=graph_height, width=graph_width)
				 )
			), # end
		#End Alternative 2: Improve Fish Passage Facilities Tab


		tabPanel(
				#"Alternative 3: Upgrade or Replace Turbines",
				htmlOutput("Alt3"), # status and title
				HTML("Upgrading or replacing turbines improves longevity for a hydropower plant. Costs for turbine upgrade or replacement are recouped through improved efficiency in operation over the lifetime of each hydropower project. Nameplate hydropower capacity at the dam may increase 
          with turbine replacement or upgrade. However, when turbines are upgraded or replaced, the impact to sea-run fish survival is unclear. If new turbines are fast moving or impulse driven (where a nozzle sprays high-pressure water at buckets on a runner to turn the turbine),
          fish may be more at risk than if turbines are slow moving or reaction driven (where flow and pressure of water over angled blades on a runner) turbine. <br>\
          <br> Warning: decision criteria ratings must sum to 1! The tracking indicator will help you keep track of the sum. Be aware that decision criteria are directly compensating (i.e., if the sum of all ratings is 1, then increasing the rating on one criterion requires another criterion 
          rating to decrease in order to keep the sum equal to 1). <br>\ 
				  <br> Please consider and rate the given set of criteria based on the case of improved or upgraded turbines at the dam. In each case, 0 = not at all important and 1 = extremely important.<br>"),

				htmlOutput("Alt3Progress"),

				#Fish Survival
				sliderInput(inputId = "FishBiomass3", label = "Fish Survival (thousands of lbs or metric tonnes per acre): proxy criteria estimated as sea-run fish (Atlantic salmon, Alewife, Blueback herring, American eel) biomass calculated using functional habitat units (Roy et al., 2018).", 
				            value=0, min=0, max=1, step = 0.05),
				#River Recreation
				sliderInput(inputId = "RiverRec3", label = "River Recreation Area (square miles or kilometers): estimated area of river that may increase or decrease with a dam decision alternative, combines functional area for whitewater and flatwater recreation defined by Roy et al. (2018).", 
				            value=0, min=0, max=1, step = 0.05),
				#Reservoir Storage
				sliderInput(inputId = "Reservoir3", label = "Reservoir Storage (cubic miles or kilometers): estimated storage potential of the reservoir, based on its volume (Roy et al., 2018).", 
				            value=0, min=0, max=1, step = 0.05),
				#Annuitized Project Costs
				sliderInput(inputId = "ProjectCost3", label = "Annuitized Project Costs (2018 $USD): estimated total project costs (capital and operation & maintenance) on an annual basis using a 10% discount rate.", 
				            value=0, min=0, max=1, step = 0.05),
				#Breach Damage Potential
				sliderInput(inputId = "Safety3", label = "Breach Damage Potential (unitless): a proxy for safety based on the State hazard rating, which indicates the potential for downstream property damage, injury, and death in the case of dam breach.", 
				            value=0, min=0, max=1, step = 0.05),
				#Number of Properties
				sliderInput(inputId = "NumProperties3", label = " Number of Properties Impacted: estimated number based on potential changes in viewshed or property value (Roy et al., 2018).", 
				            value=0, min=0, max=1, step = 0.05),
				#Annual Electricity Generation
				sliderInput(inputId = "ElectricityGeneration3", label = "Annual Electricity Generation (MWh/yr): data from FERC licenses; interpolated data estimated based on nameplate capacity and capacity factor assumptions.", 
				            value=0, min=0, max=1, step = 0.05),
				#GHGEmissions
				sliderInput(inputId = "AvoidEmissions3", label = "Greenhouse Gas Emissions Reduction (lbs or metric tonnes per year): estimated based on avoided fossil fuel-generated electricity, using the State's energy mix.", 
				            value=0, min=0, max=1, step = 0.05),
				#IndigenousHeritage
				sliderInput(inputId = "IndigenousHeritage3", label = "Indigenous Cultural Heritage (unitless): to convey the importance of the decision alternative for preserving/restoring the culture of indigenous people.", 
				            value=0, min=0, max=1, step = 0.05),
				#IndustrialHistory
				sliderInput(inputId = "IndustrialHistory3", label = " Industrial Historical Value (unitless): rating provided by the decision maker to convey the importance of the decision alternative for preserving/restoring the industrial historical value of the infrastructure.", 
				            value=0, min=0, max=1, step = 0.05),
				#CommunityIdentity
				sliderInput(inputId = "CommunityIdentity3", label = "Town/City Identity (unitless): rating provided by decision-maker to convey the importance of the decision alternative for preserving the existing identity of the community of town/city residents.", 
				            value=0, min=0, max=1, step = 0.05),
				#Aesthetics
				sliderInput(inputId = "Aesthetics3", label = " Aesthetic Value (unitless): rating provided by the decision maker to convey the importance of the decision alternative for improving or preserving the aesthetics (e.g, appearance, scenic value, smell, sound).", 
				            value=0, min=0, max=1, step = 0.05),

				actionButton("updateBtn3", "Update"),

				div(id="alt-3-output",
					h2("Raw Scores of Alternative 3"),
					plotOutput("SummPlot3", height=graph_height, width=graph_width)
				)
			), # end
		#End Alternative 3: Upgrade or Replace Turbines Tab


		tabPanel(
				#"Alternative 4: Install Turbines or Expand Power Capacity",
				htmlOutput("Alt4"), # status and title
				HTML("When new turbines are installed on existing non-powered dam infrastructure, or hydropower capacity is increased at a powered dam, annual hydropower generation increases. Costs may be recouped through market returns over the project's lifetime, and the change in the dam's 
              operation may present opportunities for whitewater recreation downstream (dam releases are popular for river rafting). However, installing turbines or expanding existing power capacity may alter flows and confuse sea-run fish species. Fish may become caught in the grates 
              protecting system intakes, or even killed by turbine blades or rapid changes in pressure if they are small enough to move through the powerhouse. Actual reservoir storage may change based on overall hydropower operations.<br>\
              <br>Warning: decision criteria ratings must sum to 1! The tracking indicator will help you keep track of the sum. Be aware that decision criteria are directly compensating (i.e., if the sum of all ratings is 1, then increasing the rating on one criterion requires another 
              criterion rating to decrease in order to keep the sum equal to 1). <br>\
              Please consider and rate the given set of criteria based on the case of new turbine installation or expansion of existing hydropower capacity at the dam. In each case, 0 = not at all important and 1 = extremely important.<br>"),

				htmlOutput("Alt4Progress"),

				#Fish Survival
				sliderInput(inputId = "FishBiomass4", label = "Fish Survival (thousands of lbs or metric tonnes per acre): proxy criteria estimated as sea-run fish (Atlantic salmon, Alewife, Blueback herring, American eel) biomass calculated using functional habitat units (Roy et al., 2018).", 
				            value=0, min=0, max=1, step = 0.05),
				#River Recreation Area
				sliderInput(inputId = "RiverRec4", label = "River Recreation Area (square miles or kilometers): estimated area of river that may increase or decrease with a dam decision alternative, combines functional area for whitewater and flatwater recreation defined by Roy et al. (2018).", 
				            value=0, min=0, max=1, step = 0.05),
				#Reservoir Storage
				sliderInput(inputId = "Reservoir4", label = "Reservoir Storage (cubic miles or kilometers): estimated storage potential of the reservoir, based on its volume (Roy et al., 2018).", 
				            value=0, min=0, max=1, step = 0.05),
				#Annuitized Project Costs
				sliderInput(inputId = "ProjectCost4", label = "Annuitized Project Costs (2018 $USD): estimated total project costs (capital and operation & maintenance) on an annual basis using a 10% discount rate.", 
				            value=0, min=0, max=1, step = 0.05),
				#Breach Damage Potential
				sliderInput(inputId = "Safety4", label = "Breach Damage Potential (unitless): a proxy for safety based on the State hazard rating, which indicates the potential for downstream property damage, injury, and death in the case of dam breach.", 
				            value=0, min=0, max=1, step = 0.05),
				#Number of Properties
				sliderInput(inputId = "NumProperties4", label = " Number of Properties Impacted: estimated number based on potential changes in viewshed or property value (Roy et al., 2018).", 
				            value=0, min=0, max=1, step = 0.05),
				#Annual Electricity Generation
				sliderInput(inputId = "ElectricityGeneration4", label = "Annual Electricity Generation (MWh/yr): data from FERC licenses; interpolated data estimated based on nameplate capacity and capacity factor assumptions.", 
				            value=0, min=0, max=1, step = 0.05),
				#GHGEmissions
				sliderInput(inputId = "AvoidEmissions4", label = "Greenhouse Gas Emissions Reduction (lbs or metric tonnes per year): estimated based on avoided fossil fuel-generated electricity, using the State's energy mix.", 
				            value=0, min=0, max=1, step = 0.05),
				#IndigenousHeritage
				sliderInput(inputId = "IndigenousHeritage4", label = "Indigenous Cultural Heritage (unitless): to convey the importance of the decision alternative for preserving/restoring the culture of indigenous people.", 
				            value=0, min=0, max=1, step = 0.05),
				#IndustrialHistory
				sliderInput(inputId = "IndustrialHistory4", label = " Industrial Historical Value (unitless): rating provided by the decision maker to convey the importance of the decision alternative for preserving/restoring the industrial historical value of the infrastructure.", 
				            value=0, min=0, max=1, step = 0.05),
				#CommunityIdentity
				sliderInput(inputId = "CommunityIdentity4", label = "Town/City Identity (unitless): rating provided by decision-maker to convey the importance of the decision alternative for preserving the existing identity of the community of town/city residents.", 
				            value=0, min=0, max=1, step = 0.05),
				#Aesthetics
				sliderInput(inputId = "Aesthetics4", label = " Aesthetic Value (unitless): rating provided by the decision maker to convey the importance of the decision alternative for improving or preserving the aesthetics (e.g, appearance, scenic value, smell, sound).", 
				            value=0, min=0, max=1, step = 0.05),
				actionButton("updateBtn4", "Update"),

				div(id="alt-4-output",
					h2("Raw Scores of Alternative 4"),
					plotOutput("SummPlot4", height=graph_height, width=graph_width)
				)
			), # end
		#End Alternative 4: Install Turbines or Expand Power Capacity Tab

		tabPanel(
				#"Alternative 5: Keep and Maintain Dam",
				htmlOutput("Alt5"), # status and title
				HTML("Keeping and maintaining the dam is the lowest-cost option. Keeping and maintaining the dam may appeal to parties interested in preserving the area's industrial history, preserving the town/city identity for community residents (if local identity is closely tied to the dam), 
          or preserving the aesthetic value of the impoundment. Maintenance costs may be recouped somewhat if the dam is powered; however, refurbishment, restoration, or maintenance to a non-powered dam presents no direct opportunity for cost offset. Keeping the dam will likely have no 
          impact on reservoir storage volume, river recreation area, annual electricity generation, or number of properties abutting the reservoir. The impoundment will continue to present a barrier to sea-run fish species, thereby negatively impacting their survival. And, in the long run, 
          the dam is a temporary piece of infrastructure that must be removed. Decision criteria ratings must sum to 1! The tracking indicator will help you keep track of the sum. Be aware that decision criteria are directly compensating (i.e., if the sum of all ratings is 1, then 
          increasing the rating on one criterion requires another criterion rating to decrease in order to keep the sum equal to 1). <br>\ 
				  <br> Please consider and rate the given set of criteria based on the case of refurbishment or maintenance at the dam. In each case, 0 = not at all important and 1 = extremely important.<br>"),

				htmlOutput("Alt5Progress"),

				#Fish Survival
				sliderInput(inputId = "FishBiomass5", label = "Fish Survival (thousands of lbs or metric tonnes per acre): proxy criteria estimated as sea-run fish (Atlantic salmon, Alewife, Blueback herring, American eel) biomass calculated using functional habitat units (Roy et al., 2018).", 
				            value=0, min=0, max=1, step = 0.05),
				#River Recreation
				sliderInput(inputId = "RiverRec5", label = "River Recreation Area (square miles or kilometers): estimated area of river that may increase or decrease with a dam decision alternative, combines functional area for whitewater and flatwater recreation defined by Roy et al. (2018).", 
				            value=0, min=0, max=1, step = 0.05),
				#Reservoir Storage
				sliderInput(inputId = "Reservoir5", label = "Reservoir Storage (cubic miles or kilometers): estimated storage potential of the reservoir, based on its volume (Roy et al., 2018).", 
				            value=0, min=0, max=1, step = 0.05),
				#Annuitized Project Costs
				sliderInput(inputId = "ProjectCost5", label = "Annuitized Project Costs (2018 $USD): estimated total project costs (capital and operation & maintenance) on an annual basis using a 10% discount rate.", 
				            value=0, min=0, max=1, step = 0.05),
				#Breach Damage Potential
				sliderInput(inputId = "Safety5", label = "Breach Damage Potential (unitless): a proxy for safety based on the State hazard rating, which indicates the potential for downstream property damage, injury, and death in the case of dam breach.", 
				            value=0, min=0, max=1, step = 0.05),
				#Number of Properties
				sliderInput(inputId = "NumProperties5", label = " Number of Properties Impacted: estimated number based on potential changes in viewshed or property value (Roy et al., 2018).", 
				            value=0, min=0, max=1, step = 0.05),
				#Annual Electricity Generation
				sliderInput(inputId = "ElectricityGeneration5", label = "Annual Electricity Generation (MWh/yr): data from FERC licenses; interpolated data estimated based on nameplate capacity and capacity factor assumptions.", 
				            value=0, min=0, max=1, step = 0.05),
				#GHGEmissions
				sliderInput(inputId = "AvoidEmissions5", label = "Greenhouse Gas Emissions Reduction (lbs or metric tonnes per year): estimated based on avoided fossil fuel-generated electricity, using the State's energy mix.", 
				            value=0, min=0, max=1, step = 0.05),
				#IndigenousHeritage
				sliderInput(inputId = "IndigenousHeritage5", label = "Indigenous Cultural Heritage (unitless): to convey the importance of the decision alternative for preserving/restoring the culture of indigenous people.", 
				            value=0, min=0, max=1, step = 0.05),
				#IndustrialHistory
				sliderInput(inputId = "IndustrialHistory5", label = " Industrial Historical Value (unitless): rating provided by the decision maker to convey the importance of the decision alternative for preserving/restoring the industrial historical value of the infrastructure.", 
				            value=0, min=0, max=1, step = 0.05),
				#CommunityIdentity
				sliderInput(inputId = "CommunityIdentity5", label = "Town/City Identity (unitless): rating provided by decision-maker to convey the importance of the decision alternative for preserving the existing identity of the community of town/city residents.", 
				            value=0, min=0, max=1, step = 0.05),
				#Aesthetics
				sliderInput(inputId = "Aesthetics5", label = " Aesthetic Value (unitless): rating provided by the decision maker to convey the importance of the decision alternative for improving or preserving the aesthetics (e.g, appearance, scenic value, smell, sound).", 
				            value=0, min=0, max=1, step = 0.05),
				actionButton("updateBtn5", "Update"),

				div(id="alt-5-output",
					h2("Raw Scores of Alternative 5"),
					plotOutput("SummPlot5", height=graph_height, width=graph_width)
				)
			), # end
		#End Alternative 5: Keep and Maintain Dam Tab

		tabPanel("Output",
				#TODO:?  could warn user here if they havent completed everything
				HTML("<br><b>After completing all Alternatives use this button to get results:</b>"),

				# event
				# TODO:? we could make event fire by checking if all alternatives have been 'updated'
				actionButton("generateMatrix", "Generate"),

				# uncomment for debugg output helper button
				#HTML("<br><b>(for debugging output) auto complete all alternatives and generate:</b>"),
				#actionButton("autoGenerateMatrix", "Auto Complete"),

				# output post generate
				div(id="generated-output",
					#h2('All Raw Scores'),
					#tableOutput("FilledCriteriaTable"), # for debugging criteria table

					h2('Weighted Sum Matrix Output'),
					tableOutput("WSMTable"),

					h2('Summed Scores'),
					plotOutput("WSMPlot", height=300, width=1000),

					# download
					h3('Download Results'),
					downloadButton("downloadData", "Download")
				),

		id = "tabs"
    )
)))

# create the application with ui in this file and imported server from server.R
shinyApp(ui, server)
