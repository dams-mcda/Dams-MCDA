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

# multiple used labels
fishSurvivalLabel <- "Fish survival is measured in thousands of lbs or metric tonnes. It is estimated as sea-run fish (Atlantic salmon, Alewife, Blueback herring, American eel) biomass calculated using functional habitat units (Roy et al., 2018). "
riverRecLabel <- "River recreation is measured in square miles or kilometers. It is the estimated area of river that may increase or decrease with a dam decision alternative, combines functional area for whitewater and flatwater recreation defined by Roy et al. (2018)."
resStorageLabel <- "Reservoir storage is measured in cubic miles or kilometers. It is the estimated storage potential of the reservoir, based on its volume (Roy et al., 2018)."
annuitizedProjCostsLabel <- "Annuitized project cost is measured in 2018 $USD. It is the estimated total project costs (capital and operation & maintenance) on an annual basis using a 6.2% discount rate and a 20-year lifetime."
numPropertiesLabel <- "Number of properties is the estimated number of properties impcted by the decision alternative, based on potential changes in viewshed or property value (Roy et al., 2018). "
breachDamageLabel <- "Breach damange potential is a unitless proxy for safety based on the State hazard rating, which indicates the potential for downstream property damage, injury, and death in the case of dam breach (Roy et al., 2018)."
annualElectricityLabel <- "Annual electricity generation is measured in MWh/year. average estimate based on nameplate capacity from FERC licenses for each hydropower project."
GHGEmissionsLabel <- "Annual carbon dioxide (CO2) emissions reduction is measured in lbs or metric tonnes of CO2/year. It is an estimate of avoided carbon dioxide emissions from annual hydropower-generated electricity production; based on decreasing generation from the State's electricity generation mix; does not include life cycle emissions impacts."
indigenousHeritageLabel <- "Indigenous cultural heritage is a unitless measure used as a proxy for the importance of the decision alternative for preserving/restoring the culture of indigenous people."
communityIdentityLabel <- "Town/City identity is a unitless measure used to convey the importance of the decision alternative for preserving the existing identity of the community of town/city residents."
industrialHistoryLabel <- "Industrial historical value is a unitless measure to convey the imprtance of the decision alternative for preserving/restoring the industrial historical value of the infrastructure."
aestheticsLabel <- "Aesthetics is a unitless measure to convey the importance of the decision alternative for improving or preserving the aesthetics (e.g, appearance, scenic value, smell, sound)."



# Define UI for Shiny web application
ui <- shinyUI(fluidPage(
	# used for toggling display of elements
	shinyjs::useShinyjs(),

	# link css
	tags$head(
		tags$link(rel = "stylesheet", type = "text/css", href = "dams_mcda.css")
	),

	titlePanel("Dam Decision Support Tool"),

	navlistPanel(
		# Define layout widths
		widths = c(4,8),

		#Define Instructions tab
		tabPanel("Start Here",
			htmlOutput("Introduction"),
			h2("Introduction"),
			HTML(
				 "The Dam Decision Support Tool supports preference elicitation for dam decision support, using a set of decision criteria and decision alternatives identified using data from dam decision-maker interviews. The tool uses an approach consistent with the \
				 Weighted Sum approach to Multi-Criteria Decision Analysis (MCDA) to compare decision-maker preferences for decision criteria over a set of decision alternatives. Although this tool may be used on its own to elicit user preferences about dam decisions in \
				 general, it was designed to be used as part of the Dam Toolbox in an interactive workshop setting.<br> \

				 <br><b>Picture a watershed: </b>rain falls and runs downhill toward tributaries that flow into the river and ultimately to the ocean. This watershed is home to valuable ecosystem services, including pristine natural lakes, clean water sources, and significant \
						 biodiversity, including several sea-run fish species (e.g. Atlantic salmon, American eel, Blueback herring, and Alewife). The river and its tributaries are home to many dams. Dams provide important services: reservoirs for drinking water and recreation, \
						 flood protection, and generation of reliable, on-demand renewable hydropower, critical to reducing emissions that contribute to climate change and poor human health. However, dams across the U.S. are aging and pose potential safety hazards, increasing the\
				 need for regular maintenance or more extensive repair. Dams may interrupt flows and prevent sea-run fish passage, contributing to large population declines. They may also contribute to poor water quality downstream, increased predation, and climate change.\
				 Dams have long threatened indigenous cultural heritage in the U.S., while at the same time helping to shape post-industrial town or city identities over the last two centuries.<br>\

						 <br>In this Dam Decision Support Tool, <b>imagine that the future of a watershed is directly in your hands</b>. You (the decision maker) are personally tasked with using your professional expertise to make sustainable decisions for a set of dams within a \
				 given watershed. In each of the Alternative tabs, you will select a numeric rating for each decision criterion (e.g., annuitized cost, greenhouse gas emissions reductions, sea-run fish survival, etc.) involved in each decision alternative (e.g., remove dam,\
				 keep and maintain dam, improve fish passsage).<br>\

				 <h2>Specific Instructions</h2>\

				 Toggle through the DECISION ALTERNATIVE pages at left to compare decision criteria under a single decision alternative and click the UPDATE button at the bottom of each ALTERNATIVE page to view decision alternative-specific\
				 results and mark the alternative COMPLETE (i.e., to remove the red indicator message prompting user input). After you have finished rating decision criteria under ALL of the ALTERNATIVE tabs, select the OUTPUT tab and click GENERATE to view results. More\
				 information about the specific DECISION ALTERNATIVES and DECISION CRITERIA you will be considering during this exercise are listed below. If you are using this Dam Decision Support Tool  as a part of an interactive workshop facilitated by Future of Dams \

				 <h3>Decision Alternatives</h3> \

				 <ol>\
					 <li> Remove dam: dam is removed completely from the river, allowing water to flow freely </li>\
					 <li> Improve fish passage: some type of fish passage technology is installed (e.g., state-of-the-art fish lift/elevator, eel ladder, etc).</li> \
					 <li> Improve hydropower generation: (e.g., install turbines, upgrade turbines, or expand power capacity): hydropower generation capacity is increased, whether by installing new capacity or by upgrading turbines to larger power capacities or higher efficiency ratings; includes powered and non-powered dams.</li> \
					 <li> Improve hydropower generation AND fish passage: some type of fish passage technology is installed AND hydropower generation capacity is increased.</li> \
					 <li> Keep and maintain dam: this is the do-nothing option, where the dam remains in place and minimal costs are incurred to ensure dam structural integrity and safety compliance.</li> \
				 </ol>\

				 <h3>Decision Criteria</h3>\

				 <ol>\
					 <li> \
						 Fish survival (thousands of lbs or metric tonnes per acre): proxy criteria estimated as sea-run fish (Atlantic salmon, Alewife, Blueback herring, American eel) biomass calculated using functional habitat units (Roy et al., 2018). \
					 </li>\
					 <li> \
						 River recreation area (square miles or kilometers): estimated area of river that may increase or decrease with a dam decision alternative, combines functional area for whitewater and flatwater recreation defined by Roy et al. (2019). \
					 </li>\
					 <li> \
						 Reservoir storage (cubic miles or kilometers): estimated storage potential of the reservoir, based on its volume (Roy et al., 2018).\
					 </li>\
					 <li> \
						 Annuitized project costs (2018 $USD): estimated total project costs (capital and operation & maintenance) on an annual basis using a 6.2% discount rate and a 20 year lifetime. \
					 </li>\
					 <li> \
						 Number of properties impacted: estimated number of properties impacted by the decision alternative, based on potential changes in viewshed or property value (Roy et al., 2018). \
					 </li>\
					 <li> \
						 Breach damage potential (unitless): a proxy for safety based on the State hazard rating, which indicates the potential for downstream property damage, injury, and death in the case of dam breach (Roy et al., 2018). \
					 </li>\
					 <li> \
						 Annual electricity generation (MWh/yr): average estimate based on nameplate capacity from FERC licenses for each hydropower project.
					 </li>\
					 <li> \
						 Annual carbon dioxide (CO2) emissions reduction (lbs or metric tonnes per year): estimate of avoided carbon dioxide emissions from annual hydropower-generated electricity production; based on decreasing generation from the State's electricity generation mix; does not include life cycle emissions impacts. \
					 </li>\
					 <li> \
						 Indigenous cultural heritage (unitless): a proxy for the importance of the decision alternative for preserving/restoring the culture of indigenous people. \
					 </li>\
					 <li> \
						 Town/city identity (unitless): rating to convey the importance of the decision alternative for preserving the existing identity of the community of town/city residents. \
					 </li>\
					 <li> \
						 Industrial historical value (unitless): rating to convey the importance of the decision alternative for preserving/restoring the industrial historical value of the infrastructure. \
					 </li>\
					 <li> \
						 Aesthetic value (unitless): rating to convey the importance of the decision alternative for improving or preserving aesthetics (e.g., appearance, scenic value, smell, sound). \
					 </li>\
				 </ol><br>\
			 "
			 )
		),

		tabPanel(
			htmlOutput("Alt1"), # status and title
			h2("Alternative 1: Dam Removal"),
			HTML("When a dam is removed, water is allowed to flow (somewhat) freely downstream, creating greater connectivity for fish passage and river recreation, bolstering sea-run fish populations, and improving benthic (riverbed) aquatic communities. Dam removal may increase local water quality, \
				 regulate water temperature, and provide additional tourism/fishing opportunities. The river and its tributaries will likely return to their \"natural\" flows. However, dam removal may also create temporary mud flats as the reservoir empties, or release toxic or harmful impounded sediments. \
				 Dam removal eliminates lake-dwelling wildlife habitat and local flatwater recreation opportunities, reduces overall reservoir storage volume, and lowers total annual hydropower generation. Dam removal costs are typically high for dam removals, with no payback in terms of direct market returns. <br>\
				 <br>Warning: decision criteria ratings must sum to 1! The tracking indicator will help you keep track of the sum. Be aware that decision criteria are directly compensating (i.e., if the sum of all ratings is 1, then increasing the rating on one criterion requires another crterion rating to decrease in order to keep the sum equal to 1). <br>\
				 <br> Please consider and rate the given set of decision criteria based on the case of removing the dam. <b>In each case, 0 = not at all important and 1 = extremely important.</b><br>"
			),

			htmlOutput("Alt1Progress"),

			#Fish Survival
			sliderInput(inputId = "FishBiomass1", label = fishSurvivalLabel,
						value=0, min=0, max=1, step = 0.025),
			#River Recreation
			sliderInput(inputId = "RiverRec1", label = riverRecLabel,
						value=0, min=0, max=1, step = 0.025),
			#Reservoir Storage
			sliderInput(inputId = "Reservoir1", label = resStorageLabel,
						value=0, min=0, max=1, step = 0.025),
			#Annuitized Project Costs
			sliderInput(inputId = "ProjectCost1", label = annuitizedProjCostsLabel,
						value=0, min=0, max=1, step = 0.025),
			#Breach Damage Potential
			sliderInput(inputId = "Safety1", label = breachDamageLabel,
						value=0, min=0, max=1, step = 0.025),
			#Number of Properties
			sliderInput(inputId = "NumProperties1", label = numPropertiesLabel,
						value=0, min=0, max=1, step = 0.025),
			#Annual Electricity Generation
			sliderInput(inputId = "ElectricityGeneration1", label = annualElectricityLabel,
						value=0, min=0, max=1, step = 0.025),
			#GHGEmissions
			sliderInput(inputId = "AvoidEmissions1", label = GHGEmissionsLabel,
						value=0, min=0, max=1, step = 0.025),
			#IndigenousHeritage
			sliderInput(inputId = "IndigenousHeritage1", label = indigenousHeritageLabel,
						value=0, min=0, max=1, step = 0.025),
			#IndustrialHistory
			sliderInput(inputId = "IndustrialHistory1", label = industrialHistoryLabel,
						value=0, min=0, max=1, step = 0.025),
			#CommunityIdentity
			sliderInput(inputId = "CommunityIdentity1", label = communityIdentityLabel,
						value=0, min=0, max=1, step = 0.025),
			#Aesthetics
			sliderInput(inputId = "Aesthetics1", label = aestheticsLabel,
						value=0, min=0, max=1, step = 0.025),

			actionButton("updateBtn1", "Update"),

			# output post generate
			div(id="alt-1-output",
				h2("Raw Scores of Alternative 1"),
				plotOutput("SummPlot1", height=graph_height, width=graph_width)
			)
		), # end
		#End Alternative 1: Remove Dam Tab

		tabPanel(
			#"Alternative 2: Improve Fish Passage",
			htmlOutput("Alt2"), # status and title
			h2("Alternative 2: Improve Fish Passage Facilities"),
      
			HTML(
				"Improvements to a dam's fish passage may increase survival for one or more sea-run fish species within the watershed and improve angling in the river. Improvements to fish passage may even provide learning opportunities for citizens and students. However, annual electricity \
				generation may be diminished (depending on the technology selected to pass fish), and fish passage costs are typically high. Fish passage improvements may be required by law depending on the species migrating in the waterway, and additional improvements may become required as \
				other species become threatened or endangered. In the case where the owner is required to improve passage for sea-run fish species, the owners must bear the cost or risk surrendering the dam operation license. <br>\

				<br><b>Warning: decision criteria ratings must sum to 1!</b> The tracking indicator will help you keep track of the sum. Be aware that decision criteria are directly compensating (i.e., if the sum of all ratings is 1, then increasing the rating on one criterion requires another criterion \
				rating to decrease in order to keep the sum equal to 1). <br>\
				<br>Please consider and rate the given set of criteria based on the case of improvements to fish passage facilities at the dam. In each case, 0 = not at all important and 1 = extremely important.<br>"
			),

			htmlOutput("Alt2Progress"),

			#Fish Survival
			sliderInput(inputId = "FishBiomass2", label = fishSurvivalLabel,
						value=0, min=0, max=1, step = 0.025),
			#River Recreation
			sliderInput(inputId = "RiverRec2", label = riverRecLabel,
						value=0, min=0, max=1, step = 0.025),
			#Reservoir Storage
			sliderInput(inputId = "Reservoir2", label = resStorageLabel,
						value=0, min=0, max=1, step = 0.025),
			#Annuitized Project Costs
			sliderInput(inputId = "ProjectCost2", label = annuitizedProjCostsLabel,
						value=0, min=0, max=1, step = 0.025),
			#Breach Damage Potential
			sliderInput(inputId = "Safety2", label = breachDamageLabel,
						value=0, min=0, max=1, step = 0.025),
			#Number of Properties
			sliderInput(inputId = "NumProperties2", label = numPropertiesLabel,
						value=0, min=0, max=1, step = 0.025),
			#Annual Electricity Generation
			sliderInput(inputId = "ElectricityGeneration2", label = annualElectricityLabel,
						value=0, min=0, max=1, step = 0.025),
			#GHGEmissions
			sliderInput(inputId = "AvoidEmissions2", label = GHGEmissionsLabel,
						value=0, min=0, max=1, step = 0.025),
			#IndigenousHeritage
			sliderInput(inputId = "IndigenousHeritage2", label = indigenousHeritageLabel,
						value=0, min=0, max=1, step = 0.025),
			#IndustrialHistory
			sliderInput(inputId = "IndustrialHistory2", label = industrialHistoryLabel,
						value=0, min=0, max=1, step = 0.025),
			#CommunityIdentity
			sliderInput(inputId = "CommunityIdentity2", label = communityIdentityLabel,
						value=0, min=0, max=1, step = 0.025),
			#Aesthetics
			sliderInput(inputId = "Aesthetics2", label = aestheticsLabel,
						value=0, min=0, max=1, step = 0.025),


			actionButton("updateBtn2", "Update"),

			div(id="alt-2-output",
				h2("Raw Scores of Alternative 2"),
				plotOutput("SummPlot2", height=graph_height, width=graph_width)
			 )
		), # end
		#End Alternative 2: Improve Fish Passage Tab


		tabPanel(
			#"Alternative 3: Improve Hydropower Generation",
			htmlOutput("Alt3"), # status and title
			h2("Alternative 3: Improve Hydropower Generation"),
			HTML(
				"When new turbines are installed on existing non-powered dams, or hydropower capacity is increased at a powered dam, annual hydropower generation increases. Similarly, upgrading or replacing turbines may increase annual generation and improve longevity for a hydropower dam.\ 
				Increases in hydropower generation may reduce greenhouse gas emissions that contribute to climate change. Costs may be recouped through market returns over the project's lifetime, and the change in the dam's operation may even present opportunities for whitewater recreation \
				downstream (dam releases are popular for river rafting). However, installing turbines or expanding existing power capacity may alter flows and confuse sea-run fish species. Fish may become caught in the grates protecting system intakes, or even killed by turbine blades or rapid \
				changes in pressure if they are small enough to move through the powerhouse. Actual reservoir storage may change based on overall hydropower operations. <br>\

				<br><b> Warning: decision criteria ratings must sum to 1!</b> The tracking indicator will help you keep track of the sum. Be aware that decision criteria are directly compensating (i.e., if the sum of all ratings is 1, then increasing the rating on one criterion requires another criterion \
				rating to decrease in order to keep the sum equal to 1). <br>\
				<br> Please consider and rate the given set of criteria based on the case of improved hydropower generation at the dam. In each case, 0 = not at all important and 1 = extremely important.<br>"
			),

			htmlOutput("Alt3Progress"),

			#Fish Survival
			sliderInput(inputId = "FishBiomass3", label = fishSurvivalLabel,
						value=0, min=0, max=1, step = 0.025),
			#River Recreation
			sliderInput(inputId = "RiverRec3", label = riverRecLabel,
						value=0, min=0, max=1, step = 0.025),
			#Reservoir Storage
			sliderInput(inputId = "Reservoir3", label = resStorageLabel,
						value=0, min=0, max=1, step = 0.025),
			#Annuitized Project Costs
			sliderInput(inputId = "ProjectCost3", label = annuitizedProjCostsLabel,
						value=0, min=0, max=1, step = 0.025),
			#Breach Damage Potential
			sliderInput(inputId = "Safety3", label = breachDamageLabel,
						value=0, min=0, max=1, step = 0.025),
			#Number of Properties
			sliderInput(inputId = "NumProperties3", label = numPropertiesLabel,
						value=0, min=0, max=1, step = 0.025),
			#Annual Electricity Generation
			sliderInput(inputId = "ElectricityGeneration3", label = annualElectricityLabel,
						value=0, min=0, max=1, step = 0.025),
			#GHGEmissions
			sliderInput(inputId = "AvoidEmissions3", label = GHGEmissionsLabel,
						value=0, min=0, max=1, step = 0.025),
			#IndigenousHeritage
			sliderInput(inputId = "IndigenousHeritage3", label = indigenousHeritageLabel,
						value=0, min=0, max=1, step = 0.025),
			#IndustrialHistory
			sliderInput(inputId = "IndustrialHistory3", label = industrialHistoryLabel,
						value=0, min=0, max=1, step = 0.025),
			#CommunityIdentity
			sliderInput(inputId = "CommunityIdentity3", label = communityIdentityLabel,
						value=0, min=0, max=1, step = 0.025),
			#Aesthetics
			sliderInput(inputId = "Aesthetics3", label = aestheticsLabel,
						value=0, min=0, max=1, step = 0.025),

			actionButton("updateBtn3", "Update"),

			div(id="alt-3-output",
				h2("Raw Scores of Alternative 3"),
				plotOutput("SummPlot3", height=graph_height, width=graph_width)
			)
		), # end
		#End Alternative 3: Improve hyro generation 


		tabPanel(
			#"Alternative 4: Improve hydropower generation AND fish passage",
			htmlOutput("Alt4"), # status and title
			h2("Alternative 4: Improve Hydropower Generation AND Fish Passage Facilities"),
			HTML(
				"When hydropower generation improvements AND fish passage improvements are made to a dam (powered or non-powered), they may increase survival for sea-run fish species within the watershed. However, installing turbines or expanding existing power capacity may also alter flows\
				and confuse sea-run fish species, who may be attracted to the water moving through the system intake. Fish may become caught in the grates protecting the system intake, or even killed by turbine blades or rapid changes in pressure if they are small enough to move through \
				the powerhouse. Annual electricity generation will increase overall, and revenue may help recoup costs over the project's lifetime. Increases in hydropower generation may reduce greenhouse gas emissions that contribute to climate change. Turbine operation may be less efficient \
				with fish passage (depending on the technology selected), and fish passage costs are typically high. Fish passage may be required by law depending on the species migrating in the waterway, and additional improvements may become required as other species become threatened or \
				endangered.<br>\
				<br><b>Warning: decision criteria ratings must sum to 1!</b> The tracking indicator will help you keep track of the sum. Be aware that decision criteria are directly compensating (i.e., if the sum of all ratings is 1, then increasing the rating on one criterion requires another \
				criterion rating to decrease in order to keep the sum equal to 1). <br>\
				<br>Please consider and rate the given set of criteria based on the case of improved hydropower generation AND fish passage facilities at the dam. In each case, 0 = not at all important and 1 = extremely important.<br>"
			),

			htmlOutput("Alt4Progress"),

			#Fish Survival
			sliderInput(inputId = "FishBiomass4", label = fishSurvivalLabel,
						value=0, min=0, max=1, step = 0.025),
			#River Recreation
			sliderInput(inputId = "RiverRec4", label = riverRecLabel,
						value=0, min=0, max=1, step = 0.025),
			#Reservoir Storage
			sliderInput(inputId = "Reservoir4", label = resStorageLabel,
						value=0, min=0, max=1, step = 0.025),
			#Annuitized Project Costs
			sliderInput(inputId = "ProjectCost4", label = annuitizedProjCostsLabel,
						value=0, min=0, max=1, step = 0.025),
			#Breach Damage Potential
			sliderInput(inputId = "Safety4", label = breachDamageLabel,
						value=0, min=0, max=1, step = 0.025),
			#Number of Properties
			sliderInput(inputId = "NumProperties4", label = numPropertiesLabel,
						value=0, min=0, max=1, step = 0.025),
			#Annual Electricity Generation
			sliderInput(inputId = "ElectricityGeneration4", label = annualElectricityLabel,
						value=0, min=0, max=1, step = 0.025),
			#GHGEmissions
			sliderInput(inputId = "AvoidEmissions4", label = GHGEmissionsLabel,
						value=0, min=0, max=1, step = 0.025),
			#IndigenousHeritage
			sliderInput(inputId = "IndigenousHeritage4", label = indigenousHeritageLabel,
						value=0, min=0, max=1, step = 0.025),
			#IndustrialHistory
			sliderInput(inputId = "IndustrialHistory4", label = industrialHistoryLabel,
						value=0, min=0, max=1, step = 0.025),
			#CommunityIdentity
			sliderInput(inputId = "CommunityIdentity4", label = communityIdentityLabel,
						value=0, min=0, max=1, step = 0.025),
			#Aesthetics
			sliderInput(inputId = "Aesthetics4", label = aestheticsLabel,
						value=0, min=0, max=1, step = 0.025),

			actionButton("updateBtn4", "Update"),

			div(id="alt-4-output",
				h2("Raw Scores of Alternative 4"),
				plotOutput("SummPlot4", height=graph_height, width=graph_width)
			)
		), # end
		#End Alternative 4: Improve hydropower and fish passage 

		tabPanel(
			#"Alternative 5: Keep and Maintain Dam",
			htmlOutput("Alt5"), # status and title
			h2("Alternative 5: Keep and Maintain Dam"),

			HTML(
				"Keeping and maintaining the dam is the closest to a \"do-nothing\" option included in this tool. It is the lowest-cost option. Keeping and maintaining the dam may appeal to parties interested in preserving the area's industrial history, preserving the town/city identity for \
				community residents (if local identity is closely tied to the dam), or preserving the aesthetic value of the impoundment. Maintenance costs may be recouped somewhat if the dam is powered; however, refurbishment, restoration, or maintenance to a non-powered dam presents no direct \
				opportunity for cost offset. Keeping the dam will likely have no impact on reservoir storage volume, river recreation area, annual electricity generation, or number of properties abutting the reservoir. The impoundment will continue to present a barrier to sea-run fish species, \
				thereby negatively impacting their survival. And, in the long run, the dam is a temporary piece of infrastructure that may need to be removed. <br>\
				<br><b>Warning: decision criteria ratings must sum to 1!</b> The tracking indicator will help you keep track of the sum. Be aware that decision criteria are directly compensating (i.e., if the sum of all ratings is 1, then another criterion rating must decrease in order to keep the sum equal to 1). <br>\
				<br> Please consider and rate the given set of criteria based on the case of keeping and maintaining the dam. In each case, 0 = not at all important and 1 = extremely important.<br>"
			),

			htmlOutput("Alt5Progress"),

			#Fish Survival
			sliderInput(inputId = "FishBiomass5", label = fishSurvivalLabel,
						value=0, min=0, max=1, step = 0.025),
			#River Recreation
			sliderInput(inputId = "RiverRec5", label = riverRecLabel,
						value=0, min=0, max=1, step = 0.025),
			#Reservoir Storage
			sliderInput(inputId = "Reservoir5", label = resStorageLabel,
						value=0, min=0, max=1, step = 0.025),
			#Annuitized Project Costs
			sliderInput(inputId = "ProjectCost5", label = annuitizedProjCostsLabel,
						value=0, min=0, max=1, step = 0.025),
			#Breach Damage Potential
			sliderInput(inputId = "Safety5", label = breachDamageLabel,
						value=0, min=0, max=1, step = 0.025),
			#Number of Properties
			sliderInput(inputId = "NumProperties5", label = numPropertiesLabel,
						value=0, min=0, max=1, step = 0.025),
			#Annual Electricity Generation
			sliderInput(inputId = "ElectricityGeneration5", label = annualElectricityLabel,
						value=0, min=0, max=1, step = 0.025),
			#GHGEmissions
			sliderInput(inputId = "AvoidEmissions5", label = GHGEmissionsLabel,
						value=0, min=0, max=1, step = 0.025),
			#IndigenousHeritage
			sliderInput(inputId = "IndigenousHeritage5", label = indigenousHeritageLabel,
						value=0, min=0, max=1, step = 0.025),
			#IndustrialHistory
			sliderInput(inputId = "IndustrialHistory5", label = industrialHistoryLabel,
						value=0, min=0, max=1, step = 0.025),
			#CommunityIdentity
			sliderInput(inputId = "CommunityIdentity5", label = communityIdentityLabel,
						value=0, min=0, max=1, step = 0.025),
			#Aesthetics
			sliderInput(inputId = "Aesthetics5", label = aestheticsLabel,
						value=0, min=0, max=1, step = 0.025),

			actionButton("updateBtn5", "Update"),

			div(id="alt-5-output",
				h2("Raw Scores of Alternative 5"),
				plotOutput("SummPlot5", height=graph_height, width=graph_width)
			)
		), # end
		#End Alternative 5: Keep and Maintain Dam Tab

		tabPanel("Output",
			h2("Results"),
			HTML("<br>After completing all Alternatives use this button to get results<br><br>"),
			# generate event
			actionButton("generateMatrix", "Generate"),

			# output post generate
			div(id="generated-output",
				h2('Results Interpreation'),
				#tableOutput("FilledCriteriaTable"), # for debugging criteria table

				HTML(
					"<br>recall that the decision criteria ratings under every decision alternative were required to sum to 1. Here, the colored decision criteria segments within each decision alternative bar show the contribution of each decision criterion toward each decision\
					alternative score. The largest segments shows which preferences most drove your decision making under each decision alternative. You may notice that you consider different decision criteria from one decision alternative to the next. What does this tell you about how you make decisions? It is \
					up to you as a decision maker to decide what to do with this information. <br>"
				),

				h3('Alternative Scores by Criteria'),

				plotOutput("WSMPlot1", height=300, width=1000),

				HTML(
					 "<br><b>Results Interpreation</b>: the decision criterion with the largest bar shows where your greatest overall preference lies. Sometimes, preferences for decision criteria change from one decision alternative to another. You may see variation between\
					the ratings you chose between decision alternatives. What does this tell you about how you make decisions? It is up to you as a decision maker to decide what to do with this information.<br>\
					<br>"
				),

				h3('Criteria Scores by Alternative'),
				plotOutput("WSMPlot2", height=300, width=1000),

				HTML(
					 "<br>Some questions for consideration:<br>\

					<br> Do these results match your expectations? If not, why? If you feel discomfort at the result, you can return to the decision alternative tabs and re-evaluate your criteria ratings. Remember to press \"Update\" under each Alternative tab. Then, return to the Output page and click GENERATE\
					once more to see how your results change (note: you may want to download your results from this session, first).<br>\

					<br> Do these results make sense, given the tradeoffs you made in balancing the set of decision criteria under each decision alternative? Recall that the decision criteria are fully compensating, meaning that as the preference value for one increases, the value for another \
					must necessarily decrease. The idea here is to emphasize tradeoffs betwen decision criteria.<br>" 
				),
				h3('Download Results'),
				downloadButton("downloadData", "Download")
			)
		),

		tabPanel("Developers",
			 h2("Developers"),
			 HTML(
				 "Emma Fox- Lead Developer (Ph.D. candidate, University of Maine Ecology and Environmental Science Program). Designed initial user interface and server functionality based on Raymond & Klein (2018). Adjusted WSM function for new dam decision application and advised model-related changes. \
				 Designed and wrote app text, and designed accompanying multi-dam decision example fact sheets.<br>\
				 <br>Sharon Klein- Development Advisor (Associate Professor, University of Maine School of Economics). Advised user-friendliness enhancements to WSM model and UI/UX, refined criteria definitions, revised app text.<br>\
				 <br>William Winslow - Developer (Software Engineer, GeoSpatial Science Center(GSSC), University of New Hampshire). Deployment (Docker, Apache), server code reorganization, debugging/bug fixes, misc. feature implementations (UI/UX).<br>\
				 <br>Garrett Raymond- Technical Consultant. Built WSM function in R and provided basic web app design. See also: Raymond, G. and Klein, S. (2018). Web App: Multi-Criteria Decision Analysis of Fuel Pathways.https://fuel-production-pathway-comparison-tool.shinyapps.io/gr_ui_sep_models/ <br>\
				 "
			 )
		),

		tabPanel("Acknowledgements",
			 h2("Acknowledgments"),
			 HTML(
				 "We would like to thank Garrett Raymond for his technical consultation on R Shiny design.<br> \
				 Support for the Future of Dams project provided by the National Science Foundation\'s Research Infrastructure Improvement NSF #IIA-1539071, \
				 USDA National Institute of Food and Agriculture, Hatch project 0230040, and Department of the Interior, \
				 U.S. Geological Survey Grant No. G16AP00057 through the Senator George J. Mitchell Center at the University of Maine.<br>\
				 <br> Data Discovery Center at the University of New Hampshire- host for the Dam Decision Support Tool. https://ddc.unh.edu <br> \
				 "
			 ),
			 h2("Citations"),
			 HTML(
				 "Roy, S.G., Uchida, E., de Souza, S.P., Blachly, B., Fox, E., Gardner, K., Gold, A.J., Jansujwicz, J., Klein, S., McGreavy, B., Mo, W., Smith, S.M.C., Vogler, E., Wilson, K., Zydlewski, J., & Hart, D. (2018). A multiscale approach to balance trade-offs among dam infrastructure, river restoration, and cost. Proceedings of the National \
				 Academy of Sciences, 201807437. doi:10.1073/pnas.1807437115.<br>\
				 "
			 )

		)
)))

# create the application with ui in this file and imported server from server.R
shinyApp(ui, server)
