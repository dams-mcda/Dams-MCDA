# This is the user-interface definition of a Shiny web application

library(shiny)
library(dplyr, warn.conflicts=FALSE)
library(shinyjs, warn.conflicts=FALSE)
library(Cairo)
library(ggplot2)
library(RColorBrewer)
library(spatstat)
library(rgdal)
library(abind)
library(leaflet)
library(DT)

options(shiny.usecairo=TRUE)

source("server.R")

# default graph dimensions
graph_width <- "100%"
graph_height <- 500

# default map dimensions
map_width <- "100%"
map_height <- 500

# multiple used labels
fishSurvivalLabel <- "Sea-run fish habitat area is measured in hundreds of square meters. It is a proxy criteria estimated as possible upstream \
sea-run fish (Atlantic salmon, Alewife, Blueback herring, American eel) functional habitat (Roy et al., 2018). "
riverRecLabel <- "River recreation is measured in square kilometers. It is the estimated downstream area of river that may increase \
or decrease with a dam decision alternative, combines functional area for whitewater and flatwater recreation defined by Roy et al. (2018)."
resStorageLabel <- "Reservoir storage is measured in cubic kilometers. It is the estimated storage potential of the reservoir, based\ 
on its volume (Roy et al., 2018)."
annuitizedProjCostsLabel <- "Annuitized project cost is measured in 2018 $USD/year. It is the estimated total project costs (capital \
and operation & maintenance) on an annual basis using a 6.2% discount rate and a 20-year lifetime."
numPropertiesLabel <- "Number of properties is the estimated number of properties impacted near the dam, based on potential changes \
in viewshed or property value (Roy et al., 2018). "
breachDamageLabel <- "Breach damange potential is a unitless proxy for safety based on the State hazard rating, which indicates the \
potential for downstream property damage, injury, and death in the case of dam breach (Roy et al., 2018)."
annualElectricityLabel <- "Annual electricity generation is measured in GWh/year. It is the average estimate based on nameplate \
capacity from FERC licenses for each hydropower project."
GHGEmissionsLabel <- "Annual carbon dioxide (CO2) emissions reduction is measured in metric kilotonnes of CO2/year. It is an estimate \
estimate of avoided carbon dioxide emissions from annual hydropower-generated electricity production (reservoir or diversion-design \
dams); based on decreasing generation from the State's electricity generation mix; includes life cycle emissions impacts."
indigenousLifewaysLabel <- "Indigenous cultural traditions and lifeways is a unitless rating to convey the importance of preserving\
or restoring the cultural traditions and lifeways of indigenous people."
communityIdentityLabel <- "Community identity is a unitless rating to convey the importance of preserving the existing identity of \
the community residents living along or on islands within the river."
industrialHistoryLabel <- "Industrial historical importance is a unitless rating to convey the importance of preserving or restoring\
he industrial history of the site. "
aestheticsLabel <- "Aesthetic value is a rating to convey the importance of improving or preserving the aesthetics (e.g, appearance,\
scenic value, smell, sound)."
healthLabel <- "Public health is a unitless rating to convey the importance of public health connected to air, water, and land\
pollution."
justiceLabel <- "Socio-environmental justice is a unitless rating to convey the importance of socio-environmental justice issues\
(e.g. negative environmental effects that target disadvantaged groups - people of lower socio-economic status or with less political \
or economic power)."



# Define UI for Shiny web application
ui <- shinyUI(fluidPage(
	# used for toggling display of elements
	shinyjs::useShinyjs(),

	# next 5 lines for testing data passing through javascript
	includeScript("www/dams_mcda.js"),

	# link css
	tags$head(
		tags$link(rel = "stylesheet", type = "text/css", href = "dams_mcda.css")
	),

	# page banner/title + logout link
	HTML('<div id="app-logout"><a href="/logout/">Logout</a></div>\
		 <div id="page-title">Dam Decision Support Tool</div> \
		 \
	'),
	div(
		id="progress-tracker",
		htmlOutput("TotalProgress")
	),

	# intro popup-modal, forces user to decide input before proceeding


	navlistPanel(
		# Define layout widths
		widths = c(2,10),

		#Define Instructions tab
		HTML("<li class='step-label'>Step 1: Start Here</li>"),
		tabPanel("Start Here",
			h2("Welcome!"),

			img(src = 'River1.jpg', width="50%", align = "right", style="margin:0.2em;"),

			HTML(
				"<br>This <b>free and open source</b> tool was developed to aid people like you in considering tradeoffs associated with dams. It can help support your consideration of possible decision alternatives (e.g. keep and maintain dam, \
				 improve hydropower generation, improve fish passage) for hydropower dams, and could potentially be tailored toward other types of dam decisions. The tool generates a data-driven recommendation for Federal Energy Regulatory Commission \
         (FERC) licensed hydropower dams in Maine's Penobscot River, based on your preferences. This tool is based on the Weighted Sum approach to Multi-Criteria Decision Analysis (MCDA) to compare decision-maker preferences for decision criteria \
         (e.g., annuitized cost, CO2 equivalent emissions reductions, sea-run fish habitat area, etc.) for hydropower dams with FERC license expiration dates in the next 10 years. The tool gathers user inputs and calculates a ranked set of decision \
         alternatives for each dam. Then, tool optimizes across the set of dams coming up for relicensing to suggest a coordinated set of decision alternatives for multiple dams. The purpose of the coordinated multi-dam recommendation\
				 is to encourage you to consider dams on the river as a system, in addition to one at a time.\ <br>
				 <br>The results from this decision support tool are not official to any FERC licensing process and do not in any way represent the ruling of FERC.<br>"
			),

			helpText(a("Background on Dam Decision Support Tool", href = 'BackgroundDamDecisionSupportTool.pdf')),
			helpText( a("Click HERE for more information about the FERC process", href = "https://www.ferc.gov/industries/hydropower/gen-info/licensing/ilp.asp")),

			HTML(
				"<h4>More Information:</h4>\

				The Penobscot River is home to many dams and supports valuable ecosystem services. The watershed inclues pristine natural lakes, clean water sources, and significant biodiversity, including several sea-run fish species (e.g. Atlantic salmon, American eel, \
				Blueback herring, and Alewife). Dams also provide important services: reservoirs for drinking water and recreation, flood protection, and generation of reliable, on-demand renewable hydropower, critical to reducing fossil-fuel emissions that contribute \
				to climate change and poor human health. However, all dams need regular maintenance and older dams may need more extensive repairs as they age. Dams may interrupt flows and prevent sea-run fish passage, contributing to large population declines. \
				They may also contribute to poor water quality downstream, increased predation, and climate change (decaying plant matter in reservoirs release methane into the atmosphere). Dams have long threatened indigenous cultural traditions, while at the same \
				time helping to shape post-industrial town or city identities over the last two centuries.<br>"
			 ),

			helpText( 
			  HTML('<a href = "http://www.pnas.org/content/early/2018/10/31/1807437115" target="_blank">Click HERE for more information about the tradeoffs involved in dam decision making</a>')
			  )
		  ),


		HTML("<li class='step-label'> Step 2: View Dam Map </li>"),
		tabPanel("View Dam Map",
			htmlOutput("Map1"), # status and title
			h2("View Existing FERC Dams Map"),
			HTML("Please consider the following dams on the Penobscot River. These non-federally owned dams are coming up for FERC relicensing within the next 10 years. These are the dams you will focus on \
				for the rest of the activity. Note: although the Penobscot Mills Project dams are licensed together under a single FERC license, we separate them here for consistency. \
				Hover over the dams on the map for more information on each site.<br>"
			),
			helpText(
				HTML('<a href="DecisionAlternativesDescriptions.pdf" target="_blank">Click for more information about dam decision alternatives</a><br>'),
				HTML('<a href="DecisionCritriaDescriptions.pdf" target="_blank">Click for more information about decision criteria</a><br>'),
				HTML('<a href="DecisionMatrices.xlsx" target="_blank">Click to download Decision Criteria Data Matrices</a>')
			),
			HTML("You may wish to refer to the resource links above and the watershed map below throughout the activity. <br>"
			),
			leafletOutput("dam_map", width=map_width, height=map_height),
			HTML(
        "Below is an example of what the multi-dam map output will look like. For example, if no change is recommended based on site-specific data and user preference inputs, all dam sites will be marked KEEP AND MAINTAIN . <br>"
			),
			img(src = 'Penobscot_MO_14_443',width = "75%", align = "center")
		),


		# ------------------------------------------------------------
		# Preference Elicitation Tool
		# ------------------------------------------------------------

		HTML("<li class='step-label'> Step 3: Enter Preferences </li>"),

		# ----------------------------------------
		# West Enfield Dam
		# ----------------------------------------
		tabPanel(
			 # tab button text
			 htmlOutput("Dam1"),

			 # tab content
			h2("West Enfield Dam (FERC No. P-2600)"),
			HTML('Please consider and rate the decision criteria listed below for West Enfield Dam. <a href="Factsheet_WestEnfield.pdf" download>Download Dam Factsheet</a> or <a href="Factsheet_WestEnfield.pdf" target="_blank">Open in new tab</a> <br>\
				 <br><b>Warning: decision criteria ratings must sum to 100!</b> The tracking indicator (in the box to the right of the first decision criterion) will help you keep track of the sum. Be aware that decision criteria are directly compensating (i.e., if the sum of all ratings is 100, then\ 
				 increasing the rating on one criterion requires another criterion rating to decrease to keep the sum equal to 100. Click UPDATE at the bottom of the page when you are done moving the slider bars to mark this tab "Complete". <br>\
				 <br><b>For ratings, 0 = not at all important and 100 = extremely important.</b><br>'
			),
			helpText(
				HTML('<a href = "WestEnfield_RawDecisionMatrix.pdf" target="_blank">Click to view West Enfield Data</a>')
			),
			htmlOutput("Dam1Progress"),

			#----------------------------------------
			# Criteria Inputs for West Enfield Dam
			#----------------------------------------
			#Fish Survival
			div(id="fish-survival-1",
				h3("Fish Survival"),
				sliderInput(inputId = "FishBiomass1", label = fishSurvivalLabel, value=0, min=0, max=100, step = 5)
			),
			#River Recreation
			div(id="river-rec-1",
				h3("River Recreation"),
				sliderInput(inputId = "RiverRec1", label = riverRecLabel, value=0, min=0, max=100, step = 5)
			),
			#Reservoir Storage
			div(id="res-storage-1",
				h3("Reservoir Storage"),
				sliderInput(inputId = "Reservoir1", label = resStorageLabel, value=0, min=0, max=100, step = 5)
			),
			#Annuitized Project Costs
			div(id="ann-proj-costs-1",
				h3("Annuitized Project Costs"),
				sliderInput(inputId = "ProjectCost1", label = annuitizedProjCostsLabel, value=0, min=0, max=100, step = 5)
			),
			#Breach Damage Potential
			div(id="breach-damage-1",
				h3("Breach Damage Potential"),
				sliderInput(inputId = "Safety1", label = breachDamageLabel, value=0, min=0, max=100, step = 5)
			),
			#Number of Properties
			div(id="num-prop-1",
				h3("Number of Properties"),
				sliderInput(inputId = "NumProperties1", label = numPropertiesLabel, value=0, min=0, max=100, step = 5)
			),
			#Annual Electricity Generation
			div(id="ann-elec-gen-1",
				h3("Annual Electricity Generation"),
				sliderInput(inputId = "ElectricityGeneration1", label = annualElectricityLabel, value=0, min=0, max=100, step = 5)
			),
			#GHGEmissions
			div(id="ghg-emissions-1",
				h3("CO2 Emissions Reductions"),
				sliderInput(inputId = "AvoidEmissions1", label = GHGEmissionsLabel, value=0, min=0, max=100, step = 5)
			),
			#IndigenousLifeways
			div(id="indig-lifeways-1",
				h3("Indigenous Cultural Traditions and Lifeways"),
				sliderInput(inputId = "IndigenousLifeways1", label = indigenousLifewaysLabel, value=0, min=0, max=100, step = 5)
			),
			#IndustrialHistory
			div(id="industrial-1",
				h3("Industrial Historical Importance"),
				sliderInput(inputId = "IndustrialHistory1", label = industrialHistoryLabel, value=0, min=0, max=100, step = 5)
			),
			#CommunityIdentity
			div(id="community-1",
				h3("Community Identity"),
				sliderInput(inputId = "CommunityIdentity1", label = communityIdentityLabel, value=0, min=0, max=100, step = 5)
			),
			#Aesthetics
			div(id="aesthetics-1",
				h3("Aesthetic Value"),
				sliderInput(inputId = "Aesthetics1", label = aestheticsLabel, value=0, min=0, max=100, step = 5)
			),
			#PublicHealth
			div(id="health-1",
			    h3("Public Health"),
			    sliderInput(inputId = "Health1", label = healthLabel, value=0, min=0, max=100, step = 5)
			),
			#Justice
			div(id="justice-1",
			    h3("Socio-Environmental Justice"),
			    sliderInput(inputId = "Justice1", label = justiceLabel, value=0, min=0, max=100, step = 5)
			),

			# update alt 1 score
			htmlOutput("UpdateDam1Progress"),
			actionButton("updateBtn1", "Update")

		), # End West Enfield Dam Tab

		# ----------------------------------------
		# Medway Dam
		# ----------------------------------------
		tabPanel(
			# tab button text
			htmlOutput("Dam2"),

			# tab content
			h2("Medway Dam (FERC No. P-2666)"),

			HTML(
				'Please consider and rate the decision criteria listed below for Medway Dam. <a href="Factsheet_Medway.pdf" download>Download Dam Factsheet</a> or <a href="Factsheet_Medway.pdf" target="_blank">Open in new tab</a> <br>\
				<br><b>Warning: decision criteria ratings must sum to 100!</b> The tracking indicator (in the box to the right of the first decision criterion) will help you keep track of the sum. Be aware that decision criteria are directly compensating (i.e., if the sum of all ratings is 100, then\ 
				increasing the rating on one criterion requires another criterion rating to decrease to keep the sum equal to 100.Click UPDATE at the bottom of the page when you are done moving the slider bars to mark this tab "Complete". <br>\
				<br><b> For ratings, 0 = not at all important and 100 = extremely important.</b><br>'
			),
			helpText(
			  HTML('<a href = "Medway_RawDecisionMatrix.pdf" target="_blank">Click to view Medway Data</a>')
			),
			htmlOutput("Dam2Progress"),

			#----------------------------------------
			# Criteria Inputs for Alt 2
			#----------------------------------------
			#Fish Survival
			div(id="fish-survival-2",
				h3("Fish Survival"),
				sliderInput(inputId = "FishBiomass2", label = fishSurvivalLabel, value=0, min=0, max=100, step = 5)
			),
			#River Recreation
			div(id="river-rec-2",
				h3("River Recreation"),
				sliderInput(inputId = "RiverRec2", label = riverRecLabel, value=0, min=0, max=100, step = 5)
			),
			#Reservoir Storage
			div(id="res-storage-2",
				h3("Reservoir Storage"),
				sliderInput(inputId = "Reservoir2", label = resStorageLabel, value=0, min=0, max=100, step = 5)
			),
			#Annuitized Project Costs
			div(id="ann-proj-costs-2",
				h3("Annuitized Project Costs"),
				sliderInput(inputId = "ProjectCost2", label = annuitizedProjCostsLabel, value=0, min=0, max=100, step = 5)
			),
			#Breach Damage Potential
			div(id="breach-damage-2",
				h3("Breach Damage Potential"),
				sliderInput(inputId = "Safety2", label = breachDamageLabel, value=0, min=0, max=100, step = 5)
			),
			#Number of Properties
			div(id="num-prop-2",
				h3("Number of Properties"),
				sliderInput(inputId = "NumProperties2", label = numPropertiesLabel, value=0, min=0, max=100, step = 5)
			),
			#Annual Electricity Generation
			div(id="ann-elec-gen-2",
				h3("Annual Electricity Generation"),
				sliderInput(inputId = "ElectricityGeneration2", label = annualElectricityLabel, value=0, min=0, max=100, step = 5)
			),
			#GHGEmissions
			div(id="ghg-emissions-2",
				h3("CO2 Emissions Reductions"),
				sliderInput(inputId = "AvoidEmissions2", label = GHGEmissionsLabel, value=0, min=0, max=100, step = 5)
			),
			#IndigenousLifeways
			div(id="indig-lifeways-2",
				h3("Indigenous Cultural Traditions and Lifeways"),
				sliderInput(inputId = "IndigenousLifeways2", label = indigenousLifewaysLabel, value=0, min=0, max=100, step = 5)
			),
			#IndustrialHistory
			div(id="industrial-2",
				h3("Industrial Historical Importance"),
				sliderInput(inputId = "IndustrialHistory2", label = industrialHistoryLabel, value=0, min=0, max=100, step = 5)
			),
			#CommunityIdentity
			div(id="community-2",
				h3("Community Identity"),
				sliderInput(inputId = "CommunityIdentity2", label = communityIdentityLabel, value=0, min=0, max=100, step = 5)
			),
			#Aesthetics
			div(id="aesthetics-2",
				h3("Aesthetic Value"),
				sliderInput(inputId = "Aesthetics2", label = aestheticsLabel, value=0, min=0, max=100, step = 5)
			),
			#PublicHealth
			div(id="health-2",
			    h3("Public Health"),
			    sliderInput(inputId = "Health2", label = healthLabel, value=0, min=0, max=100, step = 5)
			),
			#Justice
			div(id="justice-2",
			    h3("Socio-Environmental Justice"),
			    sliderInput(inputId = "Justice2", label = justiceLabel, value=0, min=0, max=100, step = 5)
			),

			htmlOutput("UpdateDam2Progress"),
			actionButton("updateBtn2", "Update")

		), # End Medway Dam Tab


		# ----------------------------------------
		# Millinocket Dam
		# ----------------------------------------
		tabPanel(
			# tab button text
			htmlOutput("Dam3"),

			# tab content
			h2("Millinocket/Quakish Dam (FERC No. P-2458)"),
			HTML(
				'Please consider the decision criteria listed below for Millinocket Dam. <a href="Factsheet_PenobscotMills.pdf" download>Download Dam Factsheet</a> or <a href="Factsheet_PenobscotMills.pdf" target="_blank">Open in new tab</a><br>\
				<br><b>Warning: decision criteria ratings must sum to 100!</b> The tracking indicator (in the box to the right of the first decision criterion) will help you keep track of the sum. Be aware that decision criteria are directly compensating (i.e., if the sum of all ratings is 100, then\ 
				increasing the rating on one criterion requires another criterion rating to decrease to keep the sum equal to 100. Click UPDATE at the bottom of the page when you are done moving the slider bars to mark this tab "Complete". <br>\
				<br><b>For ratings, 0 = not at all important and 100 = extremely important.</b><br>'
			),
			helpText(
			  HTML('<a href = "PenobscotMills_RawDecisionMatrix.pdf" target="_blank">Click to view Penobscot Mills Data</a>')
			),
			htmlOutput("Dam3Progress"),

			#----------------------------------------
			# Criteria Inputs for Millinocket Dam
			#----------------------------------------
			#Fish Survival
			div(id="fish-survival-3",
				h3("Fish Survival"),
				sliderInput(inputId = "FishBiomass3", label = fishSurvivalLabel, value=0, min=0, max=100, step = 5)
			),
			#River Recreation
			div(id="river-rec-3",
				h3("River Recreation"),
				sliderInput(inputId = "RiverRec3", label = riverRecLabel, value=0, min=0, max=100, step = 5)
			),
			#Reservoir Storage
			div(id="res-storage-3",
				h3("Reservoir Storage"),
				sliderInput(inputId = "Reservoir3", label = resStorageLabel, value=0, min=0, max=100, step = 5)
			),
			#Annuitized Project Costs
			div(id="ann-proj-costs-3",
				h3("Annuitized Project Costs"),
				sliderInput(inputId = "ProjectCost3", label = annuitizedProjCostsLabel, value=0, min=0, max=100, step = 5)
			),
			#Breach Damage Potential
			div(id="breach-damage-3",
				h3("Breach Damage Potential"),
				sliderInput(inputId = "Safety3", label = breachDamageLabel, value=0, min=0, max=100, step = 5)
			),
			#Number of Properties
			div(id="num-prop-3",
				h3("Number of Properties"),
				sliderInput(inputId = "NumProperties3", label = numPropertiesLabel, value=0, min=0, max=100, step = 5)
			),
			#Annual Electricity Generation
			div(id="ann-elec-gen-3",
				h3("Annual Electricity Generation"),
				sliderInput(inputId = "ElectricityGeneration3", label = annualElectricityLabel, value=0, min=0, max=100, step = 5)
			),
			#GHGEmissions
			div(id="ghg-emissions-3",
				h3("CO2 Emissions Reductions"),
				sliderInput(inputId = "AvoidEmissions3", label = GHGEmissionsLabel, value=0, min=0, max=100, step = 5)
			),
			#IndigenousLifeways
			div(id="indig-lifeways-3",
				h3("Indigenous Cultural Traditions and Lifeways"),
				sliderInput(inputId = "IndigenousLifeways3", label = indigenousLifewaysLabel, value=0, min=0, max=100, step = 5)
			),
			#IndustrialHistory
			div(id="industrial-3",
				h3("Industrial Historical Importance"),
				sliderInput(inputId = "IndustrialHistory3", label = industrialHistoryLabel, value=0, min=0, max=100, step = 5)
			),
			#CommunityIdentity
			div(id="community-3",
				h3("Community Identity"),
				sliderInput(inputId = "CommunityIdentity3", label = communityIdentityLabel, value=0, min=0, max=100, step = 5)
			),
			#Aesthetics
			div(id="aesthetics-3",
				h3("Aesthetic Value"),
				sliderInput(inputId = "Aesthetics3", label = aestheticsLabel, value=0, min=0, max=100, step = 5)
			),
			#PublicHealth
			div(id="health-3",
			    h3("Public Health"),
			    sliderInput(inputId = "Health3", label = healthLabel, value=0, min=0, max=100, step = 5)
			),
			#Justice
			div(id="justice-3",
			    h3("Socio-Environmental Justice"),
			    sliderInput(inputId = "Justice3", label = justiceLabel, value=0, min=0, max=100, step = 5)
			),

			htmlOutput("UpdateDam3Progress"),
			actionButton("updateBtn3", "Update")

		), # End Millinocket Dam


		# ----------------------------------------
		# East Millinocket Dam
		# ----------------------------------------
		tabPanel(
			# tab button text
			htmlOutput("Dam4"),

			# tab content
			h2("East Millinocket Dam (FERC No. P-2458)"),
			HTML(
				'Please consider the decision criteria listed below for East Millinocket Dam. <a href="Factsheet_.pdf" download>Download Dam Factsheet</a> or <a href="Factsheet_.pdf" target="_blank">Open in new tab</a> <br>\
				<br><b>Warning: decision criteria ratings must sum to 100!</b> The tracking indicator (in the box to the right of the first decision criterion) will help you keep track of the sum. Be aware that decision criteria are directly compensating (i.e., if the sum of all ratings is 100, then\ 
				increasing the rating on one criterion requires another criterion rating to decrease to keep the sum equal to 100. Click UPDATE at the bottom of the page when you are done moving the slider bars to mark this tab "Complete". <br>\
				<br><b>For ratings, 0 = not at all important and 100 = extremely important.</b><br>'
			),

			helpText(
			  HTML('<a href = "PenobscotMills_RawDecisionMatrix.pdf" target="_blank">Click to view Penobscot Mills Data</a>')
			),
			htmlOutput("Dam4Progress"),

			#----------------------------------------
			# Criteria Inputs for East Millinocket Dam
			#----------------------------------------
			#Fish Survival
			div(id="fish-survival-4",
				h3("Fish Survival"),
				sliderInput(inputId = "FishBiomass4", label = fishSurvivalLabel, value=0, min=0, max=100, step = 5)
			),
			#River Recreation
			div(id="river-rec-4",
				h3("River Recreation"),
				sliderInput(inputId = "RiverRec4", label = riverRecLabel, value=0, min=0, max=100, step = 5)
			),
			#Reservoir Storage
			div(id="res-storage-4",
				h3("Reservoir Storage"),
				sliderInput(inputId = "Reservoir4", label = resStorageLabel, value=0, min=0, max=100, step = 5)
			),
			#Annuitized Project Costs
			div(id="ann-proj-costs-4",
				h3("Annuitized Project Costs"),
				sliderInput(inputId = "ProjectCost4", label = annuitizedProjCostsLabel, value=0, min=0, max=100, step = 5)
			),
			#Breach Damage Potential
			div(id="breach-damage-4",
				h3("Breach Damage Potential"),
				sliderInput(inputId = "Safety4", label = breachDamageLabel, value=0, min=0, max=100, step = 5)
			),
			#Number of Properties
			div(id="num-prop-4",
				h3("Number of Properties"),
				sliderInput(inputId = "NumProperties4", label = numPropertiesLabel, value=0, min=0, max=100, step = 5)
			),
			#Annual Electricity Generation
			div(id="ann-elec-gen-4",
				h3("Annual Electricity Generation"),
				sliderInput(inputId = "ElectricityGeneration4", label = annualElectricityLabel, value=0, min=0, max=100, step = 5)
			),
			#GHGEmissions
			div(id="ghg-emissions-4",
				h3("CO2 Emissions Reductions"),
				sliderInput(inputId = "AvoidEmissions4", label = GHGEmissionsLabel, value=0, min=0, max=100, step = 5)
			),
			#IndigenousLifeways
			div(id="indig-lifeways-4",
				h3("Indigenous Cultural Traditions and Lifeways"),
				sliderInput(inputId = "IndigenousLifeways4", label = indigenousLifewaysLabel, value=0, min=0, max=100, step = 5)
			),
			#IndustrialHistory
			div(id="industrial-4",
				h3("Industrial Historical Importance"),
				sliderInput(inputId = "IndustrialHistory4", label = industrialHistoryLabel, value=0, min=0, max=100, step = 5)
			),
			#CommunityIdentity
			div(id="community-4",
				h3("Community Identity"),
				sliderInput(inputId = "CommunityIdentity4", label = communityIdentityLabel, value=0, min=0, max=100, step = 5)
			),
			#Aesthetics
			div(id="aesthetics-4",
				h3("Aesthetic Value"),
				sliderInput(inputId = "Aesthetics4", label = aestheticsLabel, value=0, min=0, max=100, step = 5)
			),
			#PublicHealth
			div(id="health-4",
			    h3("Public Health"),
			    sliderInput(inputId = "Health4", label = healthLabel, value=0, min=0, max=100, step = 5)
			),
			#Justice
			div(id="justice-4",
			    h3("Socio-Environmental Justice"),
			    sliderInput(inputId = "Justice4", label = justiceLabel, value=0, min=0, max=100, step = 5)
			),

			htmlOutput("UpdateDam4Progress"),
			actionButton("updateBtn4", "Update")

		), # End East Millinocket Dam Tab


		# ----------------------------------------
		# North Twin Dam
		# ----------------------------------------
		tabPanel(
			# tab button text
			htmlOutput("Dam5"),

			# tab content
			h2("North Twin Dam (FERC No. P-2458)"),

			HTML(
				'Please consider the decision criteria listed below for North Twin Dam. <a href="Factsheet_.pdf" download>Download Dam Factsheet</a> or <a href="Factsheet_.pdf" target="_blank">Open in new tab</a> <br>\
				<br><b>Warning: decision criteria ratings must sum to 100!</b> The tracking indicator (in the box to the right of the first decision criterion) will help you keep track of the sum. Be aware that decision criteria are directly compensating (i.e., if the sum of all ratings is 100, then\ 
				increasing the rating on one criterion requires another criterion rating to decrease to keep the sum equal to 100. Click UPDATE at the bottom of the page when you are done moving the slider bars to mark this tab "Complete". <br>\
				<br><b>For ratings, 0 = not at all important and 100 = extremely important.</b><br>'
			),

			helpText(
			  HTML('<a href = "PenobscotMills_RawDecisionMatrix.pdf" target="_blank">Click to view Penobscot Mills Data</a>')
			),
			htmlOutput("Dam5Progress"),

			#----------------------------------------
			# Criteria Inputs for North Twin Dam
			#----------------------------------------
			#Fish Survival
			div(id="fish-survival-5",
				h3("Fish Survival"),
				sliderInput(inputId = "FishBiomass5", label = fishSurvivalLabel, value=0, min=0, max=100, step = 5)
			),
			#River Recreation
			div(id="river-rec-5",
				h3("River Recreation"),
				sliderInput(inputId = "RiverRec5", label = riverRecLabel, value=0, min=0, max=100, step = 5)
			),
			#Reservoir Storage
			div(id="res-storage-5",
				h3("Reservoir Storage"),
				sliderInput(inputId = "Reservoir5", label = resStorageLabel, value=0, min=0, max=100, step = 5)
			),
			#Annuitized Project Costs
			div(id="ann-proj-costs-5",
				h3("Annuitized Project Costs"),
				sliderInput(inputId = "ProjectCost5", label = annuitizedProjCostsLabel, value=0, min=0, max=100, step = 5)
			),
			#Breach Damage Potential
			div(id="breach-damage-5",
				h3("Breach Damage Potential"),
				sliderInput(inputId = "Safety5", label = breachDamageLabel, value=0, min=0, max=100, step = 5)
			),
			#Number of Properties
			div(id="num-prop-5",
				h3("Number of Properties"),
				sliderInput(inputId = "NumProperties5", label = numPropertiesLabel, value=0, min=0, max=100, step = 5)
			),
			#Annual Electricity Generation
			div(id="ann-elec-gen-5",
				h3("Annual Electricity Generation"),
				sliderInput(inputId = "ElectricityGeneration5", label = annualElectricityLabel, value=0, min=0, max=100, step = 5)
			),
			#GHGEmissions
			div(id="ghg-emissions-5",
				h3("CO2 Emissions Reductions"),
				sliderInput(inputId = "AvoidEmissions5", label = GHGEmissionsLabel, value=0, min=0, max=100, step = 5)
			),
			#IndigenousLifeways
			div(id="indig-lifeways-5",
				h3("Indigenous Cultural Traditions and Lifeways"),
				sliderInput(inputId = "IndigenousLifeways5", label = indigenousLifewaysLabel, value=0, min=0, max=100, step = 5)
			),
			#IndustrialHistory
			div(id="industrial-5",
				h3("Industrial Historical Importance"),
				sliderInput(inputId = "IndustrialHistory5", label = industrialHistoryLabel, value=0, min=0, max=100, step = 5)
			),
			#CommunityIdentity
			div(id="community-5",
				h3("Community Identity"),
				sliderInput(inputId = "CommunityIdentity5", label = communityIdentityLabel, value=0, min=0, max=100, step = 5)
			),
			#Aesthetics
			div(id="aesthetics-5",
				h3("Aesthetic Value"),
				sliderInput(inputId = "Aesthetics5", label = aestheticsLabel, value=0, min=0, max=100, step = 5)
			),
			#PublicHealth
			div(id="health-5",
			    h3("Public Health"),
			    sliderInput(inputId = "Health5", label = healthLabel, value=0, min=0, max=100, step = 5)
			),
			#Justice
			div(id="justice-5",
			    h3("Socio-Environmental Justice"),
			    sliderInput(inputId = "Justice5", label = justiceLabel, value=0, min=0, max=100, step = 5)
			),

			htmlOutput("UpdateDam5Progress"),
			actionButton("updateBtn5", "Update")

		), # End North Twin Dam Tab


		#----------------------------------------
		# Dolby Dam
		#----------------------------------------
		tabPanel(
			# tab button text
			htmlOutput("Dam6"),

			# tab content
			h2("Dolby Dam (FERC No. P-2458)"),

			HTML(
				'Please consider the decision criteria listed below for Dolby Dam. <a href="Factsheet_.pdf" download>Download Dam Factsheet</a> or <a href="Factsheet_.pdf" target="_blank">Open in new tab</a> <br>\
				<br><b>Warning: decision criteria ratings must sum to 100!</b> The tracking indicator (in the box to the right of the first decision criterion) will help you keep track of the sum. Be aware that decision criteria are directly compensating (i.e., if the sum of all ratings is 100, then\ 
				increasing the rating on one criterion requires another criterion rating to decrease to keep the sum equal to 100. Click UPDATE at the bottom of the page when you are done moving the slider bars to mark this tab "Complete". <br>\
				<br><b>For ratings, 0 = not at all important and 100 = extremely important.</b><br>'
			),

			helpText(
			  HTML('<a href = "PenobscotMills_RawDecisionMatrix.pdf" target="_blank">Click to view Penobscot Mills Data</a>')
			),
			htmlOutput("Dam6Progress"),

		  #----------------------------------------
		  # Criteria Inputs for Dolby Dam
		  #----------------------------------------
		  #Fish Survival
		  div(id="fish-survival-6",
		      h3("Fish Survival"),
		      sliderInput(inputId = "FishBiomass6", label = fishSurvivalLabel, value=0, min=0, max=100, step = 5)
		  ),
		  #River Recreation
		  div(id="river-rec-6",
		      h3("River Recreation"),
		      sliderInput(inputId = "RiverRec6", label = riverRecLabel, value=0, min=0, max=100, step = 5)
		  ),
		  #Reservoir Storage
		  div(id="res-storage-6",
		      h3("Reservoir Storage"),
		      sliderInput(inputId = "Reservoir6", label = resStorageLabel, value=0, min=0, max=100, step = 5)
		  ),
		  #Annuitized Project Costs
		  div(id="ann-proj-costs-6",
		      h3("Annuitized Project Costs"),
		      sliderInput(inputId = "ProjectCost6", label = annuitizedProjCostsLabel, value=0, min=0, max=100, step = 5)
		  ),
		  #Breach Damage Potential
		  div(id="breach-damage-6",
		      h3("Breach Damage Potential"),
		      sliderInput(inputId = "Safety6", label = breachDamageLabel, value=0, min=0, max=100, step = 5)
		  ),
		  #Number of Properties
		  div(id="num-prop-6",
		      h3("Number of Properties"),
		      sliderInput(inputId = "NumProperties6", label = numPropertiesLabel, value=0, min=0, max=100, step =5)
		  ),
		  #Annual Electricity Generation
		  div(id="ann-elec-gen-6",
		      h3("Annual Electricity Generation"),
		      sliderInput(inputId = "ElectricityGeneration6", label = annualElectricityLabel, value=0, min=0, max=100, step = 5)
		  ),
		  #GHGEmissions
		  div(id="ghg-emissions-6",
		      h3("CO2 Emissions Reductions"),
		      sliderInput(inputId = "AvoidEmissions6", label = GHGEmissionsLabel, value=0, min=0, max=100, step = 5)
		  ),
		  #IndigenousLifeways
		  div(id="indig-lifeways-6",
		      h3("Indigenous Cultural Traditions and Lifeways"),
		      sliderInput(inputId = "IndigenousLifeways6", label = indigenousLifewaysLabel, value=0, min=0, max=100, step = 5)
		  ),
		  #IndustrialHistory
		  div(id="industrial-6",
		      h3("Industrial Historical Importance"),
		      sliderInput(inputId = "IndustrialHistory6", label = industrialHistoryLabel, value=0, min=0, max=100, step = 5)
		  ),
		  #CommunityIdentity
		  div(id="community-6",
		      h3("Community Identity"),
		      sliderInput(inputId = "CommunityIdentity6", label = communityIdentityLabel, value=0, min=0, max=100, step = 5)
		  ),
		  #Aesthetics
		  div(id="aesthetics-6",
		      h3("Aesthetic Value"),
		      sliderInput(inputId = "Aesthetics6", label = aestheticsLabel, value=0, min=0, max=100, step = 5)
		  ),
		  #PublicHealth
		  div(id="health-6",
		      h3("Public Health"),
		      sliderInput(inputId = "Health6", label = healthLabel, value=0, min=0, max=100, step = 5)
		  ),
		  #Justice
		  div(id="justice-6",
		      h3("Socio-Environmental Justice"),
		      sliderInput(inputId = "Justice6", label = justiceLabel, value=0, min=0, max=100, step = 5)
		  ),

		  htmlOutput("UpdateDam6Progress"),
		  actionButton("updateBtn6", "Update")

		), # End Dolby Dam Tab


		# ----------------------------------------
		# Millinocket Lake Dam
		# ----------------------------------------
		tabPanel(
			# tab button text
			htmlOutput("Dam7"),

			# tab content
			h2("Millinocket Lake Dam (FERC No. P-2458)"),

			HTML(
				'Please consider the decision criteria listed below for Millinocket Lake Dam. <a href="Factsheet_.pdf" download>Download Dam Factsheet</a> or <a href="Factsheet_.pdf" target="_blank">Open in new tab</a> <br>\
				<br><b>Warning: decision criteria ratings must sum to 100!</b> The tracking indicator (in the box to the right of the first decision criterion) will help you keep track of the sum. Be aware that decision criteria are directly compensating (i.e., if the sum of all ratings is 100, then\ 
				increasing the rating on one criterion requires another criterion rating to decrease to keep the sum equal to 100. Click UPDATE at the bottom of the page when you are done moving the slider bars to mark this tab "Complete". <br>\
				<br><b>For ratings, 0 = not at all important and 100 = extremely important.</b><br>'
			),
			helpText(
			  HTML('<a href = "PenobscotMills_RawDecisionMatrix.pdf" target="_blank">Click to view Penobscot Mills Data</a>')
			),
			htmlOutput("Dam7Progress"),

		  #----------------------------------------
		  # Criteria Inputs for Millinocket Lake Dam
		  #----------------------------------------
		  #Fish Survival
		  div(id="fish-survival-7",
		      h3("Fish Survival"),
		      sliderInput(inputId = "FishBiomass7", label = fishSurvivalLabel, value=0, min=0, max=100, step = 5)
		  ),
		  #River Recreation
		  div(id="river-rec-7",
		      h3("River Recreation"),
		      sliderInput(inputId = "RiverRec7", label = riverRecLabel, value=0, min=0, max=100, step = 5)
		  ),
		  #Reservoir Storage
		  div(id="res-storage-7",
		      h3("Reservoir Storage"),
		      sliderInput(inputId = "Reservoir7", label = resStorageLabel, value=0, min=0, max=100, step = 5)
		  ),
		  #Annuitized Project Costs
		  div(id="ann-proj-costs-7",
		      h3("Annuitized Project Costs"),
		      sliderInput(inputId = "ProjectCost7", label = annuitizedProjCostsLabel, value=0, min=0, max=100, step = 5)
		  ),
		  #Breach Damage Potential
		  div(id="breach-damage-7",
		      h3("Breach Damage Potential"),
		      sliderInput(inputId = "Safety7", label = breachDamageLabel, value=0, min=0, max=100, step = 5)
		  ),
		  #Number of Properties
		  div(id="num-prop-7",
		      h3("Number of Properties"),
		      sliderInput(inputId = "NumProperties7", label = numPropertiesLabel, value=0, min=0, max=100, step = 5)
		  ),
		  #Annual Electricity Generation
		  div(id="ann-elec-gen-7",
		      h3("Annual Electricity Generation"),
		      sliderInput(inputId = "ElectricityGeneration7", label = annualElectricityLabel, value=0, min=0, max=100, step = 5)
		  ),
		  #GHGEmissions
		  div(id="ghg-emissions-7",
		      h3("CO2 Emissions Reductions"),
		      sliderInput(inputId = "AvoidEmissions7", label = GHGEmissionsLabel, value=0, min=0, max=100, step = 5)
		  ),
		  #IndigenousLifeways
		  div(id="indig-lifeways-7",
		      h3("Indigenous Cultural Traditions and Lifeways"),
		      sliderInput(inputId = "IndigenousLifeways7", label = indigenousLifewaysLabel, value=0, min=0, max=100, step = 5)
		  ),
		  #IndustrialHistory
		  div(id="industrial-7",
		      h3("Industrial Historical Importance"),
		      sliderInput(inputId = "IndustrialHistory7", label = industrialHistoryLabel, value=0, min=0, max=100, step = 5)
		  ),
		  #CommunityIdentity
		  div(id="community-7",
		      h3("Community Identity"),
		      sliderInput(inputId = "CommunityIdentity7", label = communityIdentityLabel, value=0, min=0, max=100, step = 5)
		  ),
		  #Aesthetics
		  div(id="aesthetics-7",
		      h3("Aesthetic Value"),
		      sliderInput(inputId = "Aesthetics7", label = aestheticsLabel, value=0, min=0, max=100, step = 5)
		  ),
		  #PublicHealth
		  div(id="health-7",
		      h3("Public Health"),
		      sliderInput(inputId = "Health7", label = healthLabel, value=0, min=0, max=100, step = 5)
		  ),
		  #Justice
		  div(id="justice-7",
		      h3("Socio-Environmental Justice"),
		      sliderInput(inputId = "Justice7", label = justiceLabel, value=0, min=0, max=100, step = 5)
		  ),


			htmlOutput("UpdateDam7Progress"),
			actionButton("updateBtn7", "Update")

		), # End Millinocket Lake Dam Tab


		# ----------------------------------------
		# Ripogenus Dam
		# ----------------------------------------
		tabPanel(
			# tab button text
			htmlOutput("Dam8"),

			# tab content
			h2("Ripogenus Dam (FERC No. P-2572)"),
			HTML(
				'Please consider the decision criteria listed below for Ripogenus Dam. <a href="Factsheet_Ripogenus.pdf" download>Download Dam Factsheet</a> or <a href="Factsheet_Ripogenus.pdf" target="_blank">Open in new tab</a> <br>\
				<br><b>Warning: decision criteria ratings must sum to 100!</b> The tracking indicator (in the box to the right of the first decision criterion) will help you keep track of the sum. Be aware that decision criteria are directly compensating (i.e., if the sum of all ratings is 100, then\ 
				increasing the rating on one criterion requires another criterion rating to decrease to keep the sum equal to 100. Click UPDATE at the bottom of the page when you are done moving the slider bars to mark this tab "Complete". <br>\
				<br><b>For ratings, 0 = not at all important and 100 = extremely important.</b><br>'
			),
			helpText(
			  HTML('<a href = "Ripogenus_RawDecisionMatrix.pdf" target="_blank">Click to view Ripogenus Data</a>')
			),
			htmlOutput("Dam8Progress"),

			#----------------------------------------
			# Criteria Inputs for Ripogenus Dam
			#----------------------------------------
			#Fish Survival
			div(id="fish-survival-8",
				h3("Fish Survival"),
				sliderInput(inputId = "FishBiomass8", label = fishSurvivalLabel, value=0, min=0, max=100, step = 5)
			),
			#River Recreation
			div(id="river-rec-8",
				h3("River Recreation"),
				sliderInput(inputId = "RiverRec8", label = riverRecLabel, value=0, min=0, max=100, step = 5)
			),
			#Reservoir Storage
			div(id="res-storage-8",
				h3("Reservoir Storage"),
				sliderInput(inputId = "Reservoir8", label = resStorageLabel, value=0, min=0, max=100, step = 5)
			),
			#Annuitized Project Costs
			div(id="ann-proj-costs-8",
				h3("Annuitized Project Costs"),
				sliderInput(inputId = "ProjectCost8", label = annuitizedProjCostsLabel, value=0, min=0, max=100, step = 5)
			),
			#Breach Damage Potential
			div(id="breach-damage-8",
				h3("Breach Damage Potential"),
				sliderInput(inputId = "Safety8", label = breachDamageLabel, value=0, min=0, max=100, step = 5)
			),
			#Number of Properties
			div(id="num-prop-8",
				h3("Number of Properties"),
				sliderInput(inputId = "NumProperties8", label = numPropertiesLabel, value=0, min=0, max=100, step = 5)
			),
			#Annual Electricity Generation
			div(id="ann-elec-gen-8",
				h3("Annual Electricity Generation"),
				sliderInput(inputId = "ElectricityGeneration8", label = annualElectricityLabel, value=0, min=0, max=100, step = 5)
			),
			#GHGEmissions
			div(id="ghg-emissions-8",
				h3("CO2 Emissions Reductions"),
				sliderInput(inputId = "AvoidEmissions8", label = GHGEmissionsLabel, value=0, min=0, max=100, step = 5)
			),
			#IndigenousLifeways
			div(id="indig-lifeways-8",
				h3("Indigenous Cultural Traditions and Lifeways"),
				sliderInput(inputId = "IndigenousLifeways8", label = indigenousLifewaysLabel, value=0, min=0, max=100, step = 5)
			),
			#IndustrialHistory
			div(id="industrial-8",
				h3("Industrial Historical Importance"),
				sliderInput(inputId = "IndustrialHistory8", label = industrialHistoryLabel, value=0, min=0, max=100, step = 5)
			),
			#CommunityIdentity
			div(id="community-8",
				h3("Community Identity"),
				sliderInput(inputId = "CommunityIdentity8", label = communityIdentityLabel, value=0, min=0, max=100, step = 5)
			),
			#Aesthetics
			div(id="aesthetics-8",
				h3("Aesthetic Value"),
				sliderInput(inputId = "Aesthetics8", label = aestheticsLabel, value=0, min=0, max=100, step = 5)
			),
			#PublicHealth
			div(id="health-8",
				h3("Public Health"),
				sliderInput(inputId = "Health8", label = healthLabel, value=0, min=0, max=100, step = 5)
			),
			#Justice
			div(id="justice-8",
				h3("Socio-Environmental Justice"),
				sliderInput(inputId = "Justice8", label = justiceLabel, value=0, min=0, max=100, step = 5)
			),

			htmlOutput("UpdateDam8Progress"),
			actionButton("updateBtn8", "Update")

		), # End Ripogenus Dam Tab

		# --------------------------------------------------------------------------------
		# RESULTS TABS
		# --------------------------------------------------------------------------------
		HTML("<li> Step 4: Multi-Dam Results </li>"),

		tabPanel("Combined Results",
		         h2("Multi-Dam Results"),
		         HTML("<br>Click GENERATE to get MCDA results graphs.<br><br>"),

		         #TODO: remove/hide for production
		         #actionButton("autoGenerateMatrix", "Autofill: debug only"),
		         #actionButton("testWSM", "test WSM: debug only"),
		         #actionButton("saveResultsToDjango", "Save Results To Django: debug only"),

		         # generate event
		         actionButton("generateOutput", "Generate"),

		         div(id="combined-output",
		             #h2('All Preferences'),
		             #tableOutput("FilledCriteriaTable"),

		             #alternatives at each dam
		             h2('Figure 1. Dam Decision Alternative Comparison'),
		             #plotOutput("AlternativesGraph_All", height="35em"),
		             HTML("<b>Results Interpretation</b> for Figure 1. This 'scenario', or group of decision alternatives for the set of dams, has been selected optimally based on your preference values and site-specific dam data values.\
		                  The scenario represents the most efficient combination of dam decision alternatives given your preferences and the site-specific data. This outcome is a recommendation designed to support the consideration of multiple\
		                  dams. This recommendation is intended to support brainstorming about possibilities for the river. This recommendation is not representative of any federal agency prescription or license ruling from FERC."),
		             
		             # by dam
		             h2('Figure 2. Decision Criteria by Dam'),
		             plotOutput("CombinedPlot1", height="35em"),
					 downloadButton("DownloadDam1CombinedPlot1", "Download Graph"),

		             HTML("<b>Results Interpretation</b> for Figure 2. The scenario is broken down by decision criteria at each dam to give you an idea of how the criteria scores (data values*preference values) contributes to the overall scenario selection."),
		             plotOutput("CombinedPlot2", height="35em"),
					 downloadButton("DownloadDam1CombinedPlot2", "Download Graph"),
					 
					 # download preferences (for UPLOAD DATA)
					 downloadButton("downloadPreferenceSelection", "Download Preferences (Step 3)")
		         )
		),


		tabPanel("Map Recommendation",
		         h2("Optimized Result"),
		         HTML('<div id="MapRecommendation"></div>')
		),


		HTML("<li class='step-label'> Step 5: View Dam Specific Results </li>"),

		tabPanel("Dam 1: West Enfield",
			h2("Results: West Enfield Dam"),

      #raw preference graph
			div(id="dam-1-output",

			    h3("Table 1. Raw preference scores for West Enfield Dam"),
			    DT::dataTableOutput("RawPrefsDam1"),
			    HTML( "This table of preference data is depicted below."),

			    h3("Figure 1. Raw Preference Scores for West Enfield"),
			    plotOutput("PrefPlot1", height=graph_height, width=graph_width),
			    HTML(
			      "<br><b>Results Interpretation</b> for Figure 1: The bars visually represent your preference scores for each decision criterion.\
					The scores are pulled directly from your slider bar settings under the West Enfield Dam tab and are not changed in any way. If you wish to go back and change your settings, please do so before continuing.<br>"
			    ),

				#raw data table/Matrix
    			h3("Table 2. Raw data values for West Enfield Dam"),
    			DT::dataTableOutput("Dam1RawTable"),
    			HTML(
    			  "<br><b>Results Interpretation</b> for Table 2. These are the raw data values for the dam development. We include the raw data values here to help make the MCDA calculation more transparent. Note: fish survival values shown here are discrete,\
				  but in reality, the values are network-dependent and would be impacted by upstream or downstream changes.<br>"
    			),

    			#normalized data table/Matrix
			    h3("Table 3. Normalized data values for West Enfield Dam"),
			    DT::dataTableOutput("Dam1NormTable"),
			    HTML(
    			  "<br><b>Results Interpretation</b> for Table 3. These are the normalized data values for the dam development. Raw data values have been normalized to a range between 0 and 1 to make them comparable across different units.\
				  Normalization was performed using a min/max procedure, where the highest values for most decision criteria are set equal to 1, and the lowest values are set equal to 0. For decision criteria where lower values are better \
				  (e.g. annuitized project cost, breach hazard potential, number of properties impacted), the highest values are actually set equal to 0, and the lowest values are set equal to 1. This allows us to indicate that, for instance,\
				  high costs are less desirable than low costs.<br>"
    			),

    			#weighted score data table/Matrix
			    h3("Table 4. Weighted scores for West Enfield Dam"),
			    DT::dataTableOutput("Dam1ScoreTable"),
				downloadButton("DownloadDam1ScoreTable", "Download Table"),
			    HTML(
    			  "<br><b>Results Interpretation</b> for Table 4. These are the raw data for the dam development. Normalized data values have been multiplied by your preference scores to achieve a weighted score.\
    			  The weighted sum for each decision alternative is considered the MCDA score, where the value closest to 100 is considered the first best alternative.<br>"
				)
			),
			# output post generate
			div(id="generated-output-1",

				HTML(
					"<br><b>Results Interpretation</b> for Figure 2: Recall that the decision criteria ratings under every dam tab were required to sum to 1. Here, the colored segments within each bar show the contribution of each decision criterion toward each decision\
					alternative score for this dam. The decision alternative scores are calculated by weighting (multiplying) normalized dam-specific data for each criterion by your preference information for this dam. The largest segments show which criterion most drive the total score for each decision alternative. \
					It is up to you to decide what to do with this information. <br>"
				),

				h3('Figure 2. West Enfield Decision Alternative Scores by Decision Criteria'),

				plotOutput("WSMPlot1a", height=600, width="100%"),
				downloadButton("DownloadDam1Plota", "Download Graph"),

				HTML(
					"<br><b>Results Interpretation</b> for Figure 3: The decision criterion with the largest bar shows where your overall priority lies, based on your preference information and the data for each decision criterion. Since preferences for decision criteria change from one dam to another, you may see variation between\
					the prioritized decision alternatives. It is up to you as a decision maker to decide what to do with this information.<br>\
					<br>"
				),

				h3('Figure 3. Total Decision Criteria Scores by Decision Alternative for West Enfield'),

				plotOutput("WSMPlot1b", height=1000, width="100%"),
				downloadButton("DownloadDam1Plotb", "Download Graph"),


				HTML(
					"<br><b>Questions for consideration:</b> Do these results match your expectations? If not, why? If you feel discomfort at the result, you can return to the decision alternative tabs and re-evaluate your criteria ratings. Remember to press \"Update\" under each Alternative tab. Then, return to the Output page and click GENERATE\
					once more to see how your results change (note: you may want to download your results from this session, first).<br>\
					<br> Do these results make sense, given the tradeoffs you made in balancing the set of decision criteria for each dam? Recall that the decision criteria are fully compensating, meaning that as the preference value for one increases, the value for another \
					must necessarily decrease. The idea here is to emphasize tradeoffs between decision criteria.<br> \
					<br><b>Next Steps</b>: You may download and save your results for personal reference, before continuing to the next step. If you are participating in the Dam Decision-Making Workshop, please save your results at this time."
				),

				plotOutput("WSMPlot1c", height=1000, width="100%"),
				downloadButton("DownloadDam1Plotc", "Download Graph"),

				h3('Download West Enfield Results'),
				downloadButton("downloadData1", "Download Table")
			)
		),

		tabPanel("Dam 2: Medway Dam",
		         h2("Results: Medway Dam"),
		         # raw preference table/Matrix
		         #raw preference graph
		         div(id="dam-2-output",
		             h3("Table 5. Raw preference scores for Medway Dam"),
		             DT::dataTableOutput("RawPrefsDam2"),
		             HTML(
		               "This table of preference data is depicted below."
		             ),
		             h3("Figure 4. Raw Preference Scores for Medway"),
		             plotOutput("PrefPlot2", height=graph_height, width=graph_width),
		             HTML(
		               "<br><b>Results Interpretation</b> for Figure 4: The bars visually represent your preference scores for each decision criterion.\
		               The scores are pulled directly from your slider bar settings under the Medway Dam tab and are not changed in any way. If you wish to go back and change your settings, please do so before continuing.<br>"
		             ),

		             #raw data table/Matrix
		             h3("Table 6. Raw data values for Medway Dam"),
		             DT::dataTableOutput("Dam2RawTable"),
		             HTML(
		               "<br><b>Results Interpretation</b> for Table 6. These are the raw data values for the dam development. We include the raw data values here to help make the MCDA calculation more transparent."
		             ),
		             # normalized data table/Matrix
		             h3("Table 7. Normalized data values for Medway Dam"),
		             DT::dataTableOutput("Dam2NormTable"),
		             HTML(
		               "<br><b>Results Interpretation</b> for Table 7. These are the normalized data values for the dam development. Raw data values have been normalized to a range between 0 and 1 to make them comparable across different units.\
		               Normalization was performed using a min/max procedure, where the highest values for most decision criteria are set equal to 1, and the lowest values are set equal to 0. For decision criteria where lower values are better \
		               (e.g. annuitized project cost, breach hazard potential, number of properties impacted), the highest values are actually set equal to 0, and the lowest values are set equal to 1. This allows us to indicate that, for instance,\
		               high costs are less desirable than low costs.<br>"
		             ),
		             #weighted score data table/Matrix
		             h3("Table 8. Weighted scores for Medway Dam"),
		             DT::dataTableOutput("Dam2ScoreTable"),
					downloadButton("DownloadDam2ScoreTable", "Download Table"),
		             HTML(
		               "<br><b>Results Interpretation</b> for Table 8. These are the raw data for the dam development. Normalized data values have been multiplied by your preference scores to achieve a weighted score.\
		               The weighted sum for each decision alternative is considered the MCDA score, where the value closest to 100 is considered the first best alternative.<br>"
		             )
		         ),

		         # output post generate
		         div(id="generated-output-2",
		             HTML(
		               "<br><b>Results Interpretation</b> for Figure 5: Recall that the decision criteria ratings under every dam tab were required to sum to 1. Here, the colored segments within each bar show the contribution of each decision criterion toward each decision\
					         alternative score for this dam. The decision alternative scores are calculated by weighting (multiplying) normalized dam-specific data for each criterion by your preference information for this dam. The largest segments show which criterion most drive the total score for each decision alternative. \
		               It is up to you to decide what to do with this information. <br>"
		             ),

		             h3('Figure 5. Medway Decision Alternative Scores by Decision Criteria'),
		             #tableOutput("WSMTable1"), # for debugging plot1
		             plotOutput("WSMPlot2a", height=600, width="100%"),
					downloadButton("DownloadDam2Plota", "Download Graph"),

		             HTML(
		               "<br><b>Results Interpretation</b> for Figure 6: The decision criterion with the largest bar shows where your overall priority lies, based on your preference information and the data for each decision criterion. Since preferences for decision criteria change from one dam to another, you may see variation between\
				            the prioritized decision alternatives. It is up to you as a decision maker to decide what to do with this information.<br>"
		             ),

		             h3('Figure 6. Total Decision Criteria Scores by Decision Alternative for Medway'),
		             plotOutput("WSMPlot2b", height=1000, width="100%"),
					 downloadButton("DownloadDam2Plotb", "Download Graph"),

					 plotOutput("WSMPlot2c", height=1000, width="100%"),
					 downloadButton("DownloadDam2Plotc", "Download Graph"),

		             HTML(
		               "<br><b>Questions for consideration:</b> Do these results match your expectations? If not, why? If you feel discomfort at the result, you can return to the decision alternative tabs and re-evaluate your criteria ratings. Remember to press \"Update\" under each Alternative tab. Then, return to the Output page and click GENERATE\
		               once more to see how your results change (note: you may want to download your results from this session, first).<br>\

		               <br> Do these results make sense, given the tradeoffs you made in balancing the set of decision criteria for each dam? Recall that the decision criteria are fully compensating, meaning that as the preference value for one increases, the value for another \
		               must necessarily decrease. The idea here is to emphasize tradeoffs between decision criteria.<br> \

		               <br><b>Next Steps</b>: You may download and save your results for personal reference, before continuing to the next step. If you are participating in the Dam Decision-Making Workshop, please save your results at this time."
		             ),

		             h3('Download Medway Results'),
		             downloadButton("downloadData2", "Download Medway")
				 )
		),

		tabPanel("Dam 3: Millinocket Dam",
		         h2("Results: Millinocket Dam"),
		         # raw preference table/Matrix
		         #raw preference graph
		         div(id="dam-3-output",
		             h3("Table 9. Raw preference scores for Medway Dam"),
		             DT::dataTableOutput("RawPrefsDam3"),
		             HTML(
		               "This table of preference data is depicted below."
		             ),
		             h3("Figure 7. Raw Preference Scores for Millinocket"),
		             plotOutput("PrefPlot3", height=graph_height, width=graph_width),
		             HTML(
		               "<br><b>Results Interpretation</b> for Figure 7: The bars visually represent your preference scores for each decision criterion.\
		               The scores are pulled directly from your slider bar settings under the Millinocket Dam tab and are not changed in any way. If you wish to go back and change your settings, please do so before continuing.<br>"
		             ),

		             #raw data table/Matrix
		             h3("Table 10. Raw data values for Millinocket Dam"),
		             DT::dataTableOutput("Dam3RawTable"),
		             HTML(
		               "<br><b>Results Interpretation</b> for Table 10. These are the raw data values for the dam development. We include the raw data values here to help make the MCDA calculation more transparent."
		             ),
		             # normalized data table/Matrix
		             h3("Table 11. Normalized data values for Millinocket Dam"),
		             DT::dataTableOutput("Dam3NormTable"),
		             HTML(
		               "<br><b>Results Interpretation</b> for Table 11. These are the normalized data values for the dam development. Raw data values have been normalized to a range between 0 and 1 to make them comparable across different units.\
		               Normalization was performed using a min/max procedure, where the highest values for most decision criteria are set equal to 1, and the lowest values are set equal to 0. For decision criteria where lower values are better \
		               (e.g. annuitized project cost, breach hazard potential, number of properties impacted), the highest values are actually set equal to 0, and the lowest values are set equal to 1. This allows us to indicate that, for instance,\
		               high costs are less desirable than low costs.<br>"
		             ),
		             #weighted score data table/Matrix
		             h3("Table 12. Weighted scores for Millinocket Dam"),
		             DT::dataTableOutput("Dam3ScoreTable"),
					 downloadButton("DownloadDam3ScoreTable", "Download Table"),
		             HTML(
		               "<br><b>Results Interpretation</b> for Table 12. These are the raw data for the dam development. Normalized data values have been multiplied by your preference scores to achieve a weighted score.\
		               The weighted sum for each decision alternative is considered the MCDA score, where the value closest to 100 is considered the first best alternative.<br>"
		             )
		       ),

				 # output post generate
		         div(id="generated-output-3",
		             HTML(
		               "<br><b>Results Interpretation</b> for Figure 8: Recall that the decision criteria ratings under every dam tab were required to sum to 1. Here, the colored segments within each bar show the contribution of each decision criterion toward each decision\
					         alternative score for this dam. The decision alternative scores are calculated by weighting (multiplying) normalized dam-specific data for each criterion by your preference information for this dam. The largest segments show which criterion most drive the total score for each decision alternative. \
		               It is up to you to decide what to do with this information. <br>"
		             ),

		             h3('Figure 8. Millinocket Decision Alternative Scores by Decision Criteria'),
		             #tableOutput("WSMTable1"), # for debugging plot1
		             plotOutput("WSMPlot3a", height=600, width="100%"),
					 downloadButton("DownloadDam3Plota", "Download Graph"),

		             HTML(
		               "<br><b>Results Interpretation</b> for Figure 9: The decision criterion with the largest bar shows where your overall priority lies, based on your preference information and the data for each decision criterion. Since preferences for decision criteria change from one dam to another, you may see variation between\
				            the prioritized decision alternatives. It is up to you as a decision maker to decide what to do with this information.<br>"
		             ),

		             h3('Figure 9. Total Decision Criteria Scores by Decision Alternative for Millinocket'),
		             plotOutput("WSMPlot3b", height=1000, width="100%"),
					 downloadButton("DownloadDam3Plotb", "Download Graph"),

					 plotOutput("WSMPlot3c", height=1000, width="100%"),
					 downloadButton("DownloadDam3Plotc", "Download Graph"),

		             HTML(
		               "<br><b>Questions for consideration:</b> Do these results match your expectations? If not, why? If you feel discomfort at the result, you can return to the decision alternative tabs and re-evaluate your criteria ratings. Remember to press \"Update\" under each Alternative tab. Then, return to the Output page and click GENERATE\
		               once more to see how your results change (note: you may want to download your results from this session, first).<br>\

		               <br> Do these results make sense, given the tradeoffs you made in balancing the set of decision criteria for each dam? Recall that the decision criteria are fully compensating, meaning that as the preference value for one increases, the value for another \
		               must necessarily decrease. The idea here is to emphasize tradeoffs between decision criteria.<br> \

		               <br><b>Next Steps</b>: You may download and save your results for personal reference, before continuing to the next step. If you are participating in the Dam Decision-Making Workshop, please save your results at this time."
		             ),

		             h3('Download Millinocket Results'),
		             downloadButton("downloadData3", "Download Millinocket")
				 )
		),

		tabPanel("Dam 4: East Millinocket Dam",
		         h2("Results: East Millinocket Dam"),
		         div(id="dam-4-output",
		             h3("Table 13. Raw preference scores for East Millinocket Dam"),
		             DT::dataTableOutput("RawPrefsDam4"),
		             HTML(
		               "This table of preference data is depicted below."
		             ),

		             h3("Figure 10. Raw Preference Scores for East Millinocket"),
					       plotOutput("PrefPlot4", height=graph_height, width=graph_width),
					       HTML(
					         "<br><b>Results Interpretation</b> for Figure 10: The bars visually represent your preference scores for each decision criterion.\
							 The scores are pulled directly from your slider bar settings under the East Millinocket Dam tab and are not changed in any way. If you wish to go back and change your settings, please do so before continuing.<br>"
					       ),

					       #raw data table/Matrix
					       h3("Table 14 Raw data values for East Millinocket Dam"),
					       DT::dataTableOutput("Dam4RawTable"),
					       HTML(
					         "<br><b>Results Interpretation</b> for Table 14. These are the raw data values for the dam development. We include the raw data values here to help make the MCDA calculation more transparent."
					       ),
					       # normalized data table/Matrix
					       h3("Table 15. Normalized data values for East Millinocket Dam"),
					       DT::dataTableOutput("Dam4NormTable"),
					       HTML(
					         "<br><b>Results Interpretation</b> for Table 15. These are the normalized data values for the dam development. Raw data values have been normalized to a range between 0 and 1 to make them comparable across different units.\
					         Normalization was performed using a min/max procedure, where the highest values for most decision criteria are set equal to 1, and the lowest values are set equal to 0. For decision criteria where lower values are better \
					         (e.g. annuitized project cost, breach hazard potential, number of properties impacted), the highest values are actually set equal to 0, and the lowest values are set equal to 1. This allows us to indicate that, for instance,\
					         high costs are less desirable than low costs.<br>"
					       ),
					       #weighted score data table/Matrix
					       h3("Table 16. Weighted scores for East Millinocket Dam"),
					       DT::dataTableOutput("Dam4ScoreTable"),
						   downloadButton("DownloadDam4ScoreTable", "Download Table"),
					       HTML(
					         "<br><b>Results Interpretation</b> for Table 16. These are the raw data for the dam development. Normalized data values have been multiplied by your preference scores to achieve a weighted score.\
					         The weighted sum for each decision alternative is considered the MCDA score, where the value closest to 100 is considered the first best alternative.<br>"
					       )
  				   ),

				     # output post generate
		         div(id="generated-output-4",
		             HTML(
		               "<br><b> Results Interpretation</b> for Figure 11: Recall that the decision criteria ratings under every dam tab were required to sum to 1. Here, the colored segments within each bar show the contribution of each decision criterion toward each decision\
					   alternative score for this dam. The decision alternative scores are calculated by weighting (multiplying) normalized dam-specific data for each criterion by your preference information for this dam. The largest segments show which criterion most drive the total score for each decision alternative. \
		               It is up to you to decide what to do with this information. <br>"
		             ),

		             h3('Figure 11. East Millinocket Decision Alternative Scores by Decision Criteria'),
		             plotOutput("WSMPlot4a", height=600, width="100%"),
					 downloadButton("DownloadDam4Plota", "Download Graph"),

		             HTML(
		               "<br><b>Results Interpretation</b> for Figure 12: The decision criterion with the largest bar shows where your overall priority lies, based on your preference information and the data for each decision criterion. Since preferences for decision criteria change from one dam to another, you may see variation between\
				            the prioritized decision alternatives. It is up to you as a decision maker to decide what to do with this information. <br>"
		             ),

		             h3('Figure 12. Total Decision Criteria Scores by Decision Alternative for East Millinocket'),
		             plotOutput("WSMPlot4b", height=1000, width="100%"),
					 downloadButton("DownloadDam4Plotb", "Download Graph"),

					 plotOutput("WSMPlot4c", height=1000, width="100%"),
					 downloadButton("DownloadDam4Plotc", "Download Graph"),

		             HTML(
		               "<br><b>Questions for consideration:</b> Do these results match your expectations? If not, why? If you feel discomfort at the result, you can return to the decision alternative tabs and re-evaluate your criteria ratings. Remember to press \"Update\" under each Alternative tab. Then, return to the Output page and click GENERATE\
		               once more to see how your results change (note: you may want to download your results from this session, first).<br>\

		               <br> Do these results make sense, given the tradeoffs you made in balancing the set of decision criteria for each dam? Recall that the decision criteria are fully compensating, meaning that as the preference value for one increases, the value for another \
		               must necessarily decrease. The idea here is to emphasize tradeoffs between decision criteria.<br> \

		               <br><b>Next Steps</b>: You may download and save your results for personal reference, before continuing to the next step. If you are participating in the Dam Decision-Making Workshop, please save your results at this time."
		             ),

		             h3('Download East Millinocket Results'),
		             downloadButton("downloadData4", "Download East Millinocket")
				 )
		 ),

		tabPanel("Dam 5: North Twin",
		         h2("Results: North Twin Dam"),
		         div(id="dam-5-output",
		             h3("Table 17. Raw preference scores for North Twin Dam"),
		             DT::dataTableOutput("RawPrefsDam5"),
		             HTML(
		               "This table of preference data is depicted below."
		             ),
		             HTML(
		               "<br><b>Results Interpretation</b> for Figure 13: The bars visually represent your preference scores for each decision criterion.\
		               The scores are pulled directly from your slider bar settings under the West Enfield Dam tab and are not changed in any way. If you wish to go back and change your settings, please do so before continuing.<br>"
		             ),
		             h3("Figure 13. Raw Preference Scores for North Twin"),
		             plotOutput("PrefPlot5", height=graph_height, width=graph_width),

		             #raw data table/Matrix
		             h3("Table 18. Raw data values for North Twin Dam"),
		             DT::dataTableOutput("Dam5RawTable"),
		             HTML(
		               "<br><b>Results Interpretation</b> for Table 18. These are the raw data values for the dam development. We include the raw data values here to help make the MCDA calculation more transparent."
		             ),
		             # normalized data table/Matrix
		             h3("Table 19. Normalized data values for North Twin Dam"),
		             DT::dataTableOutput("Dam5NormTable"),
		             HTML(
		               "<br><b>Results Interpretation</b> for Table 19. These are the normalized data values for the dam development. Raw data values have been normalized to a range between 0 and 1 to make them comparable across different units.\
		               Normalization was performed using a min/max procedure, where the highest values for most decision criteria are set equal to 1, and the lowest values are set equal to 0. For decision criteria where lower values are better \
		               (e.g. annuitized project cost, breach hazard potential, number of properties impacted), the highest values are actually set equal to 0, and the lowest values are set equal to 1. This allows us to indicate that, for instance,\
		               high costs are less desirable than low costs.<br>"
		             ),
		             #weighted score data table/Matrix
		             h3("Table 20. Weighted scores for North Twin Dam"),
		             DT::dataTableOutput("Dam5ScoreTable"),
					 downloadButton("DownloadDam5ScoreTable", "Download Table"),
		             HTML(
		               "<br><b>Results Interpretation</b> for Table 20. These are the raw data for the dam development. Normalized data values have been multiplied by your preference scores to achieve a weighted score.\
		               The weighted sum for each decision alternative is considered the MCDA score, where the value closest to 100 is considered the first best alternative.<br>"
		             )

		         ),

		         # output post generate
		         div(id="generated-output-5",
		             HTML(
		               "<br><b>Results Interpretation</b> for Figure 14: Recall that the decision criteria ratings under every dam tab were required to sum to 1. Here, the colored segments within each bar show the contribution of each decision criterion toward each decision\
					         alternative score for this dam. The decision alternative scores are calculated by weighting (multiplying) normalized dam-specific data for each criterion by your preference information for this dam. The largest segments show which criterion most drive the total score for each decision alternative. \
		               It is up to you to decide what to do with this information. <br>"
		             ),

		             h3('Figure 14. North Twin Decision Alternative Scores by Decision Criteria'),
		             plotOutput("WSMPlot5a", height=600, width="100%"),
					 downloadButton("DownloadDam5Plota", "Download Graph"),

		             HTML(
		               "<br><b>Results Interpretation</b> for Figure 15: The decision criterion with the largest bar shows where your overall priority lies, based on your preference information and the data for each decision criterion. Since preferences for decision criteria change from one dam to another, you may see variation between\
				            the prioritized decision alternatives. It is up to you as a decision maker to decide what to do with this information.<br>"
		             ),

		             h3('Figure 15. Total Decision Criteria Scores by Decision Alternative for North Twin'),
		             plotOutput("WSMPlot5b", height=1000, width="100%"),
					 downloadButton("DownloadDam5Plotb", "Download Graph"),

					 plotOutput("WSMPlot5c", height=1000, width="100%"),
					 downloadButton("DownloadDam5Plotc", "Download Graph"),

		             HTML(
		               "<br><b>Questions for consideration:</b> Do these results match your expectations? If not, why? If you feel discomfort at the result, you can return to the decision alternative tabs and re-evaluate your criteria ratings. Remember to press \"Update\" under each Alternative tab. Then, return to the Output page and click GENERATE\
		               once more to see how your results change (note: you may want to download your results from this session, first).<br>\

		               <br> Do these results make sense, given the tradeoffs you made in balancing the set of decision criteria for each dam? Recall that the decision criteria are fully compensating, meaning that as the preference value for one increases, the value for another \
		               must necessarily decrease. The idea here is to emphasize tradeoffs between decision criteria.<br> \

		               <br><b>Next Steps</b>: You may download and save your results for personal reference, before continuing to the next step. If you are participating in the Dam Decision-Making Workshop, please save your results at this time."
		             ),

		             h3('Download North Twin Results'),
		             downloadButton("downloadData5", "Download North Twin")
				 )
		 ),

		tabPanel("Dam 6: Dolby",
		         h2("Results: Dolby Dam"),
		         div(id="dam-6-output",
		             h3("Table 21. Raw preference scores for Dolby Dam"),
		             DT::dataTableOutput("RawPrefsDam6"),
		             HTML(
		               "This table of preference data is depicted below."
		             ),
		             h3("Figure 16. Raw Preference Scores for Dolby"),
		             plotOutput("PrefPlot6", height=graph_height, width=graph_width),
		             HTML(
		               "<br><b>Results Interpretation</b> for Figure 16: The bars visually represent your preference scores for each decision criterion.\
		               The scores are pulled directly from your slider bar settings under the West Enfield Dam tab and are not changed in any way. If you wish to go back and change your settings, please do so before continuing.<br>"
		             ),
		             #raw data table/Matrix
		             h3("Table 22. Raw data values for Dolby Dam"),
		             DT::dataTableOutput("Dam6RawTable"),
		             HTML(
		               "<br><b>Results Interpretation</b> for Table 22. These are the raw data values for the dam development. We include the raw data values here to help make the MCDA calculation more transparent."
		             ),
		             # normalized data table/Matrix
		             h3("Table 23. Normalized data values for Dolby Dam"),
		             DT::dataTableOutput("Dam6NormTable"),
		             HTML(
		               "<br><b>Results Interpretation</b> for Table 23. These are the normalized data values for the dam development. Raw data values have been normalized to a range between 0 and 1 to make them comparable across different units.\
		               Normalization was performed using a min/max procedure, where the highest values for most decision criteria are set equal to 1, and the lowest values are set equal to 0. For decision criteria where lower values are better \
		               (e.g. annuitized project cost, breach hazard potential, number of properties impacted), the highest values are actually set equal to 0, and the lowest values are set equal to 1. This allows us to indicate that, for instance,\
		               high costs are less desirable than low costs.<br>"
		             ),
		             #weighted score data table/Matrix
		             h3("Table 24. Weighted scores for Dolby Dam"),
		             DT::dataTableOutput("Dam6ScoreTable"),
					 downloadButton("DownloadDam6ScoreTable", "Download Table"),

		             HTML(
		               "<br><b>Results Interpretation</b> for Table 24. These are the raw data for the dam development. Normalized data values have been multiplied by your preference scores to achieve a weighted score.\
		               The weighted sum for each decision alternative is considered the MCDA score, where the value closest to 100 is considered the first best alternative.<br>"
		             )
		         ),

					  # output post generate
		         div(id="generated-output-6",
		             HTML(
		               "<br><b>Results Interpretation</b>for Figure 17: Recall that the decision criteria ratings under every dam tab were required to sum to 1. Here, the colored segments within each bar show the contribution of each decision criterion toward each decision\
					         alternative score for this dam. The decision alternative scores are calculated by weighting (multiplying) normalized dam-specific data for each criterion by your preference information for this dam. The largest segments show which criterion most drive the total score for each decision alternative. \
		               It is up to you to decide what to do with this information.<br>"
		             ),

		             h3('Figure 17. Dolby Decision Alternative Scores by Decision Criteria'),
		             plotOutput("WSMPlot6a", height=600, width="100%"),
					 downloadButton("DownloadDam6Plota", "Download Graph"),

		             HTML(
		               "<br><b>Results Interpretation</b> for Figure 18: The decision criterion with the largest bar shows where your overall priority lies, based on your preference information and the data for each decision criterion. Since preferences for decision criteria change from one dam to another, you may see variation between\
				            the prioritized decision alternatives. It is up to you as a decision maker to decide what to do with this information.<br>"
		             ),

		             h3('Figure 18. Total Decision Criteria Scores by Decision Alternative for Dolby'),
		             plotOutput("WSMPlot6b", height=1000, width="100%"),
					 downloadButton("DownloadDam6Plotb", "Download Graph"),

					 plotOutput("WSMPlot6c", height=1000, width="100%"),
					 downloadButton("DownloadDam6Plotc", "Download Graph"),

		             HTML(
		               "<br><b>Questions for consideration:</b> Do these results match your expectations? If not, why? If you feel discomfort at the result, you can return to the decision alternative tabs and re-evaluate your criteria ratings. Remember to press \"Update\" under each Alternative tab. Then, return to the Output page and click GENERATE\
		               once more to see how your results change (note: you may want to download your results from this session, first).<br>\

		               <br> Do these results make sense, given the tradeoffs you made in balancing the set of decision criteria for each dam? Recall that the decision criteria are fully compensating, meaning that as the preference value for one increases, the value for another \
		               must necessarily decrease. The idea here is to emphasize tradeoffs between decision criteria.<br> \

					   <br><b>Next Steps</b>: You may download and save your results for personal reference, before continuing to the next step. If you are participating in the Dam Decision-Making Workshop, please save your results at this time."
		             ),

		             h3('Download Dolby Results'),
		             downloadButton("downloadData6", "Download Dolby")
				 )
		 ),

		tabPanel("Dam 7: Millinocket Lake",
		         h2("Results: Millinocket Lake Dam"),
		         div(id="dam-7-output",
		             h3("Table 25. Raw preference scores for Millinocket Lake Dam"),
		             DT::dataTableOutput("RawPrefsDam7"),
		             HTML(
		               "The table of preference data is depicted below."
		             ),
		             h3("Figure 19. Raw Preference Scores for Millinocket Lake"),
		             plotOutput("PrefPlot7", height=graph_height, width=graph_width),
		             HTML(
		               "<br><b>Results Interpretation</b> for Figure 19: The bars visually represent your preference scores for each decision criterion.\
		               The scores are pulled directly from your slider bar settings under the West Enfield Dam tab and are not changed in any way. If you wish to go back and change your settings, please do so before continuing.<br>"
		             ),
		             #raw data table/Matrix
		             h3("Table 26. Raw data values for Millinocket Lake Dam"),
		             DT::dataTableOutput("Dam7RawTable"),
		             HTML(
		               "<br><b>Results Interpretation</b> for Table 26. These are the raw data values for the dam development. We include the raw data values here to help make the MCDA calculation more transparent."
		             ),
		             # normalized data table/Matrix
		             h3("Table 27. Normalized data values for Millinocket Lake Dam"),
		             DT::dataTableOutput("Dam7NormTable"),
		             HTML(
		               "<br><b>Results Interpretation</b> for Table 27. These are the normalized data values for the dam development. Raw data values have been normalized to a range between 0 and 1 to make them comparable across different units.\
		               Normalization was performed using a min/max procedure, where the highest values for most decision criteria are set equal to 1, and the lowest values are set equal to 0. For decision criteria where lower values are better \
		               (e.g. annuitized project cost, breach hazard potential, number of properties impacted), the highest values are actually set equal to 0, and the lowest values are set equal to 1. This allows us to indicate that, for instance,\
		               high costs are less desirable than low costs.<br>"
		             ),
		             #weighted score data table/Matrix
		             h3("Table 28. Weighted scores for Milinocket Lake Dam"),
		             DT::dataTableOutput("Dam7ScoreTable"),
					 downloadButton("DownloadDam7ScoreTable", "Download Table"),
		             HTML(
		               "<br><b>Results Interpretation</b> for Table 28. These are the raw data for the dam development. Normalized data values have been multiplied by your preference scores to achieve a weighted score.\
		               The weighted sum for each decision alternative is considered the MCDA score, where the value closest to 100 is considered the first best alternative.<br>"
		             )
  		         ),

		         # output post generate
		         div(id="generated-output-7",

		             HTML(
		               "<br><b>Results Interpretation </b> for Figure 20: Recall that the decision criteria ratings under every dam tab were required to sum to 1. Here, the colored segments within each bar show the contribution of each decision criterion toward each decision\
					         alternative score for this dam. The decision alternative scores are calculated by weighting (multiplying) normalized dam-specific data for each criterion by your preference information for this dam. The largest segments show which criterion most drive the total score for each decision alternative. \
		               It is up to you to decide what to do with this information. <br>"
		             ),

		             h3('Figure 20. Millinocket Lake Decision Alternative Scores by Decision Criteria'),
		             plotOutput("WSMPlot7a", height=600, width="100%"),
					 downloadButton("DownloadDam7Plota", "Download Graph"),

		             HTML(
		               "<br><b>Results Interpretation</b> for Figure 21: The decision criterion with the largest bar shows where your overall priority lies, based on your preference information and the data for each decision criterion. Since preferences for decision criteria change from one dam to another, you may see variation between\
				          the prioritized decision alternatives. It is up to you as a decision maker to decide what to do with this information.<br>"
		             ),

		             h3('Figure 21. Total Decision Criteria Scores by Decision Alternative for Millinocket Lake'),
		             plotOutput("WSMPlot7b", height=1000, width="100%"),
					 downloadButton("DownloadDam7Plotb", "Download Graph"),

					 plotOutput("WSMPlot7c", height=1000, width="100%"),
					 downloadButton("DownloadDam7Plotc", "Download Graph"),

		             HTML(
		               "<br><b>Questions for consideration:</b> Do these results match your expectations? If not, why? If you feel discomfort at the result, you can return to the decision alternative tabs and re-evaluate your criteria ratings. Remember to press \"Update\" under each Alternative tab. Then, return to the Output page and click GENERATE\
		               once more to see how your results change (note: you may want to download your results from this session, first).<br>\

		               <br> Do these results make sense, given the tradeoffs you made in balancing the set of decision criteria for each dam? Recall that the decision criteria are fully compensating, meaning that as the preference value for one increases, the value for another \
		               must necessarily decrease. The idea here is to emphasize tradeoffs between decision criteria.<br> \

		               <br><b>Next Steps</b>: You may download and save your results for personal reference, before continuing to the next step. If you are participating in the Dam Decision-Making Workshop, please save your results at this time."
		             ),

		             h3('Download Millinocket Lake Results'),
		             downloadButton("downloadData7", "Download Millinocket Lake")
			 )
		 ),

		tabPanel("Dam 8: Ripogenus",
		         h2("Results: Ripogenus Dam"),
		         div(id="dam-8-output",
		             h3("Table 29. Raw preference scores for Ripogenus Dam"),
		             DT::dataTableOutput("RawPrefsDam8"),
		             HTML(
		               "This table of preference data is depicted below."
		             ),

		             h3("Figure 22. Raw Preference Scores for Ripogenus"),
		             plotOutput("PrefPlot8", height=graph_height, width=graph_width),
		             HTML(
		               "<br><b>Results Interpretation</b> for Figure 22: The bars visually represent your preference scores for each decision criterion.\
		               The scores are pulled directly from your slider bar settings under the West Enfield Dam tab and are not changed in any way. If you wish to go back and change your settings, please do so before continuing.<br>"
		             ),

		             #raw data table/Matrix
		             h3("Table 30. Raw data values for Ripogenus Dam"),
		             DT::dataTableOutput("Dam8RawTable"),
		             HTML(
		               "<br><b>Results Interpretation</b> for Table 30. These are the raw data values for the dam development. We include the raw data values here to help make the MCDA calculation more transparent."
		             ),
		             # normalized data table/Matrix
		             h3("Table 31. Normalized data values for Ripogenus Dam"),
		             DT::dataTableOutput("Dam8NormTable"),
		             HTML(
		               "<br><b>Results Interpretation</b> for Table 31. These are the normalized data values for the dam development. Raw data values have been normalized to a range between 0 and 1 to make them comparable across different units.\
		               Normalization was performed using a min/max procedure, where the highest values for most decision criteria are set equal to 1, and the lowest values are set equal to 0. For decision criteria where lower values are better \
		               (e.g. annuitized project cost, breach hazard potential, number of properties impacted), the highest values are actually set equal to 0, and the lowest values are set equal to 1. This allows us to indicate that, for instance,\
		               high costs are less desirable than low costs.<br>"
		             ),
		             #weighted score data table/Matrix
		             h3("Table 32. Weighted scores for Ripogenus Dam"),
		             DT::dataTableOutput("Dam8ScoreTable"),
					 downloadButton("DownloadDam8ScoreTable", "Download Table"),
		             HTML(
		               "<br><b>Results Interpretation</b> for Table 32. These are the raw data for the dam development. Normalized data values have been multiplied by your preference scores to achieve a weighted score.\
		               The weighted sum for each decision alternative is considered the MCDA score, where the value closest to 100 is considered the first best alternative.<br>"
		             )
		         ),

		         # output post generate
		         div(id="generated-output-8",
		             HTML(
		               "<br><b>Results Interpretation</b> for Figure 23: Recall that the decision criteria ratings under every dam tab were required to sum to 1. Here, the colored segments within each bar show the contribution of each decision criterion toward each decision\
				          	alternative score for this dam. The decision alternative scores are calculated by weighting (multiplying) normalized dam-specific data for each criterion by your preference information for this dam. The largest segments show which criterion most drive the total score for each decision alternative. \
                    It is up to you to decide what to do with this information. <br>"
		             ),

		             h3('Figure 23. Ripogenus Decision Alternative Scores by Decision Criteria'),
		             plotOutput("WSMPlot8a", height=600, width="100%"),
					 downloadButton("DownloadDam8Plota", "Download Graph"),

		             HTML(
		               "<br><b>Results Interpretation</b> for Figure 24: The decision criterion with the largest bar shows where your overall priority lies, based on your preference information and the data for each decision criterion. Since preferences for decision criteria change from one dam to another, you may see variation between\
				            the prioritized decision alternatives. It is up to you as a decision maker to decide what to do with this information.<br>"
		             ),

		             h3('Figure 24. Total Decision Criteria Scores by Decision Alternative for Ripogenus'),
		             plotOutput("WSMPlot8b", height=1000, width="100%"),
					 downloadButton("DownloadDam8Plotb", "Download Graph"),

					 plotOutput("WSMPlot8c", height=1000, width="100%"),
					 downloadButton("DownloadDam8Plotc", "Download Graph"),

		               HTML(
		                 "<br><b>Questions for consideration:</b> Do these results match your expectations? If not, why? If you feel discomfort at the result, you can return to the decision alternative tabs and re-evaluate your criteria ratings. Remember to press \"Update\" under each Alternative tab. Then, return to the Output page and click GENERATE\
		                 once more to see how your results change (note: you may want to download your results from this session, first).<br>\

		                 <br> Do these results make sense, given the tradeoffs you made in balancing the set of decision criteria for each dam? Recall that the decision criteria are fully compensating, meaning that as the preference value for one increases, the value for another \
		                 must necessarily decrease. The idea here is to emphasize tradeoffs between decision criteria.<br> \

		                 <br><b>Next Steps</b>: You may download and save your results for personal reference, before continuing to the next step. If you are participating in the Dam Decision-Making Workshop, please save your results at this time."
		               ),

		             h3('Download Ripogenus Results'),
		             downloadButton("downloadData8", "Download Ripogenus")
				 )
		),


		# Developer and aknowledgements secion
		HTML("<li class='step-label'>About</li>"),

		tabPanel("Developers",
			 h2("Developers"),
			 HTML(
				"<b>Emma Fox </b>- Lead Developer (Ph.D. candidate, University of Maine Ecology and Environmental Science Program) for the Dam Decision Support Tool. Designed user interface and initial server functionality. Adjusted WSM function for new dam decision application and advised model-related changes. \
				Designed and wrote app text, and designed accompanying multi-dam decision example fact sheets, designed and wrote text for Dam Toolbox.<br> \
				<br><b>Dr. Sharon J. W. Klein </b>- Development Advisor (Associate Professor, University of Maine School of Economics). Helped develop and advise concept for the Dam Decision Support Tool, advised user-friendliness enhancements to the Dam Decision Support Tool and user interface/features, refined criteria \
				definitions, revised app text.<br> \
				<br><b>Dr. Samuel G. Roy </b>- Feature Developer (Postdoctoral Researcher, Senator George J. Mitchell Center for Sustainability Solutions, University of Maine) for the Dam Decision Support Tool. Created original Matlab scripts for multi-rank calculation, fitness functions for network-dependent criteria, and all maps.<br>\
				<br><b>William Winslow </b>- Developer (Software Engineer, GeoSpatial Science Center(GSSC), University of New Hampshire). Deployment (Docker, Apache), server code reorganization, debugging/bug fixes, misc. feature implementations for user interface and app function.<br>"
			 )
		 ),


		tabPanel("Acknowledgements",
			 h2("Acknowledgments"),
			 HTML(
				 "We would like to thank Garrett Raymond for his technical consultation on R Shiny design.<br> \
				 <br> This tool was developed by researchers in the Future of Dams project. Support for the Future of Dams project is provided by the National Science Foundation\'s Research Infrastructure Improvement NSF #IIA-1539071, \
				 USDA National Institute of Food and Agriculture, Hatch project 0230040, and Department of the Interior, \
				 U.S. Geological Survey Grant No. G16AP00057 through the Senator George J. Mitchell Center at the University of Maine.<br>\
				 <br> The Data Discovery Center at the University of New Hampshire is the host for this Dam Decision Support Tool. https://ddc.unh.edu <br>"
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
