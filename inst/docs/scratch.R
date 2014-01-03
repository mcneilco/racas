
#Read In Data/Format
# library(racas)
# library(data.table)
# library(gdata)
# library(drc)
# dat <- read.xls("/Users/bbolt/Documents/clients/dns/biacore/09-17-13\ PDE2\ Affinity\ E0019632\ Rmax\ free.xls", sheet = 8, skip = 2, stringsAsFactors = FALSE)
# dat <- data.table(subset(dat, dat$Fc=="4-3 corr" & dat$Report.Point == "binding" & AssayStep == "Sample", select = c("Sample_1_Sample", "Sample_1_Conc", "RelResp")))
# setnames(dat, c("curveid", "dose", "response"))
# dat[ , c("dose", "response", "flag") := list(dose = as.numeric(dose), response = as.numeric(response), flag = FALSE)]
# setkey(dat, "curveid")
# fitData <- data.table(curveid = unique(dat$curveid), renderingHint = "2 parameter Michaelis Menten", points = split(dat, dat$curveid), key = "curveid")
# fitData[ , model.sync := FALSE]
# 
# 
library(racas)
library(data.table)
library(gdata)
library(drc)
library(xtable)
#curveids <- as.character(query("select curveid from api_curve_params")[[1]])
#fitData <- getFitData(curveids)
#save(fitData,file = "data/exampleFitData.rda")

data("exampleFitData", package = "racas")

# times <- 90
# fitDat <- fitData
# for(i in 1:times) {
#   fits <- copy(fitData)
#   fits <- fits[ ,curveid := paste0(curveid,i)]
#   fitDat <- rbind(fitDat, fits)
# }
# fitData <- fitDat
# fitData <- fitData[1:300]
data("exampleFitData", package = "racas")
file <- system.file("docs", "doseResponseRequest.json", package = "racas")
fitSettingsJSON <- readChar(file, file.info(file)$size)
response <- fitCall(fitSettingsJSON, curveid = "90820_AG-00242847")
response <- fitCall(fitSettingsJSON, sessionID = fromJSON(response)$sessionID)
n <- fitCall(fitSettingsJSON, fitData = fitData)

system.time(response <- fitCall(fitSettingsJSON, curveid = "126218_AG-00242848"))
parsedResponse <- fromJSON(response)
session <- parsedResponse$sessionID
loadSession(session)
parsedResponse$fitSummary
system.time(response <- fitCall(fitSettingsJSON, sessionID = sessionID))
system.time(response <- fitCall(fitSettingsJSON, sessionID = "/var/folders/gy/w31n6hjx1fn5n3lhdpk697q80000gn/T//Rtmp6bIh3f/rSe-206b1dd613d4"))

fitData[fitConverged == TRUE, { fittedParams <- fittedParameters[[1]]
                                names(fittedParams) <- paste0("fitted",names(fittedParams))
                                plotData(points[[1]], as.data.frame(c(curveid = curveid, name = curveid,fittedParams,fixedParameters[[1]])), LL4, paramNames = c("slope", "min", "max", "ec50"), logDose = TRUE, drawIntercept = "ec50", showLegend = TRUE, outFile = paste0(curveid,".png"), xmin = NA, ymin = NA, ymax = NA)}, by = curveid]



loadSession("/var/folders/gy/w31n6hjx1fn5n3lhdpk697q80000gn/T//RtmpjqSdaF/rSe-ab612d08d53")





me <- print(xtable(as.data.frame(fitData[1]$reportedParameters[[1]]), display = rep("E", 6), caption = fitData[1]$category), 
            caption.placement = "top",
            type = "html", 
            include.rownames = FALSE, 
            comment = FALSE, 
            timestamp = FALSE, 
            rotate.rownames = TRUE, 
            file = "~/Desktop/test.html",
            html.table.attributes = "") 
# 
# 
# times <- 20
# fitDat <- fitData
# for(i in 1:times) {
#   fits <- copy(fitData)
#   fits <- fits[ ,curveid := paste0(curveid,i)]
#   fitDat <- rbind(fitDat, fits)
# }
# fitData <- fitDat
# fitData <- fitData[1:30]
# 
# #Notes
# #Rendering Hint -> Fit Model
# 
# #Fit
# #stdErr, tValue or pValue options
# 
# 
# 
# myfailSettings.parameter <- list(list(parameter = "max", type = "stdErr", value = 3, operator = ">"),list(parameter = "kd", type = "pValue", value = 0.6, operator = ">"))
# myfailSettings.interpolatedValue <- list(kdThreshold = list(parameter = "kd", reference = "dose.max", type = "logAboveReference", value = 0.5, operator = ">"),list(parameter = "max", type = "threshold", value = 30, operator = ">"))
# myfailSettings.inactive <- list(threshold = 5, activeDoses = 2, mockControls = FALSE)
# myfixedParameters <- list(max = NA, kd = NA)
# fitData[ , c("failSettings.parameter", "failSettings.interpolatedValue", "failSettings.inactive", "fixedParameters") := list(list(myfailSettings.parameter),
#                                                                                                                              list(myfailSettings.interpolatedValue),
#                                                                                                                              list(myfailSettings.inactive),
#                                                                                                                              list(myfixedParameters))]
# system.time(fitData <- doseResponseFit(fitData))
# fitData[grepl("max.fail.interpolate",failResults.interpolatedValues), c("model.sync","fixedParameters") := list(model.sync = FALSE,
#                                                                                                                 list(myfixedParameters = list(max = 10, kd = NA)))
#         ]
# fitData <- doseResponseFit(fitData)
# 
# fitData[fitConverged == FALSE, c("model.sync","renderingHint", "fixedParameters") := list(model.sync = FALSE,
#                                                                                           renderingHint = "3 parameter Michaelis Menten",
#                                                                                           list(myfixedParameters = list(NA, NA, NA)))
#         ]
# system.time(fitData <- doseResponseFit(fitData))
# 
# 
# 
# fitData[fitConverged == TRUE, {png(file = paste0(curveid,".png"))
#                                plot(model[[1]], main = paste0(category,"\nkd: ",
#                                                               round(fittedParameters[[1]]$kd,2),
#                                                               " max: ",round(fittedParameters[[1]]$max,2),
#                                                               "\nmax stderr: ", round(goodnessOfFit.parameters[[1]]$max.stdErr,2),
#                                                               " kd pValue: ", round(goodnessOfFit.parameters[[1]]$kd.pValue,2)), ylim = c(0, 40))
#                                dev.off()}, by = curveid]
# 
# fitData[fitConverged == TRUE, plotData(points[[1]], as.data.frame(c(curveid = curveid, name = curveid,fittedParameters[[1]])), MM2, paramNames = c("kd","max"), logDose = TRUE, drawIntercept = "kd", showLegend = TRUE, outFile = paste0(curveid,".png"), xmin = 1, ymin = 0, ymax = 40), by = curveid]
