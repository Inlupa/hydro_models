MyGR4j = function(dates,
                  prec,
                  temp,
                  evap,
                  flow, # временные ряды переменных
                  ZInp, # средняя высота водосбора
                  startCalib,
                  endWarmUp, 
                  endCalib,
                  startValid,
                  endValid # даты начала или конца соответствующих периодов
                  ) 
{
  Index_Calib_WarmUp = seq(which(dates == as.Date(startCalib)),
                                     which(dates == as.Date(endWarmUp)))
  Index_Calib = seq(which(dates == as.Date(startCalib)),
                              which(dates == as.Date(endCalib)))
  Index_Valid = seq(which(dates == as.Date(startValid)),
                              which(dates == as.Date(endValid)))
  
  GR4j = airGR::CreateInputsModel(FUN_MOD = RunModel_CemaNeigeGR4J,
                                            DatesR = as.POSIXct(dates),
                                            Precip = prec,
                                            PotEvap = evap,
                                            TempMean = temp,
                                            ZInputs = ZInp)
  RunOptions_Calib = airGR::CreateRunOptions(FUN_MOD = RunModel_CemaNeigeGR4J,
                                                       InputsModel = GR4j,
                                                       IndPeriod_WarmUp = Index_Calib_WarmUp,
                                                       IndPeriod_Run = Index_Calib)
  CalibOptions = airGR::CreateCalibOptions(FUN_MOD = RunModel_CemaNeigeGR4J,
                                                     FUN_CALIB = Calibration_Michel)
  
  InputsCrit = airGR::CreateInputsCrit(FUN_CRIT = ErrorCrit_NSE,
                                                 InputsModel = GR4j,
                                                 RunOptions = RunOptions_Calib,
                                                 Obs = flow[Index_Calib])
  Calibration = airGR::Calibration_Michel(InputsModel = GR4j,
                                                    RunOptions = RunOptions_Calib,
                                                    InputsCrit = InputsCrit,
                                                    CalibOptions = CalibOptions,
                                                    FUN_MOD = RunModel_CemaNeigeGR4J,
                                                    FUN_CRIT = ErrorCrit_NSE)
  Params = Calibration$ParamFinalR
  
  Modelling_Calib = airGR::RunModel_CemaNeigeGR4J(InputsModel = GR4j,
                                                            RunOptions = RunOptions_Calib,
                                                            Param = Calibration$ParamFinalR)
  plotCalib = plot(Modelling_Calib,
       Qobs = flow[Index_Calib])
  
  IniStates= airGR::CreateIniStates(FUN_MOD = RunModel_CemaNeigeGR4J,
                                              InputsModel = GR4j,
                                              ProdStore = Modelling_Calib$StateEnd$Store[[1]],
                                              RoutStore = Modelling_Calib$StateEnd$Store[[2]],
                                              UH1 = Modelling_Calib$StateEnd$UH[[1]],
                                              UH2 = Modelling_Calib$StateEnd$UH[[2]],
                                              GCemaNeigeLayers = Modelling_Calib$StateEnd$CemaNeigeLayers[[1]],
                                              eTGCemaNeigeLayers = Modelling_Calib$StateEnd$CemaNeigeLayers[[2]])
  
  RunOptions = airGR::CreateRunOptions(FUN_MOD = RunModel_CemaNeigeGR4J,
                                                 InputsModel = GR4j,
                                                 IndPeriod_Run = Index_Valid,
                                                 IniStates = IniStates)
  Modelling = airGR::RunModel_CemaNeigeGR4J(InputsModel = GR4j,
                                                      RunOptions = RunOptions,
                                                      Param = Calibration$ParamFinalR)
  plotValid = plot(Modelling,
       Qobs = flow[Index_Valid])
  outputs = list(Calibration = Calibration,
                 #Params = Params, 
                 plotCalib = plotCalib,
                 plotValid = plotValid,
                 IniStates = IniStates,
                 ysim = Modelling$Qsim,
                 NSE_Calib = hydroGOF::NSE(sim = Modelling_Calib$Qsim,
                                           obs = flow[Index_Calib]),
                 NSE_Valid = hydroGOF::NSE(sim = Modelling$Qsim,
                                           obs = flow[Index_Valid])
                 )
  print(paste0('NSE калибровочного периода = ', outputs$NSE_Calib))
  print(paste0('NSE периода валидации = ', outputs$NSE_Valid))
  outputs
  
}
