library(airGR)
library(airGRteaching)

MyGR4j = function(dates,
                  prec,
                  temp,
                  evap,
                  flow, # временные ряды переменных
                  ZInp = F, # средняя высота водосбора
                  startCalib,
                  endWarmUp, 
                  endCalib,
                  startValid,
                  endValid, # даты начала или конца соответствующих периодов
                  transfo = F,
                  IS = F
                  ) 
{
  # Разделение данных на периоды разогрева, калибровки и валидации
  Index_Calib_WarmUp = seq(which(as.Date(dates) == as.Date(startCalib)),
                                     which(as.Date(dates) == as.Date(endWarmUp)))
  Index_Calib = seq(which(as.Date(dates) == as.Date(startCalib)),
                              which(as.Date(dates) == as.Date(endCalib)))
  Index_Valid = seq(which(as.Date(dates) == as.Date(startValid)),
                              which(as.Date(dates) == as.Date(endValid)))
  
  # Создание модели с обозначением входных рядов и данных о водосборе
  if (ZInp == F) {
     GR4j = airGR::CreateInputsModel(FUN_MOD = RunModel_CemaNeigeGR4J,
                                     DatesR = as.POSIXct(dates),
                                     Precip = prec,
                                     PotEvap = evap,
                                     TempMean = temp)
  }
  else {
     GR4j = airGR::CreateInputsModel(FUN_MOD = RunModel_CemaNeigeGR4J,
                                            DatesR = as.POSIXct(dates),
                                            Precip = prec,
                                            PotEvap = evap,
                                            TempMean = temp,
                                            ZInputs = ZInp)
  }
  
  
  # Выбор настроек калибровки (тип модели, модель водосбора, периоды разогрева и непосредственно калибровки)
  RunOptions_Calib = airGR::CreateRunOptions(FUN_MOD = RunModel_CemaNeigeGR4J,
                                                       InputsModel = GR4j,
                                                       IndPeriod_WarmUp = Index_Calib_WarmUp,
                                                       IndPeriod_Run = Index_Calib)
  
  # Установка опций калибровки (тип модели, алгоритм калибровки)
  CalibOptions = airGR::CreateCalibOptions(FUN_MOD = RunModel_CemaNeigeGR4J,
                                                     FUN_CALIB = Calibration_Michel)

  # Выбор критерия оптимизации (функции, модели водосбора, опций калибровки и ряда фактических значений)
  if (transfo == F) {
     InputsCrit = airGR::CreateInputsCrit(FUN_CRIT = ErrorCrit_NSE,
                                                 InputsModel = GR4j,
                                                 RunOptions = RunOptions_Calib,
                                                 Obs = flow[Index_Calib])
  }
  else {
     InputsCrit = airGR::CreateInputsCrit(FUN_CRIT = ErrorCrit_NSE,
                                          InputsModel = GR4j,
                                          RunOptions = RunOptions_Calib,
                                          Obs = flow[Index_Calib],
                                          transfo = transfo)
  }
  
  
  # Выполнение калибровки
  Calibration = airGR::Calibration_Michel(InputsModel = GR4j,
                                                    RunOptions = RunOptions_Calib,
                                                    InputsCrit = InputsCrit,
                                                    CalibOptions = CalibOptions,
                                                    FUN_MOD = RunModel_CemaNeigeGR4J,
                                                    FUN_CRIT = ErrorCrit_NSE)
  
  # Полученные в результате калибровки оптимальные параметры
  Params = Calibration$ParamFinalR
  
  # Модельный расчёт для периода калибровки
  Modelling_Calib = airGR::RunModel_CemaNeigeGR4J(InputsModel = GR4j,
                                                            RunOptions = RunOptions_Calib,
                                                            Param = Calibration$ParamFinalR)
  # Комплексный график для периода калибровки
  plotCalib = plot(Modelling_Calib,
       Qobs = flow[Index_Calib])
  
  # Начальные условия расчёта
  IniStates = airGR::CreateIniStates(FUN_MOD = RunModel_CemaNeigeGR4J,
                                              InputsModel = GR4j,
                                              ProdStore = Modelling_Calib$StateEnd$Store[[1]],
                                              RoutStore = Modelling_Calib$StateEnd$Store[[2]],
                                              UH1 = Modelling_Calib$StateEnd$UH[[1]],
                                              UH2 = Modelling_Calib$StateEnd$UH[[2]],
                                              GCemaNeigeLayers = Modelling_Calib$StateEnd$CemaNeigeLayers[[1]],
                                              eTGCemaNeigeLayers = Modelling_Calib$StateEnd$CemaNeigeLayers[[2]])
  
  # Установка опций для периода моделирования
  if (IS == F) {
     RunOptions = airGR::CreateRunOptions(FUN_MOD = RunModel_CemaNeigeGR4J,
                                          InputsModel = GR4j,
                                          IndPeriod_Run = Index_Valid)
  }
  else {
     RunOptions = airGR::CreateRunOptions(FUN_MOD = RunModel_CemaNeigeGR4J,
                                                 InputsModel = GR4j,
                                                 IndPeriod_Run = Index_Valid,
                                                 IniStates = IniStates)
  }
  
  # Модельный расчёт (для валидационного периода)
  Modelling = airGR::RunModel_CemaNeigeGR4J(InputsModel = GR4j,
                                                      RunOptions = RunOptions,
                                                      Param = Calibration$ParamFinalR)
  print('Есть модель для валидации')
  
  # Построение комплексного графика для периода калибровки
  plotValid = plot(Modelling,
       Qobs = flow[Index_Valid])
  
  # Результатом функции является список из объектов: результата калибровки, графиков, начальных условий, расчётного ряда, метрик NSE для калибровочного и валидационного периодов
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





