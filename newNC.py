from netCDF4 import Dataset
import scipy.io
import time
import datetime
import numpy as np




def main():

#weird constants

    
    Po = 1.0
    chl_darkc = 0.058
    chl_scale = 15.0
    tur_darkc = 0.075
    tur_scale = 43.0
    A0 = 1.0513
    A1 = -1.5E-3
    A2 = 3.9329E-1
    TA0 = 7.085069E-4
    TA1 = 2.521378E-4
    TA2 = 4.688456E-7
    TA3 = 1.067418E-7
    B0 = -2.3305E-1
    B1 = 1.6100
    C0 = 1.0309E-1
    C1 = 4.3474E-3
    C2 = 5.9044E-5
    E = 1.1E-2
    xmiss = -999.0


    f = open("raw.dat", "r")

    datapoints = []
    linecount = 0

    for line in f:
        datapoints.append(line.split(","))
        linecount += 1

    temp, cond, flor, turb, doxyp, doxyt, salt = ([] for i in range(7))


    for i in range(0, len(datapoints)):
        temp.append(datapoints[i][0])
        cond.append(float(datapoints[i][1]))
        flor.append(float(datapoints[i][2]))
        turb.append(float(datapoints[i][3]))
        doxyp.append(float(datapoints[i][4]))
        doxyt.append(float(datapoints[i][5]))
        salt.append(float(datapoints[i][6]))

    for i in range(len(temp)):
        temp[i] = float(temp[i].replace("#", ""))

    dataset = Dataset('test.nc', "w", format="NETCDF4")
    dataset.createDimension("timeseries", 1)
    dataset.createDimension("obs", None)

    temperatureVar = dataset.createVariable("temperature", "f4", ("obs","timeseries",), fill_value=xmiss)
    salinityVar = dataset.createVariable("salinity", "f4", ("obs","timeseries",), fill_value=xmiss)
    oxygenSatVar = dataset.createVariable("oxygen_saturation", "f4", ("obs","timeseries",), fill_value=xmiss)
    oxygenVar = dataset.createVariable("oxygen", "f4", ("obs","timeseries",), fill_value=xmiss)
    chlorophyllVar = dataset.createVariable("chlorophyll", "f4", ("obs","timeseries",), fill_value=xmiss)
    turbidityVar = dataset.createVariable("turbidity", "f4", ("obs","timeseries",), fill_value=xmiss)
    phVar = dataset.createVariable("ph", "f4", ("obs","timeseries",), fill_value=xmiss)

    temperatureVar_raw = dataset.createVariable("temperature_raw", "f4", ("obs","timeseries",), fill_value=xmiss)
    salinityVar_raw = dataset.createVariable("salinity_raw", "f4", ("obs","timeseries",), fill_value=xmiss)
    oxygenSatVar_raw = dataset.createVariable("oxygen_saturation_raw", "f4", ("obs","timeseries",), fill_value=xmiss)
    oxygenVar_raw = dataset.createVariable("oxygen_raw", "f4", ("obs","timeseries",), fill_value=xmiss)
    chlorophyllVar_raw = dataset.createVariable("chlorophyll_raw", "f4", ("obs","timeseries",), fill_value=xmiss)
    turbidityVar_raw = dataset.createVariable("turbidity_raw", "f4", ("obs","timeseries",), fill_value=xmiss)
    phVar_raw = dataset.createVariable("ph_raw", "f4", ("obs","timeseries",), fill_value=xmiss)

    temperatureVar_dm_qd = dataset.createVariable("temperature_dm_qd", "u4", ("obs","timeseries",), fill_value=xmiss)
    salinityVar_dm_qd = dataset.createVariable("salinity_dm_qd", "u4", ("obs","timeseries",), fill_value=xmiss)
    oxygenSatVar_dm_qd = dataset.createVariable("oxygen_saturation_dm_qd", "u4", ("obs","timeseries",), fill_value=xmiss)
    oxygenVar_dm_qd = dataset.createVariable("oxygen_dm_qd", "u4", ("obs","timeseries",), fill_value=xmiss)
    chlorophyllVar_dm_qd = dataset.createVariable("chlorophyll_dm_qd", "u4", ("obs","timeseries",), fill_value=xmiss)
    turbidityVar_dm_qd = dataset.createVariable("turbidity_dm_qd", "u4", ("obs","timeseries",), fill_value=xmiss)
    phVar_dm_qd = dataset.createVariable("ph_dm_qd", "u4", ("obs","timeseries",), fill_value=xmiss)

    temperatureVar_qc_gap = dataset.createVariable("temperature_qc_gap", "u4", ("obs","timeseries",), fill_value=xmiss)
    salinityVar_qc_gap = dataset.createVariable("salinity_qc_gap", "u4", ("obs","timeseries",), fill_value=xmiss)
    oxygenSatVar_qc_gap = dataset.createVariable("oxygen_saturation_qc_gap", "u4", ("obs","timeseries",), fill_value=xmiss)
    oxygenVar_qc_gap = dataset.createVariable("oxygen_qc_gap", "u4", ("obs","timeseries",), fill_value=xmiss)
    chlorophyllVar_qc_gap = dataset.createVariable("chlorophyll_qc_gap", "u4", ("obs","timeseries",), fill_value=xmiss)
    turbidityVar_qc_gap = dataset.createVariable("turbidity_qc_gap", "u4", ("obs","timeseries",), fill_value=xmiss)
    phVar_qc_gap = dataset.createVariable("ph_qc_gap", "u4", ("obs","timeseries",), fill_value=xmiss)

    temperatureVar_qc_syn = dataset.createVariable("temperature_qc_syn", "u4", ("obs","timeseries",), fill_value=xmiss)
    salinityVar_qc_syn = dataset.createVariable("salinity_qc_syn", "u4", ("obs","timeseries",), fill_value=xmiss)
    oxygenSatVar_qc_syn = dataset.createVariable("oxygen_saturation_qc_syn", "u4", ("obs","timeseries",), fill_value=xmiss)
    oxygenVar_qc_syn = dataset.createVariable("oxygen_qc_syn", "u4", ("obs","timeseries",), fill_value=xmiss)
    chlorophyllVar_qc_syn = dataset.createVariable("chlorophyll_qc_syn", "u4", ("obs","timeseries",), fill_value=xmiss)
    turbidityVar_qc_syn = dataset.createVariable("turbidity_qc_syn", "u4", ("obs","timeseries",), fill_value=xmiss)
    phVar_qc_syn = dataset.createVariable("ph_qc_syn", "u4", ("obs","timeseries",), fill_value=xmiss)
    
    temperatureVar_qc_loc = dataset.createVariable("temperature_qc_loc", "u4", ("obs","timeseries",), fill_value=xmiss)
    salinityVar_qc_loc = dataset.createVariable("salinity_qc_loc", "u4", ("obs","timeseries",), fill_value=xmiss)
    oxygenSatVar_qc_loc = dataset.createVariable("oxygen_saturation_qc_loc", "u4", ("obs","timeseries",), fill_value=xmiss)
    oxygenVar_qc_loc = dataset.createVariable("oxygen_qc_loc", "u4", ("obs","timeseries",), fill_value=xmiss)
    chlorophyllVar_qc_loc = dataset.createVariable("chlorophyll_qc_loc", "u4", ("obs","timeseries",), fill_value=xmiss)
    turbidityVar_qc_loc = dataset.createVariable("turbidity_qc_loc", "u4", ("obs","timeseries",), fill_value=xmiss)
    phVar_qc_loc = dataset.createVariable("ph_qc_loc", "u4", ("obs","timeseries",), fill_value=xmiss)

    temperatureVar_qc_rng = dataset.createVariable("temperature_qc_rng", "u4", ("obs","timeseries",), fill_value=xmiss)
    salinityVar_qc_rng = dataset.createVariable("salinity_qc_rng", "u4", ("obs","timeseries",), fill_value=xmiss)
    oxygenSatVar_qc_rng = dataset.createVariable("oxygen_saturation_qc_rng", "u4", ("obs","timeseries",), fill_value=xmiss)
    oxygenVar_qc_rng = dataset.createVariable("oxygen_qc_rng", "u4", ("obs","timeseries",), fill_value=xmiss)
    chlorophyllVar_qc_rng = dataset.createVariable("chlorophyll_qc_rng", "u4", ("obs","timeseries",), fill_value=xmiss)
    turbidityVar_qc_rng = dataset.createVariable("turbidity_qc_rng", "u4", ("obs","timeseries",), fill_value=xmiss)
    phVar_qc_rng = dataset.createVariable("ph_qc_rng", "u4", ("obs","timeseries",), fill_value=xmiss)

    temperatureVar_qc_clm = dataset.createVariable("temperature_qc_clm", "u4", ("obs","timeseries",), fill_value=xmiss)
    salinityVar_qc_clm = dataset.createVariable("salinity_qc_clm", "u4", ("obs","timeseries",), fill_value=xmiss)
    oxygenSatVar_qc_clm = dataset.createVariable("oxygen_saturation_qc_clm", "u4", ("obs","timeseries",), fill_value=xmiss)
    oxygenVar_qc_clm = dataset.createVariable("oxygen_qc_clm", "u4", ("obs","timeseries",), fill_value=xmiss)
    chlorophyllVar_qc_clm = dataset.createVariable("chlorophyll_qc_clm", "u4", ("obs","timeseries",), fill_value=xmiss)
    turbidityVar_qc_clm = dataset.createVariable("turbidity_qc_clm", "u4", ("obs","timeseries",), fill_value=xmiss)
    phVar_qc_clm = dataset.createVariable("ph_qc_clm", "u4", ("obs","timeseries",), fill_value=xmiss)
    
    temperatureVar_qc_spk = dataset.createVariable("temperature_qc_spk", "u4", ("obs","timeseries",), fill_value=xmiss)
    salinityVar_qc_spk = dataset.createVariable("salinity_qc_spk", "u4", ("obs","timeseries",), fill_value=xmiss)
    oxygenSatVar_qc_spk = dataset.createVariable("oxygen_saturation_qc_spk", "u4", ("obs","timeseries",), fill_value=xmiss)
    oxygenVar_qc_spk = dataset.createVariable("oxygen_qc_spk", "u4", ("obs","timeseries",), fill_value=xmiss)
    chlorophyllVar_qc_spk = dataset.createVariable("chlorophyll_qc_spk", "u4", ("obs","timeseries",), fill_value=xmiss)
    turbidityVar_qc_spk = dataset.createVariable("turbidity_qc_spk", "u4", ("obs","timeseries",), fill_value=xmiss)
    phVar_qc_spk = dataset.createVariable("ph_qc_spk", "u4", ("obs","timeseries",), fill_value=xmiss)
    
    temperatureVar_qc_rtc = dataset.createVariable("temperature_qc_rtc", "u4", ("obs","timeseries",), fill_value=xmiss)
    salinityVar_qc_rtc = dataset.createVariable("salinity_qc_rtc", "u4", ("obs","timeseries",), fill_value=xmiss)
    oxygenSatVar_qc_rtc = dataset.createVariable("oxygen_saturation_qc_rtc", "u4", ("obs","timeseries",), fill_value=xmiss)
    oxygenVar_qc_rtc = dataset.createVariable("oxygen_qc_rtc", "u4", ("obs","timeseries",), fill_value=xmiss)
    chlorophyllVar_qc_rtc = dataset.createVariable("chlorophyll_qc_rtc", "u4", ("obs","timeseries",), fill_value=xmiss)
    turbidityVar_qc_rtc = dataset.createVariable("turbidity_qc_rtc", "u4", ("obs","timeseries",), fill_value=xmiss)
    phVar_qc_rtc = dataset.createVariable("ph_qc_rtc", "u4", ("obs","timeseries",), fill_value=xmiss)
    
    temperatureVar_qc_flt = dataset.createVariable("temperature_qc_flt", "u4", ("obs","timeseries",), fill_value=xmiss)
    salinityVar_qc_flt = dataset.createVariable("salinity_qc_flt", "u4", ("obs","timeseries",), fill_value=xmiss)
    oxygenSatVar_qc_flt = dataset.createVariable("oxygen_saturation_qc_flt", "u4", ("obs","timeseries",), fill_value=xmiss)
    oxygenVar_qc_flt = dataset.createVariable("oxygen_qc_flt", "u4", ("obs","timeseries",), fill_value=xmiss)
    chlorophyllVar_qc_flt = dataset.createVariable("chlorophyll_qc_flt", "u4", ("obs","timeseries",), fill_value=xmiss)
    turbidityVar_qc_flt = dataset.createVariable("turbidity_qc_flt", "u4", ("obs","timeseries",), fill_value=xmiss)
    phVar_qc_flt = dataset.createVariable("ph_qc_flt", "u4", ("obs","timeseries",), fill_value=xmiss)

    temperatureVar_qc_mvr = dataset.createVariable("temperature_qc_mvr", "u4", ("obs","timeseries",), fill_value=xmiss)
    salinityVar_qc_mvr = dataset.createVariable("salinity_qc_mvr", "u4", ("obs","timeseries",), fill_value=xmiss)
    oxygenSatVar_qc_mvr = dataset.createVariable("oxygen_saturation_qc_mvr", "u4", ("obs","timeseries",), fill_value=xmiss)
    oxygenVar_qc_mvr = dataset.createVariable("oxygen_qc_mvr", "u4", ("obs","timeseries",), fill_value=xmiss)
    chlorophyllVar_qc_mvr = dataset.createVariable("chlorophyll_qc_mvr", "u4", ("obs","timeseries",), fill_value=xmiss)
    turbidityVar_qc_mvr = dataset.createVariable("turbidity_qc_mvr", "u4", ("obs","timeseries",), fill_value=xmiss)
    phVar_qc_mvr = dataset.createVariable("ph_qc_mvr", "u4", ("obs","timeseries",), fill_value=xmiss)
    
    temperatureVar_qc_atn = dataset.createVariable("temperature_qc_atn", "u4", ("obs","timeseries",), fill_value=xmiss)
    salinityVar_qc_atn = dataset.createVariable("salinity_qc_atn", "u4", ("obs","timeseries",), fill_value=xmiss)
    oxygenSatVar_qc_atn = dataset.createVariable("oxygen_saturation_qc_atn", "u4", ("obs","timeseries",), fill_value=xmiss)
    oxygenVar_qc_atn = dataset.createVariable("oxygen_qc_atn", "u4", ("obs","timeseries",), fill_value=xmiss)
    chlorophyllVar_qc_atn = dataset.createVariable("chlorophyll_qc_atn", "u4", ("obs","timeseries",), fill_value=xmiss)
    turbidityVar_qc_atn = dataset.createVariable("turbidity_qc_atn", "u4", ("obs","timeseries",), fill_value=xmiss)
    phVar_qc_atn = dataset.createVariable("ph_qc_atn", "u4", ("obs","timeseries",), fill_value=xmiss)

    temperatureVar_qc_nbr = dataset.createVariable("temperature_qc_nbr", "u4", ("obs","timeseries",), fill_value=xmiss)
    salinityVar_qc_nbr = dataset.createVariable("salinity_qc_nbr", "u4", ("obs","timeseries",), fill_value=xmiss)
    oxygenSatVar_qc_nbr = dataset.createVariable("oxygen_saturation_qc_nbr", "u4", ("obs","timeseries",), fill_value=xmiss)
    oxygenVar_qc_nbr = dataset.createVariable("oxygen_qc_nbr", "u4", ("obs","timeseries",), fill_value=xmiss)
    chlorophyllVar_qc_nbr = dataset.createVariable("chlorophyll_qc_nbr", "u4", ("obs","timeseries",), fill_value=xmiss)
    turbidityVar_qc_nbr = dataset.createVariable("turbidity_qc_nbr", "u4", ("obs","timeseries",), fill_value=xmiss)
    phVar_qc_nbr = dataset.createVariable("ph_qc_nbr", "u4", ("obs","timeseries",), fill_value=xmiss)

    temperatureVar_qc_crv = dataset.createVariable("temperature_qc_crv", "u4", ("obs","timeseries",), fill_value=xmiss)
    salinityVar_qc_crv = dataset.createVariable("salinity_qc_crv", "u4", ("obs","timeseries",), fill_value=xmiss)
    oxygenSatVar_qc_crv = dataset.createVariable("oxygen_saturation_qc_crv", "u4", ("obs","timeseries",), fill_value=xmiss)
    oxygenVar_qc_crv = dataset.createVariable("oxygen_qc_crv", "u4", ("obs","timeseries",), fill_value=xmiss)
    chlorophyllVar_qc_crv = dataset.createVariable("chlorophyll_qc_crv", "u4", ("obs","timeseries",), fill_value=xmiss)
    turbidityVar_qc_crv = dataset.createVariable("turbidity_qc_crv", "u4", ("obs","timeseries",), fill_value=xmiss)
    phVar_qc_crv = dataset.createVariable("ph_qc_crv", "u4", ("obs","timeseries",), fill_value=xmiss)

    temperatureVar_qc_din = dataset.createVariable("temperature_qc_din", "u4", ("obs","timeseries",), fill_value=xmiss)
    salinityVar_qc_din = dataset.createVariable("salinity_qc_din", "u4", ("obs","timeseries",), fill_value=xmiss)
    oxygenSatVar_qc_din = dataset.createVariable("oxygen_saturation_qc_din", "u4", ("obs","timeseries",), fill_value=xmiss)
    oxygenVar_qc_din = dataset.createVariable("oxygen_qc_din", "u4", ("obs","timeseries",), fill_value=xmiss)
    chlorophyllVar_qc_din = dataset.createVariable("chlorophyll_qc_din", "u4", ("obs","timeseries",), fill_value=xmiss)
    turbidityVar_qc_din = dataset.createVariable("turbidity_qc_din", "u4", ("obs","timeseries",), fill_value=xmiss)
    phVar_qc_din = dataset.createVariable("ph_qc_din", "u4", ("obs","timeseries",), fill_value=xmiss)

    platformVar = dataset.createVariable("platform1", "u4")
    instrumentVar = dataset.createVariable("instrument1", "u4")
    crsVar = dataset.createVariable("crs", "u4")



    for i in range(linecount):
        To = temp[i]
        Ta = temp[i] + 273.15
        So = 35.00
        Vo = doxyp[i]
        Vt = doxyt[i]
        U = doxyp[i]
        Lv = np.log(100000 * (Vt / (3.3 - Vt)))
        V_do = U / 39.457071
        T0 = ( 1 / (TA0 + (TA1 * Lv) + (TA2 * Lv**2) + (TA3 * Lv**3))) - 273.15
        K_sv = C0 + (C1 * T0) + (C2 * T0**2)
        S_corr = 1.0
        P_corr = np.exp(E* (Po / Ta ))
        top_pt = A0 + (A1*T0) + (A2*V_do**2)
        mid_pt = B0 + (B1 * V_do)
        doxy = (S_corr * P_corr * (((top_pt/mid_pt)-1)/K_sv))



        var1 = To
        var2 = cond[i]
        var3 = salt[i]
        var4 = doxy * 1.4276 / 1000.0
        var5 = chl_scale * (flor[i] - chl_darkc) * 1E-6
        var6 = tur_scale * (turb[i] - tur_darkc)
        print(var1, var2, var3, var4, var5, var6)



        


   #print(lons)

if __name__ == "__main__":
    main()






