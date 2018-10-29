      real xlon, ylat, zlev
      parameter ( xlon = -155.082, ylat = 19.7341, zlev = -1.0 )

      character*4 tz
      data tz / '-10.' /

      character*8 clon, clat
      data clon / '-155.082' /
      data clat / '19.7341 ' /

      character*4 clev
      data clev / '-1.0' /

      character*5 sensor
      data sensor / 'wqb04' /

      character*5 code
      data code / 'WQB04' /

      character*68 title
      data title / 'PacIOOS Water Quality Buoy 04 (WQB-04): Hilo Bay, Bi
     +g Island, Hawaii' /

      character*37 web_site
      data web_site / 'http://pacioos.org/water/wqbuoy-hilo/' /

      character*45 inst
      data inst / '6600 V2-4 Multi-Parameter Water Quality Sonde' /

      character*20 pi_poc
      data pi_poc / 'Prof. Steven Colbert' /

      character*19 pi_email
      data pi_email / 'colberts@hawaii.edu' /

      character*18 tech_poc
      data tech_poc / 'Dr. Steven Colbert' /

      character*19 tech_email
      data tech_email / 'colberts@hawaii.edu' /

      character*434 abstract
      data abstract / 'The water quality buoys are part of the Pacific I
     +slands Ocean Observing System (PacIOOS) and are designed to measur
     +e a variety of ocean parameters at fixed points.  WQB04 is located
     + in Hilo Bay on the east side of the Big Island.  Continuous sampl
     +ing of this area provides a record of baseline conditions of the c
     +hemical and biological environment for comparison when there are p
     +ollution events such as storm runoff or a sewage spill.' /

      character*254 ack
      data ack / 'Data provided by PacIOOS (http://pacioos.org) which is
     + part of the U.S. Integrated Ocean Observing System (IOOS) and are
     + funded in part by the National Oceanic and Atmospheric Administra
     +tion (NOAA) award #NA11NOS0120039 and NSF EPSCoR award #EPS-090383
     +3.' /

      character*494 license
      data license / 'The data may be used and redistributed for free bu
     +t is not intended for legal use, since it may contain inaccuracies
     +. Neither the data Contributor, University of Hawaii, PacIOOS, NOA
     +A, State of Hawaii nor the United States Government, nor any of th
     +eir employees or contractors, makes any warranty, express or impli
     +ed, including warranties of merchantability and fitness for a part
     +icular purpose, or assumes any legal liability for the accuracy, c
     +ompleteness, or usefulness, of this information.' /

      integer icalib_oxyg(3,1)
      data icalib_oxyg /  04, 01, 2013 /

      integer icalib_flnt(3,1)
      data icalib_flnt /  04, 01, 2013 /

      character*197 calib_oxyg(1)
c Effective date 04/01/13
      data calib_oxyg(1) / 'SBE43-1397, Calib 01-Apr-13: S_OC=0.4588, V_
     +offset=-0.5121, Tau_20=4.52, A=-1.38745E-3, B=7.2997E-5, C=-1.372
     +9E-6, E_nom=0.036 (effective date 04-01-2013)                   
     +                  ' /

      character*114 calib_flnt(1)
c Effective date 04/01/13
      data calib_flnt(1) / 'FLNTUS-1027, Calib 01-Apr-13: FSF=15, FDC=0.0
     +77, TSF=42, TDC=0.061 (effective date 04-01-2013)               '
     + /

      character*314 keywords
      data keywords / 'Earth Science > Oceans > Ocean Chemistry > Chloro
     +phyll, Earth Science > Oceans > Ocean Chemistry > Oxygen, Earth Sc
     +ience > Oceans > Ocean Optics > Turbidity, Earth Science > Oceans 
     +> Ocean Temperature > Water Temperature, Earth Science > Oceans > 
     +Salinity/Density > Salinity, Earth Science > Oceans > Water Qualit
     +y' /
