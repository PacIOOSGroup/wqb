      real xlon, ylat, zlev
      parameter ( xlon = -157.848, ylat = 21.2799, zlev = -1.0 )

      character*4 tz
      data tz / '-10.' /

      character*8 clon, clat
      data clon / '-157.848' /
      data clat / '21.2799 ' /

      character*4 clev
      data clev / '-1.0' /

      character*5 sensor
      data sensor / 'wqbaw' /

      character*5 code
      data code / 'WQBAW' /

      character*27 title
      data title / 'Water Quality Buoy: Ala Wai' /

      character*22 pi_poc
      data pi_poc / 'Prof. Eric H. De Carlo' /

      character*19 pi_email
      data pi_email / 'edecarlo@hawaii.edu' /

      character*14 tech_poc
      data tech_poc / 'Mike Tomlinson' /

      character*17 tech_email
      data tech_email / 'tomlinson86@q.com' /

      character*514 abstract
      data abstract / 'The water quality buoys are part of the Pacific I
     +slands Ocean Observing System (PacIOOS) and are designed to measur
     +e a variety of ocean parameters at fixed points.  WQBAW is located
     + at the mouth of the Ala Wai Canal near Magic Island.  The Ala Wai
     + is a main conduit for runoff from the area in-shore Waikiki.  Con
     +tinuous sampling of this area provides a record of baseline condit
     +ions of the chemical and biological environment for comparison whe
     +n there are pollultion events such as storm runoff or a sewage spi
     +ll.' /

      integer icalib_oxyg(3,9)
      data icalib_oxyg /  08, 08, 2008, 11, 08, 2009, 11, 27, 2009, 
     +                    12, 07, 2010, 04, 17, 2012, 01, 31, 2013,
     +                    10, 24, 2013, 03, 19, 2014, 10, 10, 2014 /

      integer icalib_flnt(3,11)
      data icalib_flnt /  08, 08, 2008, 11, 08, 2009, 11, 27, 2009,
     +                    12, 07, 2010, 01, 06, 2011, 04, 17, 2012,
     +                    09, 16, 2012, 01, 31, 2013, 10, 24, 2013,
     +                    03, 19, 2014, 10, 10, 2014 /

      character*197 calib_oxyg(9)
c Effective date 08/08/08
      data calib_oxyg(1) / 'SBE43-1397, Calib 07-May-08: S_OC=0.4588, V_
     +offset=-0.5121, Tau_20=4.52, A=-1.38745E-3, B=7.2997E-5, C=-1.3729
     +E-6, E_nom=0.036 (effective date 08-08-2008)                   
     +                  ' /
c Effective date 11/08/09
      data calib_oxyg(2) / 'SBE42-1492, Calib 15-Aug-08: S_OC=0.4537, V_
     +offset=-0.4989, Tau_20=4.25, A=-1.38745E-3, B=7.2997E-5, C=-1.3729
     +E-6, E_nom=0.036 (effective date 11-08-2009)                   
     +                  ' /
c Effective date 11/27/09
      data calib_oxyg(3) / 'SBE43-1397, Calib 17-Nov-09: S_OC=0.4047, V_
     +offset=-0.5068, Tau_20=6.31, A=-2.6349E-3, B=1.6228E-4, C=-2.8976E
     +-6, E_nom=0.036 (effective date 11-27-2009)                   
     +                  ' /
c Effective date 12/07/10
      data calib_oxyg(4) / 'SBE42-1397, Calib 30-Nov-10: S_OC=0.4335, V_
     +offset=-0.5139, Tau_20=3.80, A=-1.84890E-3, B=1.3749E-5, C=-2.6862
     +E-6, E_nom=0.036 (effective date 12-07-2010)                   
     +                  ' /
c Effective date 04/17/12
      data calib_oxyg(5) / 'SBE42-1397, Calib 28-Feb-12: S_OC=0.4369, V_
     +offset=-0.5137, Tau_20=4.08, A=-1.66260E-3, B=7.6508E-5, C=-1.0344
     +E-6, E_nom=0.036 (effective date 04-17-2012)                   
     +                  ' /
c Effective date 01/31/13
      data calib_oxyg(6) / 'SBE63-0317, Calib 30-Nov-12: A0=1.0513E0, A1
     +=-1.50E-3, A2=4.0970E-1, B0=-2.-2.3697E-3, B1=1.6307E0, TA0=7.5623
     +24E-4, TA1=2.379507E-4, TA2=1.872606E-6, TA3=6.04944E-8 (effective
     + date 01-31-2013) ' /
c Effective date 10/24/13
      data calib_oxyg(7) / 'SBE43-1397, Calib 28-Feb-12: S_OC=0.4369, V_
     +offset=-0.5137, Tau_20=4.08, A=-1.6626E-3, B=7.6508E-5, C=-1.0344E
     +-6, E_nom=0.036 (effective date 10-24-2013)
     +                  ' /
c Effective date 03/19/14
      data calib_oxyg(8) / 'SBE43-1398, Calib 27-Dec-12: S_OC=0.3968, V_
     +offset=-0.4725, Tau_20=5.48, A=-2.5038E-3, B=1.9606E-4, C=-3.4861E
     +-6, E_nom=0.036 (effective date 03-19-2014)
     +                  ' /
c Effective date 10/10/14
      data calib_oxyg(9) / 'SBE63-0317, Calib 01-Jul014: A0=1.0513E0, A1
     +=-1.50E-3, A2=3.9329E-1, B0=-2.3305E-1, B1=1.610E0, TA0=7.085069E-
     +4, TA1=2.521378E-4, TA2=4.688456E-7, TA3=1.607418E-7 (effective da
     +te 10-10-2014)    ' /

      character*114 calib_flnt(11)
c Effective date 08/08/08
      data calib_flnt(1) / 'FLNTUS-1027, Calib 28-Apr-08: FSF=15, FDC=0.
     +077, TSF=42, TDC=0.061 (effective date 08-08-2008)               '
     + /
c Effective date 11/08/09       
      data calib_flnt(2) / 'FLNTURTD-1032, Calib 28-Apr-08: FSF=10, FDC=
     +0.079, TSF=5, TDC=0.076 (effective date 11-08-2009)              '
     + /
c Effective date 11/27/09
      data calib_flnt(3) / 'FLNTUS-1027, Calib 17-Nov-09: FSF=15, FDC=0.
     +077, TSF=41, TDC=0.061 (effective date 11-27-2009)               '
     + /
c Effective date 12/07/10
      data calib_flnt(4) / 'FLNTUSB-508, Calib 02-Dec-10: FSF=10, FDC=0.
     +083, TSF=5, TDC=0.131 (effective date 12-07-2010)                '
     + /
c Effective date 01/06/2011
      data calib_flnt(5) / 'FLNTUS-1027, Calib 06-Dec-10: FSF=15, FDC=0.
     +078, TSF=40, TDC=0.055 (effective date 01-06-2011)               '
     + /
c Effective date 4/17/12
      data calib_flnt(6) / 'FLNTUS-1027, Calib 13-Feb-12: FSF=16, FDC=0.
     +078, TSF=39, TDC=0.056 (effective date 04-17-2012)               '
     + /
c Effective date 9/16/12
      data calib_flnt(7) / 'FLNTUS-2756, Calib 10-Jul-12: FSF=15, FDC=0.
     +060, TSF=40, TDC=0.076 (effective date 09-16-2012)               '
     + /
c Effective date 01/31/13
      data calib_flnt(8) / 'FLNTUS-1027, Calib 25-Oct-12: FSF=16, FDC=0.
     +078, TSF=39, TDC=0.058 (effective date 01-31-2013)               '
     + /
c Effective date 10/24/13
      data calib_flnt(9) / 'FLNTUS-2756, Calib 10-Jul-12: FSF=15, FDC=0.
     +060, TSF=40, TDC=0.076 (effective date 10-24-2013)               '
     + /
c Effective date 03/19/14
      data calib_flnt(10) / 'FLNTUS-1027, Calib 21-Dec-13: FSF=16, FDC=0
     +.078, TSF=42, TDC=0.057 (effective date 03-19-2014)              '
     + /
c Effective date 10/10/14
      data calib_flnt(11) / 'FLNTUS-2756, Calib 30-Apr-14: FSF=15, FDC=0
     +.058, TSF=43, TDC=0.075 (effective date 10-10-2014)              '
     + /


      character*99 keywords
      data keywords / 'Turbidity, Chlorophyll, Oxygen, Fluorescence, Sca
     +ttering, Water Temperature, Conductivity, Salinity' /
