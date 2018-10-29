      program make_WQB_cdf

c This program reads in ascii data sets from the Water Quality
c  Buoy (WQB) sensors and produces a netcdf file.  One file is 
c  made per day and then combined using ncrcat.
c The data stream includes water temperature (C),conductivity 
c  (S/m), salinity (psu), dissolved oxygen (mL/L), chlorophyll 
c  (ug/L), and turbidity (NTU) at a 20 minute interval.
c The procedure is to create daily files with the file name given by 
c  the day of year.
c
c input: ascii data streams from WQB
c
c output: single netcdf file
c
c jimp created 22 oct 2009
c jimp modified 28 dec 2009 error in "flor" as 11 chars; made date float
c jimp modified 26 jun 2010 had to fix itime for leap year
c jimp modified 31 aug 2012 to use discrete geometry
c jimp modified 02 may 2013 for new formats (QC stuff)
c jimp modified 10 oct 2014 for new format

c Set parameters:
c    - use 72 as the maximum number of records in a given day (24 
c      hours per day, 3 measurements per hour).

c These files include the specifics for each site, mostly metadata
c and location, plus the netcdf stuff

      include 'netcdf.inc'
      include 'pacioos_specs.inc'
      include 'wqbaw_specs.inc'

      parameter ( km = 72, nvars = 6 )
      real      var(nvars,km), var1(km), var2(km)
      real      var_adj(nvars,km)
      integer   ivar(nvars,km), ivar3(km)
      real      temp, cond, salt, oxyg, flor, turb, xtime(km)
      real      temp_adj, salt_adj, oxyg_adj
      real      flor_adj, turb_adj
      real      Lv, K_sv, mid_pt, top_pt
      integer   day, d1, iyear, ihr, imin, isec, itime(km)
      integer   imon, tim_id, tim_dim, tim_rank, st_dim, qcf_id
      integer   var_id(18), var_rank
      parameter ( tim_rank = 1, var_rank = 4 )
      parameter ( lev_rank = 1, lat_rank = 1, lon_rank = 1 )
      integer   tim_dims(tim_rank), var_dims(var_rank)
      integer   st_dims(1)
      integer   iqc_flag_r(2), iqc_flag(3)
      integer   iqc_flag_rv(2), iqc_flagv(nvars)
      character*3 mon
      character*8 cvar(nvars,3)
      character*25 ctime, time_start, time_end
      character*24 ofile
      character*57 qc_flag_def
      character*101 qc_flag_defv
      double precision doubleval(2)

c Define constants (applied Mar 19, 2014)
      Po = 1.0
      chl_darkc = 0.058
      chl_scale = 15.0
      tur_darkc = 0.075
      tur_scale = 43.0
c      A = -2.5038E-3
c      B =  1.9606E-4
c      C = -3.4861E-6
c      E_nom = 0.036
c      S_OC = 0.3968
c      V_offset = -0.4725
c      Tau20 = 5.48
c      OxSol = 4.774
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
c      Sol_B0 = -6.24523E-3
c      Sol_B1 = -7.37614E-3
c      Sol_B2 = -1.03410E-2
c      Sol_B3 = -8.17083E-3
c      Sol_C0 = -4.88682E-7

c Read runtime for metadata
      read ( *, 19 ) ctime
19    format ( a20 )

c Define flag values

c file QC flag
      iqc_flag_r(1) = 0
      iqc_flag_r(2) = 2
      iqc_flag(1) = 0
      iqc_flag(2) = 1
      iqc_flag(3) = 2
      write ( qc_flag_def, 29 ) 
     +     'no_qc_applied',
     +     'realtime_qc_applied',
     +     'delayed_mode_qc_applied'
29    format ( a13, 1x, a19, 1x, a23 )

c variable QC flag
      iqc_flag_rv(1) = -9
      iqc_flag_rv(2) = 4
      iqc_flagv(1) = -9
      iqc_flagv(2) = 0
      iqc_flagv(3) = 1
      iqc_flagv(4) = 2
      iqc_flagv(5) = 3
      iqc_flagv(6) = 4
      write ( qc_flag_defv, 39 ) 
     +     'missing_value',
     +     'quality_not_evaluated',
     +     'failed/bad',
     +     'questionable/suspect',
     +     'passed/good',
     +     'interpolated/adjusted'
39    format ( a13, 1x, a21, 1x, a10, 1x, a20, 1x, a11, 1x, a21 )

c Define variables

      write ( cvar(1,1), 49 ) "temp_raw"
      write ( cvar(1,2), 59 ) "temp_qd"
      write ( cvar(1,3), 69 ) "temp"
      write ( cvar(2,1), 49 ) "cond_raw"
      write ( cvar(2,2), 59 ) "cond_qd"
      write ( cvar(2,3), 69 ) "cond"
      write ( cvar(3,1), 49 ) "salt_raw"
      write ( cvar(3,2), 59 ) "salt_qd"
      write ( cvar(3,3), 69 ) "salt"
      write ( cvar(4,1), 49 ) "oxyg_raw"
      write ( cvar(4,2), 59 ) "oxyg_qd"
      write ( cvar(4,3), 69 ) "oxyg"
      write ( cvar(5,1), 49 ) "flor_raw"
      write ( cvar(5,2), 59 ) "flor_qd"
      write ( cvar(5,3), 69 ) "flor"
      write ( cvar(6,1), 49 ) "turb_raw"
      write ( cvar(6,2), 59 ) "turb_qd"
      write ( cvar(6,3), 69 ) "turb"
49    format ( a8 )
59    format ( a7 )
69    format ( a4 )

c Define all values as missing

      xmiss = -999.0
      do 10 i = 1, nvars
      do 10 k = 1, km
         var(i,k) = xmiss
         var_adj(i,k) = xmiss
         ivar(i,k) = 0
10    continue

c Open the input ascii file, read variables

      open ( 16, file = './infile', form = 'formatted' )
      do 20 k = 1, km
c         read ( 16, 79, end = 1000 ) temp, cond, flor, turb, doxyp,
c     +                  doxyt, salt, iday, mon, iyear, ihr, imin, isec
c jimp: changed in Oct 2013 since the new sensor package does not have doxyt
c         read ( 16, 78, end = 1000 ) temp, cond, flor, turb, doxyp,
c     +                  salt, iday, mon, iyear, ihr, imin, isec
c jimp: changed in Oct 2014 for new columns
         read ( 16, 79, end = 1000 ) temp, cond, flor, turb, doxyp,
     +                  doxyt, salt, iday, mon, iyear, ihr, imin, isec
         kindex = ihr * 3 + imin / 20 + 1
         if ( k.eq.1.and.kindex.ge.360 ) kindex = 1
         if ( temp.eq.0.0.and.salt.eq.0.0 ) then
            var(1,kindex) = xmiss
            var(2,kindex) = xmiss
            var(3,kindex) = xmiss
            var(4,kindex) = xmiss
            var(5,kindex) = xmiss
            var(6,kindex) = xmiss
         else
            var(1,kindex) = temp
            var(2,kindex) = cond
            var(3,kindex) = salt

            To = temp
            Ta = temp + 273.15
            So = 35.00
            Vo = doxyp
            Vt = doxyt
c            S0 = So
            U = doxyp
            Lv = log ( 100000 * (Vt / ( 3.3 - Vt ) ) )
            V_do = U / 39.457071
c        
            T0 = ( 1 / ( TA0 + ( TA1 * Lv ) + ( TA2 * Lv**2 ) 
     +               + ( TA3 * Lv**3 ) ) ) - 273.15;
            Ta = T0 + 273.15
            K_sv = C0 + ( C1 * T0 ) + ( C2 * T0**2 )
c            Ts = log ( ( 298.15 - T0 ) / ( 273.15 + T0 ) )
c            S_corr = exp ( ( S0 * ( Sol_B0 + ( Sol_B1 * Ts ) 
c     +                   + ( Sol_B2 * Ts**2 ) + ( Sol_B3 * Ts**3 ) ) ) 
c     +                   + ( Sol_C0 * S0**2 ) )
            S_corr = 1.0
            P_corr = exp ( E * ( Po / Ta ) )
            top_pt = A0 + ( A1 * T0 ) + ( A2 * V_do**2 )
            mid_pt = B0 + ( B1 * V_do )
            doxy = ( S_corr * P_corr * ( ( ( top_pt / mid_pt ) - 1 ) 
     +             / K_sv ) )
c            OxSol_arg = A1 + A2 * ( 100.0 / Ta ) + A3 * log ( Ta / 100 )
c     +                + A4 * ( Ta / 100.0 ) + So * ( B1 
c     +                   + B2 * ( Ta / 100.0 ) 
c     +                   + B3 * ( Ta / 100.0 ) * ( Ta / 100.0 ) )
c            OxSol = exp ( OxSol_arg) 
c            doxy = S_OC * ( Vo + V_offset ) 
c     +         * ( 1.0 + A * To + B * To**2 + C * To**3 )
c     +         * OxSol * exp ( E_nom * ( Po / Ta ) )


            var(4,kindex) = doxy * 1.4276 / 1000.0
            var(5,kindex) = chl_scale * ( flor - chl_darkc ) * 1E-6
            var(6,kindex) = tur_scale * ( turb - tur_darkc )
         endif
         if ( mon.eq.'Jan' ) imon = 1
         if ( mon.eq.'Feb' ) imon = 2
         if ( mon.eq.'Mar' ) imon = 3
         if ( mon.eq.'Apr' ) imon = 4
         if ( mon.eq.'May' ) imon = 5
         if ( mon.eq.'Jun' ) imon = 6
         if ( mon.eq.'Jul' ) imon = 7
         if ( mon.eq.'Aug' ) imon = 8
         if ( mon.eq.'Sep' ) imon = 9
         if ( mon.eq.'Oct' ) imon = 10
         if ( mon.eq.'Nov' ) imon = 11
         if ( mon.eq.'Dec' ) imon = 12
         if ( kindex.eq.1 ) write ( time_start, 89 ) iyear, '-', imon, 
     +           '-', iday, 'T', ihr, ':', imin, ':', isec, '-10:00'
         write ( time_end, 89 ) iyear, '-', imon, '-',
     +             iday, 'T', ihr, ':', imin, ':', isec, '-10:00'
20    continue

1000  continue

79    format ( f7.4, 1x, f9.5, 1x, f7.4, 1x, f7.4, 1x, f7.3, 1x, 
     +    f9.6, 1x, f9.4, 2x, i2.2, 1x, a3, 1x, i4, 1x, i2.2, 1x, i2.2, 
     +    1x, i2.2 )
78    format ( f7.4, 1x, f9.5, 1x, f7.4, 1x, f7.4, 1x, f7.3, 1x, 
     +    f9.4, 2x, i2.2, 1x, a3, 1x, i4, 1x, i2.2, 1x, i2.2, 
     +    1x, i2.2 )
89    format ( i4, a1, i2.2, a1, i2.2, a1, i2.2, a1, i2.2, a1, i2.2, a6)

      if ( iyear.eq.2008.or.iyear.eq.2012.or.iyear.eq.2016 ) then
         ileap = 1
      else
         ileap = 0
      endif
      if ( mon.eq.'Jan' ) then
         d1 = 0
         imon = 1
      endif
      if ( mon.eq.'Feb' ) then
         d1 = 31 
         imon = 2
      endif
      if ( mon.eq.'Mar' ) then
         d1 = 59 + ileap
         imon = 3
      endif
      if ( mon.eq.'Apr' ) then
         d1 = 90 + ileap
         imon = 4
      endif
      if ( mon.eq.'May' ) then
         d1 = 120 + ileap
         imon = 5
      endif
      if ( mon.eq.'Jun' ) then
         d1 = 151 + ileap
         imon = 6
      endif
      if ( mon.eq.'Jul' ) then
         d1 = 181 + ileap
         imon = 7
      endif
      if ( mon.eq.'Aug' ) then
         d1 = 212 + ileap 
         imon = 8
      endif
      if ( mon.eq.'Sep' ) then
         d1 = 243 + ileap 
         imon = 9
      endif
      if ( mon.eq.'Oct' ) then
         d1 = 273 + ileap 
         imon = 10
      endif
      if ( mon.eq.'Nov' ) then
         d1 = 304 + ileap 
         imon = 11
      endif
      if ( mon.eq.'Dec' ) then
         d1 = 334 + ileap 
         imon = 12
      endif
      do 30 k = 1, km
         iy = iyear - 2008
         itime(k) = iy * 365 * 24 * 60 + ( iyear - 2005 ) / 4 * 24 * 60
     +            + ( d1 + iday - 1 ) * 24 * 60 + ( k - 1 ) * 20
         xtime(k) = real ( itime(k) )
30    continue
      do 40 j = 1, 11
         ic_mon = icalib_flnt(1,j)
         ic_day = icalib_flnt(2,j)
         ic_yr  = icalib_flnt(3,j)
         if ( ic_mon.eq.1 ) d1 = 0
         if ( ic_mon.eq.2 ) d1 = 31
         if ( ic_mon.eq.3 ) d1 = 59 + ileap
         if ( ic_mon.eq.4 ) d1 = 90 + ileap
         if ( ic_mon.eq.5 ) d1 = 120 + ileap
         if ( ic_mon.eq.6 ) d1 = 151 + ileap
         if ( ic_mon.eq.7 ) d1 = 181 + ileap
         if ( ic_mon.eq.8 ) d1 = 212 + ileap
         if ( ic_mon.eq.9 ) d1 = 243 + ileap
         if ( ic_mon.eq.10 ) d1 = 273 + ileap
         if ( ic_mon.eq.11 ) d1 = 304 + ileap
         if ( ic_mon.eq.12 ) d1 = 334 + ileap
         ical_time = ( ic_yr - 2008 ) * 365 * 24 * 60 
     +   + ( ic_yr - 2005 ) / 4 * 24 * 60 
     +   + ( ic_mon + ic_day - 1 ) * 24 * 60
         if ( itime(1).ge.ical_time ) j1cal = j
40    continue
      do 50 j = 1, 9
         ic_mon = icalib_oxyg(1,j)
         ic_day = icalib_oxyg(2,j)
         ic_yr  = icalib_oxyg(3,j)
         if ( ic_mon.eq.1 ) d1 = 0
         if ( ic_mon.eq.2 ) d1 = 31
         if ( ic_mon.eq.3 ) d1 = 59 + ileap
         if ( ic_mon.eq.4 ) d1 = 90 + ileap
         if ( ic_mon.eq.5 ) d1 = 120 + ileap
         if ( ic_mon.eq.6 ) d1 = 151 + ileap
         if ( ic_mon.eq.7 ) d1 = 181 + ileap
         if ( ic_mon.eq.8 ) d1 = 212 + ileap
         if ( ic_mon.eq.9 ) d1 = 243 + ileap
         if ( ic_mon.eq.10 ) d1 = 273 + ileap
         if ( ic_mon.eq.11 ) d1 = 304 + ileap
         if ( ic_mon.eq.12 ) d1 = 334 + ileap
         ical_time = ( ic_yr - 2008 ) * 365 * 24 * 60 
     +   + ( ic_yr - 2005 ) / 4 * 24 * 60 
     +   + ( ic_mon + ic_day - 1 ) * 24 * 60
         if ( itime(1).ge.ical_time ) j2cal = j
50    continue

c Create ouput file

      write ( ofile, 99 ) sensor, '_', iyear, '_', imon, '_', iday,'.nc'
99    format ( a5, a1, i4, a1, i2.2, a1, i2.2, a3 )

c Open and initialize the output netcdf files

      iret = nf_create ( ofile, NF_CLOBBER, ncid )

      iret = nf_put_att_text ( ncid, nf_global, 'project', 7, project )
      iret = nf_put_att_text ( ncid, nf_global, 'Conventions', 6,
     +                         convention )
      iret = nf_put_att_text ( ncid, nf_global, 'featureType', 10,
     +                         feature )
      iret = nf_put_att_text ( ncid, nf_global, 'cdm_data_type', 7,
     +                         cmd_type )
      iret = nf_put_att_text ( ncid, nf_global, 'title', 27, title )
      iret = nf_put_att_text ( ncid, nf_global, 'insitution', 8,
     +                         institution )
      iret = nf_put_att_text ( ncid, nf_global, 'date_created', 20,
     +                         ctime )
      iret = nf_put_att_text ( ncid, nf_global, 'abstract', 514,
     +                         abstract )
      iret = nf_put_att_text ( ncid, nf_global, 'calib_oxyg', 197,
     +                         calib_oxyg(j2cal) )
      iret = nf_put_att_text ( ncid, nf_global, 'calib_flnt', 114,
     +                         calib_flnt(j1cal) )
      iret = nf_put_att_text ( ncid, nf_global, 'keywords', 99,
     +                         keywords )
      iret = nf_put_att_text ( ncid, nf_global, 'references', 18,
     +                         web_site )
      iret = nf_put_att_text ( ncid, nf_global, 'platform_code', 5, 
     +                         code )
      iret = nf_put_att_text ( ncid, nf_global, 'naming_authority',
     +                         7, project )
      iret = nf_put_att_text ( ncid, nf_global, 'geospatial_lat_min',
     +                         8, clat )
      iret = nf_put_att_text ( ncid, nf_global, 'geospatial_lat_max',
     +                         8, clat )
      iret = nf_put_att_text ( ncid, nf_global, 'geospatial_lon_min',
     +                         8, clon )
      iret = nf_put_att_text ( ncid, nf_global, 'geospatial_lon_max',
     +                         8, clon )
      iret = nf_put_att_text ( ncid, nf_global, 
     +                         'geospatial_vertical_min', 4, clev )
      iret = nf_put_att_text ( ncid, nf_global, 
     +                         'geospatial_vertical_max', 4, clev )
      iret = nf_put_att_text ( ncid, nf_global, 'time_coverage_start', 
     +                         25, time_start )
      iret = nf_put_att_text ( ncid, nf_global, 'time_coverage_end', 
     +                         25, time_end )
      iret = nf_put_att_text ( ncid, nf_global, 'local_time_zone', 
     +                         4, tz )
      iret = nf_put_att_text ( ncid, nf_global, 'data_center', 
     +                         7, project )
      iret = nf_put_att_text ( ncid, nf_global, 'data_center_email', 
     +                         15, data_email )
      iret = nf_put_att_text ( ncid, nf_global, 'author_email', 
     +                         15, data_email )
      iret = nf_put_att_text ( ncid, nf_global, 'author', 
     +                         17, data_poc )
      iret = nf_put_att_text ( ncid, nf_global, 
     +                        'principal_investigator', 22, pi_poc )
      iret = nf_put_att_text ( ncid, nf_global,
     +     'principal_investigator_email', 19, pi_email )
      iret = nf_put_att_text ( ncid, nf_global, 
     +     'technical_contact', 14, tech_poc )
      iret = nf_put_att_text ( ncid, nf_global,
     +     'technical_contact_email', 17, tech_email )
      iret = nf_put_att_text ( ncid, nf_global, 'citation', 147,
     +                         citation )
      iret = nf_put_att_text ( ncid, nf_global, 'acknowledgement', 220,
     +                         ack )
      iret = nf_put_att_text ( ncid, nf_global, 
     +                         'distribution_statement', 284, distrib )

c Define dimensions and variables

      iret = nf_def_dim ( ncid, 'time', NF_UNLIMITED, tim_dim )
      tim_dims(1) = tim_dim
      iret = nf_def_dim ( ncid, 'name_strlen', 5, st_dim )
      st_dims(1) = st_dim

      iret = nf_def_var ( ncid, 'station_name', NF_CHAR, 1, st_dims, 
     +                    st_id )
      iret = nf_def_var ( ncid, 'qc_flag', NF_INT, 0, 0, qcf_id )
      iret = nf_def_var ( ncid, 'time', NF_REAL, 1, tim_dims, tim_id )
      iret = nf_def_var ( ncid, 'alt', NF_REAL, 0, 0, lev_id )
      iret = nf_def_var ( ncid, 'lat', NF_REAL, 0, 0, lat_id )
      iret = nf_def_var ( ncid, 'lon', NF_REAL, 0, 0, lon_id )
      call check_err ( iret )

      var_dims(1) = tim_dim
      do 60 i = 1, nvars
         iret = nf_def_var ( ncid, cvar(i,1), NF_REAL, 1,
     +                       var_dims, var_id(i) )
         iret = nf_def_var ( ncid, cvar(i,2), NF_INT, 1,
     +                       var_dims, var_id(i+nvars) )
         iret = nf_def_var ( ncid, cvar(i,3), NF_REAL, 1,
     +                       var_dims, var_id(i+nvars*2) )
         call check_err ( iret )
60    continue

c  Assign attributes

c  station_name:
      iret = nf_put_att_text ( ncid, st_id, 'long_name', 5, sensor )
      iret = nf_put_att_text ( ncid, st_id, 'cf_role', 13,
     +                         'timeseries_id' )

c  qc_flag:
      iret = nf_put_att_text ( ncid, qcf_id, 'long_name', 22,
     +                         'Quality control status' )
      iret = nf_put_att_text ( ncid, qcf_id, 'short_name', 7,
     +                         'qc_flag' )
      iret = nf_put_att_int ( ncid, qcf_id, 'valid_range', nf_int,
     +                         2, iqc_flag_r )
      iret = nf_put_att_int ( ncid, qcf_id, 'flag_values', nf_int,
     +                         3, iqc_flag )
      iret = nf_put_att_text ( ncid, qcf_id, 'flag_meanings', 57,
     +                         qc_flag_def )
      iret = nf_put_att_text ( ncid, qcf_id, 'units', 1, '0' )

c  time: 
      iret = nf_put_att_text ( ncid, tim_id, 'long_name', 4,
     +                         'Time' )
      call check_err ( iret )
      iret = nf_put_att_text ( ncid, tim_id, 'standard_name', 4,
     +                         'time' )
      call check_err ( iret )
      iret = nf_put_att_text ( ncid, tim_id, 'short_name', 4,
     +                         'time' )
      call check_err ( iret )
      iret = nf_put_att_text ( ncid, tim_id, 'axis', 1, 'T' )
      call check_err ( iret )
      iret = nf_put_att_text ( ncid, tim_id, 'units', 40,
     +                     'minutes since 2008-01-01 00:00:00 -10:00' )
      call check_err ( iret )

c depth:
      iret = nf_put_att_text ( ncid, lev_id, 'long_name', 26,
     +                         'depth below mean sea level' )
      call check_err ( iret )
      iret = nf_put_att_text ( ncid, lev_id, 'standard_name', 5,
     +                         'depth' )
      call check_err ( iret )
      iret = nf_put_att_text ( ncid, lev_id, 'short_name', 5,
     +                         'depth' )
      call check_err ( iret )
      iret = nf_put_att_text ( ncid, lev_id, 'axis', 1, 'z' )
      call check_err ( iret )
      iret = nf_put_att_text ( ncid, lev_id, 'units', 6,
     +                         'meters' )
      call check_err ( iret )

c latitude
      iret = nf_put_att_text ( ncid, lat_id, 'long_name', 8,
     +                         'Latitude' )
      call check_err ( iret )
      iret = nf_put_att_text ( ncid, lat_id, 'standard_name', 8,
     +                         'latitude' )
      call check_err ( iret )
      iret = nf_put_att_text ( ncid, lat_id, 'short_name', 3,
     +                         'lat' )
      call check_err ( iret )
      iret = nf_put_att_text ( ncid, lat_id, 'axis', 1, 'Y' )
      call check_err ( iret )
      iret = nf_put_att_text ( ncid, lat_id, 'units', 13,
     +                         'degrees_north' )
      call check_err ( iret )

c longitude
      iret = nf_put_att_text ( ncid, lon_id, 'long_name', 9,
     +                         'Longitude' )
      call check_err ( iret )
      iret = nf_put_att_text ( ncid, lon_id, 'standard_name', 9,
     +                         'longitude' )
      call check_err ( iret )
      iret = nf_put_att_text ( ncid, lon_id, 'short_name', 3,
     +                         'lon' )
      call check_err ( iret )
      iret = nf_put_att_text ( ncid, lon_id, 'axis', 1, 'X' )
      call check_err ( iret )
      iret = nf_put_att_text ( ncid, lon_id, 'units', 12,
     +                         'degrees_east' )
      call check_err ( iret )

c temperature
      iret = nf_put_att_text ( ncid, var_id(1), 'long_name', 17,
     +                         'Temperature (raw)' )
      iret = nf_put_att_text ( ncid, var_id(1), 'standard_name', 21,
     +                         'sea_water_temperature' )
      iret = nf_put_att_text ( ncid, var_id(1), 'short_name', 8,
     +                         'temp_raw' )
      iret = nf_put_att_text ( ncid, var_id(1), 'units', 7,
     +                         'Celsius')
      iret = nf_put_att_text ( ncid, var_id(1), 'coordinates', 16,
     +                         'time lat lon alt')
      doubleval(1) = 10
      doubleval(2) = 35
      iret = nf_put_att_double ( ncid, var_id(1), 'valid_range',
     +                           NF_DOUBLE, 2, doubleval )
      iret = nf_put_att_real ( ncid, var_id(1), '_FillValue',
     +                         NF_REAL, 1, xmiss )
      iret = nf_put_att_text ( ncid, var_id(1), 'observation_type', 8,
     +                         'measured')

c temperature qc flag
      iret = nf_put_att_text ( ncid, var_id(7), 'long_name', 30,
     +                         'Temperature quality descriptor' )
      iret = nf_put_att_text ( ncid, var_id(7), 'short_name', 7,
     +                         'temp_qd' )
      iret = nf_put_att_int ( ncid, var_id(7), 'valid_range', nf_int,
     +                         2, iqc_flag_rv )
      iret = nf_put_att_int ( ncid, var_id(7), 'flag_values', nf_int,
     +                         6, iqc_flagv )
      iret = nf_put_att_text ( ncid, var_id(7), 'flag_meanings', 101,
     +                         qc_flag_defv )
      iret = nf_put_att_text ( ncid, var_id(7), 'units', 1, '0' )

c temperature adjusted following qc
      iret = nf_put_att_text ( ncid, var_id(13), 'long_name', 23,
     +                         'Temperature (processed)' )
      iret = nf_put_att_text ( ncid, var_id(13), 'standard_name', 21,
     +                         'sea_water_temperature' )
      iret = nf_put_att_text ( ncid, var_id(13), 'short_name', 4,
     +                         'temp' )
      iret = nf_put_att_text ( ncid, var_id(13), 'units', 7,
     +                         'Celsius')
      iret = nf_put_att_text ( ncid, var_id(13), 'coordinates', 16,
     +                         'time lat lon alt')
      doubleval(1) = 10
      doubleval(2) = 35
      iret = nf_put_att_double ( ncid, var_id(13), 'valid_range',
     +                           NF_DOUBLE, 2, doubleval )
      iret = nf_put_att_real ( ncid, var_id(13), '_FillValue',
     +                         NF_REAL, 1, xmiss )
      iret = nf_put_att_text ( ncid, var_id(13), 'observation_type', 8,
     +                         'measured')

c conductivity
      iret = nf_put_att_text ( ncid, var_id(2), 'long_name', 18,
     +                         'Conductivity (raw)' )
      iret = nf_put_att_text ( ncid, var_id(2), 'standard_name', 33,
     +                         'sea_water_electrical_conductivity' )
      iret = nf_put_att_text ( ncid, var_id(2), 'short_name', 8,
     +                         'cond_raw' )
      iret = nf_put_att_text ( ncid, var_id(2), 'units', 5,
     +                         'S m-1')
      iret = nf_put_att_text ( ncid, var_id(2), 'coordinates', 16,
     +                         'time lat lon alt')
      doubleval(1) = 0
      doubleval(2) = 10
      iret = nf_put_att_double ( ncid, var_id(2), 'valid_range',
     +                           NF_DOUBLE, 2, doubleval )
      iret = nf_put_att_real ( ncid, var_id(2), '_FillValue',
     +                         NF_REAL, 1, xmiss )
      iret = nf_put_att_text ( ncid, var_id(2), 'observation_type', 8,
     +                         'measured')

c conductivity qc flag
      iret = nf_put_att_text ( ncid, var_id(8), 'long_name', 31,
     +                         'Conductivity quality descriptor' )
      iret = nf_put_att_text ( ncid, var_id(8), 'short_name', 7,
     +                         'cond_qd' )
      iret = nf_put_att_int ( ncid, var_id(8), 'valid_range', nf_int,
     +                         2, iqc_flag_rv )
      iret = nf_put_att_int ( ncid, var_id(8), 'flag_values', nf_int,
     +                         6, iqc_flagv )
      iret = nf_put_att_text ( ncid, var_id(8), 'flag_meanings', 101,
     +                         qc_flag_defv )
      iret = nf_put_att_text ( ncid, var_id(8), 'units', 1, '0' )

c conductivity adjusted
      iret = nf_put_att_text ( ncid, var_id(14), 'long_name', 24,
     +                         'Conductivity (processed)' )
      iret = nf_put_att_text ( ncid, var_id(14), 'standard_name', 33,
     +                         'sea_water_electrical_conductivity' )
      iret = nf_put_att_text ( ncid, var_id(14), 'short_name', 4,
     +                         'cond' )
      iret = nf_put_att_text ( ncid, var_id(14), 'units', 5,
     +                         'S m-1')
      iret = nf_put_att_text ( ncid, var_id(14), 'coordinates', 16,
     +                         'time lat lon alt')
      doubleval(1) = 0
      doubleval(2) = 10
      iret = nf_put_att_double ( ncid, var_id(14), 'valid_range',
     +                           NF_DOUBLE, 2, doubleval )
      iret = nf_put_att_real ( ncid, var_id(14), '_FillValue',
     +                         NF_REAL, 1, xmiss )
      iret = nf_put_att_text ( ncid, var_id(14), 'observation_type', 8,
     +                         'measured')

c salinity
      iret = nf_put_att_text ( ncid, var_id(3), 'long_name', 14,
     +                         'Salinity (raw)' )
      iret = nf_put_att_text ( ncid, var_id(3), 'standard_name', 18,
     +                         'sea_water_salinity' )
      iret = nf_put_att_text ( ncid, var_id(3), 'short_name', 8,
     +                         'salt_raw' )
      iret = nf_put_att_text ( ncid, var_id(3), 'units', 4,
     +                         '1e-3')
      iret = nf_put_att_text ( ncid, var_id(3), 'coordinates', 16,
     +                         'time lat lon alt')
      doubleval(1) = 10
      doubleval(2) = 40 
      iret = nf_put_att_double ( ncid, var_id(3), 'valid_range',
     +                           NF_DOUBLE, 2, doubleval )
      iret = nf_put_att_real ( ncid, var_id(3), '_FillValue',
     +                         NF_REAL, 1, xmiss )
      iret = nf_put_att_text ( ncid, var_id(3), 'observation_type', 10,
     +                         'calculated')
      iret = nf_put_att_text ( ncid, var_id(3), 'comment', 51,
     +         'salinity is calculated from measured temp and cond' )

c salinity qc flag
      iret = nf_put_att_text ( ncid, var_id(9), 'long_name', 27,
     +                         'Salinity quality descriptor' )
      iret = nf_put_att_text ( ncid, var_id(9), 'short_name', 7,
     +                         'salt_qd' )
      iret = nf_put_att_int ( ncid, var_id(9), 'valid_range', nf_int,
     +                         2, iqc_flag_rv )
      iret = nf_put_att_int ( ncid, var_id(9), 'flag_values', nf_int,
     +                         6, iqc_flagv )
      iret = nf_put_att_text ( ncid, var_id(9), 'flag_meanings', 101,
     +                         qc_flag_defv )
      iret = nf_put_att_text ( ncid, var_id(9), 'units', 1, '0' )

c salinity adjusted
      iret = nf_put_att_text ( ncid, var_id(15), 'long_name', 20,
     +                         'Salinity (processed)' )
      iret = nf_put_att_text ( ncid, var_id(15), 'standard_name', 18,
     +                         'sea_water_salinity' )
      iret = nf_put_att_text ( ncid, var_id(15), 'short_name', 4,
     +                         'salt' )
      iret = nf_put_att_text ( ncid, var_id(15), 'units', 4,
     +                         '1e-3')
      iret = nf_put_att_text ( ncid, var_id(15), 'coordinates', 16,
     +                         'time lat lon alt')
      doubleval(1) = 10
      doubleval(2) = 40 
      iret = nf_put_att_double ( ncid, var_id(15), 'valid_range',
     +                           NF_DOUBLE, 2, doubleval )
      iret = nf_put_att_real ( ncid, var_id(15), '_FillValue',
     +                         NF_REAL, 1, xmiss )
      iret = nf_put_att_text ( ncid, var_id(15), 'observation_type', 10,
     +                         'calculated')
      iret = nf_put_att_text ( ncid, var_id(15), 'comment', 51,
     +         'salinity is calculated from measured temp and cond' )

c oxygen
      iret = nf_put_att_text ( ncid, var_id(4), 'long_name', 22,
     +                         'Dissolved oxygen (raw)' )
      iret = nf_put_att_text ( ncid, var_id(4), 'standard_name', 41,
     +                  'mass_concentration_of_oxygen_in_sea_water' )
      iret = nf_put_att_text ( ncid, var_id(4), 'short_name', 8,
     +                         'doxy_raw' )
      iret = nf_put_att_text ( ncid, var_id(4), 'units', 6,
     +                         'kg m-3')
      iret = nf_put_att_text ( ncid, var_id(4), 'coordinates', 16,
     +                         'time lat lon alt')
      doubleval(1) = 0
      doubleval(2) = 10
      iret = nf_put_att_double ( ncid, var_id(4), 'valid_range',
     +                           NF_DOUBLE, 2, doubleval )
      iret = nf_put_att_real ( ncid, var_id(4), '_FillValue',
     +                         NF_REAL, 1, xmiss )
      iret = nf_put_att_text ( ncid, var_id(4), 'observation_type', 10,
     +                         'calculated')
      iret = nf_put_att_text ( ncid, var_id(4), 'comment', 167,
     +         'oxygen is calculated from measured T, S, P, V (DO thermi
     +stor voltage) and U (DO phase delay); see global attrib calib_oxyg
     +; and from mg/L to kg/m3 by oxy*1.4276/1000.0' )

c oxygen qc flag
      iret = nf_put_att_text ( ncid, var_id(10), 'long_name', 25,
     +                         'Oxygen quality descriptor' )
      iret = nf_put_att_text ( ncid, var_id(10), 'short_name', 7,
     +                         'doxy_qd' )
      iret = nf_put_att_int ( ncid, var_id(10), 'valid_range', nf_int,
     +                         2, iqc_flag_rv )
      iret = nf_put_att_int ( ncid, var_id(10), 'flag_values', nf_int,
     +                         6, iqc_flagv )
      iret = nf_put_att_text ( ncid, var_id(10), 'flag_meanings', 101,
     +                         qc_flag_defv )
      iret = nf_put_att_text ( ncid, var_id(10), 'units', 1, '0' )

c oxygen adjusted
      iret = nf_put_att_text ( ncid, var_id(16), 'long_name', 28,
     +                         'Dissolved oxygen (processed)' )
      iret = nf_put_att_text ( ncid, var_id(16), 'standard_name', 41,
     +                  'mass_concentration_of_oxygen_in_sea_water' )
      iret = nf_put_att_text ( ncid, var_id(16), 'short_name', 4,
     +                         'doxy' )
      iret = nf_put_att_text ( ncid, var_id(16), 'units', 6,
     +                         'kg m-3')
      iret = nf_put_att_text ( ncid, var_id(16), 'coordinates', 16,
     +                         'time lat lon alt')
      doubleval(1) = 0
      doubleval(2) = 10
      iret = nf_put_att_double ( ncid, var_id(16), 'valid_range',
     +                           NF_DOUBLE, 2, doubleval )
      iret = nf_put_att_real ( ncid, var_id(16), '_FillValue',
     +                         NF_REAL, 1, xmiss )
      iret = nf_put_att_text ( ncid, var_id(16), 'observation_type', 10,
     +                         'calculated')
      iret = nf_put_att_text ( ncid, var_id(16), 'comment', 167,
     +         'oxygen is calculated from measured T, S, P, V (DO thermi
     +stor voltage) and U (DO phase delay); see global attrib calib_oxyg
     +; and from mg/L to kg/m3 by oxy*1.4276/1000.0' )

c chlorphyll
      iret = nf_put_att_text ( ncid, var_id(5), 'long_name', 17,
     +                         'Chlorophyll (raw)' )
      iret = nf_put_att_text ( ncid, var_id(5), 'standard_name', 46,
     +              'mass_concentration_of_chlorophyll_in_sea_water' )
      iret = nf_put_att_text ( ncid, var_id(5), 'short_name', 8,
     +                         'flor_raw' )
      iret = nf_put_att_text ( ncid, var_id(5), 'units', 6,
     +                         'kg m-3')
      iret = nf_put_att_text ( ncid, var_id(5), 'coordinates', 16,
     +                         'time lat lon alt')
      doubleval(1) = 0
      doubleval(2) = 10
      iret = nf_put_att_double ( ncid, var_id(5), 'valid_range',
     +                           NF_DOUBLE, 2, doubleval )
      iret = nf_put_att_real ( ncid, var_id(5), '_FillValue',
     +                         NF_REAL, 1, xmiss )
      iret = nf_put_att_text ( ncid, var_id(5), 'observation_type', 10,
     +                         'calculated')
      iret = nf_put_att_text ( ncid, var_id(5), 'comment', 145,
     +  'flor is calculated from voltage using scale factor (FSF) and da
     +rk count (FDC); see global attrib calib_flnt; and from ug/L to kg 
     +m-3 by flor*1E-6' )
      
c chlorophyll qc flag
      iret = nf_put_att_text ( ncid, var_id(11), 'long_name', 30,
     +                         'Chlorophyll quality descriptor' )
      iret = nf_put_att_text ( ncid, var_id(11), 'short_name', 7,
     +                         'flor_qd' )
      iret = nf_put_att_int ( ncid, var_id(11), 'valid_range', nf_int,
     +                         2, iqc_flag_rv )
      iret = nf_put_att_int ( ncid, var_id(11), 'flag_values', nf_int,
     +                         6, iqc_flagv )
      iret = nf_put_att_text ( ncid, var_id(11), 'flag_meanings', 101,
     +                         qc_flag_defv )
      iret = nf_put_att_text ( ncid, var_id(11), 'units', 1, '0' )

c chlorphyll adjusted
      iret = nf_put_att_text ( ncid, var_id(17), 'long_name', 23,
     +                         'Chlorophyll (processed)' )
      iret = nf_put_att_text ( ncid, var_id(17), 'standard_name', 46,
     +              'mass_concentration_of_chlorophyll_in_sea_water' )
      iret = nf_put_att_text ( ncid, var_id(17), 'short_name', 4,
     +                         'flor' )
      iret = nf_put_att_text ( ncid, var_id(17), 'units', 6,
     +                         'kg m-3')
      iret = nf_put_att_text ( ncid, var_id(17), 'coordinates', 16,
     +                         'time lat lon alt')
      doubleval(1) = 0
      doubleval(2) = 10
      iret = nf_put_att_double ( ncid, var_id(17), 'valid_range',
     +                           NF_DOUBLE, 2, doubleval )
      iret = nf_put_att_real ( ncid, var_id(17), '_FillValue',
     +                         NF_REAL, 1, xmiss )
      iret = nf_put_att_text ( ncid, var_id(17), 'observation_type', 10,
     +                         'calculated')
      iret = nf_put_att_text ( ncid, var_id(17), 'comment', 145,
     +  'flor is calculated from voltage using scale factor (FSF) and da
     +rk count (FDC); see global attrib calib_flnt; and from ug/L to kg 
     +m-3 by flor*1E-6' )
      
c turbidity
      iret = nf_put_att_text ( ncid, var_id(6), 'long_name', 15,
     +                         'Turbidity (raw)' )
      iret = nf_put_att_text ( ncid, var_id(6), 'standard_name', 19,
     +                         'sea_water_turbidity' )
      iret = nf_put_att_text ( ncid, var_id(6), 'short_name', 8,
     +                         'turb_raw' )
      iret = nf_put_att_text ( ncid, var_id(6), 'units', 3,
     +                         'ntu')
      iret = nf_put_att_text ( ncid, var_id(6), 'coordinates', 16,
     +                         'time lat lon alt')
      doubleval(1) = 0
      doubleval(2) = 10
      iret = nf_put_att_double ( ncid, var_id(6), 'valid_range',
     +                           NF_DOUBLE, 2, doubleval )
      iret = nf_put_att_real ( ncid, var_id(6), '_FillValue',
     +                         NF_REAL, 1, xmiss )
      iret = nf_put_att_text ( ncid, var_id(6), 'observation_type', 10,
     +                         'calculated')
      iret = nf_put_att_text ( ncid, var_id(6), 'comment', 107,
     +  'turb is calculated from voltage using scale factor (TSF) and da
     +rk count (TDC); see global attrib calib_flnt' )

c turbidity qc flag
      iret = nf_put_att_text ( ncid, var_id(12), 'long_name', 28,
     +                         'Turbidity quality descriptor' )
      iret = nf_put_att_text ( ncid, var_id(12), 'short_name', 7,
     +                         'turb_qd' )
      iret = nf_put_att_int ( ncid, var_id(12), 'valid_range', nf_int,
     +                         2, iqc_flag_rv )
      iret = nf_put_att_int ( ncid, var_id(12), 'flag_values', nf_int,
     +                         6, iqc_flagv )
      iret = nf_put_att_text ( ncid, var_id(12), 'flag_meanings', 101,
     +                         qc_flag_defv )
      iret = nf_put_att_text ( ncid, var_id(12), 'units', 1, '0' )
      
c turbidity adjusted
      iret = nf_put_att_text ( ncid, var_id(18), 'long_name', 21,
     +                         'Turbidity (processed)' )
      iret = nf_put_att_text ( ncid, var_id(18), 'standard_name', 19,
     +                         'sea_water_turbidity' )
      iret = nf_put_att_text ( ncid, var_id(18), 'short_name', 4,
     +                         'turb' )
      iret = nf_put_att_text ( ncid, var_id(18), 'units', 3,
     +                         'ntu')
      iret = nf_put_att_text ( ncid, var_id(18), 'coordinates', 16,
     +                         'time lat lon alt')
      doubleval(1) = 0
      doubleval(2) = 10
      iret = nf_put_att_double ( ncid, var_id(18), 'valid_range',
     +                           NF_DOUBLE, 2, doubleval )
      iret = nf_put_att_real ( ncid, var_id(18), '_FillValue',
     +                         NF_REAL, 1, xmiss )
      iret = nf_put_att_text ( ncid, var_id(18), 'observation_type', 10,
     +                         'calculated')
      iret = nf_put_att_text ( ncid, var_id(18), 'comment', 107,
     +  'turb is calculated from voltage using scale factor (TSF) and da
     +rk count (TDC); see global attrib calib_flnt' )

      iret = nf_enddef ( ncid )

c Write data

      iret = nf_put_vara_real ( ncid, tim_id, 1, km, xtime )
      iret = nf_put_var_text ( ncid, st_id, code )
      iret = nf_put_vara_real ( ncid, lev_id, 1, 1, zlev )
      iret = nf_put_vara_real ( ncid, lat_id, 1, 1, ylat )
      iret = nf_put_vara_real ( ncid, lon_id, 1, 1, xlon )
      iret = nf_put_vara_int ( ncid, qcf_id, 1, 1, iqc_flag(1) )
      do 70 i = 1, nvars
         do 80 k = 1, km
            var1(k) = var(i,k)
c            var2(k) = var_adj(i,k)
            var2(k) = var(i,k)
            ivar3(k) = ivar(i,k)
80       continue
         iret = nf_put_vara_real ( ncid, var_id(i), 1, km, var1 )
         iret = nf_put_vara_real ( ncid, var_id(i+nvars*2), 1, km, var2)
         iret = nf_put_vara_int ( ncid, var_id(i+nvars), 1, km, ivar3 )
70    continue

c Close file

      iret = nf_close ( ncid )
      call check_err ( iret )

      stop
      end

c  ----------------------------------------------------------------------------
      subroutine check_err(iret)
c  ----------------------------------------------------------------------------
      integer iret
      include 'netcdf.inc'
      if ( iret .ne. NF_NOERR ) then
         print *, nf_strerror ( iret )
         stop
      endif
      end
