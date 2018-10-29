      program make_WQB_cdf

c This program reads in ascii data sets from the Water Quality
c  Buoy (WQB) sensors and produces a netcdf file.  One file is 
c  made per day and then combined using ncrcat.
c This program is based on the one used for the Oahu WQB's (AW 
c  KN).
c The data stream includes water temperature (C), salinity (ppt),
c  dissolved oxygen (mg/L), dissolved oxygen (%), chlorophyll (RFU), 
c  turbidity (NTU) and pH at a 15 minute interval.
c The procedure is to create daily files with the file name given by 
c  the day of year.
c NOTE: QC is done in a somewhat ad-hoc way.  There are three
c  variables of importance:
c  1. iqc_flag: this states whether the file has undergone real-
c     time, delayed-mode, neither or both QC.  It has to be 
c     manually set (search on
c     iret = nf_put_vara_int ( ncid, qcf_id, 1, 1, iqc_flag(4) )
c     0=none; 1=rt; 2=dm; 3=both
c  2. iqc_rt_flagv, ivar1, ivar4: this is the value of one of
c     the real-time QARTOD tests.  The default is set to 2
c     (not evaluated) then replaced as appropriate when tests
c     are run.  Finally, it is set to 9 if the variable is
c     missing.
c  3. iqc_dm_flagv, ivar2, ivar3: this is the value of the
c     single delayed-mode flag.  The default is set to 0
c     (not evaluated) and later -9 if variable is missing
c     This one is set manually (like item 1; search on
c     ivar2(i,k) = 0 )
c
c input: ascii data streams from WQB
c
c output: single netcdf file
c
c jimp created 30 apr 2013
c jimp changed 18 apr 2016 for new file format (redid all back to 2010)
c jimp changed 07 jul 2016 for Q/C checks and NCEI changes for archive
c jimp changed 03 aug 2016 added several new things; most importantly
c      times now written from data itself, and only valid reports are
c      included (no padding for missing data points).  This means files
c      will have variable lengths (in time).  Second, minor changes
c      were made to NCEI attributes (e.g., scale and offset).
c jimp changed 09 aug 2016 major redo; added QARTOD flags and needed
c      changes for NCEI archiving
c jimp changed 20 dec 2016 for addded metadata changes recommended by
c      NCEI
c jimp changed 09 jan 2017 few more minor changes to attributes
c jimp changed 11 jan 2017 variable names now more intuitive and removes
c      the need to change these in ERDDAP config 
c      (e.g., temp --> temperature)
c jimp changed 13 jan 2017 changed additional metadata things, and added
c      a kludge to get time_coverage_start to reflect correct day (the
c      conversion to UTC was causing this to go wrong)
c jimp changed 23 jan 2017 additional metadata changes; changed dimensions
c      to be, e.g., temperature(timeseries,time) 
c jimp changed 24 jan 2017 changed the dimension "time" to "obs"
c      and the station name to be an integer
c jimp changed 31 jan 2017 more metadata changes from John Maurer's msg
c jimp changed 01 feb 2017 to be netcdf-4, and dimension order (needed
c      netcdf4 to have unlimited dimension second)
c jimp changed 03 feb to fix NCEI template type; orthogonal needs to
c      have dimension "time", while "incomplete" has dimensions "obs"
c      also changed title to include (WQB-04)
c jimp changed 25 sep 2017 for new PI (Adolf --> Colbert)

c Set parameters:
c    - use 96 as the maximum number of records in a given day (24 
c      hours per day, 4 measurements per hour).

c These files include the specifics for each site, mostly metadata
c and location, plus the netcdf stuff

      include 'netcdf.inc'
      include 'pacioos_specs.inc'
      include 'wqb04_specs.inc'

      parameter ( km = 96, nvars = 7, nqc = 13 )
      real      var(nvars,km), var1(km), var2(km)
      real      var_adj(nvars,km), range_val(2)
      integer   ivar1(nvars,nqc,km), ivar2(nvars,km), ivar4(nqc,km)
      integer   ivar3(km), istime(km), istart(2), icount(2)
      integer   iqc_flag_r(2), iqc_flag(4)
      integer   iqc_rt_flag_rv(2), iqc_rt_flagv(5)
      integer   iqc_dm_flag_rv(2), iqc_dm_flagv(6)
      real      temp, salt, oxy1, oxy2, flor, turb, ph01, xtime(km)
      integer   day, d1, iyear, ihr, imin, isec, itime, iday1
      integer   imon, tim_id, tim_dim, tim_rank, qcf_id, tsr_dim
      integer   ts_id, plt_id, ins_id, crs_id, tsr_rank
      integer   var_id(16*nvars), var_rank
      parameter ( tim_rank = 1, tsr_rank = 1, var_rank = 2 )
      parameter ( lev_rank = 1, lat_rank = 1, lon_rank = 1 )
      integer   tim_dims(tim_rank), tsr_dims(tsr_rank)
      integer   var_dims(var_rank)
      character*3 cmon
      character*11 cvar(nvars,16)
      character*20 time_start, time_end
      character*25 ctime
      character*24 ofile
      character*75 seriesID
      character*71 qc_rt_flag_defv
      character*94 qc_flag_def
      character*113 qc_dm_flag_defv

c Read runtime for metadata
      read ( *, 19 ) ctime
19    format ( a25 )

c Define flag values

c file QC flag
      iqc_flag_r(1) = 0
      iqc_flag_r(2) = 3
      iqc_flag(1) = 0
      iqc_flag(2) = 1
      iqc_flag(3) = 2
      iqc_flag(4) = 3
      write ( qc_flag_def, 29 ) 
     +     'no_qc_applied',
     +     'realtime_qc_applied',
     +     'delayed_mode_qc_applied',
     +     'realtime_and_delayed_mode_qc_applied'
29    format ( a13, 1x, a19, 1x, a23, 1x, a36 )

c real-time variable QC flag
      iqc_rt_flag_rv(1) = 1
      iqc_rt_flag_rv(2) = 9
      iqc_rt_flagv(1) = 1
      iqc_rt_flagv(2) = 2
      iqc_rt_flagv(3) = 3
      iqc_rt_flagv(4) = 4
      iqc_rt_flagv(5) = 9
      write ( qc_rt_flag_defv, 39 )
     +     'pass',
     +     'quality_not_evaluated',
     +     'suspect_or_high_interest',
     +     'failed',
     +     'missing_data'
39    format ( a4, 1x, a21, 1x, a24, 1x, a6, 1x, a12 )

c delayed-mode variable QC flag
      iqc_dm_flag_rv(1) = -9
      iqc_dm_flag_rv(2) = 4
      iqc_dm_flagv(1) = -9
      iqc_dm_flagv(2) = 0
      iqc_dm_flagv(3) = 1
      iqc_dm_flagv(4) = 2
      iqc_dm_flagv(5) = 3
      iqc_dm_flagv(6) = 4
      write ( qc_dm_flag_defv, 49 ) 
     +     'missing_value',
     +     'quality_not_evaluated',
     +     'failed_or_bad',
     +     'questionable_or_suspect',
     +     'passed_or_good',
     +     'interpolated_or_adjusted'
49    format ( a13, 1x, a21, 1x, a13, 1x, a23, 1x, a14, 1x, a24 )

c Define variables

      write ( cvar(1,1), 59 ) "temp"
      write ( cvar(2,1), 59 ) "salt"
      write ( cvar(3,1), 59 ) "osat"
      write ( cvar(4,1), 59 ) "odoc"
      write ( cvar(5,1), 59 ) "flor"
      write ( cvar(6,1), 59 ) "turb"
      write ( cvar(7,1), 59 ) "ph01"
59    format ( a4 )

      do 20 i = 1, nvars
         write ( cvar(i, 2), 69 ) cvar(i,1), "_raw"
         write ( cvar(i, 3), 79 ) cvar(i,1), "_dm_qd"
         write ( cvar(i, 4), 89 ) cvar(i,1), "_qc_gap"
         write ( cvar(i, 5), 89 ) cvar(i,1), "_qc_syn"
         write ( cvar(i, 6), 89 ) cvar(i,1), "_qc_loc"
         write ( cvar(i, 7), 89 ) cvar(i,1), "_qc_rng"
         write ( cvar(i, 8), 89 ) cvar(i,1), "_qc_clm"
         write ( cvar(i, 9), 89 ) cvar(i,1), "_qc_spk"
         write ( cvar(i,10), 89 ) cvar(i,1), "_qc_rtc"
         write ( cvar(i,11), 89 ) cvar(i,1), "_qc_flt"
         write ( cvar(i,12), 89 ) cvar(i,1), "_qc_mvr"
         write ( cvar(i,13), 89 ) cvar(i,1), "_qc_atn"
         write ( cvar(i,14), 89 ) cvar(i,1), "_qc_nbr"
         write ( cvar(i,15), 89 ) cvar(i,1), "_qc_crv"
         write ( cvar(i,16), 89 ) cvar(i,1), "_qc_din"
20    continue
69    format ( a4, a4 )
79    format ( a4, a6 )
89    format ( a4, a7 ) 

c Define all values as missing and flags as default
c ivar1 is the real-time qc check (=2 for not evaluated)
c ivar2 is the delayed mode check (=0 for not evaluated)

      xmiss = -999.0
      do 30 i = 1, nvars
      do 30 k = 1, km
         var(i,k) = xmiss
         var_adj(i,k) = xmiss
         do 40 j = 1, nqc
            ivar1(i,j,k) = 2
40       continue
c Pre-2016 DMQC was done (ivar2 = 3)
c         ivar2(i,k) = 3
c Post-2015 no DMQC (ivar2 = 0)
         ivar2(i,k) = 0
30    continue

c Open the input ascii files, read variables

      open ( 16, file = './infile', form = 'formatted' )

      kindex = 0
      itime_old = -999
      do 50 k = 1, km
c         read ( 16, 99, end = 1000 ) iyear, imon, iday, ihr, imin, isec, 
c     +                   temp, salt, oxy1, oxy2, flor, turb, ph01
         read ( 16, 99, end = 1000 ) temp, salt, oxy1, oxy2, flor, turb,
     +                  ph01, iday, cmon, iyear, ihr, imin, isec

c  convert from local (HST) to UTC
c  NOTE: we have to do this with new files, but the ones from 2010-2015 were
c        reprocessed to be UTC already in the ASCII files (arg).
c Pre-2016 files already in UTC
c         ihr = ihr + 0
c Post-2015 files need to be converted
         ihr = ihr + 10
         if ( ihr.ge.24 ) then
            ihr = ihr - 24
            iday = iday + 1
         endif
         if ( ihr.lt.0 ) then
            ihr = ihr + 24
            iday = iday - 1
         endif
         itime_new = ihr * 60 * 60 + imin * 60 + isec
         if ( itime_new.ne.itime_old ) then
            kindex = kindex + 1
            istime(kindex) = ihr * 60 + imin
         endif
         itime_old = itime_new
         if ( k.eq.1 ) then
            iday1 = iday
            ihr1 = ihr
            imin1 = imin
            isec1 = isec
         endif

c QARTOD test 1: make sure time is reasonable
         if ( iday.gt.31.or.iday.lt.0.or.iyear.gt.2100.or.iyear.lt.2000.
     +        or.ihr.gt.60.or.ihr.lt.0.or.imin.gt.60.or.imin.lt.0.
     +        or.isec.gt.60.or.isec.lt.0 ) then
            do 60 i = 1, nvars
               ivar1(i,1,kindex) = 4
60          continue
         else
            do 70 i = 1, nvars
               ivar1(i,1,kindex) = 1
70          continue
         endif

         var(1,kindex) = temp
         var(2,kindex) = salt
         var(3,kindex) = oxy1 / 100.0
         var(4,kindex) = oxy2 / 1000.0
         var(5,kindex) = ( flor * 4.55 - 0.88 ) * 1E-6
         var(6,kindex) = turb
         var(7,kindex) = ph01
         var_adj(1,kindex) = var(1,kindex)
         var_adj(2,kindex) = var(2,kindex)
         var_adj(3,kindex) = var(3,kindex)
         var_adj(4,kindex) = var(4,kindex)
         var_adj(5,kindex) = var(5,kindex)
         var_adj(6,kindex) = var(6,kindex)
         var_adj(7,kindex) = var(7,kindex)

c QARTOD test 2+3: make sure variables are numbers (note this does nothing)
         do 80 i = 1, nvars
            if ( var(i,kindex).ne.var(i,kindex) ) then
               ivar1(i,2,kindex) = 4
               ivar1(i,3,kindex) = 4
            else
               ivar1(i,2,kindex) = 1
               ivar1(i,3,kindex) = 1
            endif
80       continue

         if ( cmon.eq.'Jan' ) imon = 1
         if ( cmon.eq.'Feb' ) imon = 2
         if ( cmon.eq.'Mar' ) imon = 3
         if ( cmon.eq.'Apr' ) imon = 4
         if ( cmon.eq.'May' ) imon = 5
         if ( cmon.eq.'Jun' ) imon = 6
         if ( cmon.eq.'Jul' ) imon = 7
         if ( cmon.eq.'Aug' ) imon = 8
         if ( cmon.eq.'Sep' ) imon = 9
         if ( cmon.eq.'Oct' ) imon = 10
         if ( cmon.eq.'Nov' ) imon = 11
         if ( cmon.eq.'Dec' ) imon = 12
         if ( k.eq.1 ) write ( time_start, 109 ) iyear, '-', imon, '-',
     +             iday, 'T', ihr, ':', imin, ':', isec, 'Z'
         write ( time_end, 109 ) iyear, '-', imon, '-',
     +             iday, 'T', ihr, ':', imin, ':', isec, 'Z'
50    continue
109   format ( i4, a1, i2.2, a1, i2.2, a1, i2.2, a1, i2.2, a1, i2.2, a1)

1000  continue

c This is a kludge for the first day of the month (converting to UTC
c gives a day of, e.g., 32 instead of resetting to one)
      if (iday1.gt.iday)write ( time_start, 109 ) iyear, '-', imon, '-',
     +             iday, 'T', ihr1, ':', imin1, ':', isec1, 'Z'

      km2 = kindex

c Don't know..
c99    format ( i4.4, 1x, i2.2, 1x, i2.2, 1x, i2.2, 1x, i2.2, 1x, i2.2, 
c     +         1x, f8.5, 1x, f8.5, 1x, f9.5, 1x, f8.5, 1x, f8.5, 1x,
c     +         f9.5, 1x, f8.5 )
c Works for new data from Jason, 2010 through 2015
c99    format ( 4x, f5.2, 5x, f5.2, 4x, f5.1, 5x, f5.2, 5x, f12.9, 6x,
c     +         f3.2, 3x, f6.1, 3x, i2.2, 1x, a3, 1x, i4.4, 1x, i2.2,
c     +         1x, i2.2, 1x, i2.2 )
c January 1, 2016 to July 12, 2016
c99    format ( 2x, f7.2, 4x, f7.2, 4x, f6.1, 4x, f7.2, 4x, f14.9, 4x,
c     +         f6.1, 4x, f6.1, 4x, i2.2, 1x, a3, 1x, i4.4, 1x, i2.2,
c     +         1x, i2.2, 1x, i2.2 )
c July 13, 2016 to present
99    format ( 2x, f7.2, 4x, f7.2, 4x, f10.6, 4x, f7.2, 4x, f14.9, 3x,
     +         f7.2, 4x, f6.1, 4x, i2.2, 1x, a3, 1x, i4.4, 1x, i2.2,
     +         1x, i2.2, 1x, i2.2 )

      if ( iyear.eq.2008.or.iyear.eq.2012.or.iyear.eq.2016 ) then
         ileap = 1
      else
         ileap = 0
      endif
      if ( imon.eq.1 ) d1 = 0
      if ( imon.eq.2 ) d1 = 31
      if ( imon.eq.3 ) d1 = 59 + ileap
      if ( imon.eq.4 ) d1 = 90 + ileap
      if ( imon.eq.5 ) d1 = 120 + ileap
      if ( imon.eq.6 ) d1 = 151 + ileap
      if ( imon.eq.7 ) d1 = 181 + ileap
      if ( imon.eq.8 ) d1 = 212 + ileap
      if ( imon.eq.9 ) d1 = 243 + ileap
      if ( imon.eq.10 ) d1 = 273 + ileap
      if ( imon.eq.11 ) d1 = 304 + ileap
      if ( imon.eq.12 ) d1 = 334 + ileap
c check to see if the file has only data in the first ten hours of
c the day; this mean
c      day = iday + d1
      day = iday
      do 90 k = 1, km2
         iy = iyear - 2008
         itime = iy * 365 * 24 * 60 + ( iyear - 2005 ) / 4 * 24 * 60
     +            + ( d1 + day - 1 ) * 24 * 60 + istime(k)
         xtime(k) = real ( itime )
90    continue
c these are not used by YSI instruments on WQB04
c      do 100 j = 1, 1
c         ic_mon = icalib_flnt(1,j)
c         ic_day = icalib_flnt(2,j)
c         ic_yr  = icalib_flnt(3,j)
c         if ( ic_mon.eq.1 ) d1 = 0
c         if ( ic_mon.eq.2 ) d1 = 31
c         if ( ic_mon.eq.3 ) d1 = 59 + ileap
c         if ( ic_mon.eq.4 ) d1 = 90 + ileap
c         if ( ic_mon.eq.5 ) d1 = 120 + ileap
c         if ( ic_mon.eq.6 ) d1 = 151 + ileap
c         if ( ic_mon.eq.7 ) d1 = 181 + ileap
c         if ( ic_mon.eq.8 ) d1 = 212 + ileap
c         if ( ic_mon.eq.9 ) d1 = 243 + ileap
c         if ( ic_mon.eq.10 ) d1 = 273 + ileap
c         if ( ic_mon.eq.11 ) d1 = 304 + ileap
c         if ( ic_mon.eq.12 ) d1 = 334 + ileap
c         ical_time = ( ic_yr - 2008 ) * 365 * 24 * 60 
c     +   + ( ic_yr - 2005 ) / 4 * 24 * 60 
c     +   + ( d1 + ic_day - 1 ) * 24 * 60
c         if ( int(xtime(1)).ge.ical_time ) j1cal = j
c100   continue
c      do 110 j = 1, 1
c         ic_mon = icalib_oxyg(1,j)
c         ic_day = icalib_oxyg(2,j)
c         ic_yr  = icalib_oxyg(3,j)
c         if ( ic_mon.eq.1 ) d1 = 0
c         if ( ic_mon.eq.2 ) d1 = 31
c         if ( ic_mon.eq.3 ) d1 = 59 + ileap
c         if ( ic_mon.eq.4 ) d1 = 90 + ileap
c         if ( ic_mon.eq.5 ) d1 = 120 + ileap
c         if ( ic_mon.eq.6 ) d1 = 151 + ileap
c         if ( ic_mon.eq.7 ) d1 = 181 + ileap
c         if ( ic_mon.eq.8 ) d1 = 212 + ileap
c         if ( ic_mon.eq.9 ) d1 = 243 + ileap
c         if ( ic_mon.eq.10 ) d1 = 273 + ileap
c         if ( ic_mon.eq.11 ) d1 = 304 + ileap
c         if ( ic_mon.eq.12 ) d1 = 334 + ileap
c         ical_time = ( ic_yr - 2008 ) * 365 * 24 * 60 
c     +   + ( ic_yr - 2005 ) / 4 * 24 * 60 
c     +   + ( d1 + ic_day - 1 ) * 24 * 60
c         if ( int(xtime(1)).ge.ical_time ) j2cal = j
c110   continue

c Create output file

      write ( ofile, 119 ) sensor, '_', iyear, '_', imon, '_',iday,'.nc'
119   format ( a5, a1, i4, a1, i2.2, a1, i2.2, a3 )

c Open and initialize the output netcdf files

      iret = nf_create ( ofile, NF_NETCDF4, ncid )
c      iret = nf_create ( ofile, NF_NOCLOBBER, ncid )
c      iret = nf_create ( ofile, OR(NF_NOCLOBBER,NF_NETCDF4), ncid )

      iret = nf_put_att_text ( ncid, nf_global, 'project', 48, project )
      iret = nf_put_att_text ( ncid, nf_global, 'Conventions', 16,
     +                         convention )
      iret = nf_put_att_text ( ncid, nf_global, 'featureType', 10,
     +                         feature )
      iret = nf_put_att_text ( ncid, nf_global, 'cdm_data_type', 7,
     +                         cmd_type )
      iret = nf_put_att_text ( ncid, nf_global, 'title', 68, title )
      iret = nf_put_att_text ( ncid, nf_global, 'institution', 20,
     +                         institution )
      iret = nf_put_att_text ( ncid, nf_global, 'date_created', 25,
     +                         ctime )
      iret = nf_put_att_text ( ncid, nf_global, 'date_modified', 25,
     +                         ctime )
      iret = nf_put_att_text ( ncid, nf_global,'date_metadata_modified',
     +                         25, ctime )
      iret = nf_put_att_text ( ncid, nf_global,'date_issued',25, ctime )
      iret = nf_put_att_text ( ncid, nf_global,'metadata_link', 41,
     +                     'http://pacioos.org/metadata/WQB04agg.html' )
      iret = nf_put_att_text ( ncid, nf_global, 'summary', 434,
     +                         abstract )
c these calibs are done internally
c      iret = nf_put_att_text ( ncid, nf_global, 'calib_oxyg', 197,
c     +                         calib_oxyg(j2cal) )
c      iret = nf_put_att_text ( ncid, nf_global, 'calib_flnt', 114,
c     +                         calib_flnt(j1cal) )
      iret = nf_put_att_text ( ncid, nf_global, 'infoUrl', 18,
     +                         'http://pacioos.org' )
      iret = nf_put_att_text ( ncid, nf_global, 'ISO_Topic_Categories',
     +                         19, 'oceans, environment' )
      iret = nf_put_att_text ( ncid, nf_global, 'keywords', 314,
     +                         keywords )
      iret = nf_put_att_text ( ncid, nf_global, 'keywords_vocabulary',
     +                         21, key_vocab )
      iret = nf_put_att_text (ncid,nf_global,'standard_name_vocabulary',
     +                         26, std_vocab )
      iret = nf_put_att_text ( ncid, nf_global, 'references', 37,
     +                         web_site )
      iret = nf_put_att_text ( ncid, nf_global, 'id', 8, 'WQB04agg' )
      iret = nf_put_att_text ( ncid, nf_global, 'platform_code', 5, 
     +                         code )
      iret = nf_put_att_text ( ncid, nf_global, 'naming_authority',
     +                         11, name_auth )
      iret = nf_put_att_text ( ncid, nf_global, 'source', 39, 
     +                     'in-situ measurement of water properties' )
      iret = nf_put_att_text ( ncid, nf_global, 'processing_level', 
     +                         1, ' ' )
      iret = nf_put_att_text ( ncid, nf_global, 'ncei_template_version',
     +                         47, ncei_temp )
      iret = nf_put_att_real ( ncid, nf_global, 'geospatial_lat_min',
     +                         nf_real, 1, ylat )
      iret = nf_put_att_real ( ncid, nf_global, 'geospatial_lat_max',
     +                         nf_real, 1, ylat )
      iret = nf_put_att_text ( ncid, nf_global, 'geospatial_lat_units',
     +                         13, 'degrees_north' )
      iret = nf_put_att_text ( ncid, nf_global, 
     +          'geospatial_lat_resolution', 1, '0' )
      iret = nf_put_att_real ( ncid, nf_global, 'geospatial_lon_min',
     +                         nf_real, 1, xlon )
      iret = nf_put_att_real ( ncid, nf_global, 'geospatial_lon_max',
     +                         nf_real, 1, xlon )
      iret = nf_put_att_text ( ncid, nf_global, 'geospatial_lon_units',
     +                         12, 'degrees_east' )
      iret = nf_put_att_text ( ncid, nf_global, 
     +          'geospatial_lon_resolution', 1, '0' )
      iret = nf_put_att_real ( ncid,nf_global,'geospatial_vertical_min',
     +                        nf_real, 1, zlev )
      iret = nf_put_att_real ( ncid,nf_global,'geospatial_vertical_max',
     +                        nf_real, 1, zlev )
      iret = nf_put_att_text ( ncid, nf_global, 
     +                         'geospatial_vertical_units', 1, 'm' )
      iret = nf_put_att_text ( ncid, nf_global, 
     +                       'geospatial_vertical_resolution', 1, '0' )
      iret = nf_put_att_text ( ncid, nf_global, 
     +                         'geospatial_vertical_positive', 2, 'up' )
      iret = nf_put_att_text ( ncid, nf_global, 'time_coverage_start', 
     +                         20, time_start )
      iret = nf_put_att_text ( ncid, nf_global, 'time_coverage_end', 
     +                         20, time_end )
      iret = nf_put_att_text ( ncid, nf_global, 'local_time_zone', 
     +                         4, tz )
      iret = nf_put_att_real ( ncid, nf_global, 'Easternmost_Easting',
     +                         nf_real, 1, xlon )
      iret = nf_put_att_real ( ncid, nf_global, 'Westernmost_Easting',
     +                         nf_real, 1, xlon )
      iret = nf_put_att_real ( ncid, nf_global, 'Northernmost_Northing',
     +                         nf_real, 1, ylat )
      iret = nf_put_att_real ( ncid, nf_global, 'Southernmost_Northing',
     +                         nf_real, 1, ylat )
      iret = nf_put_att_text ( ncid, nf_global, 'data_center', 
     +                         48, project )
      iret = nf_put_att_text ( ncid, nf_global, 'data_center_email', 
     +                         15, data_email )
      iret = nf_put_att_text ( ncid, nf_global, 'creator_email', 
     +                         19, pi_email )
      iret = nf_put_att_text ( ncid, nf_global, 'creator_name',
     +                         20, pi_poc )
      iret = nf_put_att_text ( ncid, nf_global, 'creator_type',
     +                         6, 'person' )
      iret = nf_put_att_text ( ncid, nf_global, 'creator_institution',
     +                         7, 'UH Hilo' )
      iret = nf_put_att_text ( ncid, nf_global, 'creator_url', 32,
     +                          'http://www2.hawaii.edu/~colberts' )
      iret = nf_put_att_text ( ncid, nf_global, 
     +                        'principal_investigator', 20, pi_poc )
      iret = nf_put_att_text ( ncid, nf_global,
     +     'principal_investigator_email', 19, pi_email )
      iret = nf_put_att_text ( ncid, nf_global, 
     +     'technical_contact', 18, tech_poc )
      iret = nf_put_att_text ( ncid, nf_global,
     +     'technical_contact_email', 19, tech_email )
      iret = nf_put_att_text ( ncid, nf_global, 'contributor_name',
     +     11, data_poc )
      iret = nf_put_att_text ( ncid, nf_global, 'contributor_role',
     +     11, 'distributor' )
      iret = nf_put_att_text ( ncid, nf_global, 'history', 38,
     +     'see creation_date and/or date_modified' )
      iret = nf_put_att_text ( ncid, nf_global, 'geospatial_bounds',
     +                         29, 'POINT Z(19.734 -155.082 -1.0)' )
      iret = nf_put_att_text ( ncid, nf_global, 'geospatial_bounds_crs',
     =                         9, 'EPSG:4326' )
      iret = nf_put_att_text ( ncid, nf_global, 
     +  'geospatial_bounds_vertical_crs', 9, 'EPSG:5829' )
      iret = nf_put_att_text ( ncid, nf_global,'time_coverage_duration',
     +                         5, 'PT24H' )
      iret = nf_put_att_text (ncid,nf_global,'time_coverage_resolution',
     +                         5, 'PT15M' )
      iret = nf_put_att_text ( ncid, nf_global, 'uuid',
     +                         20, 'org.pacioos.WQB04agg' )
      iret = nf_put_att_text ( ncid, nf_global, 'sea_name',
     +                         19, 'North Pacific Ocean' )
      iret = nf_put_att_text ( ncid, nf_global, 'locations', 229, 
     +      'Continent > North America > United States Of America > Hawa
     +ii, Ocean > Pacific Ocean > Central Pacific Ocean > Hawaiian Islan
     +ds > Big Island, Ocean > Pacific Ocean > Central Pacific Ocean > H
     +awaiian Islands > Hawaii Island > Hilo' )
      iret = nf_put_att_text ( ncid, nf_global, 'locations_vocabulary', 
     +                        22, 'GCMD Location Keywords' )
      iret = nf_put_att_text ( ncid, nf_global, 'publisher_name',
     +                         48, project )
      iret = nf_put_att_text ( ncid, nf_global, 'publisher_email',
     +                         16, 'info@pacioos.org' )
      iret = nf_put_att_text ( ncid, nf_global, 'publisher_url',
     +                         18, 'http://pacioos.org' )
      iret = nf_put_att_text ( ncid, nf_global, 'publisher_institution',
     +                         20, institution )
      iret = nf_put_att_text ( ncid, nf_global, 'publisher_type', 
     +                         5, 'group' )
      iret = nf_put_att_text ( ncid, nf_global, 'program',
     +                         48, project )
      iret = nf_put_att_text ( ncid, nf_global, 'platform',
     +        79, 'In Situ Ocean-based Platforms > Buoys, In Situ Ocean-
     +based Platforms > Moorings' )
      iret = nf_put_att_text ( ncid, nf_global, 'platform_vocabulary',
     +                 22, 'GCMD Platform Keywords' )
      iret = nf_put_att_text ( ncid, nf_global, 'instrument',
     +                522, 'In Situ/Laboratory Instruments > Chemical Me
     +ters/Analyzers > > > Fluorometers, In Situ/Laboratory Instruments 
     +> Chemical Meters/Analyzers > > > Oxygen Meters, In Situ/Laborator
     +y Instruments > Chemical Meters/Analyzers > > > pH Meters, In Situ
     +/Laboratory Instruments > Conductivity Sensors, In Situ/Laboratory
     + Instruments > Photon/Optical Detectors > > > Turbidity Meters, In
     + Situ/Laboratory Instruments > Profilers/Sounders > > > CTD, In Si
     +tu/Laboratory Instruments > Temperature/Humidity Sensors > > > Tem
     +perature Sensors' )
      iret = nf_put_att_text ( ncid, nf_global, 'instrument_vocabulary',
     +                 24, 'GCMD Instrument Keywords' )
      iret = nf_put_att_text ( ncid, nf_global, 'processing_level', 75,
     +   'near real-time (nrt) and possibly delayed mode (dm); check var
     +iable qc_flag' )
      iret = nf_put_att_text ( ncid, nf_global, 'citation', 147,
     +                         citation )
      iret = nf_put_att_text ( ncid, nf_global, 'acknowledgement', 254,
     +                         ack )
      iret = nf_put_att_text ( ncid, nf_global, 
     +                         'distribution_statement', 285, distrib )
      iret = nf_put_att_text ( ncid, nf_global, 'license',494, license )
      iret = nf_put_att_text ( ncid, nf_global, 'product_version', 
     +                         3, '1.0' )
      iret = nf_put_att_text ( ncid, nf_global, 'comment', 4, 'none' )

c Define dimensions and variables

      iret = nf_def_dim ( ncid, 'timeseries', 1, tsr_dim )
      tsr_dims(1) = tsr_dim
      iret = nf_def_dim ( ncid, 'obs', NF_UNLIMITED, tim_dim )
      tim_dims(1) = tim_dim

c      var_dims(1) = tsr_dim
c      var_dims(2) = tim_dim
      var_dims(1) = tim_dim
      var_dims(2) = tsr_dim
      iret = nf_def_var ( ncid, 'station_name', NF_INT, 1, tsr_dims,
     +                    ts_id )
      iret = nf_def_var ( ncid, 'time', NF_REAL, 2, var_dims, tim_id )
      iret = nf_def_var ( ncid, 'depth', NF_REAL, 1, tsr_dims, lev_id )
      iret = nf_def_var ( ncid, 'latitude', NF_REAL, 1,tsr_dims,lat_id )
      iret = nf_def_var ( ncid, 'longitude', NF_REAL, 1,tsr_dims,lon_id)
      iret = nf_def_var ( ncid, 'qc_flag', NF_INT, 1, tsr_dims, qcf_id )

c  define variables
      iret = nf_def_var ( ncid, 'temperature', NF_REAL, 2, 
     +                    var_dims, var_id(1) )
      iret = nf_def_var ( ncid, 'salinity', NF_REAL, 2, 
     +                    var_dims, var_id(2) )
      iret = nf_def_var ( ncid, 'oxygen_saturation', NF_REAL, 2, 
     +                    var_dims, var_id(3) )
      iret = nf_def_var ( ncid, 'oxygen', NF_REAL, 2, 
     +                    var_dims, var_id(4) )
      iret = nf_def_var ( ncid, 'chlorophyll', NF_REAL, 2, 
     +                    var_dims, var_id(5) )
      iret = nf_def_var ( ncid, 'turbidity', NF_REAL, 2, 
     +                    var_dims, var_id(6) )
      iret = nf_def_var ( ncid, 'ph', NF_REAL, 2, 
     +                    var_dims, var_id(7) )

c  define variables_raw
      iret = nf_def_var ( ncid, 'temperature_raw', NF_REAL, 2, 
     +                    var_dims, var_id(8) )
      iret = nf_def_var ( ncid, 'salinity_raw', NF_REAL, 2, 
     +                    var_dims, var_id(9) )
      iret = nf_def_var ( ncid, 'oxygen_saturation_raw', NF_REAL, 2, 
     +                    var_dims, var_id(10) )
      iret = nf_def_var ( ncid, 'oxygen_raw', NF_REAL, 2, 
     +                    var_dims, var_id(11) )
      iret = nf_def_var ( ncid, 'chlorophyll_raw', NF_REAL, 2, 
     +                    var_dims, var_id(12) )
      iret = nf_def_var ( ncid, 'turbidity_raw', NF_REAL, 2, 
     +                    var_dims, var_id(13) )
      iret = nf_def_var ( ncid, 'ph_raw', NF_REAL, 2, 
     +                    var_dims, var_id(14) )

c  define variables_dm_qd
      iret = nf_def_var ( ncid, 'temperature_dm_qd', NF_INT, 2, 
     +                    var_dims, var_id(15) )
      iret = nf_def_var ( ncid, 'salinity_dm_qd', NF_INT, 2, 
     +                    var_dims, var_id(16) )
      iret = nf_def_var ( ncid, 'oxygen_saturation_dm_qd', NF_INT, 2, 
     +                    var_dims, var_id(17) )
      iret = nf_def_var ( ncid, 'oxygen_dm_qd', NF_INT, 2, 
     +                    var_dims, var_id(18) )
      iret = nf_def_var ( ncid, 'chlorophyll_dm_qd', NF_INT, 2, 
     +                    var_dims, var_id(19) )
      iret = nf_def_var ( ncid, 'turbidity_dm_qd', NF_INT, 2, 
     +                    var_dims, var_id(20) )
      iret = nf_def_var ( ncid, 'ph_dm_qd', NF_INT, 2, 
     +                    var_dims, var_id(21) )

c  define variables_qc_gap
      iret = nf_def_var ( ncid, 'temperature_qc_gap', NF_INT, 2, 
     +                    var_dims, var_id(22) )
      iret = nf_def_var ( ncid, 'salinity_qc_gap', NF_INT, 2, 
     +                    var_dims, var_id(23) )
      iret = nf_def_var ( ncid, 'oxygen_saturation_qc_gap', NF_INT, 2, 
     +                    var_dims, var_id(24) )
      iret = nf_def_var ( ncid, 'oxygen_qc_gap', NF_INT, 2, 
     +                    var_dims, var_id(25) )
      iret = nf_def_var ( ncid, 'chlorophyll_qc_gap', NF_INT, 2, 
     +                    var_dims, var_id(26) )
      iret = nf_def_var ( ncid, 'turbidity_qc_gap', NF_INT, 2, 
     +                    var_dims, var_id(27) )
      iret = nf_def_var ( ncid, 'ph_qc_gap', NF_INT, 2, 
     +                    var_dims, var_id(28) )

c  define variables_qc_syn
      iret = nf_def_var ( ncid, 'temperature_qc_syn', NF_INT, 2, 
     +                    var_dims, var_id(29) )
      iret = nf_def_var ( ncid, 'salinity_qc_syn', NF_INT, 2, 
     +                    var_dims, var_id(30) )
      iret = nf_def_var ( ncid, 'oxygen_saturation_qc_syn', NF_INT, 2, 
     +                    var_dims, var_id(31) )
      iret = nf_def_var ( ncid, 'oxygen_qc_syn', NF_INT, 2, 
     +                    var_dims, var_id(32) )
      iret = nf_def_var ( ncid, 'chlorophyll_qc_syn', NF_INT, 2, 
     +                    var_dims, var_id(33) )
      iret = nf_def_var ( ncid, 'turbidity_qc_syn', NF_INT, 2, 
     +                    var_dims, var_id(34) )
      iret = nf_def_var ( ncid, 'ph_qc_syn', NF_INT, 2, 
     +                    var_dims, var_id(35) )

c  define variables_qc_loc
      iret = nf_def_var ( ncid, 'temperature_qc_loc', NF_INT, 2, 
     +                    var_dims, var_id(36) )
      iret = nf_def_var ( ncid, 'salinity_qc_loc', NF_INT, 2, 
     +                    var_dims, var_id(37) )
      iret = nf_def_var ( ncid, 'oxygen_saturation_qc_loc', NF_INT, 2, 
     +                    var_dims, var_id(38) )
      iret = nf_def_var ( ncid, 'oxygen_qc_loc', NF_INT, 2, 
     +                    var_dims, var_id(39) )
      iret = nf_def_var ( ncid, 'chlorophyll_qc_loc', NF_INT, 2, 
     +                    var_dims, var_id(40) )
      iret = nf_def_var ( ncid, 'turbidity_qc_loc', NF_INT, 2, 
     +                    var_dims, var_id(41) )
      iret = nf_def_var ( ncid, 'ph_qc_loc', NF_INT, 2, 
     +                    var_dims, var_id(42) )

c  define variables_qc_rng
      iret = nf_def_var ( ncid, 'temperature_qc_rng', NF_INT, 2, 
     +                    var_dims, var_id(43) )
      iret = nf_def_var ( ncid, 'salinity_qc_rng', NF_INT, 2, 
     +                    var_dims, var_id(44) )
      iret = nf_def_var ( ncid, 'oxygen_saturation_qc_rng', NF_INT, 2, 
     +                    var_dims, var_id(45) )
      iret = nf_def_var ( ncid, 'oxygen_qc_rng', NF_INT, 2, 
     +                    var_dims, var_id(46) )
      iret = nf_def_var ( ncid, 'chlorophyll_qc_rng', NF_INT, 2, 
     +                    var_dims, var_id(47) )
      iret = nf_def_var ( ncid, 'turbidity_qc_rng', NF_INT, 2, 
     +                    var_dims, var_id(48) )
      iret = nf_def_var ( ncid, 'ph_qc_rng', NF_INT, 2, 
     +                    var_dims, var_id(49) )

c  define variables_qc_clm
      iret = nf_def_var ( ncid, 'temperature_qc_clm', NF_INT, 2, 
     +                    var_dims, var_id(50) )
      iret = nf_def_var ( ncid, 'salinity_qc_clm', NF_INT, 2, 
     +                    var_dims, var_id(51) )
      iret = nf_def_var ( ncid, 'oxygen_saturation_qc_clm', NF_INT, 2, 
     +                    var_dims, var_id(52) )
      iret = nf_def_var ( ncid, 'oxygen_qc_clm', NF_INT, 2, 
     +                    var_dims, var_id(53) )
      iret = nf_def_var ( ncid, 'chlorophyll_qc_clm', NF_INT, 2, 
     +                    var_dims, var_id(54) )
      iret = nf_def_var ( ncid, 'turbidity_qc_clm', NF_INT, 2, 
     +                    var_dims, var_id(55) )
      iret = nf_def_var ( ncid, 'ph_qc_clm', NF_INT, 2, 
     +                    var_dims, var_id(56) )

c  define variables_qc_spk
      iret = nf_def_var ( ncid, 'temperature_qc_spk', NF_INT, 2, 
     +                    var_dims, var_id(57) )
      iret = nf_def_var ( ncid, 'salinity_qc_spk', NF_INT, 2, 
     +                    var_dims, var_id(58) )
      iret = nf_def_var ( ncid, 'oxygen_saturation_qc_spk', NF_INT, 2, 
     +                    var_dims, var_id(59) )
      iret = nf_def_var ( ncid, 'oxygen_qc_spk', NF_INT, 2, 
     +                    var_dims, var_id(60) )
      iret = nf_def_var ( ncid, 'chlorophyll_qc_spk', NF_INT, 2, 
     +                    var_dims, var_id(61) )
      iret = nf_def_var ( ncid, 'turbidity_qc_spk', NF_INT, 2, 
     +                    var_dims, var_id(62) )
      iret = nf_def_var ( ncid, 'ph_qc_spk', NF_INT, 2, 
     +                    var_dims, var_id(63) )

c  define variables_qc_rtc
      iret = nf_def_var ( ncid, 'temperature_qc_rtc', NF_INT, 2, 
     +                    var_dims, var_id(64) )
      iret = nf_def_var ( ncid, 'salinity_qc_rtc', NF_INT, 2, 
     +                    var_dims, var_id(65) )
      iret = nf_def_var ( ncid, 'oxygen_saturation_qc_rtc', NF_INT, 2, 
     +                    var_dims, var_id(66) )
      iret = nf_def_var ( ncid, 'oxygen_qc_rtc', NF_INT, 2, 
     +                    var_dims, var_id(67) )
      iret = nf_def_var ( ncid, 'chlorophyll_qc_rtc', NF_INT, 2, 
     +                    var_dims, var_id(68) )
      iret = nf_def_var ( ncid, 'turbidity_qc_rtc', NF_INT, 2, 
     +                    var_dims, var_id(69) )
      iret = nf_def_var ( ncid, 'ph_qc_rtc', NF_INT, 2, 
     +                    var_dims, var_id(70) )

c  define variables_qc_flt
      iret = nf_def_var ( ncid, 'temperature_qc_flt', NF_INT, 2, 
     +                    var_dims, var_id(71) )
      iret = nf_def_var ( ncid, 'salinity_qc_flt', NF_INT, 2, 
     +                    var_dims, var_id(72) )
      iret = nf_def_var ( ncid, 'oxygen_saturation_qc_flt', NF_INT, 2, 
     +                    var_dims, var_id(73) )
      iret = nf_def_var ( ncid, 'oxygen_qc_flt', NF_INT, 2, 
     +                    var_dims, var_id(74) )
      iret = nf_def_var ( ncid, 'chlorophyll_qc_flt', NF_INT, 2, 
     +                    var_dims, var_id(75) )
      iret = nf_def_var ( ncid, 'turbidity_qc_flt', NF_INT, 2, 
     +                    var_dims, var_id(76) )
      iret = nf_def_var ( ncid, 'ph_qc_flt', NF_INT, 2, 
     +                    var_dims, var_id(77) )

c  define variables_qc_mvr
      iret = nf_def_var ( ncid, 'temperature_qc_mvr', NF_INT, 2, 
     +                    var_dims, var_id(78) )
      iret = nf_def_var ( ncid, 'salinity_qc_mvr', NF_INT, 2, 
     +                    var_dims, var_id(79) )
      iret = nf_def_var ( ncid, 'oxygen_saturation_qc_mvr', NF_INT, 2, 
     +                    var_dims, var_id(80) )
      iret = nf_def_var ( ncid, 'oxygen_qc_mvr', NF_INT, 2, 
     +                    var_dims, var_id(81) )
      iret = nf_def_var ( ncid, 'chlorophyll_qc_mvr', NF_INT, 2, 
     +                    var_dims, var_id(82) )
      iret = nf_def_var ( ncid, 'turbidity_qc_mvr', NF_INT, 2, 
     +                    var_dims, var_id(83) )
      iret = nf_def_var ( ncid, 'ph_qc_mvr', NF_INT, 2, 
     +                    var_dims, var_id(84) )

c  define variables_qc_atn
      iret = nf_def_var ( ncid, 'temperature_qc_atn', NF_INT, 2, 
     +                    var_dims, var_id(85) )
      iret = nf_def_var ( ncid, 'salinity_qc_atn', NF_INT, 2, 
     +                    var_dims, var_id(86) )
      iret = nf_def_var ( ncid, 'oxygen_saturation_qc_atn', NF_INT, 2, 
     +                    var_dims, var_id(87) )
      iret = nf_def_var ( ncid, 'oxygen_qc_atn', NF_INT, 2, 
     +                    var_dims, var_id(88) )
      iret = nf_def_var ( ncid, 'chlorophyll_qc_atn', NF_INT, 2, 
     +                    var_dims, var_id(89) )
      iret = nf_def_var ( ncid, 'turbidity_qc_atn', NF_INT, 2, 
     +                    var_dims, var_id(90) )
      iret = nf_def_var ( ncid, 'ph_qc_atn', NF_INT, 2, 
     +                    var_dims, var_id(91) )

c  define variables_qc_nbr
      iret = nf_def_var ( ncid, 'temperature_qc_nbr', NF_INT, 2, 
     +                    var_dims, var_id(92) )
      iret = nf_def_var ( ncid, 'salinity_qc_nbr', NF_INT, 2, 
     +                    var_dims, var_id(93) )
      iret = nf_def_var ( ncid, 'oxygen_saturation_qc_nbr', NF_INT, 2, 
     +                    var_dims, var_id(94) )
      iret = nf_def_var ( ncid, 'oxygen_qc_nbr', NF_INT, 2, 
     +                    var_dims, var_id(95) )
      iret = nf_def_var ( ncid, 'chlorophyll_qc_nbr', NF_INT, 2, 
     +                    var_dims, var_id(96) )
      iret = nf_def_var ( ncid, 'turbidity_qc_nbr', NF_INT, 2, 
     +                    var_dims, var_id(97) )
      iret = nf_def_var ( ncid, 'ph_qc_nbr', NF_INT, 2, 
     +                    var_dims, var_id(98) )

c  define variables_qc_crv
      iret = nf_def_var ( ncid, 'temperature_qc_crv', NF_INT, 2, 
     +                    var_dims, var_id(99) )
      iret = nf_def_var ( ncid, 'salinity_qc_crv', NF_INT, 2, 
     +                    var_dims, var_id(100) )
      iret = nf_def_var ( ncid, 'chlorophyll_qc_crv', NF_INT, 2, 
     +                    var_dims, var_id(103) )
      iret = nf_def_var ( ncid, 'turbidity_qc_crv', NF_INT, 2, 
     +                    var_dims, var_id(104) )
      iret = nf_def_var ( ncid, 'ph_qc_crv', NF_INT, 2, 
     +                    var_dims, var_id(105) )

c  define variables_qc_din
      iret = nf_def_var ( ncid, 'temperature_qc_din', NF_INT, 2, 
     +                    var_dims, var_id(106) )
      iret = nf_def_var ( ncid, 'salinity_qc_din', NF_INT, 2, 
     +                    var_dims, var_id(107) )
      iret = nf_def_var ( ncid, 'chlorophyll_qc_din', NF_INT, 2, 
     +                    var_dims, var_id(110) )
      iret = nf_def_var ( ncid, 'turbidity_qc_din', NF_INT, 2, 
     +                    var_dims, var_id(111) )
      iret = nf_def_var ( ncid, 'ph_qc_din', NF_INT, 2, 
     +                    var_dims, var_id(112) )

      iret = nf_def_var ( ncid, 'platform1', NF_INT, 0, 0, plt_id )
      iret = nf_def_var ( ncid, 'instrument1', NF_INT, 0, 0, ins_id )
      iret = nf_def_var ( ncid, 'crs', NF_INT, 0, 0, crs_id )

c  Assign attributes

c  station_name:
      write ( seriesID, 129 ) code, ': ', title
129   format ( a5, a2, a68 )
      iret = nf_put_att_text ( ncid, ts_id, 'long_name', 75, seriesID )
      iret = nf_put_att_text ( ncid, ts_id, 'short_name', 5, code )
      iret = nf_put_att_text ( ncid, ts_id, 'cf_role', 13,
     +                         'timeseries_id' )
      iret = nf_put_att_text ( ncid, ts_id, 'ioos_category', 10,
     +                         'Identifier' )
      iret = nf_put_att_int ( ncid, ts_id, '_FillValue',
     +                         NF_INT, 1, int ( xmiss ) )

c  qc_flag:
      iret = nf_put_att_text ( ncid, qcf_id, 'long_name', 22,
     +                         'Quality control status' )
      iret = nf_put_att_text ( ncid, qcf_id, 'short_name', 7,
     +                         'qc_flag' )
      iret = nf_put_att_int ( ncid, qcf_id, 'valid_range', nf_int,
     +                         2, iqc_flag_r )
      iret = nf_put_att_int ( ncid, qcf_id, 'flag_values', nf_int,
     +                         4, iqc_flag )
      iret = nf_put_att_text ( ncid, qcf_id, 'flag_meanings', 94,
     +                         qc_flag_def )
      iret = nf_put_att_text ( ncid, qcf_id, 'units', 1, '0' )
      iret = nf_put_att_text ( ncid, qcf_id, 'ioos_category', 7,
     +                         'Quality' )
      iret = nf_put_att_text ( ncid, qcf_id, 'comment',136, 'QC is appli
     +ed to real-time data using automatic QARTOD checks; in addition, o
     +perators may supply QCd data in delayed mode after analysis' )

c  time: 
      iret = nf_put_att_text ( ncid, tim_id, 'long_name', 4,
     +                         'Time' )
      iret = nf_put_att_text ( ncid, tim_id, 'standard_name', 4,
     +                         'time' )
      iret = nf_put_att_text ( ncid, tim_id, 'short_name', 4,
     +                         'time' )
      iret = nf_put_att_text ( ncid, tim_id, 'axis', 1, 'T' )
      iret = nf_put_att_text ( ncid, tim_id, '_CoordinateAxisType', 4, 
     '                         'Time' )
      iret = nf_put_att_text ( ncid, tim_id, 'units', 34,
     +                     'minutes since 2008-01-01T00:00:00Z' )
      iret = nf_put_att_text ( ncid, tim_id, 'calendar', 6,
     +                         'julian' )
      iret = nf_put_att_text ( ncid, tim_id, 'ioos_category', 4,
     +                         'Time' )
      iret = nf_put_att_text ( ncid, lev_id, 'comment', 58, 
     +   'ideally there is one file per day, 4 measurements per hour' )

c depth:
      iret = nf_put_att_text ( ncid, lev_id, 'long_name', 26,
     +                         'Depth below mean sea level' )
      iret = nf_put_att_text ( ncid, lev_id, 'standard_name', 5,
     +                         'depth' )
      iret = nf_put_att_text ( ncid, lev_id, 'short_name', 5,
     +                         'depth' )
      iret = nf_put_att_text ( ncid, lev_id, 'axis', 1, 'Z' )
      iret = nf_put_att_text ( ncid, lev_id, '_CoordinateAxisType', 6, 
     '                         'Height' )
      iret = nf_put_att_text ( ncid, lev_id, '_CoordinateZisPositive',2,
     '                         'up' )
      iret = nf_put_att_text ( ncid, lev_id, 'positive', 2, 'up' )
      iret = nf_put_att_text ( ncid, lev_id, 'units', 6,
     +                         'meters' )
      iret = nf_put_att_text ( ncid, lev_id, 'valid_range', 1, '0' )
      iret = nf_put_att_text ( ncid, lev_id, 'ioos_category', 8,
     +                         'Location' )
      iret = nf_put_att_text ( ncid, lev_id, 'comment', 31, 
     +                         'instrument is in fixed location' )

c latitude
      iret = nf_put_att_text ( ncid, lat_id, 'long_name', 8,
     +                         'Latitude' )
      iret = nf_put_att_text ( ncid, lat_id, 'standard_name', 8,
     +                         'latitude' )
      iret = nf_put_att_text ( ncid, lat_id, 'short_name', 3,
     +                         'lat' )
      iret = nf_put_att_text ( ncid, lat_id, 'axis', 1, 'Y' )
      iret = nf_put_att_text ( ncid, lat_id, '_CoordinateAxisType', 3, 
     '                         'Lat' )
      iret = nf_put_att_text ( ncid, lat_id, 'units', 13,
     +                         'degrees_north' )
      iret = nf_put_att_text ( ncid, lat_id, 'valid_range', 1, '0' )
      iret = nf_put_att_text ( ncid, lat_id, 'ioos_category', 8,
     +                         'Location' )
      iret = nf_put_att_text ( ncid, lat_id, 'comment', 31, 
     +                         'instrument is in fixed location' )

c longitude
      iret = nf_put_att_text ( ncid, lon_id, 'long_name', 9,
     +                         'Longitude' )
      iret = nf_put_att_text ( ncid, lon_id, 'standard_name', 9,
     +                         'longitude' )
      iret = nf_put_att_text ( ncid, lon_id, 'short_name', 3,
     +                         'lon' )
      iret = nf_put_att_text ( ncid, lon_id, 'axis', 1, 'X' )
      iret = nf_put_att_text ( ncid, lon_id, '_CoordinateAxisType', 3, 
     '                         'Lon' )
      iret = nf_put_att_text ( ncid, lon_id, 'units', 12,
     +                         'degrees_east' )
      iret = nf_put_att_text ( ncid, lon_id, 'valid_range', 1, '0' )
      iret = nf_put_att_text ( ncid, lon_id, 'ioos_category', 8,
     +                         'Location' )
      iret = nf_put_att_text ( ncid, lon_id, 'comment', 31, 
     +                         'instrument is in fixed location' )

      ivar = 1
c temperature adjusted following delayed-mode qc
      i = ivar + 0 * nvars
      iret = nf_put_att_text ( ncid, var_id(i), 'long_name', 23,
     +                         'Temperature (processed)' )
      iret = nf_put_att_text ( ncid, var_id(i), 'standard_name', 21,
     +                         'sea_water_temperature' )
      iret = nf_put_att_text ( ncid, var_id(i), 'short_name', 4,
     +                         'temp' )
      iret = nf_put_att_text ( ncid, var_id(i), 'units', 7,
     +                         'Celsius')
      iret = nf_put_att_text ( ncid, var_id(i), 'coordinates', 29,
     +                         'time latitude longitude depth')
      range_val(1) = 10.0
      range_val(2) = 35.0
      iret = nf_put_att_real ( ncid, var_id(i), 'valid_range',
     +                           NF_REAL, 2, range_val )
      iret = nf_put_att_real ( ncid, var_id(i), '_FillValue',
     +                         NF_REAL, 1, xmiss )
      iret = nf_put_att_text ( ncid, var_id(i), 'observation_type', 8,
     +                         'measured')
      iret = nf_put_att_text ( ncid, var_id(i), 'ncei_name', 17,
     +                         'WATER TEMPERATURE' )
      iret = nf_put_att_text ( ncid, var_id(i), 'ioos_category', 11,
     +                         'Temperature' )
      iret = nf_put_att_real ( ncid, var_id(i), 'scale_factor', 
     +                         NF_REAL, 1, 1.0 )
      iret = nf_put_att_real ( ncid, var_id(i), 'add_offset', 
     +                         NF_REAL, 1, 0.0 )
      iret = nf_put_att_text ( ncid, var_id(i), 'grid_mapping',3,'crs' )
      iret = nf_put_att_text ( ncid, var_id(i), 'coverage_content_type',
     +                         19, 'physicalMeasurement' )
      iret = nf_put_att_text ( ncid, var_id(i), 'source', 1, ' ' )
      iret = nf_put_att_text ( ncid, var_id(i), 'references', 1, ' ' )
      iret = nf_put_att_text ( ncid, var_id(i), 'cell_methods', 57,
     +   'time: point longitude: point latitude: point depth: point' )
      iret = nf_put_att_text ( ncid,var_id(i),'ancillary_variables',305,
     + 'instrument1 platform1 temperature_dm_qd temperature_qc_gap tempe
     +rature_qc_syn temperature_qc_loc temperature_qc_rng temperature_qc
     +_clm temperature_qc_spk temperature_qc_rtc temperature_qc_rtc temp
     +erature_qc_flt temperature_qc_mvr temperature_qc_atn temperature_q
     +c_nbr temperature_qc_crv temperature_qc_din' )
      iret = nf_put_att_text ( ncid, var_id(i), 'platform', 9,
     +                         'platform1' )
      iret = nf_put_att_text ( ncid, var_id(i), 'instrument', 11,
     +                 'instrument1' )
      iret = nf_put_att_text ( ncid, var_id(i), 'comment', 1, ' ' )

c raw temperature
      i = ivar + 1 * nvars
      iret = nf_put_att_text ( ncid, var_id(i), 'long_name', 17,
     +                         'Temperature (raw)' )
      iret = nf_put_att_text ( ncid, var_id(i), 'standard_name', 21,
     +                         'sea_water_temperature' )
      iret = nf_put_att_text ( ncid, var_id(i), 'short_name', 8,
     +                         'temp_raw' )
      iret = nf_put_att_text ( ncid, var_id(i), 'units', 7,
     +                         'Celsius')
      iret = nf_put_att_text ( ncid, var_id(i), 'coordinates', 29,
     +                         'time latitude longitude depth')
      range_val(1) = 10.0
      range_val(2) = 35.0
      iret = nf_put_att_real ( ncid, var_id(i), 'valid_range',
     +                           NF_REAL, 2, range_val )
      iret = nf_put_att_real ( ncid, var_id(i), '_FillValue',
     +                         NF_REAL, 1, xmiss )
      iret = nf_put_att_text ( ncid, var_id(i), 'observation_type', 8,
     +                         'measured')
      iret = nf_put_att_text ( ncid, var_id(i), 'ncei_name', 17,
     +                         'WATER TEMPERATURE' )
      iret = nf_put_att_text ( ncid, var_id(i), 'ioos_category', 11,
     +                         'Temperature' )
      iret = nf_put_att_real ( ncid, var_id(i), 'scale_factor', 
     +                         NF_REAL, 1, 1.0 )
      iret = nf_put_att_real ( ncid, var_id(i), 'add_offset', 
     +                         NF_REAL, 1, 0.0 )
      iret = nf_put_att_text ( ncid, var_id(i), 'grid_mapping', 3,'crs')
      iret = nf_put_att_text ( ncid, var_id(i), 'coverage_content_type',
     +                         19, 'physicalMeasurement' )
      iret = nf_put_att_text ( ncid, var_id(i), 'source', 1, ' ' )
      iret = nf_put_att_text ( ncid, var_id(i), 'references', 1, ' ' )
      iret = nf_put_att_text ( ncid, var_id(i), 'cell_methods', 57,
     +   'time: point longitude: point latitude: point depth: point' )
      iret = nf_put_att_text ( ncid,var_id(i),'ancillary_variables',305,
     + 'instrument1 platform1 temperature_dm_qd temperature_qc_gap tempe
     +rature_qc_syn temperature_qc_loc temperature_qc_rng temperature_qc
     +_clm temperature_qc_spk temperature_qc_rtc temperature_qc_rtc temp
     +erature_qc_flt temperature_qc_mvr temperature_qc_atn temperature_q
     +c_nbr temperature_qc_crv temperature_qc_din' )
      iret = nf_put_att_text ( ncid, var_id(i), 'platform', 9,
     +                         'platform1' )
      iret = nf_put_att_text ( ncid, var_id(i), 'instrument', 11,
     +                 'instrument1' )
      iret = nf_put_att_text ( ncid, var_id(i), 'comment', 1, ' ' )

c temperature delayed mode qc flag
      i = ivar + 2 * nvars
      iret = nf_put_att_text ( ncid, var_id(i), 'long_name', 43,
     +                  'Temperature delayed-mode quality descriptor' )
      iret = nf_put_att_text ( ncid, var_id(i), 'short_name', 10,
     +                         'temp_dm_qd' )
      iret = nf_put_att_text ( ncid, var_id(i), 'standard_name', 33,
     +                         'sea_water_temperature status_flag' )
      iret = nf_put_att_int ( ncid, var_id(i), 'valid_range', nf_int,
     +                         2, iqc_dm_flag_rv )
      iret = nf_put_att_int ( ncid, var_id(i), '_FillValue',
     +                         NF_INT, 1, int(xmiss) )
      iret = nf_put_att_int ( ncid, var_id(i), 'flag_values', nf_int,
     +                         6, iqc_dm_flagv )
      iret = nf_put_att_text ( ncid, var_id(i), 'flag_meanings', 113,
     +                         qc_dm_flag_defv )
      iret = nf_put_att_text ( ncid, var_id(i), 'units', 1, '0' )
      iret = nf_put_att_text ( ncid, var_id(i), 'ioos_category', 7,
     +                         'Quality' )

c temperature QARTOD flags
      i = ivar + 3 * nvars
      iret = nf_put_att_text ( ncid, var_id(i), 'long_name', 15,
     +                  'QARTOD Gap Test' )
      iret = nf_put_att_text ( ncid, var_id(i), 'short_name', 11,
     +                         cvar(ivar,4) )
      iret = nf_put_att_int ( ncid, var_id(i), '_FillValue',
     +                         NF_INT, 1, int(xmiss) )
      i = ivar + 4 * nvars
      iret = nf_put_att_text ( ncid, var_id(i), 'long_name', 18,
     +                  'QARTOD Syntax Test' )
      iret = nf_put_att_text ( ncid, var_id(i), 'short_name', 11,
     +                         cvar(ivar,5) )
      iret = nf_put_att_int ( ncid, var_id(i), '_FillValue',
     +                        NF_INT, 1, int ( xmiss ) )
      i = ivar + 5 * nvars
      iret = nf_put_att_text ( ncid, var_id(i), 'long_name', 20,
     +                  'QARTOD Location Test' )
      iret = nf_put_att_text ( ncid, var_id(i), 'short_name', 11,
     +                         cvar(ivar,6) )
      iret = nf_put_att_int ( ncid, var_id(i), '_FillValue',
     +                        NF_INT, 1, int ( xmiss ) )
      i = ivar + 6 * nvars
      iret = nf_put_att_text ( ncid, var_id(i), 'long_name', 23,
     +                  'QARTOD Gross Range Test' )
      iret = nf_put_att_text ( ncid, var_id(i), 'short_name', 11,
     +                         cvar(ivar,7) )
      iret = nf_put_att_int ( ncid, var_id(i), '_FillValue',
     +                        NF_INT, 1, int ( xmiss ) )
      i = ivar + 7 * nvars
      iret = nf_put_att_text ( ncid, var_id(i), 'long_name', 26,
     +                  'QARTOD Climatological Test' )
      iret = nf_put_att_text ( ncid, var_id(i), 'short_name', 11,
     +                         cvar(ivar,8) )
      iret = nf_put_att_int ( ncid, var_id(i), '_FillValue',
     +                        NF_INT, 1, int ( xmiss ) )
      i = ivar + 8 * nvars
      iret = nf_put_att_text ( ncid, var_id(i), 'long_name', 17,
     +                  'QARTOD Spike Test' )
      iret = nf_put_att_text ( ncid, var_id(i), 'short_name', 11,
     +                         cvar(ivar,9) )
      iret = nf_put_att_int ( ncid, var_id(i), '_FillValue',
     +                        NF_INT, 1, int ( xmiss ) )
      i = ivar + 9 * nvars
      iret = nf_put_att_text ( ncid, var_id(i), 'long_name', 26,
     +                  'QARTOD Rate of Change Test' )
      iret = nf_put_att_text ( ncid, var_id(i), 'short_name', 11,
     +                         cvar(ivar,10) )
      iret = nf_put_att_int ( ncid, var_id(i), '_FillValue',
     +                        NF_INT, 1, int ( xmiss ) )
      i = ivar + 10 * nvars
      iret = nf_put_att_text ( ncid, var_id(i), 'long_name', 21,
     +                  'QARTOD Flat Line Test' )
      iret = nf_put_att_text ( ncid, var_id(i), 'short_name', 11,
     +                         cvar(ivar,11) )
      iret = nf_put_att_int ( ncid, var_id(i), '_FillValue',
     +                        NF_INT, 1, int ( xmiss ) )
      i = ivar + 11 * nvars
      iret = nf_put_att_text ( ncid, var_id(i), 'long_name', 25,
     +                  'QARTOD Multi-Variate Test' )
      iret = nf_put_att_text ( ncid, var_id(i), 'short_name', 11,
     +                         cvar(ivar,12) )
      iret = nf_put_att_int ( ncid, var_id(i), '_FillValue',
     +                        NF_INT, 1, int ( xmiss ) )
      i = ivar + 12 * nvars
      iret = nf_put_att_text ( ncid, var_id(i), 'long_name', 29,
     +                  'QARTOD Attenuated Signal Test' )
      iret = nf_put_att_text ( ncid, var_id(i), 'short_name', 11,
     +                         cvar(ivar,13) )
      iret = nf_put_att_int ( ncid, var_id(i), '_FillValue',
     +                        NF_INT, 1, int ( xmiss ) )
      i = ivar + 13 * nvars
      iret = nf_put_att_text ( ncid, var_id(i), 'long_name', 20,
     +                  'QARTOD Neighbor Test' )
      iret = nf_put_att_text ( ncid, var_id(i), 'short_name', 11,
     +                         cvar(ivar,14) )
      iret = nf_put_att_int ( ncid, var_id(i), '_FillValue',
     +                        NF_INT, 1, int ( xmiss ) )
      i = ivar + 14 * nvars
      iret = nf_put_att_text ( ncid, var_id(i), 'long_name', 27,
     +                  'QARTOD T/S Curve/Space Test' )
      iret = nf_put_att_text ( ncid, var_id(i), 'short_name', 11,
     +                         cvar(ivar,15) )
      iret = nf_put_att_int ( ncid, var_id(i), '_FillValue',
     +                        NF_INT, 1, int ( xmiss ) )
      i = ivar + 15 * nvars
      iret = nf_put_att_text ( ncid, var_id(i), 'long_name', 29,
     +                  'QARTOD Density Inversion Test' )
      iret = nf_put_att_text ( ncid, var_id(i), 'short_name', 11,
     +                         cvar(ivar,16) )
      iret = nf_put_att_int ( ncid, var_id(i), '_FillValue',
     +                        NF_INT, 1, int ( xmiss ) )
      do 140 j = 1, nqc
         i = ivar + (j+2) * nvars
         iret = nf_put_att_text ( ncid, var_id(i), 'standard_name', 33,
     +                         'sea_water_temperature status_flag' )
         iret = nf_put_att_int ( ncid, var_id(i), 'valid_range', nf_int,
     +                         2, iqc_rt_flag_rv )
         iret = nf_put_att_int ( ncid, var_id(i), '_FillValue',
     +                           NF_INT, 1, int ( xmiss ) )
         iret = nf_put_att_int ( ncid, var_id(i), 'flag_values', nf_int,
     +                         5, iqc_rt_flagv )
         iret = nf_put_att_text ( ncid, var_id(i), 'flag_meanings', 71,
     +                         qc_rt_flag_defv )
         iret = nf_put_att_text ( ncid, var_id(i), 'units', 1, '0' )
      iret = nf_put_att_text ( ncid, var_id(i), 'ioos_category', 7,
     +                         'Quality' )
140   continue

      ivar = 2
c salinity adjusted following delayed-mode qc
      i = ivar + 0 * nvars
      iret = nf_put_att_text ( ncid, var_id(i), 'long_name', 20,
     +                         'Salinity (processed)' )
      iret = nf_put_att_text ( ncid, var_id(i), 'standard_name', 18,
     +                         'sea_water_salinity' )
      iret = nf_put_att_text ( ncid, var_id(i), 'short_name', 4,
     +                         'salt' )
      iret = nf_put_att_text ( ncid, var_id(i), 'units', 4,
     +                         '1e-3')
      iret = nf_put_att_text ( ncid, var_id(i), 'ioos_category', 8,
     +                         'Salinity' )
      iret = nf_put_att_text ( ncid, var_id(i), 'coordinates', 29,
     +                         'time latitude longitude depth')
      range_val(1) = 10.0
      range_val(2) = 40.0
      iret = nf_put_att_real ( ncid, var_id(i), 'valid_range',
     +                           NF_REAL, 2, range_val )
      iret = nf_put_att_real ( ncid, var_id(i), '_FillValue',
     +                         NF_REAL, 1, xmiss )
      iret = nf_put_att_text ( ncid, var_id(i), 'observation_type', 10,
     +                         'calculated')
      iret = nf_put_att_text ( ncid, var_id(i), 'comment', 51,
     +         'salinity is calculated from measured temp and cond' )
      iret = nf_put_att_text ( ncid, var_id(i), 'ncei_name', 8,
     +                         'SALINITY' )
      iret = nf_put_att_real ( ncid, var_id(i), 'scale_factor', 
     +                         NF_REAL, 1, 1.0 )
      iret = nf_put_att_real ( ncid, var_id(i), 'add_offset', 
     +                         NF_REAL, 1, 0.0 )
      iret = nf_put_att_text ( ncid, var_id(i), 'grid_mapping',3,'crs' )
      iret = nf_put_att_text ( ncid, var_id(i), 'coverage_content_type',
     +                         19, 'physicalMeasurement' )
      iret = nf_put_att_text ( ncid, var_id(i), 'source', 1, ' ' )
      iret = nf_put_att_text ( ncid, var_id(i), 'references', 1, ' ' )
      iret = nf_put_att_text ( ncid, var_id(i), 'cell_methods', 57,
     +   'time: point longitude: point latitude: point depth: point' )
      iret = nf_put_att_text ( ncid,var_id(i),'ancillary_variables',262,
     + 'instrument1 platform1 salinity_dm_qd salinity_qc_gap salinity_qc
     +_syn salinity_qc_loc salinity_qc_rng salinity_qc +_clm salinity_qc
     +_spk salinity_qc_rtc salinity_qc_rtc salinity_qc_flt salinity_qc_m
     +vr salinity_qc_atn salinity_qc_nbr salinity_qc_crv salinity_qc_din
     +' )
      iret = nf_put_att_text ( ncid, var_id(i), 'platform', 9,
     +                         'platform1' )
      iret = nf_put_att_text ( ncid, var_id(i), 'instrument', 11,
     +                 'instrument1' )
      iret = nf_put_att_text ( ncid, var_id(i), 'comment', 1, ' ' )

c raw salinity
      i = ivar + 1 * nvars
      iret = nf_put_att_text ( ncid, var_id(i), 'long_name', 14,
     +                         'Salinity (raw)' )
      iret = nf_put_att_text ( ncid, var_id(i), 'standard_name', 18,
     +                         'sea_water_salinity' )
      iret = nf_put_att_text ( ncid, var_id(i), 'short_name', 8,
     +                         'salt_raw' )
      iret = nf_put_att_text ( ncid, var_id(i), 'units', 4,
     +                         '1e-3')
      iret = nf_put_att_text ( ncid, var_id(i), 'ioos_category', 8,
     +                         'Salinity' )
      iret = nf_put_att_text ( ncid, var_id(i), 'coordinates', 29,
     +                         'time latitude longitude depth')
      range_val(1) = 10.0
      range_val(2) = 40.0
      iret = nf_put_att_real ( ncid, var_id(i), 'valid_range',
     +                           NF_REAL, 2, range_val )
      iret = nf_put_att_real ( ncid, var_id(i), '_FillValue',
     +                         NF_REAL, 1, xmiss )
      iret = nf_put_att_text ( ncid, var_id(i), 'observation_type', 10,
     +                         'calculated')
      iret = nf_put_att_text ( ncid, var_id(i), 'comment', 51,
     +         'salinity is calculated from measured temp and cond' )
      iret = nf_put_att_text ( ncid, var_id(i), 'ncei_name', 8,
     +                         'SALINITY' )
      iret = nf_put_att_real ( ncid, var_id(i), 'scale_factor', 
     +                         NF_REAL, 1, 1.0 )
      iret = nf_put_att_real ( ncid, var_id(i), 'add_offset', 
     +                         NF_REAL, 1, 0.0 )
      iret = nf_put_att_text ( ncid, var_id(i), 'grid_mapping',3,'crs' )
      iret = nf_put_att_text ( ncid, var_id(i), 'coverage_content_type',
     +                         19, 'physicalMeasurement' )
      iret = nf_put_att_text ( ncid, var_id(i), 'source', 1, ' ' )
      iret = nf_put_att_text ( ncid, var_id(i), 'references', 1, ' ' )
      iret = nf_put_att_text ( ncid, var_id(i), 'cell_methods', 57,
     +   'time: point longitude: point latitude: point depth: point' )
      iret = nf_put_att_text ( ncid,var_id(i),'ancillary_variables',262,
     + 'instrument1 platform1 salinity_dm_qd salinity_qc_gap salinity_qc
     +_syn salinity_qc_loc salinity_qc_rng salinity_qc +_clm salinity_qc
     +_spk salinity_qc_rtc salinity_qc_rtc salinity_qc_flt salinity_qc_m
     +vr salinity_qc_atn salinity_qc_nbr salinity_qc_crv salinity_qc_din
     +' )
      iret = nf_put_att_text ( ncid, var_id(i), 'platform', 9,
     +                         'platform1' )
      iret = nf_put_att_text ( ncid, var_id(i), 'instrument', 11,
     +                 'instrument1' )
      iret = nf_put_att_text ( ncid, var_id(i), 'comment', 1, ' ' )

c salinity delayed mode qc flag
      i = ivar + 2 * nvars
      iret = nf_put_att_text ( ncid, var_id(i), 'long_name', 40,
     +                     'Salinity delayed-mode quality descriptor' )
      iret = nf_put_att_text ( ncid, var_id(i), 'short_name', 10,
     +                         'salt_dm_qd' )
      iret = nf_put_att_text ( ncid, var_id(i), 'standard_name', 30,
     +                         'sea_water_salinity status_flag' )
      iret = nf_put_att_int ( ncid, var_id(i), 'valid_range', nf_int,
     +                         2, iqc_dm_flag_rv )
      iret = nf_put_att_int ( ncid, var_id(i), '_FillValue',
     +                         NF_INT, 1, int ( xmiss ) )
      iret = nf_put_att_int ( ncid, var_id(i), 'flag_values', nf_int,
     +                         6, iqc_dm_flagv )
      iret = nf_put_att_text ( ncid, var_id(i), 'flag_meanings', 113,
     +                         qc_dm_flag_defv )
      iret = nf_put_att_text ( ncid, var_id(i), 'units', 1, '0' )
      iret = nf_put_att_text ( ncid, var_id(i), 'ioos_category', 7,
     +                         'Quality' )

c salinity QARTOD flags
      i = ivar + 3 * nvars
      iret = nf_put_att_text ( ncid, var_id(i), 'long_name', 15,
     +                  'QARTOD Gap Test' )
      iret = nf_put_att_text ( ncid, var_id(i), 'short_name', 11,
     +                         cvar(ivar,4) )
      iret = nf_put_att_int ( ncid, var_id(i), '_FillValue',
     +                         NF_INT, 1, int ( xmiss ) )
      i = ivar + 4 * nvars
      iret = nf_put_att_text ( ncid, var_id(i), 'long_name', 18,
     +                  'QARTOD Syntax Test' )
      iret = nf_put_att_text ( ncid, var_id(i), 'short_name', 11,
     +                         cvar(ivar,5) )
      iret = nf_put_att_int ( ncid, var_id(i), '_FillValue',
     +                         NF_INT, 1, int ( xmiss ) )
      i = ivar + 5 * nvars
      iret = nf_put_att_text ( ncid, var_id(i), 'long_name', 20,
     +                  'QARTOD Location Test' )
      iret = nf_put_att_text ( ncid, var_id(i), 'short_name', 11,
     +                         cvar(ivar,6) )
      iret = nf_put_att_int ( ncid, var_id(i), '_FillValue',
     +                         NF_INT, 1, int ( xmiss ) )
      i = ivar + 6 * nvars
      iret = nf_put_att_text ( ncid, var_id(i), 'long_name', 23,
     +                  'QARTOD Gross Range Test' )
      iret = nf_put_att_text ( ncid, var_id(i), 'short_name', 11,
     +                         cvar(ivar,7) )
      iret = nf_put_att_int ( ncid, var_id(i), '_FillValue',
     +                         NF_INT, 1, int ( xmiss ) )
      i = ivar + 7 * nvars
      iret = nf_put_att_text ( ncid, var_id(i), 'long_name', 26,
     +                  'QARTOD Climatological Test' )
      iret = nf_put_att_text ( ncid, var_id(i), 'short_name', 11,
     +                         cvar(ivar,8) )
      iret = nf_put_att_int ( ncid, var_id(i), '_FillValue',
     +                         NF_INT, 1, int ( xmiss ) )
      i = ivar + 8 * nvars
      iret = nf_put_att_text ( ncid, var_id(i), 'long_name', 17,
     +                  'QARTOD Spike Test' )
      iret = nf_put_att_text ( ncid, var_id(i), 'short_name', 11,
     +                         cvar(ivar,9) )
      iret = nf_put_att_int ( ncid, var_id(i), '_FillValue',
     +                         NF_INT, 1, int ( xmiss ) )
      i = ivar + 9 * nvars
      iret = nf_put_att_text ( ncid, var_id(i), 'long_name', 26,
     +                  'QARTOD Rate of Change Test' )
      iret = nf_put_att_text ( ncid, var_id(i), 'short_name', 11,
     +                         cvar(ivar,10) )
      iret = nf_put_att_int ( ncid, var_id(i), '_FillValue',
     +                         NF_INT, 1, int ( xmiss ) )
      i = ivar + 10 * nvars
      iret = nf_put_att_text ( ncid, var_id(i), 'long_name', 21,
     +                  'QARTOD Flat Line Test' )
      iret = nf_put_att_text ( ncid, var_id(i), 'short_name', 11,
     +                         cvar(ivar,11) )
      iret = nf_put_att_int ( ncid, var_id(i), '_FillValue',
     +                         NF_INT, 1, int ( xmiss ) )
      i = ivar + 11 * nvars
      iret = nf_put_att_text ( ncid, var_id(i), 'long_name', 25,
     +                  'QARTOD Multi-Variate Test' )
      iret = nf_put_att_text ( ncid, var_id(i), 'short_name', 11,
     +                         cvar(ivar,12) )
      iret = nf_put_att_int ( ncid, var_id(i), '_FillValue',
     +                         NF_INT, 1, int ( xmiss ) )
      i = ivar + 12 * nvars
      iret = nf_put_att_text ( ncid, var_id(i), 'long_name', 29,
     +                  'QARTOD Attenuated Signal Test' )
      iret = nf_put_att_text ( ncid, var_id(i), 'short_name', 11,
     +                         cvar(ivar,13) )
      iret = nf_put_att_int ( ncid, var_id(i), '_FillValue',
     +                         NF_INT, 1, int ( xmiss ) )
      i = ivar + 13 * nvars
      iret = nf_put_att_text ( ncid, var_id(i), 'long_name', 20,
     +                  'QARTOD Neighbor Test' )
      iret = nf_put_att_text ( ncid, var_id(i), 'short_name', 11,
     +                         cvar(ivar,14) )
      iret = nf_put_att_int ( ncid, var_id(i), '_FillValue',
     +                         NF_INT, 1, int ( xmiss ) )
      i = ivar + 14 * nvars
      iret = nf_put_att_text ( ncid, var_id(i), 'long_name', 27,
     +                  'QARTOD T/S Curve/Space Test' )
      iret = nf_put_att_text ( ncid, var_id(i), 'short_name', 11,
     +                         cvar(ivar,15) )
      iret = nf_put_att_int ( ncid, var_id(i), '_FillValue',
     +                         NF_INT, 1, int ( xmiss ) )
      i = ivar + 15 * nvars
      iret = nf_put_att_text ( ncid, var_id(i), 'long_name', 29,
     +                  'QARTOD Density Inversion Test' )
      iret = nf_put_att_text ( ncid, var_id(i), 'short_name', 11,
     +                         cvar(ivar,16) )
      iret = nf_put_att_int ( ncid, var_id(i), '_FillValue',
     +                         NF_INT, 1, int ( xmiss ) )
      do 150 j = 1, nqc
         i = ivar + (j+2) * nvars
         iret = nf_put_att_text ( ncid, var_id(i), 'standard_name', 30,
     +                         'sea_water_salinity status_flag' )
         iret = nf_put_att_int ( ncid, var_id(i), 'valid_range', nf_int,
     +                         2, iqc_rt_flag_rv )
         iret = nf_put_att_int ( ncid, var_id(i), '_FillValue',
     +                         NF_INT, 1, int ( xmiss ) )
         iret = nf_put_att_int ( ncid, var_id(i), 'flag_values', nf_int,
     +                         5, iqc_rt_flagv )
         iret = nf_put_att_text ( ncid, var_id(i), 'flag_meanings', 71,
     +                         qc_rt_flag_defv )
         iret = nf_put_att_text ( ncid, var_id(i), 'units', 1, '0' )
         iret = nf_put_att_text ( ncid, var_id(i), 'ioos_category', 7,
     +                         'Quality' )
150   continue

      ivar = 3
c oxygen saturation (fraction/percent) adjusted following delayed-mode qc
      i = ivar + 0 * nvars
      iret = nf_put_att_text ( ncid, var_id(i), 'long_name', 39,
     +                     'Dissolved oxygen saturation (processed)' )
      iret = nf_put_att_text ( ncid, var_id(i), 'standard_name', 44,
     +                'fractional_saturation_of_oxygen_in_sea_water' )
      iret = nf_put_att_text ( ncid, var_id(i), 'short_name', 4,
     +                         'osat' )
      iret = nf_put_att_text ( ncid, var_id(i), 'units', 1,
     +                         '1')
      iret = nf_put_att_text ( ncid, var_id(i), 'ioos_category', 12,
     +                         'Dissolved O2' )
      iret = nf_put_att_text ( ncid, var_id(i), 'coordinates', 29,
     +                         'time latitude longitude depth')
      range_val(1) = 0.0
      range_val(2) = 2.0
      iret = nf_put_att_real ( ncid, var_id(i), 'valid_range',
     +                           NF_REAL, 2, range_val )
      iret = nf_put_att_real ( ncid, var_id(i), '_FillValue',
     +                         NF_REAL, 1, xmiss )
      iret = nf_put_att_text ( ncid, var_id(i), 'observation_type', 10,
     +                         'calculated')
      iret = nf_put_att_text ( ncid, var_id(i), 'ncei_name', 27,
     +                         'OXYGEN - PERCENT SATURATION' )
      iret = nf_put_att_real ( ncid, var_id(i), 'scale_factor', 
     +                         NF_REAL, 1, 1.0 )
      iret = nf_put_att_real ( ncid, var_id(i), 'add_offset', 
     +                         NF_REAL, 1, 0.0 )
      iret = nf_put_att_text ( ncid, var_id(i), 'grid_mapping',3,'crs' )
      iret = nf_put_att_text ( ncid, var_id(i), 'coverage_content_type',
     +                         19, 'physicalMeasurement' )
      iret = nf_put_att_text ( ncid, var_id(i), 'source', 1, ' ' )
      iret = nf_put_att_text ( ncid, var_id(i), 'references', 1, ' ' )
      iret = nf_put_att_text ( ncid, var_id(i), 'cell_methods', 57,
     +   'time: point longitude: point latitude: point depth: point' )
      iret = nf_put_att_text ( ncid,var_id(i),'ancillary_variables',345,
     + 'instrument1 platform1 oxygen_saturation_dm_qd oxygen_saturation_
     +qc_gap oxygen_saturation_qc_syn oxygen_saturation_qc_loc oxygen_sa
     +turation_qc_rng oxygen_saturation_qc_clm oxygen_saturation_qc_spk 
     +oxygen_saturation_qc_rtc oxygen_saturation_qc_rtc oxygen_saturatio
     +n_qc_flt oxygen_saturation_qc_mvr oxygen_saturation_qc_atn oxygen_
     +saturation_qc_nbr' )
      iret = nf_put_att_text ( ncid, var_id(i), 'platform', 9,
     +                         'platform1' )
      iret = nf_put_att_text ( ncid, var_id(i), 'instrument', 11,
     +                 'instrument1' )
      iret = nf_put_att_text ( ncid, var_id(i), 'comment', 1, ' ' )

c raw oxygen saturation (fraction/percent)
      i = ivar + 1 * nvars
      iret = nf_put_att_text ( ncid, var_id(i), 'long_name', 33,
     +                         'Dissolved oxygen saturation (raw)' )
      iret = nf_put_att_text ( ncid, var_id(i), 'standard_name', 44,
     +                'fractional_saturation_of_oxygen_in_sea_water' )
      iret = nf_put_att_text ( ncid, var_id(i), 'short_name', 8,
     +                         'osat_raw' )
      iret = nf_put_att_text ( ncid, var_id(i), 'units', 1,
     +                         '1')
      iret = nf_put_att_text ( ncid, var_id(i), 'ioos_category', 12,
     +                         'Dissolved O2' )
      iret = nf_put_att_text ( ncid, var_id(i), 'coordinates', 29,
     +                         'time latitude longitude depth')
      range_val(1) = 0.0
      range_val(2) = 2.0
      iret = nf_put_att_real ( ncid, var_id(i), 'valid_range',
     +                           NF_REAL, 2, range_val )
      iret = nf_put_att_real ( ncid, var_id(i), '_FillValue',
     +                         NF_REAL, 1, xmiss )
      iret = nf_put_att_text ( ncid, var_id(i), 'observation_type', 10,
     +                         'calculated')
      iret = nf_put_att_text ( ncid, var_id(i), 'comment', 42,
     +         'values are converted from ODO% as ODO%/100' )
      iret = nf_put_att_text ( ncid, var_id(i), 'ncei_name', 27,
     +                         'OXYGEN - PERCENT SATURATION' )
      iret = nf_put_att_real ( ncid, var_id(i), 'scale_factor', 
     +                         NF_REAL, 1, 1.0 )
      iret = nf_put_att_real ( ncid, var_id(i), 'add_offset', 
     +                         NF_REAL, 1, 0.0 )
      iret = nf_put_att_text ( ncid, var_id(i), 'grid_mapping',3,'crs' )
      iret = nf_put_att_text ( ncid, var_id(i), 'coverage_content_type',
     +                         19, 'physicalMeasurement' )
      iret = nf_put_att_text ( ncid, var_id(i), 'source', 1, ' ' )
      iret = nf_put_att_text ( ncid, var_id(i), 'references', 1, ' ' )
      iret = nf_put_att_text ( ncid, var_id(i), 'cell_methods', 57,
     +   'time: point longitude: point latitude: point depth: point' )
      iret = nf_put_att_text ( ncid,var_id(i),'ancillary_variables',345,
     + 'instrument1 platform1 oxygen_saturation_dm_qd oxygen_saturation_
     +qc_gap oxygen_saturation_qc_syn oxygen_saturation_qc_loc oxygen_sa
     +turation_qc_rng oxygen_saturation_qc_clm oxygen_saturation_qc_spk 
     +oxygen_saturation_qc_rtc oxygen_saturation_qc_rtc oxygen_saturatio
     +n_qc_flt oxygen_saturation_qc_mvr oxygen_saturation_qc_atn oxygen_
     +saturation_qc_nbr' )
      iret = nf_put_att_text ( ncid, var_id(i), 'platform', 9,
     +                         'platform1' )
      iret = nf_put_att_text ( ncid, var_id(i), 'instrument', 11,
     +                 'instrument1' )
      iret = nf_put_att_text ( ncid, var_id(i), 'comment', 1, ' ' )

c oxygen saturation (fraction/percent) delayed mode qc flag
      i = ivar + 2 * nvars
      iret = nf_put_att_text ( ncid, var_id(i), 'long_name', 49,
     +            'Oxygen saturation delayed-mode quality descriptor' )
      iret = nf_put_att_text ( ncid, var_id(i), 'short_name', 10,
     +                         'osat_dm_qd' )
      iret = nf_put_att_text ( ncid, var_id(i), 'standard_name', 56,
     +      'fractional_saturation_of_oxygen_in_sea_water status_flag' )
      iret = nf_put_att_int ( ncid, var_id(i), 'valid_range', nf_int,
     +                         2, iqc_dm_flag_rv )
      iret = nf_put_att_int ( ncid, var_id(i), '_FillValue',
     +                         NF_INT, 1, int ( xmiss ) )
      iret = nf_put_att_int ( ncid, var_id(i), 'flag_values', nf_int,
     +                         6, iqc_dm_flagv )
      iret = nf_put_att_text ( ncid, var_id(i), 'flag_meanings', 113,
     +                         qc_dm_flag_defv )
      iret = nf_put_att_text ( ncid, var_id(i), 'units', 1, '0' )
      iret = nf_put_att_text ( ncid, var_id(i), 'ioos_category', 7,
     +                         'Quality' )

c oxygen saturation (fraction/percent) QARTOD flags
      i = ivar + 3 * nvars
      iret = nf_put_att_text ( ncid, var_id(i), 'long_name', 15,
     +                  'QARTOD Gap Test' )
      iret = nf_put_att_text ( ncid, var_id(i), 'short_name', 11,
     +                         cvar(ivar,4) )
      iret = nf_put_att_int ( ncid, var_id(i), '_FillValue',
     +                         NF_INT, 1, int ( xmiss ) )
      i = ivar + 4 * nvars
      iret = nf_put_att_text ( ncid, var_id(i), 'long_name', 18,
     +                  'QARTOD Syntax Test' )
      iret = nf_put_att_text ( ncid, var_id(i), 'short_name', 11,
     +                         cvar(ivar,5) )
      iret = nf_put_att_int ( ncid, var_id(i), '_FillValue',
     +                         NF_INT, 1, int ( xmiss ) )
      i = ivar + 5 * nvars
      iret = nf_put_att_text ( ncid, var_id(i), 'long_name', 20,
     +                  'QARTOD Location Test' )
      iret = nf_put_att_text ( ncid, var_id(i), 'short_name', 11,
     +                         cvar(ivar,6) )
      iret = nf_put_att_int ( ncid, var_id(i), '_FillValue',
     +                         NF_INT, 1, int ( xmiss ) )
      i = ivar + 6 * nvars
      iret = nf_put_att_text ( ncid, var_id(i), 'long_name', 23,
     +                  'QARTOD Gross Range Test' )
      iret = nf_put_att_text ( ncid, var_id(i), 'short_name', 11,
     +                         cvar(ivar,7) )
      iret = nf_put_att_int ( ncid, var_id(i), '_FillValue',
     +                         NF_INT, 1, int ( xmiss ) )
      i = ivar + 7 * nvars
      iret = nf_put_att_text ( ncid, var_id(i), 'long_name', 26,
     +                  'QARTOD Climatological Test' )
      iret = nf_put_att_text ( ncid, var_id(i), 'short_name', 11,
     +                         cvar(ivar,8) )
      iret = nf_put_att_int ( ncid, var_id(i), '_FillValue',
     +                         NF_INT, 1, int ( xmiss ) )
      i = ivar + 8 * nvars
      iret = nf_put_att_text ( ncid, var_id(i), 'long_name', 17,
     +                  'QARTOD Spike Test' )
      iret = nf_put_att_text ( ncid, var_id(i), 'short_name', 11,
     +                         cvar(ivar,9) )
      iret = nf_put_att_int ( ncid, var_id(i), '_FillValue',
     +                         NF_INT, 1, int ( xmiss ) )
      i = ivar + 9 * nvars
      iret = nf_put_att_text ( ncid, var_id(i), 'long_name', 26,
     +                  'QARTOD Rate of Change Test' )
      iret = nf_put_att_text ( ncid, var_id(i), 'short_name', 11,
     +                         cvar(ivar,10) )
      iret = nf_put_att_int ( ncid, var_id(i), '_FillValue',
     +                         NF_INT, 1, int ( xmiss ) )
      i = ivar + 10 * nvars
      iret = nf_put_att_text ( ncid, var_id(i), 'long_name', 21,
     +                  'QARTOD Flat Line Test' )
      iret = nf_put_att_text ( ncid, var_id(i), 'short_name', 11,
     +                         cvar(ivar,11) )
      iret = nf_put_att_int ( ncid, var_id(i), '_FillValue',
     +                         NF_INT, 1, int ( xmiss ) )
      i = ivar + 11 * nvars
      iret = nf_put_att_text ( ncid, var_id(i), 'long_name', 25,
     +                  'QARTOD Multi-Variate Test' )
      iret = nf_put_att_text ( ncid, var_id(i), 'short_name', 11,
     +                         cvar(ivar,12) )
      iret = nf_put_att_int ( ncid, var_id(i), '_FillValue',
     +                         NF_INT, 1, int ( xmiss ) )
      i = ivar + 12 * nvars
      iret = nf_put_att_text ( ncid, var_id(i), 'long_name', 29,
     +                  'QARTOD Attenuated Signal Test' )
      iret = nf_put_att_text ( ncid, var_id(i), 'short_name', 11,
     +                         cvar(ivar,13) )
      iret = nf_put_att_int ( ncid, var_id(i), '_FillValue',
     +                         NF_INT, 1, int ( xmiss ) )
      i = ivar + 13 * nvars
      iret = nf_put_att_text ( ncid, var_id(i), 'long_name', 20,
     +                  'QARTOD Neighbor Test' )
      iret = nf_put_att_text ( ncid, var_id(i), 'short_name', 11,
     +                         cvar(ivar,14) )
      iret = nf_put_att_int ( ncid, var_id(i), '_FillValue',
     +                         NF_INT, 1, int ( xmiss ) )
c these are not defined for oxygen
c      i = ivar + 14 * nvars
c      iret = nf_put_att_text ( ncid, var_id(i), 'long_name', 27,
c     +                  'QARTOD T/S Curve/Space Test' )
c      iret = nf_put_att_text ( ncid, var_id(i), 'short_name', 11,
c     +                         cvar(ivar,15) )
c      i = ivar + 15 * nvars
c      iret = nf_put_att_text ( ncid, var_id(i), 'long_name', 29,
c     +                  'QARTOD Density Inversion Test' )
c      iret = nf_put_att_text ( ncid, var_id(i), 'short_name', 11,
c     +                         cvar(ivar,16) )
c      do 160 j = 1, nqc
      do 160 j = 1, 11
         i = ivar + (j+2) * nvars
         iret = nf_put_att_text ( ncid, var_id(i), 'standard_name', 56,
     +      'fractional_saturation_of_oxygen_in_sea_water status_flag' )
         iret = nf_put_att_int ( ncid, var_id(i), 'valid_range', nf_int,
     +                         2, iqc_rt_flag_rv )
         iret = nf_put_att_int ( ncid, var_id(i), '_FillValue',
     +                         NF_INT, 1, int ( xmiss ) )
         iret = nf_put_att_int ( ncid, var_id(i), 'flag_values', nf_int,
     +                         5, iqc_rt_flagv )
         iret = nf_put_att_text ( ncid, var_id(i), 'flag_meanings', 71,
     +                         qc_rt_flag_defv )
         iret = nf_put_att_text ( ncid, var_id(i), 'units', 1, '0' )
         iret = nf_put_att_text ( ncid, var_id(i), 'ioos_category', 7,
     +                         'Quality' )
160   continue

      ivar = 4
c oxygen concentration (ml/L) adjusted followind delayed-mode qc
      i = ivar + 0 * nvars
      iret = nf_put_att_text ( ncid, var_id(i), 'long_name', 42,
     +                   'Dissolved oxygen concentration (processed)' )
      iret = nf_put_att_text ( ncid, var_id(i), 'standard_name', 41,
     +                'mass_concentration_of_oxygen_in_sea_water' )
      iret = nf_put_att_text ( ncid, var_id(i), 'short_name', 4,
     +                         'odoc' )
      iret = nf_put_att_text ( ncid, var_id(i), 'units', 6,
     +                         'kg m-3')
      iret = nf_put_att_text ( ncid, var_id(i), 'ioos_category', 12,
     +                         'Dissolved O2' )
      iret = nf_put_att_text ( ncid, var_id(i), 'coordinates', 29,
     +                         'time latitude longitude depth')
      range_val(1) = 0.0
      range_val(2) = 10.0
      iret = nf_put_att_real ( ncid, var_id(i), 'valid_range',
     +                           NF_REAL, 2, range_val )
      iret = nf_put_att_real ( ncid, var_id(i), '_FillValue',
     +                         NF_REAL, 1, xmiss )
      iret = nf_put_att_text ( ncid, var_id(i), 'observation_type', 10,
     +                         'calculated')
      iret = nf_put_att_text ( ncid, var_id(i), 'ncei_name', 16,
     +                         'DISSOLVED OXYGEN' )
      iret = nf_put_att_real ( ncid, var_id(i), 'scale_factor', 
     +                         NF_REAL, 1, 1.0 )
      iret = nf_put_att_real ( ncid, var_id(i), 'add_offset', 
     +                         NF_REAL, 1, 0.0 )
      iret = nf_put_att_text ( ncid, var_id(i), 'grid_mapping',3,'crs' )
      iret = nf_put_att_text ( ncid, var_id(i), 'coverage_content_type',
     +                         19, 'physicalMeasurement' )
      iret = nf_put_att_text ( ncid, var_id(i), 'source', 1, ' ' )
      iret = nf_put_att_text ( ncid, var_id(i), 'references', 1, ' ' )
      iret = nf_put_att_text ( ncid, var_id(i), 'cell_methods', 57,
     +   'time: point longitude: point latitude: point depth: point' )
      iret = nf_put_att_text ( ncid,var_id(i),'ancillary_variables',202,
     + 'instrument1 platform1 oxygen_dm_qd oxygen_qc_gap oxygen_qc_syn o
     +xygen_qc_loc oxygen_qc_rng oxygen_qc_clm oxygen_qc_spk oxygen_qc_r
     +tc oxygen_qc_rtc oxygen_qc_flt oxygen_qc_mvr oxygen_qc_atn oxygen_
     +qc_nbr' )
      iret = nf_put_att_text ( ncid, var_id(i), 'platform', 9,
     +                         'platform1' )
      iret = nf_put_att_text ( ncid, var_id(i), 'instrument', 11,
     +                 'instrument1' )
      iret = nf_put_att_text ( ncid, var_id(i), 'comment', 1, ' ' )

c oxygen concentration (ml/L) raw
      i = ivar + 1 * nvars
      iret = nf_put_att_text ( ncid, var_id(i), 'long_name', 36,
     +                         'Dissolved oxygen concentration (raw)' )
      iret = nf_put_att_text ( ncid, var_id(i), 'standard_name', 41,
     +                'mass_concentration_of_oxygen_in_sea_water' )
      iret = nf_put_att_text ( ncid, var_id(i), 'short_name', 8,
     +                         'odoc_raw' )
      iret = nf_put_att_text ( ncid, var_id(i), 'units', 6,
     +                         'kg m-3')
      iret = nf_put_att_text ( ncid, var_id(i), 'ioos_category', 12,
     +                         'Dissolved O2' )
      iret = nf_put_att_text ( ncid, var_id(i), 'coordinates', 29,
     +                         'time latitude longitude depth')
      range_val(1) = 0.0
      range_val(2) = 10.0
      iret = nf_put_att_real ( ncid, var_id(i), 'valid_range',
     +                           NF_REAL, 2, range_val )
      iret = nf_put_att_real ( ncid, var_id(i), '_FillValue',
     +                         NF_REAL, 1, xmiss )
      iret = nf_put_att_text ( ncid, var_id(i), 'observation_type', 10,
     +                         'calculated')
      iret = nf_put_att_text ( ncid, var_id(i), 'comment', 88,
     +       'values are converted from ODO Conc [mg/L] to odoc [kg m-3]
     + as odoc=ODO / 1000.0' )
      iret = nf_put_att_text ( ncid, var_id(i), 'ncei_name', 16,
     +                         'DISSOLVED OXYGEN' )
      iret = nf_put_att_real ( ncid, var_id(i), 'scale_factor', 
     +                         NF_REAL, 1, 1.0 )
      iret = nf_put_att_real ( ncid, var_id(i), 'add_offset', 
     +                         NF_REAL, 1, 0.0 )
      iret = nf_put_att_text ( ncid, var_id(i), 'grid_mapping',3,'crs' )
      iret = nf_put_att_text ( ncid, var_id(i), 'coverage_content_type',
     +                         19, 'physicalMeasurement' )
      iret = nf_put_att_text ( ncid, var_id(i), 'source', 1, ' ' )
      iret = nf_put_att_text ( ncid, var_id(i), 'references', 1, ' ' )
      iret = nf_put_att_text ( ncid, var_id(i), 'cell_methods', 57,
     +   'time: point longitude: point latitude: point depth: point' )
      iret = nf_put_att_text ( ncid,var_id(i),'ancillary_variables',202,
     + 'instrument1 platform1 oxygen_dm_qd oxygen_qc_gap oxygen_qc_syn o
     +xygen_qc_loc oxygen_qc_rng oxygen_qc_clm oxygen_qc_spk oxygen_qc_r
     +tc oxygen_qc_rtc oxygen_qc_flt oxygen_qc_mvr oxygen_qc_atn oxygen_
     +qc_nbr' )
      iret = nf_put_att_text ( ncid, var_id(i), 'platform', 9,
     +                         'platform1' )
      iret = nf_put_att_text ( ncid, var_id(i), 'instrument', 11,
     +                 'instrument1' )
      iret = nf_put_att_text ( ncid, var_id(i), 'comment', 1, ' ' )

c oxygen concentration delayed mode qc flag
      i = ivar + 2 * nvars
      iret = nf_put_att_text ( ncid, var_id(i), 'long_name', 52,
     +         'Oxygen concentration delayed-mode quality descriptor' )
      iret = nf_put_att_text ( ncid, var_id(i), 'short_name', 10,
     +                         'odoc_dm_qd' )
      iret = nf_put_att_text ( ncid, var_id(i), 'standard_name', 53,
     +         'mass_concentration_of_oxygen_in_sea_water status_flag' )
      iret = nf_put_att_int ( ncid, var_id(i), 'valid_range', nf_int,
     +                         2, iqc_dm_flag_rv )
      iret = nf_put_att_int ( ncid, var_id(i), '_FillValue',
     +                         NF_INT, 1, int ( xmiss ) )
      iret = nf_put_att_int ( ncid, var_id(i), 'flag_values', nf_int,
     +                         6, iqc_dm_flagv )
      iret = nf_put_att_text ( ncid, var_id(i), 'flag_meanings', 113,
     +                         qc_dm_flag_defv )
      iret = nf_put_att_text ( ncid, var_id(i), 'units', 1, '0' )
      iret = nf_put_att_text ( ncid, var_id(i), 'ioos_category', 7,
     +                         'Quality' )

c oxygen concentration QARTOD flags
      i = ivar + 3 * nvars
      iret = nf_put_att_text ( ncid, var_id(i), 'long_name', 15,
     +                  'QARTOD Gap Test' )
      iret = nf_put_att_text ( ncid, var_id(i), 'short_name', 11,
     +                         cvar(ivar,4) )
      iret = nf_put_att_int ( ncid, var_id(i), '_FillValue',
     +                         NF_INT, 1, int ( xmiss ) )
      i = ivar + 4 * nvars
      iret = nf_put_att_text ( ncid, var_id(i), 'long_name', 18,
     +                  'QARTOD Syntax Test' )
      iret = nf_put_att_text ( ncid, var_id(i), 'short_name', 11,
     +                         cvar(ivar,5) )
      iret = nf_put_att_int ( ncid, var_id(i), '_FillValue',
     +                         NF_INT, 1, int ( xmiss ) )
      i = ivar + 5 * nvars
      iret = nf_put_att_text ( ncid, var_id(i), 'long_name', 20,
     +                  'QARTOD Location Test' )
      iret = nf_put_att_text ( ncid, var_id(i), 'short_name', 11,
     +                         cvar(ivar,6) )
      iret = nf_put_att_int ( ncid, var_id(i), '_FillValue',
     +                         NF_INT, 1, int ( xmiss ) )
      i = ivar + 6 * nvars
      iret = nf_put_att_text ( ncid, var_id(i), 'long_name', 23,
     +                  'QARTOD Gross Range Test' )
      iret = nf_put_att_text ( ncid, var_id(i), 'short_name', 11,
     +                         cvar(ivar,7) )
      iret = nf_put_att_int ( ncid, var_id(i), '_FillValue',
     +                         NF_INT, 1, int ( xmiss ) )
      i = ivar + 7 * nvars
      iret = nf_put_att_text ( ncid, var_id(i), 'long_name', 26,
     +                  'QARTOD Climatological Test' )
      iret = nf_put_att_text ( ncid, var_id(i), 'short_name', 11,
     +                         cvar(ivar,8) )
      iret = nf_put_att_int ( ncid, var_id(i), '_FillValue',
     +                         NF_INT, 1, int ( xmiss ) )
      i = ivar + 8 * nvars
      iret = nf_put_att_text ( ncid, var_id(i), 'long_name', 17,
     +                  'QARTOD Spike Test' )
      iret = nf_put_att_text ( ncid, var_id(i), 'short_name', 11,
     +                         cvar(ivar,9) )
      iret = nf_put_att_int ( ncid, var_id(i), '_FillValue',
     +                         NF_INT, 1, int ( xmiss ) )
      i = ivar + 9 * nvars
      iret = nf_put_att_text ( ncid, var_id(i), 'long_name', 26,
     +                  'QARTOD Rate of Change Test' )
      iret = nf_put_att_text ( ncid, var_id(i), 'short_name', 11,
     +                         cvar(ivar,10) )
      iret = nf_put_att_int ( ncid, var_id(i), '_FillValue',
     +                         NF_INT, 1, int ( xmiss ) )
      i = ivar + 10 * nvars
      iret = nf_put_att_text ( ncid, var_id(i), 'long_name', 21,
     +                  'QARTOD Flat Line Test' )
      iret = nf_put_att_text ( ncid, var_id(i), 'short_name', 11,
     +                         cvar(ivar,11) )
      iret = nf_put_att_int ( ncid, var_id(i), '_FillValue',
     +                         NF_INT, 1, int ( xmiss ) )
      i = ivar + 11 * nvars
      iret = nf_put_att_text ( ncid, var_id(i), 'long_name', 25,
     +                  'QARTOD Multi-Variate Test' )
      iret = nf_put_att_text ( ncid, var_id(i), 'short_name', 11,
     +                         cvar(ivar,12) )
      iret = nf_put_att_int ( ncid, var_id(i), '_FillValue',
     +                         NF_INT, 1, int ( xmiss ) )
      i = ivar + 12 * nvars
      iret = nf_put_att_text ( ncid, var_id(i), 'long_name', 29,
     +                  'QARTOD Attenuated Signal Test' )
      iret = nf_put_att_text ( ncid, var_id(i), 'short_name', 11,
     +                         cvar(ivar,13) )
      iret = nf_put_att_int ( ncid, var_id(i), '_FillValue',
     +                         NF_INT, 1, int ( xmiss ) )
      i = ivar + 13 * nvars
      iret = nf_put_att_text ( ncid, var_id(i), 'long_name', 20,
     +                  'QARTOD Neighbor Test' )
      iret = nf_put_att_text ( ncid, var_id(i), 'short_name', 11,
     +                         cvar(ivar,14) )
      iret = nf_put_att_int ( ncid, var_id(i), '_FillValue',
     +                         NF_INT, 1, int ( xmiss ) )
c these are not used for oxygen
c      i = ivar + 14 * nvars
c      iret = nf_put_att_text ( ncid, var_id(i), 'long_name', 27,
c     +                  'QARTOD T/S Curve/Space Test' )
c      iret = nf_put_att_text ( ncid, var_id(i), 'short_name', 11,
c     +                         cvar(ivar,15) )
c      i = ivar + 15 * nvars
c      iret = nf_put_att_text ( ncid, var_id(i), 'long_name', 29,
c     +                  'QARTOD Density Inversion Test' )
c      iret = nf_put_att_text ( ncid, var_id(i), 'short_name', 11,
c     +                         cvar(ivar,16) )
c      do 140 j = 1, nqc
      do 170 j = 1, 11
         i = ivar + (j+2) * nvars
         iret = nf_put_att_text ( ncid, var_id(i), 'standard_name', 53,
     +         'mass_concentration_of_oxygen_in_sea_water status_flag' )
         iret = nf_put_att_int ( ncid, var_id(i), 'valid_range', nf_int,
     +                         2, iqc_rt_flag_rv )
         iret = nf_put_att_int ( ncid, var_id(i), '_FillValue',
     +                         NF_INT, 1, int ( xmiss ) )
         iret = nf_put_att_int ( ncid, var_id(i), 'flag_values', nf_int,
     +                         5, iqc_rt_flagv )
         iret = nf_put_att_text ( ncid, var_id(i), 'flag_meanings', 71,
     +                         qc_rt_flag_defv )
         iret = nf_put_att_text ( ncid, var_id(i), 'units', 1, '0' )
         iret = nf_put_att_text ( ncid, var_id(i), 'ioos_category', 7,
     +                         'Quality' )
170   continue

      ivar = 5
c chlorphyll adjusted following delayed-mode qc
      i = ivar + 0 * nvars
      iret = nf_put_att_text ( ncid, var_id(i), 'long_name', 23,
     +                         'Chlorophyll (processed)' )
      iret = nf_put_att_text ( ncid, var_id(i), 'standard_name', 46,
     +              'mass_concentration_of_chlorophyll_in_sea_water' )
      iret = nf_put_att_text ( ncid, var_id(i), 'short_name', 4,
     +                         'flor' )
      iret = nf_put_att_text ( ncid, var_id(i), 'units', 6,
     +                         'kg m-3')
      iret = nf_put_att_text ( ncid, var_id(i), 'ioos_category', 11,
     +                         'Ocean Color' )
      iret = nf_put_att_text ( ncid, var_id(i), 'coordinates', 29,
     +                         'time latitude longitude depth')
      range_val(1) = 0.0
      range_val(2) = 10.0
      iret = nf_put_att_real ( ncid, var_id(i), 'valid_range',
     +                           NF_REAL, 2, range_val )
      iret = nf_put_att_real ( ncid, var_id(i), '_FillValue',
     +                         NF_REAL, 1, xmiss )
      iret = nf_put_att_text ( ncid, var_id(i), 'observation_type', 10,
     +                         'calculated')
      iret = nf_put_att_text ( ncid, var_id(i), 'comment', 52,
     +         'chl is converted from RFU as [mgr/L] = 4.55*RFU-0.88' )
      iret = nf_put_att_text ( ncid, var_id(i), 'ncei_name', 27,
     +                         'CHLOROPHYLL A CONCENTRATION' )
      iret = nf_put_att_real ( ncid, var_id(i), 'scale_factor', 
     +                         NF_REAL, 1, 1.0 )
      iret = nf_put_att_real ( ncid, var_id(i), 'add_offset', 
     +                         NF_REAL, 1, 0.0 )
      iret = nf_put_att_text ( ncid, var_id(i), 'grid_mapping',3,'crs' )
      iret = nf_put_att_text ( ncid, var_id(i), 'coverage_content_type',
     +                         19, 'physicalMeasurement' )
      iret = nf_put_att_text ( ncid, var_id(i), 'source', 1, ' ' )
      iret = nf_put_att_text ( ncid, var_id(i), 'references', 1, ' ' )
      iret = nf_put_att_text ( ncid, var_id(i), 'cell_methods', 57,
     +   'time: point longitude: point latitude: point depth: point' )
      iret = nf_put_att_text ( ncid,var_id(i),'ancillary_variables',305,
     + 'instrument1 platform1 chlorophyll_dm_qd chlorophyll_qc_gap chlor
     +ophyll_qc_syn chlorophyll_qc_loc chlorophyll_qc_rng chlorophyll_qc
     +_clm chlorophyll_qc_spk chlorophyll_qc_rtc chlorophyll_qc_rtc chlo
     +rophyll_qc_flt chlorophyll_qc_mvr chlorophyll_qc_atn chlorophyll_q
     +c_nbr chlorophyll_qc_crv chlorophyll_qc_din' )
      iret = nf_put_att_text ( ncid, var_id(i), 'platform', 9,
     +                         'platform1' )
      iret = nf_put_att_text ( ncid, var_id(i), 'instrument', 11,
     +                 'instrument1' )
      iret = nf_put_att_text ( ncid, var_id(i), 'comment', 1, ' ' )

c raw chlorphyll 
      i = ivar + 1 * nvars
      iret = nf_put_att_text ( ncid, var_id(i), 'long_name', 17,
     +                         'Chlorophyll (raw)' )
      iret = nf_put_att_text ( ncid, var_id(i), 'standard_name', 46,
     +              'mass_concentration_of_chlorophyll_in_sea_water' )
      iret = nf_put_att_text ( ncid, var_id(i), 'short_name', 8,
     +                         'flor_raw' )
      iret = nf_put_att_text ( ncid, var_id(i), 'units', 6,
     +                         'kg m-3')
      iret = nf_put_att_text ( ncid, var_id(i), 'ioos_category', 11,
     +                         'Ocean Color' )
      iret = nf_put_att_text ( ncid, var_id(i), 'coordinates', 29,
     +                         'time latitude longitude depth')
      range_val(1) = 0.0
      range_val(2) = 10.0
      iret = nf_put_att_real ( ncid, var_id(i), 'valid_range',
     +                           NF_REAL, 2, range_val )
      iret = nf_put_att_real ( ncid, var_id(i), '_FillValue',
     +                         NF_REAL, 1, xmiss )
      iret = nf_put_att_text ( ncid, var_id(i), 'observation_type', 10,
     +                         'calculated')
      iret = nf_put_att_text ( ncid, var_id(i), 'comment', 52,
     +         'chl is converted from RFU as [mgr/L] = 4.i5*RFU-0.88' )
      iret = nf_put_att_text ( ncid, var_id(i), 'ncei_name', 27,
     +                         'CHLOROPHYLL A CONCENTRATION' )
      iret = nf_put_att_real ( ncid, var_id(i), 'scale_factor', 
     +                         NF_REAL, 1, 1.0 )
      iret = nf_put_att_real ( ncid, var_id(i), 'add_offset', 
     +                         NF_REAL, 1, 0.0 )
      iret = nf_put_att_text ( ncid, var_id(i), 'grid_mapping',3,'crs' )
      iret = nf_put_att_text ( ncid, var_id(i), 'coverage_content_type',
     +                         19, 'physicalMeasurement' )
      iret = nf_put_att_text ( ncid, var_id(i), 'source', 1, ' ' )
      iret = nf_put_att_text ( ncid, var_id(i), 'references', 1, ' ' )
      iret = nf_put_att_text ( ncid, var_id(i), 'cell_methods', 57,
     +   'time: point longitude: point latitude: point depth: point' )
      iret = nf_put_att_text ( ncid,var_id(i),'ancillary_variables',305,
     + 'instrument1 platform1 chlorophyll_dm_qd chlorophyll_qc_gap chlor
     +ophyll_qc_syn chlorophyll_qc_loc chlorophyll_qc_rng chlorophyll_qc
     +_clm chlorophyll_qc_spk chlorophyll_qc_rtc chlorophyll_qc_rtc chlo
     +rophyll_qc_flt chlorophyll_qc_mvr chlorophyll_qc_atn chlorophyll_q
     +c_nbr chlorophyll_qc_crv chlorophyll_qc_din' )
      iret = nf_put_att_text ( ncid, var_id(i), 'platform', 9,
     +                         'platform1' )
      iret = nf_put_att_text ( ncid, var_id(i), 'instrument', 11,
     +                 'instrument1' )
      iret = nf_put_att_text ( ncid, var_id(i), 'comment', 1, ' ' )
      
c chlorophyll delayed mode qc flag
      i = ivar + 2 * nvars
      iret = nf_put_att_text ( ncid, var_id(i), 'long_name', 43,
     +                  'Chlorophyll delayed-mode quality descriptor' )
      iret = nf_put_att_text ( ncid, var_id(i), 'short_name', 10,
     +                         'flor_dm_qd' )
      iret = nf_put_att_text ( ncid, var_id(i), 'standard_name', 58,
     +    'mass_concentration_of_chlorophyll_in_sea_water status_flag' )
      iret = nf_put_att_int ( ncid, var_id(i), 'valid_range', nf_int,
     +                         2, iqc_dm_flag_rv )
      iret = nf_put_att_int ( ncid, var_id(i), '_FillValue',
     +                         NF_INT, 1, int ( xmiss ) )
      iret = nf_put_att_int ( ncid, var_id(i), 'flag_values', nf_int,
     +                         6, iqc_dm_flagv )
      iret = nf_put_att_text ( ncid, var_id(i), 'flag_meanings', 113,
     +                         qc_dm_flag_defv )
      iret = nf_put_att_text ( ncid, var_id(i), 'units', 1, '0' )
      iret = nf_put_att_text ( ncid, var_id(i), 'ioos_category', 7,
     +                         'Quality' )

c chlorophyll QARTOD flags
      i = ivar + 3 * nvars
      iret = nf_put_att_text ( ncid, var_id(i), 'long_name', 15,
     +                  'QARTOD Gap Test' )
      iret = nf_put_att_text ( ncid, var_id(i), 'short_name', 11,
     +                         cvar(ivar,4) )
      iret = nf_put_att_int ( ncid, var_id(i), '_FillValue',
     +                         NF_INT, 1, int ( xmiss ) )
      i = ivar + 4 * nvars
      iret = nf_put_att_text ( ncid, var_id(i), 'long_name', 18,
     +                  'QARTOD Syntax Test' )
      iret = nf_put_att_text ( ncid, var_id(i), 'short_name', 11,
     +                         cvar(ivar,5) )
      iret = nf_put_att_int ( ncid, var_id(i), '_FillValue',
     +                         NF_INT, 1, int ( xmiss ) )
      i = ivar + 5 * nvars
      iret = nf_put_att_text ( ncid, var_id(i), 'long_name', 20,
     +                  'QARTOD Location Test' )
      iret = nf_put_att_text ( ncid, var_id(i), 'short_name', 11,
     +                         cvar(ivar,6) )
      iret = nf_put_att_int ( ncid, var_id(i), '_FillValue',
     +                         NF_INT, 1, int ( xmiss ) )
      i = ivar + 6 * nvars
      iret = nf_put_att_text ( ncid, var_id(i), 'long_name', 23,
     +                  'QARTOD Gross Range Test' )
      iret = nf_put_att_text ( ncid, var_id(i), 'short_name', 11,
     +                         cvar(ivar,7) )
      iret = nf_put_att_int ( ncid, var_id(i), '_FillValue',
     +                         NF_INT, 1, int ( xmiss ) )
      i = ivar + 7 * nvars
      iret = nf_put_att_text ( ncid, var_id(i), 'long_name', 26,
     +                  'QARTOD Climatological Test' )
      iret = nf_put_att_text ( ncid, var_id(i), 'short_name', 11,
     +                         cvar(ivar,8) )
      iret = nf_put_att_int ( ncid, var_id(i), '_FillValue',
     +                         NF_INT, 1, int ( xmiss ) )
      i = ivar + 8 * nvars
      iret = nf_put_att_text ( ncid, var_id(i), 'long_name', 17,
     +                  'QARTOD Spike Test' )
      iret = nf_put_att_text ( ncid, var_id(i), 'short_name', 11,
     +                         cvar(ivar,9) )
      iret = nf_put_att_int ( ncid, var_id(i), '_FillValue',
     +                         NF_INT, 1, int ( xmiss ) )
      i = ivar + 9 * nvars
      iret = nf_put_att_text ( ncid, var_id(i), 'long_name', 26,
     +                  'QARTOD Rate of Change Test' )
      iret = nf_put_att_text ( ncid, var_id(i), 'short_name', 11,
     +                         cvar(ivar,10) )
      iret = nf_put_att_int ( ncid, var_id(i), '_FillValue',
     +                         NF_INT, 1, int ( xmiss ) )
      i = ivar + 10 * nvars
      iret = nf_put_att_text ( ncid, var_id(i), 'long_name', 21,
     +                  'QARTOD Flat Line Test' )
      iret = nf_put_att_text ( ncid, var_id(i), 'short_name', 11,
     +                         cvar(ivar,11) )
      iret = nf_put_att_int ( ncid, var_id(i), '_FillValue',
     +                         NF_INT, 1, int ( xmiss ) )
      i = ivar + 11 * nvars
      iret = nf_put_att_text ( ncid, var_id(i), 'long_name', 25,
     +                  'QARTOD Multi-Variate Test' )
      iret = nf_put_att_text ( ncid, var_id(i), 'short_name', 11,
     +                         cvar(ivar,12) )
      iret = nf_put_att_int ( ncid, var_id(i), '_FillValue',
     +                         NF_INT, 1, int ( xmiss ) )
      i = ivar + 12 * nvars
      iret = nf_put_att_text ( ncid, var_id(i), 'long_name', 29,
     +                  'QARTOD Attenuated Signal Test' )
      iret = nf_put_att_text ( ncid, var_id(i), 'short_name', 11,
     +                         cvar(ivar,13) )
      iret = nf_put_att_int ( ncid, var_id(i), '_FillValue',
     +                         NF_INT, 1, int ( xmiss ) )
      i = ivar + 13 * nvars
      iret = nf_put_att_text ( ncid, var_id(i), 'long_name', 20,
     +                  'QARTOD Neighbor Test' )
      iret = nf_put_att_text ( ncid, var_id(i), 'short_name', 11,
     +                         cvar(ivar,14) )
      iret = nf_put_att_int ( ncid, var_id(i), '_FillValue',
     +                         NF_INT, 1, int ( xmiss ) )
      i = ivar + 14 * nvars
      iret = nf_put_att_text ( ncid, var_id(i), 'long_name', 51,
     +    'QARTOD Decreasing Radiance, Irradiance and PAR Test' )
      iret = nf_put_att_text ( ncid, var_id(i), 'short_name', 11,
     +                         cvar(ivar,15) )
      iret = nf_put_att_int ( ncid, var_id(i), '_FillValue',
     +                         NF_INT, 1, int ( xmiss ) )
      i = ivar + 15 * nvars
      iret = nf_put_att_text ( ncid, var_id(i), 'long_name', 47,
     +        'QARTOD Photic Zone Limit for Rad/Irrad/PAR Test' )
      iret = nf_put_att_text ( ncid, var_id(i), 'short_name', 11,
     +                         cvar(ivar,16) )
      iret = nf_put_att_int ( ncid, var_id(i), '_FillValue',
     +                         NF_INT, 1, int ( xmiss ) )
      do 180 j = 1, nqc
         i = ivar + (j+2) * nvars
         iret = nf_put_att_text ( ncid, var_id(i), 'standard_name', 58,
     +    'mass_concentration_of_chlorophyll_in_sea_water status_flag' )
         iret = nf_put_att_int ( ncid, var_id(i), 'valid_range', nf_int,
     +                         2, iqc_rt_flag_rv )
         iret = nf_put_att_int ( ncid, var_id(i), '_FillValue',
     +                         NF_INT, 1, int ( xmiss ) )
         iret = nf_put_att_int ( ncid, var_id(i), 'flag_values', nf_int,
     +                         5, iqc_rt_flagv )
         iret = nf_put_att_text ( ncid, var_id(i), 'flag_meanings', 71,
     +                         qc_rt_flag_defv )
         iret = nf_put_att_text ( ncid, var_id(i), 'units', 1, '0' )
         iret = nf_put_att_text ( ncid, var_id(i), 'ioos_category', 7,
     +                         'Quality' )
180   continue

      ivar = 6
c turbidity adjusted following delayed-mode qc
      i = ivar + 0 * nvars
      iret = nf_put_att_text ( ncid, var_id(i), 'long_name', 21,
     +                         'Turbidity (processed)' )
      iret = nf_put_att_text ( ncid, var_id(i), 'standard_name', 19,
     +                         'sea_water_turbidity' )
      iret = nf_put_att_text ( ncid, var_id(i), 'short_name', 4,
     +                         'turb' )
      iret = nf_put_att_text ( ncid, var_id(i), 'units', 3,
     +                         'NTU')
      iret = nf_put_att_text ( ncid, var_id(i), 'ioos_category', 18,
     +                         'Optical Properties' )
      iret = nf_put_att_text ( ncid, var_id(i), 'coordinates', 29,
     +                         'time latitude longitude depth')
      range_val(1) = 0.0
      range_val(2) = 1000.0
      iret = nf_put_att_real ( ncid, var_id(i), 'valid_range',
     +                           NF_REAL, 2, range_val )
      iret = nf_put_att_real ( ncid, var_id(i), '_FillValue',
     +                         NF_REAL, 1, xmiss )
      iret = nf_put_att_text ( ncid, var_id(i), 'observation_type', 10,
     +                         'calculated')
      iret = nf_put_att_text ( ncid, var_id(i), 'ncei_name', 9,
     +                         'TURBIDITY' )
      iret = nf_put_att_real ( ncid, var_id(i), 'scale_factor', 
     +                         NF_REAL, 1, 1.0 )
      iret = nf_put_att_real ( ncid, var_id(i), 'add_offset', 
     +                         NF_REAL, 1, 0.0 )
      iret = nf_put_att_text ( ncid, var_id(i), 'grid_mapping',3,'crs' )
      iret = nf_put_att_text ( ncid, var_id(i), 'coverage_content_type',
     +                         19, 'physicalMeasurement' )
      iret = nf_put_att_text ( ncid, var_id(i), 'source', 1, ' ' )
      iret = nf_put_att_text ( ncid, var_id(i), 'references', 1, ' ' )
      iret = nf_put_att_text ( ncid, var_id(i), 'cell_methods', 57,
     +   'time: point longitude: point latitude: point depth: point' )
      iret = nf_put_att_text ( ncid,var_id(i),'ancillary_variables',275,
     + 'instrument1 platform1 turbidity_dm_qd turbidity_qc_gap turbidity
     +_qc_syn turbidity_qc_loc turbidity_qc_rng turbidity_qc_clm turbidi
     +ty_qc_spk turbidity_qc_rtc turbidity_qc_rtc turbidity_qc_flt turbi
     +dity_qc_mvr turbidity_qc_atn turbidity_qc_nbr turbidity_qc_crv tur
     +bidity_qc_din' )
      iret = nf_put_att_text ( ncid, var_id(i), 'platform', 9,
     +                         'platform1' )
      iret = nf_put_att_text ( ncid, var_id(i), 'instrument', 11,
     +                 'instrument1' )
      iret = nf_put_att_text ( ncid, var_id(i), 'comment', 1, ' ' )

c raw turbidity
      i = ivar + 1 * nvars
      iret = nf_put_att_text ( ncid, var_id(i), 'long_name', 15,
     +                         'Turbidity (raw)' )
      iret = nf_put_att_text ( ncid, var_id(i), 'standard_name', 19,
     +                         'sea_water_turbidity' )
      iret = nf_put_att_text ( ncid, var_id(i), 'short_name', 8,
     +                         'turb_raw' )
      iret = nf_put_att_text ( ncid, var_id(i), 'units', 3,
     +                         'NTU')
      iret = nf_put_att_text ( ncid, var_id(i), 'ioos_category', 18,
     +                         'Optical Properties' )
      iret = nf_put_att_text ( ncid, var_id(i), 'coordinates', 29,
     +                         'time latitude longitude depth')
      range_val(1) = 0.0
      range_val(2) = 1000.0
      iret = nf_put_att_real ( ncid, var_id(i), 'valid_range',
     +                           NF_REAL, 2, range_val )
      iret = nf_put_att_real ( ncid, var_id(i), '_FillValue',
     +                         NF_REAL, 1, xmiss )
      iret = nf_put_att_text ( ncid, var_id(i), 'observation_type', 10,
     +                         'calculated')
      iret = nf_put_att_text ( ncid, var_id(i), 'ncei_name', 9,
     +                         'TURBIDITY' )
      iret = nf_put_att_real ( ncid, var_id(i), 'scale_factor', 
     +                         NF_REAL, 1, 1.0 )
      iret = nf_put_att_real ( ncid, var_id(i), 'add_offset', 
     +                         NF_REAL, 1, 0.0 )
      iret = nf_put_att_text ( ncid, var_id(i), 'grid_mapping',3,'crs' )
      iret = nf_put_att_text ( ncid, var_id(i), 'coverage_content_type',
     +                         19, 'physicalMeasurement' )
      iret = nf_put_att_text ( ncid, var_id(i), 'source', 1, ' ' )
      iret = nf_put_att_text ( ncid, var_id(i), 'references', 1, ' ' )
      iret = nf_put_att_text ( ncid, var_id(i), 'cell_methods', 57,
     +   'time: point longitude: point latitude: point depth: point' )
      iret = nf_put_att_text ( ncid,var_id(i),'ancillary_variables',275,
     + 'instrument1 platform1 turbidity_dm_qd turbidity_qc_gap turbidity
     +_qc_syn turbidity_qc_loc turbidity_qc_rng turbidity_qc_clm turbidi
     +ty_qc_spk turbidity_qc_rtc turbidity_qc_rtc turbidity_qc_flt turbi
     +dity_qc_mvr turbidity_qc_atn turbidity_qc_nbr turbidity_qc_crv tur
     +bidity_qc_din' )
      iret = nf_put_att_text ( ncid, var_id(i), 'platform', 9,
     +                         'platform1' )
      iret = nf_put_att_text ( ncid, var_id(i), 'instrument', 11,
     +                 'instrument1' )
      iret = nf_put_att_text ( ncid, var_id(i), 'comment', 1, ' ' )

c turbidity delayed mode qc flag
      i = ivar + 2 * nvars
      iret = nf_put_att_text ( ncid, var_id(i), 'long_name', 41,
     +                   'Turbidity delayed-mode quality descriptor' )
      iret = nf_put_att_text ( ncid, var_id(i), 'short_name', 10,
     +                         'turb_dm_qd' )
      iret = nf_put_att_text ( ncid, var_id(i), 'standard_name', 31,
     +                         'sea_water_turbidity status_flag' )
      iret = nf_put_att_int ( ncid, var_id(i), 'valid_range', nf_int,
     +                         2, iqc_dm_flag_rv )
      iret = nf_put_att_int ( ncid, var_id(i), '_FillValue',
     +                         NF_INT, 1, int ( xmiss ) )
      iret = nf_put_att_int ( ncid, var_id(i), 'flag_values', nf_int,
     +                         6, iqc_dm_flagv )
      iret = nf_put_att_text ( ncid, var_id(i), 'flag_meanings', 113,
     +                         qc_dm_flag_defv )
      iret = nf_put_att_text ( ncid, var_id(i), 'units', 1, '0' )
      iret = nf_put_att_text ( ncid, var_id(i), 'ioos_category', 7,
     +                         'Quality' )

c turbidity QARTOD flags
      i = ivar + 3 * nvars
      iret = nf_put_att_text ( ncid, var_id(i), 'long_name', 15,
     +                  'QARTOD Gap Test' )
      iret = nf_put_att_text ( ncid, var_id(i), 'short_name', 11,
     +                         cvar(ivar,4) )
      iret = nf_put_att_int ( ncid, var_id(i), '_FillValue',
     +                         NF_INT, 1, int ( xmiss ) )
      i = ivar + 4 * nvars
      iret = nf_put_att_text ( ncid, var_id(i), 'long_name', 18,
     +                  'QARTOD Syntax Test' )
      iret = nf_put_att_text ( ncid, var_id(i), 'short_name', 11,
     +                         cvar(ivar,5) )
      iret = nf_put_att_int ( ncid, var_id(i), '_FillValue',
     +                         NF_INT, 1, int ( xmiss ) )
      i = ivar + 5 * nvars
      iret = nf_put_att_text ( ncid, var_id(i), 'long_name', 20,
     +                  'QARTOD Location Test' )
      iret = nf_put_att_text ( ncid, var_id(i), 'short_name', 11,
     +                         cvar(ivar,6) )
      iret = nf_put_att_int ( ncid, var_id(i), '_FillValue',
     +                         NF_INT, 1, int ( xmiss ) )
      i = ivar + 6 * nvars
      iret = nf_put_att_text ( ncid, var_id(i), 'long_name', 23,
     +                  'QARTOD Gross Range Test' )
      iret = nf_put_att_text ( ncid, var_id(i), 'short_name', 11,
     +                         cvar(ivar,7) )
      iret = nf_put_att_int ( ncid, var_id(i), '_FillValue',
     +                         NF_INT, 1, int ( xmiss ) )
      i = ivar + 7 * nvars
      iret = nf_put_att_text ( ncid, var_id(i), 'long_name', 26,
     +                  'QARTOD Climatological Test' )
      iret = nf_put_att_text ( ncid, var_id(i), 'short_name', 11,
     +                         cvar(ivar,8) )
      iret = nf_put_att_int ( ncid, var_id(i), '_FillValue',
     +                         NF_INT, 1, int ( xmiss ) )
      i = ivar + 8 * nvars
      iret = nf_put_att_text ( ncid, var_id(i), 'long_name', 17,
     +                  'QARTOD Spike Test' )
      iret = nf_put_att_text ( ncid, var_id(i), 'short_name', 11,
     +                         cvar(ivar,9) )
      iret = nf_put_att_int ( ncid, var_id(i), '_FillValue',
     +                         NF_INT, 1, int ( xmiss ) )
      i = ivar + 9 * nvars
      iret = nf_put_att_text ( ncid, var_id(i), 'long_name', 26,
     +                  'QARTOD Rate of Change Test' )
      iret = nf_put_att_text ( ncid, var_id(i), 'short_name', 11,
     +                         cvar(ivar,10) )
      iret = nf_put_att_int ( ncid, var_id(i), '_FillValue',
     +                         NF_INT, 1, int ( xmiss ) )
      i = ivar + 10 * nvars
      iret = nf_put_att_text ( ncid, var_id(i), 'long_name', 21,
     +                  'QARTOD Flat Line Test' )
      iret = nf_put_att_text ( ncid, var_id(i), 'short_name', 11,
     +                         cvar(ivar,11) )
      iret = nf_put_att_int ( ncid, var_id(i), '_FillValue',
     +                         NF_INT, 1, int ( xmiss ) )
      i = ivar + 11 * nvars
      iret = nf_put_att_text ( ncid, var_id(i), 'long_name', 25,
     +                  'QARTOD Multi-Variate Test' )
      iret = nf_put_att_text ( ncid, var_id(i), 'short_name', 11,
     +                         cvar(ivar,12) )
      iret = nf_put_att_int ( ncid, var_id(i), '_FillValue',
     +                         NF_INT, 1, int ( xmiss ) )
      i = ivar + 12 * nvars
      iret = nf_put_att_text ( ncid, var_id(i), 'long_name', 29,
     +                  'QARTOD Attenuated Signal Test' )
      iret = nf_put_att_text ( ncid, var_id(i), 'short_name', 11,
     +                         cvar(ivar,13) )
      iret = nf_put_att_int ( ncid, var_id(i), '_FillValue',
     +                         NF_INT, 1, int ( xmiss ) )
      i = ivar + 13 * nvars
      iret = nf_put_att_text ( ncid, var_id(i), 'long_name', 20,
     +                  'QARTOD Neighbor Test' )
      iret = nf_put_att_text ( ncid, var_id(i), 'short_name', 11,
     +                         cvar(ivar,14) )
      iret = nf_put_att_int ( ncid, var_id(i), '_FillValue',
     +                         NF_INT, 1, int ( xmiss ) )
      i = ivar + 14 * nvars
      iret = nf_put_att_text ( ncid, var_id(i), 'long_name', 51,
     +    'QARTOD Decreasing Radiance, Irradiance and PAR Test' )
      iret = nf_put_att_text ( ncid, var_id(i), 'short_name', 11,
     +                         cvar(ivar,15) )
      iret = nf_put_att_int ( ncid, var_id(i), '_FillValue',
     +                         NF_INT, 1, int ( xmiss ) )
      i = ivar + 15 * nvars
      iret = nf_put_att_text ( ncid, var_id(i), 'long_name', 47,
     +        'QARTOD Photic Zone Limit for Rad/Irrad/PAR Test' )
      iret = nf_put_att_text ( ncid, var_id(i), 'short_name', 11,
     +                         cvar(ivar,16) )
      iret = nf_put_att_int ( ncid, var_id(i), '_FillValue',
     +                         NF_INT, 1, int ( xmiss ) )
      do 190 j = 1, nqc
         i = ivar + (j+2) * nvars
         iret = nf_put_att_text ( ncid, var_id(i), 'standard_name', 31,
     +                         'sea_water_turbidity status_flag' )
         iret = nf_put_att_int ( ncid, var_id(i), 'valid_range', nf_int,
     +                         2, iqc_rt_flag_rv )
         iret = nf_put_att_int ( ncid, var_id(i), '_FillValue',
     +                         NF_INT, 1, int ( xmiss ) )
         iret = nf_put_att_int ( ncid, var_id(i), 'flag_values', nf_int,
     +                         5, iqc_rt_flagv )
         iret = nf_put_att_text ( ncid, var_id(i), 'flag_meanings', 71,
     +                         qc_rt_flag_defv )
         iret = nf_put_att_text ( ncid, var_id(i), 'units', 1, '0' )
         iret = nf_put_att_text ( ncid, var_id(i), 'ioos_category', 7,
     +                         'Quality' )
190   continue

      ivar = 7
c pH adjusted followind delayed mode qc
      i = ivar + 0 * nvars
      iret = nf_put_att_text ( ncid, var_id(i), 'long_name', 14,
     +                         'pH (processed)' )
      iret = nf_put_att_text ( ncid, var_id(i), 'standard_name', 36,
     +                         'sea_water_ph_reported_on_total_scale' )
      iret = nf_put_att_text ( ncid, var_id(i), 'short_name', 4,
     +                         'ph01' )
      iret = nf_put_att_text ( ncid, var_id(i), 'units', 1,
     +                         '1')
      iret = nf_put_att_text ( ncid, var_id(i), 'ioos_category', 5,
     +                         'Other' )
      iret = nf_put_att_text ( ncid, var_id(i), 'coordinates', 29,
     +                         'time latitude longitude depth')
      range_val(1) = 0.0
      range_val(2) = 14.0
      iret = nf_put_att_real ( ncid, var_id(i), 'valid_range',
     +                           NF_REAL, 2, range_val )
      iret = nf_put_att_real ( ncid, var_id(i), '_FillValue',
     +                         NF_REAL, 1, xmiss )
      iret = nf_put_att_text ( ncid, var_id(i), 'observation_type', 10,
     +                         'calculated')
      iret = nf_put_att_text ( ncid, var_id(i), 'ncei_name', 2,
     +                         'PH' )
      iret = nf_put_att_real ( ncid, var_id(i), 'scale_factor', 
     +                         NF_REAL, 1, 1.0 )
      iret = nf_put_att_real ( ncid, var_id(i), 'add_offset', 
     +                         NF_REAL, 1, 0.0 )
      iret = nf_put_att_text ( ncid, var_id(i), 'grid_mapping',3,'crs' )
      iret = nf_put_att_text ( ncid, var_id(i), 'coverage_content_type',
     +                         19, 'physicalMeasurement' )
      iret = nf_put_att_text ( ncid, var_id(i), 'source', 1, ' ' )
      iret = nf_put_att_text ( ncid, var_id(i), 'references', 1, ' ' )
      iret = nf_put_att_text ( ncid, var_id(i), 'cell_methods', 57,
     +   'time: point longitude: point latitude: point depth: point' )
      iret = nf_put_att_text ( ncid,var_id(i),'ancillary_variables',170,
     + 'instrument1 platform1 ph_dm_qd ph_qc_gap ph_qc_syn ph_qc_loc ph_
     +qc_rng ph_qc_clm ph_qc_spk ph_qc_rtc ph_qc_rtc ph_qc_flt ph_qc_mvr
     + ph_qc_atn ph_qc_nbr ph_qc_crv ph_qc_din' )
      iret = nf_put_att_text ( ncid, var_id(i), 'platform', 9,
     +                         'platform1' )
      iret = nf_put_att_text ( ncid, var_id(i), 'instrument', 11,
     +                 'instrument1' )
      iret = nf_put_att_text ( ncid, var_id(i), 'comment', 1, ' ' )

c raw pH
      i = ivar + 1 * nvars
      iret = nf_put_att_text ( ncid, var_id(i), 'long_name', 8,
     +                         'pH (raw)' )
      iret = nf_put_att_text ( ncid, var_id(i), 'standard_name', 36,
     +                         'sea_water_ph_reported_on_total_scale' )
      iret = nf_put_att_text ( ncid, var_id(i), 'short_name', 8,
     +                         'ph01_raw' )
      iret = nf_put_att_text ( ncid, var_id(i), 'units', 1,
     +                         '1')
      iret = nf_put_att_text ( ncid, var_id(i), 'ioos_category', 5,
     +                         'Other' )
      iret = nf_put_att_text ( ncid, var_id(i), 'coordinates', 29,
     +                         'time latitude longitude depth')
      range_val(1) = 0.0
      range_val(2) = 14.0
      iret = nf_put_att_real ( ncid, var_id(i), 'valid_range',
     +                           NF_REAL, 2, range_val )
      iret = nf_put_att_real ( ncid, var_id(i), '_FillValue',
     +                         NF_REAL, 1, xmiss )
      iret = nf_put_att_text ( ncid, var_id(i), 'observation_type', 8,
     +                         'measured' )
      iret = nf_put_att_text ( ncid, var_id(i), 'ncei_name', 2,
     +                         'PH' )
      iret = nf_put_att_real ( ncid, var_id(i), 'scale_factor', 
     +                         NF_REAL, 1, 1.0 )
      iret = nf_put_att_real ( ncid, var_id(i), 'add_offset', 
     +                         NF_REAL, 1, 0.0 )
      iret = nf_put_att_text ( ncid, var_id(i), 'grid_mapping',3,'crs' )
      iret = nf_put_att_text ( ncid, var_id(i), 'coverage_content_type',
     +                         19, 'physicalMeasurement' )
      iret = nf_put_att_text ( ncid, var_id(i), 'source', 1, ' ' )
      iret = nf_put_att_text ( ncid, var_id(i), 'references', 1, ' ' )
      iret = nf_put_att_text ( ncid, var_id(i), 'cell_methods', 57,
     +   'time: point longitude: point latitude: point depth: point' )
      iret = nf_put_att_text ( ncid,var_id(i),'ancillary_variables',170,
     + 'instrument1 platform1 ph_dm_qd ph_qc_gap ph_qc_syn ph_qc_loc ph_
     +qc_rng ph_qc_clm ph_qc_spk ph_qc_rtc ph_qc_rtc ph_qc_flt ph_qc_mvr
     + ph_qc_atn ph_qc_nbr ph_qc_crv ph_qc_din' )
      iret = nf_put_att_text ( ncid, var_id(i), 'platform', 9,
     +                         'platform1' )
      iret = nf_put_att_text ( ncid, var_id(i), 'instrument', 11,
     +                 'instrument1' )
      iret = nf_put_att_text ( ncid, var_id(i), 'comment', 1, ' ' )

c pH delayed mode qc flag
      i = ivar + 2 * nvars
      iret = nf_put_att_text ( ncid, var_id(i), 'long_name', 34,
     +                      'pH delayed-mode quality descriptor' )
      iret = nf_put_att_text ( ncid, var_id(i), 'short_name', 10,
     +                         'ph01_dm_qd' )
      iret = nf_put_att_text ( ncid, var_id(i), 'standard_name', 48,
     +              'sea_water_ph_reported_on_total_scale status_flag' )
      iret = nf_put_att_int ( ncid, var_id(i), 'valid_range', nf_int,
     +                         2, iqc_dm_flag_rv )
      iret = nf_put_att_int ( ncid, var_id(i), '_FillValue',
     +                         NF_INT, 1, int ( xmiss ) )
      iret = nf_put_att_int ( ncid, var_id(i), 'flag_values', nf_int,
     +                         6, iqc_dm_flagv )
      iret = nf_put_att_text ( ncid, var_id(i), 'flag_meanings', 113,
     +                         qc_dm_flag_defv )
      iret = nf_put_att_text ( ncid, var_id(i), 'units', 1, '0' )
      iret = nf_put_att_text ( ncid, var_id(i), 'ioos_category', 7,
     +                         'Quality' )
      
c pH QARTOD flags
      i = ivar + 3 * nvars
      iret = nf_put_att_text ( ncid, var_id(i), 'long_name', 15,
     +                  'QARTOD Gap Test' )
      iret = nf_put_att_text ( ncid, var_id(i), 'short_name', 11,
     +                         cvar(ivar,4) )
      iret = nf_put_att_int ( ncid, var_id(i), '_FillValue',
     +                         NF_INT, 1, int ( xmiss ) )
      i = ivar + 4 * nvars
      iret = nf_put_att_text ( ncid, var_id(i), 'long_name', 18,
     +                  'QARTOD Syntax Test' )
      iret = nf_put_att_text ( ncid, var_id(i), 'short_name', 11,
     +                         cvar(ivar,5) )
      iret = nf_put_att_int ( ncid, var_id(i), '_FillValue',
     +                         NF_INT, 1, int ( xmiss ) )
      i = ivar + 5 * nvars
      iret = nf_put_att_text ( ncid, var_id(i), 'long_name', 20,
     +                  'QARTOD Location Test' )
      iret = nf_put_att_text ( ncid, var_id(i), 'short_name', 11,
     +                         cvar(ivar,6) )
      iret = nf_put_att_int ( ncid, var_id(i), '_FillValue',
     +                         NF_INT, 1, int ( xmiss ) )
      i = ivar + 6 * nvars
      iret = nf_put_att_text ( ncid, var_id(i), 'long_name', 23,
     +                  'QARTOD Gross Range Test' )
      iret = nf_put_att_text ( ncid, var_id(i), 'short_name', 11,
     +                         cvar(ivar,7) )
      iret = nf_put_att_int ( ncid, var_id(i), '_FillValue',
     +                         NF_INT, 1, int ( xmiss ) )
      i = ivar + 7 * nvars
      iret = nf_put_att_text ( ncid, var_id(i), 'long_name', 26,
     +                  'QARTOD Climatological Test' )
      iret = nf_put_att_text ( ncid, var_id(i), 'short_name', 11,
     +                         cvar(ivar,8) )
      iret = nf_put_att_int ( ncid, var_id(i), '_FillValue',
     +                         NF_INT, 1, int ( xmiss ) )
      i = ivar + 8 * nvars
      iret = nf_put_att_text ( ncid, var_id(i), 'long_name', 17,
     +                  'QARTOD Spike Test' )
      iret = nf_put_att_text ( ncid, var_id(i), 'short_name', 11,
     +                         cvar(ivar,9) )
      iret = nf_put_att_int ( ncid, var_id(i), '_FillValue',
     +                         NF_INT, 1, int ( xmiss ) )
      i = ivar + 9 * nvars
      iret = nf_put_att_text ( ncid, var_id(i), 'long_name', 26,
     +                  'QARTOD Rate of Change Test' )
      iret = nf_put_att_text ( ncid, var_id(i), 'short_name', 11,
     +                         cvar(ivar,10) )
      iret = nf_put_att_int ( ncid, var_id(i), '_FillValue',
     +                         NF_INT, 1, int ( xmiss ) )
      i = ivar + 10 * nvars
      iret = nf_put_att_text ( ncid, var_id(i), 'long_name', 21,
     +                  'QARTOD Flat Line Test' )
      iret = nf_put_att_text ( ncid, var_id(i), 'short_name', 11,
     +                         cvar(ivar,11) )
      iret = nf_put_att_int ( ncid, var_id(i), '_FillValue',
     +                         NF_INT, 1, int ( xmiss ) )
      i = ivar + 11 * nvars
      iret = nf_put_att_text ( ncid, var_id(i), 'long_name', 25,
     +                  'QARTOD Multi-Variate Test' )
      iret = nf_put_att_text ( ncid, var_id(i), 'short_name', 11,
     +                         cvar(ivar,12) )
      iret = nf_put_att_int ( ncid, var_id(i), '_FillValue',
     +                         NF_INT, 1, int ( xmiss ) )
      i = ivar + 12 * nvars
      iret = nf_put_att_text ( ncid, var_id(i), 'long_name', 29,
     +                  'QARTOD Attenuated Signal Test' )
      iret = nf_put_att_text ( ncid, var_id(i), 'short_name', 11,
     +                         cvar(ivar,13) )
      iret = nf_put_att_int ( ncid, var_id(i), '_FillValue',
     +                         NF_INT, 1, int ( xmiss ) )
      i = ivar + 13 * nvars
      iret = nf_put_att_text ( ncid, var_id(i), 'long_name', 20,
     +                  'QARTOD Neighbor Test' )
      iret = nf_put_att_text ( ncid, var_id(i), 'short_name', 11,
     +                         cvar(ivar,14) )
      iret = nf_put_att_int ( ncid, var_id(i), '_FillValue',
     +                         NF_INT, 1, int ( xmiss ) )
      i = ivar + 14 * nvars
      iret = nf_put_att_text ( ncid, var_id(i), 'long_name', 27,
     +                  'QARTOD T/S Curve/Space Test' )
      iret = nf_put_att_text ( ncid, var_id(i), 'short_name', 11,
     +                         cvar(ivar,15) )
      iret = nf_put_att_int ( ncid, var_id(i), '_FillValue',
     +                         NF_INT, 1, int ( xmiss ) )
      i = ivar + 15 * nvars
      iret = nf_put_att_text ( ncid, var_id(i), 'long_name', 29,
     +                  'QARTOD Density Inversion Test' )
      iret = nf_put_att_text ( ncid, var_id(i), 'short_name', 11,
     +                         cvar(ivar,16) )
      iret = nf_put_att_int ( ncid, var_id(i), '_FillValue',
     +                         NF_INT, 1, int ( xmiss ) )
      do 200 j = 1, nqc
         i = ivar + (j+2) * nvars
         iret = nf_put_att_text ( ncid, var_id(i), 'standard_name', 48,
     +              'sea_water_ph_reported_on_total_scale status_flag' )
         iret = nf_put_att_int ( ncid, var_id(i), 'valid_range', nf_int,
     +                         2, iqc_rt_flag_rv )
         iret = nf_put_att_int ( ncid, var_id(i), '_FillValue',
     +                         NF_INT, 1, int ( xmiss ) )
         iret = nf_put_att_int ( ncid, var_id(i), 'flag_values', nf_int,
     +                         5, iqc_rt_flagv )
         iret = nf_put_att_text ( ncid, var_id(i), 'flag_meanings', 71,
     +                         qc_rt_flag_defv )
         iret = nf_put_att_text ( ncid, var_id(i), 'units', 1, '0' )
         iret = nf_put_att_text ( ncid, var_id(i), 'comment', 66, 
     +   'NOTE: QARTOD does not yet have tests for pH; this is a placeho
     +lder' )
         iret = nf_put_att_text ( ncid, var_id(i), 'ioos_category', 7,
     +                         'Quality' )
200   continue

c extra NCEI stuff
      iret = nf_put_att_text ( ncid, plt_id, 'long_name', 75,
     +                         seriesID )
      iret = nf_put_att_text ( ncid, plt_id, 'short_name', 5, code )
      iret = nf_put_att_int ( ncid, plt_id, '_FillValue',
     +                         NF_INT, 1, int(xmiss) )
      iret = nf_put_att_text ( ncid, plt_id, 'comment', 1,
     +                         ' ' )
      iret = nf_put_att_text ( ncid, plt_id, 'call_sign', 1, ' ' )
      iret = nf_put_att_text ( ncid, plt_id, 'ncei_code', 4, '3164' )
      iret = nf_put_att_text ( ncid, plt_id, 'wmo_code', 1, ' ' )
      iret = nf_put_att_text ( ncid, plt_id, 'imo_code', 1, ' ' )

      iret = nf_put_att_text ( ncid, ins_id, 'long_name', 45, inst )
      iret = nf_put_att_text ( ncid, ins_id, 'make_model', 45, inst )
      iret = nf_put_att_text ( ncid, ins_id, 'serial_number', 1, ' ' )
      iret = nf_put_att_text ( ncid, ins_id, 'calibration_date', 1, ' ')
      iret = nf_put_att_int ( ncid, ins_id, '_FillValue', 
     +                         NF_INT, 1, int ( xmiss ) )
      iret = nf_put_att_text ( ncid, ins_id, 'comment', 1, ' ' )

      iret = nf_put_att_text ( ncid, crs_id, 'long_name', 27, 
     +           'coordinate reference system' )
      iret = nf_put_att_text ( ncid, crs_id, 'grid_mapping_name', 18,
     +                         'latitude_longitude' )
      iret = nf_put_att_text ( ncid, crs_id, 'epsg_code', 9,
     +                         'EPSG:4326' )
      iret = nf_put_att_text ( ncid, crs_id, 'semi_major_axis', 10,
     +                         '6378137.0d' )
      iret = nf_put_att_text ( ncid, crs_id, 'inverse_flattening', 14,
     +                         '298.257223563d' )
      iret = nf_put_att_int ( ncid, crs_id, '_FillValue', 
     +                         NF_INT, 1, int ( xmiss ) )

c closeout definitions so we can write data

      iret = nf_enddef ( ncid )

c Write data
c Note here that the real-time qc flag (ivar1) is reset to ivar4
c with missing values = 9; delayed-mode qc flag (ivar2) is rest
C to ivar3 with missing balues = -9

      iret = nf_put_var_text ( ncid, ts_id, code )
      iret = nf_put_vara_real ( ncid, lev_id, 1, 1, zlev )
      iret = nf_put_vara_real ( ncid, lat_id, 1, 1, ylat )
      iret = nf_put_vara_real ( ncid, lon_id, 1, 1, xlon )
      iret = nf_put_vara_int ( ncid, ts_id, 1, 1, int ( xmiss ) )
      iret = nf_put_vara_int ( ncid, plt_id, 1, 1, int ( xmiss ) )
      iret = nf_put_vara_int ( ncid, ins_id, 1, 1, int ( xmiss ) )
      iret = nf_put_vara_int ( ncid, crs_id, 1, 1, int ( xmiss ) )
c Pre-2016 both DMQC and RTQC done (iqc_flag(4))
c      iret = nf_put_vara_int ( ncid, qcf_id, 1, 1, iqc_flag(4) )
c Post-2015 only RTQC done (iqc_flag(2))
      iret = nf_put_vara_int ( ncid, qcf_id, 1, 1, iqc_flag(2) )

c      istart(1) = 1
c      icount(1) = 1
c      istart(2) = 1
c      icount(2) = km2
      istart(1) = 1
      icount(1) = km2
      istart(2) = 1
      icount(2) = 1
      iret = nf_put_vara_real ( ncid, tim_id, istart, icount, xtime )

      do 210 i = 1, nvars
         do 220 k = 1, km2
            var1(k) = var_adj(i,k)
            var2(k) = var(i,k)
            ivar3(k) = ivar2(i,k)
            if ( var2(k).eq.xmiss ) ivar3(k) = -9
            do 230 j = 1, nqc
               ivar4(j,k) = ivar1(i,j,k)
               if ( var2(k).eq.xmiss ) ivar4(j,k) = 9
230         continue
            j = 4
            call qc_check ( i, var1(k), xmiss, iqvar )
            ivar4(j,k) = iqvar
220      continue
         iret = nf_put_vara_real ( ncid, var_id(i+nvars*0), 
     +                             istart, icount, var1 )
         iret = nf_put_vara_real ( ncid, var_id(i+nvars*1),
     +                             istart, icount, var2 )
         iret = nf_put_vara_int  ( ncid, var_id(i+nvars*2),
     +                             istart, icount, ivar3 )
         do 240 j = 1, nqc
            do 250 k = 1, km2
               ivar3(k) = ivar4(j,k)
250         continue
            iret = nf_put_vara_int ( ncid, var_id(i+nvars*(j+2)),
     +                             istart, icount, ivar3 )
240      continue
210   continue

c Close file

      iret = nf_close ( ncid )

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

c  ----------------------------------------------------------------------------
      subroutine qc_check ( i, varin, xmiss, iqvar )
c  ----------------------------------------------------------------------------
      real varin, xmiss
      integer i, iqvar

c Test 4: Gross range test
c   test has two ranges, manufacturer and operator
c     1: pass (value falls within both ranges)
c     2: not evaluated
c     3: suspect (value falls within manufacturer but not operator limits)
c     4: fail (value falls outside both ranges)
c     9: data are missing

c variable 1: temperature
      if ( i.eq.1 ) then
         iqvar = 1
         if ( varin.lt.15.0.or.varin.gt.30.0 ) iqvar = 3
         if ( varin.lt.-5.0.or.varin.gt.50.0 ) iqvar = 4
         if ( varin.eq.xmiss ) iqvar = 9
      endif
c variable 2: salinity
      if ( i.eq.2 ) then
         iqvar = 1
         if ( varin.lt.3.0.or.varin.gt.36.0 ) iqvar = 3
         if ( varin.lt.0.0.or.varin.gt.70.0 ) iqvar = 4
         if ( varin.eq.xmiss ) iqvar = 9
      endif
c variable 3: oxygen
      if ( i.eq.3 ) then
         iqvar = 1
         if ( varin.lt.0.7.or.varin.gt.1.8 ) iqvar = 3
         if ( varin.lt.0.0.or.varin.gt.5.0 ) iqvar = 4
         if ( varin.eq.xmiss ) iqvar = 9
      endif
c variable 4: oxygen concentration
      if ( i.eq.4 ) then
         iqvar = 1
         if ( varin.lt.0.0035.or.varin.gt.0.0125 ) iqvar = 3
         if ( varin.lt.0.0.or.varin.gt.0.05 ) iqvar = 4
         if ( varin.eq.xmiss ) iqvar = 9
      endif
c variable 5: chloro/flor
      if ( i.eq.5 ) then
         iqvar = 1
         if ( varin.lt.0.0.or.varin.gt.9.0E-5 ) iqvar = 3
         if ( varin.lt.1.0E-8.or.varin.gt.4.5E-4 ) iqvar = 4
         if ( varin.eq.xmiss ) iqvar = 9
      endif
c variable 6: turbidity
      if ( i.eq.6 ) then
         iqvar = 1
         if ( varin.lt.0.00.or.varin.gt.1000.0 ) iqvar = 3
         if ( varin.lt.0.01.or.varin.gt.120.0 ) iqvar = 4
         if ( varin.eq.xmiss ) iqvar = 9
      endif
c variable 7: pH
      if ( i.eq.7 ) then
         iqvar = 1
         if ( varin.lt.0.0.or.varin.gt.20.0 ) iqvar = 3
         if ( varin.lt.0.0.or.varin.gt.20.0 ) iqvar = 4
         if ( varin.eq.xmiss ) iqvar = 9
      endif

      return
      end
