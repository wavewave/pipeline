C   Program to read MadEvent files, shower, hadronize and decay
C   Stores the events in a stdhep file
C
C   Version 2.0, 09/10/2006
C   Includes matching analysis
C
C   Written by Johan Alwall, 23/02/2006
C
C   E-mail: alwall@fyma.ucl.ac.be

      PROGRAM PYTHIA

      IMPLICIT NONE

C...EXTERNAL statement links PYDATA on most machines.
      EXTERNAL PYDATA

C...The event record.
      INTEGER N,NPAD,K,MSTP,MSTI,MSEL,MSELPD,MSUB,KFIN,MINT
      DOUBLE PRECISION P,V,PARP,PARI,CKIN,VINT
      COMMON/PYJETS/N,NPAD,K(4000,5),P(4000,5),V(4000,5)
      COMMON/PYPARS/MSTP(200),PARP(200),MSTI(200),PARI(200)
      COMMON/PYSUBS/MSEL,MSELPD,MSUB(500),KFIN(2,-40:40),CKIN(200)
      COMMON/PYINT1/MINT(400),VINT(400)
      INTEGER MIMX,JSMX,KFLAMX,KFLCMX,KFBEAM,NISGEN
      DOUBLE PRECISION PT2MX,PT2AMX,ZMX,RM2CMX,Q2BMX,PHIMX
      COMMON/PYISMX/MIMX,JSMX,KFLAMX,KFLCMX,KFBEAM(2),NISGEN(2,240),
     &     PT2MX,PT2AMX,ZMX,RM2CMX,Q2BMX,PHIMX

C...User process initialization commonblock.
      INTEGER MAXPUP
      PARAMETER (MAXPUP=100)
      INTEGER IDBMUP,PDFGUP,PDFSUP,IDWTUP,NPRUP,LPRUP
      DOUBLE PRECISION EBMUP,XSECUP,XERRUP,XMAXUP
      COMMON/HEPRUP/IDBMUP(2),EBMUP(2),PDFGUP(2),PDFSUP(2),
     &   IDWTUP,NPRUP,XSECUP(MAXPUP),XERRUP(MAXPUP),XMAXUP(MAXPUP),
     &   LPRUP(MAXPUP)
C...User process event common block.
      INTEGER MAXNUP
      PARAMETER (MAXNUP=500)
      INTEGER NUP,IDPRUP,IDUP,ISTUP,MOTHUP,ICOLUP
      DOUBLE PRECISION XWGTUP,SCALUP,AQEDUP,AQCDUP,PUP,VTIMUP,SPINUP
      COMMON/HEPEUP/NUP,IDPRUP,XWGTUP,SCALUP,AQEDUP,AQCDUP,IDUP(MAXNUP),
     &   ISTUP(MAXNUP),MOTHUP(2,MAXNUP),ICOLUP(2,MAXNUP),PUP(5,MAXNUP),
     &   VTIMUP(MAXNUP),SPINUP(MAXNUP)

C...Extra commonblock to transfer run info.
      INTEGER LNHIN,LNHOUT,MSCAL,IEVNT,ICKKW,ISCALE
      COMMON/UPPRIV/LNHIN,LNHOUT,MSCAL,IEVNT,ICKKW,ISCALE

C...Commonblock to transfer event-by-event matching info
      INTEGER NLJETS,IEXC,Ifile
      DOUBLE PRECISION PTCLUS
      COMMON/MEMAEV/PTCLUS(20),NLJETS,IEXC,Ifile
      DATA Ifile/0/
C...Inputs for the matching algorithm
      double precision etcjet,rclmax,etaclmax,qcut,clfact
      integer maxjets,minjets,iexcfile,ktsche,mektsc,nexcres,excres(30)
      integer nqmatch,nexcproc,iexcproc(MAXPUP),iexcval(MAXPUP)
      logical nosingrad,showerkt,jetprocs
      common/MEMAIN/etcjet,rclmax,etaclmax,qcut,clfact,
     $   maxjets,minjets,iexcfile,ktsche,mektsc,nexcres,excres,
     $   nqmatch,nexcproc,iexcproc,iexcval,nosingrad,showerkt,jetprocs

C...Inputs for jet clustering and Pythia run
C...IJET=1(getjet)/2(ktclus)
C...NITER=-1 means all available events
C...Inputs for jet clustering and Pythia run
      INTEGER NITER,NFILES,PIDCUT(2,10),PIDCUTN(2,10),NPIDCUT
      COMMON/JETPAR/NITER,NFILES,PIDCUT,PIDCUTN,NPIDCUT
      DATA NITER,NFILES/-1,0/
      DATA PIDCUT,PIDCUTN,NPIDCUT/41*0/

C...HBOOK parameters
c      character*72 FHBOOK
c      INTEGER NWPAWC,lrec,istat,icycle
c      PARAMETER (NWPAWC = 500000)
c      REAL HMEMOR
c      COMMON /PAWC/ HMEMOR(NWPAWC)
c      REAL HSUM
      INTEGER nvarev,nvar2
      PARAMETER (nvarev=57,nvar2=6)
      CHARACTER*8 htit(nvarev),htit2(nvar2)
      DATA htit/'Npart','Qjet1','Qjet2','Qjet3','Qjet4',
     $   'Ptcjet1','Ptcjet2','Ptcjet3','Ptcjet4',
     $   'Etacjet1','Etacjet2','Etacjet3','Etacjet4',
     $   'Phicjet1','Phicjet2','Phicjet3','Phicjet4',
     $   'Ptjet1','Ptjet2','Ptjet3','Ptjet4',
     $   'Etajet1','Etajet2','Etajet3','Etajet4',
     $   'Phijet1','Phijet2','Phijet3','Phijet4',
     $   'Idres1','Ptres1','Etares1','Phires1',
     $   'Idres2','Ptres2','Etares2','Phires2',
     $   'Ptlep1','Etmiss','Htjets',
     $   'Ptb','Etab','Ptbbar','Etabbar','Ptbj','Etabj',
     $   'Qpar1','Qpar2','Qpar3','Qpar4',
     $   'Ptpar1','Ptpar2','Ptpar3','Ptpar4',
     $   'Ncjets','Njets','Nfile'/
      DATA htit2/'Npart','Qjet1','Qjet2','Qjet3','Qjet4','Nfile'/
      REAL*4 varev(nvarev)
      COMMON/HISTDAT/varev
      CHARACTER*8 ftit(1)
      DATA ftit/'Xsecfact'/
      REAL*4 fvar(1)

C...Variables for the kT-clustering
      INTEGER NMAX
      PARAMETER (NMAX=512)
      INTEGER NN,IHEP,I,NSUB,JET(NMAX),NCJET
      DOUBLE PRECISION ECUT,YCUT,RAD
      DOUBLE PRECISION PCJET(4,NMAX),Y(NMAX),PP(4,NMAX),ETJETC(NMAX)
C...HEPEVT commonblock.
      INTEGER NMXHEP,NEVHEP,NHEP,ISTHEP,IDHEP,JMOHEP,JDAHEP
      DOUBLE PRECISION PHEP,VHEP
      PARAMETER (NMXHEP=4000)
      COMMON/HEPEVT/NEVHEP,NHEP,ISTHEP(NMXHEP),IDHEP(NMXHEP),
     &JMOHEP(2,NMXHEP),JDAHEP(2,NMXHEP),PHEP(5,NMXHEP),VHEP(4,NMXHEP)
      SAVE /HEPEVT/
C...GETJET commonblocks
      INTEGER MNCY,MNCPHI,NCY,NCPHI,NJMAX,JETNO,NJJET
      DOUBLE PRECISION YCMIN,YCMAX,DELY,DELPHI,ET,STHCAL,CTHCAL,CPHCAL,
     &  SPHCAL,PJJET,ETJETJ
      PARAMETER (MNCY=200)
      PARAMETER (MNCPHI=200)
      COMMON/CALOR/DELY,DELPHI,ET(MNCY,MNCPHI),
     $CTHCAL(MNCY),STHCAL(MNCY),CPHCAL(MNCPHI),SPHCAL(MNCPHI),
     $YCMIN,YCMAX,NCY,NCPHI
      DATA YCMIN,YCMAX/-2.5d0,2.5d0/
      DATA NCY,NCPHI/50,60/
      PARAMETER (NJMAX=500)
      COMMON/GETCOM/PJJET(4,NJMAX),ETJETJ(NJMAX),JETNO(MNCY,MNCPHI),
     $NJJET

C   Local variables
      INTEGER IEV,istrstd,lok,IEXCSAVE,Ifstart
      CHARACTER*5 CGIVE,BEAM(2)
      CHARACTER*30 CGIVE0
      CHARACTER*80 pythia_card,output_file,input_file
      DOUBLE PRECISION Ptres1,Etares1,Phires1,Ptres2,Etares2,Phires2
      DOUBLE PRECISION Ptlep1,Etmiss,PT,AETA,PTJET(NMAX),ETA
      DOUBLE PRECISION Pxmiss,Pymiss,ETACJET(NJMAX),ETAJJET(NJMAX)
      DOUBLE PRECISION Ptb,Etab,Phib,Ptbbar,Etabbar,Phibbar,Ptbj,Etabj
      DOUBLE PRECISION PT2NEW,DUM1,DUM2
      INTEGER JS,IFAIL
      INTEGER J,KSORTJ(NMAX),KSORTC(NMAX),ib,ibbar,idres1,idres2,IMOT
      INTEGER NFOUND(10)
      LOGICAL internal
      DATA internal/.false./

C...Set pythia input card and STDHEP output file
      pythia_card='../Cards/pythia_card.dat'
      output_file='pythia_events.hep'

C...Set Pythia output to lnhout
      WRITE(CGIVE,'(I5)') LNHOUT
      CGIVE0='MSTU(11)='//CGIVE
      CALL PYGIVE(CGIVE0)

C...Set pi0 stable to trim event listings.
c      CALL PYGIVE('MDCY(C111,1)=0')
c      CALL PYGIVE('MDCY(C211,1)=0')

C...Set tau stable to use tauola for decay
      CALL PYGIVE('MDCY(C15,1)=0')

C....Choose decay modes. Cross section is rescaled in BRSUPP for Z, W, t
C....Only electronic decay of W and Z

c      DO IPART=1,2
c        IF(IPART.EQ.1)IW = PYCOMP(24)
c        IF(IPART.EQ.2)IW = PYCOMP(23)
c      DO IDC = MDCY(IW,2), MDCY(IW,2) + MDCY(IW,3) - 1
c        IF (MDME(IDC,1).GT.0) THEN
c          IF (IABS(KFDP(IDC,1)).NE.11.AND.IABS(KFDP(IDC,2)).NE.11)THEN
c            MDME(IDC,1)=0
c          ENDIF
c        ENDIF
c      ENDDO
c      ENDDO

C...Set MSEL=0 to switch off all Pythia processes
      CALL PYGIVE('MSEL=0')
      
C...Set PARP(67)=1 as standard (can be changed in the pythia_card.dat)
c     CALL PYGIVE('PARP(67)=1')

C...Read pythia input card
      PRINT *
      PRINT *,'Reading pythia input card'
      CALL RPYCARD(pythia_card)

C...Check if any internal processes switched on, run internally
      IF(MSEL.NE.0) internal=.true.
      DO I=1,500
        IF(MSUB(I).NE.0) internal=.true.
      ENDDO

C...Format for reading lines.
      OPEN (10, FILE='events.tree')
      OPEN (20, FILE='xsecs.tree')
      OPEN (15, FILE='beforeveto.tree')
      
C...Print root tree event file header
      WRITE(CGIVE0,'(a,I2,a)') '('//char(39)//'# '//char(39)//',',
     $   nvarev-1,'(a,'//char(39)//':'//char(39)//'),a)'
      WRITE(10,'(a)') '# File with ntuple events with the variables:'
      WRITE(10,CGIVE0) (htit(I)(1:len_trim(htit(I))),I=1,nvarev)

      WRITE(CGIVE0,'(a,I2,a)') '('//char(39)//'# '//char(39)//',',
     $   nvar2-1,'(a,'//char(39)//':'//char(39)//'),a)'
      WRITE(15,'(a)')
     $   '# File with before-veto ntuple with the variables:'
      WRITE(15,CGIVE0) (htit2(I)(1:len_trim(htit2(I))),I=1,nvar2)

C...Print root tree cross-section file header
      WRITE(20,'(a)')
     $   '# File with file cross-section info with variables:'
      WRITE(20,'(a)') '# '//ftit(1)

c      IF(ijet.eq.1) write(*,*)'Using GETJET for jet finding'
c      IF(ijet.eq.2) write(*,*)'Using KTCLUS for jet finding'
      
      IEXCSAVE=IEXCFILE
      
      Ifstart=Ifile
      if(Ifstart.eq.0) Ifstart=1

      DO Ifile=Ifstart,MAX(Ifstart,nfiles)

      IF(nfiles.eq.0)THEN
C   initialize HEP logical units
        input_file='unweighted_events.lhe'
      ELSE
        WRITE(*,*) 'Enter name for file ',Ifile
        READ(*,'(a)') input_file
        IF(Ifile.lt.nfiles)THEN
          IEXCFILE=1
        ELSE
          IEXCFILE=IEXCSAVE
        ENDIF
      ENDIF
      OPEN (LNHIN, FILE=input_file, ERR=90 )
      WRITE(*,*)'Opened file ',input_file
      IF(IEXCFILE.EQ.1)THEN
        WRITE(*,*) 'Running as exclusive sample'
      else
        WRITE(*,*) 'Running as inclusive sample (highest mult.)'
      endif
C...Initialize with external process.
      PRINT *
      IF(internal) THEN
        PRINT *,'Initiating pythia with internal processes only'
c     Check if event file opened
        DO I=1,2
          beam(I)=' '
          EBMUP(I)=0d0
          IDBMUP(I)=0
        ENDDO
        READ(LNHIN,'(a)',END=10) CGIVE0
c     Yes, call UPINIT to get energy and beam types
        CALL UPINIT()
        DO I=1,2
          IF(IDBMUP(I).EQ.2212) beam(I)='p+'
          IF(IDBMUP(I).EQ.-2212) beam(I)='p-'
          IF(IDBMUP(I).EQ.11) beam(I)='e-'
          IF(IDBMUP(I).EQ.-11) beam(I)='e+'
        ENDDO
c     Otherwise, set beams and energy to LHC
 10     DO I=1,2
          IF(beam(I).EQ.' ') beam(I)='p+'
          IF(EBMUP(I).EQ.0d0) EBMUP(I)=7d3
          ktsche=4313
        ENDDO
        IF(NITER.LT.0) NITER=1000
c        IF(ktpysc.eq.0)THEN
c          IF(beam(1).eq.'e-'.AND.beam(2).eq.'e+'.OR.
c     $       beam(1).eq.'e+'.AND.beam(2).eq.'e-')THEN
c            ktpysc=1
c          ELSE
c            ktpysc=4313
c          ENDIF
c        ENDIF
        CALL PYINIT('CMS',beam(1),beam(2),EBMUP(1)+EBMUP(2))
      ELSE
        PRINT *,'Initiating pythia with external process'
        CALL PYINIT('USER',' ',' ',0D0)
      ENDIF
      PRINT *,'Initialisation done'
      PRINT *

c      CALL PYLIST(12)

      IF(NITER.LT.0)THEN
        PRINT *,'Generating all events'
      ELSE
        PRINT *,'Generating up to ',NITER,' events'
      ENDIF

C...Open STDHEP output file
      if(Ifile.eq.Ifstart)then
        call stdxwinit(output_file,'PYTHIA file',NITER,istrstd,lok)
        if (lok.ne.0) then
          print *,'Failed to open output file ',output_file,
     $       '. Quitting!'
          stop
        endif
C   Store Pythia run info in STDHEP common block
        call stdflpyxsec(NITER)
C   Write STDHEP begin run record
        call stdxwrt(100,istrstd,lok)
        if (lok.ne.0) then
          print '('' Write failure on'',
     .       '' begin run record: '',i5)',lok
          stop
        endif

        PRINT *,'stdhep initialized'

C...Initiate tauola
        call pdgrdtb
        call tauola_init(LNHOUT)
      endif

      PRINT *,'Starting event loop'

C...Event loop
      IEV=0
      DO 130 WHILE(IEV.LT.NITER.OR.NITER.LT.0)
C...Get event
        CALL PYEVNT
C...If event generation failed, quit loop
        IF(MSTI(51).EQ.1) THEN
          GOTO 135 
        ENDIF
C...If showering or hadronization failed, go to next event
        IF(MINT(51).GT.0) THEN
           cycle
        ENDIF
        IEV=IEV+1

C...Check for pids to cut on
        IF(npidcut.gt.0)THEN
          DO I=1,npidcut
            NFOUND(I)=0
          ENDDO
          DO I=7,N
            IF(K(I,1).LT.20) GOTO 121
            DO J=1,npidcut
              IF(IABS(K(I,2)).GE.pidcut(1,J).AND.
     $           IABS(K(I,2)).LE.pidcut(2,J)) NFOUND(J)=NFOUND(J)+1
            ENDDO
          ENDDO
 121      DO I=1,npidcut
            IF(NFOUND(I).LT.pidcutn(1,I).OR.
     $         NFOUND(I).GT.pidcutn(2,I)) GOTO 130
          ENDDO
        ENDIF

C...Fill hbook ntuple with jet rates
        DO I=1,nvarev
           if(i.lt.47.or.i.gt.54) varev(I)=-1
        ENDDO

        varev(1)=NLJETS
        NCJET=0
        NJJET=0
        varev(6)=VINT(360)
        varev(7)=max(VINT(360),VINT(358))
        IF(ICKKW.EQ.2.AND.NLJETS.EQ.MAXJETS)THEN
          varev(6)=SCALUP
          varev(7)=SCALUP
        ELSE IF(NLJETS.EQ.MAXJETS.AND.MAXJETS.GT.0)THEN
          varev(6)=PTCLUS(1)
          varev(7)=PTCLUS(1)
        ELSE IF(ickkw.lt.2.and.mektsc.eq.1) THEN
          varev(6)=VINT(357)
          varev(7)=max(VINT(357),VINT(358))
        ENDIF

C     Test using the Pythia Sudakovs myself. Get max splitting from the
C     two sides
        PT2MX=0d0
c        CALL PYEVOL(0,15d0**2,0d0)
c        DO 220 JS=1,2
c          MINT(30)=JS
c          CALL PYPTIS(0,15d0**2,1d0,PT2NEW,IFAIL)
c          IF (MINT(51).NE.0) THEN
c            PRINT *,'Error calling PYPTIS'
c            STOP
c          ENDIF
c 220    CONTINUE

        varev(8)=SQRT(PT2MX)
        varev(55)=NCJET
        varev(56)=NJJET
        varev(57)=Ifile
C...Write event into ntuple 
        WRITE(10,4001) (varev(I),I=1,nvarev)

C...Convert event to STDHEP format
        call lunhep(1)

C...Decay taus using tauola
        call tauola_scan

C...Set event id to the event number in the event file
        NEVHEP=IEVNT
        IF(internal) NEVHEP=IEV

C...Write event to STDHEP output file
        call stdxwrt(1,istrstd,lok)
        if (lok.ne.0) then
           print '('' Failure writing event '',i5)',lok
        endif

C...Print first 10 events

 125    IF(IEV.LE.10) THEN
          PRINT *
          PRINT *,'Event number: ',IEV
          CALL PYLIST(7)
          CALL PYLIST(2)
          IF(ickkw.gt.0) PRINT *,NLJETS,'-jet event (parton level)'
          PRINT *,'SCALUP: ',SCALUP
          PRINT *,'PS SCALE: ',VINT(55)
          call heplst(1)
          CALL FLUSH()
        ENDIF

        IF(MOD(IEV,10000).EQ.0)THEN
          CALL PYSTAT(1)
C...Save hbook histograms and ntuples to file
c          CALL HROUT(0,icycle,'T')
c          PRINT *, 'Histograms saved to ',FHBOOK
          CALL FLUSH()
        ENDIF

 130  CONTINUE

 135  CALL PYSTAT(1)
      WRITE(*,*) 'Cross section (pb): ',PARI(1)*1d9

      CLOSE (LNHIN)

C...Save conversion factor per event (total xsec/number of events)
C...(for unweighted events)
      fvar(1)=PARI(2)*1d9
c      CALL HFN(20,fvar)
C...Write into ntuple 
      WRITE(20,4001) fvar(1)      

      ENDDO ! Ifile=1,MAX(1,nfiles)

C...Finalize STDHEP file
      call stdflpyxsec(IEV)
      call stdxwrt(200,istrstd,lok) ! write STDHEP end run record
      if (lok.ne.0) then
        print '('' Write failure on'',
     .     '' end run record: '',i5)',lok
      endif
      call stdxend(istrstd)


C...Save hbook histograms and ntuples to file
c      CALL HROUT(0,icycle,'T')
c      PRINT *, 'Histograms saved to ',FHBOOK
c      CALL FLUSH()

      RETURN
 4000 FORMAT(//A15,A1)
 4001 FORMAT(60E15.6)
 999  WRITE(*,*) 'Error in one of the KTCLUS routines'
      STOP
 90   WRITE(*,*) 'Error: Could not open MadEvent event file'
      WRITE(*,*) 'Quitting...'
      END
      
C************************************************

      SUBROUTINE RPYCARD(pythia_card)

      IMPLICIT NONE
      
C...Commonblock to set PDF library path
      CHARACTER*132 LHAPATH
      COMMON/LHAPDFC/LHAPATH
      SAVE /LHAPDFC/

C...Extra commonblock to transfer run info.
      INTEGER LNHIN,LNHOUT,MSCAL,IEVNT,ICKKW,ISCALE
      COMMON/UPPRIV/LNHIN,LNHOUT,MSCAL,IEVNT,ICKKW,ISCALE

C...Inputs for jet clustering and Pythia run
      INTEGER NITER,NFILES,PIDCUT(2,10),PIDCUTN(2,10),NPIDCUT
      COMMON/JETPAR/NITER,NFILES,PIDCUT,PIDCUTN,NPIDCUT

C...GETJET commonblocks
      INTEGER MNCY,MNCPHI,NCY,NCPHI,NJMAX,JETNO,NJJET
      DOUBLE PRECISION YCMIN,YCMAX,DELY,DELPHI,ET,STHCAL,CTHCAL,CPHCAL,
     &  SPHCAL,PJJET,ETJETJ
      PARAMETER (MNCY=200)
      PARAMETER (MNCPHI=200)
      COMMON/CALOR/DELY,DELPHI,ET(MNCY,MNCPHI),
     $CTHCAL(MNCY),STHCAL(MNCY),CPHCAL(MNCPHI),SPHCAL(MNCPHI),
     $YCMIN,YCMAX,NCY,NCPHI

C...Commonblock to transfer event-by-event matching info
      INTEGER NLJETS,IEXC,Ifile
      DOUBLE PRECISION PTCLUS
      COMMON/MEMAEV/PTCLUS(20),NLJETS,IEXC,Ifile

C...Inputs for the matching algorithm
      INTEGER MAXPUP
      PARAMETER (MAXPUP=100)
      double precision etcjet,rclmax,etaclmax,qcut,clfact
      integer maxjets,minjets,iexcfile,ktsche,mektsc,nexcres,excres(30)
      integer nqmatch,nexcproc,iexcproc(MAXPUP),iexcval(MAXPUP)
      logical nosingrad,showerkt,jetprocs
      common/MEMAIN/etcjet,rclmax,etaclmax,qcut,clfact,
     $   maxjets,minjets,iexcfile,ktsche,mektsc,nexcres,excres,
     $   nqmatch,nexcproc,iexcproc,iexcval,nosingrad,showerkt,jetprocs

      INTEGER ios,i
      CHARACTER*80 pythia_card
      CHARACTER*132 line
      
      if (pythia_card.ne.' ') then
        open (85, file=pythia_card, status='old',
     .     form='formatted')    ! removed for Linux: readonly)
        ios = 0
        do while(ios.eq.0)
          read(85,fmt='(a)',iostat=ios) line
          if (ios.eq.0) then
             call remove_comments(line,132)
             if(index(line,'lhapath').ne.0.or.
     $           index(line,'LHAPATH').ne.0) then
                LHAPATH=line(index(line,'=')+1:)
                write(*,*) 'Set LHAPATH to '//LHAPATH
                cycle
             endif
             call downcase_line(line,132)
             if(index(line,'psscale').ne.0) then
                READ(line(index(line,'=')+1:),*,err=100) MSCAL
                write(*,*) 'Set scale choice to ',MSCAL
                if(MSCAL.eq.0) write(*,*) ' (fixed scale)'
                if(MSCAL.gt.0) write(*,*) ' (Herwig scale)'
             else if(index(line,'iexcfile').ne.0) then
                READ(line(index(line,'=')+1:),*,err=100) IEXCFILE
                if(IEXCFILE.eq.0)
     $             write(*,*) ' Assuming inclusive sample ',
     $             '(IEXCFILE=0)'
                if(IEXCFILE.gt.0)
     $             write(*,*) ' Assuming exclusive sample',
     $             '(IEXCFILE=',IEXCFILE,')'
             else if(index(line,'qcut').ne.0) then
                READ(line(index(line,'=')+1:),*,err=100) qcut
                write(*,*) 'Read Qcut   = ',qcut
             else if(index(line,'etcjet').ne.0) then
                READ(line(index(line,'=')+1:),*,err=100) etcjet
                write(*,*) 'Read Etcjet   = ',etcjet
             else if(index(line,'clfact').ne.0) then
                READ(line(index(line,'=')+1:),*,err=100) clfact
                write(*,*) 'Read clfact = ',clfact
             else if(index(line,'ncy').ne.0) then
                READ(line(index(line,'=')+1:),*,err=100) ncy
                write(*,*) 'Read NCY = ',ncy
             else if(index(line,'ncphi').ne.0) then
                READ(line(index(line,'=')+1:),*,err=100) ncphi
                write(*,*) 'Read NCPHI = ',ncphi
             else if(index(line,'niter').ne.0) then
                READ(line(index(line,'=')+1:),*,err=100) niter
                write(*,*) 'Read Niter = ',niter
             else if(index(line,'nfiles').ne.0) then
                READ(line(index(line,'=')+1:),*,err=100) nfiles
                write(*,*) 'Read Nfiles = ',nfiles
             else if(index(line,'ifile').ne.0) then
                READ(line(index(line,'=')+1:),*,err=100) ifile
                write(*,*) 'Read Ifile = ',ifile
             else if(index(line,'ktsche').ne.0) then
                READ(line(index(line,'=')+1:),*,err=100) ktsche
                write(*,*) 'Read Ktsche = ',ktsche
             else if(index(line,'iscale').ne.0) then
                READ(line(index(line,'=')+1:),*,err=100) ISCALE
                write(*,*) 'Read ISCALE = ',ISCALE
             else if(index(line,'maxjets').ne.0) then
                READ(line(index(line,'=')+1:),*,err=100) MAXJETS
                write(*,*) 'Read MAXJETS = ',MAXJETS
             else if(index(line,'minjets').ne.0) then
                READ(line(index(line,'=')+1:),*,err=100) MINJETS
                write(*,*) 'Read MINJETS = ',MINJETS
             else if(index(line,'excres').ne.0) then
                if(nexcres.ge.30)then
                   write(*,*) 'Too many excluded resonances.'
                   cycle
                endif
                nexcres=nexcres+1
                READ(line(index(line,'=')+1:),*,err=100) EXCRES(nexcres)
                write(*,*) 'Read EXCRES = ',EXCRES(nexcres)
             else if(index(line,'nosingrad').ne.0) then
                READ(line(index(line,'=')+1:),*,err=100) nosingrad
                write(*,*) 'Read nosingrad = ',nosingrad
             else if(index(line,'showerkt').ne.0) then
                READ(line(index(line,'=')+1:),*,err=100) showerkt
                write(*,*) 'Read showerkt = ',showerkt
             else if(index(line,'nqmatch').ne.0) then
                READ(line(index(line,'=')+1:),*,err=100) nqmatch
                write(*,*) 'Read nqmatch = ',nqmatch
             else if(index(line,'iexcproc').ne.0) then
                nexcproc=nexcproc+1
                if(nexcproc.le.MAXPUP) then
                   READ(line(index(line,'(')+1:),*,err=100)
     $                  iexcproc(nexcproc)
                   READ(line(index(line,'=')+1:),*,err=100)
     $                  iexcval(nexcproc)
                   write(*,*) 'Read iexcproc(',nexcproc,')=',
     $                  iexcproc(nexcproc),' and iexcval=',
     $                  iexcval(nexcproc)
                else
                   write(*,*) 'Too many iexcproc definitions.'
                endif
             else if(index(line,'pidcut').ne.0) then
                npidcut=npidcut+1
                if(npidcut.le.10) then
                   if(index(line,'-').gt.0)then
                     READ(line(index(line,'(')+1:index(line,'-')-1),
     $                  *,err=100) pidcut(1,npidcut)
                     READ(line(index(line,'-')+1:),*,err=100)
     $                  pidcut(2,npidcut)
                   else
                     READ(line(index(line,'(')+1:),*,err=100)
     $                  pidcut(1,npidcut)
                     pidcut(2,npidcut)=pidcut(1,npidcut)
                   endif
                   READ(line(index(line,'=')+1:),*,err=100)
     $                  pidcutn(1,npidcut),pidcutn(2,npidcut)
                   write(*,*) 'Read pidcut(',npidcut,')=',
     $                pidcut(1,npidcut),'-',pidcut(2,npidcut),
     $                ' with min=',pidcutn(1,npidcut),
     $                ' and max=',pidcutn(2,npidcut)
                else
                   write(*,*) 'Too many iexcproc definitions.'
                endif
             else if(line.ne.' ') then
                call pygive(line)
             endif
          endif
          cycle
 100      write(*,*) 'Failed to read line: ',line(1:len_trim(line))
        enddo
        close(85)
      endif
      
      RETURN
      END
      
C************************************************

      subroutine remove_comments(line,len)

      implicit none
      character*(*) line
      integer i,len

      logical comment

      comment = .false.
      if(line(1:1).eq.'#') comment = .true.
      do i=1,len
        if(line(i:i).eq.'!') comment = .true.
        if(comment) line(i:i) = ' ' 
      enddo

      return
      end

C************************************************

      subroutine downcase_line(line,len)

      implicit none
      character*(*) line
      integer i,len,k

      do i=1,len
        k=ichar(line(i:i))
        if(k.ge.65.and.k.le.90) then !upper case A-Z
          k=ichar(line(i:i))+32   
          line(i:i)=char(k)        
        endif
      enddo

      return
      end
