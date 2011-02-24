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
      INTEGER N,NPAD,K
      DOUBLE PRECISION P,V
      COMMON/PYJETS/N,NPAD,K(4000,5),P(4000,5),V(4000,5)

C...Inputs for jet clustering and Pythia run
C...IJETALG=1(getjet)/0(ktclus)
C...NITER=-1 means all available events
C...Inputs for jet clustering and Pythia run
      INTEGER NITER,NFILES,IJETALG,KTSCHE
      DOUBLE PRECISION ETCLUS,ETMIN,RMAX,ETAMAX,ETAJETMAX
      COMMON/JETPAR/ETCLUS,ETMIN,RMAX,ETAMAX,ETAJETMAX,
     $     NITER,NFILES,IJETALG,KTSCHE
      DATA ETCLUS,ETMIN,RMAX,ETAMAX,ETAJETMAX/10d0,10d0,0.7d0,5d0,4.5d0/
      DATA KTSCHE/4313/
      DATA NITER,NFILES,IJETALG/-1,0,1/
C...External functions
      DOUBLE PRECISION PYP,PYANGL,PSERAP,PT
      EXTERNAL PYP,PYANGL,PSERAP,PT
C...Variables for the kT-clustering
      INTEGER NMAX
      PARAMETER (NMAX=512)
      INTEGER NN,IHEP,I,NSUB,JET(NMAX),NCJET
      DOUBLE PRECISION ECUT,YCUT,RAD
      DOUBLE PRECISION PCJET(4,NMAX),Y(NMAX),PP(4,NMAX),ETJETC(NMAX)
C...User process event common block.
      INTEGER MAXNUP
      PARAMETER (MAXNUP=500)
      INTEGER NUP,IDPRUP,IDUP,ISTUP,MOTHUP,ICOLUP
      DOUBLE PRECISION XWGTUP,SCALUP,AQEDUP,AQCDUP,PUP,VTIMUP,SPINUP
      COMMON/HEPEUP/NUP,IDPRUP,XWGTUP,SCALUP,AQEDUP,AQCDUP,IDUP(MAXNUP),
     &   ISTUP(MAXNUP),MOTHUP(2,MAXNUP),ICOLUP(2,MAXNUP),PUP(5,MAXNUP),
     &   VTIMUP(MAXNUP),SPINUP(MAXNUP)
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
      DATA YCMIN,YCMAX/-5d0,5d0/
      DATA NCY,NCPHI/100,60/
      PARAMETER (NJMAX=500)
      COMMON/GETCOM/PJJET(4,NJMAX),ETJETJ(NJMAX),JETNO(MNCY,MNCPHI),
     $NJJET
c
c   STDHEP global variables
c   stdecom  - center-of-mass energy
c   stdxsec  - cross-section
c   stdseed1 - random number seed
c   stdseed2 - random number seed
c   nevtreq  - number of events to be generated
c   nevtgen  - number of events actually generated
c   nevtwrt  - number of events written to output file
c   nevtlh   - number of Les Houches events written to output file
c
      real stdecom,stdxsec
      double precision stdseed1,stdseed2
      integer nevtreq,nevtgen,nevtwrt,nevtlh
      common /stdcm1/ stdecom,stdxsec,stdseed1,stdseed2,
     1                nevtreq,nevtgen,nevtwrt,nevtlh
c
c -------------------------------------------------------------

C   Local variables
      INTEGER IEV,istrstd,ievold
      CHARACTER*5 CGIVE
      CHARACTER*30 CGIVE0
      CHARACTER*80 pythia_card,output_file,init_file,input_file!,banner_file
      CHARACTER*250 buff
      DOUBLE PRECISION APT,AETA,ETA,RFOUND
      DOUBLE PRECISION Pxmiss,Pymiss,Etmiss,Phimiss
      DOUBLE PRECISION ETACJET(NJMAX),ETAJJET(NJMAX)
      DOUBLE PRECISION PHICJET(NJMAX),PHIJJET(NJMAX)
      DOUBLE PRECISION PTE(NMAX),ETAE(NMAX),PHIE(NMAX)
      DOUBLE PRECISION PTMU(NMAX),ETAMU(NMAX),PHIMU(NMAX)
      DOUBLE PRECISION PTTA(NMAX),ETATA(NMAX),PHITA(NMAX)
      DOUBLE PRECISION PTPHOT(NMAX),ETAPHOT(NMAX),PHIPHOT(NMAX)
      INTEGER J,KSORTJ(NMAX),KSORTC(NMAX),KSORT(NMAX),IBTAG(NMAX)
      INTEGER NE,NMU,NTA,NPHOT,IOBJ,II,IFOUND
      DOUBLE PRECISION PI,TWOPI
      PARAMETER (PI=3.141592653,TWOPI=2*PI)

c      LOGICAL banner_open
      integer lok,istream,itype

C     IWKIM variables
      INTEGER LOK2,ISTR2
      CHARACTER*80 OUTPUTHEP
      INTEGER NCELEP, NCEJET, NPASEV
      DOUBLE PRECISION ETATEMP,ETTEMP
      DOUBLE PRECISION PT_MOM, ETA_MOM

      

C...Set pythia input card and STDHEP output file
c      pythia_card='../Cards/pythia_card.dat'
c      banner_file='banner.txt'
c      input_file='pythia_events.hep'
c      output_file='pythia_events.lhe'
c      init_file='pythia_events.init'
c      OUTPUTHEP='aftercut.hepevt'

      ISTREAM=1
      ISTR2=2

      IF(ijetalg.eq.0) THEN
         write(*,*)'Using KTCLUS for jet finding (ijetalg=0)'
      ELSE
         write(*,*)'Using GETJET for jet finding (ijetalg=1)'
      ENDIF

      CALL GETARG(1,INPUT_FILE)


C...Open STDHEP input file
 5    call stdxropen(INPUT_FILE,NITER,istream,lok)
      if (lok.ne.0) then
        write(*,'('' STDX: open failure on pythia_events.hep: '',i5)')
     $     lok
        stop
      endif
      WRITE(*,*) 'ISTR=',ISTREAM


      NITER = 100
      NPASEV = 0

      IF(NITER.LE.0)THEN
        WRITE(*,*),'Generating all events'
      ELSE
        WRITE(*,*),'Generating up to ',NITER,' events'
      ENDIF

      IEV=0
      DO 130 WHILE(.TRUE.)
C...Get event
        call stdxrd(itype,istream,lok)
c        if (lok.ne.0) then
c          write(*,
c     .       '('' READ_EVENT: error reading file: '',i5)') lok
c          GOTO 135
c        endif
        if(itype.eq.100) then
cc start run record
           GOTO 130 
        endif
        if (itype.eq.200) then
c end run record        
C...Write init info to the .lhe init file
c          call stdxend(istream)
          GOTO 135
        endif

        IF(IEV.GE.NITER) CYCLE

        IEV=IEV+1
        WRITE(*,*) IEV
C...Convert event from STDHEP format to Pythia format
        call lunhep(2)
C...IWKIM TEST
        call lunhep(1)
c        WRITE(*,*) 'NPAD = ',NPAD
        WRITE(*,*) 'NEVHEP',NEVHEP
        call PYLIST(3)
        call FLUSH()
c        NUP=N
        goto 130

 130  CONTINUE

C...Close lhe file
 135  WRITE(*,*),'Done. Try ',IEV,' events.'

      WRITE(*,*),'Write ',NPASEV,' lhe events.'
      RETURN


 4000 FORMAT(i3,i14,i7)
 4001 FORMAT(i3,i5,f9.3,f7.3,f8.2,f8.2,2f6.1,f9.2,2f6.1)
c...IWKIM HEPEVT FORMAT
 500  FORMAT(i7,i4)
 501  FORMAT(i5,i4,i7,i4,i4,i4,i4,9E16.8)


 999  WRITE(*,*) 'Error in one of the KTCLUS routines'
      STOP
 90   WRITE(*,*) 'Error: Could not open MadEvent event file'
      WRITE(*,*) 'Quitting...'
      END
      
C************************************************

      SUBROUTINE RPYCARD(pythia_card)

      IMPLICIT NONE
      
C...Inputs for jet clustering and Pythia run
      INTEGER NITER,NFILES,IJETALG,KTSCHE
      DOUBLE PRECISION ETCLUS,ETMIN,RMAX,ETAMAX,ETAJETMAX
      COMMON/JETPAR/ETCLUS,ETMIN,RMAX,ETAMAX,ETAJETMAX,
     $     NITER,NFILES,IJETALG,KTSCHE

C...GETJET commonblocks
      INTEGER MNCY,MNCPHI,NCY,NCPHI,NJMAX,JETNO,NJJET
      DOUBLE PRECISION YCMIN,YCMAX,DELY,DELPHI,ET,STHCAL,CTHCAL,CPHCAL,
     &  SPHCAL,PJJET,ETJETJ
      PARAMETER (MNCY=200)
      PARAMETER (MNCPHI=200)
      COMMON/CALOR/DELY,DELPHI,ET(MNCY,MNCPHI),
     $CTHCAL(MNCY),STHCAL(MNCY),CPHCAL(MNCPHI),SPHCAL(MNCPHI),
     $YCMIN,YCMAX,NCY,NCPHI

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
             if(index(line,'IJETALG').ne.0.or.
     $               index(line,'ijetalg').ne.0) then
                READ(line(index(line,'=')+1:),*) ijetalg
                if(ijetalg.eq.0)then
                   write(*,*) 'Using kt jets'
                else
                   write(*,*) 'Using cone jets'
                endif
             else if(index(line,'ETCLUS').ne.0.or.
     $               index(line,'etclus').ne.0) then
                READ(line(index(line,'=')+1:),*) etclus
                write(*,*) 'Read Etclus = ',etclus
             else if(index(line,'ETMIN').ne.0.or.
     $               index(line,'etmin').ne.0) then
                READ(line(index(line,'=')+1:),*) etmin
                write(*,*) 'Read Etmin = ',etmin
             else if(index(line,'RMAX').ne.0.or.
     $               index(line,'rmax').ne.0) then
                READ(line(index(line,'=')+1:),*) rmax
                write(*,*) 'Read Rmax  = ',rmax
             else if(index(line,'ETAMAX').ne.0.or.
     $               index(line,'etamax').ne.0) then
                READ(line(index(line,'=')+1:),*) etamax
                write(*,*) 'Read Etamax = ',etamax
             else if(index(line,'ETAJETMAX').ne.0.or.
     $               index(line,'etajetmax').ne.0) then
                READ(line(index(line,'=')+1:),*) etajetmax
                write(*,*) 'Read Etamax = ',etajetmax
             else if(index(line,'NCY').ne.0.or.
     $               index(line,'ncy').ne.0) then
                READ(line(index(line,'=')+1:),*) ncy
                write(*,*) 'Read NCY = ',ncy
             else if(index(line,'NCPHI').ne.0.or.
     $               index(line,'ncphi').ne.0) then
                READ(line(index(line,'=')+1:),*) ncphi
                write(*,*) 'Read NCPHI = ',ncphi
             else if(index(line,'NITER').ne.0.or.
     $               index(line,'niter').ne.0) then
                READ(line(index(line,'=')+1:),*) niter
                write(*,*) 'Read Niter = ',niter
             else if(index(line,'KTSCHE').ne.0.or.
     $               index(line,'ktsche').ne.0) then
                READ(line(index(line,'=')+1:),*) ktsche
                write(*,*) 'Read Ktsche = ',ktsche
             endif
          endif
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

      double precision function PT(P)

      implicit none

      double precision P(4)

      PT=sqrt(P(1)**2+P(2)**2)
      return

      end



c generated particles --------------------------------------------------
      
      DOUBLE PRECISION FUNCTION PT_MOM(P1,P2,P3,P4)

c this function returns the ET of a generated (HEPEVT) object

      implicit none
      DOUBLE PRECISION P1,P2,P3,P4
      DOUBLE PRECISION PT_MOM

      PT_MOM = SQRT(P1*P1 + P2*P2)
      RETURN 
      END


      DOUBLE PRECISION FUNCTION ETA_MOM(P1,P2,P3,P4)

c this function returns the eta (pseudorapidity) 
c of a generated (HEPEVT) particle

      implicit none
      DOUBLE PRECISION P1,P2,P3,P4
      DOUBLE PRECISION ETA_MOM,PT_MOM
      DOUBLE PRECISION P,PT

      P = SQRT (P1*P1+P2*P2+P3*P3)
      PT = PT_MOM(P1,P2,P3,P4)

      if ((P-P3).ne.0.0) then
        ETA_MOM = DLOG(PT/(P-P3))
      endif
 
      return
      end


      DOUBLE PRECISION FUNCTION PHI_MOM(P1,P2,P3,P4)

c this function returns the phi (azimuthal angle)
c of a generated (HEPEVT) object

      implicit none
      DOUBLE PRECISION P1,P2,P3,P4
      DOUBLE PRECISION PHI_MOM
      DOUBLE PRECISION P,PT,PT_MOM
      DOUBLE PRECISION PI,TWOPI
      PARAMETER (PI=3.141592653,TWOPI=2*PI)


      P = SQRT (P1*P1+P2*P2+P3*P3)
      PT = PT_MOM(P1,P2,P3,P4)


      if ((P-P3).ne.0.0) then
        PHI_MOM = ATAN2(P2,P1)
        if (PHI_MOM.lt.0.) PHI_MOM = PHI_MOM + TWOPI
      endif
 
      return
      end

