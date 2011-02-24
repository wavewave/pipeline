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
     .     NITER,NFILES,IJETALG,KTSCHE
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
     .   ISTUP(MAXNUP),MOTHUP(2,MAXNUP),ICOLUP(2,MAXNUP),PUP(5,MAXNUP),
     .   VTIMUP(MAXNUP),SPINUP(MAXNUP)
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
     .CTHCAL(MNCY),STHCAL(MNCY),CPHCAL(MNCPHI),SPHCAL(MNCPHI),
     .YCMIN,YCMAX,NCY,NCPHI
      DATA YCMIN,YCMAX/-5d0,5d0/
      DATA NCY,NCPHI/100,60/
      PARAMETER (NJMAX=500)
      COMMON/GETCOM/PJJET(4,NJMAX),ETJETJ(NJMAX),JETNO(MNCY,MNCPHI),
     .NJJET
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
      CHARACTER*250 INHEP,OUTHEPEVT
      INTEGER NCELEP, NCEJET, NPASEV
      DOUBLE PRECISION ETATEMP,ETTEMP
      DOUBLE PRECISION PT_MOM, ETA_MOM


      if(iargc().ne.2) then 
         write(*,*) 'hepevt2stdhep inputfile outputfile'
         stop
      endif
      CALL getarg(1,INHEP)
      CALL getarg(2,OUTHEPEVT)

      OPEN (22, FILE=OUTHEPEVT, ERR=90)


C...Set pythia input card and STDHEP output file
c      pythia_card='../Cards/pythia_card.dat'
c      banner_file='banner.txt'

      ISTREAM=1
      ISTR2=2

      IF(ijetalg.eq.0) THEN
         write(*,*)'Using KTCLUS for jet finding (ijetalg=0)'
      ELSE
         write(*,*)'Using GETJET for jet finding (ijetalg=1)'
      ENDIF


      

C...Open STDHEP input file
 5    call stdxropen(INHEP,NITER,istream,lok)
      if (lok.ne.0) then
        write(*,'('' STDX: open failure on pythia_events.hep: '',i5)')
     .     lok
        stop
      endif
c      WRITE(*,*) 'ISTR=',ISTREAM


C...Read pythia input card
c      WRITE(*,*)
c      WRITE(*,*),'Reading pythia input card'
c      CALL RPYCARD(pythia_card)

c     IWKIM TEMPORARY
c      NITER = 100
      NPASEV = 0

c      call stdflpyxsec(NITER)
c      call stdxwrt(100,ISTR2,LOK2)


c      IF(NITER.LE.0)THEN
c        WRITE(*,*),'Generating all events'
c      ELSE
c        WRITE(*,*),'Generating up to ',NITER,' events'
c      ENDIF

      IEV=0
      DO 130 WHILE(.TRUE.)
C...Get event
        call stdxrd(itype,istream,lok)
        if (lok.ne.0) then
          write(*,
     .       '('' READ_EVENT: error reading file: '',i5)') lok
          GOTO 135
        endif
        if(itype.eq.100) then
c start run record
           cycle
        endif
        if (itype.eq.200) then
c end run record        
          call stdxend(istream)
          GOTO 135
        endif

        IF(IEV.GE.NITER) CYCLE

        IEV=IEV+1
c        WRITE(*,*) IEV
C...Convert event from STDHEP format to Pythia format
        call lunhep(2)
C...IWKIM TEST
c        call lunhep(1)
c        call pylist(1)
c       call FLUSH()
c        NUP=N
c        goto 127
C...Prepare for jet clustering
        NN=0
        NCJET=0
        NJJET=0
        NHEP=N
        DO I=7,N
C...Make only interesting jet particles active
          IF(K(I,1).LT.10.AND.
     .          (ABS(K(I,2)).LT.6 .OR. K(I,2).EQ.21 .OR. K(I,2).EQ.22
     .          .OR.(ABS(K(I,2)).GT.100.AND.ABS(K(I,2)).LT.10000)))THEN
             ISTHEP(I)=1
          ELSE
             ISTHEP(I)=2
          ENDIF
          
C...Prepare for kT clustering
          IF(IJETALG.EQ.0.AND.ISTHEP(I).EQ.1)THEN
            APT=sqrt(P(I,1)**2+P(I,2)**2)
            AETA=ABS(LOG(MIN((SQRT(APT**2+P(I,3)**2)+
     .         ABS(P(I,3)))/APT,1d5)))
            IF(AETA.GT.ETAMAX) CYCLE
            IF(APT.EQ.0) CYCLE
            NN=NN+1
            IF (NN.GT.NMAX) then
              WRITE(*,*), 'Too many particles for kt clustering, ',NN,
     .              ', skipping extra particles.'
              NN=NN-1
              EXIT
            endif
            DO J=1,4
              PP(J,NN)=P(I,J)
            ENDDO
          ENDIF
        ENDDO
C...Perform jet clustering with ktclus
        IF(IJETALG.EQ.0)THEN
          IF (NN.GT.1) then
            ECUT=1
            YCUT=ETCLUS**2
            CALL KTCLUS(KTSCHE,PP,NN,ECUT,Y,*999)
            CALL KTRECO(MOD(KTSCHE,10),PP,NN,ECUT,YCUT,YCUT,
     .           PCJET,JET,NCJET,NSUB,*999)            
            DO I=1,NCJET
               ETJETC(I)=PT(PCJET(1,I))
            ENDDO
          ELSE IF (NN.EQ.1) THEN
            Y(1)=PT(PP(1,1))
            IF(Y(1).GT.YCUT)THEN
              NCJET=1
              DO I=1,4
                PCJET(I,1)=PP(I,1)
              ENDDO
            ENDIF
          ENDIF
          CALL ALPSOR(ETJETC,NCJET,KSORTC,2)
          DO I=1,NCJET
             ETACJET(I)=PSERAP(PCJET(1,KSORTC(NCJET+1-I)))
             PHICJET(I)= PYANGL(PCJET(1,KSORTC(NCJET+1-I)),
     .            PCJET(2,KSORTC(NCJET+1-I)))
             IBTAG(I)=0
          ENDDO
        ELSE
C...Perform jet reconstruction with getjet
          YCMAX=ETAMAX
          YCMIN=-YCMAX
          CALL CALINI
          CALL CALSIM
          CALL GETJET(RMAX,ETMIN,ETAMAX)
          CALL ALPSOR(ETJETJ,NJJET,KSORTJ,2)
          DO I=1,NJJET
             ETAJJET(I)=PSERAP(PJJET(1,KSORTJ(NJJET+1-I)))
             PHIJJET(I)=PYANGL(PJJET(1,KSORTJ(NJJET+1-I)),
     .            PJJET(2,KSORTJ(NJJET+1-I)))
             IBTAG(I)=0
          ENDDO
        ENDIF

C...Perform event analysis to generate .lhe and .lhco files
        Pxmiss=0
        Pymiss=0
        NUP=0
        NE=0
        NMU=0
        NTA=0
        NPHOT=0
        DO I=5,N
          IF(K(I,1).GT.20)THEN
            NUP=NUP+1
            IDUP(NUP)=K(I,2)
            IF(I.LE.6)THEN
              ISTUP(NUP)=-1
              MOTHUP(1,NUP)=0
            ELSE
              ISTUP(NUP)=2
              MOTHUP(1,NUP)=MAX(0,K(I,3)-4)
            ENDIF
            MOTHUP(2,NUP)=MOTHUP(1,NUP)
            DO J=1,5
              PUP(J,NUP)=P(I,J)
            ENDDO
          ENDIF
          IF(K(I,1).LT.10.AND.(IABS(K(I,2)).EQ.11.OR.
     .       IABS(K(I,2)).EQ.13.OR.IABS(K(I,2)).EQ.15.OR.
     .       IABS(K(I,2)).EQ.22)
     .       .AND.K(K(I,3),2).EQ.K(I,2))THEN
c            NE=NE+1
c            PTE(NE)=PYP(I,10)
c            ETAE(NE)=PYP(I,19)
c            PHIE(NE)=PYP(I,15)
            NUP=NUP+1
            IDUP(NUP)=K(I,2)
            ISTUP(NUP)=1
            MOTHUP(1,NUP)=0
            MOTHUP(2,NUP)=MOTHUP(1,NUP)
            DO J=1,5
              PUP(J,NUP)=P(I,J)
            ENDDO
c  IWKIM : take care of tau lepton correctly
          ELSE IF(K(I,1).LT.10.AND.(IABS(K(I,2)).EQ.11.OR.
     .       IABS(K(I,2)).EQ.13) 
     .       .AND.IABS(K(K(I,3),2)).EQ.15
     .       .AND.IABS(K(K(K(I,3),3),2)).EQ.15) THEN
c            NE=NE+1
c            PTE(NE)=PYP(I,10)
c            ETAE(NE)=PYP(I,19)
c            PHIE(NE)=PYP(I,15)
            WRITE(*,*) 'I = ',I
            NUP=NUP+1
            IDUP(NUP)=K(I,2)
            ISTUP(NUP)=1
            MOTHUP(1,NUP)=0
            MOTHUP(2,NUP)=MOTHUP(1,NUP)
            DO J=1,5
              PUP(J,NUP)=P(I,J)
            ENDDO
c   END IWKIM
          ELSE IF(K(I,1).LT.10.AND.(IABS(K(I,2)).EQ.12.OR.
     .         IABS(K(I,2)).EQ.14.OR.IABS(K(I,2)).EQ.16.OR.
     .         K(I,2).EQ.1000022))THEN
            Pxmiss=Pxmiss+P(I,1)
            Pymiss=Pymiss+P(I,2)
          ELSE IF(K(I,1).LT.10.AND.IABS(K(I,2)).GT.1000000)THEN
            NUP=NUP+1
            IDUP(NUP)=K(I,2)
            ISTUP(NUP)=1
            MOTHUP(1,NUP)=MAX(0,K(K(I,3),3)-4)
            MOTHUP(2,NUP)=MOTHUP(1,NUP)
            DO J=1,5
              PUP(J,NUP)=P(I,J)
            ENDDO
          ELSE IF(K(I,1).LT.20.AND.(IABS(K(I,2)).EQ.5.OR.
     .     (IABS(K(I,2)).GT.500.AND.IABS(K(I,2)).LT.600).OR.
     .     (IABS(K(I,2)).GT.5000.AND.IABS(K(I,2)).LT.6000)).AND.
     .     (K(I,4).EQ.0.OR.K(K(I,4),2).NE.92.AND.
     .      (IABS(K(K(I,4),2)).LT.500.OR.IABS(K(K(I,4),2)).GT.600).AND.
     .      (IABS(K(K(I,4),2)).LT.5000.OR.IABS(K(K(I,4),2)).GT.6000)))
     .      THEN
c     Check if there is a b quark/hadron going in the direction of one of the jets
            IF(IJETALG.GT.0)THEN
              RFOUND=1.5*RMAX
              IFOUND=0
c              PRINT *,'Found b particle at ',I
              DO J=1,NJJET
c                PRINT *,'DeltaR, Pt ratio: ',
c     .             SQRT((PSERAP(PJJET(1,J))-PYP(I,19))**2+
c     .           (PYANGL(PJJET(1,J),PJJET(2,J))-PYP(I,15))**2),
c     .             PYP(I,10)/PT(PJJET(1,J))
                IF(SQRT((PSERAP(PJJET(1,J))-PYP(I,19))**2+
     .             MIN(ABS(PYANGL(PJJET(1,J),PJJET(2,J))-PYP(I,15)),
     .             TWOPI-ABS(PYANGL(PJJET(1,J),PJJET(2,J))-PYP(I,15))
     .             )**2).LT.RFOUND
     .             .AND. PT(PJJET(1,J))/PYP(I,10).GT.0.3d0 
     .          ) THEN
                  RFOUND=SQRT((PSERAP(PJJET(1,J))-PYP(I,19))**2+
     .             MIN(ABS(PYANGL(PJJET(1,J),PJJET(2,J))-PYP(I,15)),
     .             TWOPI-ABS(PYANGL(PJJET(1,J),PJJET(2,J))-PYP(I,15))
     .             )**2)
                  IFOUND=J
                ENDIF
              ENDDO
              IF(IFOUND.GT.0) THEN
                IBTAG(IFOUND)=1
c                PRINT *,'Found b jet'
c              ELSE
c                PRINT *,'Couldnt find match for b in event ',IEV
c                CALL PYLIST(2)
c                WRITE(*,'(a,3I5,3F8.3)')'b: ',I,K(I,1),K(I,2),PYP(I,10),
c     .             PYP(I,19),PYP(I,15)
c                PRINT *,'Jets: pt eta phi px py pz'
c                DO J=1,NJJET
c                  WRITE(*,'(6F8.3)')PT(PJJET(1,J)),
c     .               PSERAP(PJJET(1,J)),PYANGL(PJJET(1,J),PJJET(2,J)),
c     .               (PJJET(II,J),II=1,3)
c                ENDDO
              ENDIF
            ELSE
              RFOUND=1.5*RMAX
              IFOUND=0
              DO J=1,NCJET
c             For now just use cone also for this case
                IF(SQRT((PSERAP(PCJET(1,J))-PYP(I,19))**2+
     .             MIN(ABS(PYANGL(PCJET(1,J),PCJET(2,J))-PYP(I,15)),
     .             TWOPI-ABS(PYANGL(PCJET(1,J),PCJET(2,J))-PYP(I,15))
     .             )**2).LT.RFOUND
     .             .AND. PT(PCJET(1,J))/PYP(I,10).GT.0.3d0 
     .             ) THEN
                  RFOUND=SQRT((PSERAP(PCJET(1,J))-PYP(I,19))**2+
     .             MIN(ABS(PYANGL(PCJET(1,J),PCJET(2,J))-PYP(I,15)),
     .             TWOPI-ABS(PYANGL(PCJET(1,J),PCJET(2,J))-PYP(I,15))
     .             )**2)
                  IFOUND=J
                ENDIF
              ENDDO
              IF(IFOUND.GT.0) THEN
                IBTAG(IFOUND)=1
c              ELSE
c                PRINT *,'Couldnt find match for b in event ',IEV
c                WRITE(*,'(a,3I5,3F8.3)')'b: ',I,K(I,1),K(I,2),PYP(I,10),
c     .             PYP(I,19),PYP(I,15)
c                PRINT *,'Jets: pt eta phi px py pz'
c                DO J=1,NCJET
c                  WRITE(*,'(6F8.3)')PT(PCJET(1,J)),
c     .               PSERAP(PCJET(1,J)),PYANGL(PCJET(1,J),PCJET(2,J)),
c     .               (PCJET(II,J),II=1,3)
c                ENDDO
c                CALL PYLIST(2)
              ENDIF
            ENDIF
          ENDIF
        ENDDO

c     Jets
        IF(IJETALG.EQ.0)THEN        
          DO I=1,NCJET
            NUP=NUP+1
            IF(IBTAG(KSORTC(NCJET+1-I)).EQ.0)THEN
              IDUP(NUP)=21
            ELSE
              IDUP(NUP)=5
            ENDIF
            ISTUP(NUP)=1
            MOTHUP(1,NUP)=0
            MOTHUP(2,NUP)=MOTHUP(1,NUP)
            DO J=1,4
              PUP(J,NUP)=PCJET(J,KSORTC(NCJET+1-I))
            ENDDO
            PUP(5,NUP)=SQRT(MAX(0d0,PUP(4,NUP)**2-
     .         PUP(1,NUP)**2-PUP(2,NUP)**2-PUP(3,NUP)**2))
          ENDDO
        ELSE
          DO I=1,NJJET
            NUP=NUP+1
            IF(IBTAG(KSORTJ(NJJET+1-I)).EQ.0)THEN
              IDUP(NUP)=21
            ELSE
              IDUP(NUP)=5
            ENDIF
            ISTUP(NUP)=1
            MOTHUP(1,NUP)=0
            MOTHUP(2,NUP)=MOTHUP(1,NUP)
            DO J=1,4
              PUP(J,NUP)=PJJET(J,KSORTJ(NJJET+1-I))
            ENDDO
            PUP(5,NUP)=SQRT(MAX(0d0,PUP(4,NUP)**2-
     .         PUP(1,NUP)**2-PUP(2,NUP)**2-PUP(3,NUP)**2))
          ENDDO
        ENDIF

c     Missing transverse energy
        Etmiss  = sqrt(Pxmiss**2+Pymiss**2)
        IF(Etmiss.GT.0d0)THEN
          Phimiss = PYANGL(Pxmiss,Pymiss)
          NUP=NUP+1
          IDUP(NUP)=12
          ISTUP(NUP)=1
          MOTHUP(1,NUP)=0
          MOTHUP(2,NUP)=0
          PUP(1,NUP)=Pxmiss
          PUP(2,NUP)=Pymiss
          PUP(4,NUP)=Etmiss
          PUP(3,NUP)=0
          PUP(5,NUP)=0
        ENDIF
C...First write dummy events into lhe file
c        DO WHILE(ievold.lt.IEVNT-1)
c          WRITE(20,'(a)') '<event>'
c          WRITE(20,'(I3,I4,4E16.8)')
c     .       0,661,IEVNT*1d9*PARI(2),SCALUP,AQEDUP,AQCDUP
c          WRITE(20,'(a)') '</event>'
c          ievold=ievold+1
c        ENDDO
        ievold=IEV


C       IWKIM 
C       User defined cut
C

c       Missing Energy Cut
        IF(Etmiss.LT.$metcut$) THEN
c           WRITE(*,*) 'Event ',IEV,' has not enough missing energy'
           GOTO 130
        ENDIF

C       Semileptonic Cut and four jet pass cut 
        NCELEP=0
        NCEJET=0
        DO I=1,NUP
           IF(ISTUP(I).EQ.1) THEN 
C     Electron or Muon passed eta cut and et cut  
              IF( (IDUP(I).EQ.11) .OR. (IDUP(I).EQ.-11) .OR.
     .            (IDUP(I).EQ.13) .OR. (IDUP(I).EQ.-13) ) THEN
                 ETATEMP = ETA_MOM(PUP(1,I),PUP(2,I),PUP(3,I),PUP(4,I))
                 ETTEMP  = PT_MOM(PUP(1,I),PUP(2,I),PUP(3,I),PUP(4,I))
                 IF( (ETATEMP.GT. -$etacutlep$) .AND. (ETATEMP.LT. $etacutlep$) 
     .               .AND. (ETTEMP.GT.$etcutlep$) ) THEN 
                    NCELEP=NCELEP+1

                 ENDIF
              ENDIF
C     Four Jet pass eta cut and et cut  
              IF( (IDUP(I).EQ.5) .OR. (IDUP(I).EQ.21) ) THEN
                 ETATEMP = ETA_MOM(PUP(1,I),PUP(2,I),PUP(3,I),PUP(4,I))
                 ETTEMP  = PT_MOM(PUP(1,I),PUP(2,I),PUP(3,I),PUP(4,I))

                 IF( (ETATEMP.GT. -$etacutjet$) .AND. (ETATEMP.LT.$etcutjet$) 
     .               .AND. (ETTEMP.GT.$etcutjet$) ) THEN 

                    NCEJET=NCEJET+1
                 ENDIF
              ENDIF

           ENDIF


        ENDDO

        IF( (NCELEP /= 1) .OR. (NCEJET.LT.4) ) THEN
c           WRITE(*,*) 'Event ',IEV,
c     .                ' is not Semileptonic, not passed a cut',
c     .                NCELEP,' ',NCEJET
           GOTO 130
        ENDIF

        NPASEV = NPASEV + 1

C          Tau : I do not count tau
c           IF( (IDUP(I).EQ.15) .OR. (IDUP(I).EQ.-15) ) THEN
c              NLEP=NLEP+1
c           ENDIF
c        ENDDO
        

        CALL LUNHEP(1)
        WRITE(22,500) IEV, N
c        WRITE (*,*) K(2,1)
        DO J=1,N
           WRITE (22,501) J,K(J,1), K(J,2), K(J,3), K(J,3), K(J,4)
     .           , K(J,5),P(J,1), P(J,2), P(J,3), P(J,4), P(J,5)  
     .           , V(J,1), V(J,2), V(J,3), V(J,4)
        ENDDO

        CALL FLUSH()


C...Print first 0 events   IWKIM

c 125    IF(IEV.LE.0) THEN
c          WRITE(*,*)
c          WRITE(*,*),'Event number: ',IEV
c          CALL PYLIST(2)
c          IF(NN.GT.0) WRITE(*,*),'Jet clustering values:'
c          WRITE(*,*),(SQRT(Y(J)),J=1,MIN(4,NN))
c          WRITE(*,*),'Jet momenta (ktclus):'
c          DO I=1,NCJET
c            WRITE(*,*),(PCJET(J,I),J=1,4)
c          ENDDO
c          WRITE(*,*),'Jet Pts:',(ETJETC(KSORTC(NCJET+1-I)),I=1,NCJET)
c          WRITE(*,*),'Jet momenta (getjet):'
c          DO I=1,NJJET
c            WRITE(*,*),(PJJET(J,KSORTJ(NJJET+1-I)),J=1,4)
c          ENDDO
c          WRITE(*,*),'Jet Pts:',(ETJETJ(KSORTJ(NJJET+1-I)),I=1,NJJET)
cc          call heplst(1)
c          CALL FLUSH()
c        ENDIF

 130  CONTINUE

      
c     IWKIM
c 135  WRITE(*,*),'Done. Try ',IEV,' events.'

c      WRITE(*,*),'Write ',NPASEV,' lhe events.'

c      call stdflpyxsec(NPASEV)
c      call stdxwrt(200,ISTR2,LOK2)
c      call stdxend(ISTR2)

 135  RETURN


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
     .     NITER,NFILES,IJETALG,KTSCHE

C...GETJET commonblocks
      INTEGER MNCY,MNCPHI,NCY,NCPHI,NJMAX,JETNO,NJJET
      DOUBLE PRECISION YCMIN,YCMAX,DELY,DELPHI,ET,STHCAL,CTHCAL,CPHCAL,
     &  SPHCAL,PJJET,ETJETJ
      PARAMETER (MNCY=200)
      PARAMETER (MNCPHI=200)
      COMMON/CALOR/DELY,DELPHI,ET(MNCY,MNCPHI),
     .CTHCAL(MNCY),STHCAL(MNCY),CPHCAL(MNCPHI),SPHCAL(MNCPHI),
     .YCMIN,YCMAX,NCY,NCPHI

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
     .               index(line,'ijetalg').ne.0) then
                READ(line(index(line,'=')+1:),*) ijetalg
                if(ijetalg.eq.0)then
                   write(*,*) 'Using kt jets'
                else
                   write(*,*) 'Using cone jets'
                endif
             else if(index(line,'ETCLUS').ne.0.or.
     .               index(line,'etclus').ne.0) then
                READ(line(index(line,'=')+1:),*) etclus
                write(*,*) 'Read Etclus = ',etclus
             else if(index(line,'ETMIN').ne.0.or.
     .               index(line,'etmin').ne.0) then
                READ(line(index(line,'=')+1:),*) etmin
                write(*,*) 'Read Etmin = ',etmin
             else if(index(line,'RMAX').ne.0.or.
     .               index(line,'rmax').ne.0) then
                READ(line(index(line,'=')+1:),*) rmax
                write(*,*) 'Read Rmax  = ',rmax
             else if(index(line,'ETAMAX').ne.0.or.
     .               index(line,'etamax').ne.0) then
                READ(line(index(line,'=')+1:),*) etamax
                write(*,*) 'Read Etamax = ',etamax
             else if(index(line,'ETAJETMAX').ne.0.or.
     .               index(line,'etajetmax').ne.0) then
                READ(line(index(line,'=')+1:),*) etajetmax
                write(*,*) 'Read Etamax = ',etajetmax
             else if(index(line,'NCY').ne.0.or.
     .               index(line,'ncy').ne.0) then
                READ(line(index(line,'=')+1:),*) ncy
                write(*,*) 'Read NCY = ',ncy
             else if(index(line,'NCPHI').ne.0.or.
     .               index(line,'ncphi').ne.0) then
                READ(line(index(line,'=')+1:),*) ncphi
                write(*,*) 'Read NCPHI = ',ncphi
             else if(index(line,'NITER').ne.0.or.
     .               index(line,'niter').ne.0) then
                READ(line(index(line,'=')+1:),*) niter
                write(*,*) 'Read Niter = ',niter
             else if(index(line,'KTSCHE').ne.0.or.
     .               index(line,'ktsche').ne.0) then
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

