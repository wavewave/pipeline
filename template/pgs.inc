c ------------------------------------------------------------------------------------------
c PGS.INC - include file for PGS package 
c
c           Version 4.0
c ------------------------------------------------------------------------------------------
c
c   This include file is to be used with the PGS package.  See
c   http://www.physics.rutgers.edu/~jconway/soft/pgs/pgs.html
c   for documentation and details.
c
c ------------------------------------------------------------------------------------------


c STDHEP common blocks ---------------------------------------------------------------------

c generated particle list

      integer    nmxhep
      parameter (nmxhep=4000)

      integer nevhep,nhep,isthep,idhep,jmohep,jdahep

      double precision phep,vhep

      common /hepevt/ nevhep,           ! event number
     .                nhep,             ! number of entries in record
     .                isthep(nmxhep),   ! status code
     .                idhep(nmxhep),    ! particle ID (PDG standard)
     .                jmohep(2,nmxhep), ! index to first and second particle mothers
     .                jdahep(2,nmxhep), ! index to first and last daughter particles
     .                phep(5,nmxhep),   ! 4-vector and mass
     .                vhep(4,nmxhep)    ! (x,y,z) of production, and production time (mm/c)

      integer numhep                    ! for symmetry
      equivalence(numhep,nhep)          ! with other lists

      integer numgen                    ! for symmetry
      equivalence(numgen,nhep)          ! with other lists

c table of particle properties

      integer    nmxln

      parameter (nmxln=2000)

      integer idt
      real*4 stmerr,stwerr

      real*8 stmass,stwidth

      character*21 stname

      common /stdtbl/ idt(nmxln),       ! particle ID (PDG standard)
     .                stmerr(2,nmxln),  ! positive (1) and negative (2) error on mass
     .                stwerr(2,nmxln),  ! positive (1) and negative (2) error on width
     .                stmass(nmxln),    ! particle mass (GeV/c^2 for this, and rest)
     .                stwidth(nmxln),   ! particle width
     .                stname(nmxln)     ! particle name


c STDHEP logical unit numbers and I/O information

      integer lnhwrt,lnhrd,lnhout,lnhdcy,lnhrdm

      common /heplun/ lnhwrt,           ! logical unit number for writing
     .                lnhrd,            ! logical unit number for reading
     .                lnhout,           ! logical unit number for output
     .                lnhdcy,           ! logical unit number for decay table (NYI)
     .                lnhrdm(15)        ! logical unit number for reading multiple streams

      real stdecom,stdxsec
      double precision stdseed1,stdseed2
      integer nevtreq,nevtgen,nevtwrt

      common /stdcm1/ stdecom,  ! center of mass energy
     .                stdxsec,  ! cross section
     .                stdseed1, ! random number seed 1
     .                stdseed2, ! random number seed 2
     .                nevtreq,  ! events to generate
     .                nevtgen,  ! actually generated
     .                nevtwrt   ! written to output file
      save /stdcm1/


c ---------------------------------------------------------------------------------------------

c PGS functions for ET, pt, etc. of generated, reconstructed, clusters and tracks

      real*8 et_gen,pt_gen,p_gen,eta_gen,phi_gen
      real*8 et_obj,pt_obj,p_obj,eta_obj,phi_obj
      real*8 et_clu,pt_clu,p_clu,eta_clu,phi_clu
      real*8 et_trc,pt_trc,p_trc,eta_trc,phi_trc
      real*8 et_trk,pt_trk,p_trk,eta_trk,phi_trk

      real*8 cos_theta,cos_del_phi,del_phi
      real*8 pgs_trigger_cluster_width,pgs_cluster_width

      real*8 v4mass,v4mass2,v4et,v4p,v4pt,v4eta,v4phi

      logical pgs_cal_fid_cut

c PGS detector parameters

      integer netacal,nphical

      character*8 jet_alg
      real*8 deta,dphi,crack_frac,rmax,bfield,sagitta_res
      real*8 ecal_res_a,ecal_res_b,hcal_res,cal_fid_eff,met_res
      real*8 seed_thresh,shoulder_thresh,conesize
      real*8 eff_track,eta_max_track,min_track_pt
      real*8 eta_max_egamma,eta_max_muon,eta_max_tau

      common /pgspar/ netacal,		! number of eta cells in calorimeter
     .                nphical,		! number of phi cells in calorimeter
     .                deta,		! eta width of calorimeter cells
     .                dphi,		! phi width of calorimeter cells  (2*pi/nphical)
     .                ecal_res_a,       ! electromagnetic calorimeter resolution const
     .                ecal_res_b,       ! electromagnetic calorimeter resolution * sqrt(E)
     .                hcal_res,         ! hadronic calorimeter resolution * sqrt(E)
     .                cal_fid_eff,      ! em calorimeter tower fiducial fraction
     .                met_res,          ! MET resolution
     .                crack_frac,       ! calorimeter cell edge "crack" fraction (energy deposit)
     .                jet_alg,          ! jet algorithm (cone, ktjet)
     .                seed_thresh,      ! calorimeter cluster seed tower threshold
     .                shoulder_thresh,  ! calorimeter shoulder tower threshold
     .                conesize,         ! cluster finder cone size
     .                rmax,             ! outer radius of tracking (m)
     .                bfield,           ! magnetic field (T)
     .                sagitta_res,      ! sagitta resolution (m)
     .                eff_track,	! track finding efficiency
     .                min_track_pt,     ! minimum track pt (GeV/c)
     .                eta_max_track,    ! maximum tracking coverage in eta
     .                eta_max_egamma,   ! maximum muon coverage in eta
     .                eta_max_muon,     ! maximum muon coverage in eta
     .                eta_max_tau       ! maximum muon coverage in eta

c PGS event header and control information

      integer nevpgs,nprpgs,numarg,pgs_iseed,pgs_jseed
      integer pgs_log_unit,target_lum
      integer pgs_herwig_proc

      character*6 optpgs,evtlum
      character*250 pgs_args
      character*250 pgs_input_file,pgs_output_file
      character*250 pgs_param_file,pgs_log_file,pgs_herwig_susy
      character*250 pgs_isajet_decay,pgs_isajet_cards,pgs_pythia_cards
      character*250 pgs_alpgen_stem 	

      common /pgsevt/ numarg,             ! number of arguments supplied to program
     .                pgs_args(10),       ! list of arguments (char*40)
     .                nevpgs,             ! number of events to generate/read
     .                target_lum,         ! target luminosity (in pb-1)
     .                nprpgs,             ! number of events to print out 
     .                pgs_iseed,pgs_jseed,! seeds for pgs_ranmar
     .                pgs_log_unit,       ! log file unit number
     .                optpgs,             ! type of run: 'PYTHIA', 'ISAJET', 'FILE', ...
     .                evtlum,             ! number of events ('events') or luminosity ('pb-1')
     .                pgs_input_file,     ! input file
     .                pgs_output_file,    ! output file
     .                pgs_log_file,       ! log file
     .                pgs_param_file,     ! detector parameter file
     .                pgs_isajet_decay,   ! ISAJET decay table file name
     .                pgs_isajet_cards,   ! ISAJET card file name
     .                pgs_pythia_cards,   ! PYTHIA card file name
     .                pgs_herwig_proc,    ! HERWIG process to generate
     .                pgs_herwig_susy,    ! HERWIG SUSY data file
     .                pgs_alpgen_stem     ! ALPGEN unweighted events file stem

c PGS track list

      integer ntrkmx
      parameter (ntrkmx=500)

      integer numtrk,dumtrk,indtrk

      real*8 ptrk,qtrk

      common /pgstrk/ numtrk,dumtrk,          ! number of tracks
     .                indtrk(ntrkmx),         ! index to HEPEVT particle
     .                ptrk(3,ntrkmx),         ! track 3-vector 
     .                qtrk(ntrkmx)            ! track charge


c PGS calorimeter tower arrays

      real*8 pi
      parameter(pi=3.141592654)      

      integer nphimax,netamax   
      parameter (nphimax=600)   
      parameter (netamax=600)   

      real*8 ecal,hcal,met_cal,phi_met_cal,met_cor,phi_met_cor

      common /pgscal/ ecal(netamax,nphimax),  ! electromagnetic energy in each tower
     .                hcal(netamax,nphimax),  ! hadronic energy in each tower
     .                met_cal,                ! calorimeter missing ET
     .                phi_met_cal,            ! calorimeter missing ET phi
     .                met_cor,                ! missing ET corrected for muons
     .                phi_met_cor             ! corrected missing ET phi


c PGS calorimeter trigger cluster list

      integer ntrcmx

      parameter (ntrcmx=50)

      integer ctrc,numtrc,dumtrc,etatrc,phitrc,multrc

      real*8 ptrc,ettrc,emtrc,ehtrc,eftrc,widtrc

      common /pgstrc/ ctrc(netamax,nphimax),  ! map of trigger indices
     .                numtrc,dumtrc,          ! number of clusters in list
     .                ptrc(5,ntrcmx),         ! cluster 4 vector and mass
     .                etatrc(ntrcmx),         ! cluster seed tower eta
     .                phitrc(ntrcmx),         ! cluster seed tower phi
     .                emtrc(ntrcmx),          ! cluster electromagnetic energy
     .                ehtrc(ntrcmx),          ! cluster hadronic energy
     .                eftrc(ntrcmx),          ! cluster electromagnetic fraction
     .                widtrc(ntrcmx),         ! cluster width sqrt(deta**2+dphi**2)
     .                multrc(ntrcmx)          ! cluster tower multiplicity


c PGS calorimeter cluster list (kt algorithm)

      integer nclumx

      parameter (nclumx=300)

      integer cclu,numclu,dumclu,etaclu,phiclu,mulclu

      real*8 pclu,etclu,emclu,ehclu,efclu,widclu

      common /pgsclu/ cclu(netamax,nphimax),  ! map of cluster indices
     .                numclu,dumclu,          ! number of clusters in list
     .                pclu(5,nclumx),         ! cluster 4 vector and mass
     .                etaclu(nclumx),         ! cluster seed tower eta
     .                phiclu(nclumx),         ! cluster seed tower phi
     .                emclu(nclumx),          ! cluster electromagnetic energy
     .                ehclu(nclumx),          ! cluster hadronic energy
     .                efclu(nclumx),          ! cluster electromagnetic fraction
     .                widclu(nclumx),         ! cluster width sqrt(deta**2+dphi**2)
     .                mulclu(nclumx)          ! cluster tower multiplicity


c PGS trigger object list

      integer    ntrgmx
      parameter (ntrgmx=500)

      integer numtrg,dumtrg,indtrg,typtrg,ithresh,trigword,ntrigged

      real*8 vectrg

      common /pgstrg/ numtrg,dumtrg,          ! number of trigger objects
     .                indtrg(ntrgmx),         ! index to HEPEVT particle (where relevant)
     .                typtrg(ntrgmx),         ! reconstructed type:  0=photon
                                              !                      1=electron
                                              !                      2=muon
                                              !                      3=tau (hadronic)
                                              !                      4=jet
                                              !                      5=detached vertex
                                              !                      6=MET
     .                vectrg(10,ntrgmx),      ! trigger object vector: 1 = eta
                                              !                        2 = phi
                                              !                        3 = ET of cluster
                                              !                        4 = cluster #
                                              !                        5 = pt of track (if any)
                                              !                        6 = track #
     .                ithresh,                ! user trigger threshold 0 = no trigger
                                              !                        1 = low thresh 
                                              !                        2 = high thresh
     .                trigword,ntrigged       ! user trigger word and count

c PGS reconstructed object candidate list

      integer    nobjmx
      parameter (nobjmx=500)

      integer numobj,dumobj,indobj,typobj
      real*8 pobj,qobj,vecobj

      logical unique

      common /pgsrec/ numobj,dumobj,          ! number of reconstructed objects
     .                indobj(nobjmx),         ! index to HEPEVT particle (where relevant)
     .                typobj(nobjmx),         ! reconstructed type:  0 = photon
                                              !                      1 = electron
                                              !                      2 = muon
                                              !                      3 = tau (hadronic)
                                              !                      4 = jet
                                              !                      5 = heavy charged
     .                pobj(4,nobjmx),         ! four vector of reconstructed object
     .                qobj(nobjmx),           ! charge of reconstructed object
     .                vecobj(10,nobjmx),      ! interesting object quantities (see below)
     .                unique(nobjmx)          ! true for object if it is uniquely identified
                                              ! and passes cuts in pgs_object_cuts

c            --------------------------
c            table of vecobj quantities
c            --------------------------
c
c -----------------------------------------------------------------------------------------------------------
c  type            1           2          3        4        5        6        7        8        9       10
c -----------------------------------------------------------------------------------------------------------
c 0  photon     EM energy  HAD energy  track E   N(trk)     -       ET      etiso    ptiso    hadem     ep
c 1  electron    "   "      "     "       "        "        -       ET      etiso ptiso-pttrk hadem     ep
c 2  muon        "   "      "     "       "        "     trkiso E  ptiso      -        -       -        -
c 3  tau         "   "      "     "       "        "      width    mtau     ptmax   trk iso    npi0   pi0 iso
c 4  jet         "   "      "     "       "        "        "      flavor   c,b tags   
c -----------------------------------------------------------------------------------------------------------
c
c   taus:        vecobj( 8,numobj) =  sum pt of tracks not in cone
c                vecobj( 9,numobj) =  number of pi0 in cone
c                vecobj(10,numobj) =  sum pt of pi0's not in cone 
c
c  b, c tagging: vecobj(6,iobj) = 21 (g);  2,1,3 (u,d,s);  4 (c) 5 (b)
c                vecobj(7,iobj) non-zero if loose b tag
c                vecobj(8,iobj) non-zero if tight b tag
c
c    --> tight and loose tags include fake rates for gluon, uds, c and b jets
c
