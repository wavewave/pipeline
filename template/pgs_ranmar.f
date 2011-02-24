      real*4 function pgs_ranmar()

      implicit none

      include 'pgs.inc'

      logical first

      data first/.true./

      real*4 rvec(1000)
      integer ivec,lvec

      data ivec,lvec/1000,1000/

c initialize

      if(first) then
        call rmarin(pgs_iseed,pgs_jseed)  ! seeds must be in the range 1-30000
        first = .false.
      endif

c return number

      if(ivec.eq.lvec) then
        call ranmar(rvec,lvec)
        ivec = 0
      endif

      ivec = ivec + 1
      pgs_ranmar = rvec(ivec)

      return
      end


c RANMAR source code

      SUBROUTINE RANMAR(RVEC,LENV)
C Universal random number generator proposed by Marsaglia and Zaman
C in report FSU-SCRI-87-50
C        slightly modified by F. James, 1988, to generate a vector
C        of pseudorandom numbers RVEC of length LENV, and to put in
C        the COMMON block everything needed to restart at same place. 
      DIMENSION RVEC(*)
      COMMON/RASET1/U(97),C,CD,CM,I97,J97
      DATA INIT/0/
C
      IF (INIT .GT. 0)  GO TO 50
C
C        Default initialization. User has called RANMAR without RMARIN.
      IJ = 1802
      KL = 9373
      KALLED = 0
      GO TO 1
C
      ENTRY      RMARIN(IJIN,KLIN)
C         Initializing routine for RANMAR, may be called before
C         generating pseudorandom numbers with RANMAR. The input
C         values should be in the ranges:  0<=IJ<=31328
C                                          0<=KL<=30081
C To get the standard values in Marsaglia's paper, IJ=1802, KL=9373
C  
      IJ = IJIN
      KL = KLIN
      KALLED = 1
    1 CONTINUE
      INIT = 1
      I = MOD(IJ/177, 177) + 2
      J = MOD(IJ, 177)     + 2
      K = MOD(KL/169, 178) + 1
      L = MOD(KL, 169)
c      PRINT '(A,2I7,4I4)',' RANMAR INITIALIZED: ', IJ,KL,I,J,K,L
      DO 2 II= 1, 97
      S = 0.
      T = .5
      DO 3 JJ= 1, 24
         M = MOD(MOD(I*J,179)*K, 179)
         I = J
         J = K
         K = M
         L = MOD(53*L+1, 169)
         IF (MOD(L*M,64) .GE. 32)  S = S+T
    3    T = 0.5*T
    2 U(II) = S
      TWOM24 = 1.0
      DO 4 I24= 1, 24
    4 TWOM24 = 0.5*TWOM24
      C  =   362436.*TWOM24
      CD =  7654321.*TWOM24
      CM = 16777213.*TWOM24
      I97 = 97
      J97 = 33
      IF (KALLED .EQ. 1)  RETURN
C
C          Normal entry to generate LENV random numbers
   50 CONTINUE
      DO 100 IVEC= 1, LENV
      UNI = U(I97)-U(J97)
      IF (UNI .LT. 0.)  UNI=UNI+1.
      U(I97) = UNI
      I97 = I97-1
      IF (I97 .EQ. 0)  I97=97
      J97 = J97-1
      IF (J97 .EQ. 0)  J97=97
      C = C - CD
      IF (C .LT. 0.)  C=C+CM
      UNI = UNI-C
      IF (UNI .LT. 0.) UNI=UNI+1.
C        Replace exact zeroes by uniform distr. *2**-24
         IF (UNI .EQ. 0.)  THEN
         UNI = TWOM24*U(2)
C             An exact zero here is very unlikely, but let's be safe.
         IF (UNI .EQ. 0.) UNI= TWOM24*TWOM24
         ENDIF
      RVEC(IVEC) = UNI
  100 CONTINUE
      RETURN
      END
