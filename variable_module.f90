! TITLE REQUIRED: Purpose of this module
!    Copyright (C) 2020  Nimish Dwarkanath
!
!    This program is free software: you can redistribute it and/or modify
!    it under the terms of the GNU General Public License as published by
!    the Free Software Foundation, either version 3 of the License, or
!    (at your option) any later version.
!
!    This program is distributed in the hope that it will be useful,
!    but WITHOUT ANY WARRANTY; without even the implied warranty of
!    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
!    GNU General Public License for more details.
!
!    You should have received a copy of the GNU General Public License
!    along with this program.  If not, see <https://www.gnu.org/licenses/>.

MODULE Variable
 IMPLICIT NONE
 
 INTEGER , PARAMETER :: nchar = 200 , Input = 5 , MaxLines=1000
 INTEGER    (KIND=4)    :: AnInteger4 , NDim_AReal8AllocatableArray
 INTEGER    (KIND=8)    :: AnInteger8
 REAL       (KIND=4)    :: AReal4
 REAL       (KIND=8)    :: AReal8
 REAL       (KIND=8)    :: AReal8Array (2)
 REAL       (KIND=8) , ALLOCATABLE    :: AReal8AllocatableArray (:)
 CHARACTER  (LEN=100)   :: AString100

 INTEGER :: nlines
 CHARACTER  (LEN=nchar) , ALLOCATABLE   :: FullInputRecord (:,:)
 
 CONTAINS
 ! the only subroutine a user has to edit
 SUBROUTINE ReadInputFile ()
  INTEGER :: i
  CHARACTER (LEN=nchar) :: record , var , val
 ! Default value
  AnInteger4 = 4
  AnInteger8 = 8
  AReal4     = 4.0E+0
  AReal8     = 8.0D+0
  AString100 = 'Some characters'
  NDim_AReal8AllocatableArray = 3
  ALLOCATE (AReal8AllocatableArray (NDim_AReal8AllocatableArray))
  AReal8AllocatableArray = 0.0D0

  !Over-ride default values by reading the input file
  CALL GetInput ()
  DO i = 1 , nlines
   var = FullInputRecord (1,i)
   val = FullInputRecord (2,i)
    SELECT CASE ( TRIM (var))
     CASE ( 'AnInteger4' )
        READ (val,*) AnInteger4
        WRITE (6,*) 'AnInteger4' , AnInteger4
     CASE ( 'AnInteger8' )
        READ (val,*) AnInteger8
        WRITE (6,*) 'AnInteger8' , AnInteger8
     CASE ( 'AReal4' )
        READ (val,*) AReal4
        WRITE (6,*) 'AReal4' , AReal4
     CASE ( 'AReal8' )
        READ (val,*) AReal8
        WRITE (6,*) 'AReal8' , AReal8
     CASE ( 'AReal8Array' )
        READ (val,*) AReal8Array
        WRITE (6,*) 'AReal8Array' , AReal8Array
     CASE ( 'NDim_AReal8AllocatableArray' )
        READ (val,*) NDim_AReal8AllocatableArray
        WRITE (6,*) 'NDim_AReal8AllocatableArray' , NDim_AReal8AllocatableArray
     CASE ( 'AReal8AllocatableArray' )
        IF (ALLOCATED (AReal8AllocatableArray)) DEALLOCATE (AReal8AllocatableArray)
        ALLOCATE (AReal8AllocatableArray (NDim_AReal8AllocatableArray))
        READ (val,*) AReal8AllocatableArray
        WRITE (6,*) 'AReal8AllocatableArray' , AReal8AllocatableArray
    ENDSELECT
  ENDDO

 ENDSUBROUTINE ReadInputFile

 !Doesn't required any modifications
 SUBROUTINE GetInput ()
  INTEGER :: i , ios
  CHARACTER (LEN=nchar) :: record , var , val
  ! Get all the meaningful input lines to FullInputRecord
  OPEN ( UNIT = 60 , FILE = "input_variables.out" , ACTION = 'WRITE')
  ALLOCATE (FullInputRecord (2,MaxLines))
  nlines = 0
  DO
   READ (Input,'(a200)', IOSTAT = ios) record
   IF ( ios .NE. 0 ) EXIT
   CALL CleanLine (record,var,val)
   IF (record (1:nchar) .EQ. ACHAR(32)) CYCLE
   nlines = nlines + 1
   FullInputRecord (1,nlines) = var
   FullInputRecord (2,nlines) = val
   WRITE (60,'(A)') TRIM(record)
  ENDDO
  WRITE (60,*)"Number of meaningful lines in the input:", nlines
  CLOSE (60)
 ENDSUBROUTINE GetInput

 SUBROUTINE CleanLine (record,var,val)
  IMPLICIT NONE
  CHARACTER (LEN=nchar) , INTENT (INOUT)    :: record , var , val
  CHARACTER (LEN=nchar)               :: line , line2
  CHARACTER (LEN=1)                   :: ch
  INTEGER :: i , counter, ich , lastspaceposition , lastnonspaceposition
  LOGICAL :: consecutivespaces , prevcharspace

  line = record
  DO i = 1 , nchar
   ich = ICHAR (line(i:i))
   !substitute a tab by a whitespace
   IF (ich .EQ. 9) line(i:i) = ACHAR(32)
   !substitute all characters starting wiht '#' by whitespaces
   IF (ich .EQ. 35) THEN
    line(i:nchar) = ACHAR (32)
    EXIT
   ENDIF
  ENDDO
  !remove empty lines & leading spaces, and replace multiple spaces by only ONE whitespace
  prevcharspace = .TRUE.
  line2 = line
  line = ACHAR(32)
  counter = 1
  DO i = 1 , nchar
   ch = line2(i:i)
   ich = ICHAR (ch)
   IF ( ich .NE. 32 )THEN
    line(counter:counter) = ch
    counter = counter + 1
    prevcharspace = .FALSE.
   ELSEIF ( .NOT. prevcharspace .AND. (ich .EQ. 32)) THEN
    line(counter:counter) = ch
    counter = counter + 1
    prevcharspace = .TRUE.
   ELSEIF ( prevcharspace .AND. (ich .EQ. 32)) THEN
    CYCLE
   ENDIF
  ENDDO
  record = line
  var = ACHAR(32)
  val = ACHAR(32)
  DO i = 1 , nchar
   ch = line(i:i)
   ich = ICHAR (ch)
   IF ( ich .EQ. 32 ) THEN
    var = line (1:i)
    val = line (i+1:nchar)
    EXIT 
   ENDIF
  ENDDO
 ENDSUBROUTINE CleanLine
 
ENDMODULE Variable
