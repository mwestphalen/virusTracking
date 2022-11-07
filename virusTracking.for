*************************
* This program simulates and track the spread of a fictional virus,
* named "CMS 495". It is spreading over 6 offices
* 

* Labels:
* 1-9: Input Check
* 11-19: Output formatting
*
* Written by: M. Westphalen
* 11/08/2022
*************************
      
      PROGRAM virusTracking

* Variable Declaration/Initialization        
        BYTE, DIMENSION(20,6) :: people ! 20 people / 6 rooms 
        BYTE, DIMENSION(20) :: peopleInfo ! 1 = infected, 0 = not infected
        BYTE, DIMENSION(9) :: occupantsInfo ! 1 = infected, 0 = not infected

* First spots in the arrays are saved for the room occupants
        INTEGER, DIMENSION(29) :: room1,room2,room3,room4,room5,room6 

        INTEGER :: infectedPerson

* Random Number Generator
        INTEGER :: clock
        CALL System_Clock(clock)
        CALL srand(clock)

* Simulation Starts
* Initialize people and room arrays to 0
        DO i=1, 29
            room1(i) = 0
            room2(i) = 0
            room3(i) = 0
            room4(i) = 0
            room5(i) = 0
            room6(i) = 0
        ENDDO
        DO i=1,20
            peopleInfo(i) = 0
            IF (i .LE. 9) THEN
                occupantsInfo(i) = 0
            ENDIF
            DO j=1, 6
                people(i,j) = 0
            ENDDO
        ENDDO

* Initialize room arrays for occupants in the room
        room1(1) = 1 ! Professor X = 1
        room2(1) = 2 ! Student A = 2
        room2(2) = 3 ! Student B = 3
        room2(3) = 4 ! Student C = 4
        room4(1) = 5 ! Professor Z = 5
        room4(2) = 6 ! Student F = 6
        room5(1) = 7 ! Professor V = 7
        room6(1) = 8 ! Student H = 8
        room6(2) = 9 ! Student I = 9
        ! 10 - 29 are reserved for the people entering the building

* One of the people going in the building is randomly infected
        infectedPerson = INT(rand() * 19) + 10
        CALL infectPerson(infectedPerson,peopleInfo, occupantsInfo)
        
        DO i=1,21
            WRITE(*,*) peopleInfo(i)
        ENDDO
        WRITE(*,*) '*******'
        DO i=1,9
            WRITE(*,*) occupantsInfo(i)
        ENDDO

* Additional people are randomly assigned a room out of the 6
        CALL assignRooms(room1,room2,room3,room4,room5,room6,people)

        DO i=1, 10
            CALL infectChance(room1, peopleInfo, occupantsInfo)
            CALL infectChance(room2, peopleInfo, occupantsInfo)
            CALL infectChance(room3, peopleInfo, occupantsInfo)
            CALL infectChance(room4, peopleInfo, occupantsInfo)
            CALL infectChance(room5, peopleInfo, occupantsInfo)
            CALL infectChance(room6, peopleInfo, occupantsInfo)

* Moving people out of the rooms
            CALL resetRoom(room1)
            CALL resetRoom(room2)
            IF (room3(3) .NE. 0) THEN ! Wait until 3rd spot in Room 3 is filled to reset room
                CALL resetRoom(room3)
            ENDIF
            CALL resetRoom(room4)
            CALL resetRoom(room5)
            CALL resetRoom(room6)

* Moving people into new rooms
          ! CALL assignRooms(room1,room2,room3,room4,room5,room6,people)
        ENDDO

       
       ! OPEN(unit=1,file='virus.csv')
        
        !DO i=1,20
           ! DO J=1,6
          !      WRITE(1,*) people(i,j)
         !   ENDDO
        !ENDDO
        

        !CLOSE(unit=1)
* End of Program
      STOP
      END

* Subroutine to infect people in a room
      SUBROUTINE infectPerson(person, peopleInfo, occupantsInfo)
        BYTE, DIMENSION(20) :: peopleInfo
        BYTE, DIMENSION(9) :: occupantsInfo
        INTEGER :: person

        IF (person > 0 .AND. person < 10) THEN ! A room occupant
            occupantsInfo(person) = 1 ! 1 = infected, 0 = not infected
        ELSEIF (person > 9 .AND. person < 30) THEN ! One of the 20 people
            peopleInfo(person - 9) = 1 ! 1 = infected, 0 = not infected
        ENDIF
      END

* Subroutine to infect people
      SUBROUTINE infectChance(roomNum, peopleInfo, occupantsInfo)
          INTEGER, DIMENSION(29) :: roomNum
          BYTE, DIMENSION(20) :: peopleInfo
          BYTE, DIMENSION(9) :: occupantsInfo
          REAL :: infectionChance
          INTEGER :: person
      
          DO i=1, 29 ! Go through the room and see who is in it
            person = roomNum(i)         
            IF(occupantsInfo(person)==1 .OR. peopleInfo(person)==1)THEN
                DO j=1, 29 ! Go through each one to give 50% chance of infecting them
                    IF (roomNum(j) .NE. 0) THEN ! Only those in the room 
                        infectionChance = rand()
                        IF (infectionChance .LT. 0.5) THEN
                     CALL infectPerson(person,peopleInfo,occupantsInfo)
                        ENDIF
                    ENDIF
                ENDDO
            ENDIF
          ENDDO
      END

* Subroutine to add a person to a specific room
      SUBROUTINE addPersonToRoom(roomNum, person) 
        INTEGER, DIMENSION(29) :: roomNum
        INTEGER :: person
         DO i=1, 29 ! 29 is the size of the room arrays
            IF (roomNum(i) == 0) THEN
                roomNum(i) = person
                EXIT
            ENDIF
         ENDDO
      END

* Subroutine to remove a person from a specific room
      SUBROUTINE resetRoom(roomNum)
        INTEGER, DIMENSION(29) :: roomNum
        INTEGER :: person
        DO i=1, 29 ! 29 is the size of the room arrays
            IF (roomNum(i) > 9) THEN ! Occupants stay, other people leave
                roomNum(i) = 0 ! 0 = unoccupied
            ENDIF
        ENDDO
      END

* Subroutine to randomly assign people rooms    
      SUBROUTINE assignRooms(r1,r2,r3,r4,r5,r6,people)
        INTEGER, DIMENSION(29) :: r1,r2,r3,r4,r5,r6
        BYTE, DIMENSION(20,6) :: people
        INTEGER :: randRoom, person, sum

        DO person=10,29 
1           randRoom = INT(rand() * 5) + 1
            sum = turnSum(people, person - 9)

            ! If person took 4 turns, has visited Prof. X (Room 1), but has not visited Prof. V (Room 5)
            IF ((sum > 3) .AND. (people(person - 9, 1) == 1) .AND.
     +                  (people(person - 9, 5) == 0)) THEN 
                randRoom = 5
                CALL addPersonToRoom(r5,person) ! Add person to room 5 instead
            ENDIF
                
            IF((people(person - 9, 6) .NE. 1) .OR. (sum < 3)) THEN ! If room 6 has not been visited OR they haven't had 4 runs yet
                IF ((randRoom == 1) .AND. (people(person-9,randRoom)
     +                    .NE. 1)) THEN
                    CALL addPersonToRoom(r1, person)

                ELSE IF ((randRoom == 2) .AND. 
     +                      people(person-9,randRoom) .NE. 1) THEN
        
                    IF (people(person-9,4) .NE. 1) THEN ! Can't visit Room 2 if Room 4 has been visited
                        CALL addPersonToRoom(r2, person)
                    ELSE
                        GO TO 1
                    ENDIF

                ELSE IF ((randRoom == 3) .AND. 
     +                      people(person-9,randRoom) .NE. 1) THEN
                        IF (r3(3) .NE. 0) THEN
                            CALL addPersonToRoom(r3, person)
                        ENDIF
        
                ELSE IF ((randRoom == 4) .AND. 
     +                      people(person-9,randRoom) .NE. 1) THEN
                        CALL addPersonToRoom(r4, person)

                ELSE IF ((randRoom == 5) .AND. 
     +                      people(person-9,randRoom) .NE. 1) THEN
                        CALL addPersonToRoom(r5, person)

                ELSE IF ((randRoom == 6) .AND. 
     +                      people(person-9,randRoom) .NE. 1) THEN
                        CALL addPersonToRoom(r6, person)
                ELSE 
                    GO TO 1
                ENDIF
            ENDIF    
            people(person - 9, randRoom) = 1        
        ENDDO
      END

! Returns the number of turns a person has done
      FUNCTION turnSum(people, person)
        BYTE, DIMENSION(20,6) :: people  
        INTEGER :: person
        DO i=1,6
         turnSum = turnSum + INT(people(person, i))
        ENDDO
        RETURN
      END
