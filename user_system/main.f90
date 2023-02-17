! Very sorry to those who for whatever reason
! tries to use this to get a grasp of Fortran, I am an awful code commenter
! and too lazy to even comment my code lmao.
! Anyway good luck enjoying this!

PROGRAM main

 IMPLICIT none

 ! String arrays
 CHARACTER(99), DIMENSION(500) :: users
 CHARACTER(50), DIMENSION(3) :: options = ["Add User   ", "Remove User", "List Users "]
 
 ! Characters
 CHARACTER :: program_char = ' '

 ! Integers
 INTEGER :: users_index = 1
 INTEGER :: arrow_index = 1 ! Used for the >   {option} you see when the options are listed
 INTEGER :: user_count  = 0

 ! Booleans
 LOGICAL :: program_running = .TRUE.

 ! Strings
 CHARACTER(:), ALLOCATABLE :: option
 CHARACTER(:), ALLOCATABLE :: user_name

 ALLOCATE(CHARACTER(30) :: option)
 ALLOCATE(CHARACTER(100) :: user_name)

 DO WHILE (program_running)
  CALL system("clear")
  CALL print_options
 
  read(*,*) program_char

  ! Go down
  IF (program_char == 's') THEN

   IF (arrow_index > SIZE(options) - 1) THEN
    arrow_index = 1
   ELSE
    arrow_index = arrow_index + 1
   ENDIF
  ! Go up  
  ELSEIF (program_char == 'w') THEN
   
   IF (arrow_index < 1) THEN
    arrow_index = SIZE(options)
   ELSE
    arrow_index = arrow_index - 1
   ENDIF
  ! Quit the program.
  ELSEIF (program_char == 'q') THEN

   program_running = .FALSE.
  
  ELSEIF (program_char == 'e') THEN

   option = options(arrow_index)
   CALL execute_command

  ENDIF
 END DO

 CONTAINS
 ! Subroutines
 SUBROUTINE print_users
  
  INTEGER :: iterator

  DO iterator = 1, user_count
   IF (users(iterator) == "") THEN
    ! do nothing
   ELSE
     print *, users(iterator)
   ENDIF
  END DO
 
 END SUBROUTINE print_users


 SUBROUTINE print_options

  INTEGER :: iterator
  
  DO iterator = 1, SIZE(options)
   IF (iterator == arrow_index) THEN
    print *, ">    ", options(arrow_index)
   ELSE
    print *, options(iterator)
   ENDIF
  END DO

 END SUBROUTINE print_options

 
 SUBROUTINE remove_user

  print *, "User: ", users(users_index - 1), "removed."
  users(users_index - 1) = ""

  users_index = users_index - 1

 END SUBROUTINE remove_user 
 

 SUBROUTINE execute_command

  CALL system("clear")

  print *, "Would you like to execute command: ", option, "? (y, n): "
  read(*,*) program_char

  IF (program_char == 'y' .or. program_char == 'Y') THEN
   print *, "Executing command: ", option

   IF (option == "Add User") THEN
    print *, "What is the users name?"
    read(*,*) user_name

    users(users_index) = user_name
    users_index = users_index + 1
    user_count = user_count + 1

   ELSEIF (option == "List Users") THEN
    print *, "Users: "
    CALL print_users

    read(*,*) program_char

   ELSEIF (option == "Remove User") THEN
    CALL remove_user
    read(*,*) program_char
   ENDIF

  ELSEIF (program_char == 'n' .or. program_char == 'N') THEN
   print *, "Exitting execution mode..."
   read(*,*) program_char
  ENDIF

 END SUBROUTINE execute_command

END PROGRAM main
