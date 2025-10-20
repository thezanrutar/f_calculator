program calculator
  implicit none
  character *1 :: method
  real :: num, answer

  read *, answer
  read *, method

  do while (method /= "=")
  read *, num

  select case(method)
  case("+")
    answer = answer + num
  case("-")
    answer = answer - num
  case("*")
    answer = answer * num
  case("/")
    if (num == 0) then
      print *, "Division by 0 is not possible."
      stop
    end if
    answer = answer / num
  case("%")
    answer = mod(answer, num)
  case("^")
    answer = answer ** num
  case default
    print *, "Invalid method."
    stop
  end select
  end do

  print "(a8, 1f16.4)", "Answer: ", answer
end program calculator
