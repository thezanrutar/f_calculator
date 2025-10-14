program calculator
  implicit none
  character *1 :: method, num_type
  real :: first_num, second_num, answer

  print *, "Choose a method (+, -, *, /, m, e) "
  read *, method

  print *, "Choose a method (i, d) "
  read *, num_type

  print *, "First number "
  read *, first_num
  print *, "Second number "
  read *, second_num

  select case(method)
  case("+")
    answer = first_num + second_num
  case("-")
    answer = first_num - second_num
  case("*")
    answer = first_num * second_num
  case("/")
    if (second_num == 0) then
      print *, "Division by 0 is not possible."
      stop
    end if
    answer = first_num / second_num
  case("m")
    answer = mod(first_num, second_num)
  case("e")
    answer = first_num ** second_num
  case default
    print *, "Invalid method."
    stop
  end select

  print "(a8, 1f16.4)", "Answer: ", answer
end program calculator
