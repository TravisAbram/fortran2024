program main
   implicit none
   integer :: io_status, line_count, i, j = 0
   integer :: part1, part2, difference, similar = 0
   integer :: unit_number = 10
   integer, allocatable :: left(:), right(:)
   character(len=13) :: line

   ! Open the file
   open(unit=unit_number, file='input/day1', status='old', action='read', iostat=io_status)

   ! Check for successful open
   if (io_status /=0) then
      print *, "Error opening the file!"
      stop
   end if

   ! Read file line by line
   line_count = 0
   do while (.true.)
      read(unit_number, '(A)', iostat=io_status) line
      if (io_status < 0) exit
      line_count = line_count + 1
   end do

   ! Rewind file to beginning
   rewind(unit_number)

   ! Allocate memory for array
   allocate(left(line_count), stat=io_status)
   if (io_status /= 0) then
      print *, "Error allocating memory"
      stop
   end if
   allocate(right(line_count), stat=io_status)
   if (io_status /= 0) then
      print *, "Error allocating memory"
      stop
   end if

   ! Populate arrays
   do i = 1, line_count
      read(unit_number, '(A)', iostat=io_status) line
      if (io_status < 0) exit
      ! Hard-coded to input size/pattern of 5 int, 3 buffer, 5 int
      ! These reads are used to turn characters into integers
      read(line(1:5), '(I5)', iostat=io_status) left(i)
      read(line(9:13), '(I5)', iostat=io_status) right(i)
   end do

   ! Sort arrays
   call sort(left)
   call sort(right)

   ! Part 1
   do i = 1, line_count
      difference = left(i) - right(i)
      if ( difference < 0 ) then
         difference = -difference
      end if
      part1 = part1 + difference
   end do
   print *, "Part 1:", part1

   ! Part 2
   do i = 1, line_count
      do j = 1, line_count-1
         if (left(i) == right(j)) then
            similar = similar + 1
         end if
      end do
      part2 = part2 + (left(i) * similar)
      similar = 0
   end do
   print *, "Part 2:", part2

   ! Close the file
   close(unit_number)

contains
   ! Bubble sort
   subroutine sort(arr)
      integer, intent(inout) :: arr(:)
      integer :: k, l, temp, n

      n = size(arr)
      do k = 1, n-1
         do l = 1, n-1
            if (arr(l) > arr(l+1)) then
               temp = arr(l)
               arr(l) = arr(l+1)
               arr(l+1) = temp
            end if
         end do
      end do
   end subroutine sort

end program main
