    
subroutine zmq_ezfio_has(cmd_in,exists)
  use f77_zmq
  implicit none
  character*(*)                  :: cmd_in
  logical, intent(out)           :: exists
  BEGIN_DOC
  ! ezfio_has through a ZMQ connexion
  END_DOC
  integer                        :: rc
  character*(128)                :: cmd

  cmd = 'has_'//cmd_in
  rc = f77_zmq_send(zmq_to_dataserver_socket, 'Ezfio', 5, ZMQ_SNDMORE)
  if (rc /= 5) then
    print *,  irp_here, rc
    stop 1
  endif
  rc = f77_zmq_send(zmq_to_dataserver_socket, cmd, len_trim(cmd), 0)
  if (rc < 0) then
    print *,  irp_here, rc
    stop 2
  endif
  
  character(len=8)               :: buffer
  integer                        :: buffer_size
  character*(4)                  :: buffer_size_char
  
  rc = f77_zmq_recv(zmq_to_dataserver_socket, buffer_size_char,      &
      len(buffer_size_char), ZMQ_SNDMORE)
  read(buffer_size_char(1:rc),*) buffer_size
  
  logical                        :: w
  rc = f77_zmq_recv(zmq_to_dataserver_socket, buffer, buffer_size, 0)
  read( buffer(1:rc), *) w
  
  exists = w
  
end

subroutine zmq_ezfio_get_logical(cmd_in,w,d)
  implicit none
  use f77_zmq
  BEGIN_DOC
! Fetch a logical variable in EZFIO using ZMQ
  END_DOC
  
  integer                        :: d
  logical                        :: w(d)
  character*(*)                  :: cmd_in
  character*(128)                :: cmd
  
  character(len=:), allocatable  :: buffer
  integer                        :: buffer_size
  character*(20)                 :: buffer_size_char
  integer                        :: rc

  cmd = 'get_'//cmd_in
  rc = f77_zmq_send(zmq_to_dataserver_socket, 'Ezfio', 5, ZMQ_SNDMORE)
  rc = f77_zmq_send(zmq_to_dataserver_socket, cmd, len_trim(cmd), 0)

  rc = f77_zmq_recv(zmq_to_dataserver_socket, buffer_size_char, len(buffer_size_char), ZMQ_SNDMORE)
  read(buffer_size_char(1:rc),*) buffer_size
  allocate (character(len=buffer_size) :: buffer)

  rc = f77_zmq_recv(zmq_to_dataserver_socket, buffer, buffer_size, 0)
  read( buffer(1:rc), *) w(1:d)
  deallocate(buffer)
end



subroutine zmq_ezfio_get_double_precision(cmd_in,w,d)
  implicit none
  use f77_zmq
  BEGIN_DOC
  ! Fetch a double precision variable
  END_DOC
  
  integer                        :: d
  double precision               :: w(d)
  character*(*)                  :: cmd_in
  character*(128)                :: cmd
  
  character(len=:), allocatable  :: buffer
  integer                        :: buffer_size
  character*(20)                 :: buffer_size_char
  integer                        :: rc
  
  cmd = 'get_'//cmd_in
  rc = f77_zmq_send(zmq_to_dataserver_socket, 'Ezfio', 5, ZMQ_SNDMORE)
  rc = f77_zmq_send(zmq_to_dataserver_socket, cmd, len_trim(cmd), 0)
  
  rc = f77_zmq_recv(zmq_to_dataserver_socket, buffer_size_char, len(buffer_size_char), ZMQ_SNDMORE)
  read(buffer_size_char(1:rc),*) buffer_size
  allocate (character(len=buffer_size) :: buffer)
  
  rc = f77_zmq_recv(zmq_to_dataserver_socket, buffer, buffer_size, 0)
  read( buffer(1:rc), *) w(1:d)
  
  deallocate(buffer)
end


subroutine zmq_ezfio_get_integer(cmd_in,w,d)
  implicit none
  use f77_zmq
  BEGIN_DOC
  ! Fetch an integer variable
  END_DOC
  
  integer                        :: d
  integer                        :: w(d)
  character*(*)                  :: cmd_in
  character*(128)                :: cmd
  
  character(len=:), allocatable  :: buffer
  integer                        :: buffer_size
  character*(20)                 :: buffer_size_char
  integer                        :: rc
  
  cmd = 'get_'//cmd_in
  rc = f77_zmq_send(zmq_to_dataserver_socket, 'Ezfio', 5, ZMQ_SNDMORE)
  rc = f77_zmq_send(zmq_to_dataserver_socket, cmd, len_trim(cmd), 0)
  
  rc = f77_zmq_recv(zmq_to_dataserver_socket, buffer_size_char, len(buffer_size_char), ZMQ_SNDMORE)
  read(buffer_size_char(1:rc),*) buffer_size
  allocate (character(len=buffer_size) :: buffer)
  
  rc = f77_zmq_recv(zmq_to_dataserver_socket, buffer, buffer_size, 0)
  read( buffer(1:rc), *) w(1:d)
  
  deallocate(buffer)
end


subroutine zmq_ezfio_get_integer8(cmd_in,w,d)
  implicit none
  use f77_zmq
  BEGIN_DOC
  ! Fetch an integer*8 variable
  END_DOC
  
  integer                        :: d
  integer*8                      :: w(d)
  character*(*)                  :: cmd_in
  character*(128)                :: cmd
  
  character(len=:), allocatable  :: buffer
  integer                        :: buffer_size
  character*(20)                 :: buffer_size_char
  integer                        :: rc
  
  cmd = 'get_'//cmd_in
  rc = f77_zmq_send(zmq_to_dataserver_socket, 'Ezfio', 5, ZMQ_SNDMORE)
  rc = f77_zmq_send(zmq_to_dataserver_socket, cmd, len_trim(cmd), 0)
  
  rc = f77_zmq_recv(zmq_to_dataserver_socket, buffer_size_char, len(buffer_size_char), ZMQ_SNDMORE)
  read(buffer_size_char(1:rc),*) buffer_size
  allocate (character(len=buffer_size) :: buffer)
  
  rc = f77_zmq_recv(zmq_to_dataserver_socket, buffer, buffer_size, 0)
  read( buffer(1:rc), *) w(1:d)
  
  deallocate(buffer)
end


subroutine zmq_ezfio_get_real(cmd_in,w,d)
  implicit none
  use f77_zmq
  BEGIN_DOC
  !  Fetch a real variable
  END_DOC
  
  integer                        :: rc
  
  integer                        :: d
  real                           :: w(d)
  character*(*)                  :: cmd_in
  character*(128)                :: cmd
  
  character(len=:), allocatable  :: buffer
  integer                        :: buffer_size
  character*(20)                 :: buffer_size_char
  
  cmd = 'get_'//cmd_in
  rc = f77_zmq_send(zmq_to_dataserver_socket, 'Ezfio', 5, ZMQ_SNDMORE)
  rc = f77_zmq_send(zmq_to_dataserver_socket, cmd, len_trim(cmd), 0)
  
  rc = f77_zmq_recv(zmq_to_dataserver_socket, buffer_size_char, len(buffer_size_char), ZMQ_SNDMORE)
  read(buffer_size_char(1:rc),*) buffer_size
  allocate (character(len=buffer_size) :: buffer)
  
  rc = f77_zmq_recv(zmq_to_dataserver_socket, buffer, buffer_size, 0)
  read( buffer(1:rc), *) w(1:d)
  
  deallocate(buffer)
end


subroutine zmq_ezfio_get_character(cmd_in,w,d)
  implicit none
  use f77_zmq
  BEGIN_DOC
  !  Fetch a text variable
  END_DOC
  
  integer                        :: rc
  
  integer                        :: d
  character*(*)                  :: w
  character*(*)                  :: cmd_in
  character*(128)                :: cmd
  
  character(len=:), allocatable  :: buffer
  integer                        :: buffer_size
  character*(20)                 :: buffer_size_char
  
  cmd = 'get_'//cmd_in
  rc = f77_zmq_send(zmq_to_dataserver_socket, 'Ezfio', 5, ZMQ_SNDMORE)
  rc = f77_zmq_send(zmq_to_dataserver_socket, cmd, len_trim(cmd), 0)
  
  rc = f77_zmq_recv(zmq_to_dataserver_socket, buffer_size_char, len(buffer_size_char), ZMQ_SNDMORE)
  read(buffer_size_char(1:rc),*) buffer_size
  allocate (character(len=buffer_size) :: buffer)
  
  rc = f77_zmq_recv(zmq_to_dataserver_socket, buffer, buffer_size, 0)
  read( buffer(1:rc), '(A)') w
  
  deallocate(buffer)
end


