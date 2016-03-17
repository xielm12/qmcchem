      integer, parameter :: t_Brownian = 3
      integer, parameter :: t_Langevin = 4
      integer, parameter :: t_MTM      = 5
      integer, parameter :: t_Read     = 6

      integer, parameter :: t_VMC      = 7
      integer, parameter :: t_DMC      = 8
      integer, parameter :: t_SRMC     = 9
      integer, parameter :: t_FKMC     = 10

      integer, parameter :: t_Simple   = 11
      integer, parameter :: t_None     = 12
      integer, parameter :: t_Core     = 14

      integer, parameter :: t_Stopped  = 0
      integer, parameter :: t_Queued   = 1
      integer, parameter :: t_Running  = 2
      integer, parameter :: t_Stopping = 3

      character*(32)     :: types(15) = &
      (/  '               ',   &
          '               ',   &
          'Brownian       ',   &
          'Langevin       ',   &
          '               ',   &
          '               ',   &
          'VMC            ',   &
          'DMC            ',   &
          'SRMC           ',   &
          'FKMC           ',   &
          'Simple         ',   &
          'None           ',   &
          '               ',   &
          'Core           ',   &
          '               '/)

