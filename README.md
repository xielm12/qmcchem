QMC=Chem : Quantum Monte Carlo for Chemistry
============================================


QMC=Chem is the quantum Monte Carlo program of the
[Toulouse group](http://qmcchem.ups-tlse.fr).
It is meant to be used in the *post-Full-CI* context : a quasi-Full-CI
calculation is done with the
[quantum package](https://github.com/LCPQ/quantum_package),
and this wave function is used as a trial wave function for the fixed-node
diffusion Monte Carlo algorithm.


* Parallel efficiency of 98.3% on 16_000 cores
* The load balancing is optimal: the workers always work 100% of the time,
  independently of their respective CPU speeds
* Efficient: 0.96 Pflops/s on 76_800 cores of Curie in 2011
* All network communications are non-blocking,
  with the [ZeroMQ](http://zeromq.org) library
* All the implemented algorithms are CPU-bound : the only limit
  is the available CPU time
* The number of simultaneous worker nodes can be variable during a calculation
* Fully fault-tolerant (crashing nodes don't stop the running calculation)
* QMC=Chem has been used in grid environments (EGI european grid) and 
  in Cloud environments (rance Grilles) coupled to supercomputers 


*Warning*: QMC=Chem is under the GPLv2 license. Any modifications to or
software including (via compiler) GPL-licensed code must also be made available
under the GPL along with build & install instructions.

Example of a QMC=Chem calculation
---------------------------------

Calculation with the quantum package
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

1) Create the ``xyz`` file containing the nuclear coordinates of the system

```bash
$ cat > h2o.xyz << EOF
3
Water molecule
O 0. 0. 0.
H 0.9572 0. 0.
H -0.239987 0.926627 0.
EOF
```

2) Choose a suitable basis set and create the [EZFIO database](https://github.com/LCPQ/ezfio)

```bash
$ qp_create_ezfio_from_xyz -b cc-pvdz h2o.xyz -o h2o
```

3) Run the SCF calculation

```bash
$ qp_run SCF h2o
```
4) Run the CIPSI calculation

```bash
$ qp_run full_ci h2o
```

5) Transform the input for use in QMC=Chem

```bash
$ qp_run save_for_qmcchem h2o
```

FN-DMC calculation with QMC=Chem
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Before using QMC=Chem, you need to load the environment variables:

```bash
$ source qmcchem.rc
```

In QMC=Chem, everything goes through the use of the ``qmcchem`` command.
When a command is run with no arguments, it prints a help message.
This is mainly the manual of QMC=Chem. For example:

```bash
$ qmcchem 
QMC=Chem command

  qmcchem SUBCOMMAND

=== subcommands ===

  debug    Debug ZeroMQ communications
  edit     Edit input data
  md5      Manipulate input MD5 keys
  result   Displays the results computed in an EZFIO directory.
  run      Run a calculation
  stop     Stop a running calculation
  version  print version information
  help     explain a given subcommand (perhaps recursively)

missing subcommand for command qmcchem

$ qmcchem edit
Run a calculation

  qmcchem run EZFIO_FILE


Run QMC=Chem
      

=== flags ===

  [-a]                    Add more resources to a running calculation.
  [-d]                    Start a dataserver process on the local host.
  [-q <dataserver_addr>]  Start a qmc process on the local host.
  [-s <host>]             Start a qmc process on <host>.
  [-help]                 print this help text and exit
                          (alias: -?)

missing anonymous argument: EZFIO_FILE
```

1) Set the parameters for a VMC calculation to create initial walker positions

```bash
$ qmcchem edit -h
Edit input data

  qmcchem edit EZFIO_FILE [INPUT]


Edit input data
      

=== flags ===

  [-c]                Clear blocks
  [-e energy]         Fixed reference energy to normalize DMC weights
  [-f 0|1]            Correct wave function to verify electron-nucleus cusp
                      condition
  [-j jastrow_type]   Type of Jastrow factor [ None | Core | Simple ]
  [-l seconds]        Length (seconds) of a block
  [-m method]         QMC Method : [ VMC | DMC ]
  [-n norm]           Truncation t of the wave function : Remove determinants
                      with a
                      contribution to the norm less than t
  [-s sampling]       Sampling algorithm : [ Langevin | Brownian ]
  [-t seconds]        Requested simulation time (seconds)
  [-ts time_step]     Simulation time step
  [-w walk_num]       Number of walkers per CPU core
  [-wt walk_num_tot]  Total number of stored walkers for restart
  [-help]             print this help text and exit
                      (alias: -?)

$ qmcchem edit h2o -f 1 -m VMC -n 1.e-5 -s Langevin -t 300 -l 10
```

3) Get info on the wave function

```bash
$ qmcchem info h2o
```

4) Run the VMC calculation

```bash
$ qmcchem run h2o
```

5) Set the correct parameters for FN-DMC

```bash
$ qmcchem edit h2o -e -76.438 -m DMC -s Brownian -ts 3.e-4 -t 3600 -l 30
```

6) Run the FN-DMC calculation

```bash
$ qmcchem run h2o
```

7) Print the result

```bash
$ qmcchem result h2o

```




References
----------

[Quantum Monte Carlo for large chemical systems: Implementing efficient strategies for petascale platforms and beyond](http://dx.doi.org/10.1002/jcc.23216)
> Anthony Scemama , Michel Caffarel , Emmanuel Oseret and William Jalby (2013), in: Journal of Computational Chemistry, 34:11(938--951) 

[Quantum Monte Carlo with very large multideterminant wavefunctions](http://arxiv.org/abs/1509.03114)
> Anthony Scemama , Thomas Applencourt , Emmanuel Giner and Michel Caffarel (2015), in: ArXiv ePrints:arXiv:1510.00730 [physics.chem-ph] 

