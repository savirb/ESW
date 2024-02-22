c#######################################################################
c
c ****** This tool uses modules from ZM's tools library.
c
c#######################################################################
      module ident
c
      character(*), parameter :: cname='GETV'
      character(*), parameter :: cvers='1.10'
      character(*), parameter :: cdate='04/29/2021'
c
      end module
c#######################################################################
      module params
c
c-----------------------------------------------------------------------
c ****** Parameters.
c-----------------------------------------------------------------------
c
      use number_types
c
      implicit none
c
c ****** Input and output file names.
c
      character(512) :: params_file
      character(512) :: chd_file
      character(512) :: ef_file
      character(512) :: v_file
      character(512) :: rho_file
      character(512) :: t_file
c
c ****** Model type selected.
c
      character(16) :: model_type
c
c ****** Verbosity.
c
      logical :: verbose
c
      end module
c#######################################################################
      module wsa_params
c
c-----------------------------------------------------------------------
c ****** Parameters for the WSA model.
c-----------------------------------------------------------------------
c
      use number_types
c
      implicit none
c
      real(r_typ) :: wsa_vslow=250._r_typ
      real(r_typ) :: wsa_vfast=680._r_typ
      real(r_typ) :: wsa_vmax=800._r_typ
      real(r_typ) :: wsa_ef_power=1._r_typ/3._r_typ
      real(r_typ) :: wsa_chd_mult_fac=.8_r_typ
      real(r_typ) :: wsa_chd_arg_fac=.25_r_typ
      real(r_typ) :: wsa_chd_power=4._r_typ
      real(r_typ) :: wsa_c5=3._r_typ
c
      end module
c#######################################################################
      module psi_params
c
c-----------------------------------------------------------------------
c ****** Parameters for the PSI empirical solar wind speed model.
c-----------------------------------------------------------------------
c
      use number_types
c
      implicit none
c
      real(r_typ) :: psi_vslow=250._r_typ
      real(r_typ) :: psi_vfast=650._r_typ
      real(r_typ) :: psi_eps=.05_r_typ
      real(r_typ) :: psi_width=.025_r_typ
c
      end module
c#######################################################################
      module rho_t_params
c
c-----------------------------------------------------------------------
c ****** Parameters for the PSI empirical solar wind speed model.
c-----------------------------------------------------------------------
c
      use number_types
c
      implicit none
c
      real(r_typ) :: rhofast=152._r_typ
      real(r_typ) :: tfast=1.85e6_r_typ
c
      end module
c#######################################################################
      program GETV
c
c-----------------------------------------------------------------------
c
      use ident
      use number_types
      use sds_def
      use params
      use wsa_params
      use psi_params
      use rho_t_params
c
c-----------------------------------------------------------------------
c
      implicit none
c
c-----------------------------------------------------------------------
c
      real(r_typ), parameter :: zero=0.
      real(r_typ), parameter :: one=1._r_typ
      real(r_typ), parameter :: half=.5_r_typ
      real(r_typ), parameter :: rad_to_deg=57.2957795130823_r_typ
c
c-----------------------------------------------------------------------
c
      type(sds) :: chd,ef
      type(sds) :: v,rho,temp
c
      real(r_typ), dimension(:), pointer :: t,p
c
      integer :: nt,np
      integer :: ierr,i,j
      real(r_typ) :: chd_value,chd_deg,chd_arg,
     &               chd_factor,ef_factor,ef_value,
     &               profile,vmax
c
c-----------------------------------------------------------------------
c
c ****** Parameters for the WSA model.
c
      namelist /wsa/ wsa_vslow,
     &               wsa_vfast,
     &               wsa_vmax,
     &               wsa_ef_power,
     &               wsa_chd_mult_fac,
     &               wsa_chd_arg_fac,
     &               wsa_chd_power,
     &               wsa_c5
c
c ****** Parameters for the MAS empirical speed model.
c
      namelist /psi/ psi_vslow,
     &               psi_vfast,
     &               psi_eps,
     &               psi_width
c
c ****** Parameters for the density and temperature.
c
      namelist /rhot/ rhofast,tfast
c
c-----------------------------------------------------------------------
c
c ****** Set the parameters.
c
      call set_parameters
c
c ****** Check that the model type requested is one of the
c ****** allowed models.
c
      if (model_type.eq.'WSA') then
      else if (model_type.eq.'WSA2') then
      else if (model_type.eq.'PSI') then
      else
        write (*,*)
        write (*,*) '### ERROR in ',cname,':'
        write (*,*) '### Invalid model type requested: ',
     &              trim(model_type)
        write (*,*)
        write (*,*) 'The allowed model types are:'
        write (*,*) '  WSA'
        write (*,*) '  WSA2'
        write (*,*) '  PSI'
        call exit (1)
      end if
c
c ****** Check that the user did not specify an expansion factor
c ****** file when using the PSI ad hoc model.  This is probably
c ****** a usage error, since the PSI ad hoc model does not require
c ****** an expansion factor file.
c
c ****** Also, check that the expansion factor was specified when
c ****** using the WSA model, as required.
c
      if (model_type.eq.'PSI') then
        if (ef_file.ne.' ') then
          write (*,*)
          write (*,*) '### ERROR in ',cname,':'
          write (*,*) '### The expansion factor file was'//
     &                ' specified when using the PSI ad hoc model.'
          write (*,*) '### The PSI ad hoc model does not use the'//
     &                ' expansion factor.'
          call exit (1)
        end if
      else if (model_type.eq.'WSA'.or.model_type.eq.'WSA2') then
        if (ef_file.eq.' ') then
          write (*,*)
          write (*,*) '### ERROR in ',cname,':'
          write (*,*) '### The expansion factor file was'//
     &                ' not specified.'
          write (*,*) '### The WSA/WSA2 model requires an'//
     &                ' expansion factor to be specified.'
          call exit (1)
        end if
      end if
c
c ****** Read the input parameters file.
c
      if (params_file.ne.' ') then
        if (verbose) then
          write (*,*)
          write (*,*) 'Reading input parameters from file: ',
     &                trim(params_file)
        end if
        call ffopen (1,params_file,'r',ierr)
        if (ierr.ne.0) then
          write (*,*)
          write (*,*) '### ERROR in ',cname,':'
          write (*,*) '### Could not open the input parameters file:'
          write (*,*) 'File name: ',trim(params_file)
          call exit (1)
        end if
        if (model_type.eq.'WSA'.or.model_type.eq.'WSA2') then
          read (1,wsa)
        else if (model_type.eq.'PSI') then
          read (1,psi)
        end if
        read(1,rhot)
        close (1)
      end if
c
c ****** Read the coronal hole distance file.
c
      if (verbose) then
        write (*,*)
        write (*,*) 'Reading coronal hole distance file: ',
     &              trim(chd_file)
      end if
c
      call rdhdf (chd_file,chd,ierr)
c
      if (ierr.ne.0) then
        write (*,*)
        write (*,*) '### ERROR in ',cname,':'
        write (*,*) '### Could not read the coronal hole distance'//
     &              ' file.'
        write (*,*) 'IERR (from RDSDS) = ',ierr
        write (*,*) 'File name: ',trim(chd_file)
        call exit (1)
      end if
c
c ****** Check that the coronal hole distance file has a 2D data set.
c
      if (chd%ndim.ne.2) then
        write (*,*)
        write (*,*) '### ERROR in ',cname,':'
        write (*,*) '### The coronal hole distance file does'//
     &              ' not have a 2D data set.'
        write (*,*) 'File name: ',trim(chd_file)
        write (*,*) 'Number of dimensions = ',chd%ndim
        call exit (1)
      end if
c
c ****** Check that the coronal hole distance file has scales.
c
      if (.not.chd%scale) then
        write (*,*)
        write (*,*) '### ERROR in ',cname,':'
        write (*,*) '### The coronal hole distance file does not'//
     &              ' have scales.'
        write (*,*) 'File name: ',trim(chd_file)
        call exit (1)
      end if
c
c ****** Read the expansion factor file (when using the WSA model).
c
      if (model_type.eq.'WSA'.or.model_type.eq.'WSA2') then
c
        if (verbose) then
          write (*,*)
          write (*,*) 'Reading expansion factor file: ',
     &                trim(ef_file)
        end if
c
        call rdhdf (ef_file,ef,ierr)
c
        if (ierr.ne.0) then
          write (*,*)
          write (*,*) '### ERROR in ',cname,':'
          write (*,*) '### Could not read the expansion factor'//
     &                ' file.'
          write (*,*) 'IERR (from RDSDS) = ',ierr
          write (*,*) 'File name: ',trim(ef_file)
          call exit (1)
        end if
c
c ****** Check that the expansion factor file has a 2D data set.
c
        if (ef%ndim.ne.2) then
          write (*,*)
          write (*,*) '### ERROR in ',cname,':'
          write (*,*) '### The expansion factor file does'//
     &                ' not have a 2D data set.'
          write (*,*) 'File name: ',trim(ef_file)
          write (*,*) 'Number of dimensions = ',ef%ndim
          call exit (1)
        end if
c
c ****** Check that the expansion factor file has scales.
c
        if (.not.ef%scale) then
          write (*,*)
          write (*,*) '### ERROR in ',cname,':'
          write (*,*) '### The expansion factor file does not'//
     &                ' have scales.'
          write (*,*) 'File name: ',trim(ef_file)
          call exit (1)
        end if
c
      end if
c
c ****** Write the parameters, if requested.
c
      if (verbose) then
        if (model_type.eq.'WSA'.or.model_type.eq.'WSA2') then
          write (*,*)
          write (*,*) '### Computing the speed for the WSA model:'
          write (*,*)
          write (*,*) 'Slow wind speed [km/s]    = ',wsa_vslow
          write (*,*) 'Fast wind speed [km/s]    = ',wsa_vfast
          write (*,*) 'Maximum wind speed [km/s] = ',wsa_vmax
          write (*,*) 'Expansion factor power [WSA_EF_POWER]     = ',
     &                wsa_ef_power
          write (*,*) 'CH distance multiplier [WSA_CHD_MULT_FAC] = ',
     &                wsa_chd_mult_fac
          write (*,*) 'CH distance factor [WSA_CHD_ARG_FAC]      = ',
     &                wsa_chd_arg_fac
          write (*,*) 'CH distance power [WSA_CHD_POWER]         = ',
     &                wsa_chd_power
        else if (model_type.eq.'PSI') then
          write (*,*)
          write (*,*) '### Computing the speed for the PSI ad'//
     &                ' hoc model:'
          write (*,*)
          write (*,*) 'Slow wind speed [km/s]    = ',psi_vslow
          write (*,*) 'Fast wind speed [km/s]    = ',psi_vfast
          write (*,*) 'CH distance parameter [PSI_EPS] = ',
     &                psi_eps
          write (*,*) 'CH distance width [PSI_WIDTH]   = ',
     &                psi_width
        end if
        write (*,*)
        write (*,*) '### Computing the density and temperature:'
        write (*,*)
        write (*,*) 'Fast wind density  [#/cm3]    = ',rhofast
        write (*,*) 'Fast wind temperature [K]    = ',tfast
      end if
c
c ****** Use the scales from the coronal hole distance file
c ****** to compute the speed.
c
      nt=chd%dims(1)
      np=chd%dims(2)
c
      t=>chd%scales(1)%f
      p=>chd%scales(2)%f
c
c ****** Allocate the storage.
c
      allocate (v%f(nt,np,1))
      allocate (rho%f(nt,np,1))
      allocate (temp%f(nt,np,1))
c
      if (model_type.eq.'WSA') then
c
c ****** Compute the WSA solar wind speed.
c
        do i=1,nt
          do j=1,np
            chd_value=chd%f(i,j,1)
            chd_deg=chd_value*rad_to_deg
            chd_arg=wsa_chd_arg_fac*chd_deg
            chd_factor=one-wsa_chd_mult_fac*exp(-chd_arg**wsa_chd_power)
            call interp_2d (ef%dims(1),ef%dims(2),
     &                      ef%scales(1)%f,ef%scales(2)%f,
     &                      zero,ef%f,t(i),p(j),ef_value)
            ef_factor=(one+ef_value)**wsa_ef_power
            v%f(i,j,1)=wsa_vslow+(wsa_vfast/ef_factor)*chd_factor
            v%f(i,j,1)=min(v%f(i,j,1),wsa_vmax)
          enddo
        enddo
c
      else if (model_type.eq.'WSA2') then
c
c ****** Compute the WSA2 solar wind speed.
c
        do i=1,nt
          do j=1,np
            chd_value=chd%f(i,j,1)
            chd_deg=chd_value*rad_to_deg
            chd_arg=wsa_chd_arg_fac*chd_deg
            chd_factor=(one
     &         -wsa_chd_mult_fac*exp(-chd_arg**wsa_chd_power))**wsa_c5
            call interp_2d (ef%dims(1),ef%dims(2),
     &                      ef%scales(1)%f,ef%scales(2)%f,
     &                      zero,ef%f,t(i),p(j),ef_value)
            ef_factor=(one+ef_value)**wsa_ef_power
            v%f(i,j,1)=wsa_vslow+(wsa_vfast/ef_factor)*chd_factor
          enddo
        enddo
c
      else if (model_type.eq.'PSI') then
c
c ****** Compute the PSI ad hoc solar wind speed.
c
        do i=1,nt
          do j=1,np
            chd_value=chd%f(i,j,1)
            profile=half*(one+tanh((chd_value-psi_eps)/psi_width))
            v%f(i,j,1)=psi_vslow+(psi_vfast-psi_vslow)*profile
          enddo
        enddo
c
      end if
c
c ****** Compute the ad hoc density and temperature from
c ****** pressure balance, as is done in the original MAS_IP.
c
      vmax=maxval(v%f)
      do i=1,nt
        do j=1,np
          rho%f(i,j,1)=rhofast*(vmax/v%f(i,j,1))**2
          temp%f(i,j,1)=tfast*rhofast/rho%f(i,j,1)
        enddo
      enddo
c
c ****** Write the speed to the output file.
c
      v%ndim=2
      v%dims(1)=nt
      v%dims(2)=np
      v%dims(3)=1
      v%scale=.true.
      v%hdf32=chd%hdf32
      v%scales(1)%f=>t
      v%scales(2)%f=>p
      v%scales(3)%f=>t
c
      call wrhdf (v_file,v,ierr)
c
      if (ierr.ne.0) then
        write (*,*)
        write (*,*) '### ERROR in ',cname,':'
        write (*,*) '### Could not write the speed '//
     &              ' file.'
        write (*,*) 'File name: ',trim(v_file)
        call exit (1)
      end if
c
      if (verbose) then
        write (*,*)
        write (*,*) 'Wrote the speed to file: ',
     &              trim(v_file)
      end if
c
c ****** Write the density to the output file.
c
      rho%ndim=2
      rho%dims(1)=nt
      rho%dims(2)=np
      rho%dims(3)=1
      rho%scale=.true.
      rho%hdf32=chd%hdf32
      rho%scales(1)%f=>t
      rho%scales(2)%f=>p
      rho%scales(3)%f=>t
c
      call wrhdf (rho_file,rho,ierr)
c
      if (ierr.ne.0) then
        write (*,*)
        write (*,*) '### ERROR in ',cname,':'
        write (*,*) '### Could not write the density '//
     &              ' file.'
        write (*,*) 'File name: ',trim(rho_file)
        call exit (1)
      end if
c
      if (verbose) then
        write (*,*)
        write (*,*) 'Wrote the density to file: ',
     &              trim(rho_file)
      end if
c
c ****** Write the temperature to the output file.
c
      temp%ndim=2
      temp%dims(1)=nt
      temp%dims(2)=np
      temp%dims(3)=1
      temp%scale=.true.
      temp%hdf32=chd%hdf32
      temp%scales(1)%f=>t
      temp%scales(2)%f=>p
      temp%scales(3)%f=>t
c
      call wrhdf (t_file,temp,ierr)
c
      if (ierr.ne.0) then
        write (*,*)
        write (*,*) '### ERROR in ',cname,':'
        write (*,*) '### Could not write the temperature '//
     &              ' file.'
        write (*,*) 'File name: ',trim(t_file)
        call exit (1)
      end if
c
      if (verbose) then
        write (*,*)
        write (*,*) 'Wrote the temperature to file: ',
     &              trim(t_file)
      end if
c
      call exit (0)
c
      end
c#######################################################################
      subroutine interp_2d (nx,ny,x,y,oval,f,xv,yv,fv)
c
c-----------------------------------------------------------------------
c
c ****** Interpolate the value of the 2D field FV at (XV,YV) from
c ****** array F(NX,NY), defined on the mesh X(NX) x Y(NY).
c
c ****** Note that if the point (XV,YV) is outside the bounds of
c ****** the X x Y mesh, FV=OVAL is returned.
c
c-----------------------------------------------------------------------
c
      use number_types
c
c-----------------------------------------------------------------------
c
      implicit none
c
c-----------------------------------------------------------------------
c
      integer :: nx,ny
      real(r_typ), dimension(nx) :: x
      real(r_typ), dimension(ny) :: y
      real(r_typ) :: oval
      real(r_typ), dimension(nx,ny) :: f
      real(r_typ) :: xv,yv,fv
c
c-----------------------------------------------------------------------
c
      real(r_typ), parameter :: one=1._r_typ
c
c-----------------------------------------------------------------------
c
      integer :: i,j,ip1,jp1
      real(r_typ) :: ax,ay
c
c-----------------------------------------------------------------------
c
c ****** If the point is outside the data limits, return the
c ****** value OVAL.
c
      if (xv.lt.x(1).or.xv.gt.x(nx).or.
     &    yv.lt.y(1).or.yv.gt.y(ny)) then
        fv=oval
        return
      end if
c
      call interp (nx,x,xv,i,ip1,ax)
      call interp (ny,y,yv,j,jp1,ay)
c
      fv= (one-ax)*( (one-ay)*f(i  ,j  )
     &              +     ay *f(i  ,jp1))
     &   +     ax *( (one-ay)*f(ip1,j  )
     &              +     ay *f(ip1,jp1))
c
      return
      end
c#######################################################################
      subroutine interp (n,x,xv,i,ip1,alpha)
c
c-----------------------------------------------------------------------
c
c ****** Find the interval I in table X(i), i=1,2,...,N, that encloses
c ****** the value XV, such that X(I).le.XV.le.X(I+1).
c ****** For the special case when N=1, XV must equal X(1) exactly.
c
c ****** This routine uses LOCATE_INTERVAL (from the SPLINE library)
c ****** to do the actual work.  If the interval is not found
c ****** LOCATE_INTERVAL terminates with an error.
c
c ****** This routine does not do the actual interpolation.  However,
c ****** the returned values of I, IP1 (which generally equals I+1),
c ****** and ALPHA can be used to get the interpolant.
c
c-----------------------------------------------------------------------
c
      use number_types
      use locate_interval_interface
c
c-----------------------------------------------------------------------
c
      implicit none
c
c-----------------------------------------------------------------------
c
      integer :: n
      real(r_typ), dimension(n) :: x
      real(r_typ) :: xv
      integer :: i
      integer :: ip1
      real(r_typ) :: alpha
      intent(in) :: n,x,xv
      intent(out) :: i,ip1,alpha
c
c-----------------------------------------------------------------------
c
      i=locate_interval(n,x,xv)
c
      if (n.eq.1) then
        ip1=1
        alpha=0.
      else
        ip1=i+1
        if (x(i).eq.x(i+1)) then
          alpha=0.
        else
          alpha=(xv-x(i))/(x(i+1)-x(i))
        end if
      end if
c
      return
      end
c#######################################################################
      subroutine set_parameters
c
c-----------------------------------------------------------------------
c
c ****** Set parameters from the command line arguments.
c
c-----------------------------------------------------------------------
c
      use ident
      use number_types
      use syntax
      use paragraph_def
      use get_usage_line_interface
      use print_par_interface
      use delete_par_interface
      use ucase_interface
      use params
c
c-----------------------------------------------------------------------
c
      implicit none
c
c-----------------------------------------------------------------------
c
c ****** Storage the for usage line.
c
      type(paragraph), pointer :: usage
c
c ****** Storage for the error message.
c
      character(72) :: errmsg
c
c-----------------------------------------------------------------------
c
      integer :: ierr
      character(256) :: arg
      logical :: set
c
c-----------------------------------------------------------------------
c
c ****** Define the syntax.
c
      call defarg (GROUP_K ,'-v',' ',' ')
      call defarg (GROUP_KA,'-model',' ','<type>')
      call defarg (GROUP_KA,'-params','<default>','<file>')
      call defarg (GROUP_KA,'-chd',' ','<file>')
      call defarg (GROUP_KA,'-ef','<default>','<file>')
      call defarg (GROUP_A ,'vfile',' ',' ')
      call defarg (GROUP_A ,'rhofile',' ',' ')
      call defarg (GROUP_A ,'tfile',' ',' ')
c
c ****** Parse the command line.
c
      call parse (errmsg,ierr)
c
      if (ierr.ne.0) then
c
        write (*,*)
        write (*,*) '### ',cname,' Version ',cvers,' of ',cdate,'.'
        write (*,*) '### Compute the empirical solar'//
     &              ' wind speed.'
c
        if (ierr.gt.1) then
          write (*,*)
          write (*,*) errmsg
        end if
c
c ****** Print the usage line.
c
        call get_usage_line (usage)
c
        write (*,*)
        write (*,*) 'Usage:'
        write (*,*)
c
        call print_par (usage)
c
        write (*,*)
        write (*,*) 'Use -model to specify the model type.'//
     &              '  The allowed model types are:'
        write (*,*)
        write (*,*) '   WSA: The Wang-Sheeley-Arge model, in which'//
     &              ' the solar wind speed depends on'
        write (*,*) '        the expansion factor and the distance'//
     &              ' to the coronal hole boundary'
        write (*,*) '   WSA2: The Wang-Sheeley-Arge model, in which'//
     &              ' the solar wind speed depends on'
        write (*,*) '        the expansion factor and the distance'//
     &              ' to the coronal hole boundary - version 2'
        write (*,*)
        write (*,*) '   PSI: The PSI ad hoc model, in which the'//
     &              ' solar wind speed depends on the'
        write (*,*) '        distance to the coronal hole boundary'
        write (*,*)
        write (*,*) 'Use -params to specify an input file'//
     &              ' (in NAMELIST format) that contains the'
        write (*,*) 'parameters for the models.  The parameters'//
     &              ' are specified in NAMELIST blocks'
        write (*,*) 'named "wsa" and "psi".  If this file is'//
     &              ' not specified, the default parameter'
        write (*,*) 'values are used.'
        write (*,*)
        write (*,*) 'Use -chd to specify the 2D HDF file that'//
     &              ' contains the distance to the coronal'
        write (*,*) 'hole boundary (in radians).'
        write (*,*)
        write (*,*) 'Use -ef to specify the 2D HDF file that'//
     &              ' contains the expansion factor.'
        write (*,*) 'This should only be specified for the WSA model.'
        write (*,*)
        write (*,*) 'These files should have the same order of'//
     &              ' scales, but do not need to have the'
        write (*,*) 'same dimensions.  The scales for the'//
     &              ' computed speed are taken from those of'
        write (*,*) 'the coronal hole boundary distance file.'
        write (*,*)
        write (*,*) 'The estimated solar wind speed [km/s]'//
     &              ' is written to HDF file <vfile>.'
        write (*,*)
        write (*,*) 'The estimated solar wind density [#/cm3]'//
     &              ' is written to HDF file <rhofile>.'
        write (*,*)
        write (*,*) 'The estimated solar wind temperature [K]'//
     &              ' is written to HDF file <tfile>.'
        call delete_par (usage)
c
        call exit (1)
c
      end if
c
c ****** Set the parameters.
c
c ****** Verbose flag.
c
      call fetcharg ('-v',set,arg)
      verbose=set
c
c ****** Model type.
c
      call fetcharg ('-model',set,arg)
      model_type=ucase(trim(arg))
c
c ****** Parameter input file name.
c
      call fetcharg ('-params',set,arg)
      if (set) then
        params_file=trim(arg)
      else
        params_file=' '
      end if
c
c ****** Coronal hole distance file name.
c
      call fetcharg ('-chd',set,arg)
      chd_file=trim(arg)
c
c ****** Expansion factor file name.
c
      call fetcharg ('-ef',set,arg)
      if (set) then
        ef_file=trim(arg)
      else
        ef_file=' '
      end if
c
c ****** Output solar wind speed file name.
c
      call fetcharg ('vfile',set,arg)
      v_file=trim(arg)
c
c ****** Output solar wind speed file name.
c
      call fetcharg ('rhofile',set,arg)
      rho_file=trim(arg)
c
c ****** Output solar wind speed file name.
c
      call fetcharg ('tfile',set,arg)
      t_file=trim(arg)
c
      return
      end
