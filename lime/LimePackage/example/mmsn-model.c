/*
 *  model.c
 *  LIME, The versatile 3D line modeling tool 
 *
 *  Created by Christian Brinch on 11/05/07.
 *  Copyright 2006-2013, Christian Brinch, 
 *  <brinch@nbi.dk>
 *  Niels Bohr institutet
 *  University of Copenhagen
 *	All rights reserved.
 *
 */

#include "math.h"
#include "lime.h"

char* img_file_name[3] = {"image-hco+.fits","image-n2h+.fits","image-dco+.fits"};

/******************************************************************************/

void
input(inputPars *par, image *img){
  int i;
  /*
   * Basic parameters. See cheat sheet for details.
   */
  par->radius			= 2000*AU;
  par->minScale	   		= 0.5*AU;
  par->pIntensity    	= 4000;
  par->sinkPoints    	= 3000;
  par->dust				= "jena_thin_e6.tab";
  par->moldatfile[0] 	= "hco+@xpol.dat";
  par->moldatfile[1] 	= "n2h+@xpol.dat";
  par->moldatfile[2] 	= "dco+@xpol.dat";

  par->sampling			= 0;

  par->outputfile 		= "populations.pop";
  par->gridfile			= "grid.vtk";

  /* 
   * Definitions for image #0. Add blocks for additional images.
   */
  
  for (i = 0; i < 3; ++i) {
    img[i].nchan			= 60;		  // Number of channels
    img[i].velres			= 500.;       // Channel resolution in m/s
    img[i].trans			= 3;          // zero-indexed J quantum number
    img[i].pxls			= 100;	      // Pixels per dimension
    img[i].imgres			= 0.1;		  // Resolution in arc seconds
    img[i].theta			= 0.0;		  // 0: face-on, pi/2: edge-on
    img[i].distance		= 140*PC;	  // source distance in m
    img[i].source_vel		= 0;          // source velocity in m/s
    img[i].unit			= 0;		  // 0:Kelvin 1:Jansky/pixel 2:SI 3:Lsun/pixel 4:tau
    img[i].filename		= img_file_name[i];	// Output filename
  }
}

/******************************************************************************/

void
density(double x, double y, double z, double *density){	
  /*
   * Define variable for radial coordinate
   */
  double r,h;


  /* 
   * Calculate radial distance from origin
   */
  r=sqrt(x*x+y*y);

  // calculate scaleheight
  h = 3.34e-2 * AU * pow(r/AU,1.25);

  
  /*
   * Calculate a spherical power-law density profile
   * (Multiply with 1e6 to go to SI-units)
   */
  density[0] = 4.09e14 * exp(-r*r/(2*h*h)) * pow(r/AU, -1.5);
}

/******************************************************************************/

void
temperature(double x, double y, double z, double *temperature){
  int i,x0=0;
  double r;
  /*
   * Calculate coordinate distance from origin
   */
  r=sqrt(x*x+y*y);
  temperature[0]=temperature[1]=
    280 * pow(r/AU, -0.5);
}

/******************************************************************************/

void
abundance(double x, double y, double z, double *abundance){
  /* 
   * Here we use a constant abundance. Could be a 
   * function of (x,y,z).
   */
  abundance[0] = 2.2e-10;
  abundance[1] = 2.8e-12;
  abundance[2] = 4.2e-15;
}

/******************************************************************************/

void
doppler(double x, double y, double z, double *doppler){
  /* 
   * 200 m/s as the doppler b-parameter. This
   * can be a function of (x,y,z) as well.
   * Note that *doppler is a pointer, not an array. 
   * Remember the * in front of doppler.
   */
  *doppler = 0.;
}

/******************************************************************************/

void
velocity(double x, double y, double z, double *vel){
  /*
   * Variables for spherical coordinates
   */
  double v, phi,r,theta;
  /*
   * Transform Cartesian coordinates into spherical coordinates
   */
  r=sqrt(x*x+y*y);
  theta=atan2(sqrt(x*x+y*y),z);
  phi=atan2(y,x);
  /*
   * Free-fall velocity in the radial direction onto a central 
   * mass of 1.0 solar mass
   */  
  v = sqrt(6.67384e-11 * 1.98892e30 / r);
  /*
   * Vector transformation back into Cartesian basis
   */
  vel[0]=v*cos(phi);
  vel[1]=v*sin(phi);
  vel[2]=0;
}

/******************************************************************************/


