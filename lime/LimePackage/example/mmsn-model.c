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

const int mol_mode_id=0;
char* moldata_file_name[3] = {"hco+@xpol.dat","dco+@xpol.dat", "n2h+@xpol.dat"};
char* img_file_name[3] = {"image-hco+.fits","image-dco+.fits", "image-n2h+.fits"};
// double line_freq[3] = {267.5648e9, 216.1204e9, 279.5175e9};

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
  par->moldatfile[0] 	= moldata_file_name[mol_mode_id];

  par->sampling			= 0;

  par->outputfile 		= "populations.pop";
  par->gridfile			= "grid.vtk";

  /* 
   * Definitions for image #0. Add blocks for additional images.
   */
  
  img[0].nchan			= 40;		  // Number of channels
  img[0].velres			= 50.;       // Channel resolution in m/s
  img[0].trans			= 2;          // zero-indexed J quantum number (2 indicates 3-2 transition.)
  img[0].pxls			= 400;	      // Pixels per dimension
  img[0].imgres			= 0.05;		  // Resolution in arc seconds
  img[0].theta			= 0.122;		  // 0: face-on, pi/2: edge-on
  img[0].distance		= 56*PC;	  // source distance in m
  img[0].source_vel		= 0;          // source velocity in m/s
  img[0].unit			= 1;		  // 0:Kelvin 1:Jansky/pixel 2:SI 3:Lsun/pixel 4:tau
  img[0].filename		= img_file_name[mol_mode_id];	// Output filename

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
  density[0] = 4.09e14 * exp(-z*z/(2*h*h)) * pow(r/AU, -1.5)
    * exp(-r/(300*AU));
  if (r < 0.1*AU) density[0] = 0;
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
  //  double aba[3] = {2.2e-10, 4.2e-15, 2.8e-12};
    double aba[3] = {1e-8, 0.66e-10, 2.8e-12};

  abundance[0] = aba[mol_mode_id];

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
  double tmp[1];
  temperature(x,y,z,tmp);

  *doppler = sqrt(284.6 * tmp[0]);
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
  vel[0]=-sin(phi)*v;
  vel[1]= cos(phi)*v;
  vel[2]=0;
}

/******************************************************************************/


