/*
 *  
 *  A protoplanetary disk model for LIME, a line modeling tool 
 *
 *  based on model.c Created by Christian Brinch.
 */

#include "math.h"
#include "lime.h"

// double line_freq[3] = {267.5648e9, 216.1204e9, 279.5175e9};

/******************************************************************************/

void
input(inputPars *par, image *img){
  int i;
  /*
   * Basic parameters. See cheat sheet for details.
   */
  par->radius			= 500*AU;
  par->minScale	   		= 0.5*AU;
  par->pIntensity    	=  ParticleNumber;
  par->sinkPoints    	=  SinkParticleNumber;
  par->dust				= "material/lime/jena_thin_e6.tab";
  par->moldatfile[0] 	= MoldataFileName;

  par->sampling			= 0;

  par->outputfile 		= "material/lime/populations.pop";
  par->gridfile			= "material/lime/grid.vtk";
  
  /* 
   * Definitions for image #0. Add blocks for additional images.
   */
  
  img[0].nchan			= VelocityChannelNumber;		  // Number of channels
  img[0].velres			= VelocityResolution;       // Channel resolution in m/s
  img[0].trans			= 2;          // zero-indexed J quantum number (2 indicates 3-2 transition.)
  img[0].pxls			= 400;	      // Pixels per dimension
  img[0].imgres			= 0.025;  // Resolution in arc seconds
  img[0].theta			= 0.122;		  // 0: face-on, pi/2: edge-on
  img[0].distance		= 56*PC;	  // source distance in m
  img[0].source_vel		= 0;          // source velocity in m/s
  img[0].unit			= 1;		  // 0:Kelvin 1:Jansky/pixel 2:SI 3:Lsun/pixel 4:tau
  img[0].filename		= ImageFileName;	// Output filename

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
  density[0] = 1.54e14 * exp(-z*z/(2*h*h)) * pow(r/AU, -1.0)
    * exp(-3*r/(140*AU));
  if (r < 3.5*AU) density[0] = 0;
}

/******************************************************************************/

void
temperature(double x, double y, double z, double *temperature){
  int i,x0=0;
  double r,v;
  /*
   * Calculate coordinate distance from origin
   */
  r=sqrt(x*x+y*y);

  
  v = 0;

  temperature[0]=temperature[1]=
    273 * pow(r/AU, -0.5) + v*v/284.6;
}

/******************************************************************************/

void
abundance(double x, double y, double z, double *abundance){
  /* 
   * Here we use a constant abundance. Could be a 
   * function of (x,y,z).
   */
  //  double aba[3] = {2.2e-10, 4.2e-15, 2.8e-12};

  abundance[0] = MolAbundance;
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

  double r;
  r=sqrt(x*x+y*y);
  
  double tmp;
  tmp =  273 * pow(r/AU, -0.5);
  double alpha = 1e-3;
  double thermal_velocity = sqrt(alpha) * sqrt(4127 * tmp);

  /*
  double tmp[1];
  temperature(x,y,z,tmp);
  double thermal_velocity = 0 ; //sqrt(284.6 * tmp[0]);
    */

  if(LightningInnerRadius*AU <= r && r <= LightningOuterRadius*AU )
    *doppler = sqrt(pow(LightningVelocity,2.0) + pow(thermal_velocity,2.0));
  else
    *doppler = thermal_velocity;

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

  // random velocities
  double rvx = 0,rvy = 0, rvz=0;
  if(LightningInnerRadius*AU <= r && r <= LightningOuterRadius*AU ){
    //rvz = 200;
  }

  /*
   * Vector transformation back into Cartesian basis
   */

  vel[0]=-sin(phi)*v + rvx;
  vel[1]= cos(phi)*v + rvy;
  vel[2]=0 + rvz;
}

/******************************************************************************/


