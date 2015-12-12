#include "R.h"
// several routines expect integer values for x/y!
// but we use "double" because we need higher than 2^31 for high precision maps
void mapdups(double*x,double*y,int*len,int*result);
void mapclean(double*x,double*y,int*len,double*nx,double*ny,int*nlen);
void mapvalence_seg(double*x,double*y,int*len,int*valence);
void mapmerge_seg(double*x, double*y, int *xlen, int*valence, int* linebuf,
              int*gon, int*gonlen, int*ngon,
              double *x_out, double *y_out, int *xlen_out,
              int* gon_out, int* gonlen_out);
void mapsizes(int* result);

// for writing maps in binary format, we have to be sure of types used by the C compiler
void mapsizes(int* result){
  result[0] = sizeof(char);
  result[1] = sizeof(short);
  result[2] = sizeof(int);
  result[3] = sizeof(float);
}


// find duplicate segments (=lines of length 2)
void mapdups(double*x,double*y,int*len,int*result){
  int i,j;
  double x1,x2,y1,y2;
  double xp1,xp2,yp1,yp2;

  for(i=0;i< *len;i++) result[i]=0;
  for(i=0;i< *len; i++) {
    if (result[i]) continue;
    x1=x[3*i];
    y1=y[3*i];
    x2=x[3*i+1];
    y2=y[3*i+1];
    for(j=i+1;j< *len; j++) {
      if (result[j]) continue;
      xp1=x[3*j];
      xp2=x[3*j+1];
      yp1=y[3*j];
      yp2=y[3*j+1];
      if (x1==xp1 && x2==xp2 && y1==yp1 && y2==yp2) result[j] = i+1; // R is 1-based, C 0-based
      else if (x1==xp2 && x2==xp1 && y1==yp2 && y2==yp1) result[j] = -i-1;
    }
  }
}


void mapclean(double*x,double*y,int*len,double*x_out,double*y_out,int*len_out){
  int i,j;
/* BUGfix: mini-islands that are 1 point repeated twice become corrupted! */ 
/* if there is only one point in the polygon, you want it repeated! */
  x_out[0]=x[0];
  y_out[0]=y[0];
  j=1;
  for (i=1 ; i<*len ; i++){
    if( ISNA(x[i]) || ISNA(x_out[j-1]) ||
        (j>1 && i< *len-1 && ISNA(x_out[j-2]) && ISNA(x[i+1])) ||
        x[i]!=x_out[j-1] || y[i]!=y_out[j-1]){
       x_out[j]=x[i];
       y_out[j]=y[i];
       j++;
    }
  }
  *len_out=j;
}

void mapvalence(double*x,double*y,int*len,int*valence){
  int i,j,val;

  for (i=0 ; i< *len ; i++) valence[i]=0; 
  for (i=0 ; i< *len ; i++){
    if (ISNA(x[i])) continue;
// If this point was equal to an earlier one, we already know the result
    if (valence[i]) valence[i]=valence[valence[i]-1];
    else {
      val = 1;
      for (j=i+1; j<*len;j++) {
        if( ISNA(x[j]) || (x[i]-x[j]) || (y[i]-y[j]) ) continue;
        val++;
        valence[j]=i+1; // leave a sign that this point is already known
                        // but it must be >0, so add 1
      }
      valence[i] = val;
    }
  }
}

// only 'positive' line numbers may be merged: the negative ones have already been done
// except if there are polygons with different winding (lakes? City states? -> NO ,that should still be consistent)
// output is a list of length nline, giving for every segment the line number
// it should belong to.
// BUG: there is no guarantee that the first gon starts with segment 1 (after rotation)
// so don't copy x,y in-place
void mapmerge_seg(double*x, double*y, int *xlen, int*valence, int* linebuf,
              int*gon, int*gonlen, int*ngon,
              double *x_out, double *y_out, int *xlen_out,
              int* gon_out, int* gonlen_out){
  int i,j,gg;
  int lnum, lline, llen, glen, segnum;

  lnum=1;//lline=current line, lnum=max number of lines already assigned
  (*xlen_out)=0;
  gg=0;
  for (i=0; i<*ngon; i++) {
    glen=0;
    lline=0;
    for (j=0; j<gonlen[i] ; j++) {
      segnum = gon[gg++]; 
      if (segnum > 0) {
// linebuf remembers to which line a segment has been assigned
        if (lnum != lline) {
          lline = lnum; //new 'current' line, start it with "NA"
          *(gon_out++) = lline;
          glen++;
          llen=0;
          if (lline > 1) { // pre-fill x_out with NA => {x_out++;y_out++;}
            *(x_out++) = NA_REAL;
            *(y_out++) = NA_REAL;
            (*xlen_out)++;
          }
        }
        linebuf[segnum] = lline;
        *(x_out++) = *(x + 3*(segnum-1));
        *(y_out++) = *(y + 3*(segnum-1));
        llen++;
        if(valence[3*segnum-2] > 2 || j==gonlen[i]-1)  { // this segment finishes in a vertex OR the polygon is finished
          *(x_out++) = *(x + 3*(segnum-1)+1);
          *(y_out++) = *(y + 3*(segnum-1)+1);
          llen++;
          lnum++;
          (*xlen_out) += llen;
//          Rprintf("Finished line %i: llen=%i, xlen_out=%i\n",lnum,llen,*xlen_out);
        }
      }
      else {
// the segment is already known
// now see if it is in the 'current' line
// If it's a new one, do some work, otherwise: just move on...
        if (linebuf[-segnum] != -lline) {
          lline = -linebuf[-segnum];
          *(gon_out++) = lline;
          glen++;
        }
      }
    }
    gonlen_out[i] = glen;
  }
}



