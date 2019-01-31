#include <Rcpp.h>
using namespace Rcpp;

// pnpoly from: https://wrf.ecse.rpi.edu//Research/Short_Notes/pnpoly.html
/*
 * Copyright (c) 1970-2003, Wm. Randolph Franklin
 
 Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the "Software"), to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:

Redistributions of source code must retain the above copyright notice, this list of conditions and the following disclaimers.
Redistributions in binary form must reproduce the above copyright notice in the documentation and/or other materials provided with the distribution.
The name of W. Randolph Franklin may not be used to endorse or promote products derived from this Software without specific prior written permission. 

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE. 
 */


int pnpoly(int nvert, NumericVector vertx, NumericVector verty, float testx, float testy)
{
  int i, j, c = 0;
  for (i = 0, j = nvert-1; i < nvert; j = i++) {
    if ( ((verty[i]>testy) != (verty[j]>testy)) &&
	 (testx < (vertx[j]-vertx[i]) * (testy-verty[i]) / (verty[j]-verty[i]) + vertx[i]) )
       c = !c;
  }
  return c;
}


int rect_in_poly(float l, float r, float u, float d, NumericVector vertx, NumericVector verty) {
    int n = vertx.size();
    return (pnpoly(n, vertx, verty, l, u) &&
            pnpoly(n, vertx, verty, r, d) &&
            pnpoly(n, vertx, verty, l, d) &&
            pnpoly(n, vertx, verty, r, u) );
}

// [[Rcpp::export]]
float scale_rect_to_poly(float x, float y, float w, float h, NumericVector vertx, NumericVector verty) {
  float scale = 1.0;
  while (!rect_in_poly(x - (w * scale)/2,
      x + (w * scale)/2,
      y + (h * scale)/2,
      y - (h * scale)/2,
      vertx, verty)) { 
    scale *= 0.9;
    if (scale < 1e-20)
      return 0.0;
  }
  return scale;

}