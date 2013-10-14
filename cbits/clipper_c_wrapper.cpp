//
// C wrappers around C++ clipper library.
// The whole thing is compiled with C++
// compiler but these parts follow C/FFI
// conventions
//

#include "clipper_c_wrapper.hpp"
//#include <cmath>
//#include <vector>
//#include <algorithm>
//#include <stdexcept>
//#include <cstring>
//#include <cstdlib>
//#include <stdio.h>

extern "C" {

  using namespace ClipperLib;

  //****************** POLYGON
  polygon polygon_new(int numPoints)
  {
    // Polygon * p;
    // if(numPoints > 0)
    //   p = new Polygon(numPoints);
    // else
    //   p = new Polygon();
    // printf("NEW Poly:0x%X\n", p);
    // return p;
    if(numPoints > 0)
      return new Polygon(numPoints);
    else
      return new Polygon();
  }

  void polygon_clear(polygon poly)
  {
    // printf("CLEAR Poly:0x%X\n", poly);
    ((Polygon *) poly)->clear();
  }

  int polygon_size(polygon poly)
  {
    return ((Polygon *) poly)->size();
  }

  void polygon_addPoint(polygon poly, long64 x, long64 y)
  {
    // printf("ADD Point Poly:0x%X, (%lld, %lld)\n", poly, x, y);
    IntPoint pt(x, y);
    ((Polygon *)poly)->push_back(pt);
  }

  long64 polygon_getPointX(polygon poly, int i)
  {
    return ((Polygon *)poly)->at(i).X;
  }

  long64 polygon_getPointY(polygon poly, int i)
  {
    return ((Polygon *)poly)->at(i).Y;
  }

  void polygon_free(polygon poly)
  {
    // printf("FREE Poly:0x%X\n", poly);
    delete (Polygon *) poly;
  }


  double polygon_getArea(polygon poly, int useFullInt64Range)
  {
    return Area(*(Polygon *) poly);
  } 

  //****************** POLYGONS
  polygons polygons_new(int numPolys)
  {
    // Polygons * ps;
    // if(numPolys > 0)
    //   ps = new Polygons(numPolys);
    // else
    //   ps = new Polygons();
    // printf("NEW Polygons:0x%X\n", ps);
    // return ps;
    if(numPolys > 0)
      return new Polygons(numPolys);
    else
      return new Polygons();
  }

  void polygons_clear(polygons poly)
  {
    // printf("CLEAR Polys: 0x%X\n", poly);
    ((Polygons *) poly)->clear();
  }

  int polygons_size(polygons poly)
  {
    return ((Polygons *) poly)->size();
  }

  polygon polygons_getPoly(polygons polys, int i)
  {
    // Polygon * p = &(((Polygons *)polys)->at(i));
    // printf("GETPoly Polys: 0x%X, Poly:0x%X(%d)\n", polys, p, p->size());
    // return p;
    return &(((Polygons *)polys)->at(i));
  }

  void polygons_addPoly(polygons polys, polygon poly)
  {
    ((Polygons *)polys)->push_back(*((Polygon *)poly));
    // printf("ADDPoly Polys: 0x%X, poly:0x%X(%d), size:%d\n", polys, poly, ((Polygon *)poly)->size(), ((Polygons *)polys)->size());
  }

	void polygons_SimplifyPolygons(polygons inPolys,
                                 polygons outPolys,
                                 PolyFillType fillType)
  {
    //void SimplifyPolygons(const Polygons &in_polys, Polygons &out_polys, PolyFillType fillType = pftEvenOdd);\015
		SimplifyPolygons(*(Polygons*)inPolys, *(Polygons*)outPolys, fillType);
	}


  //void OffsetPolygons(const Polygons &in_polys, Polygons &out_polys,
  //                    double delta, JoinType jointype = jtSquare,
  //                    double limit = 0, bool autoFix = true);
  void polygons_OffsetPolygons(polygons inPolys,
                               polygons outPolys,
                               double delta,
                               JoinType jointype,
                               double limit,
                               bool autoFix)
  {
    OffsetPolygons(*(Polygons*)inPolys,
                   *(Polygons*)outPolys,
                   delta, jointype, limit, autoFix);
  }

  void polygons_free(polygons poly)
  {
    // printf("FREE Polys: 0x%X\n", poly);
    delete (Polygons *) poly;
  }


  //****************** EXPOLYGON
  // expolygon expolygon_new(int numPoints)
  // {
  //   if(numPoints > 0)
  //     return new ExPolygons(numPoints);
  //   else
  //     return new ExPolygons();
  // }

  // void expolygon_addPoint(expolygon poly, long64 x, long64 y)
  // {
  //   IntPoint pt(x, y);
  //   ((ExPolygon *)poly)->push_back(pt);
  // }

  // void expolygon_free(expolygon poly)
  // {
  //   delete (ExPolygon *) poly;
  // }

  //****************** CLIPPER
  clipper clipper_new()
  {
    // printf("NEW Clipper\n");
    return new Clipper();
  }

  void clipper_addPolygon(clipper c, polygon poly, PolyType ptype)
  {
    ((Clipper *) c)->AddPolygon(*((Polygon *) poly), ptype);
  }

  void clipper_addPolygons(clipper c, polygons poly, PolyType ptype)
  {
    // printf("AddPolygons CLipper:0x%X, Poly:0x%X, Size:%d\n", c, poly, ((Polygons *) poly)->size());
    ((Clipper *) c)->AddPolygons(*((Polygons *) poly), ptype);
  }

  void clipper_executePoly(clipper c, ClipType ctype, polygons soln)
  {
    // printf("Execute CLipper:0x%X, Soln:0x%X\n", c, soln);
    ((Clipper *) c)->Execute(ctype, *((Polygons *) soln));
  }

  // void clipper_executeExPoly(clipper c, ClipType ctype, polygons soln)
  // {
  //   ((Clipper *) c)->AddPolygons(*((ExPolygons *) poly));
  // }

  void clipper_free(clipper c)
  {
    // printf("FREE Clipper\n");
    delete ((Clipper *) c);
  }
}
