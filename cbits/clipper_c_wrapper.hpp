//
// C wrappers around C++ clipper library.
// The whole thing is compiled with C++
// compiler but these parts follow C/FFI
// conventions
//

#ifndef clipper_c_wrapper_hpp
#define clipper_c_wrapper_hpp

#include <polyclipping/clipper.hpp> 

extern "C" {

  using namespace ClipperLib;

  typedef void * polygon;
  typedef void * polygons;
  typedef void * expolygon;
  typedef void * expolygons;
  typedef void * clipper;

  polygon polygon_new(int numPoints);
  void polygon_clear(polygon poly);
  int polygon_size(polygon poly);
  void polygon_addPoint(polygon poly, long64 x, long64 y);
  long64 polygon_getPointX(polygon poly, int i);
  long64 polygon_getPointY(polygon poly, int i);
  void polygon_free(polygon poly);

  int polygon_isClockwise(polygon poly, int useFullInt64Range);
  double polygon_getArea(polygon poly, int useFullInt64Range);

  polygons polygons_new(int numPolys);
  void polygons_clear(polygons poly);
  int polygons_size(polygons poly);
  polygon polygons_getPoly(polygons polys, int i);
  void polygons_addPoly(polygons polys, polygon poly);
	void polygons_SimplifyPolygons(polygons inPolys, polygons outPolys, PolyFillType fillType);

  //void OffsetPolygons(const Polygons &in_polys, Polygons &out_polys,
  //                    double delta, JoinType jointype = jtSquare,
  //                    double limit = 0, bool autoFix = true);
  void polygons_OffsetPolygons(polygons inPolys,
                               polygons outPolys,
                               double delta,
                               JoinType jointype,
                               double limit,
                               bool autoFix);


  void polygons_free(polygons poly);


  clipper clipper_new();
  void clipper_addPolygon(clipper c, polygon poly, PolyType ptype);
  void clipper_addPolygons(clipper c, polygons poly, PolyType ptype);
  void clipper_executePoly(clipper c, ClipType ctype, polygons soln);
  void clipper_free(clipper c);
}

#endif //clipper_c_wrapper_hpp
