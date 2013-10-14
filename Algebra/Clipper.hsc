{-# LANGUAGE CPP, ForeignFunctionInterface, EmptyDataDecls #-}
module Algebra.Clipper
(
 ClipType,ctIntersection,ctUnion,ctDifference,ctXor
,PolyType,ptSubject,ptClip
,PolyFillType,pftEvenOdd,pftNonZero,pftPositive,pftNegative
,IntPoint(..)
,Polygon(..), getPoints
,Polygons(..), getPolys
,execute
,intersection
,union
,difference
,Algebra.Clipper.xor
,area
,simplify
,Orientation(..),orientation
) where

import Foreign
import Foreign.C.Types
import Data.Int(Int64)
import Data.Word(Word64)
import Data.Monoid
import Control.Applicative((<$>), (<*>))

#include <clipper_c_wrapper.hpp>

-- enum ClipType { ctIntersection, ctUnion, ctDifference, ctXor };
newtype ClipType = ClipType Int
#enum ClipType, ClipType, \
        ctIntersection = ClipperLib::ctIntersection, \
        ctUnion = ClipperLib::ctUnion, \
        ctDifference = ClipperLib::ctDifference, \
        ctXor = ClipperLib::ctXor

instance Eq ClipType where
  (ClipType ct1) == (ClipType ct2) = ct1 == ct2

instance Show ClipType where
  show (ClipType ct) = "ClipType " ++ (show ct)

-- enum PolyType { ptSubject, ptClip };
newtype PolyType = PolyType Int
#enum PolyType, PolyType, \
        ptSubject = ClipperLib::ptSubject, \
        ptClip = ClipperLib::ptClip

-- enum PolyFillType { pftEvenOdd, pftNonZero, pftPositive, pftNegative };
newtype PolyFillType = PolyFillType Int
#enum PolyFillType, PolyFillType, \
        pftEvenOdd = ClipperLib::pftEvenOdd, \
        pftNonZero = ClipperLib::pftNonZero, \
        pftPositive = ClipperLib::pftPositive, \
        pftNegative = ClipperLib::pftNegative

--enum JoinType { jtSquare, jtRound, jtMiter };
newtype JoinType = JoinType Int
#enum JoinType, JoinType, \
        jtSquare = ClipperLib::jtSquare, \
        jtRound = ClipperLib::jtRound, \
        jtMiter = ClipperLib::jtMiter

--enum EndType { etClosed, etButt, etSquare, etRound};
newtype EndType = EndType Int
#enum EndType, EndType, \
        etClosed = ClipperLib::etClosed, \
        etButt = ClipperLib::etButt, \
        etSquare = ClipperLib::etSquare, \
        etRound = ClipperLib::etRound

-- struct IntPoint {
--   long64 X;
--   long64 Y;
--   IntPoint(long64 x = 0, long64 y = 0): X(x), Y(y) {};
-- };

data IntPoint = IntPoint
    {
      pointX :: #{type ClipperLib::long64}
    , pointY :: #{type ClipperLib::long64}
    } deriving Show

-- typedef std::vector< IntPoint > Polygon;
-- typedef std::vector< Polygon > Polygons;

newtype Polygon = Polygon [IntPoint] deriving Show
type PolygonPtr = Ptr Polygon

getPoints (Polygon ps) = ps

instance Monoid Polygon where
    mempty = Polygon mempty
    mappend (Polygon x) (Polygon y) = Polygon (x `mappend` y)

instance Storable Polygon where
    sizeOf _ = #{size ClipperLib::Polygon}
    alignment _ = alignment (undefined :: CInt)
    peek ptr = do
         numPts <- fromIntegral <$> polygonSize ptr
         Polygon <$> mapM (polygonGetPoint ptr) [0..(numPts-1)]
    poke ptr (Polygon ps) = do
         polygonClear ptr
         mapM_ addPt ps
        where addPt (IntPoint x y) = polygonAddPoint ptr x y

newtype Polygons = Polygons [Polygon] deriving Show
type PolygonsPtr = Ptr Polygons

getPolys (Polygons ps) = ps

size (Polygon ps) = fromIntegral $ length ps
sizes (Polygons ps) = fromIntegral $ length ps

instance Monoid Polygons where
    mempty = Polygons mempty
    mappend (Polygons x) (Polygons y) = Polygons (x `mappend` y)

instance Storable Polygons where
    sizeOf _ = #{size ClipperLib::Polygons}
    alignment _ = alignment (undefined :: CInt)
    peek ptr = do
         numPolys <- fromIntegral <$> polygonsSize ptr
         polyPtrs <- mapM (polygonsGetPoly ptr) [0..(numPolys-1)]
         Polygons <$> mapM peek polyPtrs
    poke ptr (Polygons ps) = do
         polygonsClear ptr
         mapM_ addPoly ps
        where addPoly poly = polygonNew (size poly) >>=
                             newForeignPtr polygonFree >>=
                             flip withForeignPtr (setPoly poly)
              setPoly poly pptr = poke pptr poly >> polygonsAddPoly ptr pptr

-- struct ExPolygon {
--   Polygon  outer;
--   Polygons holes;
-- };
-- typedef std::vector< ExPolygon > ExPolygons;

data ExPolygon
type ExPolygonPtr = Ptr ExPolygon

data ExPolygons
type ExPolygonsPtr = Ptr ExPolygons

data Clipper
type ClipperPtr = Ptr Clipper

-- extern "C" {
-- typedef void * polygon;
-- typedef void * polygons;

polygonGetPoint :: PolygonPtr -> Int -> IO IntPoint
polygonGetPoint ptr i = IntPoint <$> polygonGetPointX ptr i' <*> polygonGetPointY ptr i'
    where i' = fromIntegral i

area :: Polygon -> IO Double
area poly = do
  fptr <- polygonNew (size poly) >>=
          newForeignPtr polygonFree
  withForeignPtr fptr (flip poke poly)
  withForeignPtr fptr (flip polygonArea_ 0)

-- C++ version is True/False
data Orientation = Outer | Hole deriving (Eq,Show)

orientation:: Polygon -> IO Orientation
orientation p = do
  a <- area p
  case (a>0) of
    True -> return Outer
    False -> return Hole

execute :: ClipType -> Polygons -> Polygons -> IO Polygons
execute cType sPolys cPolys = clipperNew >>=
                              newForeignPtr clipperFree >>=
                              flip withForeignPtr exec_
    where exec_ cPtr = do
            spPtr <- polygonsNew (sizes sPolys) >>= newForeignPtr polygonsFree
            withForeignPtr spPtr (\subptr -> poke subptr sPolys >>
                                             clipperAddPolygons cPtr subptr ptSubject)
            cpPtr <- polygonsNew (sizes cPolys) >>= newForeignPtr polygonsFree
            withForeignPtr cpPtr (\clpptr -> poke clpptr cPolys >>
                                             clipperAddPolygons cPtr clpptr ptClip)
            rPtr <- polygonsNew 0 >>= newForeignPtr polygonsFree
            withForeignPtr rPtr (\resPtr -> clipperExecutePolys cPtr cType resPtr)
            withForeignPtr rPtr peek

intersection = execute ctIntersection
union = execute ctUnion
difference = execute ctDifference
xor = execute ctXor


simplify :: PolyFillType -> Polygons -> IO Polygons
simplify pFt ps = do
    withInOutPolys ps $ \inptr outptr ->
        polygonsSimplifyPolygons inptr outptr pFt


offset :: Bool -- | autoFix
       -> Double -- | limit
       -> JoinType
       -> Double -- | Delta
       -> Polygons
       -> IO Polygons
offset af lim jt da ps = do
    withInOutPolys ps $ \inptr outptr ->
      polygonsOffsetPolygons inptr outptr da jt lim af


withInOutPolys:: Polygons -- | input polys
              -> ( PolygonsPtr -- | input polys ptr
                   -> PolygonsPtr -- | output polys ptr
                   -> IO () )  -- | populate output polys
              -> IO Polygons
withInOutPolys ps runForeignFun = do
    psPtr <- polygonsNew (sizes ps) >>= newForeignPtr polygonsFree
    rPtr <- polygonsNew 0 >>= newForeignPtr polygonsFree

    withForeignPtr psPtr (\inptr ->
      withForeignPtr rPtr (\outptr-> do
        poke inptr ps
        runForeignFun inptr outptr ))

    withForeignPtr rPtr peek


--   long64 polygon_getPointX(polygon poly, int i);
foreign import ccall "clipper_c_wrapper.hpp polygon_getPointX"
        polygonGetPointX :: PolygonPtr -> #{type int} -> IO #{type ClipperLib::long64}

--   long64 polygon_getPointY(polygon poly, int i);
foreign import ccall "clipper_c_wrapper.hpp polygon_getPointY"
        polygonGetPointY :: PolygonPtr -> #{type int} -> IO #{type ClipperLib::long64}

--   polygon polygon_new(int numPoints);
foreign import ccall "clipper_c_wrapper.hpp polygon_new"
        polygonNew :: #{type int} -> IO PolygonPtr

--   void polygon_clear(polygon poly);
foreign import ccall "clipper_c_wrapper.hpp polygon_clear"
        polygonClear :: PolygonPtr -> IO ()

--   void polygon_size(polygon poly);
foreign import ccall "clipper_c_wrapper.hpp polygon_size"
        polygonSize :: PolygonPtr -> IO CInt

--   void polygon_addPoint(polygon poly, long64 x, long64 y)
foreign import ccall "clipper_c_wrapper.hpp polygon_addPoint"
        polygonAddPoint :: PolygonPtr -> #{type ClipperLib::long64} -> #{type ClipperLib::long64} -> IO ()

--   void polygon_free(polygon poly);
foreign import ccall "clipper_c_wrapper.hpp &polygon_free"
        polygonFree :: FunPtr (PolygonPtr -> IO ())

--   double polygon_getArea(polygon poly, int useFullInt64Range)
foreign import ccall "clipper_c_wrapper.hpp polygon_getArea"
        polygonArea_ :: PolygonPtr -> #{type int} -> IO #{type double}

--   polygons polygons_new(int numPolys);
foreign import ccall "clipper_c_wrapper.hpp polygons_new"
        polygonsNew :: #{type int}  -> IO PolygonsPtr

--   void polygons_clear(polygons poly);
foreign import ccall "clipper_c_wrapper.hpp polygons_clear"
        polygonsClear :: PolygonsPtr -> IO ()

--   void polygons_size(polygons poly);
foreign import ccall "clipper_c_wrapper.hpp polygons_size"
        polygonsSize :: PolygonsPtr -> IO CInt

--   void polygons_addPoly(polygons polys, polygon poly);
foreign import ccall "clipper_c_wrapper.hpp polygons_addPoly"
        polygonsAddPoly :: PolygonsPtr -> PolygonPtr -> IO ()

--   polygon polygons_getPoly(polygons polys, int i);
foreign import ccall "clipper_c_wrapper.hpp polygons_getPoly"
        polygonsGetPoly :: PolygonsPtr -> CInt -> IO PolygonPtr

--  void polygons_SimplifyPolygons(polygons inPolys, polygons outPolys, PolyFillType fillType) {
foreign import ccall "clipper_c_wrapper.hpp polygons_SimplifyPolygons"
        polygonsSimplifyPolygons:: PolygonsPtr -> PolygonsPtr -> PolyFillType -> IO ()

--  void polygons_OffsetPolygons(polygons inPolys,
--                               polygons outPolys,
--                               double delta,
--                               JoinType jointype,
--                               double limit,
--                               bool autoFix)
foreign import ccall "clipper_c_wrapper.hpp polygons_OffsetPolygons"
        polygonsOffsetPolygons:: PolygonsPtr
                              -> PolygonsPtr
                              -> Double
                              -> JoinType
                              -> Double
                              -> Bool
                              -> IO ()

--   void polygons_free(polygons poly);
foreign import ccall "clipper_c_wrapper.hpp &polygons_free"
        polygonsFree :: FunPtr (PolygonsPtr -> IO ())

--   clipper clipper_new();
foreign import ccall "clipper_c_wrapper.hpp clipper_new"
        clipperNew :: IO ClipperPtr

--   void clipper_addPolygon(clipper c, polygon poly, PolyType ptype);
foreign import ccall "clipper_c_wrapper.hpp clipper_addPolygon"
        clipperAddPolygon :: ClipperPtr -> PolygonPtr -> PolyType -> IO ()

--   void clipper_addPolygons(clipper c, polygons poly, PolyType ptype);
foreign import ccall "clipper_c_wrapper.hpp clipper_addPolygons"
        clipperAddPolygons :: ClipperPtr -> PolygonsPtr -> PolyType -> IO ()

--   void clipper_executePoly(clipper c, ClipType ctype, polygons soln);
foreign import ccall "clipper_c_wrapper.hpp clipper_executePoly"
        clipperExecutePolys :: ClipperPtr -> ClipType -> PolygonsPtr -> IO ()

--   void clipper_free(clipper c);
foreign import ccall "clipper_c_wrapper.hpp &clipper_free"
        clipperFree :: FunPtr (ClipperPtr -> IO ())

-- }
