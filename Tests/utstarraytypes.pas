unit utstArrayTypes;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

type

 // Model as Association with Single TByteArray to unbounded Byte.
 // Requires Classifer -> Association -> AssociationEnd.
 // Association is a Feature on TByteArray not navigable from Byte end of
 // association.
 TByteArray = Array of Byte;

 // Same as above
 TIntegerArray = Array of Integer;

 // functionally identical, semantically different
 TA = Array of array of Integer;
 TIntegerArrayArray = Array of TIntegerArray;

 RealArray = Array [1..100] of Real;

 // functionally identical, semantically identical, CodeStyle different
 APointsA = array[1..100] of Array[1..3] of Real;
 APointsB = array[1..100,1..3] of Real;

 // Bound array with implied 0 min.
 Buffer = String[255];
 // functionally identical, semantically identical, CodeStyle different to above
 Buffer1 = Array[255] of string;
 // functionally identical, semantically identical, CodeStyle different to above
 Buffer2 = Array[0..255] of string;

implementation

end.

