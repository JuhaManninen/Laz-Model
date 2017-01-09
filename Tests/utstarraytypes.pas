unit utstArrayTypes;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

type

 TByteArray = Array of Byte;

 TA = Array of array of Integer;

 TIntegerArray = Array of Integer;
 TIntegerArrayArray = Array of TIntegerArray;

 RealArray = Array [1..100] of Real;

 APointsA = array[1..100] of Array[1..3] of Real;
 APointsB = array[1..100,1..3] of Real;


implementation

end.

