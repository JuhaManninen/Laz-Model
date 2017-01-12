unit utstfiletypes;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

type

  // See utstrecordtypes for handling of record
  FPoint = Record
    X,Y,Z : real;
    end;

  PointFile = File of FPoint;

implementation

end.

