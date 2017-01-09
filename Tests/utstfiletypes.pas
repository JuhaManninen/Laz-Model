unit utstfiletypes;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

type

  FPoint = Record
    X,Y,Z : real;
    end;

  PointFile = File of FPoint;

implementation

end.

