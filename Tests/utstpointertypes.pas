unit utstpointertypes;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

type

  Buffer = String[255];

  // Model as Reference Association to Classifier
  // Modeled with 0..1 BufPtr to 0..1 Buffer
  // Association line has dot notation on Buffer end.
  BufPtr = ^Buffer;

implementation

end.

