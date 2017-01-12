unit utstrecordtypes;

{
  UML 2.5 10.2.3.1 these should  be represented as structured datatypes
  with attributes. ? How to handle RPoint below.


}

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

type

 // Model as Aggregated Association Classifiers.
 // ?? could be done by just having more than one Association in datatype features ??
 // ?? TRecord datatype in Model ??
 // ?? «Record» keyword if viewed in diagram ??
   Point = Record
          X,Y,Z : Real;
          end;

// functionally identical, semantically identical, CodeStyle different
// Require Packed {Feature | Member of TRecord}
  {$PackRecords 1}
   Trec2A = Record
     A : Byte;
     B : Word;
   end;
  {$PackRecords default}

   Trec2B = Packed Record
     A : Byte;
     B : Word;
   end;


  // Requires Single Control Classifier (TDataType parent)
  // Control Classifier Instance[s] ->  Aggregated Association Classifier[s]
  // OR Control Classifier Instance[s] ->  TRecord
  // ?? Probably requires seperate Model Class ??
  //
  RPoint = Record
          Case Boolean of
          False : (X,Y,Z : Real);
          True : (R,theta,phi : Real);
          end;

  BetterRPoint = Record
          Case UsePolar : Boolean of
          False : (X,Y,Z : Real);
          True : (R,theta,phi : Real);
          end;



 // ???????  Control Classifier implied integer ???
  MyRec = Record
        X : Longint;
        Case byte of
          2 : (Y : Longint;
               case byte of
               3 : (Z : Longint);
               );
        end;



implementation

end.

