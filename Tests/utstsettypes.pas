unit utstsettypes;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

type

  Junk = Set of Char;

  Days = (Mon, Tue, Wed, Thu, Fri, Sat, Sun);
  WorkDays = Set of days;

implementation

end.

