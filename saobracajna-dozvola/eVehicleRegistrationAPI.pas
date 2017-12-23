////////////////////////////////////////////////////////////////////////////
//
//  DATUM NASTANKA:    11/11/2017
//
//  POSLEDNJA PROMENA: 23/12/2017
//
//  OPIS:              Deklaracije za rad sa Saobracajna API (Citac elektronske saobracajne dozvole)
//                     Za opis funkcija pogledati uputstvo u dokumetnu
//                     eVehicle Registration SDK Korisnicko Uputstvo.pdf
//
//                     (eVehicleRegistrationAPI.h ver 1.3.2.0; prevod Srdjan Obucina)
//
////////////////////////////////////////////////////////////////////////////
unit eVehicleRegistrationAPI;

interface

uses
  Windows, Classes;

const
  SD_REGISDATA_MAX_SIZE = 4096;
  SD_SIGNATURE_MAX_SIZE	= 1024;
  SD_AUTHORITY_MAX_SIZE	= 4096;

type
// ---------------------------------
//	Structure used to read a registration data file, its
//	signature and the certificate of the document signer
//	These three fields shall be used together to  verify
//	the signature and ensure that the card is not a fake
  groupSD_REGISTRATION_DATA = record
    registrationData: array[0..SD_REGISDATA_MAX_SIZE - 1] of AnsiChar;
    registrationDataSize: LongInt;
    signatureData: array[0..SD_SIGNATURE_MAX_SIZE - 1] of AnsiChar;
    signatureDataSize: LongInt;
    issuingAuthority: array[0..SD_AUTHORITY_MAX_SIZE - 1] of AnsiChar;
    issuingAuthoritySize: LongInt;
  end;

  SD_REGISTRATION_DATA = groupSD_REGISTRATION_DATA;
  PSD_REGISTRATION_DATA = ^groupSD_REGISTRATION_DATA;
  TSdRegistrationData = groupSD_REGISTRATION_DATA;
  PSdRegistrationData = ^TSdRegistrationData;

  TNormRegistrationData = record
    RegistrationData: WideString;
    SignatureData: WideString;
    IssuingAuthority: WideString;
  end;

// ---------------------------------
//	Structure used to read a document data
  groupSD_DOCUMENT_DATA = record
    stateIssuing: array[0..49] of AnsiChar;
    stateIssuingSize: LongInt;
    competentAuthority: array[0..49] of AnsiChar;
    competentAuthoritySize: LongInt;
    authorityIssuing: array[0..49] of AnsiChar;
    authorityIssuingSize: LongInt;
    unambiguousNumber: array[0..29] of AnsiChar;
    unambiguousNumberSize: LongInt;
    issuingDate: array[0..15] of AnsiChar;
    issuingDateSize: LongInt;
    expiryDate: array[0..15] of AnsiChar;
    expiryDateSize: LongInt;
    serialNumber: array[0..19] of AnsiChar;
    serialNumberSize: LongInt;
  end;

  SD_DOCUMENT_DATA = groupSD_DOCUMENT_DATA;
  PSD_DOCUMENT_DATA = ^groupSD_DOCUMENT_DATA;
  TSdDocumentData = groupSD_DOCUMENT_DATA;
  PSdDocumentData = ^TSdDocumentData;

  TNormDocumentData = record
    StateIssuing: WideString;
    CompetentAuthority: WideString;
    AuthorityIssuing: WideString;
    UnambiguousNumber: WideString;
    IssuingDate: WideString;
    ExpiryDate: WideString;
    SerialNumber: WideString;
  end;

// ---------------------------------
//	Structure used to read a vehicle data
  groupSD_VEHICLE_DATA = record
    dateOfFirstRegistration: array[0..15] of AnsiChar;
    dateOfFirstRegistrationSize: LongInt;
    yearOfProduction: array[0..4] of AnsiChar;
    yearOfProductionSize: LongInt;
    vehicleMake: array[0..99] of AnsiChar;
    vehicleMakeSize: LongInt;
    vehicleType: array[0..99] of AnsiChar;
    vehicleTypeSize: LongInt;
    commercialDescription: array[0..99] of AnsiChar;
    commercialDescriptionSize: LongInt;
    vehicleIDNumber: array[0..99] of AnsiChar;
    vehicleIDNumberSize: LongInt;
    registrationNumberOfVehicle: array[0..19] of AnsiChar;
    registrationNumberOfVehicleSize: LongInt;
    maximumNetPower: array[0..19] of AnsiChar;
    maximumNetPowerSize: LongInt;
    engineCapacity: array[0..19] of AnsiChar;
    engineCapacitySize: LongInt;
    typeOfFuel: array[0..99] of AnsiChar;
    typeOfFuelSize: LongInt;
    powerWeightRatio: array[0..19] of AnsiChar;
    powerWeightRatioSize: LongInt;
    vehicleMass: array[0..19] of AnsiChar;
    vehicleMassSize: LongInt;
    maximumPermissibleLadenMass: array[0..19] of AnsiChar;
    maximumPermissibleLadenMassSize: LongInt;
    typeApprovalNumber: array[0..49] of AnsiChar;
    typeApprovalNumberSize: LongInt;
    numberOfSeats: array[0..19] of AnsiChar;
    numberOfSeatsSize: LongInt;
    numberOfStandingPlaces: array[0..19] of AnsiChar;
    numberOfStandingPlacesSize: LongInt;
    engineIDNumber: array[0..99] of AnsiChar;
    engineIDNumberSize: LongInt;
    numberOfAxles: array[0..19] of AnsiChar;
    numberOfAxlesSize: LongInt;
    vehicleCategory: array[0..49] of AnsiChar;
    vehicleCategorySize: LongInt;
    colourOfVehicle: array[0..49] of AnsiChar;
    colourOfVehicleSize: LongInt;
    restrictionToChangeOwner: array[0..199] of AnsiChar;
    restrictionToChangeOwnerSize: LongInt;
    vehicleLoad: array[0..19] of AnsiChar;
    vehicleLoadSize: LongInt;
  end;

  SD_VEHICLE_DATA = groupSD_VEHICLE_DATA;
  PSD_VEHICLE_DATA = ^groupSD_VEHICLE_DATA;
  TSdVehicleData = groupSD_VEHICLE_DATA;
  PSdVehicleData = ^TSdVehicleData;

  TNormVehicleData = record
    DateOfFirstRegistration: WideString;
    YearOfProduction: WideString;
    VehicleMake: WideString;
    VehicleType: WideString;
    CommercialDescription: WideString;
    VehicleIDNumber: WideString;
    RegistrationNumberOfVehicle: WideString;
    MaximumNetPower: WideString;
    EngineCapacity: WideString;
    TypeOfFuel: WideString;
    PowerWeightRatio: WideString;
    VehicleMass: WideString;
    MaximumPermissibleLadenMass: WideString;
    TypeApprovalNumber: WideString;
    NumberOfSeats: WideString;
    NumberOfStandingPlaces: WideString;
    EngineIDNumber: WideString;
    NumberOfAxles: WideString;
    VehicleCategory: WideString;
    ColourOfVehicle: WideString;
    RestrictionToChangeOwner: WideString;
    VehicleLoad: WideString;
  end;

// ---------------------------------
//	Structure used to read a personal data

  groupSD_PERSONAL_DATA = record
    ownersPersonalNo: array[0..19] of AnsiChar;
    ownersPersonalNoSize: LongInt;
    ownersSurnameOrBusinessName: array[0..99] of AnsiChar;
    ownersSurnameOrBusinessNameSize: LongInt;
    ownerName: array[0..99] of AnsiChar;
    ownerNameSize: LongInt;
    ownerAddress: array[0..199] of AnsiChar;
    ownerAddressSize: LongInt;
    usersPersonalNo: array[0..19] of AnsiChar;
    usersPersonalNoSize: LongInt;
    usersSurnameOrBusinessName: array[0..99] of AnsiChar;
    usersSurnameOrBusinessNameSize: LongInt;
    usersName: array[0..99] of AnsiChar;
    usersNameSize: LongInt;
    usersAddress: array[0..199] of AnsiChar;
    usersAddressSize: LongInt;
  end;

  SD_PERSONAL_DATA = groupSD_PERSONAL_DATA;
  PSD_PERSONAL_DATA = ^groupSD_PERSONAL_DATA;
  TSdPersonalData = groupSD_PERSONAL_DATA;
  PSdPersonalData = ^TSdPersonalData;

  TNormPersonalData = record
    OwnersPersonalNo: WideString;
    OwnersSurnameOrBusinessName: WideString;
    OwnerName: WideString;
    OwnerAddress: WideString;
    UsersPersonalNo: WideString;
    UsersSurnameOrBusinessName: WideString;
    UsersName: WideString;
    UsersAddress: WideString;
  end;

var

// ---------------------------------
//	Initialization, finalization of library

  sdStartup: function(apiVersion: Integer): LongInt; cdecl;
  sdCleanup: function(): LongInt; cdecl;

// ---------------------------------
//	enumeration, selection of card readers

  GetReaderName: function(index: LongInt; readerName: PChar; nameSize: PLongInt): LongInt; cdecl;
  SelectReader: function(readerName: PChar): LongInt; cdecl;

// ---------------------------------
//	new card process request - shall be called prior to read a new card

  sdProcessNewCard: function(): LongInt; cdecl;

// ---------------------------------
//	Read data file, signature & cert for the file with given index
//	(indexes are in 1 up to 3, the 4th file is not signed)

  sdReadRegistration: function(pData: PSdRegistrationData; index: LongInt): LongInt; cdecl;

// ---------------------------------
//	Read Document, Vehicle and Personal Data

  sdReadDocumentData: function(pData: PSdDocumentData): LongInt; cdecl;
  sdReadVehicleData: function(pData: PSdVehicleData): LongInt; cdecl;
  sdReadPersonalData: function(pData: PSdPersonalData): LongInt; cdecl;

// ---------------------------------

  procedure LoadDLL(APathToDll: String = '');
  procedure UnloadDLL();

  procedure SdToNormRegistrationData(ASdData: TSdRegistrationData; var ANormData: TNormRegistrationData);
  procedure SdToNormDocumentData(ASdData: TSdDocumentData; var ANormData: TNormDocumentData);
  procedure SdToNormVehicleData(ASdData: TSdVehicleData; var ANormData: TNormVehicleData);
  procedure SdToNormPersonalData(ASdData: TSdPersonalData; var ANormData: TNormPersonalData);

  function NormReadRegistrationData(var AData: TNormRegistrationData): LongInt;
  function NormReadDocumentData(var AData: TNormDocumentData): LongInt;
  function NormReadVehicleData(var AData: TNormVehicleData): LongInt;
  function NormReadPersonalData(var AData: TNormPersonalData): LongInt;

implementation

uses
  SysUtils;

var
  DLLHandle: HMODULE;

procedure LoadDLL(APathToDll: String);
begin
  APathToDll := APathToDll + 'eVehicleRegistrationAPI.dll';
  DLLHandle := LoadLibrary(PWideChar(APathToDll));

  if (DLLHandle <> 0) then
  begin
    @sdStartup := GetProcAddress(DLLHandle, 'sdStartup');
    @sdCleanup := GetProcAddress(DLLHandle, 'sdCleanup');
    @GetReaderName := GetProcAddress(DLLHandle, 'GetReaderName');
    @SelectReader := GetProcAddress(DLLHandle, 'SelectReader');
    @sdProcessNewCard := GetProcAddress(DLLHandle, 'sdProcessNewCard');
    @sdReadRegistration := GetProcAddress(DLLHandle, 'sdReadRegistration');
    @sdReadDocumentData := GetProcAddress(DLLHandle, 'sdReadDocumentData');
    @sdReadVehicleData := GetProcAddress(DLLHandle, 'sdReadVehicleData');
    @sdReadPersonalData := GetProcAddress(DLLHandle, 'sdReadPersonalData');
  end;
end;

procedure UnloadDLL();
begin
  if (DLLHandle <> 0) then
  begin
    FreeLibrary(DLLHandle);
    DLLHandle := 0;
  end;
end;

procedure DataToHex(var ADest: WideString; const ASource: PAnsiChar; const ASize: Integer);
var
  i: Integer;
  Temp: AnsiString;
begin
  for i := 0 to ASize - 1 do
  begin
    Temp := Temp + IntToHex(Ord(ASource[i]), 2);
  end;
  Temp := UpperCase(Temp);
  //SetString(Temp, ASource, ASize);
  ADest := UTF8ToWideString(Temp);
end;

procedure NormGetData(var ADest: WideString; const ASource: PAnsiChar; const ASize: Integer);
var
  Temp: AnsiString;
begin
  // Saobracajna API vraca sve stringove kao AnsiString u UTF8 formatu, konvertujemo ga u WideString
  SetString(Temp, ASource, ASize);
  ADest := UTF8ToWideString(Temp);
end;

procedure SdToNormRegistrationData(ASdData: TSdRegistrationData; var ANormData: TNormRegistrationData);
begin
  DataToHex(ANormData.RegistrationData, ASdData.registrationData, ASdData.registrationDataSize);
  DataToHex(ANormData.SignatureData, ASdData.signatureData, ASdData.signatureDataSize);
  DataToHex(ANormData.IssuingAuthority, ASdData.issuingAuthority, ASdData.issuingAuthoritySize);
end;

procedure SdToNormDocumentData(ASdData: TSdDocumentData; var ANormData: TNormDocumentData);
begin
  NormGetData(ANormData.StateIssuing, ASdData.stateIssuing, ASdData.stateIssuingSize);
  NormGetData(ANormData.CompetentAuthority, ASdData.competentAuthority, ASdData.competentAuthoritySize);
  NormGetData(ANormData.AuthorityIssuing, ASdData.authorityIssuing, ASdData.authorityIssuingSize);
  NormGetData(ANormData.UnambiguousNumber, ASdData.unambiguousNumber, ASdData.unambiguousNumberSize);
  NormGetData(ANormData.IssuingDate, ASdData.issuingDate, ASdData.issuingDateSize);
  NormGetData(ANormData.ExpiryDate, ASdData.expiryDate, ASdData.expiryDateSize);
  NormGetData(ANormData.SerialNumber, ASdData.serialNumber, ASdData.serialNumberSize);
end;

procedure SdToNormVehicleData(ASdData: TSdVehicleData; var ANormData: TNormVehicleData);
begin
  NormGetData(ANormData.DateOfFirstRegistration, ASdData.dateOfFirstRegistration, ASdData.dateOfFirstRegistrationSize);
  NormGetData(ANormData.YearOfProduction, ASdData.yearOfProduction, ASdData.yearOfProductionSize);
  NormGetData(ANormData.VehicleMake, ASdData.vehicleMake, ASdData.vehicleMakeSize);
  NormGetData(ANormData.VehicleType, ASdData.vehicleType, ASdData.vehicleTypeSize);
  NormGetData(ANormData.CommercialDescription, ASdData.commercialDescription, ASdData.commercialDescriptionSize);
  NormGetData(ANormData.VehicleIDNumber, ASdData.vehicleIDNumber, ASdData.vehicleIDNumberSize);
  NormGetData(ANormData.RegistrationNumberOfVehicle, ASdData.registrationNumberOfVehicle, ASdData.registrationNumberOfVehicleSize);
  NormGetData(ANormData.MaximumNetPower, ASdData.maximumNetPower, ASdData.maximumNetPowerSize);
  NormGetData(ANormData.EngineCapacity, ASdData.engineCapacity, ASdData.engineCapacitySize);
  NormGetData(ANormData.TypeOfFuel, ASdData.typeOfFuel, ASdData.typeOfFuelSize);
  NormGetData(ANormData.PowerWeightRatio, ASdData.powerWeightRatio, ASdData.powerWeightRatioSize);
  NormGetData(ANormData.VehicleMass, ASdData.vehicleMass, ASdData.vehicleMassSize);
  NormGetData(ANormData.MaximumPermissibleLadenMass, ASdData.maximumPermissibleLadenMass, ASdData.maximumPermissibleLadenMassSize);
  NormGetData(ANormData.TypeApprovalNumber, ASdData.typeApprovalNumber, ASdData.typeApprovalNumberSize);
  NormGetData(ANormData.NumberOfSeats, ASdData.numberOfSeats, ASdData.numberOfSeatsSize);
  NormGetData(ANormData.NumberOfStandingPlaces, ASdData.numberOfStandingPlaces, ASdData.numberOfStandingPlacesSize);
  NormGetData(ANormData.EngineIDNumber, ASdData.engineIDNumber, ASdData.engineIDNumberSize);
  NormGetData(ANormData.NumberOfAxles, ASdData.numberOfAxles, ASdData.numberOfAxlesSize);
  NormGetData(ANormData.VehicleCategory, ASdData.vehicleCategory, ASdData.vehicleCategorySize);
  NormGetData(ANormData.ColourOfVehicle, ASdData.colourOfVehicle, ASdData.colourOfVehicleSize);
  NormGetData(ANormData.RestrictionToChangeOwner, ASdData.restrictionToChangeOwner, ASdData.restrictionToChangeOwnerSize);
  NormGetData(ANormData.VehicleLoad, ASdData.vehicleLoad, ASdData.vehicleLoadSize);
end;

procedure SdToNormPersonalData(ASdData: TSdPersonalData; var ANormData: TNormPersonalData);
begin
  NormGetData(ANormData.OwnersPersonalNo, ASdData.ownersPersonalNo, ASdData.ownersPersonalNoSize);
  NormGetData(ANormData.OwnersSurnameOrBusinessName, ASdData.ownersSurnameOrBusinessName, ASdData.ownersSurnameOrBusinessNameSize);
  NormGetData(ANormData.OwnerName, ASdData.ownerName, ASdData.ownerNameSize);
  NormGetData(ANormData.OwnerAddress, ASdData.ownerAddress, ASdData.ownerAddressSize);
  NormGetData(ANormData.UsersPersonalNo, ASdData.usersPersonalNo, ASdData.usersPersonalNoSize);
  NormGetData(ANormData.UsersSurnameOrBusinessName, ASdData.usersSurnameOrBusinessName, ASdData.usersSurnameOrBusinessNameSize);
  NormGetData(ANormData.UsersName, ASdData.usersName, ASdData.usersNameSize);
  NormGetData(ANormData.UsersAddress, ASdData.usersAddress, ASdData.usersAddressSize);
end;

function NormReadRegistrationData(var AData: TNormRegistrationData): LongInt;
var
  SdData: TSdRegistrationData;
begin
  ZeroMemory(@SdData, SizeOf(TSdRegistrationData));
  Result := sdReadRegistration(@SdData, 1);
  SdToNormRegistrationData(SdData, AData);
end;

function NormReadDocumentData(var AData: TNormDocumentData): LongInt;
var
  SdData: TSdDocumentData;
begin
  ZeroMemory(@SdData, SizeOf(TSdDocumentData));
  Result := sdReadDocumentData(@SdData);
  SdToNormDocumentData(SdData, AData);
end;

function NormReadVehicleData(var AData: TNormVehicleData): LongInt;
var
  SdData: TSdVehicleData;
begin
  ZeroMemory(@SdData, SizeOf(TSdVehicleData));
  Result := sdReadVehicleData(@SdData);
  SdToNormVehicleData(SdData, AData);
end;

function NormReadPersonalData(var AData: TNormPersonalData): LongInt;
var
  SdData: TSdPersonalData;
begin
  ZeroMemory(@SdData, SizeOf(TSdPersonalData));
  Result := sdReadPersonalData(@SdData);
  SdToNormPersonalData(SdData, AData);
end;

end.
