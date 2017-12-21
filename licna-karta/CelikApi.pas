////////////////////////////////////////////////////////////////////////////
//
//  DATUM NASTANKA:    03/10/2010
//
//  POSLEDNJA PROMENA: 17/10/2014
//
//  OPIS:              Deklaracije za rad sa Celik API (Citac elektronske licne karte)
//                     za opis funkcija pogledati zvanicno uputstvo dato u CelikApi.pdf
//                     (CelikApi.h ver1.0.2.1; prevod Igor Savkic)
//                     (CelikApi.h ver1.2.0.0; update mnenad & Igor Savkic)
//
////////////////////////////////////////////////////////////////////////////
unit CelikApi;

interface

uses
  Windows, Classes;

const
  // Size of all UTF-8 and binary fields in bytes
  EID_MAX_DocRegNo = 9;
  EID_MAX_DocumentType = 2;
  EID_MAX_IssuingDate = 10;
  EID_MAX_ExpiryDate = 10;
  EID_MAX_IssuingAuthority = 100;

  EID_MAX_PersonalNumber = 13;
  EID_MAX_Surname = 200;
  EID_MAX_GivenName = 200;
  EID_MAX_ParentGivenName = 200;
  EID_MAX_Sex = 2;
  EID_MAX_PlaceOfBirth = 200;
  EID_MAX_StateOfBirth = 200;
  EID_MAX_DateOfBirth = 10;
  EID_MAX_CommunityOfBirth = 200;

  EID_MAX_State = 100;
  EID_MAX_Community = 200;
  EID_MAX_Place = 200;
  EID_MAX_Street = 200;
  EID_MAX_HouseNumber = 20;
  EID_MAX_HouseLetter = 8;
  EID_MAX_Entrance = 10;
  EID_MAX_Floor = 6;
  EID_MAX_ApartmentNumber = 12;
  EID_MAX_AddressDate = 10;
  EID_MAX_AddressLabel = 60;

  EID_MAX_Portrait = 7700;
  EID_MAX_Certificate = 2048;

  // Option identifiers, used in function EidSetOption
  EID_O_KEEP_CARD_CLOSED = 1;

  // Certificate types, used in function EidReadCertificate
  EID_Cert_MoiIntermediateCA = 1;
  EID_Cert_User1  = 2;
  EID_Cert_User2  = 3;

  // Block types, used in function EidVerifySignature
  EID_SIG_CARD  = 1;
  EID_SIG_FIXED = 2;
  EID_SIG_VARIABLE  = 3;
  EID_SIG_PORTRAIT  = 4;

  // For new card version EidVerifySignature function will return EID_E_UNABLE_TO_EXECUTE for
  // parameter EID_SIG_PORTRAIT. Portrait is in new cards part of EID_SIG_FIXED. To determine
  // the card version use second parameter of function EidBeginRead

  // Function return values
  EID_OK = 0;
  EID_E_GENERAL_ERROR = -1;
  EID_E_INVALID_PARAMETER = -2;
  EID_E_VERSION_NOT_SUPPORTED = -3;
  EID_E_NOT_INITIALIZED = -4;
  EID_E_UNABLE_TO_EXECUTE = -5;
  EID_E_READER_ERROR = -6;
  EID_E_CARD_MISSING = -7;
  EID_E_CARD_UNKNOWN = -8;
  EID_E_CARD_MISMATCH = -9;
  EID_E_UNABLE_TO_OPEN_SESSION = -10;
  EID_E_DATA_MISSING = -11;
  EID_E_CARD_SECFORMAT_CHECK_ERROR = -12;
  EID_E_SECFORMAT_CHECK_CERT_ERROR = -13;
  EID_E_INVALID_PASSWORD = -14;
  EID_E_PIN_BLOCKED = -15;

type
  // NOTE: char arrays DO NOT have zero (null) char at the end
  tagEID_DOCUMENT_DATA = record
    docRegNo: array[0..EID_MAX_DocRegNo - 1] of AnsiChar;
    docRegNoSize: Integer;
	  documentType:  array[0..EID_MAX_DocumentType - 1] of AnsiChar;
	  documentTypeSize: integer;
    issuingDate: array[0..EID_MAX_IssuingDate - 1] of AnsiChar;
    issuingDateSize: Integer;
    expiryDate: array[0..EID_MAX_ExpiryDate - 1] of AnsiChar;
    expiryDateSize: Integer;
    issuingAuthority: array[0..EID_MAX_IssuingAuthority - 1] of AnsiChar;
    issuingAuthoritySize: Integer;
  end;

  EID_DOCUMENT_DATA  = tagEID_DOCUMENT_DATA;
  PEID_DOCUMENT_DATA = ^tagEID_DOCUMENT_DATA;
  TEidDocumentData = tagEID_DOCUMENT_DATA;
  PEidDocumentData = ^TEidDocumentData;

  TNormDocumentData = record
    DocRegNo: WideString;
    DocumentType: WideString;
    IssuingDate: WideString;
    ExpiryDate: WideString;
    IssuingAuthority: WideString;
  end;

  tagEID_FIXED_PERSONAL_DATA = record
    personalNumber: array[0..EID_MAX_PersonalNumber - 1] of AnsiChar;
    personalNumberSize: Integer;
    surname: array[0..EID_MAX_Surname - 1] of AnsiChar;
    surnameSize: Integer;
    givenName: array[0..EID_MAX_GivenName - 1] of AnsiChar;
    givenNameSize: Integer;
    parentGivenName: array[0..EID_MAX_ParentGivenName - 1] of AnsiChar;
    parentGivenNameSize: Integer;
    sex: array[0..EID_MAX_Sex - 1] of AnsiChar;
    sexSize: Integer;
    placeOfBirth: array[0..EID_MAX_PlaceOfBirth - 1] of AnsiChar;
    placeOfBirthSize: Integer;
    stateOfBirth: array[0..EID_MAX_StateOfBirth - 1] of AnsiChar;
    stateOfBirthSize: Integer;
    dateOfBirth: array[0..EID_MAX_DateOfBirth - 1] of AnsiChar;
    dateOfBirthSize: Integer;
    communityOfBirth: array[0..EID_MAX_CommunityOfBirth - 1] of AnsiChar;
    communityOfBirthSize: Integer;
  end;

  EID_FIXED_PERSONAL_DATA  = tagEID_FIXED_PERSONAL_DATA;
  PEID_FIXED_PERSONAL_DATA = ^tagEID_FIXED_PERSONAL_DATA;
  TEidFixedPersonalData = tagEID_FIXED_PERSONAL_DATA;
  PEidFixedPersonalData = ^TEidFixedPersonalData;

  TNormFixedPersonalData = record
    PersonalNumber: WideString;
    Surname: WideString;
    GivenName: WideString;
    ParentGivenName: WideString;
    Sex: WideString;
    PlaceOfBirth: WideString;
    StateOfBirth: WideString;
    DateOfBirth: WideString;
    CommunityOfBirth: WideString;
  end;

  tagEID_VARIABLE_PERSONAL_DATA = record
    state: array[0..EID_MAX_State - 1] of AnsiChar;
    stateSize: Integer;
    community: array[0..EID_MAX_Community - 1] of AnsiChar;
    communitySize: Integer;
    place: array[0..EID_MAX_Place - 1] of AnsiChar;
    placeSize: Integer;
    street: array[0..EID_MAX_Street - 1] of AnsiChar;
    streetSize: Integer;
    houseNumber: array[0..EID_MAX_HouseNumber - 1] of AnsiChar;
    houseNumberSize: Integer;
    houseLetter: array[0..EID_MAX_HouseLetter - 1] of AnsiChar;
    houseLetterSize: Integer;
    entrance: array[0..EID_MAX_Entrance - 1] of AnsiChar;
    entranceSize: Integer;
    floor: array[0..EID_MAX_Floor - 1] of AnsiChar;
    floorSize: Integer;
    apartmentNumber: array[0..EID_MAX_ApartmentNumber - 1] of AnsiChar;
    apartmentNumberSize: Integer;
	  addressDate: array[0..EID_MAX_AddressDate - 1] of AnsiChar;
	  addressDateSize: integer;
	  addressLabel: array[0..EID_MAX_AddressLabel - 1] of AnsiChar;
	  addressLabelSize: integer;
  end;

  TNormVariablePersonalData = record
    State: WideString;
    Community: WideString;
    Place: WideString;
    Street: WideString;
    HouseNumber: WideString;
    HouseLetter: WideString;
    Entrance: WideString;
    Floor: WideString;
    ApartmentNumber: WideString;
    AddressDate: WideString; //ako nije bilo promena vraca 01.01.0001.
    AddressLabel: WideString;
  end;

  EID_VARIABLE_PERSONAL_DATA  = tagEID_VARIABLE_PERSONAL_DATA;
  PEID_VARIABLE_PERSONAL_DATA = ^tagEID_VARIABLE_PERSONAL_DATA;
  TEidVariablePersonalData = tagEID_VARIABLE_PERSONAL_DATA;
  PEidVariablePersonalData = ^TEidVariablePersonalData;

type
  tagEID_PORTRAIT = record
    portrait: array[0..EID_MAX_Portrait - 1] of BYTE;
    portraitSize: Integer;
  end;

  EID_PORTRAIT  = tagEID_PORTRAIT;
  PEID_PORTRAIT = ^tagEID_PORTRAIT;
  TEidPortrait = tagEID_PORTRAIT;
  PEidPortrait = ^TEidPortrait;

type
  tagEID_CERTIFICATE = record
    certificate: array[0..EID_MAX_Certificate - 1] of BYTE;
    certificateSize: Integer;
  end;

  EID_CERTIFICATE  = tagEID_CERTIFICATE;
  PEID_CERTIFICATE = ^tagEID_CERTIFICATE;
  TEidCertificate = tagEID_CERTIFICATE;
  PEidCertificate = ^TEidcertificate;

  UINT_PTR = Longword;

var
  EidSetOption: function(nOptionID: Integer; nOptionValue: UINT_PTR): Integer; stdcall;

  EidStartup: function(nApiVersion: Integer): Integer; stdcall;
  EidCleanup: function: Integer; stdcall;

  EidBeginRead: function(szReader: LPCSTR; nCardVersion: PInteger): Integer; stdcall;
  EidEndRead: function: Integer; stdcall;

  EidReadDocumentData: function(pData: PEID_DOCUMENT_DATA): Integer; stdcall;
  EidReadFixedPersonalData: function(pData: PEID_FIXED_PERSONAL_DATA): Integer; stdcall;
  EidReadVariablePersonalData: function(pData: PEID_VARIABLE_PERSONAL_DATA): Integer; stdcall;
  EidReadPortrait: function(pData: PEID_PORTRAIT): Integer; stdcall;
  EidReadCertificate: function(pData: PEID_CERTIFICATE): Integer; stdcall;

  EidChangePassword: function(szOldPassword, szNewPassword: LPCSTR; pnTriesLeft: PInteger): Integer; stdcall;
  EidVerifySignature: function(nSignatureID: UINT): Integer; stdcall;

  // Funkcije koje su izbacene u novom APIju
  {EidReadDocumentDataP: function(pchDocRegNo: PAnsiChar; pnDocRegNoSize: PInteger; pchIssuingDate: PAnsiChar; var pnIssuingDateSize: Integer;
    pchDocumentType: PAnsiChar; var pnDocumentTypeSize: Integer;
    pchExpiryDate: PAnsiChar; var pnExpiryDateSize: Integer; pchIssuingAuthority: PAnsiChar; var pnIssuingAuthoritySize: Integer): Integer; stdcall;

  EidReadFixedPersonalDataP: function(pchPersonalNumber: PAnsiChar; var pnPersonalNumberSize: Integer; pchSurname: PAnsiChar; var pnSurnameSize: Integer;
    pchGivenName: PAnsiChar; var pnGivenNameSize: Integer; pchParentGivenName: PAnsiChar; var pnParentGivenNameSize: Integer;
    pchSex: PAnsiChar; var pnSexSize: Integer; pchPlaceOfBirth: PAnsiChar; var pnPlaceOfBirthSize: Integer; pchStateOfBirth: PAnsiChar; var pnStateOfBirthSize: Integer;
    pchDateOfBirth: PAnsiChar; var pnDateOfBirthSize: Integer; pchCommunityOfBirth: PAnsiChar; var pnCommunityOfBirthSize: Integer): Integer; stdcall;

  EidReadVariablePersonalDataP: function(pchState: PAnsiChar; var pnStateSize: Integer; pchCommunity: PAnsiChar; var pnCommunitySize: Integer;
    pchPlace: PAnsiChar; var pnPlaceSize: Integer; pchStreet: PAnsiChar; var pnStreetSize: Integer;
    pchHouseNumber: PAnsiChar; var pnHouseNumberSize: Integer; pchHouseLetter: PAnsiChar; var pnHouseLetterSize: Integer;
    pchEntrance: PAnsiChar; var pnEntranceSize: Integer; pchFloor: PAnsiChar; var pnFloorSize: Integer;
    pchApartmentNumber: PAnsiChar; var pnApartmentNumberSize: Integer; pchAddressDate: PAnsiChar; var pnAddressDateSize: Integer;
    pchAddressLabel: PAnsiChar; var pnAddressLabelSize: Integer): Integer; stdcall;

  EidReadPortraitP: function(pchPortrait: PByte; var pnPortraitSize: Integer): Integer; stdcall;
  EidReadCertificateP: function(pchCertificate: PByte; var pnCertificateSize: Integer): Integer; stdcall;}

  function NormReadDocumentData(var AData: TNormDocumentData): Integer;
  function NormReadFixedPersonalData(var AData: TNormFixedPersonalData): Integer;
  function NormReadVariablePersonalData(var AData: TNormVariablePersonalData): Integer;
  function NormReadPicture(AStream: TStream): Integer;

implementation

var
  DLLHandle: HMODULE;

procedure LoadDLL;
begin
  DLLHandle := LoadLibrary('CelikApi.dll');

  if DLLHandle <> 0 then
  begin
    @EidStartup := GetProcAddress(DLLHandle, 'EidStartup');
    @EidCleanup := GetProcAddress(DLLHandle, 'EidCleanup');
    @EidBeginRead := GetProcAddress(DLLHandle, 'EidBeginRead');
    @EidEndRead := GetProcAddress(DLLHandle, 'EidEndRead');
    @EidReadDocumentData := GetProcAddress(DLLHandle, 'EidReadDocumentData');
    @EidReadFixedPersonalData := GetProcAddress(DLLHandle, 'EidReadFixedPersonalData');
    @EidReadVariablePersonalData := GetProcAddress(DLLHandle, 'EidReadVariablePersonalData');
    @EidReadPortrait := GetProcAddress(DLLHandle, 'EidReadPortrait');
    @EidChangePassword := GetProcAddress(DLLHandle, 'EidChangePassword');
    @EidVerifySignature := GetProcAddress(DLLHandle, 'EidVerifySignature');

    {@EidReadDocumentDataP := GetProcAddress(DLLHandle, 'EidReadDocumentDataP');
    @EidReadFixedPersonalDataP := GetProcAddress(DLLHandle, 'EidReadFixedPersonalDataP');
    @EidReadVariablePersonalDataP := GetProcAddress(DLLHandle, 'EidReadVariablePersonalDataP');
    @EidReadPortraitP := GetProcAddress(DLLHandle, 'EidReadPortraitP');}
  end;
end;

procedure NormGetData(var ADest: WideString; const ASource: PAnsiChar; const ASize: Integer);
var
  Temp: AnsiString;
begin
  // CelikAPi vraca sve stringove kao AnsiString u UTF8 formatu, konvertujemo ga u WideString
  SetString(Temp, ASource, ASize);
  ADest := UTF8Decode(Temp);
end;

function NormReadDocumentData(var AData: TNormDocumentData): Integer;
var
  Data: TEidDocumentData;
  Temp: AnsiString;
begin
  ZeroMemory(@Data, SizeOf(TEidDocumentData));
  EidReadDocumentData(@Data);

  NormGetData(AData.DocRegNo, Data.docRegNo, Data.docRegNoSize);
  NormGetData(AData.DocumentType, Data.documentType, Data.documentTypeSize);
  NormGetData(AData.IssuingDate, Data.issuingDate, Data.issuingDateSize);
  NormGetData(AData.ExpiryDate, Data.expiryDate, Data.expiryDateSize);
  NormGetData(AData.IssuingAuthority, Data.issuingAuthority, Data.issuingAuthoritySize);
end;

function NormReadFixedPersonalData(var AData: TNormFixedPersonalData): Integer;
var
  Data: TEidFixedPersonalData;
  Temp: AnsiString;
begin
  ZeroMemory(@Data, SizeOf(TEidFixedPersonalData));
  Result := EidReadFixedPersonalData(@Data);

  NormGetData(AData.PersonalNumber, Data.personalNumber, Data.personalNumberSize);
  NormGetData(AData.Surname, Data.surname, Data.surnameSize);
  NormGetData(AData.GivenName, Data.givenName, Data.givenNameSize);
  NormGetData(AData.ParentGivenName, Data.parentGivenName, Data.parentGivenNameSize);
  NormGetData(AData.Sex, Data.sex, Data.sexSize);
  NormGetData(AData.PlaceOfBirth, Data.placeOfBirth, Data.placeOfBirthSize);
  NormGetData(AData.StateOfBirth, Data.stateOfBirth, Data.stateOfBirthSize);
  NormGetData(AData.DateOfBirth, Data.dateOfBirth, Data.dateOfBirthSize);
  NormGetData(AData.CommunityOfBirth, Data.communityOfBirth, Data.communityOfBirthSize);
end;

function NormReadVariablePersonalData(var AData: TNormVariablePersonalData): Integer;
var
  Data: TEidVariablePersonalData;
  Temp: AnsiString;
begin
  ZeroMemory(@Data, SizeOf(TEidVariablePersonalData));
  Result := EidReadVariablePersonalData(@Data);

  NormGetData(AData.State, Data.state, Data.stateSize);
  NormGetData(AData.Community, Data.community, Data.communitySize);
  NormGetData(AData.Place, Data.place, Data.placeSize);
  NormGetData(AData.Street, Data.street, Data.streetSize);
  NormGetData(AData.HouseNumber, Data.houseNumber, Data.houseNumberSize);
  NormGetData(AData.HouseLetter, Data.houseLetter, Data.houseLetterSize);
  NormGetData(AData.Entrance, Data.entrance, Data.entranceSize);
  NormGetData(AData.Floor, Data.floor, Data.floorSize);
  NormGetData(AData.ApartmentNumber, Data.apartmentNumber, Data.apartmentNumberSize);
  NormGetData(AData.AddressDate, Data.addressDate, Data.addressDateSize);
  NormGetData(AData.AddressLabel, Data.addressLabel, Data.addressLabelSize);
end;

function NormReadPicture(AStream: TStream): Integer;
var
  Temp: TEidPortrait;
begin
  Result := EidReadPortrait(@Temp);

  AStream.Size := Temp.portraitSize;
  AStream.Position := 0;
  AStream.Write(Temp.portrait, Temp.portraitSize);
end;

initialization
  LoadDLL;

finalization
  if DLLHandle <> 0 then
    FreeLibrary(DLLHandle)

end.
