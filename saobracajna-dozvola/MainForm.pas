unit MainForm;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, eVehicleRegistrationAPI;

type
  TForm1 = class(TForm)
    btnCitajSDSd: TButton;
    btnCitajSDNorm: TButton;
    btnSacuvajPodatkeUFajl: TButton;
    btnUcitajPodatkeIzFajla: TButton;
    mmoPodaci: TMemo;
    SaveDialog1: TSaveDialog;
    OpenDialog1: TOpenDialog;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure btnCitajSDSdClick(Sender: TObject);
    procedure btnCitajSDNormClick(Sender: TObject);
    procedure btnSacuvajPodatkeUFajlClick(Sender: TObject);
    procedure btnUcitajPodatkeIzFajlaClick(Sender: TObject);
  private
    { Private declarations }

    ReaderName: array[0..255] of AnsiChar;
    ReaderNameSize: LongInt;

    SdRegistrationData: TSdRegistrationData;
    SdDocumentData: TSdDocumentData;
    SdVehicleData: TSdVehicleData;
    SdPersonalData: TSdPersonalData;

    NormRegistrationData: TNormRegistrationData;
    NormDocumentData: TNormDocumentData;
    NormVehicleData: TNormVehicleData;
    NormPersonalData: TNormPersonalData;

    procedure IzbrisiPodatke();
    procedure PrikaziPodatke();
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

procedure TForm1.FormCreate(Sender: TObject);
begin
  ReaderNameSize := Length(ReaderName);
  ZeroMemory(@ReaderName, ReaderNameSize);
  eVehicleRegistrationAPI.LoadDll();
  sdStartup(0);
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  sdCleanup();
  eVehicleRegistrationAPI.UnloadDLL();
end;

procedure TForm1.btnCitajSDSdClick(Sender: TObject);
begin
  IzbrisiPodatke();

  ReaderNameSize := Length(ReaderName);
  GetReaderName(0, @ReaderName[0], @ReaderNameSize);
  SelectReader(@ReaderName[0]);
  sdProcessNewCard();

  ZeroMemory(@SdRegistrationData, SizeOf(TSdRegistrationData));
  ZeroMemory(@SdDocumentData, SizeOf(TSdDocumentData));
  ZeroMemory(@SdVehicleData, SizeOf(TSdVehicleData));
  ZeroMemory(@SdPersonalData, SizeOf(TSdPersonalData));

  sdReadRegistration(@SdRegistrationData, 1);
  sdReadDocumentData(@SdDocumentData);
  sdReadVehicleData(@SdVehicleData);
  sdReadPersonalData(@SdPersonalData);

  SdToNormRegistrationData(SdRegistrationData, NormRegistrationData);
  SdToNormDocumentData(SdDocumentData, NormDocumentData);
  SdToNormVehicleData(SdVehicleData, NormVehicleData);
  SdToNormPersonalData(SdPersonalData, NormPersonalData);

  PrikaziPodatke();
end;

procedure TForm1.btnCitajSDNormClick(Sender: TObject);
begin
  IzbrisiPodatke();

  ReaderNameSize := Length(ReaderName);
  GetReaderName(0, @ReaderName[0], @ReaderNameSize);
  SelectReader(@ReaderName[0]);
  sdProcessNewCard();

  NormReadRegistrationData(NormRegistrationData);
  NormReadDocumentData(NormDocumentData);
  NormReadVehicleData(NormVehicleData);
  NormReadPersonalData(NormPersonalData);

  PrikaziPodatke();
end;

procedure TForm1.btnSacuvajPodatkeUFajlClick(Sender: TObject);
var
  StrukturaSD: TFileStream;
begin
  SaveDialog1.FileName := NormVehicleData.RegistrationNumberOfVehicle + '.dat';
  if (SaveDialog1.Execute) then
  begin
    StrukturaSD := TFileStream.Create(SaveDialog1.FileName, fmCreate);
    StrukturaSD.Write(SdRegistrationData, sizeof(TSdRegistrationData));
    StrukturaSD.Write(SdDocumentData, sizeof(TSdDocumentData));
    StrukturaSD.Write(SdVehicleData, sizeof(TSdVehicleData));
    StrukturaSD.Write(SdPersonalData, sizeof(TSdPersonalData));
    StrukturaSD.Free;
  end;
end;

procedure TForm1.btnUcitajPodatkeIzFajlaClick(Sender: TObject);
var
  StrukturaSD: TFileStream;
begin
  IzbrisiPodatke();

  OpenDialog1.FileName := NormVehicleData.RegistrationNumberOfVehicle + '.dat';
  if (OpenDialog1.Execute) then
  begin
    StrukturaSD := TFileStream.Create(OpenDialog1.FileName, fmOpenRead);
    StrukturaSD.Read(SdRegistrationData, SizeOf(TSdRegistrationData));
    StrukturaSD.Read(SdDocumentData, SizeOf(TSdDocumentData));
    StrukturaSD.Read(SdVehicleData, SizeOf(TSdVehicleData));
    StrukturaSD.Read(SdPersonalData, SizeOf(TSdPersonalData));
    StrukturaSD.Free;

    SdToNormRegistrationData(SdRegistrationData, NormRegistrationData);
    SdToNormDocumentData(SdDocumentData, NormDocumentData);
    SdToNormVehicleData(SdVehicleData, NormVehicleData);
    SdToNormPersonalData(SdPersonalData, NormPersonalData);

    PrikaziPodatke();
  end;
end;

procedure TForm1.IzbrisiPodatke();
begin
  mmoPodaci.Clear;
end;

procedure TForm1.PrikaziPodatke();
begin
  IzbrisiPodatke();

  mmoPodaci.Lines.Add('Čitač: ' + ReaderName);

  mmoPodaci.Lines.Add('----- Podaci o registraciji 1 -----------');
  mmoPodaci.Lines.Add('Podaci o registraciji: ' + NormRegistrationData.RegistrationData);
  mmoPodaci.Lines.Add('Podaci o digitalnom potpisu: ' + NormRegistrationData.SignatureData);
  mmoPodaci.Lines.Add('Izdao: ' + NormRegistrationData.IssuingAuthority);

  mmoPodaci.Lines.Add('----- Podaci o dokumentu ----------------');
  mmoPodaci.Lines.Add('Država izdavanja: ' + NormDocumentData.StateIssuing);
  mmoPodaci.Lines.Add('Ovlašćeni organ: ' + NormDocumentData.CompetentAuthority);
  mmoPodaci.Lines.Add('Dokument izdao: ' + NormDocumentData.AuthorityIssuing);
  mmoPodaci.Lines.Add('Broj dokumenta: ' + NormDocumentData.UnambiguousNumber);
  mmoPodaci.Lines.Add('Datum izdavanja: ' + NormDocumentData.IssuingDate);
  mmoPodaci.Lines.Add('Važi do: ' + NormDocumentData.ExpiryDate);
  mmoPodaci.Lines.Add('Serijski broj: ' + NormDocumentData.SerialNumber);

  mmoPodaci.Lines.Add('----- Podaci o vozilu -------------------');
  mmoPodaci.Lines.Add('Datum prve registracije: ' + NormVehicleData.DateOfFirstRegistration);
  mmoPodaci.Lines.Add('Godina proizvodnje: ' + NormVehicleData.YearOfProduction);
  mmoPodaci.Lines.Add('Marka: ' + NormVehicleData.VehicleMake);
  mmoPodaci.Lines.Add('Tip: ' + NormVehicleData.VehicleType);
  mmoPodaci.Lines.Add('Model: ' + NormVehicleData.CommercialDescription);
  mmoPodaci.Lines.Add('Broj šasije : ' + NormVehicleData.VehicleIDNumber);
  mmoPodaci.Lines.Add('Registarski broj: ' + NormVehicleData.RegistrationNumberOfVehicle);
  mmoPodaci.Lines.Add('Snaga motora: ' + NormVehicleData.MaximumNetPower);
  mmoPodaci.Lines.Add('Zapremina motora: ' + NormVehicleData.EngineCapacity);
  mmoPodaci.Lines.Add('Gorivo: ' + NormVehicleData.TypeOfFuel);
  mmoPodaci.Lines.Add('Odnos snage i mase: ' + NormVehicleData.PowerWeightRatio);
  mmoPodaci.Lines.Add('Masa: ' + NormVehicleData.VehicleMass);
  mmoPodaci.Lines.Add('Najveća dozvoljena masa: ' + NormVehicleData.MaximumPermissibleLadenMass);
  mmoPodaci.Lines.Add('Homologacijska oznaka: ' + NormVehicleData.TypeApprovalNumber);
  mmoPodaci.Lines.Add('Broj mesta za sedenje: ' + NormVehicleData.NumberOfSeats);
  mmoPodaci.Lines.Add('Broj mesta za stajanje: ' + NormVehicleData.NumberOfStandingPlaces);
  mmoPodaci.Lines.Add('Broj motora: ' + NormVehicleData.EngineIDNumber);
  mmoPodaci.Lines.Add('Broj osovina: ' + NormVehicleData.NumberOfAxles);
  mmoPodaci.Lines.Add('Kategorija: ' + NormVehicleData.VehicleCategory);
  mmoPodaci.Lines.Add('Boja: ' + NormVehicleData.ColourOfVehicle);
  mmoPodaci.Lines.Add('Zabrana otuđenja: ' + NormVehicleData.RestrictionToChangeOwner);
  mmoPodaci.Lines.Add('Nosivost vozila: ' + NormVehicleData.VehicleLoad);

  mmoPodaci.Lines.Add('----- Liиni podaci ----------------------');
  mmoPodaci.Lines.Add('Vlasnik, Matični broj: ' + NormPersonalData.OwnersPersonalNo);
  mmoPodaci.Lines.Add('Vlasnik, Prezime: ' + NormPersonalData.OwnersSurnameOrBusinessName);
  mmoPodaci.Lines.Add('Vlasnik, Ime: ' + NormPersonalData.OwnerName);
  mmoPodaci.Lines.Add('Vlasnik, Adresa: ' + NormPersonalData.OwnerAddress);
  mmoPodaci.Lines.Add('Korisnik, Matični broj: ' + NormPersonalData.UsersPersonalNo);
  mmoPodaci.Lines.Add('Korisnik, Prezime: ' + NormPersonalData.UsersSurnameOrBusinessName);
  mmoPodaci.Lines.Add('Korisnik, Ime: ' + NormPersonalData.UsersName);
  mmoPodaci.Lines.Add('Korisnik, Adresa: ' + NormPersonalData.UsersAddress);
end;

end.
