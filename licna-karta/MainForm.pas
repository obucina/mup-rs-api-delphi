unit MainForm;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, CelikApi, Vcl.ExtCtrls;

type
  TForm1 = class(TForm)
    btnCitajLKEid: TButton;
    btnCitajLKNorm: TButton;
    Image1: TImage;
    btnSacuvajFotografijuUFajl: TButton;
    btnSacuvajPodatkeUFajl: TButton;
    btnUcitajPodatkeIzFajla: TButton;
    mmoPodaci: TMemo;
    SaveDialog1: TSaveDialog;
    OpenDialog1: TOpenDialog;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure btnCitajLKEidClick(Sender: TObject);
    procedure btnCitajLKNormClick(Sender: TObject);
    procedure btnSacuvajFotografijuUFajlClick(Sender: TObject);
    procedure btnSacuvajPodatkeUFajlClick(Sender: TObject);
    procedure btnUcitajPodatkeIzFajlaClick(Sender: TObject);
  private
    { Private declarations }

    VerzijaLK: Integer;

    EidDocumentData: TEidDocumentData;
    EidFixedPersonalData: TEidFixedPersonalData;
    EidVariablePersonalData: TEidVariablePersonalData;
    EidPortrait: TEidPortrait;

    NormDocumentData: TNormDocumentData;
    NormFixedPersonalData: TNormFixedPersonalData;
    NormVariablePersonalData: TNormVariablePersonalData;
    NormPortrait: TMemoryStream;

    procedure IzbrisiPodatke();
    procedure PrikaziPodatke();
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

uses
  Jpeg;

procedure TForm1.FormCreate(Sender: TObject);
begin
  CelikApi.LoadDll();
  EidStartup(2);
  NormPortrait := TMemoryStream.Create;
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  NormPortrait.Free;
  EidCleanup;
  CelikApi.UnloadDll();
end;

procedure TForm1.btnCitajLKEidClick(Sender: TObject);
begin
  IzbrisiPodatke();

  ZeroMemory(@EidDocumentData, SizeOf(TEidDocumentData));
  ZeroMemory(@EidFixedPersonalData, SizeOf(TEidFixedPersonalData));
  ZeroMemory(@EidVariablePersonalData, SizeOf(TEidVariablePersonalData));
  ZeroMemory(@EidPortrait, SizeOf(TEidPortrait));

  EidBeginRead('', @VerzijaLK);
  EidReadDocumentData(@EidDocumentData);
  EidReadFixedPersonalData(@EidFixedPersonalData);
  EidReadVariablePersonalData(@EidVariablePersonalData);
  EidReadPortrait(@EidPortrait);
  EidEndRead();

  EidToNormDocumentData(EidDocumentData, NormDocumentData);
  EidToNormFixedPersonalData(EidFixedPersonalData, NormFixedPersonalData);
  EidToNormVariablePersonalData(EidVariablePersonalData, NormVariablePersonalData);
  EidToNormPortrait(EidPortrait, NormPortrait);

  PrikaziPodatke();
end;

procedure TForm1.btnCitajLKNormClick(Sender: TObject);
begin
  IzbrisiPodatke();

  EidBeginRead('', @VerzijaLK);
  NormReadDocumentData(NormDocumentData);
  NormReadFixedPersonalData(NormFixedPersonalData);
  NormReadVariablePersonalData(NormVariablePersonalData);
  NormReadPortrait(NormPortrait);
  EidEndRead;

  PrikaziPodatke();
end;

procedure TForm1.btnSacuvajFotografijuUFajlClick(Sender: TObject);
begin
  SaveDialog1.FileName := NormFixedPersonalData.Surname + '-' + NormFixedPersonalData.GivenName + '.jpg';
  if (SaveDialog1.Execute) then
  begin
    NormPortrait.SaveToFile(SaveDialog1.FileName);
  end;
end;

procedure TForm1.btnSacuvajPodatkeUFajlClick(Sender: TObject);
var
  StrukturaLK: TFileStream;
begin
  SaveDialog1.FileName := NormFixedPersonalData.Surname + '-' + NormFixedPersonalData.GivenName + '.dat';
  if (SaveDialog1.Execute) then
  begin
    StrukturaLK := TFileStream.Create(SaveDialog1.FileName, fmCreate);
    StrukturaLK.Write(EidDocumentData, sizeof(TEidDocumentData));
    StrukturaLK.Write(EidFixedPersonalData, sizeof(TEidFixedPersonalData));
    StrukturaLK.Write(EidVariablePersonalData, sizeof(TEidVariablePersonalData));
    StrukturaLK.Write(EidPortrait, sizeof(TEidPortrait));
    StrukturaLK.Free;
  end;
end;

procedure TForm1.btnUcitajPodatkeIzFajlaClick(Sender: TObject);
var
  StrukturaLK: TFileStream;
begin
  IzbrisiPodatke();

  OpenDialog1.FileName := NormFixedPersonalData.Surname + '-' + NormFixedPersonalData.GivenName + '.dat';
  if (OpenDialog1.Execute) then
  begin
    StrukturaLK := TFileStream.Create(OpenDialog1.FileName, fmOpenRead);
    StrukturaLK.Read(EidDocumentData, SizeOf(TEidDocumentData));
    StrukturaLK.Read(EidFixedPersonalData, SizeOf(TEidFixedPersonalData));
    StrukturaLK.Read(EidVariablePersonalData, SizeOf(TEidVariablePersonalData));
    StrukturaLK.Read(EidPortrait, SizeOf(TEidPortrait));
    StrukturaLK.Free;

    EidToNormDocumentData(EidDocumentData, NormDocumentData);
    EidToNormFixedPersonalData(EidFixedPersonalData, NormFixedPersonalData);
    EidToNormVariablePersonalData(EidVariablePersonalData, NormVariablePersonalData);
    EidToNormPortrait(EidPortrait, NormPortrait);

    PrikaziPodatke();
  end;
end;

procedure TForm1.IzbrisiPodatke();
begin
  mmoPodaci.Clear;
  Image1.Picture.Assign(nil);
end;

procedure TForm1.PrikaziPodatke();
var
  PortraitJPG: TJPEGImage;
begin
  IzbrisiPodatke();

  mmoPodaci.Lines.Add('Verzija lične karte: ' + IntToStr(VerzijaLK));

  mmoPodaci.Lines.Add('----- Podaci o dokumentu ----------------');
  mmoPodaci.Lines.Add('Reg.no: ' + NormDocumentData.DocRegNo);
  mmoPodaci.Lines.Add('Tip dokumenta: ' + NormDocumentData.DocumentType);
  mmoPodaci.Lines.Add('Datum izdavanja: ' + NormDocumentData.IssuingDate);
  mmoPodaci.Lines.Add('Datum isteka: ' + NormDocumentData.ExpiryDate);
  mmoPodaci.Lines.Add('Izdaje: ' + NormDocumentData.IssuingAuthority);

  mmoPodaci.Lines.Add('----- Podaci o osobi, nepromenljivi -----');
  mmoPodaci.Lines.Add('Matični broj: ' + NormFixedPersonalData.PersonalNumber);
  mmoPodaci.Lines.Add('Prezime: ' + NormFixedPersonalData.Surname);
  mmoPodaci.Lines.Add('Ime: ' + NormFixedPersonalData.GivenName);
  mmoPodaci.Lines.Add('Ime roditelja: ' + NormFixedPersonalData.ParentGivenName);
  mmoPodaci.Lines.Add('Pol: ' + NormFixedPersonalData.Sex);
  mmoPodaci.Lines.Add('Mesto rođenja: ' + NormFixedPersonalData.PlaceOfBirth);
  mmoPodaci.Lines.Add('Država: ' + NormFixedPersonalData.StateOfBirth);
  mmoPodaci.Lines.Add('Datum rođenja: ' + NormFixedPersonalData.DateOfBirth);
  mmoPodaci.Lines.Add('Opština: ' + NormFixedPersonalData.CommunityOfBirth);

  mmoPodaci.Lines.Add('----- Podaci o osobi, promenljivi -------');
  mmoPodaci.Lines.Add('Prebivalište - oznaka države: ' + EidVariablePersonalData.state);
  mmoPodaci.Lines.Add('Prebivalište - opština: ' + EidVariablePersonalData.community);
  mmoPodaci.Lines.Add('Prebivalište - mesto: ' + EidVariablePersonalData.place);
  mmoPodaci.Lines.Add('Prebivalište - ulica: ' + EidVariablePersonalData.street);
  mmoPodaci.Lines.Add('Prebivalište - kućni broj: ' + EidVariablePersonalData.houseNumber);
  mmoPodaci.Lines.Add('Prebivalište - slovo uz broj: ' + EidVariablePersonalData.houseLetter);
  mmoPodaci.Lines.Add('Prebivalište - ulaz: ' + EidVariablePersonalData.entrance);
  mmoPodaci.Lines.Add('Prebivalište - sprat: ' + EidVariablePersonalData.floor);
  mmoPodaci.Lines.Add('Prebivalište - broj stana: ' + EidVariablePersonalData.apartmentNumber);
  mmoPodaci.Lines.Add('Prebivalište - promena datuma: ' + EidVariablePersonalData.AddressDate);
  mmoPodaci.Lines.Add('Prebivalište - adresna oznaka: ' + EidVariablePersonalData.AddressLabel);

  if NormPortrait.Size > 0 then
  begin
    PortraitJPG := TJPEGImage.Create;
    try
      NormPortrait.Position := 0;
      PortraitJPG.LoadFromStream(NormPortrait);
      Image1.Picture.Assign(PortraitJPG);
    finally
      PortraitJPG.Free;
    end;
  end;
end;

end.
