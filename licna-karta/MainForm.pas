unit MainForm;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, TntStdCtrls, ExtCtrls,
  CelikApi;

type
  TForm1 = class(TForm)
    btnCitajKarticu: TButton;
    btnSaveMemoToFile: TButton;
    btnLoadFromFile: TButton;
    mmoPodaci: TTntMemo;
    imgPic: TImage;
    procedure btnCitajKarticuClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure btnSaveMemoToFileClick(Sender: TObject);
    procedure btnLoadFromFileClick(Sender: TObject);
  private
    procedure DisplayData(const ADocData: TNormDocumentData; AFixPerData: TNormFixedPersonalData; AVarPerData: TNormVariablePersonalData);
    procedure DisplayPicture(const AImg: TStream);
  public

  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

uses
  Jpeg;

procedure TForm1.btnCitajKarticuClick(Sender: TObject);
var
  DocData: TNormDocumentData;
  FixedData: TNormFixedPersonalData;
  VariableData: TNormVariablePersonalData;
  ms: TMemoryStream;
begin
  ms := TMemoryStream.Create;
  try
    EidBeginRead('', nil);
    NormReadDocumentData(DocData);
    NormReadFixedPersonalData(FixedData);
    NormReadVariablePersonalData(VariableData);
    NormReadPicture(ms);
    EidEndRead;

    DisplayData(DocData, FixedData, VariableData);
    DisplayPicture(ms);
  finally
    ms.Free;
  end;
end;

procedure TForm1.DisplayData(const ADocData: TNormDocumentData; AFixPerData: TNormFixedPersonalData; AVarPerData: TNormVariablePersonalData);
begin
  mmoPodaci.Clear;
  mmoPodaci.Lines.Add('Reg.no: ' + ADocData.DocRegNo);
  mmoPodaci.Lines.Add('Tip dokumenta: ' + ADocData.DocumentType);
  mmoPodaci.Lines.Add('Datum izdavanja: ' + ADocData.IssuingDate);
  mmoPodaci.Lines.Add('Datum isteka: ' + ADocData.ExpiryDate);
  mmoPodaci.Lines.Add('Izdaje: ' + ADocData.IssuingAuthority);

  mmoPodaci.Lines.Add('-----------------------------------------');
  mmoPodaci.Lines.Add('Prezime: ' + AFixPerData.Surname);
  mmoPodaci.Lines.Add('Ime: ' + AFixPerData.GivenName);
  mmoPodaci.Lines.Add('Ime roditelja: ' + AFixPerData.ParentGivenName);
  mmoPodaci.Lines.Add('Pol: ' + AFixPerData.Sex);
  mmoPodaci.Lines.Add('Mesto roðenja: ' + AFixPerData.PlaceOfBirth);
  mmoPodaci.Lines.Add('Država: ' + AFixPerData.StateOfBirth);
  mmoPodaci.Lines.Add('Datum roðenja: ' + AFixPerData.DateOfBirth);
  mmoPodaci.Lines.Add('Opština: ' + AFixPerData.CommunityOfBirth);

  mmoPodaci.Lines.Add('-----------------------------------------');
  mmoPodaci.Lines.Add('Prebivalište - oznaka države: ' + AVarPerData.state);
  mmoPodaci.Lines.Add('Prebivalište - opština: ' + AVarPerData.community);
  mmoPodaci.Lines.Add('Prebivalište - mesto: ' + AVarPerData.place);
  mmoPodaci.Lines.Add('Prebivalište - ulica: ' + AVarPerData.street);
  mmoPodaci.Lines.Add('Prebivalište - kuæni broj: ' + AVarPerData.houseNumber);
  mmoPodaci.Lines.Add('Prebivalište - slovo uz broj: ' + AVarPerData.houseLetter);
  mmoPodaci.Lines.Add('Prebivalište - ulaz: ' + AVarPerData.entrance);
  mmoPodaci.Lines.Add('Prebivalište - sprat: ' + AVarPerData.floor);
  mmoPodaci.Lines.Add('Prebivalište - broj stana: ' + AVarPerData.apartmentNumber);
  mmoPodaci.Lines.Add('Prebivalište - promena datuma: ' + AVarPerData.AddressDate);
  mmoPodaci.Lines.Add('Prebivalište - adresna oznaka: ' + AVarPerData.AddressLabel);
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  EidStartup(2);
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  EidCleanup;
end;

procedure TForm1.btnSaveMemoToFileClick(Sender: TObject);
var
  DocData: TEidDocumentData;
  FixedData: TEidFixedPersonalData;
  VariableData: TEidVariablePersonalData;
  f: TFileStream;
begin
  FillChar(DocData, SizeOf(TEidDocumentData), 0);
  FillChar(FixedData, SizeOf(TEidFixedPersonalData), 0);
  FillChar(VariableData, SizeOf(TEidVariablePersonalData), 0);

  EidBeginRead('', nil);
  EidReadDocumentData(@DocData);
  EidReadFixedPersonalData(@FixedData);
  EidReadVariablePersonalData(@VariableData);
  EidEndRead;

  f := TFileStream.Create('Data.dat', fmCreate);
  try
    f.Write(DocData, SizeOf(TEidDocumentData));
    f.Write(FixedData, SizeOf(TEidFixedPersonalData));
    f.Write(VariableData, SizeOf(TEidVariablePersonalData));
  finally
    f.Free;
  end;
end;

procedure TForm1.btnLoadFromFileClick(Sender: TObject);
var
  DocData: TEidDocumentData;
  FixedData: TEidFixedPersonalData;
  VariableData: TEidVariablePersonalData;
  f: TFileStream;
begin
  f := TFileStream.Create('Data.dat', fmOpenRead);
  try
    f.Read(DocData, SizeOf(TEidDocumentData));
    f.Read(FixedData, SizeOf(TEidFixedPersonalData));
    f.Read(VariableData, SizeOf(TEidVariablePersonalData));
  finally
    f.Free;
  end;
end;

procedure TForm1.DisplayPicture(const AImg: TStream);
var
  img: TJPEGImage;
  fs: TFileStream;
begin
  if AImg.Size > 0 then
  begin
    fs := nil;
    img := TJPEGImage.Create;
    try
      AImg.Position := 0;
      img.LoadFromStream(AImg);
      imgPic.Picture.Assign(img);

      fs := TFileStream.Create('IDPicture.jpg', fmCreate);
      AImg.Position := 0;      
      fs.CopyFrom(AImg, 0);
    finally
      img.Free;
      fs.Free;
    end;
  end;
end;

end.

