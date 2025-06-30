unit paradoxds;

{ TParadoxdataSet
  Christian Ulrich christian@ullihome.de
  License: LGPL
}

{$mode objfpc}{$H+}

{$IF FPC_FullVersion >= 30200}
  {$WARN 6058 off : Call to subroutine "$1" marked as inline is not inlined}
{$IFEND}

interface

uses                               
  Classes, SysUtils, db, lconvencoding, bufdataset_parser;


const
  { Paradox codes for field types }
  pxfAlpha        = $01;
  pxfDate         = $02;
  pxfShort        = $03;
  pxfLong         = $04;
  pxfCurrency     = $05;
  pxfNumber       = $06;
  pxfLogical      = $09;
  pxfMemoBLOb     = $0C;
  pxfBLOb         = $0D;
  pxfFmtMemoBLOb  = $0E;
  pxfOLE          = $0F;
  pxfGraphic      = $10;
  pxfTime         = $14;
  pxfTimestamp    = $15;
  pxfAutoInc      = $16;
  pxfBCD          = $17;
  pxfBytes        = $18;


type
  {Internal Record information}
  PRecInfo = ^TRecInfo;
  TRecInfo = packed record
    RecordNumber: PtrInt;
    BookmarkFlag: TBookmarkFlag;
  end;
  
  PLongWord = ^Longword;

  { field information record used in TPxHeader below }
  PFldInfoRec = ^TFldInfoRec;
  TFldInfoRec = packed record
    fType: byte;
    fSize: byte;
  end;

  PPxHeader = ^TPxHeader;
  TPxHeader =  packed record
    recordSize              :  word;
    headerSize              :  word;
    fileType                :  byte;
    maxTableSize            :  byte;
    numRecords              :  longint;
    nextBlock               :  word;
    fileBlocks              :  word;
    firstBlock              :  word;
    lastBlock               :  word;
    unknown12x13            :  word;
    modifiedFlags1          :  byte;
    indexFieldNumber        :  byte;
    primaryIndexWorkspace   :  longint;   // currently not used; cast to "pointer"
    unknownPtr1A            :  longint;   // not used; cast to pointer;
    unknown1Ex20            :  array[$001E..$0020] of byte;
    numFields               :  smallint;
    primaryKeyFields        :  smallint;
    encryption1             :  longint;
    sortOrder               :  byte;
    modifiedFlags2          :  byte;
    unknown2Bx2C            :  array[$002B..$002C] of byte;
    changeCount1            :  byte;
    changeCount2            :  byte;
    unknown2F               :  byte;
    tableNamePtrPtr         :  longint;  // must be cast to ^pchar
    fldInfo                 :  longint;  // use FFieldInfoPtr instead
    writeProtected          :  byte;
    fileVersionID           :  byte;
    maxBlocks               :  word;
    unknown3C               :  byte;
    auxPasswords            :  byte;
    unknown3Ex3F            :  array[$003E..$003F] of byte;
    cryptInfoStartPtr       :  longint;    // not used; cast to pointer
    cryptInfoEndPtr         :  longint;    // not used; cast to pointer
    unknown48               :  byte;
    autoIncVal              :  longint;
    unknown4Dx4E            :  array[$004D..$004E] of byte;
    indexUpdateRequired     :  byte;
    unknown50x54            :  array[$0050..$0054] of byte;
    refIntegrity            :  byte;
    unknown56x57            :  array[$0056..$0057] of byte;
    case smallint of
      3:   (fieldInfo35     :  array[1..255] of TFldInfoRec);
      4:   (fileVerID2      :  smallint;
            fileVerID3      :  smallint;
            encryption2     :  longint;
            fileUpdateTime  :  longint;  { 4.0 only }
            hiFieldID       :  word;
            hiFieldIDinfo   :  word;
            sometimesNumFields:smallint;
            dosCodePage     :  word;
            unknown6Cx6F    :  array[$006C..$006F] of byte;
            changeCount4    :  smallint;
            unknown72x77    :  array[$0072..$0077] of byte;
            fieldInfo       :  array[1..255] of TFldInfoRec);

    { This is only the first part of the file header.  The last field
      is described as an array of 255 elements, but its size is really
      determined by the number of fields in the table.  The actual
      table header has more information that follows. }
  end;

  {Paradox Data Block Header}
  PDataBlock  = ^TDataBlock;
  TDataBlock  = packed RECORD
    nextBlock     : word;
    prevBlock     : word;
    addDataSize   : smallint;
    //fileData      : array[0..$0FF9] of byte;
    { fileData size varies according to maxTableSize }
  end;

  TPxField = record
    Info: PFldInfoRec;
    Offset: LongInt;
    Name: String;
  end;

  {10-byte Blob Info Block}
  TPxBlobInfo = packed record
    FileLoc: LongWord;
    Length: LongWord;
    ModCount: Word;
  end;

  {Blob Pointer Array Entry}
  TPxBlobIndex = packed record
    Offset: Byte;
    Len16: Byte;
    ModCount: Word;
    Len: Byte;
  end;


  { TParadoxDataset }

  TParadoxDataset = class(TDataset)
  private
    FActive: Boolean;
    FStream: TStream;
    FBlobStream: TStream;
    FFileName: TFileName;
    FHeader: PPxHeader;
    FaRecord: LongInt;  // was: LongWord;
    FaBlockstart: LongInt;
    FaBlock: PDataBlock;
    FaBlockIdx: word;
    FBlockReaded: Boolean;
    FFieldInfoPtr: PFldInfoRec;
    FTableNameLen: Integer;
    FInputEncoding: String;
    FTargetEncoding: String;
    FPxFields: Array of TPxField;
    FFilterBuffer : TRecordBuffer;
    FParser: TBufDatasetParser;
    function GetEncrypted: Boolean;
    function GetInputEncoding: String; inline;
    function GetPrimaryKeyFieldCount: Integer;
    function GetTargetEncoding: String; inline;
    function GetVersion: String;
    function IsStoredTargetEncoding: Boolean;
    function PxFilterRecord(Buffer: TRecordBuffer): Boolean;
    function PxGetActiveBuffer(var Buffer: TRecordBuffer): Boolean;
    procedure ReadBlock;
    procedure ReadNextBlockHeader;
    procedure ReadPrevBlockHeader;
    procedure SetFileName(const AValue: TFileName);
    procedure SetTargetEncoding(AValue: String);
  protected
    function  AllocRecordBuffer: PChar; override;
    procedure FreeRecordBuffer(var Buffer: PChar); override;
    procedure GetBookmarkData(Buffer: PChar; Data: Pointer); override;
    function  GetBookmarkFlag(Buffer: PChar): TBookmarkFlag; override;
    function  GetCanModify: Boolean;override;
    function  GetRecNo: Integer; override;
    function  GetRecord(Buffer: PChar; GetMode: TGetMode; {%H-}DoCheck: Boolean): TGetResult; override;
    function  GetRecordCount: Integer; override;
    function  GetRecordSize: Word; override;
    procedure InternalClose; override;
    procedure InternalEdit; override;
    procedure InternalFirst; override;
    procedure InternalGotoBookmark(ABookmark: Pointer); override;
//    procedure InternalHandleException; override;
    procedure InternalInitFieldDefs; override;
    procedure InternalInitRecord({%H-}Buffer: PChar); override;
    procedure InternalLast; override;
    procedure InternalOpen; override;
    procedure InternalPost; override;
    procedure InternalSetToRecord(Buffer: PChar); override;
    function  IsCursorOpen: Boolean; override;
    procedure ParseFilter(const AFilter: string);
    procedure SetBookmarkData({%H-}Buffer: PChar; {%H-}Data: Pointer); override;
    procedure SetBookmarkFlag(Buffer: PChar; Value: TBookmarkFlag); override;
    procedure SetFiltered(Value: Boolean); override;
    procedure SetFilterText(const Value: String); override;
    procedure SetRecNo(Value: Integer); override;
  public
    constructor Create(AOwner: TComponent); override;
    function BookmarkValid(ABookmark: TBookmark): Boolean; override;
    function CompareBookmarks(Bookmark1, Bookmark2: TBookmark): Longint; override;
    function CreateBlobStream(Field: TField; Mode: TBlobStreamMode): TStream; override;
    function GetFieldData(Field: TField; Buffer: Pointer): Boolean; override;
    procedure SetFieldData({%H-}Field: TField; {%H-}Buffer: Pointer); override;
    property Encrypted: Boolean read GetEncrypted;
    property PrimaryKeyFieldCount: Integer read GetPrimaryKeyFieldCount;
  published
    property TableName: TFileName read FFileName write SetFileName;
    property TableLevel: String read GetVersion;
    property InputEncoding: String read FInputEncoding write FInputEncoding;
    property TargetEncoding: String read FTargetEncoding write SetTargetEncoding stored IsStoredTargetEncoding;
    property Active;
    property AutoCalcFields;
    property FieldDefs;
    property Filter;
    property Filtered;
    property BeforeOpen;
    property AfterOpen;
    property BeforeClose;
    property AfterClose;
//    property BeforeInsert;
//    property AfterInsert;
//    property BeforeEdit;
//    property AfterEdit;
//    property BeforePost;
//    property AfterPost;
//    property BeforeCancel;
//    property AfterCancel;
//    property BeforeDelete;
//    property AfterDelete;
    property BeforeScroll;
    property AfterScroll;
//    property BeforeRefresh;
//    property AfterRefresh;
    property OnCalcFields;
//    property OnDeleteError;
//    property OnEditError;
    property OnFilterRecord;
//    property OnNewRecord;
//    property OnPostError;
  end;


implementation

{ TParadoxDataset }

constructor TParadoxDataset.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FHeader := nil;
  FTargetEncoding := Uppercase(EncodingUTF8);
  FInputEncoding := '';
  BookmarkSize := SizeOf(LongWord);
end;

function TParadoxDataset.AllocRecordBuffer: PChar;
begin
  if Assigned(Fheader) then
    Result := AllocMem(GetRecordSize)
  else
    Result := nil;
end;

function TParadoxDataset.BookmarkValid(ABookmark: TBookmark): Boolean;
begin
  Result := Assigned(ABookmark) and (Length(ABookmark) >= BookmarkSize);
end;

function TParadoxDataset.CompareBookmarks(Bookmark1, Bookmark2: TBookmark): Longint;
var
  idx1, idx2: LongWord;
begin
  idx1 := PLongWord(Bookmark1)^;
  idx2 := PLongWord(Bookmark2)^;
  if idx1 > idx2 then
    Result := +1
  else
  if idx1 = idx2 then
    Result := 0
  else
    Result := -1
end;

function TParadoxDataset.CreateBlobStream(Field: TField;
  Mode: TBlobStreamMode): TStream;
var
  memStream: TMemoryStream;
  p: PChar;
  header: PAnsiChar;
  idx: Byte;
  loc: Integer;
  s: String = '';
  blobInfo: TPxBlobInfo;
  blobIndex: TPxBlobIndex;
begin
  memStream := TMemoryStream.Create;
  Result := memStream;

  if (Mode <> bmRead) then
    exit;

  p := ActiveBuffer + FPxFields[Field.FieldNo - 1].Offset;
  header := p + Field.Size - SizeOf(TPxBlobInfo);
  Move(header^, blobInfo{%H-}, SizeOf(blobInfo));
  if blobInfo.Length = 0 then
    exit;

  if Integer(blobInfo.Length) > Field.Size - SizeOf(TPxBlobInfo) then
  begin
    if Assigned(FBlobStream) then begin
      idx := blobInfo.FileLoc and $FF;
      loc := blobInfo.FileLoc and $FFFFFF00;
      if idx = $FF then begin
        // Read from a single blob block
        FBlobStream.Seek(loc + 9, soFromBeginning);
        if Field.DataType = ftMemo then begin
          SetLength(s, blobInfo.Length);
          FBlobStream.Read(s[1], blobInfo.Length);
          s := ConvertEncoding(s, GetInputEncoding, GetTargetEncoding);
          memStream.Write(s[1], Length(s));
        end else
        begin
          if Field.DataType = ftGraphic then begin
            memstream.WriteAnsiString('bmp');     // Assuming that Paradox can store only bmp as ftGraphic... Wrong?
            FBlobStream.Position := FBlobStream.Position + 8;
          end;
          memStream.CopyFrom(FBlobStream, blobInfo.Length);
        end;
      end else begin
        // Read from a suballocated block
        FBlobStream.Seek(loc + 12 + 5*idx, soFromBeginning);
        FBlobStream.Read(blobIndex{%H-}, SizeOf(TPxBlobIndex));
        FBlobStream.Seek(loc + 16*blobIndex.Offset, soFromBeginning);
        if Field.DataType = ftMemo then begin
          SetLength(s, blobInfo.Length);
          FBlobStream.Read(s[1], blobInfo.Length);
          s := ConvertEncoding(s, GetInputEncoding, GetTargetEncoding);
          memStream.Write(s[1], Length(s));
        end else
          memStream.CopyFrom(FBlobStream, blobInfo.Length);
      end;
    end;
  end else
  if Field.DataType = ftMemo then begin
    SetLength(s, blobInfo.Length);
    Move(p^, s[1], blobInfo.Length);
    s := ConvertEncoding(s, GetInputEncoding, GetTargetEncoding);
    memStream.Write(s[1], Length(s));
  end else
    memStream.Write(p, blobInfo.Length);

  memStream.Position := 0;
end;

procedure TParadoxDataset.FreeRecordBuffer(var Buffer: PChar);
begin
  if Assigned(Buffer) then
    FreeMem(Buffer);
end;

procedure TParadoxDataset.GetBookmarkData(Buffer: PChar; Data: Pointer);
begin
  PLongWord(Data)^ := PRecInfo(Buffer + FHeader^.RecordSize)^.RecordNumber;
end;

function TParadoxDataset.GetBookmarkFlag(Buffer: PChar): TBookmarkFlag;
begin
  Result := PRecInfo(Buffer + FHeader^.RecordSize)^.BookmarkFlag;
end;

function TParadoxDataset.GetCanModify: Boolean;
begin
  Result := False;
end;

function TParadoxDataset.GetEncrypted: Boolean;
begin
  if not Assigned(FHeader) then exit;
  If (FHeader^.fileVersionID <= 4) or not (FHeader^.fileType in [0,2,3,5]) then
    Result := (FHeader^.encryption1 <> 0)
  else
    Result := (FHeader^.encryption2 <> 0)
end;

function TParadoxDataset.GetFieldData(Field: TField; Buffer: Pointer): Boolean;
var
  b: WordBool;
  F: PFldInfoRec;
  i: Integer;
  size: Integer;
  p: PChar;
  s: array[0..7] of byte;
  si:  SmallInt absolute s;
  int: LongInt  absolute s;
  d:   Double   absolute s;
  str: String;
  buf: TRecordBuffer = nil;
begin
  Result := False;
  if (RecordCount = 0) then
    exit;

  PXGetActiveBuffer(Buf);
  p := buf + FPxFields[Field.FieldNo - 1].Offset;
  F := FPxFields[Field.FieldNo - 1].Info;
  size := F^.fSize;

  // These numeric fields are stored as big endian --> swap bytes
  if F^.fType in [pxfDate..pxfNumber, pxfTime..pxfAutoInc] then begin
    for i := 0 to pred(size) do
      s[pred(size-i)] := byte(p[i]);
    s[pred(size)] := s[pred(size)] xor $80;
  end;

  case F^.fType of
  pxfAlpha:
    if (Buffer <> nil) then begin
      str := ConvertEncoding(StrPas(p), GetInputEncoding, GetTargetEncoding);
      if str <> '' then begin
        StrLCopy(Buffer, PChar(str), Length(str));
        Result := true;
      end;
    end;
  pxfBytes:
    if Buffer <> nil then begin
      StrLCopy(Buffer, PAnsiChar(p), F^.fSize);
      Result := true;
    end;
  pxfDate:
    if int <> $FFFFFFFF80000000 then begin     // This transforms to Dec/12/9999 and probably is NULL
      Move(int, Buffer^, SizeOf(LongInt));
      Result := True;
    end;
  pxfShort:
    begin
      Move(si, Buffer^, SizeOf(SmallInt));
      Result := True;
    end;
  pxfLong, pxfAutoInc:
    begin
      Move(int, Buffer^, SizeOf(LongInt));
      Result := True;
    end;
  pxfCurrency, pxfNumber, pxfTimeStamp:
    begin
      Move(d, Buffer^, SizeOf(d));
      Result := True;
    end;
  pxfLogical:
    begin
      b := not ((p^ = #$80) or (p^ = #0));
      if Assigned(Buffer) then
        Move(b, Buffer^, SizeOf(b));
      Result := true;  // Keep outside "if Assigned" otherwise checkboxes will be wrong.
    end;
  pxfTime:
    begin
      Move(int, Buffer^, SizeOf(LongInt));
      Result := True;
    end;
  pxfGraphic:
    Result := ActiveBuffer <> nil;
  end;
end;

function TParadoxDataset.GetInputEncoding: String;
begin
  if FInputEncoding = '' then
    Result := GetDefaultTextEncoding
  else
    Result := FInputEncoding;
end;

function TParadoxDataset.GetPrimaryKeyFieldCount: Integer;
begin
  if FHeader <> nil then
    Result := FHeader^.primaryKeyFields
  else
    Result := 0;
end;

function TParadoxDataset.GetRecNo: Integer;
begin
  Result := -1;
  if Assigned(ActiveBuffer) then
    Result := PRecInfo(ActiveBuffer + FHeader^.recordSize)^.RecordNumber;
end;

function TParadoxDataset.GetRecord(Buffer: PChar; GetMode: TGetMode; DoCheck: Boolean): TGetResult;
var
  L: Longword;
  accepted: Boolean;
begin
  Result := grOK;
  accepted := false;

  repeat
    case GetMode of
      gmNext:
        begin
          inc(FaRecord);
          if (FaBlockIdx = FHeader^.lastBlock) and
             (FaRecord > FaBlockStart + FaBlock^.addDataSize div FHeader^.recordSize + 1)
          then
            Result := grEOF
          else
          if FaRecord > FaBlockStart+1+(FaBlock^.addDataSize div FHeader^.recordSize) then
            ReadNextBlockHeader;
        end;
      gmPrior:
        begin
          dec(FaRecord);
          if (FaBlockIdx = FHeader^.firstBlock) and (FaRecord < 1) then
            Result := grBOF
          else
          if FaRecord <= FaBlockStart then
          begin
            ReadPrevBlockHeader;
            FaRecord := FaBlockStart+(FaBlock^.addDataSize div FHeader^.recordSize)+1;
          end;
        end;
      gmCurrent:
        if (FaRecord > RecordCount) or (FaRecord < 1) then
          result := grError;
    end;

    if Result = grOK then begin
      if not FBlockReaded then
        ReadBlock;
      L := ((faRecord - (FaBlockstart + 1))*FHeader^.recordSize) + 6;
      if (faRecord - (FaBlockstart + 1)) >= 0 then
        Move(PChar(FaBlock)[L],Buffer[0],FHeader^.recordSize)
      else
        Result := grError;
      with PRecInfo(Buffer + FHeader^.recordSize)^ do begin
        BookmarkFlag := bfCurrent;
        RecordNumber := FaRecord;
      end;

      // Filtering
      if Filtered then
        accepted := PXFilterRecord(Buffer)
      else
        accepted := True;
      if (GetMode = gmCurrent) and not accepted then
        Result := grError;
    end;
  until (Result <> grOK) or Accepted;
end;

function TParadoxDataset.GetRecordCount: Integer;
begin
  if Assigned(FHeader) then
    Result := FHeader^.numRecords
  else
    Result := 0;
end;

function TParadoxDataset.GetRecordSize: Word;
begin
  Result := FHeader^.recordSize + SizeOf(TRecInfo);
end;

function TParadoxDataset.GetTargetEncoding: String;
begin
  if (FTargetEncoding = '') or SameText(FTargetEncoding, 'utf-8') then
    Result := EncodingUTF8
  else
    Result := FTargetEncoding;
end;

function TParadoxDataset.GetVersion: String;
begin
  Result := '';
  if not FActive then
    exit;
  if not Assigned(FHeader) then
    exit;
  case FHeader^.fileVersionID of
    $3     : Result := '3.0';
    $4     : Result := '3.5';
    $5..$9 : Result := '4.0';
    $a..$b : Result := '5.0';
    $c     : Result := '7.0';
  end;
end;

procedure TParadoxDataset.InternalClose;
begin
  FInputEncoding := '';
  BindFields(FALSE);
  if DefaultFields then // Destroy the TField
    DestroyFields;
  FreeMem(FHeader);
  FreeMem(FaBlock);
  FreeAndNil(FParser);
  FreeAndNil(FBlobStream);
  FreeAndNil(FStream);
  FActive := False;
end;

procedure TParadoxDataset.InternalEdit;
begin
end;

procedure TParadoxDataset.InternalFirst;
begin
  FaBlockIdx := FHeader^.firstBlock;
  FaBlockstart := 0;
  FaRecord := 0;
  ReadBlock;
end;

{
procedure TParadoxDataset.InternalGotoBookmark(ABookmark: Pointer);
begin
  if BookmarkValid(ABookmark) then
    SetRecNo(PLongWord(ABookmark)^);
end;
}
procedure TParadoxDataset.InternalGotoBookmark(ABookmark: Pointer);
var
  bm: LongWord;
begin
  if (ABookmark <> nil) and (Length(TBookmark(ABookmark)) >= BookmarkSize) then
  begin
    bm := PLongWord(ABookmark)^;
    if bm <= GetRecordCount then
      SetRecNo(bm);
  end;
end;

{
procedure TParadoxDataset.InternalHandleException;
begin
  Application.HandleException(Self);
end;
}
procedure TParadoxDataset.InternalInitFieldDefs;
var
  i: integer;
  F: PFldInfoRec;
  FNamesStart: PChar;
  fname: String;
  offs: LongInt;
begin
  FieldDefs.Clear;
  F := FFieldInfoPtr;                  { begin with the first field identifier }
  FNamesStart := Pointer(F);
  inc(FNamesStart, SizeOf(F^)*(FHeader^.numFields));      //Jump over FieldDefs
  inc(FNamesStart, SizeOf(LongInt));                      //over TableName pointer
  inc(FNamesStart, SizeOf(LongInt)*(FHeader^.numFields)); //over FieldName pointers
  inc(FNamesStart, FTableNameLen);                        // over Tablename and padding

  SetLength(FPxFields, FHeader^.NumFields);
  offs := 0;

  for i := 1 to FHeader^.NumFields do
  begin
    fname := ConvertEncoding(StrPas(FNamesStart), GetInputEncoding, GetTargetEncoding);
    case F^.fType of
      pxfAlpha:       FieldDefs.Add(fname, ftString, F^.fSize);
      pxfDate:        FieldDefs.Add(fname, ftDate, 0);
      pxfShort:       FieldDefs.Add(fname, ftSmallInt, F^.fSize);
      pxfLong:        FieldDefs.Add(fname, ftInteger, F^.fSize);
      pxfCurrency:    FieldDefs.Add(fname, ftCurrency, F^.fSize);
      pxfNumber:      FieldDefs.Add(fname, ftFloat, F^.fSize);
      pxfLogical:     FieldDefs.Add(fname, ftBoolean, 0); //F^.fSize);
      pxfMemoBLOb:    FieldDefs.Add(fname, ftMemo, F^.fSize);
      pxfBLOb:        FieldDefs.Add(fname, ftBlob, F^.fSize);
      pxfFmtMemoBLOb: FieldDefs.Add(fname, ftMemo, F^.fSize);
      pxfOLE:         FieldDefs.Add(fname, ftBlob, F^.fSize);
      pxfGraphic:     FieldDefs.Add(fname, ftGraphic, F^.fSize);  // was: ftBlob
      pxfTime:        FieldDefs.Add(fname, ftTime, 0); //F^.fSize);
      pxfTimestamp:   FieldDefs.Add(fname, ftDateTime, 0);
      pxfAutoInc:     FieldDefs.Add(fname, ftAutoInc, F^.fSize);
      pxfBCD:         FieldDefs.Add(fname, ftBCD, F^.fSize);
      pxfBytes:       FieldDefs.Add(fname, ftBytes, F^.fSize);  // was: ftString
    end;
    with FPxFields[i-1] do begin
      Name := fname;
      Info := F;
      Offset := offs;
    end;
    offs := offs + F^.fSize;
    inc(FNamesStart, Length(fname)+1);
    inc(F);
  end;
end;

procedure TParadoxDataset.InternalInitRecord(Buffer: PChar);
begin
end;

procedure TParadoxDataset.InternalLast;
begin
  while FaBlockIdx <> FHeader^.lastBlock do
    ReadNextBlockHeader;
  inc(FaRecord,(FaBlock^.addDataSize div FHeader^.RecordSize)+1);
end;

procedure TParadoxDataset.InternalOpen;
var
  hdrSize: Word;
  blobfn: String;
  cp: Word;
begin
  if FFileName = '' then
    DatabaseError('Tablename is not set');
  if not FileExists(FFileName) then
    DatabaseError(Format('Paradox file "%" does not exist.', [FFileName]));

  FStream := TFileStream.Create(FFilename,fmOpenRead or fmShareDenyNone);
  FStream.Position := 2;
  hdrSize := FStream.ReadWord;
  FHeader := AllocMem(hdrSize);
  FStream.Position := 0;
  if not FStream.Read(FHeader^, hdrSize) = hdrSize then
    DatabaseError('No valid Paradox file !');
  if not ((FHeader^.maxTableSize >= 1) and (FHeader^.maxTableSize <= 32)) then
    DatabaseError('No valid Paradox file !');

  if (FHeader^.fileVersionID >= 12) then
    FTableNameLen := 261
  else
    FTableNameLen := 79;

  if (FHeader^.fileVersionID <= 4) or not (FHeader^.FileType in [0,2,3,5]) then
    FFieldInfoPtr := @FHeader^.FieldInfo35
   else begin
     FFieldInfoPtr := @FHeader^.FieldInfo;
     cp := FHeader^.DosCodePage;
     if FInputEncoding = '' then
       FInputEncoding := 'CP' + IntToStr(cp);
   end;

  if Encrypted then
    exit;

  FBlobStream := nil;
  blobfn := ChangeFileExt(FFileName, '.mb');
  if FileExists(blobfn) then
    FBlobStream := TFileStream.Create(blobfn, fmOpenRead + fmShareDenyNone)
  else begin
    blobfn := ChangeFileExt(FFileName, '.MB');
    if FileExists(blobfn) then
      FBlobStream := TFileStream.Create(blobfn, fmOpenRead + fmShareDenyNone);
  end;

  FaBlock := AllocMem(FHeader^.maxTableSize * $0400);
  BookmarkSize := SizeOf(longword);
  InternalFirst;
  InternalInitFieldDefs;
  if DefaultFields then CreateFields;
  BindFields(True);
  FActive := True;

  try
    ParseFilter(Filter);
  except
    on E: Exception do
      Filter := '';
  end;
end;

procedure TParadoxDataset.InternalPost;
begin
end;

procedure TParadoxDataset.InternalSetToRecord(Buffer: PChar);
var
  bm: LongWord;
begin
  if (State <> dsInsert) then begin
    bm := PRecInfo(Buffer + FHeader^.RecordSize)^.RecordNumber;
    InternalGotoBookmark(@bm);
  end;
end;

function TParadoxDataset.IsCursorOpen: Boolean;
begin
  Result := FActive;
end;

function TParadoxDataset.IsStoredTargetEncoding: Boolean;
begin
  Result := not SameText(FTargetEncoding, EncodingUTF8);
end;

procedure TParadoxDataset.ParseFilter(const AFilter: string);
begin
  if Length(AFilter) > 0 then
  begin
    if (FParser = nil) and IsCursorOpen then
      FParser := TBufDatasetParser.Create(Self);
    if FParser <> nil then
    begin
      FParser.PartialMatch := not (foNoPartialCompare in FilterOptions);
      FParser.CaseInsensitive := foCaseInsensitive in FilterOptions;
      FParser.ParseExpression(AFilter);
    end;
  end;
end;

function TParadoxDataset.PxFilterRecord(Buffer: TRecordBuffer): Boolean;
var
  SaveState: TDatasetState;
begin
  Result := True;
  if not Assigned(OnFilterRecord) and not Filtered then
    Exit;
  SaveState := SetTempState(dsFilter);
  Try
    FFilterBuffer := Buffer;
    If Assigned(OnFilterRecord) then
      OnFilterRecord(Self, Result);
    If Result and Filtered and (Filter <> '') then
      Result := Boolean((FParser.ExtractFromBuffer(FFilterBuffer))^);
  Finally
    RestoreState(SaveState);
  end;
end;

function TParadoxDataset.PxGetActiveBuffer(var Buffer: TRecordBuffer): Boolean;
begin
  case State of
    dsBrowse:
      if IsEmpty then
        Buffer := nil
      else
        Buffer := ActiveBuffer;
    dsEdit,
    dsInsert:
      Buffer := ActiveBuffer;
    dsFilter:
      Buffer := FFilterBuffer;
  else
    Buffer := nil;
  end;
  Result := (Buffer <> nil);
end;

procedure TParadoxDataset.ReadBlock;
var
  L   : longint;
begin
  L := FaBlockIdx-1;
  L := (L * FHeader^.maxTableSize * $0400) + FHeader^.headerSize;
  FStream.Position := L;
  FStream.Read(FaBlock^, FHeader^.maxTableSize * $0400);
  FBlockReaded := True;
end;

procedure TParadoxDataset.ReadNextBlockHeader;
var
  L   : longint;
begin
  if FaBlock^.nextBlock = 0 then exit; //last block
  //Increment Blockstart
  FaBlockStart := FaBlockStart+(FaBlock^.addDataSize div FHeader^.recordSize)+1;
  FaRecord := FaBlockStart+1;
  L := FaBlock^.nextBlock-1;
  L := (L * FHeader^.maxTableSize * $0400) + FHeader^.headerSize;
  FaBlockIdx := FaBlock^.nextBlock;
  FBlockReaded := False;
  FStream.Position := L;
  FStream.Read(FaBlock^,6); //read only Block header
end;

procedure TParadoxDataset.ReadPrevBlockHeader;
var
  L: LongWord;
begin
  L := FaBlock^.prevBlock-1;
  L := (L * FHeader^.maxTableSize * $0400) + FHeader^.headerSize;
  FaBlockIdx := FaBlock^.prevBlock;
  FBlockReaded := False;
  FStream.Position := L;
  FStream.Read(FaBlock^,6); //read only Block header
  //decrement Blockstart
  L := ((FaBlock^.addDataSize div FHeader^.recordSize)+1);
  FaBlockStart := FaBlockStart-L;
  FaRecord := FaBlockStart+1;
end;

procedure TParadoxDataset.SetBookmarkData(Buffer: PChar; Data: Pointer);
begin
  if Data <> nil then
    PRecInfo(Buffer + FHeader^.RecordSize)^.RecordNumber := PLongWord(Data)^
  else
    PRecInfo(Buffer + FHeader^.RecordSize)^.RecordNumber := 0;
end;

procedure TParadoxDataset.SetBookmarkFlag(Buffer: PChar; Value: TBookmarkFlag);
begin
  PRecInfo(Buffer + FHeader^.RecordSize)^.BookmarkFlag := Value;
end;

procedure TParadoxDataset.SetFieldData(Field: TField; Buffer: Pointer);
begin
end;

procedure TParadoxDataset.SetFileName(const AValue: TFileName);
begin
  if Active then
    Close;
  FFilename := AValue;
end;

procedure TParadoxDataset.SetFiltered(Value: Boolean);
begin
  if (Value <> Filtered) then begin
    inherited;
    if IsCursorOpen then
      Refresh;
  end;
end;

procedure TParadoxDataset.SetFilterText(const Value: String);
begin
  if (Value <> Filter) then begin
    ParseFilter(Value);
    inherited;
    if IsCursorOpen and Filtered then
      Refresh;
  end;
end;

procedure TParadoxDataset.SetRecNo(Value: Integer);
begin
  if Value < FaRecord then
  begin
    while (Value <= FaBlockstart) do
      ReadPrevBlockHeader;
    FaRecord := Value;
  end else
  begin
    while (Value > FaBlockstart+((FaBlock^.addDataSize div FHeader^.recordSize)+1)) do
      ReadNextBlockHeader;
    FaRecord := Value;
  end;
end;

procedure TParadoxDataset.SetTargetEncoding(AValue: String);
begin
  if AValue = FTargetEncoding then exit;
  FTargetEncoding := Uppercase(AValue);
end;


end.

