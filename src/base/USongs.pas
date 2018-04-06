{* UltraStar Deluxe - Karaoke Game
 *
 * UltraStar Deluxe is the legal property of its developers, whose names
 * are too numerous to list here. Please refer to the COPYRIGHT
 * file distributed with this source distribution.
 *
 * This program is free software; you can redistribute it and/or
 * modify it under the terms of the GNU General Public License
 * as published by the Free Software Foundation; either version 2
 * of the License, or (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; see the file COPYING. If not, write to
 * the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
 * Boston, MA 02110-1301, USA.
 *
 * $URL: svn://basisbit@svn.code.sf.net/p/ultrastardx/svn/trunk/src/base/USongs.pas $
 * $Id: USongs.pas 3103 2014-11-22 23:21:19Z k-m_schindler $
 *}

unit USongs;

interface

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

{$I switches.inc}

{$IFDEF DARWIN}
  {$IFDEF DEBUG}
    {$DEFINE USE_PSEUDO_THREAD}
  {$ENDIF}
{$ENDIF}

uses
  SysUtils,
  Classes,
  {$IFDEF MSWINDOWS}
    Windows,
    LazUTF8Classes,
  {$ELSE}
    {$IFNDEF DARWIN}
    syscall,
    {$ENDIF}
    baseunix,
    UnixType,
  {$ENDIF}
  UPlatform,
  ULog,
  UTexture,
  UCommon,
  {$IFDEF USE_PSEUDO_THREAD}
  PseudoThread,
  {$ENDIF}
  UPath,
  UPlaylist,
  USong,
  UIni,
  UCatCovers,
    sdl2,
  SQLite3,
  SQLiteTable3,
  UTextEncoding;

type
  TSongFilter = (
    fltAll,
    fltTitle,
    fltArtist,
    fltLanguage,
    fltEdition,
    fltGenre,
    fltYear,
    fltCreator
  );

  TBPM = record
    BPM:       real;
    StartBeat: real;
  end;

  TScore = record
    Name:   UTF8String;
    Score:  integer;
    Length: string;
  end;

  TPathDynArray = array of IPath;

  {$IFDEF USE_PSEUDO_THREAD}
  TSongs = class(TPseudoThread)
  {$ELSE}
  TSongs = class(TThread)
  {$ENDIF}
  private

    fNotify, fWatch:     longint;
    fParseSongDirectory: boolean;
    fProcessing:         boolean;
    procedure int_LoadSongList;
    procedure DoDirChanged(Sender: TObject);
  protected
    procedure Execute; override;
  public
    SongList: TList;            // array of songs

    Selected: integer;        // selected song index
    constructor Create();
    destructor  Destroy(); override;

    procedure LoadSongList;     // load all songs
    procedure FindFilesByExtension(const Dir: IPath; const Ext: IPath; Recursive: Boolean; var Files: TPathDynArray);
    procedure BrowseDir(Dir: IPath); // should return number of songs in the future
    procedure BrowseTXTFiles(Dir: IPath);
    procedure BrowseXMLFiles(Dir: IPath);
    procedure Sort(Order: TSortingType);
    property  Processing: boolean read fProcessing;
  end;

  TSongsDatabase = class
    private
      DB: TSQLiteDatabase;
      fdatabaseExist: boolean;
      procedure Open();
      procedure InitSongsDatabase();
    public
      constructor Create();
      destructor Destroy; override;
      procedure SaveAllSongs(SongList:TList);
      procedure RetrieveAllSongs(SongList:TList);
      property  DatabaseExist: boolean read fdatabaseExist;
  end;



  TCatSongs = class
    Song:       array of TSong; // array of categories with songs
    SongSort:   array of TSong;

    Selected:   integer; // selected song index
    Order:      integer; // order type (0=title)
    CatNumShow: integer; // Category Number being seen
    CatCount:   integer; // Number of Categorys

    procedure SortSongs();
    procedure Refresh;                                      // refreshes arrays by recreating them from Songs array
    procedure ShowCategory(Index: integer);                 // expands all songs in category
    procedure HideCategory(Index: integer);                 // hides all songs in category
    procedure ClickCategoryButton(Index: integer);          // uses ShowCategory and HideCategory when needed
    procedure ShowCategoryList;                             // Hides all Songs And Show the List of all Categorys
    function FindNextVisible(SearchFrom: integer): integer; // Find Next visible Song
    function FindPreviousVisible(SearchFrom: integer): integer; // Find Previous visible Song
    function VisibleSongs: integer;                         // returns number of visible songs (for tabs)
    function VisibleIndex(Index: integer): integer;         // returns visible song index (skips invisible)

    function SetFilter(FilterStr: UTF8String; Filter: TSongFilter): cardinal;
  end;

var
  Songs:    TSongs;    // all songs
  CatSongs: TCatSongs; // categorized songs
  SongsDatabase: TSongsDatabase;
const
  IN_ACCESS        = $00000001; //* File was accessed */
  IN_MODIFY        = $00000002; //* File was modified */
  IN_ATTRIB        = $00000004; //* Metadata changed */
  IN_CLOSE_WRITE   = $00000008; //* Writtable file was closed */
  IN_CLOSE_NOWRITE = $00000010; //* Unwrittable file closed */
  IN_OPEN          = $00000020; //* File was opened */
  IN_MOVED_FROM    = $00000040; //* File was moved from X */
  IN_MOVED_TO      = $00000080; //* File was moved to Y */
  IN_CREATE        = $00000100; //* Subfile was created */
  IN_DELETE        = $00000200; //* Subfile was deleted */
  IN_DELETE_SELF   = $00000400; //* Self was deleted */

  SONGSDB_FILENAME: UTF8String = 'songsDB.db';

implementation

uses
  StrUtils,
  UCovers,
  UFiles,
  UGraphic,
  UMain,
  UPathUtils,
  UNote,
  UFilesystem,
  UUnicodeUtils;

constructor TSongs.Create();
begin
  // do not start thread BEFORE initialization (suspended = true)
  inherited Create(true);
  Self.FreeOnTerminate := true;

  SongList           := TList.Create();
  SongsDatabase      := TSongsDatabase.Create();
  // until it is fixed, simply load the song-list
  int_LoadSongList();
end;

destructor TSongs.Destroy();
begin
  FreeAndNil(SongList);

  inherited;
end;

procedure TSongs.DoDirChanged(Sender: TObject);
begin
  LoadSongList();
end;

procedure TSongs.Execute();
var
  fChangeNotify: THandle;
begin
{$IFDEF USE_PSEUDO_THREAD}
  int_LoadSongList();
{$ELSE}
  fParseSongDirectory := true;

  while not terminated do
  begin

    if fParseSongDirectory then
    begin
      Log.LogStatus('Calling int_LoadSongList', 'TSongs.Execute');
      int_LoadSongList();
    end;

    Suspend();
  end;
{$ENDIF}
end;
constructor TSongsDatabase.Create();
begin
  inherited;
  Open();
  InitSongsDatabase();
end;

destructor TSongsDatabase.Destroy;
begin
  DB.Free;
  inherited;
end;


procedure TSongsDatabase.Open();
var
  Filename: IPath;
begin
  Filename := Platform.GetGameUserPath().Append(SONGSDB_FILENAME);
  fdatabaseExist := Filename.Exists;
  DB := TSQLiteDatabase.Create(Filename.ToUTF8());
end;

procedure TSongsDatabase.InitSongsDatabase();
begin
  DB.ExecSQL('CREATE TABLE IF NOT EXISTS [Songs] (' +
'[ID] INTEGER  NOT NULL PRIMARY KEY AUTOINCREMENT, ' +
'[Path] TEXT NULL, ' +
'[Folder] TEXT NULL, ' +
'[FileName] TEXT NULL, ' +
'[MD5] TEXT NULL, ' +
'[Cover] TEXT NULL, ' +
'[Mp3] TEXT NULL, ' +
'[Background] TEXT NULL, ' +
'[Video] TEXT NULL, ' +
'[Genre] TEXT NULL, ' +
'[Edition] TEXT NULL, ' +
'[Language] TEXT NULL, ' +
'[Year] INTEGER NULL, ' +
'[Title] TEXT NULL, ' +
'[Artist] TEXT NULL, ' +
'[TitleNoAccent] TEXT NULL, ' +
'[ArtistNoAccent] TEXT NULL, ' +
'[LanguageNoAccent] TEXT NULL, ' +
'[EditionNoAccent] TEXT NULL, ' +
'[GenreNoAccent] TEXT NULL, ' +
'[CreatorNoAccent] TEXT NULL, ' +
'[Creator] TEXT NULL, ' +
'[CoverTex] TEXT NULL, ' +
'[VideoGAP] REAL NULL, ' +
'[NotesGAP] INTEGER NULL, ' +
'[Start] REAL NULL, ' +
'[Finish] INTEGER NULL, ' +
'[Relative] TEXT NULL, ' +
'[Resolution] INTEGER NULL, ' +
// '[BPM] TEXT NULL, ' +
'[GAP] REAL NULL, ' +
'[Encoding] TEXT NULL, ' +
'[PreviewStart] REAL NULL, ' +
'[HasPreview] TEXT NULL, ' +
'[CalcMedley] TEXT NULL, ' +
'[MedleySource] TEXT NULL, ' +
'[MedleyStartBeat] INTEGER NULL, ' +
'[MedleyEndBeat] INTEGER NULL, ' +
'[MedleyFadeIn_time] REAL NULL, ' +
'[MedleyFadeOut_time] REAL NULL, ' +
'[isDuet] TEXT NULL, ' +
// '[DuetNames] TEXT NULL, ' +
'[hasRap] TEXT NULL, ' +
//'[CustomTags] TEXT NULL, ' +
'[Score] TEXT NULL, ' +
'[Visible] TEXT NULL, ' +
'[Main] TEXT NULL, ' +
'[OrderNum] INTEGER NULL, ' +
'[OrderTyp] INTEGER NULL, ' +
'[CatNumber] INTEGER NULL, ' +
'[Base0] INTEGER NULL, ' +
'[Base1] INTEGER NULL, ' +
'[Rel0] INTEGER NULL, ' +
'[Rel1] INTEGER NULL, ' +
'[Mult] INTEGER NULL, ' +
'[MultBPM] INTEGER NULL' +
             ')');

DB.ExecSQL('CREATE TABLE IF NOT EXISTS [Songs_BPM] (' +
'[ID] INTEGER  NOT NULL PRIMARY KEY AUTOINCREMENT, ' +
'[SongID] INTEGER  NOT NULL, ' +
'[BPM] REAL NULL, ' +
'[StartBeat] REAL NULL' +
          ')');

 DB.ExecSQL('CREATE TABLE IF NOT EXISTS [Songs_DuetNames] (' +
'[ID] INTEGER  NOT NULL PRIMARY KEY AUTOINCREMENT, ' +
'[SongID] INTEGER  NOT NULL, ' +
'[DuetNames] TEXT NULL ' +
          ')');

DB.ExecSQL('CREATE TABLE IF NOT EXISTS [Songs_CustomTags] (' +
'[ID] INTEGER  NOT NULL PRIMARY KEY AUTOINCREMENT, ' +
'[SongID] INTEGER  NOT NULL, ' +
'[Tag] TEXT NULL, ' +
'[Content] TEXT NULL' +
          ')');
end;

procedure TSongsDatabase.SaveAllSongs(SongList:TList);
var
    CurSong:     TSong;
    ID:        integer;
    SongIndex: integer;
    CurBPM : USong.TBPM;
    CurDuetName: UTF8String;
    CurCustomTag: USong.TCustomHeaderTag   ;
begin
try
    // Note: use a transaction to speed-up file-writing.
    // Without data written by the first INSERT might be moved at the second INSERT.
DB.BeginTransaction();
for SongIndex := 0 to SongList.Count - 1 do
 begin
 CurSong := TSong(SongList[SongIndex]);
 DB.ExecSQL(
         'INSERT INTO [Songs] ' +
         '(Path, Folder, FileName, MD5, Cover, Mp3, Background, Video, Genre, Edition, Language, Year, Title, Artist, TitleNoAccent, ArtistNoAccent, LanguageNoAccent, EditionNoAccent, GenreNoAccent, CreatorNoAccent, Creator, CoverTex, VideoGAP, NotesGAP, Start, Finish, Relative, Resolution, GAP, Encoding, PreviewStart, HasPreview, CalcMedley, MedleySource, MedleyStartBeat, MedleyEndBeat, MedleyFadeIn_time, MedleyFadeOut_time, isDuet, hasRap, Score, Visible, Main, OrderNum, OrderTyp, CatNumber, Base0, Base1, Rel0, Rel1, Mult, MultBPM) VALUES ' +
         '(?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?);',
         [CurSong.Path.ToNative,
         CurSong.Folder,
         CurSong.FileName.ToNative,
         CurSong.MD5,
         CurSong.Cover.ToNative,
         CurSong.Mp3.ToNative,
         CurSong.Background.ToNative,
         CurSong.Video.ToNative,
         CurSong.Genre,
         CurSong.Edition,
         CurSong.Language,
         CurSong.Year,
         CurSong.Title,
         CurSong.Artist,
         CurSong.TitleNoAccent,
         CurSong.ArtistNoAccent,
         CurSong.LanguageNoAccent,
         CurSong.EditionNoAccent,
         CurSong.GenreNoAccent,
         CurSong.CreatorNoAccent,
         CurSong.Creator,
         '', // CoverTex Not implemented
         CurSong.VideoGAP,
         CurSong.NotesGAP,
         CurSong.Start,
         CurSong.Finish,
         CurSong.Relative,
         CurSong.Resolution,
         // '', // Other table BPM
         CurSong.GAP,
         CurSong.Encoding,
         CurSong.PreviewStart,
         CurSong.HasPreview,
         CurSong.CalcMedley,
         CurSong.Medley.Source,
         CurSong.Medley.StartBeat,
         CurSong.Medley.EndBeat,
         CurSong.Medley.FadeIn_time,
         CurSong.Medley.FadeOut_time,
         CurSong.isDuet,
         // '', // Other table DuetNames
         CurSong.hasRap,
         // '', // Other Table CustomTags Not implemented
         '', // Score Not implemented
         CurSong.Visible,
         CurSong.Main,
         CurSong.OrderNum,
         CurSong.OrderTyp,
         CurSong.CatNumber,
         CurSong.Base[0], // Base
         CurSong.Base[1],
         CurSong.Rel[0], // Rel
         CurSong.Rel[1],
         CurSong.Mult,
         CurSong.MultBPM]);

    ID := DB.GetLastInsertRowID();

    for CurBPM in CurSong.BPM do
     begin
         DB.ExecSQL(
         'INSERT INTO [Songs_BPM] ' +
         '([SongID],[BPM], [StartBeat]) VALUES ' +
         '(?,?,?);',
         [ID,CurBPM.BPM,CurBPM.StartBeat]);
     end;

    for CurDuetName in CurSong.DuetNames do
     begin
         DB.ExecSQL(
         'INSERT INTO [Songs_DuetNames] ' +
         '([SongID],[DuetNames]) VALUES ' +
         '(?,?);',
         [ID,CurDuetName]);
     end;

    for CurCustomTag in CurSong.CustomTags do
     begin
         DB.ExecSQL(
         'INSERT INTO [Songs_CustomTags] ' +
         '([SongID],[Tag],[Content]) VALUES ' +
         '(?,?);',
         [ID,CurCustomTag.Tag,CurCustomTag.Content]);
     end;
  end;
  except on E: Exception do
    Log.LogError(E.Message, 'TSongsDatabase.AddSongs');
  end;

  DB.Commit();

end;

procedure TSongsDatabase.RetrieveAllSongs(SongList:TList);
 var
       BPMIndex: integer;
       DuetNamesIndex: integer;
       CurSong: TSong;
       TableData_Songs: TSQLiteUniTable;
       TableData_Songs_BPM: TSQLiteUniTable;
       TableData_Songs_DuetNames: TSQLiteUniTable;
       TableData_Songs_CustomTags: TSQLiteUniTable;
       Medley: TMedley;
 begin
   if not Assigned(DB) then
    Exit;

   TableData_Songs := nil;
   TableData_Songs_BPM := nil;
   TableData_Songs_DuetNames := nil;
   TableData_Songs_CustomTags := nil;

   try

   TableData_Songs := DB.GetUniTable('SELECT * FROM [Songs]');

   while (not TableData_Songs.EOF) do
    begin
      CurSong := TSong.Create();
      CurSong.Path  := UPath.Path(TableData_Songs.Fields[1]);
      CurSong.Folder := TableData_Songs.Fields[2] ;
      CurSong.FileName := UPath.Path(TableData_Songs.Fields[3]);
      CurSong.MD5 := TableData_Songs.Fields[4] ;
      CurSong.Cover := UPath.Path(TableData_Songs.Fields[5]);
      CurSong.Mp3 := UPath.Path(TableData_Songs.Fields[6]);
      CurSong.Background := UPath.Path(TableData_Songs.Fields[7]);
      CurSong.Video := UPath.Path(TableData_Songs.Fields[8]);
      CurSong.Genre := TableData_Songs.Fields[9] ;
      CurSong.Edition := TableData_Songs.Fields[10];
      CurSong.Language := TableData_Songs.Fields[11];
      CurSong.Year := TableData_Songs.FieldAsInteger(12);
      CurSong.Title := TableData_Songs.Fields[13];
      CurSong.Artist := TableData_Songs.Fields[14];
      CurSong.TitleNoAccent := TableData_Songs.Fields[15];
      CurSong.ArtistNoAccent := TableData_Songs.Fields[16];
      CurSong.LanguageNoAccent := TableData_Songs.Fields[17];
      CurSong.EditionNoAccent := TableData_Songs.Fields[18];
      CurSong.GenreNoAccent := TableData_Songs.Fields[19];
      CurSong.CreatorNoAccent := TableData_Songs.Fields[20];
      CurSong.Creator := TableData_Songs.Fields[21];
      // CoverTex Not implemente
      CurSong.VideoGAP := TableData_Songs.FieldAsDouble(23);
      CurSong.NotesGAP := TableData_Songs.FieldAsInteger(24) ;
      CurSong.Start := TableData_Songs.FieldAsDouble(25);
      CurSong.Finish := TableData_Songs.FieldAsInteger(26) ;
      CurSong.Relative := StrToBool(TableData_Songs.Fields[27])  ;
      CurSong.Resolution := TableData_Songs.FieldAsInteger(28);
      CurSong.GAP := TableData_Songs.FieldAsDouble(29) ;
      CurSong.Encoding := UTextEncoding.TEncoding(TableData_Songs.FieldAsInteger(30));
      CurSong.PreviewStart := TableData_Songs.FieldAsDouble(31);
      CurSong.HasPreview := StrToBool(TableData_Songs.Fields[32]) ;
      CurSong.CalcMedley := StrToBool(TableData_Songs.Fields[33]) ;
      CurSong.Medley.Source := TMedleySource(TableData_Songs.FieldAsInteger(34));
      CurSong.Medley.StartBeat := TableData_Songs.FieldAsInteger(35);
      CurSong.Medley.EndBeat := TableData_Songs.FieldAsInteger(36);
      CurSong.Medley.FadeIn_time := TableData_Songs.FieldAsDouble(37);
      CurSong.Medley.FadeOut_time := TableData_Songs.FieldAsDouble(38);
      CurSong.isDuet := StrToBool(TableData_Songs.Fields[39]) ;
      CurSong.hasRap := StrToBool(TableData_Songs.Fields[40]) ;
      // Score Not implemented, use different DB
      CurSong.Visible := StrToBool(TableData_Songs.Fields[42]);
      CurSong.Main := StrToBool(TableData_Songs.Fields[43]);
      CurSong.OrderNum := TableData_Songs.FieldAsInteger(44) ;
      CurSong.OrderTyp := TableData_Songs.FieldAsInteger(45);
      CurSong.CatNumber := TableData_Songs.FieldAsInteger(46) ;
      CurSong.Base[0] := TableData_Songs.FieldAsInteger(47);
      CurSong.Base[1] := TableData_Songs.FieldAsInteger(48);
      CurSong.Rel[0] := TableData_Songs.FieldAsInteger(49) ;
      CurSong.Rel[1] := TableData_Songs.FieldAsInteger(50) ;
      CurSong.Mult := TableData_Songs.FieldAsInteger(51) ;
      CurSong.MultBPM := TableData_Songs.FieldAsInteger(52) ;

     TableData_Songs_BPM := DB.GetUniTable(
      'SELECT * FROM [Songs_BPM] ' +
      'WHERE [SongID] = ? ',
      [TableData_Songs.FieldAsInteger(0)]);

     TableData_Songs_DuetNames := DB.GetUniTable(
      'SELECT * FROM [Songs_DuetNames] ' +
      'WHERE [SongID] = ? ',
      [TableData_Songs.FieldAsInteger(0)]);

     TableData_Songs_CustomTags := DB.GetUniTable(
      'SELECT * FROM [Songs_CustomTags] ' +
      'WHERE [SongID] = ? ',
      [TableData_Songs.FieldAsInteger(0)]);

     while (not TableData_Songs_BPM.EOF) do
      begin
           SetLength(CurSong.BPM,TableData_Songs_BPM.Row);
           CurSong.BPM[TableData_Songs_BPM.Row-1].BPM :=TableData_Songs.FieldAsDouble(2);
           CurSong.BPM[TableData_Songs_BPM.Row-1].StartBeat := TableData_Songs.FieldAsDouble(3);
           TableData_Songs_BPM.Next;
      end;
     while (not TableData_Songs_DuetNames.EOF) do
      begin
           SetLength(CurSong.DuetNames,TableData_Songs_DuetNames.Row);
           CurSong.DuetNames[TableData_Songs_DuetNames.Row-1] := TableData_Songs_DuetNames.Fields[2];
           TableData_Songs_DuetNames.Next;
      end;
     while (not TableData_Songs_CustomTags.EOF) do
      begin
           SetLength(CurSong.CustomTags,TableData_Songs_CustomTags.Row);
           CurSong.CustomTags[TableData_Songs_CustomTags.Row-1].Tag := TableData_Songs_CustomTags.Fields[2];
           CurSong.CustomTags[TableData_Songs_CustomTags.Row-1].Content := TableData_Songs_CustomTags.Fields[3];
           TableData_Songs_CustomTags.Next;
      end;

     SongList.Add(CurSong);
     TableData_Songs.Next;
    end; // while
    except on E: Exception do
           Log.LogError(E.Message, 'TSongsDatabase.ReadSongs');
    end;
    TableData_Songs.Free;
    TableData_Songs_BPM.Free;
    TableData_Songs_DuetNames.Free;
    TableData_Songs_CustomTags.Free;
end;

procedure TSongs.int_LoadSongList;
var
  I: integer;
  Filename: IPath;
  SongIndex:   integer;

begin
  try
    fProcessing := true;
    Log.LogStatus('Searching For Songs', 'SongList');
    // browse directories
    if(SongsDatabase.DatabaseExist) then
        SongsDatabase.RetrieveAllSongs(SongList)
    else
    begin
     for I := 0 to SongPaths.Count-1 do
        BrowseDir(SongPaths[I] as IPath);
     SongsDatabase.SaveAllSongs(SongList);
    end;
    Log.LogStatus('Search Songs Complete', 'SongList');
    if assigned(CatSongs) then
    begin
      CatSongs.Refresh;
      Log.LogStatus('Search CatSongs Complete', 'SongList');
    end;
    if assigned(CatCovers) then
    begin
      CatCovers.Load;
      Log.LogStatus('Search CatCovers Complete', 'SongList');
    end;

    if assigned(Covers) then
      //Covers.Load;

    if assigned(ScreenSong)  then
    begin
      ScreenSong.GenerateThumbnails();
      ScreenSong.OnShow; // refresh ScreenSong
      Log.LogStatus('Search ScreenSong Complete', 'SongList');
    end;

  finally
    Log.LogStatus('Search Complete', 'SongList');

    fParseSongDirectory := false;
    fProcessing         := false;
  end;
end;


procedure TSongs.LoadSongList;
begin
  fParseSongDirectory := true;
  Resume();
end;

procedure TSongs.BrowseDir(Dir: IPath);
begin
  BrowseTXTFiles(Dir);
  BrowseXMLFiles(Dir);
end;

procedure TSongs.FindFilesByExtension(const Dir: IPath; const Ext: IPath; Recursive: Boolean; var Files: TPathDynArray);
var
  Iter: IFileIterator;
  FileInfo: TFileInfo;
  FileName: IPath;
begin
  // search for all files and directories
  Iter := FileSystem.FileFind(Dir.Append('*'), faAnyFile);
  while (Iter.HasNext) do
  begin
    FileInfo := Iter.Next;
    FileName := FileInfo.Name;
    if ((FileInfo.Attr and faDirectory) <> 0) then
    begin
      if Recursive and (not FileName.Equals('.')) and (not FileName.Equals('..')) and (not FileName.Equals('')) then
        FindFilesByExtension(Dir.Append(FileName), Ext, true, Files);
    end
    else
    begin
      // do not load files which either have wrong extension or start with a point
      if (Ext.Equals(FileName.GetExtension(), true) and not (FileName.ToUTF8()[1] = '.')) then
      begin
        SetLength(Files, Length(Files)+1);
        Files[High(Files)] := Dir.Append(FileName);
      end;
    end;
  end;
end;

procedure TSongs.BrowseTXTFiles(Dir: IPath);
var
  I, C: integer;
  Files: TPathDynArray;
  Song: TSong;
  //CloneSong: TSong;
  Extension: IPath;
begin
  SetLength(Files, 0);
  Extension := Path('.txt');
  FindFilesByExtension(Dir, Extension, true, Files);

  for I := 0 to High(Files) do
  begin
    Song := TSong.Create(Files[I]);

    if Song.Analyse then
      SongList.Add(Song)
    else
    begin
      Log.LogError('AnalyseFile failed for "' + Files[I].ToNative + '".');
      FreeAndNil(Song);
    end;
  end;

  SetLength(Files, 0);
end;

procedure TSongs.BrowseXMLFiles(Dir: IPath);
var
  I: integer;
  Files: TPathDynArray;
  Song: TSong;
  Extension: IPath;
begin
  SetLength(Files, 0);
  Extension := Path('.xml');
  FindFilesByExtension(Dir, Extension, true, Files);

  for I := 0 to High(Files) do
  begin
    Song := TSong.Create(Files[I]);

    if Song.AnalyseXML then
      SongList.Add(Song)
    else
    begin
      Log.LogError('AnalyseFile failed for "' + Files[I].ToNative + '".');
      FreeAndNil(Song);
    end;
  end;

  SetLength(Files, 0);
end;

(*
 * Comparison functions for sorting
 *)

function CompareByEdition(Song1, Song2: Pointer): integer;
begin
  Result := UTF8CompareText(TSong(Song1).Edition, TSong(Song2).Edition);
end;

function CompareByGenre(Song1, Song2: Pointer): integer;
begin
  Result := UTF8CompareText(TSong(Song1).Genre, TSong(Song2).Genre);
end;

function CompareByTitle(Song1, Song2: Pointer): integer;
begin
  Result := UTF8CompareText(TSong(Song1).Title, TSong(Song2).Title);
end;

function CompareByArtist(Song1, Song2: Pointer): integer;
begin
  Result := UTF8CompareText(TSong(Song1).Artist, TSong(Song2).Artist);
end;

function CompareByFolder(Song1, Song2: Pointer): integer;
begin
  Result := UTF8CompareText(TSong(Song1).Folder, TSong(Song2).Folder);
end;

function CompareByLanguage(Song1, Song2: Pointer): integer;
begin
  Result := UTF8CompareText(TSong(Song1).Language, TSong(Song2).Language);
end;

function CompareByYear(Song1, Song2: Pointer): integer;
begin
  if (TSong(Song1).Year > TSong(Song2).Year) then
    Result := 1
  else
    Result := 0;
end;

procedure TSongs.Sort(Order: TSortingType);
var
  CompareFunc: TListSortCompare;
begin
  // FIXME: what is the difference between artist and artist2, etc.?
  case Order of
    sEdition: // by edition
      CompareFunc := CompareByEdition;
    sGenre: // by genre
      CompareFunc := CompareByGenre;
    sTitle: // by title
      CompareFunc := CompareByTitle;
    sArtist: // by artist
      CompareFunc := CompareByArtist;
    sFolder: // by folder
      CompareFunc := CompareByFolder;
    sArtist2: // by artist2
      CompareFunc := CompareByArtist;
    sLanguage: // by Language
      CompareFunc := CompareByLanguage;
    sYear: // by Year
      CompareFunc := CompareByYear;
    sDecade: // by Decade
      CompareFunc := CompareByYear;
    else
      Log.LogCritical('Unsupported comparison', 'TSongs.Sort');
      Exit; // suppress warning
  end; // case

  // Note: Do not use TList.Sort() as it uses QuickSort which is instable.
  // For example, if a list is sorted by title first and
  // by artist afterwards, the songs of an artist will not be sorted by title anymore.
  // The stable MergeSort guarantees to maintain this order. 
  MergeSort(SongList, CompareFunc);
end;

procedure TCatSongs.SortSongs();
begin
  case TSortingType(Ini.Sorting) of
    sEdition: begin
        Songs.Sort(sTitle);
        Songs.Sort(sArtist);
        Songs.Sort(sEdition);
      end;
    sGenre: begin
        Songs.Sort(sTitle);
        Songs.Sort(sArtist);
        Songs.Sort(sGenre);
      end;
    sLanguage: begin
        Songs.Sort(sTitle);
        Songs.Sort(sArtist);
        Songs.Sort(sLanguage);
      end;
    sFolder: begin
        Songs.Sort(sTitle);
        Songs.Sort(sArtist);
        Songs.Sort(sFolder);
      end;
    sTitle: begin
        Songs.Sort(sTitle);
      end;
    sArtist: begin
        Songs.Sort(sTitle);
        Songs.Sort(sArtist);
      end;
    sArtist2: begin
        Songs.Sort(sTitle);
        Songs.Sort(sArtist2);
      end;
    sYear: begin
        Songs.Sort(sTitle);
        Songs.Sort(sArtist);
        Songs.Sort(sYear);
      end;
    sDecade: begin
        Songs.Sort(sTitle);
        Songs.Sort(sArtist);
        Songs.Sort(sYear);
      end;
    sPlaylist: begin
        Songs.Sort(sTitle);
        Songs.Sort(sArtist);
      end;
  end; // case
end;

procedure TCatSongs.Refresh;
var
  SongIndex:   integer;
  CurSong:     TSong;
  CatIndex:    integer;    // index of current song in Song
  Letter:      UCS4Char;   // current letter for sorting using letter
  CurCategory: UTF8String; // current edition for sorting using edition, genre etc.
  Order:       integer;    // number used for ordernum
  LetterTmp:   UCS4Char;
  CatNumber:   integer;    // Number of Song in Category
  tmpCategory: UTF8String; //
  I, J:        integer;
  StringIndex: integer;
  MainArtist:  UTF8String;

  procedure AddCategoryButton(const CategoryName: UTF8String);
  var
    PrevCatBtnIndex: integer;
  begin
    Inc(Order);
    CatIndex := Length(Song);
    SetLength(Song, CatIndex+1);
    Song[CatIndex]          := TSong.Create();
    Song[CatIndex].Artist   := '[' + CategoryName + ']';
    Song[CatIndex].Main     := true;
    Song[CatIndex].OrderTyp := 0;
    Song[CatIndex].OrderNum := Order;
    Song[CatIndex].Cover    := CatCovers.GetCover(TSortingType(Ini.Sorting), CategoryName);
    Song[CatIndex].Visible  := true;

    // set number of songs in previous category
    PrevCatBtnIndex := CatIndex - CatNumber - 1;
    if ((PrevCatBtnIndex >= 0) and Song[PrevCatBtnIndex].Main) then
      Song[PrevCatBtnIndex].CatNumber := CatNumber;

    CatNumber := 0;
  end;

begin
  CatNumShow  := -1;

  SortSongs();

  CurCategory := '';
  Order       := 0;
  CatNumber   := 0;

  // Note: do NOT set Letter to ' ', otherwise no category-button will be
  // created for songs beginning with ' ' if songs of this category exist.
  // TODO: trim song-properties so ' ' will not occur as first chararcter.
  Letter      := 0;

  // clear song-list
  for SongIndex := 0 to Songs.SongList.Count - 1 do
  begin
    // free category buttons
    // Note: do NOT delete songs, they are just references to Songs.SongList entries
    CurSong := TSong(Songs.SongList[SongIndex]);
    if (CurSong.Main) then
      CurSong.Free;
  end;
  SetLength(Song, 0);

  for SongIndex := 0 to Songs.SongList.Count - 1 do
  begin
    CurSong := TSong(Songs.SongList[SongIndex]);
    // if tabs are on, add section buttons for each new section
    if (Ini.Tabs = 1) then
    begin
      case (TSortingType(Ini.Sorting)) of
        sEdition: begin
          if (CompareText(CurCategory, CurSong.Edition) <> 0) then
          begin
            CurCategory := CurSong.Edition;

            // add Category Button
            AddCategoryButton(CurCategory);
          end;
        end;

        sGenre: begin
          if (CompareText(CurCategory, CurSong.Genre) <> 0) then
          begin
            CurCategory := CurSong.Genre;
            // add Genre Button
            AddCategoryButton(CurCategory);
          end;
        end;

        sLanguage: begin
          if (CompareText(CurCategory, CurSong.Language) <> 0) then
          begin
            CurCategory := CurSong.Language;
            // add Language Button
            AddCategoryButton(CurCategory);
          end
        end;

        sTitle: begin
          if (Length(CurSong.Title) >= 1) then
          begin
            LetterTmp := UCS4UpperCase(UTF8ToUCS4String(CurSong.Title)[0]);
            { all numbers and some punctuation chars are put into a
              category named '#'
              we can't put the other punctuation chars into this category
              because they are not in order, so there will be two different
              categories named '#' }
            if (LetterTmp in [Ord('!') .. Ord('?')]) then
              LetterTmp := Ord('#')
            else
              LetterTmp := UCS4UpperCase(LetterTmp);
            if (Letter <> LetterTmp) then
            begin
              Letter := LetterTmp;
              // add a letter Category Button
              AddCategoryButton(UCS4ToUTF8String(Letter));
            end;
          end;
        end;

        sArtist: begin
          if (Length(CurSong.Artist) >= 1) then
          begin
            LetterTmp := UCS4UpperCase(UTF8ToUCS4String(CurSong.Artist)[0]);
            { all numbers and some punctuation chars are put into a
              category named '#'
              we can't put the other punctuation chars into this category
              because they are not in order, so there will be two different
              categories named '#' }
            if (LetterTmp in [Ord('!') .. Ord('?')]) then
              LetterTmp := Ord('#')
            else
              LetterTmp := UCS4UpperCase(LetterTmp);

            if (Letter <> LetterTmp) then
            begin
              Letter := LetterTmp;
              // add a letter Category Button
              AddCategoryButton(UCS4ToUTF8String(Letter));
            end;
          end;
        end;

        sFolder: begin
          if (UTF8CompareText(CurCategory, CurSong.Folder) <> 0) then
          begin
            CurCategory := CurSong.Folder;
            // add folder tab
            AddCategoryButton(CurCategory);
          end;
        end;

        sArtist2: begin
          { this new sorting puts all songs by the same artist into
            a single category }
          //
          if (UTF8ContainsText(CurSong.Artist, ' feat.')) then
          begin
            StringIndex := UTF8Pos(' feat', CurSong.Artist);
            MainArtist := TrimRight(UTF8Copy(CurSong.Artist, 1, StringIndex-1));
          end
          else
            MainArtist := CurSong.Artist;
          //
          if (UTF8CompareText(CurCategory, MainArtist) <> 0) then
          begin
            CurCategory := MainArtist;
            // add folder tab
            AddCategoryButton(CurCategory);
          end;
        end;

        sYear: begin
           if (CurSong.Year <> 0) then
             tmpCategory := IntToStr(CurSong.Year)
           else
             tmpCategory := 'Unknown';

           if (tmpCategory <> CurCategory) then
           begin
             CurCategory := tmpCategory;

             // add Category Button
             AddCategoryButton(CurCategory);
           end;
         end;

        sDecade: begin
           if (CurSong.Year <> 0) then
             tmpCategory := IntToStr(Trunc(CurSong.Year/10)*10) + '-' + IntToStr(Trunc(CurSong.Year/10)*10+9)
           else
             tmpCategory := 'Unknown';

           if (tmpCategory <> CurCategory) then
           begin
             CurCategory := tmpCategory;

             // add Category Button
             AddCategoryButton(CurCategory);
           end;
        end;
      end; // case (Ini.Sorting)
    end; // if (Ini.Tabs = 1)

    CatIndex := Length(Song);
    SetLength(Song, CatIndex+1);

    Inc(CatNumber); // increase number of songs in category

    // copy reference to current song
    Song[CatIndex] := CurSong;

    // set song's category info
    CurSong.OrderNum := Order; // assigns category
    CurSong.CatNumber := CatNumber;

    if (Ini.Tabs = 0) then
    begin
      CurSong.Visible := true;
    end
    else if (Ini.Tabs = 1) then
    begin
      CurSong.Visible := false;
    end;
{
    if (Ini.Tabs = 1) and (Order = 1) then
    begin
      //open first tab
      CurSong.Visible := true;
    end;
    CurSong.Visible := true;
}
  end;

  // set CatNumber of last category
  if (Ini.TabsAtStartup = 1) and (High(Song) >= 1) then
  begin
    // set number of songs in previous category
    SongIndex := CatIndex - CatNumber;
    if ((SongIndex >= 0) and Song[SongIndex].Main) then
      Song[SongIndex].CatNumber := CatNumber;
  end;

  // update number of categories
  CatCount := Order;
end;

procedure TCatSongs.ShowCategory(Index: integer);
var
  S: integer; // song
begin
  CatNumShow := Index;
  for S := 0 to high(CatSongs.Song) do
  begin
{
    if (CatSongs.Song[S].OrderNum = Index) and (not CatSongs.Song[S].Main) then
      CatSongs.Song[S].Visible := true
    else
      CatSongs.Song[S].Visible := false;
}
//  KMS: This should be the same, but who knows :-)
    CatSongs.Song[S].Visible := ((CatSongs.Song[S].OrderNum = Index) and (not CatSongs.Song[S].Main));
  end;
end;

procedure TCatSongs.HideCategory(Index: integer); // hides all songs in category
var
  S: integer; // song
begin
  for S := 0 to high(CatSongs.Song) do
  begin
    if not CatSongs.Song[S].Main then
      CatSongs.Song[S].Visible := false // hides all at now
  end;
end;

procedure TCatSongs.ClickCategoryButton(Index: integer);
var
  Num: integer;
begin
  Num := CatSongs.Song[Index].OrderNum;
  if Num <> CatNumShow then
  begin
    ShowCategory(Num);
  end
  else
  begin
    ShowCategoryList;
  end;
end;

//Hide Categorys when in Category Hack
procedure TCatSongs.ShowCategoryList;
var
  S: integer;
begin
  // Hide All Songs Show All Cats
  for S := 0 to high(CatSongs.Song) do
    CatSongs.Song[S].Visible := CatSongs.Song[S].Main;
  CatSongs.Selected := CatNumShow; //Show last shown Category
  CatNumShow := -1;
end;
//Hide Categorys when in Category Hack End

// Wrong song selected when tabs on bug
function TCatSongs.FindNextVisible(SearchFrom:integer): integer;// Find next Visible Song
var
  I: integer;
begin
  Result := -1;
  I := SearchFrom;
  while (Result = -1) do
  begin
    Inc (I);

    if (I > High(CatSongs.Song)) then
      I := Low(CatSongs.Song);

    if (I = SearchFrom) then // Make One Round and no song found->quit
      Break;

    if (CatSongs.Song[I].Visible) then
      Result := I;
  end;
end;

function TCatSongs.FindPreviousVisible(SearchFrom:integer): integer;// Find previous Visible Song
var
  I: integer;
begin
  Result := -1;
  I := SearchFrom;
  while (Result = -1) do
  begin
    Dec (I);

    if (I < Low(CatSongs.Song)) then
      I := High(CatSongs.Song);

    if (I = SearchFrom) then // Make One Round and no song found->quit
      Break;

    if (CatSongs.Song[I].Visible) then
      Result := I;
  end;
end;

// Wrong song selected when tabs on bug End

(**
 * Returns the number of visible songs.
 *)
function TCatSongs.VisibleSongs: integer;
var
  SongIndex: integer;
begin
  Result := 0;
  for SongIndex := 0 to High(CatSongs.Song) do
  begin
    if (CatSongs.Song[SongIndex].Visible) then
      Inc(Result);
  end;
end;

(**
 * Returns the index of a song in the subset of all visible songs.
 * If all songs are visible, the result will be equal to the Index parameter. 
 *)
function TCatSongs.VisibleIndex(Index: integer): integer;
var
  SongIndex: integer;
begin
  Result := 0;
  for SongIndex := 0 to Index - 1 do
  begin
    if (CatSongs.Song[SongIndex].Visible) then
      Inc(Result);
  end;
end;

function TCatSongs.SetFilter(FilterStr: UTF8String; Filter: TSongFilter): cardinal;
var
  I, J:      integer;
  TmpString: UTF8String;
  WordArray: array of UTF8String;
begin

  FilterStr := Trim(LowerCase(FilterStr));
  FilterStr := GetStringWithNoAccents(FilterStr);

  if (FilterStr <> '') then
  begin
    Result := 0;

    // initialize word array
    SetLength(WordArray, 1);

    // Copy words to SearchStr
    I := Pos(' ', FilterStr);
    while (I <> 0) do
    begin
      WordArray[High(WordArray)] := Copy(FilterStr, 1, I-1);
      SetLength(WordArray, Length(WordArray) + 1);

      FilterStr := TrimLeft(Copy(FilterStr, I+1, Length(FilterStr)-I));
      I := Pos(' ', FilterStr);
    end;

    // Copy last word
    WordArray[High(WordArray)] := FilterStr;

    for I := 0 to High(Song) do
    begin
      if not Song[i].Main then
      begin
        case Filter of
          fltAll:
            TmpString := Song[I].ArtistNoAccent + ' ' + Song[i].TitleNoAccent + ' ' + Song[i].LanguageNoAccent + ' ' + Song[i].EditionNoAccent + ' ' + Song[i].GenreNoAccent + ' ' + IntToStr(Song[i].Year) + ' ' + Song[i].CreatorNoAccent; //+ ' ' + Song[i].Folder;
          fltTitle:
            TmpString := Song[I].TitleNoAccent;
          fltArtist:
            TmpString := Song[I].ArtistNoAccent;
          fltLanguage:
            TmpString := Song[I].LanguageNoAccent;
          fltEdition:
            TmpString := Song[I].EditionNoAccent;
          fltGenre:
            TmpString := Song[I].GenreNoAccent;
          fltYear:
            TmpString := IntToStr(Song[I].Year);
          fltCreator:
            TmpString := Song[I].CreatorNoAccent;
        end;
        Song[i].Visible := true;
        // Look for every searched word
        for J := 0 to High(WordArray) do
        begin
          Song[i].Visible := Song[i].Visible and
                             UTF8ContainsStr(TmpString, WordArray[J])
        end;
        if Song[i].Visible then
          Inc(Result);
      end
      else
        Song[i].Visible := false;
    end;
    CatNumShow := -2;
  end
  else
  begin
    for i := 0 to High(Song) do
    begin
      Song[i].Visible := (Ini.Tabs = 1) = Song[i].Main;
      CatNumShow := -1;
    end;
    Result := 0;
  end;
end;

// -----------------------------------------------------------------------------

end.
