{$ifndef WIN32}
  Only 32 bit Windows supported!
{$endif}
{$B-,H+,X+,T+,R-,V+,Z-}

{$IfDef CLR}
   {$DEFINE BADCOMPILER}
    YOU ARE USING A COMPILER FOR WHICH THIS PACKAGE IS NOT DESIGNED!

    YOU HAVE BEEN WARNED !!!
{$EndIf}

{$IFDEF CONDITIONALEXPRESSIONS}
  {$WARN UNIT_PLATFORM OFF}
  {$WARN SYMBOL_PLATFORM OFF}
  {$IF (CompilerVersion >= 14)} // Delphi 6.0 or newer
    {$DEFINE VERD6up}
  {$IFEND}
  {$IF (CompilerVersion >= 15)} // Delphi 7.0 or newer
    {$DEFINE VERD7up}
  {$IFEND}
  {$IF (CompilerVersion >= 17)} // Delphi 2005 or newer
    {$DEFINE VERD2005up}
  {$IFEND}
  {$IF (CompilerVersion >= 18.5)} // Delphi 2007 or newer
    {$DEFINE VERD2007up}
    {$DEFINE ZM_INLINE}
  {$IFEND}
  {$IF (CompilerVersion >= 20)} // Delphi 2009 or newer   - UNICODE
    {$DEFINE VERD2009up}
  {$IFEND}
  {$IF (CompilerVersion >= 21)} // Delphi 2010 or newer   - UNICODE
    {$DEFINE VERD2010up}
  {$IFEND}
  {$IF (CompilerVersion >= 22)} // Delphi XE or newer   - UNICODE
    {$DEFINE VERDXEup}
  {$IFEND}
  {$IF (CompilerVersion >= 23)} // Delphi XE2 or newer   - UNICODE
    {$DEFINE VERDXE2up}
  {$IFEND}
{$ELSE}
  {$DEFINE VERpre6}
  {$IfDef VER120}
     {$DEFINE KNOWNCOMPILER}
  {$EndIf}
  {$IfDef VER125}
     {$ObjExportAll On}
  {$EndIf}
  {$IfDef VER130}
    {$DEFINE KNOWNCOMPILER}
    {$IfDef BCB}
      {$ObjExportAll On}
    {$EndIf}
  {$EndIf}

  {$IfNDef KNOWNCOMPILER}
    YOU ARE USING A COMPILER FOR WHICH THIS PACKAGE IS NOT DESIGNED!

    YOU HAVE BEEN WARNED !!!
  {$EndIf}


{$ENDIF}   // $IFDEF CONDITIONALEXPRESSIONS

{$IfDef BADCOMPILER}
  YOU ARE USING A COMPILER FOR WHICH THIS PACKAGE IS NOT DESIGNED!

  YOU HAVE BEEN WARNED !!!
{$EndIf}

{$IfDef BCB}
  {$ObjExportAll On}
{$Endif}

//{$IFDEF VERD6up}
//{$WARN UNIT_PLATFORM OFF}
//{$WARN SYMBOL_PLATFORM OFF}
//{$ENDIF}
