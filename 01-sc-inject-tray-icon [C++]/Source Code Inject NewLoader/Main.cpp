#include <Windows.h>
#include <stdlib.h>
#include <stdio.h>
#include <string>

#include "TrayIcon.h"
#include "HTTP.h"


#pragma warning(disable:4996)

BOOL GetFile (CHAR *szUrl,        // Full URL
	CHAR *szFileName)   // Local file name
{
	HINTERNET hOpen = InternetOpen("Mozilla/5.0 (Windows NT 6.3;)", INTERNET_OPEN_TYPE_PRECONFIG, NULL, NULL, NULL);
	DWORD dwSize;
	CHAR   szHead[] = "Accept: */*\r\n\r\n";
	VOID * szTemp[25];
	HINTERNET  hConnect;
	FILE * pFile;

	if ( !(hConnect = InternetOpenUrl ( hOpen, szUrl, szHead,
		lstrlen (szHead), INTERNET_FLAG_DONT_CACHE, 0)))
	{

		return 0;
	}

	if  ( !(pFile = fopen (szFileName, "wb" ) ) )
	{

		return FALSE;
	}
	do
	{
		// Keep coping in 25 bytes chunks, while file has any data left.
		// Note: bigger buffer will greatly improve performance.
		if (!InternetReadFile (hConnect, szTemp, 50,  &dwSize) )
		{
			fclose (pFile);

			return FALSE;
		}
		if (!dwSize)
			break;  // Condition of dwSize=0 indicate EOF. Stop.
		else
			fwrite(szTemp, sizeof (char), dwSize , pFile);
	}   // do
	while (TRUE);
	fflush (pFile);
	fclose (pFile);
	return TRUE;
}

inline bool file_exists (const std::string& name) {
    if (FILE *file = fopen(name.c_str(), "r")) {
        fclose(file);
        return true;
    } else {
        return false;
    }   
}

#define INTERNAL_PATH "C:\\test.dll"
#define LOCAL_FILENAME "test.dll"

int APIENTRY WinMain(HINSTANCE hInstance,
	HINSTANCE hPrevInstance,
	LPTSTR    lpCmdLine,
	int       nCmdShow)

{
	CTrayIcon *TrayIcon = new CTrayIcon("VIP Generation Loader", true, LoadIcon(GetModuleHandle(NULL), NULL));
	TrayIcon->ShowBalloonTooltip("VIP Generation", "Welcome To VIP Generation!", TrayIcon->eTI_Info);
	Sleep(2000);
	TrayIcon->ShowBalloonTooltip("VIP Generation", "Downloading Cheat Point Blank!", TrayIcon->eTI_Info);

	GetFile("https://www.upload.ee/download/18897431/c521dedb6cc021c6c610/1.txt", LOCAL_FILENAME);

	TrayIcon->ShowBalloonTooltip("VIP Generation", "Cheat Has Been Downloaded!", TrayIcon->eTI_Info);
	Sleep(2000);
	TrayIcon->ShowBalloonTooltip("VIP Generation", "Installing Cheat!", TrayIcon->eTI_Info);

	if(file_exists(INTERNAL_PATH))
	{
		int res = MessageBoxA(0, "Cheat Has Been Installed. Click YES For Update Cheat, Click No For Disabled Cheat", "VIP Generation", MB_YESNO | MB_ICONINFORMATION | MB_TOPMOST);
		if(res == IDYES)
		{
			DeleteFileA(INTERNAL_PATH);
			CopyFileA(LOCAL_FILENAME, INTERNAL_PATH, FALSE);
			TrayIcon->ShowBalloonTooltip("VIP Generation", "Success Update Cheat!", TrayIcon->eTI_Info);
			DeleteFileA(LOCAL_FILENAME);
			Sleep(3000);
			return 0;
		}
		else if(res == IDNO)
		{
			DeleteFileA(INTERNAL_PATH);
			TrayIcon->ShowBalloonTooltip("VIP VIP Generation", "Success Deleted Cheat!", TrayIcon->eTI_Info);
			DeleteFileA(LOCAL_FILENAME);
			Sleep(3000);
			return 0;
		}
	}
	CopyFileA(LOCAL_FILENAME, INTERNAL_PATH, FALSE);
	TrayIcon->ShowBalloonTooltip("VIP Generation", "Success Installed cheat!", TrayIcon->eTI_Info);
	DeleteFileA(LOCAL_FILENAME);
	Sleep(3000);

	return 0;
}