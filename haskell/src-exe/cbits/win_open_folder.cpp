#include <windows.h>
#include <shobjidl.h>
#include <cstdio>

extern "C" {

wchar_t *OnyxPickFolder(wchar_t *StartFolder) {
    PWSTR result = NULL;

    // Adapted from code by Grizz, https://stackoverflow.com/q/8269696
    // TODO do we need to initialize COM, as in https://docs.microsoft.com/en-us/windows/win32/learnwin32/example--the-open-dialog-box ?
    IFileDialog *pfd;
    if (SUCCEEDED(CoCreateInstance(CLSID_FileOpenDialog, NULL, CLSCTX_INPROC_SERVER, IID_PPV_ARGS(&pfd))))
    {
        DWORD dwOptions;
        if (SUCCEEDED(pfd->GetOptions(&dwOptions)))
        {
            pfd->SetOptions(dwOptions | FOS_PICKFOLDERS);
        }

        if (StartFolder) {
            IShellItem* psiStart;
            if (SUCCEEDED(SHCreateItemFromParsingName(StartFolder, NULL, IID_IShellItem, (void**) &psiStart))) {
                pfd->SetFolder(psiStart);
                psiStart->Release();
            }
        }

        if (SUCCEEDED(pfd->Show(NULL)))
        {
            IShellItem *psi;
            if (SUCCEEDED(pfd->GetResult(&psi)))
            {
                if(!SUCCEEDED(psi->GetDisplayName(SIGDN_DESKTOPABSOLUTEPARSING, &result)))
                {
                    result = NULL; // probably not necessary but just to be safe
                }
                psi->Release();
            }
        }
        pfd->Release();
    }

    return result;
}

}
