#include "stdafx.h"
#pragma hdrstop
#include "dz_errs.h"

#undef _DZ_FILE_
#define _DZ_FILE_ DZ_UNZXTRCT_CPP
/*
  extract.c -

  Copyright (c) 1990-2007 Info-ZIP.  All rights reserved.

  See the accompanying file LICENSE, version 2007-Mar-4 or later
  (the contents of which are also included in zip.h) for terms of use.
  If, for some reason, all these files are missing, the Info-ZIP license
  also may be found at:  ftp://ftp.info-zip.org/pub/infozip/license.html

  parts Copyright (C) 1997 Mike White, Eric W. Engler
************************************************************************
 Copyright (C) 2009, 2010  by Russell J. Peters, Roger Aelbrecht

   This file is part of TZipMaster Version 1.9.

    TZipMaster is free software: you can redistribute it and/or modify
    it under the terms of the GNU Lesser General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    (at your option) any later version.

    TZipMaster is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU Lesser General Public License for more details.

    You should have received a copy of the GNU Lesser General Public License
    along with TZipMaster.  If not, see <http://www.gnu.org/licenses/>.

    contact: problems@delphizip.org (include ZipMaster in the subject).
    updates: http://www.delphizip.org
    DelphiZip maillist subscribe at http://www.freelists.org/list/delphizip 
************************************************************************/


/* This version modified by Chris Vleghert and Eric W. Engler * for BCB/Delphi Zip, Sep 22, 2000.
*/

/*--------------------------------------------------------
* extract.c
* This file contains the high-level routines ("driver routines") for extrac-
* ting and testing zipfile members.  It calls the low-level routines in files
* explode.c, inflate.c, unreduce.c and unshrink.c.
* Contains:  extract_or_test_files()
*            store_info()   -> StoreInfo()
*            extract_or_test_member()
*            TestExtraField()   RCV Removed
*            MemExtract()
*            MemFlush()
*            fnfilter()
*-------------------------------------------------------------------------*/
#include "DZOper.h"
#include "UnzOp.h"
#include <stdio.h>

/* ==================
*    Function extract_or_test_files()
*/
int UnzOpr::SelectMembers(void)
{
	/* return PK-type error code */
	uch * cd_inptr;
	int i, j = 0, cd_incnt;
	int error, error_in_archive = PK_COOL;
	unsigned members;
	int why_skipped = 0;
	ZInt64 cd_bufstart;
	ulg sig;
    ZInt64 TotSize;
    unsigned fspecs;
	UnzFileSpec *pfn;

    int cnt;
	bool notEOC = false;
	DZStrW g_excludes = MakeExclFilters();   // Process arguments
    if (Verbose < 0)
		Notify(IVERBOSE, _T("Exclude: \"%s\""), g_excludes.c_str());

    ffilespecs = 0;
    // get first spec and count
    DZStrW fspec = CB->UserArg(zcbFSpecArgs, 0, &ffilespecs);
	DZStrW spec;
	DZStrW tmp;
	unsigned index = 0;

    if (fspec.IsEmpty())
    {
		ffilespecs = 1;
		spec = _T("*.*");
		fprocess_all_files = true;
	}
    else
    if (ffilespecs < 2)
	{
		// if is *.* just do all - quicker
		spec = GetArg(fspec, index, true);
		if (spec.IsEmpty() || !spec.Compare(_T("*.*")) || !spec.Compare(_T("*")))
		{
			fprocess_all_files = true;
			ffilespecs = 1;
		}
	}
	if (fprocess_all_files)
		fprocess_all_files = g_excludes.IsEmpty();

    if (Verbose < 0)
        Notify(ITRACE, _T("search specs = %d, all = %d"), ffilespecs, fprocess_all_files ? 1 : 0);

    SrchSpecs = new UnzFileSpec[ffilespecs];
    pfn = &SrchSpecs[0];

	bool ExcChanged = false;
	pfn->SetExcludes(g_excludes);
	for (i = 0; i < ffilespecs; i++)
	{
		if (ExcChanged)
		{
			pfn->SetExcludes(g_excludes);
            if (Verbose < 0)
				Notify(IVERBOSE, _T("Exclude now: \"%s\""), pfn->GetExcludes().c_str());
            ExcChanged = false;
		}
		// initial set Match -1= wild, 1=check
		if (i)
		{
			// get next
			fspec = CB->UserArg(zcbFSpecArgs, i, 0);
			if (!fspec)
			{
				if (Verbose < 0)
					Notify(ITRACE, _T("fewer specs than expected = %d [%d]"), i, ffilespecs);
				ffilespecs = i;
				break;
			}
		}
		index = 0;//1;
		tmp = GetArg(fspec, index, true);
		int nerr = CleanPath(tmp, spec);
		if (nerr < 0)
		{
			int bad = DZ_ERM_BAD_OPTIONS;
			if (Verbose)
				Notify((unsigned)bad, _T("invalid filespec %s"), fspec.c_str());
			return bad;
		}

		if (spec.IsEmpty())
			spec = _T("*.*");   // presume they mean all

		while (fspec[index] == '/')
		{
			// process switches
			tmp = GetArg(fspec, ++index, true);
			if (tmp.length() < 1)
				continue;
			TCHAR ch = tmp[0]; // the switch
			if (tmp.length() >= 2 && tmp[1] == _T(':'))
			{
				DZStrW tmp2;
				switch (ch)
				{
					case _T('e'):
					case _T('E'):
						tmp2 = ConvExclFilters(tmp.Mid(2));
						if (!tmp2.IsEmpty() && tmp[0] == _T('|'))
							tmp2 = g_excludes + tmp2; // append the new ones
						pfn->SetExcludes(tmp2);
						ExcChanged = true;

						if (Verbose < 0)
							Notify(IVERBOSE, _T("Exclude now: \"%s\""), pfn->GetExcludes().c_str());
						break;
					case _T('f'):
					case _T('F'):
						if (tmp.length() == 2)
						{
							// use current dir
							if (!GetFullPathName(_T(".\\"), MAX_PATH, tmp.GetBuffer(MAX_PATH), NULL))
								throw DZFatalException(DZ_ERM_MEMORY);
							tmp.ReleaseBuffer();
						}
						if (tmp.LastChar() != BSLASH)
							tmp += BSLASH;
						// new base folder
						if (CleanPath(tmp.Mid(2), tmp) == 0)//, checkA))
						{
							pfn->Base = AddBase(tmp, true);   // add to list, bring to front
						}
						break;
					default:
						if (Verbose)
							Notify(IVERBOSE, _T("invalid switch %s ignored"), tmp.c_str());
				}
				continue;
			}
		}
		if (fspec[index] == ZPasswordFollows)
		{
			tmp = tmp.Mid(++index);
			pfn->SetPassword(AddPW(tmp, true));
		}
		if (!pfn->Base)
			pfn->Base = CurBase;  // default
		if (fprocess_all_files)
		{
			pfn->Match = -1;
			break;
		}

		pfn->SetSpec(spec);
		pfn->Match = IsWild(spec) ? -1 : 1;
//	if (Verbose < 0)
//		Notify(ITRACE, _T("Search spec %s [%d]"), spec.c_str(), pfn->Match);
		pfn++;
	}

    /*---------------------------------------------------------------------------
    * Since the central directory lies at the end of the zipfile, and the
    * member files lie at the beginning or middle or wherever, it is not very
    * efficient to read a central directory entry, jump to the member file
    * and extract it, and then jump back to the central directory.
    *
    * Instead, we read from the central directory the pertinent information
    * for all files, then go extract/test the whole block.  Thus, this
    * routine contains two loops:
    * the first loop reads all files from the central directory storing
    * information for the required files
    * the second extracts or tests each file
    *
    * Because of this jumping around, we can afford to be lenient if an
    * error occurs in one of the member files:  we should still be able to
    * go find the other members, since we know the offset of each from the
    * beginning of the zipfile.
    *---------------------------------------------------------------------------*/
    TotSize = 0;
    chdrCount = 0;
    fpInfo = NULL;

    // We're going to check each member file inside the archive
    members = (ulg)fecrec.total_entries_central_dir;

    if (Verbose < 0)
    {
        Notify(ITRACE, _T("Searching for files to extract or test"));
        Notify(ITRACE, _T("Central entries = %d, filespecs = %d"), members, ffilespecs);
    }

//	fnewzip = true;
    if (fP_flag)
        fkey = AddPW(fpwdarg, true);   // save and set the global password
	freported_backslash = false;
    CB->UserXItem(members, 9, _T("*Loading central"));

    cnt = 0;
    cd_bufstart = fcur_zipfile_bufstart;
    cd_inptr = finptr;
    cd_incnt = fincnt;
	fspecs = (unsigned)ffilespecs;
    CHdrInfo *chdrEnd = NULL;
    int chdrseq = 0;

    while (members && !Abort_Flag)
    {
        int DoThisFile = false;

        if (!fpInfo)
        {
            fpInfo = new CHdrInfo;
        }
        else
            fpInfo->Clear();
        fpInfo->chdrseq = (unsigned)(chdrseq++);

        members--;

        if (readbuf((char*)&sig, 4) == 0)
            break;

        if (sig != CentralFileHeaderSig)
            break;

        if ((error = process_cdir_file_hdr()) != PK_COOL)
            break;
        if ((error = do_string(fcrec.filename_length, DS_FN)) != PK_COOL)
        {
            if (error > error_in_archive)
                error_in_archive = error;

        if (PK_ERR_NOWARN(error))
                break;   /* fatal */
        }

        if ((error = do_string(fcrec.extra_field_length, EXTRA_FIELD)) != PK_COOL)
        {
            if (PK_Rank(error) > PK_Rank(error_in_archive)) 
                error_in_archive = error;

            if (PK_ERR_NOWARN(error))
                    break;  /* fatal */
        }

        do_string(fcrec.file_comment_length, SKIP);

		int flen = fcrec.filename_length;
		fpInfo->SetHName(file_name);

		DZStrW wn;
		if (fcrec.extra_field_length && (fEncodedAs == zeoAuto || fEncodedAs == zeoUPATH))
		{
			const UPhead *up = (const UPhead*)extra_field.Find(UPATH_XDATA_TAG);
			if (up && up->ver == 1 && up->hed.vsize > sizeof(UPhead))
			{
				ulg ocrc = crc32(0, (uch*)file_name.c_str(), flen);
				if (ocrc == up->crc)
				{
					int len = up->hed.vsize - 5;
					const char *bf = ((char*)up) + sizeof(UPhead);
					wn = DZStrW(CP_UTF8, bf, len);
				}
			}

		}
		// use UPATH if was available
		if (wn.IsEmpty())
		{
			int Enc = (int)IsEncoded((ush)fpInfo->host, fpInfo->UTF8flag);
			if (Enc == zeoAuto && ValidUTF8(file_name) > 0)
				Enc = zeoUTF8;
			/*int*/UINT UseCP = 0;
			switch ((Encodings)Enc)
			{
				case zeoAuto:
				case zeoNone: UseCP = fFromPage;
					break;

				case zeoUTF8: UseCP = CP_UTF8;
					break;

				case zeoOEM: UseCP = CP_OEM;
					break;
			}
			wn = DZStrW(UseCP, file_name.c_str(), flen);
		}
 /*  defer test until extract
		DZStrW wt = StrExtSep(wn);
		int nerr = CleanPath(wt, ffilename);
		if (nerr)
		{
			if (Verbose)
				Notify(IVERBOSE, _T("read file - invalid filename %s [%d]"),
								fpInfo->hname, nerr);
			error_in_archive = DZ_ERR_INVAL_NAME;
			break;  // fatal
		}     */
		ffilename = StrExtSep(wn);
		fpInfo->SetXName(ffilename);
		if (Verbose < 0)
			Notify(ITRACE, _T("Filename = %s"), ffilename.c_str());

        pfn = (UnzFileSpec *) SrchSpecs;

		if (fprocess_all_files)
            DoThisFile = true;
        else
		{
			unsigned hash = HashFunc(ffilename);
			int k;// = 0;
			for (k = 0; k < ffilespecs; k++, ++pfn)
			{
				DoThisFile = false;
                if (pfn->Match)
				{

					if (pfn->Match == 1)
					{
						if (pfn->Hash != hash)
							continue;   // not wild and will not match
					}

					const TCHAR *fs = pfn->GetFileSpec();
					if (Is_Drv(pfn->GetFileSpec()) < 0)
						fs += DriveLen(pfn->GetFileSpec()); // get filename for stream

					if (ZMatch(fs, ffilename))
					{
						DZStrW exc = pfn->GetExcludes();
						DoThisFile = exc.IsEmpty() ? true : !ZMatch(exc, ffilename);
//						DoThisFile = true;

						pfn->Match = pfn->Match & -2;   // reset bit 0 for match
						if ((!pfn->Match) &&
								(! --fspecs)) // removed non-wild from search
                        {
                            members = 0; // none left to match
                            notEOC = true;
                        }

						if (Verbose < 0)
							Notify(ITRACE, _T("Matched %s"), ffilename.c_str());

                        break;  // found match, so stop looping
                    }
                }
            }
        }

        if (DoThisFile)
        {
            int r;

            if ((r = StoreInfo()) == 0)
            {
                if (Verbose < 0) //** less noisy
                    Notify(ITRACE, _T("selected: %s"), ffilename.c_str());

                fpInfo->spec = pfn;
                j++;
                TotSize += fcrec.ucsize;

                if (chdrList)
                    chdrEnd->next = fpInfo;   // append to list
                else
                    chdrList = fpInfo;       // first in list

                chdrEnd = fpInfo;
                fpInfo = NULL;              // been used
                chdrCount++;
            }
            else
			{
				bool usr = Skipping(ffilename, 0, r);

                if (!why_skipped)
                    why_skipped = r;
                else
                    if (why_skipped != r)
                        why_skipped = -1;
				if (usr)
				{
					error_in_archive = DZ_ERR_SKIPPED;
                    break;
                }
            }
        }

        if (++cnt >= 50)
        {
            CB->UserXProgress(cnt, 9);
            cnt = 0;
        }
    }

	if (cnt)
		CB->UserXProgress(cnt, 9);

    // delete if unused
    if (fpInfo)
    {

		CHdrInfo* tmp1 = fpInfo;

        fpInfo = NULL;

		delete tmp1;
    }

    if (PK_ERR_NOWARN(error_in_archive))
        return error_in_archive;  /* fatal */

	CB->SetArg1(j);
    CB->UserCB(zacCount);
    CB->SetFileSize(TotSize);
    CB->UserCB(zacSize);

    /*---------------------------------------------------------------------------
    * Double-check that we're back at the end-of-central-directory record, and
    * print quick summary of results, if we were just testing the archive.
    *---------------------------------------------------------------------------*/

    if (readbuf((char*)&sig, 4) == 0)
    {
        if (Verbose)
            Notify(ITRACE, _T("bad signature at end of archive, or premature EOF"));

        error_in_archive = PK_EOF;
    }

    if (sig != EndCentralDirSig && sig != EndCentral64Sig &&
            (notEOC && sig != CentralFileHeaderSig))
    {
        /* just to make sure */
        Notify(IWARNING,  _T("Bad Ending Signature for Central dir"));

        if (!error_in_archive) /* don't overwrite stronger error */
            error_in_archive = PK_WARN;
    }


    // RCV Changed 29-1-99 USE_STREAM_INPUT
	fcur_zipfile_bufstart = fUnzInfile->SetPosition(cd_bufstart, SEEK_SET);
    vclRead((char *) finbuf, INBUFSIZ);

    finptr = cd_inptr;
    fincnt = cd_incnt;

    return error_in_archive;
}

// true = overwrite
int UnzOpr::AskOverwrite(const DZStrW& filename, int cond)
{
	int ucb;
	int ret = 1;   // default overwrite
    switch (cond)
    {

        case DOES_NOT_EXIST:
            // NOVELL_BUG_FAILSAFE:
            //            fdne = true;
            /* stat() says file DOES NOT EXIST */
            /* if freshening, don't skip if just renamed */

            if (ffflag)
			{
				ret = Skipping(filename, 0, SKIPPED_ON_FRESHEN /*fpInfo->chdrseq */) ? -1 : 0;
                /* freshen (no new files):  skip */
            }

            break;

        case EXISTS_AND_OLDER:
            // Ask the user what to do, take overwrite_all as default. RCV: 1.6010
			CB->SetArg3(1);
			CB->SetArg2(fpInfo->chdrseq);
			CB->SetArg1(foverwrite_all);
			CB->SetMsg(filename);
            ucb = CB->UserCB(zacOverwrite);

            if (ucb <= 0)           // no response - use default
                ucb = foverwrite_all ? CALLBACK_TRUE : CALLBACK_2;

            if (ucb != CALLBACK_TRUE)          // responded 'no'
			{
				ret = Skipping(filename, 0, SKIPPED_NO_OVERWRITE) ? -1 : 0;
                if (Verbose)
                    Notify(ITRACE, _T("File exists: %s, overwrite false"),
							filename.c_str());
				 /* never overwrite:  skip file */
            }
            break;

        case EXISTS_AND_NEWER:
            /* or equal */
            // Ask the user what to do, take overwrite_all as default. RCV: 1.6010
            // If no event handler present w'll keep the default.
			CB->SetArg3(2);
			CB->SetArg2(fpInfo->chdrseq);
			CB->SetArg1(foverwrite_all && !(fuflag || ffflag));
			CB->SetMsg(filename);
            ucb = CB->UserCB(zacOverwrite);

            if (ucb <= 0)           // no response - use default
                ucb = CB->GetArg1() ? CALLBACK_TRUE : CALLBACK_2;

			if (ucb != CALLBACK_TRUE)          // responded 'skip'
			{
				if (Verbose)
					Notify(ITRACE, _T("File exists: %s, skipped"), filename.c_str());
				ret = Skipping(filename, 0, SKIPPED_FILE_EXISTS) ? -1 : 0;
                /* skip if update/freshen & orig name */
			}
    }     /* end switch */
    return ret;
}

int UnzOpr::extract_or_test_files(void)
{
    /* return PK-type error code */
    int i, filnum = (-1);
    int error, error_in_archive;// = PK_COOL;
    unsigned num_skipped = 0, num_bad_pwd = 0;
    int why_skipped = 0;
    int did_skip = 0;       // skipped previous file
    ZInt64 bufstart, inbuf_offset, request;
    ZInt64 old_extra_bytes = 0;
    ZInt64 fsize;
    ulg sig;

    int changed;
	int ucb;          // result of user_cb

	error_in_archive = SelectMembers();
//  if (Verbose < 0)
//    Notify(ITRACE, "starting main loop");

    InProgress = false;
    fBytesWritten = 0;
    /*---------------------------------------------------------------------------
    *    Begin main loop over member files.
    *---------------------------------------------------------------------------*/
    const TCHAR *OpMsg = ftflag ? _T("test") : _T("extract");

    if (Verbose)
        Notify(IVERBOSE,  _T("Expects to %s %d files"), OpMsg, chdrCount);

    /*-----------------------------------------------------------------------
    *  Second loop:  process files in current block, extracting or testing
    *  each one.
    *-----------------------------------------------------------------------*/
    if (Verbose < 0)
        Notify(ITRACE, _T("starting second loop - THIS TIME we take action."));

    CHdrInfo* Info = chdrList;

    DZStrW CurrentName;

    bool Stopped = false;
	while (Info)
    {
        if (did_skip)
		{
//			did_skip = 0;
			if (Skipping(ffilename, 0, did_skip))
			{
				Fatal(DZ_ERM_SKIPPED, 2, false);
				break;
			}
			did_skip = 0;
        }
		if (Abort_Flag)
		{
			Fatal(DZ_ERM_CANCELLED, 2, false);
			break;
		}
        fpInfo = Info;
        Encrypted = fpInfo->encrypted != 0;
        Info = Info->next;
        if (Stopped)
        {
			// give zacSkipped
			if (Skipping(fpInfo->GetXName(), DZ_ERR_SKIPPED, SKIPPED_USER))
				Fatal(DZ_ERM_SKIPPING, 2);
            continue;
        }
        filnum++;
		CurrentName = fpInfo->GetXName();

		if (Verbose < 0)   //** less noisy
            Notify(ITRACE, _T("%sing %s"), OpMsg, CurrentName.c_str());

        /* if the target position is not within the current input buffer
        * (either haven't yet read far enough, or (maybe) skipping back-
        * ward), skip to the target position and reset readbuf(). */
        request = fpInfo->offset + fextra_bytes;
        inbuf_offset = request % INBUFSIZ;
        bufstart = request - inbuf_offset;

        if (Verbose < 0)
            Notify(ITRACE, _T("loc A: request = %Ld, inbuf_offset = %Ld"),
                request, inbuf_offset);

        if (request < fpInfo->offset)
        {
			DZError(DZ_ERM_INVAL_ZIP);
            error_in_archive = PK_ERR;

            if (filnum == 0 && fextra_bytes != 0L)
            {
                if (Verbose < 0)
                    Notify(ITRACE,  _T("attempting to recompensate"));

                old_extra_bytes = fextra_bytes;
                fextra_bytes = 0L;
                request = fpInfo->offset;

                /* could also check if this != 0 */
                inbuf_offset = request % INBUFSIZ;
                bufstart = request - inbuf_offset;

                if (Verbose < 0)
                    Notify(ITRACE, _T("loc B: request = %Ld, inbuf_offset = %Ld"),
                        request, inbuf_offset);
            }
            else
            {
                error_in_archive = PK_BADERR;
				if (Verbose)
                    Notify(ITRACE, _T("loc A: hosed - try next file"));

                continue;    /* this one hosed; try next */
            }
        }

        /* try again */
        if (request < 0)
        {
            if (Verbose < 0)
                Notify(0, _T("the recompensated request is still < 0"));

			DZError(DZ_ERM_INVAL_ZIP);
            did_skip = SKIPPED_FILEFORMAT_WRONG;
            error_in_archive = PK_BADERR;
            continue;
        }
        else
            if (bufstart != fcur_zipfile_bufstart)
            {
                if (Verbose < 0)
                    Notify(ITRACE, _T("bufstart != cur_zipfile_bufstart"));

                // RCV Changed 29-1-99 USE_STRM_INPUT
                fcur_zipfile_bufstart = fUnzInfile->SetPosition(bufstart, SEEK_SET);

                fincnt = vclRead((char *) finbuf, INBUFSIZ);

                if (fincnt <= 0)
                {
					DZError(DZ_ERM_INVAL_ZIP);
					error_in_archive = PK_BADERR;

					if (Verbose)
                        Notify(ITRACE, _T("B. hosed - try next file"));

                    continue;    /* can still do next file */
                }

                finptr = finbuf + (int)inbuf_offset;
                fincnt -= (int)inbuf_offset;
            }
            else
            {
                fincnt += (int)(finptr - finbuf) - (int)inbuf_offset;
                finptr = finbuf + (int)inbuf_offset;
            }

        /* should be in proper position now, so check for sig */
        if (readbuf((char*)&sig, 4) == 0)
        {
            /* bad offset */
			DZError(DZ_ERM_INVAL_ZIP);
            did_skip = SKIPPED_FILEFORMAT_WRONG;
            error_in_archive = PK_BADERR;
            continue;      /* but can still try next one */

        }

        if (sig != LocalFileHeaderSig)
        {
			DZError(DZ_ERM_INVAL_ZIP);
            error_in_archive = PK_ERR;

            if ((filnum == 0 && fextra_bytes != 0L)
                    || (fextra_bytes == 0L && old_extra_bytes != 0L))
            {
                if (Verbose < 0)
                    Notify(ITRACE,  _T("Attempting to Recompensate"));

                if (fextra_bytes)
                {
                    old_extra_bytes = fextra_bytes;
                    fextra_bytes = 0L;
                }
                else
                    fextra_bytes = old_extra_bytes;

                /* third attempt */
                zlseek(fpInfo->offset);

                if (readbuf((char*)&sig, 4) == 0)
                {
                    /* bad offset */
					DZError(DZ_ERM_INVAL_ZIP);
                    did_skip = SKIPPED_FILEFORMAT_WRONG;
                    error_in_archive = PK_BADERR;
                    continue;         /* but can still try next one */

                }

                if (sig != LocalFileHeaderSig)
                {
					DZError(DZ_ERM_INVAL_ZIP);
                    error_in_archive = PK_BADERR;
                    continue;
                }
            }
            else
            {
                if (Verbose)
                    Notify(ITRACE, _T("C: hosed - try next file"));

                continue;          /* this one hosed; try next */
            }
        }

        if (Verbose < 0)
            Notify(ITRACE, _T("about to process local file hdr"));

		if ((error = process_local_file_hdr()) != PK_COOL)
        {
			DZError(DZ_ERM_INVAL_ZIP);
            did_skip = SKIPPED_FILEFORMAT_WRONG;
            error_in_archive = error;  /* only PK_EOF defined */

            if (Verbose)
                Notify(ITRACE, _T("D. hosed - try next file"));

            continue;         /* can still try next one */
        }

        if ((error = do_string(flrec.filename_length, DS_FN)) != PK_COOL)
        {
            if (PK_Rank(error) > PK_Rank(error_in_archive))
                error_in_archive = error;

			if (PK_ERR_NOWARN(error))
            {
                did_skip = SKIPPED_FILEFORMAT_WRONG;
				DZError(DZ_ERM_INVAL_ZIP);

                if (Verbose)
                    Notify(ITRACE, _T("E. hosed - try next file"));

                continue;       /* go on to next one */
            }
        }

        extra_field.Empty();

        if ((error = do_string(flrec.extra_field_length, EXTRA_FIELD)) != 0)
        {
            if (PK_Rank(error) > PK_Rank(error_in_archive))
                error_in_archive = error;

            if (PK_ERR_NOWARN(error))
            {
                did_skip = SKIPPED_FILEFORMAT_WRONG;
				DZError(DZ_ERM_INVAL_ZIP);
                if (Verbose)
                    Notify(ITRACE, _T("F. hosed - try next file"));
                continue;    /* go on */                   
            }
        }

        /* Just about to extract file:  if extracting to disk, check if
        * already exists, and if so, take appropriate action according to
        * fflag/uflag/overwrite_all/etc. (we couldn't do this in upper
        * loop because we don't store the possibly renamed filename[] in
        * info[]) */
        // use central name
		if (file_name.CompareNoCase(fpInfo->GetHName()))
        {
            error = PK_WARN;
            Notify(IWARNING, _T("Names do not match - using '%s' [local:%s]"),
                   CurrentName.c_str(), file_name.c_str());

            if (PK_Rank(error) > PK_Rank(error_in_archive))
				error_in_archive = error;
        }

		if (Is_Drv(fpInfo->spec->GetSpec()) < 0)
            ffilename = fpInfo->spec->GetSpec();
        else
        {
            ffilename = CurrentName;

			if (!ftflag)
            {
                /* for files from DOS FAT, check for use of backslash instead
                * of slash as directory separator (bug in some zipper(s); so
				* far, not a problem in HPFS, NTFS or VFAT systems) */
				if (((fpInfo->host & 0xff00) == (FS_FAT_ * 256)) &&
					ffilename.ReverseFind(_T('/')) >= 0)
				{
					ffilename = StrExtSep(ffilename);

                    if (Verbose < 0)
                        Notify(ITRACE, _T("parsing a FAT file"));
                }

				ucb = CB->UserCB(zacExtName, ffilename, fpInfo->spec->Base->GetBaseDir());
                if (Abort_Flag)
                {
                    Fatal(DZ_ERM_CANCELLED, 2, false);
                    break;
                }
                changed = 0;
				if (ucb == CALLBACK_TRUE)      // changed
				{
					unsigned index = 0;
					DZStrW nname = StrExtSep(CB->GetMsg());
					DZStrW tmp = GetArg(nname, index, false);
					int ss = CleanPath(tmp, nname);
					if (ss < 0 || ss == Z_WILD)
					{
						if (Verbose)//< 0)
							Notify(ITRACE, _T("invalid new name %s"), nname.c_str());
						error_in_archive = PK_ERR;
						did_skip = SKIPPED_BAD_NAME;
						continue; // try next
					}
                    if (!nname)
                    {
                        if (Verbose)//< 0)
                            Notify(ITRACE, _T("file ignored - try next file"));
                        error_in_archive = PK_ERR;
                        did_skip = SKIPPED_USER;
                        continue; // try next
                    }

                    bool wasDir = ffilename.LastChar() == BSLASH;
                    if ((nname.LastChar() == BSLASH) != wasDir)
                    {
                        if (Verbose)
                            Notify(IVERBOSE, _T("Must not change %s to %s"),
                                ffilename.c_str(), nname.c_str());
						error_in_archive = PK_ERR;
						did_skip = SKIPPED_BAD_NAME;
                        continue; // try next
                    }

                    if (Verbose)
                        Notify(IVERBOSE,  _T("filename changed from %s to %s"),
                            ffilename.c_str(), nname.c_str());
					if (fpInfo->ntfs_data &&
						(ffilename.CompareNoCase(nname.Right(ffilename.length()).c_str())))
                    {
                        delete fpInfo->ntfs_data;   // new file - times invalid
                        fpInfo->ntfs_data = NULL;
                        if (Verbose)
                            Notify(IVERBOSE,  _T("filename changed - times invalid"));
                    }
                    ffilename = nname;
                    changed++;
                }
				// verify an acceptable filename
				DZStrW wt = ffilename;
				int nerr = CleanPath(wt, ffilename);
				if (nerr)
				{
					if (Verbose)
						Notify(IVERBOSE, _T("extract file - invalid filename %s [%d]"),
										/*fpInfo->hname*/wt.c_str(), nerr);
					error_in_archive = DZ_ERR_INVAL_NAME;
					did_skip = SKIPPED_BAD_NAME;
					continue; // skip file
				}

				bool IsDir = ffilename.LastChar() == '\\';
				/* filename contains the name as stored in the archive if the user want
				   to extract directories too then this part of the path is also present.
				   So we only need to complement it with the currentdir or the extractbasedir.
				   Mapname can create dirs if not freshening or if renamed */
				if (IsDir)
					CB->UserItem(0, ffilename);
				error = mapname(changed);
//				if (IsDir)
//					CB->UserItem(-1, ffilename);  // mark end of item
				if (error < 0)
				{
					if (error == IZ_CREATED_DIR || error == IZ_SKIP_DIR)
					{
						/* GRR:  add code to set times/attribs on dirs--
						* save to list, sort when done (a la zip), set
						* times/attributes on deepest dirs first */
						int doset = 0;
						if (error == IZ_CREATED_DIR)
						{
							if (IsDir)
							{
								Notify(0,  _T("   created: %s"), ffilename.c_str());
								doset = 1;//true; // didn't exist
							}
							else
								Notify(0,  _T("   forced: %s"), ffilename.c_str());
							if (!changed && fpInfo->ntfs_data && IsDir)
							{
								if (!doset)
									doset = AskOverwrite(ffilename, doset ? -1 :
											check_for_newer(ffilename));
								if (doset < 0)
									Fatal(DZ_ERM_ABORT, 2);
								if (doset > 0)
								{
									bool notset = true;
									// set folder stamp
									HANDLE hFolder = CreateFile(ffilename.c_str(), GENERIC_WRITE,
											FILE_SHARE_READ, NULL, OPEN_EXISTING,
											FILE_ATTRIBUTE_DIRECTORY | FILE_FLAG_BACKUP_SEMANTICS, NULL);
									if(hFolder != INVALID_HANDLE_VALUE)
									{
										notset = !SetFileTime(hFolder, &fpInfo->ntfs_data->CTime,
										&fpInfo->ntfs_data->ATime, &fpInfo->ntfs_data->MTime);
										CloseHandle(hFolder);
									}
									if (notset && Verbose < 0)
										Notify(IWARNING, _T("Could not set times for %s"),
											ffilename.c_str());
								}
							}
						}
					}
					//-----------------------------------------------------------
					// BUG FIX since v1.4: bump up the file count when we extract
					// a dirname (whether or not it already existed).
					// This keeps SuccessCnt in line with TZipContents.Count
					// BUG FIX since v1.9.0.103 extract the file we forced the
					//  directory for
					if (IsDir || error != IZ_CREATED_DIR)
					{
						CB->UserItem(-1, ffilename);  // mark end of item
						ffiles_acted_on++;
						continue;  // don't try to 'extract' it
					}
				}
				if (IsDir)
					CB->UserItem(-1, ffilename);  // mark end of item
				if (PK_Rank(error) > PK_Rank(PK_WARN))
				{
					if (error == IZ_VOL_LABEL)
					{
						if (Verbose)// < 0)
							Notify(ITRACE, _T("file is a vol label"));

						DZError(DZ_ERM_INVAL_ZIP);
					}
					else
					if (PK_Rank(error) > PK_Rank(PK_ERR) &&
							PK_Rank(error_in_archive) < PK_Rank(PK_ERR))
					{
						if (Verbose)// < 0)
							Notify(IVERBOSE, _T("f hosed - try next file"));
						error_in_archive = PK_ERR;
					}

                    if (Verbose < 0)
                        Notify(ITRACE, _T("mapname(%s) returns error = %d"),
                            ffilename.c_str(), error);
                    continue;     /* go on to next file */
                }

                // v1.6024
                flrec.filename_length = (WORD)ffilename.length();

                if (Abort_Flag)
					Fatal(DZ_ERM_ABORT, 0);

                if (Verbose < 0)
                    Notify(ITRACE, _T("starting switch near Novell failsafe in extract.c"));
				int ucbb = AskOverwrite(ffilename, check_for_newer(ffilename));
				if (ucbb < 0)
					Fatal(DZ_ERM_ABORT, 2);
				if (!ucbb)
                    continue;
            }   /* end if (extracting to disk) */
        }

		if (Verbose < 0)
            Notify(ITRACE, _T("in extract.c, about to call decrypt"));

        ulg crc = fpInfo->ExtLocHdr ? flrec.last_mod_file_time >> 8 : (ush)(flrec.crc32 >> 24);

        if (Encrypted)
        {
			if (fpInfo->spec->GetPassword())
            {
                const char* savedKey = fkey;
                fkey = fpInfo->spec->GetPassword();   // use copy of specified
                error = decrypt(crc, true);
                fkey = savedKey; // restore effective key
            }
            else
//                fkey = AddPW(fpwdarg, false);     // use global
                error = decrypt(crc, false);
//            if ((error = decrypt(crc)) != PK_COOL)
            if (error != PK_COOL)
			{
				++num_bad_pwd;

				if (Verbose < 0)
					Notify(ITRACE, _T("Skipping encrypted file %s, bad password"),
						ffilename.c_str());
				if (Skipping(ffilename, 0, SKIPPED_BAD_PASSWORD))
					Fatal(DZ_ERM_SKIPPED, 2);

                if (Abort_Flag)
                {
                    Fatal(DZ_ERM_CANCELLED, 2, false);
					break;
                }
                continue;  // go on to next file
            }
		}
		fdisk_full = 0;

        if (Abort_Flag)
			Fatal(DZ_ERM_ABORT, 0);

        fsize = flrec.ucsize;
		CB->UserItem(fsize, ffilename);

        // ====================================================
        // Function:  extract_or_test_member() does the unzip
        // ====================================================

		if ((error = extract_or_test_member()) != PK_COOL)
        {
			unsigned int zerr = DZ_ERR(error);
            if (zerr <= DZ_ERR_CALLBACK)
            {
                Fatal(DZ_ERM_CANCELLED, 2, false);
                break;
            }
            if (zerr != DZ_ERR(PK_WARN) && 
				(zerr == DZ_ERR(PK_BADERR) || zerr == DZ_ERR(PK_NOZIP) ||
                zerr == DZ_ERR(PK_FIND) || zerr == DZ_ERR(PK_EOF))) 
            {
                /* abort check v1.6026 */
				DZError(error);
                break;
            }

			if (Skipping(ffilename, error, SKIPPED_EXTRACT_ERROR))
				return DZ_ERR_SKIPPED;

            if (Verbose)// < 0)
                Notify(ITRACE, _T("error occured while extracting or testing"));

			if (PK_Rank(error) > PK_Rank(error_in_archive))
                error_in_archive = error;

            /* ...and keep going */
            if (fdisk_full > 1)
            {
                return error_in_archive;
            }
        }
        else
        {
            Notify(IMSG,  _T("%s file %s of size %Lu"), ftflag ?
                _T("Tested  ") : _T("Unzipped"), ffilename.c_str(), fsize);
            ffiles_acted_on++;
		}
		/* Finished Item */
		int uret = CB->UserItem(-1, ffilename);  // mark end of item
        if (uret == CALLBACK_TRUE)
          Stopped = true;
    }

    /* end for-loop (i:  files in current block) */
//  if (Verbose < 0)
//    Notify(ITRACE, "done with big outer block");
    CB->UserCB(zacEndOfBatch); // done with a batch of files

    /*---------------------------------------------------------------------------
    *  Check for unmatched filespecs on command line and print warning if any *  found.  Free allocated memory.
    *---------------------------------------------------------------------------*/
    if (Abort_Flag)
		return Fatal(DZ_ERM_ABORT, 0, false);

    // R. Peters - changed reporting fn_matched not used
    if (!fprocess_all_files && ffilespecs > 0 && SrchSpecs)
    {
        UnzFileSpec *pfn = SrchSpecs;

        for (i = 0; i < ffilespecs; i++)
        {
            // initial bit 0 set if not found
            if (pfn->Match & 1)
            {
                UnzMsg.Format(_T("Filespec Not Matched: %s"), pfn->GetFileSpec());
				DZError(DZ_ERM_MISS);

                if (!DZ_ERR(error_in_archive) || DZ_ERR(error_in_archive) == PK_WARN)
                    error_in_archive = PK_FIND; // some files not found
            }

            pfn++;
        }
    }

    ++filnum;

    /* initialized to -1, so now zero if no files found */
    if (Verbose < 0)
        Notify(ITRACE, _T("filnum = %d"), filnum);

    if (ftflag)
    {
        // testing archive
		int num = filnum - (int)num_bad_pwd;

        if (fqflag < 2)
        {
            /* GRR 930710:  was (fqflag == 1) */
            if (error_in_archive)
            {     
                UnzMsg.Format(_T("Error In Archive %s %s"),
                    (error_in_archive == 1) ? _T("warning-") : _T(" "), fzipfn.c_str());
				DZError(DZM_MessageBit | DZ_ERM_GENERAL, UnzMsg.c_str());
            }
            else
                if (num == 0)
                {
                    UnzMsg.Format(_T("No Files Tested %s"), fzipfn.c_str());
					Notify(0, UnzMsg.c_str());
                }
                else
                    if (fprocess_all_files && (num_skipped + num_bad_pwd == 0))
                    {
                        UnzMsg.Format(_T("no error in %s"), fzipfn.c_str());
						Notify(0, UnzMsg.c_str());
                    }
                    else
                    {
                        if (num > 1)
							UnzMsg.Format(_T("No Errors Found In %d Tested Files of %s"),
									num, fzipfn.c_str());
                        else
							UnzMsg.Format(_T("No Error Found In %d Tested File of %s"),
									num, fzipfn.c_str());

						Notify(0, UnzMsg.c_str());
                    }
        }

        if (num_skipped > 0)
        {
            if (why_skipped > 0)
                why_skipped = (why_skipped & 63) - 1;

            UnzMsg.Format(_T("Skipped %d Files: %d"), num_skipped, why_skipped);
			DZError(DZM_MessageBit | DZ_ERM_SKIPPING, UnzMsg.c_str());
        }

		if (num_bad_pwd > 0)
		{
			UnzMsg.Format(_T("Files with bad pwd: %d"), num_bad_pwd);
			DZError(DZ_ERM_GENERAL, UnzMsg.c_str());
		}
        else
            if ((fqflag == 0) && !error_in_archive && (num == 0))
            {
                UnzMsg.Format(_T("Zero Files Tested %s"), fzipfn.c_str());
				Notify(0, UnzMsg.c_str());
            }
    }

    /* give warning if files not tested or extracted (first condition can still
    * happen if zipfile is empty and no files specified on command line) */
    if ((filnum == 0) && error_in_archive <= PK_WARN)
    {
		DZError(DZ_ERM_NOTHING_TO_DO, _T("no files found"));
        error_in_archive = PK_FIND;
        /* no files found at all */
    }
    else
        if ((num_skipped > 0) && !error_in_archive)
        {
            UnzMsg.Format(_T("some files skipped"));
			Notify(0, UnzMsg.c_str());
            error_in_archive = PK_WARN;
        }    
//#ifdef CRYPT
		else
			if ((num_bad_pwd > 0) && !error_in_archive)
				error_in_archive = PK_WARN;
//#endif
	return error_in_archive;
}

/* end function extract_or_test_files() */


#ifdef COPYRIGHT_CLEAN          /* no reduced or tokenized files */
#  define UNKN_COMPR  (fcrec.compression_method >= SHRUNK && \
                       fcrec.compression_method != IMPLODED \
                       && fcrec.compression_method != DEFLATED \
                       && fcrec.compression_method != ENHDEFLATED)
#else /* !COPYRIGHT_CLEAN */
#  define UNKN_COMPR \
    (fcrec.compression_method > IMPLODED && fcrec.compression_method != DEFLATED)
#endif /* ?COPYRIGHT_CLEAN */

/* ===========================================================================
*   Check central directory info for version/compatibility requirements.
*      Function store_info()
*  changed to StoreInfo - returns reason for skipping */
int UnzOpr::StoreInfo(void)
{
    fpInfo->ntfs_data = NULL;
    /* return err if skipping, 0 if OK */
    fpInfo->encrypted = fcrec.general_purpose_bit_flag & FLAG_ENCRYPT_BIT;

    /* bit field */
    fpInfo->ExtLocHdr = (fcrec.general_purpose_bit_flag & FLAG_EXTEND_BIT) == FLAG_EXTEND_BIT;

    /* bit field */
    fpInfo->textfile = fcrec.internal_file_attributes & 1;

    fpInfo->crc = fcrec.crc32;
    fpInfo->compr_size = fcrec.csize;
    fpInfo->uncomp_size = fcrec.ucsize;
    fpInfo->offset = fcrec.relative_offset_local_header;

    fpInfo->textmode = false;

    // EWE note: all platforms define VMS_UNZIP_VERSION (currently 42)
    if (fcrec.version_needed_to_extract[1] == VMS_)
    {
        if (fcrec.version_needed_to_extract[0] > VMS_UNZIP_VERSION)
        {
            if (Verbose)// < 0)
                Notify(ITRACE, _T("Skipping Unsupported zip version or hosttype: %s"),
                    ffilename.c_str());

            return SKIPPED_UNKNOWN_ZIPHOST;
        }

#ifndef VMS /* won't be able to use extra field, but still have data */
        else
            if (!ftflag && !foverwrite_all)
            {
                /* if -o, extract regardless */
                if (Verbose)// < 0)
                    Notify(ITRACE, _T("Warning - file's format may be incorrect: %s"),
                        ffilename.c_str());

                return SKIPPED_FILEFORMAT_WRONG;
            }

#endif
        /* usual file type:  don't need VMS to extract */
    }
    else
	if (fcrec.version_needed_to_extract[0] > UNZIP_VERSION)
	{
		if (Verbose)// < 0)
			Notify(ITRACE, _T("Skipping Unsupported zip version or hosttype: %s"),
				ffilename.c_str());

		return SKIPPED_UNKNOWN_ZIPHOST;
	}

    if UNKN_COMPR
	{
		// RCV: 1.610
		if (Verbose)// < 0)
				Notify(ITRACE, _T("Skipping Unsupported compression type: %s"),
					ffilename.c_str());

        return SKIPPED_COMPRESSION_UNKNOWN;
	}

    /* map whatever file attributes we have into the local format */
    mapattr();

    if (!extra_field.IsEmpty() && IsNTorAbove && fNTFSStamps)
	{
		const X_NTFSHeader* data = (const X_NTFSHeader*)extra_field.Find(NTFS_STAMP_TAG);
		if (data && data->hed.vsize >= 32)
		{
			unsigned siz = data->hed.vsize;
			const XNTFSData *hdata = (const XNTFSData *)FindTag(1, (const unsigned char*)++data, siz);
			if (hdata && siz == 24)
			{
				fpInfo->ntfs_data = new XNTFSData;
				memcpy((void*)(fpInfo->ntfs_data), (void*)hdata, sizeof(XNTFSData));
			}
		}
	}

    /* GRR:  worry about return value later */
    return 0;
}
/* end function store_info() */


/* ===========================================================================
*    Function extract_or_test_member() *
* return PK-type error code or PK_COOL.
* direct: PK_DISK (open error or full), PK_ERR, PK_WARN, PK_MEM3
* indirect caused by unshrink:
* */
int UnzOpr::extract_or_test_member(void)
{
    register int b;
    int ucb;      // user_cb result
	int r, error = PK_COOL;

    if (Verbose < 0)
        Notify(ITRACE, _T("Start extract_or_test_member: %s"), ffilename.c_str());

    /*---------------------------------------------------------------------------
    *    Initialize variables, buffers, etc.
    *---------------------------------------------------------------------------*/
    fbits_left = 0;

    fbitbuf = 0L;

    /* unreduce and unshrink only */
    fzipeof = 0;

    fnewfile = true;
    fcrc32val = CRCVAL_INITIAL;


	if (ftflag)
	{
		// if test desired
		if (Verbose)
			Notify(0, _T("Testing %s"), ffilename.c_str());
	}
	else
		if ((r = open_outfile()) != 0)
			return r;
	/*---------------------------------------------------------------------------
	*    Unpack the file.
	*---------------------------------------------------------------------------*/
	if (Verbose < 0)
		Notify(ITRACE, _T("unpack the file"));

	defer_leftover_input(); /* so NEXTBYTE bounds check will work */

	fMax_Write = flrec.ucsize; 	// restrict extraction length
	if (fMax_Write)
	{
		switch (flrec.compression_method)
		{

			case STORED:
				foutptr = Slide;
				foutcnt = 0L;

				while ((b = NEXTBYTE) != EOF && !fdisk_full)
				{
					* foutptr++ = (uch)b;

					if (++foutcnt == wsize)
					{
						// EWE: wsize = 32K
						flush(Slide, foutcnt, 0);
						foutptr = Slide;
						foutcnt = 0L;

						if (Abort_Flag)
						{
							/* v1.6026 */
							CloseOut();
							undefer_input();
							Fatal(DZ_ERM_ABORT, 0);
						}
					}
				}

				if (foutcnt) /* flush final (partial) buffer */
					flush(Slide, foutcnt, 0);
				break;

			case IMPLODED:
				if (((r = explode()) != 0) && r != 5)
				{
					/* treat 5 specially */
					Notify(0, _T("Error unzipping files"));
					error = PK_ERR;
				}

				if (r == 5)
				{
					int warning = ((ulg)fused_csize <= flrec.csize);
					Notify(0, _T("Error unzipping files"));
					error = warning ? PK_WARN : PK_ERR;
				}

				break;

			case DEFLATED:
			case ENHDEFLATED:
				if ((r = inflate(flrec.compression_method == ENHDEFLATED)) != 0)
				{
					unsigned int zerr = DZ_ERR(r);
					if (zerr != DZ_ERR(PK_WARN) &&
						(zerr == DZ_ERR(PK_BADERR) || zerr == DZ_ERR(PK_NOZIP) ||
						zerr == DZ_ERR(PK_FIND) || zerr == DZ_ERR(PK_EOF)))
//						if (r != 1 && r & 0x09) // RP 1.7
					{
						/* user want to cancel operation v 1.6026 */
						CloseOut();
						undefer_input();
						return r;
					}

					Notify(0, _T("Error unzipping files"));

					error = PK_ERR;
				}
				break;

			default:
				/* should never get to this point */
				if (Verbose)// < 0)
					Notify(ITRACE, _T("should NEVER get here"));

				Notify(0, _T("Error unzipping files - unknown method"));

				/* close and delete file before return? */
				undefer_input();
				CloseOut();  // finished the file

				return PK_WARN;
		}

		/* end switch (compression method) */
		if (fdisk_full)
		{
			/* set by flush() */
			if (fdisk_full > 1)
			{
				undefer_input();
				return PK_DISKFULL;
			}

			error = PK_WARN;
		}

		if (error != PK_COOL && Verbose)
			Notify(ITRACE, _T("had an error of %d before closing file"), error);

//		CloseOut();  // finished the file
	}
    // RP 20110107
    undefer_input();
    CloseOut();  // finished the file

	if (PK_ERR_NOWARN(error))
	{
		/* don't print redundant CRC error if error already */
		undefer_input();
		return error;
	}

	if (Verbose < 0)
	{
		Notify(ITRACE, _T("After extraction, fcrc32val = %08X"), fcrc32val);
		Notify(ITRACE, _T("File's CRC in local hdr = %08X"), flrec.crc32);
	}

	if (fcrc32val != flrec.crc32)
	{
        // Call the component with a request for what do with with this CRC error.
		CB->SetArg1((long)fcrc32val);
		CB->SetArg2(flrec.crc32);
		CB->SetMsg(ffilename);
		ucb = CB->UserCB(zacCRCError);

		if (ucb == CALLBACK_UNHANDLED)
			ucb = CALLBACK_2;     // warn

        if (ucb == CALLBACK_3)          // responded 'delete'
        {
            DeleteFile(ffilename.c_str());
            error = PK_ERR;
        }

        if (ucb == CALLBACK_2)          // responded 'warn'
        {
            /* if quiet enough, we haven't output the filename yet:  do it */
			Notify(0, _T("After extraction, file %s had a CRC error"), ffilename.c_str());
			if (Encrypted)
				Notify(0, _T("May be Bad Password for file: %s"), ffilename.c_str());
			error = PK_WARN;
        }
    }

    undefer_input();

    if (error != PK_COOL && Verbose < 0)
        Notify(ITRACE, _T("extract_or_test_member returning error: %d"), error);

    return error;
}
/* end function extract_or_test_member() */


/*---------------------------------------------------------------------------
* Close the file and set its date and time (not necessarily in that order),
* and make sure the CRC checked out OK.  Logical-AND the CRC for 64-bit
* machines (redundant on 32-bit machines).
*---------------------------------------------------------------------------*/
void UnzOpr::CloseOut(void)
{
    if (!ftflag) /* don't close NULL file or stdout */
		close_outfile();
}





