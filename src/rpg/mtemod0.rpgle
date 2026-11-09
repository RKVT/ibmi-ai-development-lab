     H*====================================================================*
     H*
     H*  Program ID        -  MTEMOD0
     H*  Application Name  - 方式Ｍ保守（一覧）
     H*  Author            -  H.Kubo        (Riken Vitamin Co.,Ltd.)
     H*  Date Generation   -  JUN 15, 2010  (Ver 1.0)
     H*
     H*  Remark  ｎｎｎｎｎｎｎｎｎｎｎｎｎｎｎｎｎｎｎｎｎｎｎｎｎｎ
     H*          ｎｎｎｎｎｎｎｎｎｎｎｎｎｎｎｎｎｎｎｎｎｎｎｎｎｎ
     H*
     H*  ----------------------------------------------------------------
     H*  Date Generation   -  xxx 00, 20xx  (Ver 1.1)  X.Xxxxxxx
     H*                    -  xxx 00, 20xx  (Ver 1.2)  X.Xxxxxxx
     H*
     H*====================================================================*
     H DATEDIT(*YMD/)  debug
     F*====================================================================*
     FMTEMOD0D  CF   E             WORKSTN SFILE(dSFL:dRRN)
     FMMOD01    IF   E           K Disk
     FMKJO01    IF   E           K Disk
     FMTRM01    IF   E           K Disk
     D*====================================================================*
     D
     D*//システムフィールド
     D                SDS
     D  wProgm           *PROC
     D  wWsid                244    253
     D  wUser                254    263
     D  wJobNo               264    269
     D
     D
     D*//ＳＦの行数
     D wSFLSIZ         C                   const(17)
     D
     D
     D*//ＷＫ日付                   --------------------------------------
     D  w_Date         S               D
     D
     D                 DS
     D  w_ymd                  1      8  0
     D  w_yyyymm               1      6  0
     D  w_yyyy                 1      4  0
     D  w_mm                   5      6  0
     D  w_dd                   7      8  0
     D
     D                 DS
     D  w_hhmmss               1      6  0
     D  w_hhmm                 1      4  0
     D  w_ss                   5      6  0
     D
     D                 DS
     D  wDateTime              1     14  0
     D  wTime                  1      6  0
     D  wDate                  7     14  0
     D
     D
     D*// SubFileセット用ワーク     --------------------------------------
     D  sSubFileRec    DS
     D   sKJcd
     D   sMode
     D
     D  wSubFileStr    S                   like(sSubFileRec)
     D  wSubFileEnd    S                   like(sSubFileRec)
     D
     D  wKeyField      DS
     D   kKJcd                        2  0
     D   kMode                        2
     D
     D*                               --------------------------------------
     D dDspFields      DS
     D  dKJCD
     D  dMODE
     D wDspFields      S                   like(dDspFields)
     D
     D
     C*====================================================================*
     C*//                Parameter List
     C*====================================================================*
     C
     C*//方式Ｍ保守（詳細）
     C     p_@Execute    plist
     C                   parm                    p_iOption         2 0
     C                   parm                    p_iKJcd           2 0
     C                   parm                    p_iKJnm           8
     C                   parm                    p_iMode           2
     C                   parm                    p_oReturn         5
     C
     C
     C*//メッセージ
     C     p_@Message    plist
     C                   parm                    p_iMsgID          7
     C                   parm                    p_iMsgVar       130
     C                   parm                    p_oMsgText      130
     C
     C
     C*====================================================================*
     C*//                Key List
     C*====================================================================*
     C
     C*//Ｋｅｙ（方式Ｍ）
     C     k_MMOD01      klist
     C                   kfld                    kKJcd             2 0
     C                   kfld                    kMode             2
     C
     C*//Ｋｅｙ（端末Ｍ）
     C     k_MTRM01      klist
     C                   kfld                    kUser            10
     C                   kfld                    kJob             10
     C
     C
     C*====================================================================*
     C*//(1.0.0)         First Processing.
     C*====================================================================*
     C
     C*//初期
     C                   clear                   dCTL
     C                   movel     wProgm        dProgm
     C                   movel     *blank        dMsgText
     C                   movel     *off          *in27
     C                   movel     *on           *in28
     C
     C
     C*//読む（端末Ｍ）
     C                   movel     wUser         kUser
     C                   movel     wWsid         kJob
     C     k_MTRM01      chain     MTRMR                              90
     C   90              movel     *blank        kJob
     C   90k_MTRM01      chain     MTRMR                              90
     C                   if        ( *in90 = *off )
     C     MTRMJGYCD     chain     MKJOR                              90
     C                   if        ( *in90 = *off )
     C                   movel     *on           *in69
     C                   z-add     MKJOKJCD      dKJcd
     C                   z-add     MKJOKJCD      wKJcd             2 0
     C                   else
     C                   movel     *off          *in69
     C     *loval        setll     MMODR
     C                   read      MMODR                                  90
     C  n90              z-add     MMODKJCD      dKJcd
     C                   endIf
     C                   endIf
     C
     C
     C*//更新（ＳＦＬへ更新）
     C                   z-add     dKJcd         kKJcd
     C                   movel     *blank        kMode
     C     k_MMOD01      setll     MMODR
     C                   exsr      #SubFileWrt
     C
     C
     C*====================================================================*
     C*//(2.0.0)         Main Processing.
     C*====================================================================*
     C
     C                   movel     'Screen '     w_Loop            7
     C     w_Loop        douEq     'End    '
     C     w_Loop        casEq     'Screen '     #Screen
     C                   endCs
     C                   endDo
     C
     C
     C                   setOn                                        LR
     C                   return
     C
     C*====================================================================*
     C*//(2.1.0)         Screen Routine
     C*====================================================================*
     C     #Screen       begSr
     C
     C
     C*//表示（画面）
     C                   write     dFNC
     C                   movel     dDspFields    wDspFields
     C                   movel     *on           *in38
     C                   exfmt     dCTL
     C                   movel     *off          *in38
     C     dDspFields    comp      wDspFields                         8181      ≠
     C
     C                   movea     '0000000000'  *in(40)
     C                   movea     '0000000000'  *in(50)
     C                   z-add     0             dLine
     C                   z-add     0             dColumn
     C                   movel     *blank        dMsgText
     C
     C
     C*//Ｆ３（終了）
     C   03              do
     C                   movel     'End    '     w_Loop
     C                   leaveSr
     C                   endDo
     C
     C
     C*//検査（工場）
     C                   if        ( dKJcd =  *zero )
     C                   movel     *on           *in51
     C                   movel     'ENT1001'     p_iMsgID
     C                   exsr      #MsgDisplay
     C                   leavesr
     C                   endIf
     C
     C     dKJcd         chain     MKJOR                              90
     C                   if        ( *in90 = *off )
     C                   movel     MKJOKJNMS     dKJnmS
     C                   else
     C                   movel     *on           *in51
     C                   movel     'EXT1349'     p_iMsgID
     C                   exsr      #MsgDisplay
     C                   leavesr
     C                   endIf
     C
     C
     C*//更新？
     C   81              do        *hival
     C
     C                   z-add     dKJcd         kKJcd
     C                   movel     dMode         kMode
     C     k_MMOD01      setll     MMODR
     C
     C                   do        *hival
     C     kKJcd         reade     MMODR                                  90
     C                   if        ( *in90 = *on )
     C                   movel     'GID0419'     p_iMsgID
     C                   exsr      #MsgDisplay
     C                   z-add     dLine#        dLine
     C                   z-add     dColumn#      dColumn
     C                   leaveSr
     C                   endIf
     C
     C                   exsr      #SubFileSel
     C   99              iter
     C
     C     k_MMOD01      setll     MMODR
     C                   movel     *off          *in27
     C                   movel     *on           *in28
     C                   exsr      #SubFileWrt
     C                   movel     *blank        dMode
     C                   movel     dDspFields    wDspFields
     C                   leaveSr
     C
     C                   endDo
     C                   endDo
     C
     C
     C*//Ｆ５（最新表示）
     C   05              do
     C                   movel     wSubFileStr   wKeyField
     C     k_MMOD01      setll     MMODR
     C                   movel     *off          *in27
     C                   movel     *on           *in28
     C                   exsr      #SubFileWrt
     C                   leaveSr
     C                   endDo
     C
     C
     C*//Ｆ６（作成）
     C   06              do
     C                   z-add     01            p_iOption
     C                   z-add     dKJcd         p_iKJcd
     C                   movel     dKJnms        p_iKJnm
     C                   movel     *Blank        p_iMode
     C                   movel     *blank        p_oReturn
     C                   call      'MTEMOD2'     p_@Execute
     C                   if        ( p_oReturn = 'OK   ' )
     C                   movel     'GID0401'     p_iMsgID
     C                   exsr      #MsgDisplay
     C                   movel     wSubFileStr   wKeyField
     C     k_MMOD01      setll     MMODR
     C                   movel     *off          *in27
     C                   movel     *on           *in28
     C                   exsr      #SubFileWrt
     C                   leaveSr
     C                   endIf
     C                   z-add     dLine#        dLine
     C                   z-add     dColumn#      dColumn
     C                   endDo
     C
     C
     C*//前頁（PageUp）
     C   27              do
     C
     C                   movel     wSubFileStr   wKeyField
     C     k_MMOD01      setll     MMODR
     C
     C                   do        *hival
     C     kKJcd         readpe    MMODR                                  90
     C   90              leave
     C                   exsr      #SubFileSel
     C  n99              leave
     C                   endDo
     C                   if        ( *in90 = *on )
     C                   movel     'GID0101'     p_iMsgID
     C                   exsr      #MsgDisplay
     C                   z-add     dLine#        dLine
     C                   z-add     dColumn#      dColumn
     C                   leaveSr
     C                   endIf
     C
     C                   movel     wSubFileStr   wKeyField
     C     k_MMOD01      setll     MMODR
     C                   exsr      #SubFileWrt
     C
     C                   leaveSr
     C
     C                   endDo
     C
     C
     C*//次頁（PageDown）
     C   28              do
     C
     C                   movel     wSubFileEnd   wKeyField
     C     k_MMOD01      setgt     MMODR
     C
     C                   do        *hival
     C     kKJcd         reade     MMODR                                  90
     C   90              leave
     C                   exsr      #SubFileSel
     C  n99              leave
     C                   endDo
     C                   if        ( *in90 = *on )
     C                   movel     'GID0102'     p_iMsgID
     C                   exsr      #MsgDisplay
     C                   z-add     dLine#        dLine
     C                   z-add     dColumn#      dColumn
     C                   leaveSr
     C                   endIf
     C
     C                   movel     wSubFileEnd   wKeyField
     C     k_MMOD01      setgt     MMODR
     C                   exsr      #SubFileWrt
     C
     C                   leaveSr
     C
     C                   endDo
     C
     C
     C*//Ｆ２０（工場入力可）
     C   20              do
     C                   movel     *off          *in69
     C                   z-add     04            dLine
     C                   z-add     58            dColumn
     C                   leaveSr
     C                   endDo
     C
     C
     C*//検査（サブファイル）
     C                   exsr      #SubFileChk
     C   99              leaveSr
     C
     C
     C*//実行（作成・編集・コピー・削除・表示）
     C  n99              do
     C                   exsr      #SubFileExe
     C                   endDo
     C
     C
     C*//転送（カーソルのセット）
     C  n99              z-add     dLine#        dLine
     C  n99              z-add     dColumn#      dColumn
     C
     C
     C     @Screen       endSr
     C*====================================================================*
     C*//(2.1.1)         SubFile Writing Routine
     C*====================================================================*
     C     #SubFileWrt   begSr
     C
     C
     C*//初期
     C                   movel     *on           *in37
     C                   write     dCTL
     C                   movel     *off          *in37
     C
     C   27wSFLSIZ       add       1             dRRN              4 0
     C   28              z-add     *zero         dRRN
     C                   z-add     *zero         dRCDcount         4 0
     C                   z-add     *zero         wRRNcount         4 0
     C                   clear                   wSubFileStr
     C                   clear                   wSubFileEnd
     C
     C
     C*//読む（工場Ｍ）
     C     dKJcd         chain     MKJOR                              90
     C  n90              movel     MKJOKJNMS     dKJnmS
     C   90              movel     *blank        dKJnmS
     C
     C
     C*//更新（ＳＦＬへ）
     C                   do        *hival
     C
     C   27kKJcd         readpe    MMODR                                  90
     C   28kKJcd         reade     MMODR                                  90
     C
     C                   if        ( *in90 = *on   )
     C                   if        ( *in27 = *on   )  and
     C                             ( wRRNcount > 0 )  and
     C                             ( wRRNcount < wSFLSIZ )
     C                   movel     *on           *in37
     C                   write     dCTL
     C                   movel     *off          *in37
     C                   movel     *off          *in27
     C                   movel     *on           *in28
     C                   z-add     *zero         dRRN
     C                   z-add     *zero         wRRNcount
     C                   clear                   wSubFileStr
     C                   clear                   wSubFileEnd
     C                   z-add     dKJcd         kKJcd
     C                   movel     *blank        kMode
     C     k_MMOD01      setll     MMODR
     C                   iter
     C                   else
     C                   leave
     C                   endIf
     C                   endIf
     C
     C
     C                   exsr      #SubFileSel
     C   99              iter
     C
     C
     C   27              sub       1             dRRN
     C   28              add       1             dRRN
     C
     C
     C                   clear                   dSFL
     C
     C                   z-add     *zero         sOption
     C
     C                   z-add     MMODKJCD      sKJcd
     C                   movel     MMODMODE      sMode
     C                   movel     MMODNAME      sName
     C
     C                   if        ( MMODNONE = 1 )
     C                   movel     '**'          sNone
     C                   endIf
     C
     C                   write     dSFL
     C
     C
     C                   add       1             wRRNcount
     C
     C                   select
     C                   when      ( wRRNCount = 1 )
     C                   movel     sSubFileRec   wSubFileStr
     C                   movel     sSubFileRec   wSubFileEnd
     C                   when      ( *in27 = *on )
     C                   movel     sSubFileRec   wSubFileStr
     C                   when      ( *in28 = *on )
     C                   movel     sSubFileRec   wSubFileEnd
     C                   endSl
     C
     C                   if        ( wRRNcount = wSFLSIZ )
     C                   leave
     C                   endIf
     C
     C
     C                   endDo
     C
     C
     C*//検査（ＳＦＬへの書き出しがない場合）
     C                   if        ( wRRNcount = *zero )
     C                   z-add     1             dRRN
     C                   clear                   dSFL
     C                   movel     *on           *in40
     C                   write     dSFL
     C                   endIf
     C
     C
     C*//検査（続く...／終わり）
     C                   movel     *on           *in39
     C                   movel     wSubFileEnd   wKeyField
     C     k_MMOD01      setgt     MMODR
     C                   do        *hival
     C     kKjcd         reade     MMODR                                  90
     C   90              leave
     C                   exsr      #SubFileSel
     C   99              iter
     C                   movel     *off          *in39
     C                   endDo
     C
     C
     C*//転送
     C                   movel     *blank        dMode
     C
     C
     C*//検査（カーソル）
     C                   if        ( wRRNcount = *zero )
     C                   z-add     04            dLine
     C                   z-add     87            dColumn
     C                   else
     C                   z-add     08            dLine
     C                   z-add     45            dColumn
     C                   endIf
     C
     C
     C     @SubFileWrt   endSr
     C*====================================================================*
     C*//(2.1.2)         SubFile Data Select
     C*====================================================================*
     C     #SubFileSel   begSr
     C
     C
     C                   move      *on           *in99
     C
     C
     C*              //(絞込＠ノンリスト)
     C***                if        ( dNone    =  *zero )
     C***                if        ( MMODNONE <> *zero )
     C***                leavesr
     C***                endIf
     C***                endIf
     C
     C
     C                   move      *off          *in99
     C
     C
     C     @SubFileSel   endSr
     C*====================================================================*
     C*//(2.1.3)         SubFile Check
     C*====================================================================*
     C     #SubFileChk   begSr
     C
     C
     C*//初期
     C                   movel     *off          *in99
     C                   z-add     *zero         dRRN
     C                   z-add     *zero         wSelectCount      4 0
     C
     C
     C*//検査（ＳＦＬを読む）
     C                   do        *hival
     C
     C                   add       1             dRRN
     C     dRRN          chain     dSFL                               90
     C   90              leave
     C
     C                   if        ( sMode = *blank )
     C                   leave
     C                   endIf
     C
     C                   movea     '000000'      *in(41)
     C
     C
     C*              //(オプションの妥当性を検査)
     C                   if        ( sOption <> 0 ) and
     C                             ( sOption <> 2 ) and
     C                             ( sOption <> 3 ) and
     C                             ( sOption <> 4 ) and
     C                             ( sOption <> 5 )
     C                   movel     *on           *in41
     C  n99              movel     'LTR3014'     p_iMsgID
     C  n99              exsr      #MsgDisplay
     C  n99              movel     *on           *in99
     C                   endIf
     C
     C
     C*              //(他工場のレコードは編集不可)
     C                   if        ( sOption =  2 ) or
     C                             ( sOption =  4 )
     C                   if        ( sKJCD   <> wKJCD )
     C                   movel     *on           *in41
     C  n99              movel     'ENT6489'     p_iMsgID
     C  n99              exsr      #MsgDisplay
     C  n99              movel     *on           *in99
     C                   endIf
     C                   endIf
     C
     C
     C*              //(オプションの妥当性を検査)
     C**                 if        ( sOption = 4 )
     C**                 z-add     sWHcd         kWHcd
     C**                 movel     sLOC          kLOC
     C**   k_FZLD15      chain     FZLDR                              90
     C**                 if        ( *in90 = *off )
     C**                 movel     *on           *in41
     C**n99              movel     'ENT6465'     p_iMsgID
     C**n99              exsr      #MsgDisplay
     C**n99              movel     *on           *in99
     C**                 endIf
     C**                 endIf
     C**
     C
     C*              //(編集・コピー・削除を選択した時のレコード確認)
     C                   if        ( sOption  = 2 ) or
     C                             ( sOption  = 3 ) or
     C                             ( sOption  = 4 )
     C                   movel     sMode         kMode
     C     k_MMOD01      chain     MMODR                              90
     C                   if        ( *in90 = *on )
     C                   movel     *on           *in41
     C  n99              movel     'GID0418'     p_iMsgID
     C  n99              exsr      #MsgDisplay
     C  n99              movel     *on           *in99
     C                   else
     C                   if        ( MMODJOB  <> *blank ) or
     C                             ( MMODJOBN <> *blank )
     C                   movel(p)  MMODJOB       wMsgVar          16
     C                   move      MMODJOBN      wMsgVar
     C                   movel     *on           *in41
     C  n99              movel     'CND1102'     p_iMsgID
     C  n99              movel     wMsgVar       p_iMsgVar
     C  n99              exsr      #MsgDisplay
     C  n99              movel     *on           *in99
     C                   endIf
     C                   endIf
     C                   endIf
     C
     C
     C*              //(編集・コピー・削除・表示の件数をカウント)
     C                   if        ( sOption  = 2 ) or
     C                             ( sOption  = 3 ) or
     C                             ( sOption  = 4 )
     C                   add       1             wSelectCount
     C                   endIf
     C
     C*              //(編集・コピー・削除・表示の件数が２件以上はエラー)
     C                   if        ( sOption  = 2 ) or
     C                             ( sOption  = 3 ) or
     C                             ( sOption  = 4 )
     C                   if        ( wSelectCount >= 2 )
     C                   movel     *on           *in41
     C  n99              movel     'LTR1001'     p_iMsgID
     C  n99              exsr      #MsgDisplay
     C  n99              movel     *on           *in99
     C                   endIf
     C                   endIf
     C
     C
     C                   update    dSFL
     C
     C
     C                   endDo
     C
     C
     C     @SubFileChk   endSr
     C*====================================================================*
     C*//(2.1.4)         SubFile Execute
     C*====================================================================*
     C     #SubFileExe   begSr
     C
     C
     C*//初期
     C                   movel     *off          *in99
     C                   z-add     *zero         dRRN
     C                   z-add     *zero         wUpdateCount      4 0
     C
     C
     C*//検査（ＳＦＬを読む）
     C                   do        *hival
     C
     C                   add       1             dRRN
     C     dRRN          chain     dSFL                               90
     C   90              leave
     C
     C                   if        ( sMode = *blank )
     C                   leave
     C                   endIf
     C
     C
     C                   if        ( sOption <> 0 )
     C
     C*              //(方式Ｍ保守＠詳細を実行する)
     C                   z-add     sOption       p_iOption
     C                   z-add     sKJcd         p_iKJcd
     C                   movel     dKJnms        p_iKJnm
     C                   movel     sMode         p_iMode
     C                   movel     *blank        p_oReturn
     C                   call      'MTEMOD2'     p_@Execute
     C
     C                   z-add     *zero         sOption
     C                   update    dSFL
     C
     C
     C                   select
     C*
     C*                        //(マスターに存在しない時はエラー)
     C                   when      ( p_oReturn = 'ERR01' )
     C                   movel     'GID0417'     p_iMsgID
     C                   exsr      #MsgDisplay
     C
     C*                        //(他の端末でメンテナンス中はエラー)
     C                   when      ( p_oReturn = 'ERR02' )
     C                   movel     'CND1112'     p_iMsgID
     C                   exsr      #MsgDisplay
     C
     C*                        //(編集････更新されました)
     C                   when      ( p_oReturn = 'OK   ' ) and
     C                             ( p_iOption =  02     )
     C                   movel     'GID0402'     p_iMsgID
     C                   exsr      #MsgDisplay
     C
     C*                        //( ｺﾋﾟｰ ････作成されました)
     C                   when      ( p_oReturn = 'OK   ' ) and
     C                             ( p_iOption =  03     )
     C                   movel     'GID0401'     p_iMsgID
     C                   exsr      #MsgDisplay
     C
     C*                        //(削除････削除されました)
     C                   when      ( p_oReturn = 'OK   ' ) and
     C                             ( p_iOption =  04     )
     C                   movel     'GID0403'     p_iMsgID
     C                   exsr      #MsgDisplay
     C
     C                   endSl
     C
     C*               //(編集・コピー・削除・表示の更新件数をカウント)
     C                   if        ( p_oReturn = 'OK   ' )
     C                   add       1             wUpdateCount
     C                   endIf
     C
     C                   endIf
     C
     C
     C                   endDo
     C
     C
     C*//更新（編集・コピー・削除された時はＳＦＬを更新する）
     C                   if        ( wUpdateCount > *zero )
     C                   movel     wSubFileStr   wKeyField
     C     k_MMOD01      setll     MMODR
     C                   movel     *off          *in27
     C                   movel     *on           *in28
     C                   exsr      #SubFileWrt
     C                   endIf
     C
     C
     C     @SubFileExe   endSr
     C*====================================================================*
     C*//(2.x.0)         Message Display
     C*====================================================================*
     C     #MsgDisplay   begSr
     C
     C
     C                   call      'ZSM0030C'    p_@Message
     C
     C                   movel     p_oMsgText    dMsgText
     C                   movel     *blank        p_iMsgID
     C                   movel     *blank        p_iMsgVar
     C                   movel     *blank        p_oMsgText
     C
     C
     C     @MsgDisplay   endSr
     C*====================================================================*
