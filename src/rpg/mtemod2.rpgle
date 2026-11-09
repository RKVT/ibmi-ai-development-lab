     H*====================================================================*
     H*
     H*  Program ID        -  MTEMOD2
     H*  Application Name  - 方式Ｍ保守（詳細）
     H*  Author            -  H.Kubo        (Riken Vitamin Co.,Ltd.)
     H*  Date Generation   -  JUN 15, 2010  (Ver 1.0)
     H*
     H*  Remark  ｎｎｎｎｎｎｎｎｎｎｎｎｎｎｎｎｎｎｎｎｎｎｎｎｎｎ
     H*          ｎｎｎｎｎｎｎｎｎｎｎｎｎｎｎｎｎｎｎｎｎｎｎｎｎｎ
     H*
     H*  ----------------------------------------------------------------
     H*  Date Generation   -  xxx 00, 200x  (Ver 1.1)  X.Xxxxxxx
     H*                    -  xxx 00, 200x  (Ver 1.2)  X.Xxxxxxx
     H*
     H*====================================================================*
     H DATEDIT(*YMD/)  debug
     F*====================================================================*
     FMTEMOD2D  CF   E             WORKSTN
     FMKJO01    IF   E           K Disk
     FMTRM01    IF   E           K Disk
     FMMOD01    UF A E           K Disk    commit
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
     D  wDateTime              1     14  0
     D  wTime                  1      6  0
     D  wDate                  7     14  0
     D
     D
     C*====================================================================*
     C*//                Parameter List
     C*====================================================================*
     C
     C     *entry        plist
     C                   parm                    p_iOption         2 0
     C                   parm                    p_iKJcd           2 0
     C                   parm                    p_iKJnm           8
     C                   parm                    p_iMode           2
     C                   parm                    p_oReturn         5
     C
     C*//工場Ｍ検索
     C     p_@MTKKJO     plist
     C                   parm                    p_oKJcd           2 0
     C                   parm                    p_oKJnm          26
     C                   parm                    p_oKJnmS          8
     C                   parm                    p_oReturnMKJO     5
     C
     C*//実行管理
     C     p_@ZCC2020    plist
     C                   parm                    p_iProgm         10            iプログラム
     C                   parm                    p_iPhase          3            iフェーズ
     C                   parm                    p_oExclude        1            o排他？
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
     C*//                Main Processing.
     C*====================================================================*
     C
     C                   exsr      #FirstProcess
     C
     C     w_Loop        douEq     'End    '
     C     w_Loop        casEq     'Screen '     #Screen
     C     w_Loop        casEq     'Update '     #Update
     C                   endCs
     C                   endDo
     C
     C
     C*              //(実行管理＠終了)
     C                   if        ( p_iOption <> 05 )
     C                   movel     wProgm        p_iProgm                       iプログラム
     C                   movel     'END'         p_iPhase                       iフェーズ
     C                   movel     *blank        p_oExclude                     o排他？
     C                   call      'ZCC2020'     p_@ZCC2020
     C                   endIf
     C
     C
     C                   commit
     C                   seton                                        LR
     C                   return
     C
     C*====================================================================*
     C*//(1.0.0)         First Processing.
     C*====================================================================*
     C     #FirstProcess begSr
     C
     C
     C*              //(実行管理＠開始)
     C                   if        ( p_iOption <> 05 )
     C                   movel     wProgm        p_iProgm                       iプログラム
     C                   movel     'STR'         p_iPhase                       iフェーズ
     C                   movel     *blank        p_oExclude                     o排他？
     C                   call      'ZCC2020'     p_@ZCC2020
     C                   if        ( p_oExclude = *on )
     C                   movel     'End    '     w_Loop
     C                   leaveSr
     C                   endIf
     C                   endIf
     C
     C
     C*//初期
     C                   clear                   dFMT
     C
     C                   movel     wProgm        dProgm
     C
     C
     C                   select
     C     p_iOption     whenEq    01
     C                   movel     *on           *in31
     C                   movel     '(作成)'    dModew
     C                   movel     'Entry  '     dModex            7
     C     p_iOption     whenEq    02
     C                   movel     *on           *in32
     C                   movel     '(編集)'    dModew
     C                   movel     'Change '     dModex
     C     p_iOption     whenEq    03
     C                   movel     *on           *in33
     C                   movel     '( ﾃﾍﾟｰ )'    dModew
     C                   movel     'Copy   '     dModex
     C     p_iOption     whenEq    04
     C                   movel     *on           *in34
     C                   movel     '(削除)'    dModew
     C                   movel     'Cancel '     dModex
     C     p_iOption     whenEq    05
     C                   movel     *on           *in35
     C                   movel     '(表示)'    dModew
     C                   movel     'Display'     dModex
     C                   endSl
     C
     C
     C*//読む（端末Ｍ）
     C                   movel     wUser         kUser
     C                   movel     wWsid         kJob
     C     k_MTRM01      chain     MTRMR                              90
     C   90              movel     *blank        kJob
     C   90k_MTRM01      chain     MTRMR                              90
     C
     C
     C*//読む（方式Ｍ）
     C                   if        ( dModex = 'Change ' ) or
     C                             ( dModex = 'Cancel ' )
     C
     C                   z-add     p_iKJcd       kKJcd
     C                   movel     p_iMode       kMode
     C     k_MMOD01      chain     MMODR                              90
     C                   if        ( *in90  = *on )
     C                   movel     'ERR01'       p_oReturn
     C                   movel     'End    '     w_Loop
     C                   leaveSr
     C                   else
     C
     C*              //(他の端末でメンテナンス中はエラー)
     C                   if        ( MMODJOB  <> *blank ) or                    ジョブ
     C                             ( MMODJOBN <> *blank )                       ジョブ№
     C                   movel     'ERR02'       p_oReturn
     C                   movel     'End    '     w_Loop
     C                   leaveSr
     C                   endIf
     C                   z-add     MMODKJCD      dKJcd                          工場ｺｰﾄﾞ
     C                   movel     MMODMODE      dMode                          方式
     C                   movel     MMODNAME      dName                          方式名
     C                   z-add     MMODNONE      dNone                          ノンリスト
     C                   z-add     MMODWRDT      dWRDT                          作成日付
     C                   z-add     MMODWRTM      dWRTM                          作成時刻
     C                   movel     MMODWRWS      dWRWS                          作成W/S
     C                   movel     MMODWRPG      dWRPG                          作成PGM
     C                   z-add     MMODUDDT      dUDDT                          更新日付
     C                   z-add     MMODUDTM      dUDTM                          更新時刻
     C                   movel     MMODUDWS      dUDWS                          更新W/S
     C                   movel     MMODUDPG      dUDPG                          更新PGM
     C                   movel     wWsid         MMODJOB                        ジョブ
     C                   movel     wJobNo        MMODJOBN                       ジョブ№
     C                   update    MMODR
     C                   commit
     C
     C     dKJcd         chain     MKJOR                              90
     C  n90              movel     MKJOKJNM      dKJnm
     C   90              movel     *blank        dKJnm
     C
     C                   endIf
     C                   endIf
     C
     C
     C                   if        ( dModex = 'Copy   ' ) or
     C                             ( dModex = 'Display' )
     C
     C                   z-add     p_iKJcd       kKJcd
     C                   movel     p_iMode       kMode
     C     k_MMOD01      chain(n)  MMODR                              90
     C                   if        ( *in90  = *on )
     C                   movel     'ERR01'       p_oReturn
     C                   movel     'End    '     w_Loop
     C                   leaveSr
     C                   else
     C
     C                   z-add     MMODKJCD      dKJcd                          工場ｺｰﾄﾞ
     C                   movel     MMODMODE      dMode                          方式
     C                   movel     MMODNAME      dName                          方式名
     C                   z-add     MMODNONE      dNone                          ノンリスト
     C                   z-add     MMODWRDT      dWRDT                          作成日付
     C                   z-add     MMODWRTM      dWRTM                          作成時刻
     C                   movel     MMODWRWS      dWRWS                          作成W/S
     C                   movel     MMODWRPG      dWRPG                          作成PGM
     C                   z-add     MMODUDDT      dUDDT                          更新日付
     C                   z-add     MMODUDTM      dUDTM                          更新時刻
     C                   movel     MMODUDWS      dUDWS                          更新W/S
     C                   movel     MMODUDPG      dUDPG                          更新PGM
     C
     C     dKJcd         chain     MKJOR                              90
     C  n90              movel     MKJOKJNM      dKJnm
     C   90              movel     *blank        dKJnm
     C
     C                   endIf
     C                   endIf
     C
     C
     C                   if        ( dModex = 'Entry  ' )
     C                   z-add     p_iKJcd       dKJcd
     C                   movel     p_iKJnm       dKJnm
     C                   endif
     C
     C
     C*//制御（画面のルーティンへ）
     C                   select
     C                   when      ( dModex = 'Entry  ' )
     C                   z-add     09            dLine
     C                   z-add     60            dColumn
     C                   when      ( dModex = 'Change ' )
     C                   z-add     11            dLine
     C                   z-add     60            dColumn
     C                   when      ( dModex = 'Copy   ' )
     C                   z-add     09            dLine
     C                   z-add     60            dColumn
     C                   other
     C                   z-add     01            dLine
     C                   z-add     01            dColumn
     C                   endSl
     C
     C                   movel     'Screen '     w_Loop            7
     C
     C
     C     @FirstProcess endSr
     C*====================================================================*
     C*//(2.0.0)         Screen Routine
     C*====================================================================*
     C     #Screen       begSr
     C
     C
     C*//表示（画面）
     C                   write     dFNC
     C                   exfmt     dFMT
     C                   movea     '0000000000'  *in(40)
     C                   movea     '0000000000'  *in(50)
     C                   movel     *blank        dMsgText
     C
     C                   z-add     *zero         dLine
     C                   z-add     *zero         dColumn
     C
     C
     C*              //(実行管理＠表示後)
     C                   if        ( p_iOption <> 05 )
     C
     C                   movel     wProgm        p_iProgm                       iプログラム
     C                   movel     'SCR'         p_iPhase                       iフェーズ
     C                   movel     *blank        p_oExclude                     o排他？
     C                   call      'ZCC2020'     p_@ZCC2020
     C
     C                   if        ( p_oExclude = *on )
     C                   if        ( dModex = 'Change ') or
     C                             ( dModex = 'Cancel ')
     C                   z-add     p_iKJcd       kKJcd
     C                   movel     p_iMode       kMode
     C     k_MMOD01      chain     MMODR                              90
     C                   if        ( *in90 = *off )
     C                   movel     *blank        MMODJOB                        ジョブ
     C                   movel     *blank        MMODJOBN                       ジョブ№
     C                   update    MMODR
     C                   commit
     C                   endIf
     C                   endIf
     C                   movel     *blank        p_oReturn
     C                   movel     'End    '     w_Loop
     C                   leaveSr
     C                   endIf
     C
     C                   endIf
     C
     C
     C*//Ｆ３（終了）
     C   03              Do
     C
     C                   if        ( wNotice <> *blank )
     C                   movel     *blank        wNotice
     C                   else
     C                   if        ( dModex = 'Change ') or
     C                             ( dModex = 'Cancel ')
     C                   z-add     p_iKJcd       kKJcd
     C                   movel     p_iMode       kMode
     C     k_MMOD01      chain     MMODR                              90
     C                   if        ( *in90 = *off )
     C                   movel     *blank        MMODJOB                        ジョブ
     C                   movel     *blank        MMODJOBN                       ジョブ№
     C                   update    MMODR
     C                   commit
     C                   endIf
     C                   endIf
     C                   movel     *blank        p_oReturn
     C                   movel     'End    '     w_Loop
     C                   endIf
     C
     C                   leaveSr
     C                   endDo
     C
     C
     C*//Ｆ４（検索）
     C   04              do
     C
     C                   if        ( dModex =  'Copy   ' )
     C*                  ---------------------------------*
     C*              //(工場検索)
     C*                  ---------------------------------*
     C                   if        ( dFIELD =  'DKJCD  ' )
     C                   z-add     dKJcd         p_oKJcd
     C                   movel     *blank        p_oKJnm
     C                   movel     *blank        p_oKJnmS
     C                   movel     *blank        p_oReturnMKJO
     C                   call      'MTKKJO'      p_@MTKKJO
     C                   if        ( p_oReturnMKJO = 'OK' )
     C                   z-add     p_oKJcd       dKJcd
     C                   movel     p_oKJnm       dKJnm
     C                   endIf
     C                   movel     *blank        wNotice
     C                   z-add     dLine#        dLine
     C                   z-add     dColumn#      dColumn
     C                   endIf
     C
     C                   endIf
     C
     C*                  ---------------------------------*
     C                   z-add     dLine#        dLine
     C                   z-add     dColumn#      dColumn
     C                   leaveSr
     C
     C                   endDo
     C
     C
     C*//検査（工場）
     C                   if        ( dModex = 'Copy   ')
     C
     C                   if        ( dKJcd = *zero )
     C                   movel     *on           *in41
     C                   movel     'ENT1001'     p_iMsgID
     C                   exsr      #MsgDisplay
     C                   movel     *blank        wNotice
     C                   leaveSr
     C                   endIf
     C
     C     dKJcd         chain     MKJOR                              90
     C                   if        ( *in90 = *on )
     C                   movel     *blank        dKJnm
     C                   movel     *on           *in41
     C                   movel     'EXT1011'     p_iMsgID
     C                   exsr      #MsgDisplay
     C                   movel     *blank        wNotice
     C                   leaveSr
     C                   else
     C                   movel     MKJOKJNM      dKJnm
     C                   endIf
     C
     C                   endIf
     C
     C
     C*//検査（方式）
     C                   if        ( dModex = 'Entry  ') or
     C                             ( dModex = 'Copy   ')
     C
     C                   if        ( dMode = *blank )
     C                   movel     *on           *in42
     C                   movel     'ENT1001'     p_iMsgID
     C                   exsr      #MsgDisplay
     C                   movel     *blank        wNotice
     C                   leaveSr
     C                   endIf
     C
     C                   z-add     dKJcd         kKJcd
     C                   movel     dMode         kMode
     C     k_MMOD01      chain(n)  MMODR                              90
     C                   if        ( *in90 = *off )
     C                   movel     *on           *in42
     C                   movel     'EXT1012'     p_iMsgID
     C                   exsr      #MsgDisplay
     C                   movel     *blank        wNotice
     C                   leaveSr
     C                   endIf
     C
     C                   endIf
     C
     C
     C*//検査（方式名）
     C                   if        ( dModex = 'Entry  ') or
     C                             ( dModex = 'Change ') or
     C                             ( dModex = 'Copy   ')
     C
     C                   if        ( %subst(dName:2:20) = *blank )
     C                   movel     *on           *in43
     C                   movel     'ENT1001'     p_iMsgID
     C                   exsr      #MsgDisplay
     C                   movel     *blank        wNotice
     C                   leaveSr
     C                   endIf
     C
     C                   endIf
     C
     C
     C*//検査（ノンリスト）
     C                   if        ( dModex = 'Entry  ') or
     C                             ( dModex = 'Change ') or
     C                             ( dModex = 'Copy   ')
     C
     C                   if        ( dNone <> 0 ) and ( dNone <> 1 )
     C                   movel     *on           *in44
     C                   movel     'LTR2101'     p_iMsgID
     C                   exsr      #MsgDisplay
     C                   movel     *blank        wNotice
     C                   leaveSr
     C                   endIf
     C
     C                   endIf
     C
     C
     C*//制御（Change Flagがオンの時はwNoticeをクリアする）
     C                   if        ( *in81 = *on )
     C                   movel     *blank        wNotice
     C                   endIf
     C
     C
     C*//表示（更新の確認メッセージ）
     C                   if        ( wNotice = *blank )
     C                   z-add     dLine#        dLine
     C                   z-add     dColumn#      dColumn
     C   31              movel     'GID0207'     p_iMsgID
     C   32              movel     'GID0208'     p_iMsgID
     C   33              movel     'GID0207'     p_iMsgID
     C   34              movel     'GID0209'     p_iMsgID
     C   35              movel     'GID0206'     p_iMsgID
     C                   exsr      #MsgDisplay
     C                   write     dFNC
     C                   movel     '*on'         wNotice           3
     C                   else
     C                   movel     *blank        wNotice
     C                   movel     'Update '     w_Loop
     C                   endIf
     C
     C
     C     @Screen       endSr
     C*====================================================================*
     C*//(3.0.0)         Update Routine
     C*====================================================================*
     C     #Update       begSr
     C
     C
     C*//更新（作成・編集・コピー・削除・表示）
     C                   select
     C     dModex        whenEq    'Entry  '
     C                   exsr      #updEntry
     C     dModex        whenEq    'Change '
     C                   exsr      #updChange
     C     dModex        whenEq    'Copy   '
     C                   exsr      #updCopy
     C     dModex        whenEq    'Cancel '
     C                   exsr      #updCancel
     C     dModex        whenEq    'Display'
     C                   movel     *blank        p_oReturn
     C                   movel     'End    '     w_Loop
     C                   endSl
     C
     C
     C     @Update       endSr
     C*====================================================================*
     C*//(3.1.0)         Update Routine  ( Entry  )
     C*====================================================================*
     C     #updEntry     begSr
     C
     C
     C*//読む（方式Ｍ）
     C                   z-add     dKJcd         kKJcd
     C                   movel     dMode         kMode
     C     k_MMOD01      chain(n)  MMODR                              90
     C                   if        ( *in90 = *off )
     C                   movea     '1'           *in(42)
     C                   movel     'EXT1012'     p_iMsgID
     C                   exsr      #MsgDisplay
     C                   movel     'Screen '     w_Loop
     C                   leaveSr
     C                   endIf
     C
     C
     C*//更新（方式Ｍ）
     C                   clear                   MMODR
     C
     C                   movel     *blank        MMODDLT                        削除
     C                   z-add     dKJcd         MMODKJCD                       工場ｺｰﾄﾞ
     C                   movel     dMode         MMODMODE                       方式
     C                   movel     dName         MMODNAME                       方式名
     C
     C                   z-add     dNONE         MMODNONE                       ノンリスト
     C                   movel     *blank        MMODJOB                        ジョブ
     C                   movel     *blank        MMODJOBN                       ジョブ№
     C
     C                   time                    wDateTime
     C                   z-add     wDate         MMODWRDT                       作成日付
     C                   z-add     wTime         MMODWRTM                       作成時刻
     C                   movel     wWsid         MMODWRWS                       作成W/S
     C                   movel     wProgm        MMODWRPG                       作成PGM
     C                   z-add     wDate         MMODUDDT                       更新日付
     C                   z-add     wTime         MMODUDTM                       更新時刻
     C                   movel     wWsid         MMODUDWS                       更新W/S
     C                   movel     wProgm        MMODUDPG                       更新PGM
     C
     C                   write     MMODR
     C                   commit
     C
     C
     C*//処理（終了）
     C                   z-add     dKJcd         p_iKJcd
     C                   movel     dMode         p_iMode
     C                   movel(p)  'OK'          p_oReturn
     C                   movel     'End    '     w_Loop
     C
     C
     C     @updEntry     endSr
     C*====================================================================*
     C*//(3.2.0)         Update Routine  ( Change )
     C*====================================================================*
     C     #updChange    begSr
     C
     C
     C*//読む（方式Ｍ）
     C                   z-add     dKJcd         kKJcd
     C                   movel     dMode         kMode
     C     k_MMOD01      chain     MMODR                              90
     C                   if        ( *in90 = *on )
     C                   movea     '11'          *in(41)
     C                   movel     'EXT1011'     p_iMsgID
     C                   exsr      #MsgDisplay
     C                   movel     'Screen '     w_Loop
     C                   leaveSr
     C                   endIf
     C
     C
     C*//更新（方式Ｍ）
     C                   movel     *blank        MMODDLT                        削除
     C                   z-add     dKJcd         MMODKJCD                       工場ｺｰﾄﾞ
     C                   movel     dMode         MMODMODE                       方式
     C                   movel     dName         MMODNAME                       方式名
     C
     C                   z-add     dNONE         MMODNONE                       ノンリスト
     C                   movel     *blank        MMODJOB                        ジョブ
     C                   movel     *blank        MMODJOBN                       ジョブ№
     C
     C                   time                    wDateTime
     C                   z-add     wDate         MMODUDDT                       更新日付
     C                   z-add     wTime         MMODUDTM                       更新時刻
     C                   movel     wWsid         MMODUDWS                       更新W/S
     C                   movel     wProgm        MMODUDPG                       更新PGM
     C
     C                   update    MMODR
     C                   commit
     C
     C
     C*//処理（戻り）
     C                   z-add     dKJcd         p_iKJcd
     C                   movel     dMode         p_iMode
     C                   movel(p)  'OK'          p_oReturn
     C                   movel     'End    '     w_Loop
     C
     C
     C     @updChange    endSr
     C*====================================================================*
     C*//(3.3.0)         Update Routine  ( Copy   )
     C*====================================================================*
     C     #updCopy      begSr
     C
     C
     C*//読む（方式Ｍ）
     C                   z-add     dKJcd         kKJcd
     C                   movel     dMode         kMode
     C     k_MMOD01      chain(n)  MMODR                              90
     C                   if        ( *in90 = *off )
     C                   movea     '11'          *in(41)
     C                   movel     'EXT1012'     p_iMsgID
     C                   exsr      #MsgDisplay
     C                   movel     'Screen '     w_Loop
     C                   leaveSr
     C                   endIf
     C
     C
     C*//更新（方式Ｍ）
     C                   clear                   MMODR
     C
     C                   movel     *blank        MMODDLT                        削除
     C                   z-add     dKJcd         MMODKJCD                       工場ｺｰﾄﾞ
     C                   movel     dMode         MMODMODE                       方式
     C                   movel     dName         MMODNAME                       方式名
     C
     C                   z-add     dNONE         MMODNONE                       ノンリスト
     C                   movel     *blank        MMODJOB                        ジョブ
     C                   movel     *blank        MMODJOBN                       ジョブ№
     C
     C                   time                    wDateTime
     C                   z-add     wDate         MMODWRDT                       作成日付
     C                   z-add     wTime         MMODWRTM                       作成時刻
     C                   movel     wWsid         MMODWRWS                       作成W/S
     C                   movel     wProgm        MMODWRPG                       作成PGM
     C                   z-add     wDate         MMODUDDT                       更新日付
     C                   z-add     wTime         MMODUDTM                       更新時刻
     C                   movel     wWsid         MMODUDWS                       更新W/S
     C                   movel     wProgm        MMODUDPG                       更新PGM
     C
     C                   write     MMODR
     C                   commit
     C
     C
     C*//処理（終了）
     C                   z-add     dKJcd         p_iKJcd
     C                   movel     dMode         p_iMode
     C                   movel(p)  'OK'          p_oReturn
     C                   movel     'End    '     w_Loop
     C
     C
     C     @updCopy      endSr
     C*====================================================================*
     C*//(2.3.4)         Update Routine  ( Cancel )
     C*====================================================================*
     C     #updCancel    begSr
     C
     C
     C*//読む（方式Ｍ）
     C                   z-add     dKJcd         kKJcd
     C                   movel     dMode         kMode
     C     k_MMOD01      chain     MMODR                              90
     C                   if        ( *in90 = *on )
     C                   movea     '11'          *in(41)
     C                   movel     'GID0417'     p_iMsgID
     C                   exsr      #MsgDisplay
     C                   movel     'Screen '     w_Loop
     C                   leaveSr
     C                   endIf
     C
     C
     C*//更新（方式Ｍ）
     C                   movel     'D'           MMODDLT                        削除
     C                   movel     *blank        MMODJOB                        ジョブ
     C                   movel     *blank        MMODJOBN                       ジョブ№
     C                   time                    wDateTime
     C                   z-add     wDate         MMODUDDT                       更新日付
     C                   z-add     wTime         MMODUDTM                       更新時刻
     C                   movel     wWsid         MMODUDWS                       更新W/S
     C                   movel     wProgm        MMODUDPG                       更新PGM
     C
     C                   update    MMODR
     C                   commit
     C
     C
     C*//処理（戻り）
     C                   z-add     dKJcd         p_iKJcd
     C                   movel     dMode         p_iMode
     C                   movel(p)  'OK'          p_oReturn
     C                   movel     'End    '     w_Loop
     C
     C
     C     @updCancel    endSr
     C*====================================================================*
     C*//(2.x.1.)        Message Display
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
