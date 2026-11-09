     H*====================================================================*
     H*
     H*  Program ID        -  ZCM0010
     H*  Application Name  - 日付の妥当性検査
     H*  Author            -  T.Sannohe     (Riken Vitamin Co.,Ltd.)
     H*  Date Generation   -  JUL 28, 2004  (Ver 1.0)
     H*
     H*  Remark リターン(p_oReturn)
     H*                   ..."OK   "日付が妥当である時
     H*                   ..."ERR01"日付がセットされていない時
     H*                   ..."ERR02"月の妥当性に問題のある時
     H*                   ..."ERR03"日の妥当性に問題のある時
     H*
     H*  ----------------------------------------------------------------
     H*  Date Generation   -  xxx 00, 200x  (Ver 1.1)  X.Xxxxxxx
     H*                    -  xxx 00, 200x  (Ver 1.2)  X.Xxxxxxx
     H*
     H*====================================================================*
     H DATEDIT(*YMD/)  debug
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
     D  w_yy1                  1      2  0
     D  w_yy2                  3      4  0
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
     C                   parm                    p_iDate           8 0
     C                   parm                    p_oReturn         5
     C
     C
     C*====================================================================*
     C*//(1.0.0)         Main Processing.
     C*====================================================================*
     C
     C*//検査
     C                   exsr      #Check
     C
     C
     C
     C                   setOn                                        LR
     C                   return
     C
     C
     C*====================================================================*
     C*//(1.1.0)         Check Routine
     C*====================================================================*
     C     #Check        begSr
     C
     C
     C*//転送（検査用のワークへ）
     C                   z-add     p_iDate       w_ymd
     C
     C
     C*//検査（日付がセットされていない時）
     C                   if        ( w_ymd = *zero )
     C                   movel     'ERR01'       p_oReturn
     C                   leaveSr
     C                   endIf
     C
     C
     C*//転送（年が２桁でセットされている時）
     C                   if        ( w_yy1 = *zero )
     C                   if        ( w_yy2 < 90    )
     C                   z-add     20            w_yy1
     C                   else
     C****  t.s          z-add     19            w_yy1
     C                   z-add     20            w_yy1
     C                   endIf
     C                   endIf
     C
     C                   z-add     w_ymd         p_iDate
     C
     C
     C*//検査（月の検査）
     C                   if        ( w_mm < 01 ) or ( w_mm > 12 )
     C                   movel     'ERR02'       p_oReturn
     C                   leaveSr
     C                   endIf
     C
     C
     C*//検査（日の検査）
     C
     C*                  ---------------------------------*
     C                   if        ( w_mm = 01 ) or ( w_mm = 03 ) or
     C                             ( w_mm = 05 ) or ( w_mm = 07 ) or
     C                             ( w_mm = 08 ) or ( w_mm = 10 ) or
     C                             ( w_mm = 12 )
     C                   if        ( w_dd < 01 ) or ( w_dd > 31 )
     C                   movel     'ERR03'       p_oReturn
     C                   leaveSr
     C                   endIf
     C                   endIf
     C
     C*                  ---------------------------------*
     C                   if        ( w_mm = 04 ) or ( w_mm = 06 ) or
     C                             ( w_mm = 09 ) or ( w_mm = 11 )
     C                   if        ( w_dd < 01 ) or ( w_dd > 30 )
     C                   movel     'ERR03'       p_oReturn
     C                   leaveSr
     C                   endIf
     C                   endIf
     C
     C*                  ---------------------------------*
     C                   if        ( w_mm = 02 )
     C                   select
     C
     C                   when      ( w_dd < 01 ) or ( w_dd > 29 )
     C                   movel     'ERR03'       p_oReturn
     C                   leaveSr
     C
     C                   when      ( w_dd = 29 )
     C     w_yyyy        div       4             Shou              4 0
     C                   mvr                     Amari1            4 0
     C     w_yyyy        div       100           Shou
     C                   mvr                     Amari2            4 0
     C     w_yyyy        div       400           Shou
     C                   mvr                     Amari3            4 0
     C                   if        ( Amari1 <> 0 )
     C                   movel     'ERR03'       p_oReturn
     C                   leaveSr
     C                   endIf
     C                   if        ( Amari2 =  0 ) and ( Amari3 <> 0 )
     C                   movel     'ERR03'       p_oReturn
     C                   leaveSr
     C                   endIf
     C
     C                   endSl
     C                   endIf
     C
     C
     C*//検査（ＯＫ）
     C                   movel(p)  'OK'          p_oReturn
     C
     C
     C
     C     @Check        endSr
     C*====================================================================*
