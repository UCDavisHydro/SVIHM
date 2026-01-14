ptf $
* parameter data
KminFF1 = $ KminFF1          $
KminMF1 = KminFF1 * $ KminMF1_M       $
KminSC1 = KminMF1 * $ KminSC1_M       $
KminMC1 = KminSC1 * $ KminMC1_M       $
KminVC1 = KminMC1 * $ KminVC1_M       $
AnisoVC1 = $ AnisoVC1        $
AnisoMC1 = AnisoVC1 * $ AnisoMC1_M     $
AnisoSC1 = AnisoMC1 * $ AnisoSC1_M     $
AnisoMF1 = AnisoSC1 * $ AnisoMF1_M     $
AnisoFF1 = AnisoMF1 * $ AnisoFF1_M     $
SsFF1 = $ SsFF1        $
SsMF1 = SsFF1 * $ SsMF1_M      $
SsSC1 = SsMF1 * $ SsSC1_M      $
SsMC1 = SsSC1 * $ SsMC1_M      $
SsVC1 = SsMC1 * $ SsVC1_M      $
SySC1 = $ SySC1        $
SyMF1 = SySC1 * $ SyMF1_M      $
SyFF1 = SyMF1 * $ SyFF1_M      $
SyMC1 = SySC1 * $ SyMC1_M      $
SyVC1 = SyMC1 * $ SyVC1_M      $
KHp1 = $ KHp1           $
KVp1 = $ KVp1           $
KmaxFF1 = KminFF1 * 1.0
KmaxMF1 = KminMF1 * 1.0
KmaxSC1 = KminSC1 * 1.0
KmaxMC1 = KminMC1 * 1.0
KmaxVC1 = KminVC1 * 1.0
* template and model input files
..\svihmt2p.tpl .\preproc\svihm.t2p