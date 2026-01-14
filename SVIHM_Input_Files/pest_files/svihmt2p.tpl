ptf $
*==============================================================================
* Texture2Par Main Input File
*==============================================================================

BEGIN OPTIONS
  INTERP_DIM 3D
  USE_MODEL_GSE
  MAX_OUTSIDE_DIST 500
  MAX_LOG_LENGTH 3
  KVME
  NO_TEX_BOUNDS
  ANISOTROPIC_SEARCH
  WRITE_DATASET_FILES
  WRITE_NODE_FILES
END OPTIONS

BEGIN FLOW_MODEL
  TYPE MODFLOW
  NAM_FILE      ../MODFLOW/SVIHM_t2p.nam
  TEMPLATE_FILE SVIHM_TEMPLATE.upw
  XOFFSET       499977.0
  YOFFSET       4571330.0
  ROTATION      0.0
  BLOCK_MODE
END FLOW_MODEL

BEGIN CLASSES
  Fine
  Mixed_Fine
  Sand
  Mixed_Coarse
  Very_Coarse
END CLASSES

BEGIN PREPROC
  MODE  RES2TEXTURE
  CLASS logrho
  SCALE_FILE interp_tex_dists.csv
  MULT_FILE kv_mult.csv
END PREPROC

BEGIN DATASET
  FILE     res_log.csv
END DATASET

BEGIN VARIOGRAMS
  # Structure Vtype  Nugget  Sill  Range_min Range_max Range_vert ang1 ang2 ang3  nnear
  CLASS logrho
           1    Sph     0.0 0.430      670.0     984.0       78.4  0.0  0.0  0.0    64
  # Structure Vtype  Nugget  Sill  Range_min Range_max ang1  nnear
  CLASS PilotPoints
           1    Sph    0.00   1.0      1.0E5     1.0E5  0.0  25
END VARIOGRAMS

BEGIN PP_LOCS
# ID          X           Y Zone
  1    511983.0   4599271.0    1
END PP_LOCS

BEGIN PP_PARAMETERS
  TYPE Global
# ID   KHp    KVp   STp
    1    $  KHp1    $  $  KVp1    $   1.0
  TYPE Aquifer
# ID    Class             Kmin             Kmax              Ss              Sy           Aniso     Kd
   1   Fine         $  KminFF1     $   $  KmaxFF1     $  $  SsFF1       $  $  SyFF1       $  $  AnisoFF1    $  0.007
   1   Mixed_Fine   $  KminMF1     $   $  KmaxMF1     $  $  SsMF1       $  $  SyMF1       $  $  AnisoMF1    $  0.007
   1   Sand         $  KminSC1     $   $  KmaxSC1     $  $  SsSC1       $  $  SySC1       $  $  AnisoSC1    $  0.007
   1   Mixed_Coarse $  KminMC1     $   $  KmaxMC1     $  $  SsMC1       $  $  SyMC1       $  $  AnisoMC1    $  0.007
   1   Very_Coarse  $  KminVC1     $   $  KmaxVC1     $  $  SsVC1       $  $  SyVC1       $  $  AnisoVC1    $  0.007
END PP_PARAMETERS

