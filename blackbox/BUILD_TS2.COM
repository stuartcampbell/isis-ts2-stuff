$!
$ SET DEF USER$DISK:[SIC.TS2.BLACKBOX]
$!
$ WRITE SYS$OUTPUT "Compiling TS2 files..."
$ WRITE SYS$OUTPUT "Doing...TS2_COUPLED_GROOVED_V1_1"
$ FOR TS2_COUPLED_GROOVED_V1_1.F90
$ WRITE SYS$OUTPUT "Doing...TS2_COUPLED_UNGROOVED_V1_1"
$ FOR TS2_COUPLED_UNGROOVED_V1_1.F90
$ WRITE SYS$OUTPUT "Doing...TS2_DECOUPLED_BROAD_V1_1"
$ FOR TS2_DECOUPLED_BROAD_V1_1.F90
$ WRITE SYS$OUTPUT "Doing...TS2_DECOUPLED_SHARP_V1_1"
$ FOR TS2_DECOUPLED_SHARP_V1_1.F90
$!
$ WRITE SYS$OUTPUT "Finished."
