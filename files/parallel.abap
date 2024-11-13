*&---------------------------------------------------------------------*
*& Report RS_ABAP_TEST_PARALLEL
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
report RS_ABAP_TEST_PARALLEL.


parameters PERC type I.


class LCL_MAIN definition final.
  public section.
    types:
      T_GRADE type P length 5 decimals 2.

    class-methods START     importing P_PERCENTAGE type I.
    class-methods GET_GRADE importing P_PERCENTAGE type I optional
                            exporting P_GRADE      type T_GRADE
                                      P_TIME       type I.
endclass.


class LCL_SYNTAX_CHECK definition final.
  public section.
    interfaces IF_ABAP_PARALLEL.
    methods CONSTRUCTOR importing P_PROGRAM type PROGRAM.
    data:
      PROGRAM type PROGRAM.

endclass.

LCL_MAIN=>START( PERC ).


class LCL_MAIN implementation.

  method START ##NEEDED.
    GET_GRADE( exporting P_PERCENTAGE =  P_PERCENTAGE importing P_GRADE = data(L_GRADE) P_TIME = data(L_TIME) ).
    write: / L_GRADE, L_TIME.
  endmethod.

  method GET_GRADE.
    data:
      L_IN_TAB   type CL_ABAP_PARALLEL=>T_IN_INST_TAB,
      L_TIME_1   type I,
      L_TIME_2   type I,
      L_TIME_SEQ type I,
      L_TIME_PAR type I.

    if P_PERCENTAGE = 0.
      data(L_REF) = new CL_ABAP_PARALLEL( ).
    else.
      L_REF = new CL_ABAP_PARALLEL( P_PERCENTAGE = P_PERCENTAGE ).
    endif.

    clear L_IN_TAB.

    select NAME from TRDIR into @data(L_PROGRAM) where NAME like 'CL_ABAP_%'  and SUBC <> 'I'
                                                 order by NAME.
      append new LCL_SYNTAX_CHECK( L_PROGRAM ) to L_IN_TAB.
    endselect.

    get run time field L_TIME_1.

    L_REF->RUN_INST( exporting P_IN_TAB = L_IN_TAB ).

    get run time field L_TIME_2.

    L_TIME_PAR = L_TIME_2 - L_TIME_1.

    get run time field L_TIME_1.

    L_REF->RUN_INST( exporting P_IN_TAB = L_IN_TAB P_DEBUG = ABAP_TRUE ).

    get run time field L_TIME_2.

    L_TIME_SEQ = L_TIME_2 - L_TIME_1.

    P_GRADE =   L_TIME_SEQ / L_TIME_PAR.

    P_TIME  = L_TIME_SEQ.

  endmethod.

endclass.


class LCL_SYNTAX_CHECK implementation.

  method CONSTRUCTOR.
    SUPER->CONSTRUCTOR( ).
    PROGRAM = P_PROGRAM.
  endmethod.

  method IF_ABAP_PARALLEL~DO.

    data:
      L_MESSAGE type STRING   ##NEEDED,
      L_LINE    type I        ##NEEDED,
      L_WORD    type SYCHAR30 ##NEEDED.

    syntax-check for program PROGRAM
                message      L_MESSAGE
                line         L_LINE
                word         L_WORD.
  endmethod.

endclass.


class LCL_TEST definition final
                    for testing duration long risk level harmless
                    inheriting from CL_AUNIT_ASSERT.
  private section.
    methods TEST for testing.
endclass.


*----------------------------------------------------------------------*
*       CLASS LCL_TEST IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
class LCL_TEST implementation.
  method TEST.
    LCL_MAIN=>GET_GRADE( importing P_GRADE = data(L_GRADE) ).
    if L_GRADE < 2.
      CL_AUNIT_ASSERT=>FAIL( MSG = 'GRADE < 2' ).
    endif.
  endmethod.                    "TEST

endclass.                    "LCL_MAIN IMPLEMENTAT