
CLASS z_nf_janw_fact_single_tstcl DEFINITION FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS
.
*?﻿<asx:abap xmlns:asx="http://www.sap.com/abapxml" version="1.0">
*?<asx:values>
*?<TESTCLASS_OPTIONS>
*?<TEST_CLASS>z_Nf_Janw_Fact_Single_Tstcl
*?</TEST_CLASS>
*?<TEST_MEMBER>f_Cut
*?</TEST_MEMBER>
*?<OBJECT_UNDER_TEST>ZCL_JW_SINGLETON_INHRT_FACTORY
*?</OBJECT_UNDER_TEST>
*?<OBJECT_IS_LOCAL/>
*?<GENERATE_FIXTURE/>
*?<GENERATE_CLASS_FIXTURE/>
*?<GENERATE_INVOCATION/>
*?<GENERATE_ASSERT_EQUAL/>
*?</TESTCLASS_OPTIONS>
*?</asx:values>
*?</asx:abap>
  PRIVATE SECTION.
    DATA:
      f_cut TYPE REF TO zcl_jw_singleton_inhrt_fact_cp.  "class under test

    METHODS: get_some_instance FOR TESTING,
      check_singleton FOR TESTING.

ENDCLASS.       "z_Nf_Janw_Fact_Single_Tstcl


CLASS z_nf_janw_fact_single_tstcl IMPLEMENTATION.

  METHOD get_some_instance.

**********************************************************************
** GIVEN

    DATA: zlr_singleton TYPE REF TO zcl_jw_singleton_inhrt_super.

    LOOP AT zcl_jw_singleton_inhrt_fact_cp=>get_possible_classes( ) ASSIGNING FIELD-SYMBOL(<fs_possible_class>).

**********************************************************************
** When

      TRY.
          zlr_singleton = CAST #( zcl_jw_singleton_inhrt_factory=>get_some_instance( zip_singleton = <fs_possible_class>-classname ) ).
        CATCH zcx_simple_error.    "
          cl_abap_unit_assert=>assert_equals(
            EXPORTING
              act                  =  '1'   " Data object with current value
              exp                  =  '2'   " Data object with expected type
          ).
      ENDTRY.


**********************************************************************
** Then
      DATA(lo_ref_descr) = CAST cl_abap_refdescr( cl_abap_refdescr=>describe_by_data( zlr_singleton ) ).
      DATA(zlv_classname) = lo_ref_descr->get_referenced_type( )->get_relative_name( ).

      CASE <fs_possible_class>-classname.
        WHEN 'ZCL_JW_SINGLETON_INHRT_SUB001'.
          cl_abap_unit_assert=>assert_char_cp(
            EXPORTING
              act              =  zlr_singleton->do_something( )  " Text to match to EXP pattern
              exp              =  '*ONE*'   " Expected simple text pattern
          ).
        WHEN 'ZCL_JW_SINGLETON_INHRT_SUB002'.
          cl_abap_unit_assert=>assert_char_cp(
            EXPORTING
              act              =   zlr_singleton->do_something( )  " Text to match to EXP pattern
              exp              =   '*TWO*'   " Expected simple text pattern
          ).
        WHEN OTHERS.
          cl_abap_unit_assert=>assert_equals(
          EXPORTING
            act                  =  '1'   " Data object with current value
            exp                  =  '2'   " Data object with expected type
        ).
      ENDCASE.
    ENDLOOP.


  ENDMETHOD.

  METHOD check_singleton.

**********************************************************************
** GIVEN    some singletons
    DATA: zlr_singleton_one TYPE REF TO zcl_jw_singleton_inhrt_super,
          zlr_singleton_two TYPE REF TO zcl_jw_singleton_inhrt_super.

**********************************************************************
** When     we try to use a singleton multiple times
    TRY.
        zlr_singleton_one = CAST #( zcl_jw_singleton_inhrt_factory=>get_some_instance( zip_singleton = 'ZCL_JW_SINGLETON_INHRT_SUB001' ) ).
        zlr_singleton_two = CAST #( zcl_jw_singleton_inhrt_factory=>get_some_instance( zip_singleton = 'ZCL_JW_SINGLETON_INHRT_SUB001' ) ).
      CATCH zcx_simple_error.    "
        cl_abap_unit_assert=>assert_equals(
          EXPORTING
            act                  =  '1'   " Data object with current value
            exp                  =  '2'   " Data object with expected type
        ).
    ENDTRY.

**********************************************************************
** Then     we expect to see both instances to be the same, because
**          they should be a singleton.
    cl_abap_unit_assert=>assert_equals(
    EXPORTING
      act                  =  zlr_singleton_one   " Data object with current value
      exp                  =  zlr_singleton_two   " Data object with expected type
  ).

  ENDMETHOD.


ENDCLASS.
