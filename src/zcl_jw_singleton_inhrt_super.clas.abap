CLASS zcl_jw_singleton_inhrt_super DEFINITION ABSTRACT
  PUBLIC
  CREATE PROTECTED .

  PUBLIC SECTION.
    INTERFACES  zif_jw_singleton_inheritance.
    ALIASES:    do_something  FOR zif_jw_singleton_inheritance~do_something,
                get_instance  FOR zif_jw_singleton_inheritance~get_instance.

  PROTECTED SECTION.

  PRIVATE SECTION.
    ALIASES zgt_instances
      FOR zif_jw_singleton_inheritance~zgt_instances .


ENDCLASS.



CLASS ZCL_JW_SINGLETON_INHRT_SUPER IMPLEMENTATION.


  METHOD do_something.
    zrv_text = | returned from super |.
  ENDMETHOD.


  METHOD get_instance.
    "  https://answers.sap.com/questions/6203810/inherited-static-method-to-return-instance-of-sub-.html

    TRY. "Get the class-type of the requested object to be created.
        IF cl_abap_refdescr=>describe_by_data( zcv_instance )->kind = cl_abap_typedescr=>kind_ref.
          DATA(lo_ref_descr) = CAST cl_abap_refdescr( cl_abap_refdescr=>describe_by_data( zcv_instance ) ).
          DATA(zlv_classname) = lo_ref_descr->get_referenced_type( )->get_relative_name( ).
          "Check for existance
          READ TABLE zcl_jw_singleton_inhrt_factory=>get_possible_classes( ) WITH TABLE KEY classname = zlv_classname TRANSPORTING NO FIELDS.
          IF sy-subrc NE 0.  "Not a valid class to be instantiated.
            zcl_jw_singleton_inhrt_factory=>raise_exception( EXPORTING iv_msg = |Error casting & creating object|  ).
          ENDIF.

          DATA: zls_instance TYPE zif_jw_singleton_inheritance~zlty_instance.
          READ TABLE zgt_instances WITH KEY classname = zlv_classname INTO zls_instance .
          IF sy-subrc NE 0.
            DATA: zlr_instance TYPE REF TO zif_jw_singleton_inheritance.
            CREATE OBJECT zlr_instance TYPE (zlv_classname).

            zls_instance = VALUE zif_jw_singleton_inheritance~zlty_instance(
                classname = zlv_classname
                instance  = zlr_instance ).

            INSERT zls_instance INTO TABLE zgt_instances .
          ENDIF.

          zcv_instance ?= zls_instance-instance.
        ELSE.
          zcl_jw_singleton_inhrt_factory=>raise_exception( EXPORTING iv_msg = |Error : No class-definition given for an instance| ).
        ENDIF.

      CATCH zcx_simple_error INTO DATA(zlcx_error_lcl).
        RAISE EXCEPTION zlcx_error_lcl.
      CATCH cx_sy_move_cast_error INTO DATA(zlcx_move_cast).
        zcl_jw_singleton_inhrt_factory=>raise_exception( EXPORTING iv_msg = |Something went wrong with casting | ).
      CATCH cx_root INTO DATA(zlcx_root).
        zcl_jw_singleton_inhrt_factory=>raise_exception( EXPORTING iv_msg = CONV #( zlcx_root->kernel_errid ) ).
    ENDTRY.


  ENDMETHOD.
ENDCLASS.
