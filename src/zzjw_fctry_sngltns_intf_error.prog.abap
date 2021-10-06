*&---------------------------------------------------------------------*
*& Report ZZJW_FCTRY_SNGLTNS_INTF_ERROR
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zzjw_fctry_sngltns_intf_error.

CLASS zlcx_error  DEFINITION DEFERRED.
**********************************************************************
***  DEFINITIONS  ****************************************************
**********************************************************************
INTERFACE: zlif_interface.
  TYPES: BEGIN OF zlty_instance,
           classname TYPE seoclsname,
           instance  TYPE REF TO zlif_interface,
         END OF zlty_instance,
         zltty_instances TYPE SORTED TABLE OF zlty_instance WITH UNIQUE KEY classname.
  CLASS-DATA zgt_instances TYPE zltty_instances.
  CLASS-METHODS:
    get_instance CHANGING zcv_instance TYPE any  RAISING zlcx_error .
  METHODS: do_something RETURNING VALUE(zrv_text) TYPE string.
ENDINTERFACE.


CLASS zlcx_error  DEFINITION
  INHERITING FROM cx_dynamic_check.
  PUBLIC SECTION.
    INTERFACES: if_t100_message.
    METHODS constructor IMPORTING id    TYPE symsgid OPTIONAL
                                  no    TYPE symsgno OPTIONAL
                                  text1 TYPE csequence OPTIONAL
                                  text2 TYPE csequence OPTIONAL
                                  text3 TYPE csequence OPTIONAL
                                  text4 TYPE csequence OPTIONAL.
    DATA text1 TYPE c LENGTH 50.
    DATA text2 TYPE c LENGTH 50.
    DATA text3 TYPE c LENGTH 50.
    DATA text4 TYPE c LENGTH 50.
ENDCLASS.


*Weird things: If I define my interface first, and my exception class
* second, I get this error, even if I use a DEFINITION DEFERRED for my
* exception class.
*       The class "ZLCX_ERROR" was not derived from
*       either "CX_STATIC_CHECK" or "CX_DYNAMIC_CHECK".

CLASS zlcl_factory DEFINITION ABSTRACT FINAL CREATE PRIVATE.
  PUBLIC SECTION.
    TYPES: BEGIN OF zlty_instances,
             instance_type TYPE string,
             classname     TYPE seoclsname,
           END OF zlty_instances,
           zltty_instances TYPE SORTED TABLE OF zlty_instances
                        WITH UNIQUE KEY  instance_type
                        WITH UNIQUE SORTED KEY k2 COMPONENTS classname.

    CLASS-METHODS:
      class_constructor,
      get_possible_instance_classes RETURNING VALUE(zrt_instances) TYPE zltty_instances,
      get_some_instance IMPORTING zip_singleton       TYPE any
                        RETURNING VALUE(zrr_instance) TYPE REF TO zlif_interface
                        RAISING   zlcx_error .
  PRIVATE SECTION.
    CLASS-DATA:
      zgt_instance_types TYPE zltty_instances,
      zgr_instance       TYPE REF TO zlcl_factory.
ENDCLASS.

CLASS zlcl_super DEFINITION ABSTRACT CREATE PROTECTED.
  PUBLIC SECTION.
    INTERFACES  zlif_interface.
    ALIASES:    do_something  FOR zlif_interface~do_something,
                get_instance  FOR zlif_interface~get_instance.
  PRIVATE SECTION.
    ALIASES:    zgt_instances FOR zlif_interface~zgt_instances.
ENDCLASS.

CLASS zlcl_subclass_one DEFINITION INHERITING FROM zlcl_super CREATE PRIVATE FRIENDS zlcl_super.
  PUBLIC SECTION.
    METHODS:
      do_something  REDEFINITION.
ENDCLASS.


CLASS zlcl_subclass_two DEFINITION INHERITING FROM zlcl_super CREATE PRIVATE FRIENDS zlcl_super.
  PUBLIC SECTION.
    METHODS:
      do_something  REDEFINITION.
ENDCLASS.



**********************************************************************
***  IMPLEMENTATIONS  ************************************************
**********************************************************************
CLASS zlcl_super IMPLEMENTATION.
  METHOD get_instance.
    "  https://answers.sap.com/questions/6203810/inherited-static-method-to-return-instance-of-sub-.html

    TRY. "Get the class-type of the requested object to be created.
        IF cl_abap_refdescr=>describe_by_data( zcv_instance )->kind = cl_abap_typedescr=>kind_ref.
          DATA(lo_ref_descr) = CAST cl_abap_refdescr( cl_abap_refdescr=>describe_by_data( zcv_instance ) ).
          DATA(zlv_classname) = lo_ref_descr->get_referenced_type( )->get_relative_name( ).
          "Check for existance
          READ TABLE zlcl_factory=>get_possible_instance_classes( ) WITH TABLE KEY k2 COMPONENTS classname = zlv_classname TRANSPORTING NO FIELDS.
          IF sy-subrc NE 0.  "Not a valid class to be instantiated.
            RAISE EXCEPTION TYPE zlcx_error EXPORTING text1 = |Error casting & creating object| .
          ENDIF.

          DATA: zls_instance TYPE zlif_interface~zlty_instance.
          READ TABLE zgt_instances WITH KEY classname = zlv_classname INTO zls_instance .
          IF sy-subrc NE 0.
            DATA: zlr_instance TYPE REF TO zlif_interface.
            CREATE OBJECT zlr_instance TYPE (zlv_classname).

            zls_instance = VALUE zlif_interface~zlty_instance(
                classname = zlv_classname
                instance  = zlr_instance ).

            INSERT zls_instance INTO TABLE zgt_instances .
          ENDIF.

          zcv_instance ?= zls_instance-instance.
        ELSE.
          RAISE EXCEPTION TYPE zlcx_error EXPORTING text1 = | Error : No class-definition given for an instance| .
        ENDIF.

      CATCH zlcx_error INTO DATA(zlcx_error_lcl).
        RAISE EXCEPTION zlcx_error_lcl.
      CATCH cx_sy_move_cast_error INTO DATA(zlcx_move_cast).
        RAISE EXCEPTION TYPE zlcx_error EXPORTING text1 = | Something went wrong with casting | .
      CATCH cx_root INTO DATA(zlcx_root).
        RAISE EXCEPTION TYPE zlcx_error EXPORTING text1 = zlcx_root->kernel_errid ." | Something went wrong | .
    ENDTRY.


*** Old code, before creating the table of singletons.
**    TRY.
*    IF  zgr_instance IS INITIAL
*    AND cl_abap_refdescr=>describe_by_data( zcv_instance )->kind = cl_abap_typedescr=>kind_ref.
*      DATA(lo_ref_descr) = CAST cl_abap_refdescr( cl_abap_refdescr=>describe_by_data( zcv_instance ) ).
*      DATA(zlv_classname) = lo_ref_descr->get_referenced_type( )->get_relative_name( ).
*      DATA: zlr_instance TYPE REF TO zlif_interface.
*      CREATE OBJECT zlr_instance TYPE (zlv_classname).
*      zgr_instance ?= zlr_instance.
*    ENDIF.
**      CATCH cx_root.
**    ENDTRY.
*
*    zcv_instance ?= zgr_instance.
  ENDMETHOD.
  METHOD do_something.
    zrv_text = | returned from super |.
  ENDMETHOD.
ENDCLASS.


CLASS zlcl_subclass_one IMPLEMENTATION .
  METHOD do_something.
    zrv_text = | returned from subclass ONE |.
  ENDMETHOD.
ENDCLASS.


CLASS zlcl_subclass_two IMPLEMENTATION.
  METHOD do_something.
    zrv_text = | returned from subclass TWO |.
  ENDMETHOD.
ENDCLASS.

CLASS zlcl_factory IMPLEMENTATION.
  METHOD class_constructor.
    zgt_instance_types  = VALUE zltty_instances(
      ( instance_type = |one| classname = |ZLCL_SUBCLASS_ONE| )
      ( instance_type = |two| classname = |ZLCL_SUBCLASS_TWO| )
      ).

  ENDMETHOD.
  METHOD get_possible_instance_classes.
    zrt_instances = zgt_instance_types.
  ENDMETHOD.

  METHOD get_some_instance.
    DATA dref TYPE REF TO data.

    TRY.

        CREATE DATA dref TYPE REF TO (zip_singleton).
        ASSIGN dref->* TO FIELD-SYMBOL(<fs_ref>).

        TRY.
            "Create & fill the signature of the method
            DATA(ptab) = VALUE abap_parmbind_tab(
                            ( name  = 'ZCV_INSTANCE'
                              kind  = cl_abap_objectdescr=>changing
                              value = REF #( <fs_ref> ) )
                              ) .

            CALL METHOD (zip_singleton)=>('GET_INSTANCE')
              PARAMETER-TABLE ptab.
          CATCH cx_sy_dyn_call_error INTO DATA(exc_ref).
            MESSAGE exc_ref->get_text( ) TYPE 'I'.
        ENDTRY.

        zrr_instance ?= <fs_ref>.
      CATCH cx_root INTO DATA(lcx_root).
        RAISE EXCEPTION TYPE zlcx_error EXPORTING text1 = |Error in factory| .
    ENDTRY.

  ENDMETHOD.

ENDCLASS.

CLASS zlcx_error IMPLEMENTATION.
  METHOD constructor.
    super->constructor( ).
    me->text1 = text1.
    me->text2 = text2.
    me->text3 = text3.
    me->text4 = text4.
    if_t100_message~t100key-msgid = COND symsgid( WHEN id = space THEN |SA| ELSE id ).
    if_t100_message~t100key-msgno = COND symsgno( WHEN no = space THEN |999| ELSE no ).
    if_t100_message~t100key-attr1 = 'TEXT1'.
    if_t100_message~t100key-attr2 = 'TEXT2'.
    if_t100_message~t100key-attr3 = 'TEXT3'.
    if_t100_message~t100key-attr4 = 'TEXT4'.
  ENDMETHOD.
ENDCLASS.


START-OF-SELECTION.
  DATA: lref_one TYPE REF TO zlcl_subclass_one.
  DATA: lref_one_via_super TYPE REF TO zlcl_subclass_one.
  DATA: lref_two TYPE REF TO zlcl_subclass_two.
  DATA: lref_two_via_super TYPE REF TO zlcl_subclass_two.

  TRY.

      zlcl_super=>get_instance(
        CHANGING
          zcv_instance =  lref_one_via_super
      ).
      cl_demo_output=>write_text( text = lref_one_via_super->do_something( ) ).


      zlcl_super=>get_instance(
        CHANGING
          zcv_instance =  lref_two_via_super
      ).
      cl_demo_output=>write_text( text = lref_two_via_super->do_something( ) ).


      zlcl_subclass_one=>get_instance(
        CHANGING
          zcv_instance =  lref_one
      ).
      cl_demo_output=>write_text( text = lref_one_via_super->do_something( ) ).


      zlcl_subclass_two=>get_instance(
        CHANGING
          zcv_instance =  lref_two
      ).
      cl_demo_output=>write_text( text = lref_two->do_something( ) ).


      cl_demo_output=>write_text( text = 'Let us try to test the factory' ).

      DO 2 TIMES.
        LOOP AT zlcl_factory=>get_possible_instance_classes( ) ASSIGNING FIELD-SYMBOL(<fs_possible_instance>).
          DATA(instance) = zlcl_factory=>get_some_instance( zip_singleton = <fs_possible_instance>-classname ).
          cl_demo_output=>write_text( text = instance->do_something( ) ).
        ENDLOOP.
      ENDDO.

    CATCH zlcx_error INTO DATA(zlcx_error).
      cl_demo_output=>write_text( text = 'Oh oh, someting went wrong' ).
      cl_demo_output=>write_text( text = |{ zlcx_error->text1 } { zlcx_error->text2 } { zlcx_error->text3 } { zlcx_error->text4 } |  ).
  ENDTRY.
  cl_demo_output=>display( ).
