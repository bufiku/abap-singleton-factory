class ZCL_JW_SINGLETON_INHRT_FACTORY definition
  public
  final
  create private .

public section.

  types:
    BEGIN OF zlty_classtree,
             classname TYPE seoclsname,
             descript  TYPE seodescr,
           END OF zlty_classtree .
  types:
    zltty_classtree TYPE SORTED TABLE OF zlty_classtree
                        WITH UNIQUE KEY  classname .

  class-methods CLASS_CONSTRUCTOR .
  class-methods GET_POSSIBLE_CLASSES
    returning
      value(ZRT_CLASSTREE) type ZLTTY_CLASSTREE .
  class-methods GET_SOME_INSTANCE
    importing
      !ZIP_SINGLETON type ANY
    returning
      value(ZRR_INSTANCE) type ref to ZIF_JW_SINGLETON_INHERITANCE
    raising
      ZCX_SIMPLE_ERROR .
  class-methods RAISE_EXCEPTION
    importing
      !IV_MSG type SYMSGV optional
      !IS_T100_KEY type SCX_T100KEY optional
    raising
      ZCX_SIMPLE_ERROR .
  PROTECTED SECTION.
  PRIVATE SECTION.
    CLASS-DATA:
      zgr_instance       TYPE REF TO zcl_jw_singleton_inhrt_factory,
      zgt_classtree      TYPE zltty_classtree.
    CLASS-METHODS: get_subclasses
      IMPORTING zit_classnames        TYPE zltty_classtree
      RETURNING VALUE(zrt_subclasses) TYPE zltty_classtree.


ENDCLASS.



CLASS ZCL_JW_SINGLETON_INHRT_FACTORY IMPLEMENTATION.


  METHOD class_constructor.

*    zgt_instance_types  = VALUE zltty_instances(
*      ( classname = |ZLCL_SUBCLASS_ONE| descript = |one| )
*      ( classname = |ZLCL_SUBCLASS_TWO| descript = |two| )
*    ).

  ENDMETHOD.


  METHOD get_possible_classes.

    IF lines( zgt_classtree ) = 0.

      DATA(zlt_classtree) =
            get_subclasses(  zit_classnames = VALUE zltty_classtree(
                              ( classname = 'ZCL_JW_SINGLETON_INHRT_SUPER' )
                             )
                           ).

      INSERT LINES OF zlt_classtree INTO TABLE zgt_classtree.
      DO.
        DATA(zlt_classtree_sub) =
              get_subclasses(  zit_classnames = zlt_classtree ).
        IF lines( zlt_classtree_sub ) = 0.
          EXIT. "from do-enddo.
        ENDIF.
        INSERT LINES OF zlt_classtree_sub INTO TABLE zgt_classtree.
        zlt_classtree = zlt_classtree_sub.
      ENDDO.
    ENDIF.

    zrt_classtree = zgt_classtree.
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
        raise_exception( EXPORTING iv_msg = |Error in factory| ).
    ENDTRY.

  ENDMETHOD.


  METHOD get_subclasses.
    CHECK lines( zit_classnames ) > 0.

    SELECT seometarel~clsname, text~descript FROM seometarel AS seometarel
      INNER JOIN seoclasstx AS text
        ON text~clsname = seometarel~clsname
        AND text~langu = @sy-langu
      FOR ALL ENTRIES IN @zit_classnames
      WHERE refclsname = @zit_classnames-classname
      INTO TABLE @zrt_subclasses.
    IF sy-subrc NE 0.
      SELECT DISTINCT seometarel~clsname, text~descript FROM seometarel AS seometarel
      INNER JOIN seoclasstx AS text
        ON text~clsname = seometarel~clsname
        AND text~langu = @sy-langu
      FOR ALL ENTRIES IN @zit_classnames
      WHERE refclsname = @zit_classnames-classname
      INTO TABLE @zrt_subclasses.
    ENDIF.

  ENDMETHOD.


  METHOD raise_exception .
    DATA zgs_t100key TYPE scx_t100key .
    zgs_t100key-msgid = COND symsgid( WHEN is_t100_key-msgid  = space THEN |SA|   ELSE is_t100_key-msgid  ).
    zgs_t100key-msgid = COND symsgid( WHEN is_t100_key-msgid  = space THEN |SA|   ELSE is_t100_key-msgid  ).
    zgs_t100key-attr1 = COND symsgv( WHEN  is_t100_key-attr1  = space THEN iv_msg ELSE is_t100_key-attr1  ).
    zgs_t100key-attr2 = is_t100_key-attr2.
    zgs_t100key-attr3 = is_t100_key-attr3.
    zgs_t100key-attr4 = is_t100_key-attr4.

    RAISE EXCEPTION TYPE zcx_simple_error
      EXPORTING
        textid = zgs_t100key.
  ENDMETHOD.
ENDCLASS.
