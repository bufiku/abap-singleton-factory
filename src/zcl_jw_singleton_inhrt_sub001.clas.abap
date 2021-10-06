CLASS zcl_jw_singleton_inhrt_sub001 DEFINITION
  PUBLIC
  INHERITING FROM zcl_jw_singleton_inhrt_super
  CREATE PUBLIC .

  PUBLIC SECTION.
    METHODS:
      do_something  REDEFINITION.
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS ZCL_JW_SINGLETON_INHRT_SUB001 IMPLEMENTATION.


  METHOD do_something.
    zrv_text = | returned from subclass ONE |.
  ENDMETHOD.
ENDCLASS.
