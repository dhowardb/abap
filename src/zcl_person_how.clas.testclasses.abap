*"* use this source file for your ABAP unit test classes
CLASS lcl_person_how DEFINITION FOR TESTING DURATION SHORT RISK LEVEL HARMLESS.

  PUBLIC SECTION.

  PRIVATE SECTION.

    DATA cut TYPE REF TO zcl_person_how.
    DATA builder TYPE REF TO zcl_person_builder_how.

    METHODS:
      verify_person_details_01 FOR TESTING RAISING cx_static_check,
      verify_person_details_02 FOR TESTING RAISING cx_static_check.

    METHODS:
      setup,
      teardown.
ENDCLASS.

CLASS lcl_person_how IMPLEMENTATION.

  METHOD setup.
    builder = NEW #( ).
  ENDMETHOD.

  METHOD teardown.
    CLEAR: builder,
           cut.
  ENDMETHOD.

  METHOD verify_person_details_01.
    builder->zif_person_builder_how~set_age( 20 )->zif_person_builder_how~set_name( 'Howard' ).
    cut = builder->zif_person_builder_how~get_person( ).

    cl_abap_unit_assert=>assert_equals( act = cut->display_person( )
                                        exp = VALUE zif_person_how=>ty_person_details( name = 'Howard'
                                                                                       age = 20 ) ).
  ENDMETHOD.

  METHOD verify_person_details_02.
    builder->zif_person_builder_how~set_age( 22 )->zif_person_builder_how~set_name( 'How' ).
    cut = builder->zif_person_builder_how~get_person( ).

    cl_abap_unit_assert=>assert_equals( act = cut->display_person( )
                                        exp = VALUE zif_person_how=>ty_person_details( name = 'How'
                                                                                       age = 22 ) ).
  ENDMETHOD.

ENDCLASS.
