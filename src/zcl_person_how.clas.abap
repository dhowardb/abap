CLASS zcl_person_how DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    INTERFACES
      if_oo_adt_classrun.

    INTERFACES:
      zif_person_how.

    METHODS:
      constructor
        IMPORTING
          VALUE(iv_name) TYPE string
          VALUE(iv_age)  TYPE i.

    METHODS:
      display_person
        RETURNING
          VALUE(personal_details) TYPE zif_person_how=>ty_person_details.

  PROTECTED SECTION.
  PRIVATE SECTION.
    DATA person_details TYPE zif_person_how=>ty_person_details.
ENDCLASS.



CLASS zcl_person_how IMPLEMENTATION.
  METHOD constructor.
    me->person_details-name = iv_name.
    me->person_details-age = iv_age.
  ENDMETHOD.

  METHOD display_person.
    personal_details = VALUE #( name = me->person_details-name
                                 age = me->person_details-age ).
  ENDMETHOD.

  METHOD if_oo_adt_classrun~main.
    DATA(person_builder) = NEW zcl_person_builder_how( ).
    person_builder->zif_person_builder_how~set_name( 'Howard'
                 )->zif_person_builder_how~set_age( '20' ).

    DATA(person) = person_builder->zif_person_builder_how~get_person( ).

    out->write( person->display_person( ) ).
  ENDMETHOD.

ENDCLASS.
