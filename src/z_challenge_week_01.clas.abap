CLASS z_challenge_week_01 DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    INTERFACES: if_oo_adt_classrun.

    TYPES group TYPE c LENGTH 1.
    TYPES: BEGIN OF initial_type,
             group       TYPE group,
             number      TYPE i,
             description TYPE string,
           END OF initial_type,
           initial_data TYPE STANDARD TABLE OF initial_type WITH EMPTY KEY.
  PROTECTED SECTION.
  PRIVATE SECTION.
    METHODS:
      fill_itab
        RETURNING
          VALUE(initial_table) TYPE initial_data,
      add_to_itab
        IMPORTING
          initial_table        TYPE initial_data
        RETURNING
          VALUE(updated_table) TYPE initial_data,
      sort_itab
        IMPORTING
          initial_table       TYPE initial_data
        RETURNING
          VALUE(sorted_table) TYPE initial_data,
      search_itab
        IMPORTING
          initial_table       TYPE initial_data
        RETURNING
          VALUE(search_index) TYPE i.

ENDCLASS.



CLASS z_challenge_week_01 IMPLEMENTATION.
  METHOD if_oo_adt_classrun~main.

    DATA(fill_table) = fill_itab( ).
    fill_table = add_to_itab( fill_table ).
    fill_table = sort_itab( fill_table ).

    DATA(search_index) = search_itab( fill_table ).

    out->write( fill_table ).
    out->write( search_index ).
  ENDMETHOD.


  METHOD fill_itab.

    initial_table = VALUE #( ( group = 'A' number = 10 description = 'Group A-2' )
                             ( group = 'B' number = 5 description = 'Group B' )
                             ( group = 'A' number = 6 description = 'Group A-1' )
                             ( group = 'C' number = 22 description = 'Group C-1' )
                             ( group = 'A' number = 13 description = 'Group A-3' )
                             ( group = 'C' number = 500 description = 'Group C-2' ) ).
  ENDMETHOD.

  METHOD add_to_itab.
*    APPEND LINES OF initial_table TO updated_table.
    updated_table = CORRESPONDING #( initial_table ).
    APPEND VALUE #( group = 'A' number = 19 description = 'Group A-4'  ) TO updated_table.
  ENDMETHOD.

  METHOD sort_itab.
    sorted_table = initial_table.
    SORT sorted_table BY group ASCENDING number DESCENDING.
  ENDMETHOD.

  METHOD search_itab.
    search_index = line_index( initial_table[ number = 6 ] ).
  ENDMETHOD.

ENDCLASS.
