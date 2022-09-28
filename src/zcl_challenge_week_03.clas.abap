CLASS zcl_challenge_week_03 DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    INTERFACES: if_oo_adt_classrun.

    TYPES:
      group TYPE c LENGTH 1,

      BEGIN OF initial_numbers_type,
        group  TYPE group,
        number TYPE i,
      END OF initial_numbers_type,

      initial_numbers TYPE STANDARD TABLE OF initial_numbers_type WITH EMPTY KEY,
      sorted_initial TYPE SORTED TABLE OF initial_numbers_type WITH NON-UNIQUE KEY group,

      BEGIN OF initial_numbers_test,
*        client     TYPE mandt,
        grouptest  TYPE group,
        numbertest TYPE i,
      END OF initial_numbers_test,

      update_database TYPE STANDARD TABLE OF initial_numbers_test WITH EMPTY KEY.

    TYPES:
      BEGIN OF aggregated_data_type,
        group   TYPE group,
        count   TYPE i,
        sum     TYPE i,
        min     TYPE i,
        max     TYPE i,
        average TYPE f,
      END OF aggregated_data_type,

      aggregated_data TYPE STANDARD TABLE OF aggregated_data_type WITH EMPTY KEY.

    METHODS:
      perform_aggregation
        IMPORTING
          initial_numbers        TYPE initial_numbers
        RETURNING
          VALUE(aggregated_data) TYPE aggregated_data,
      fill_database_table
        IMPORTING
          initial_data TYPE initial_numbers,
      get_data_from_dbtable
        RETURNING
          VALUE(initial_data) TYPE initial_numbers,
      get_updated_data_from_dbtable
        RETURNING
          VALUE(updated_data) TYPE aggregated_data,
      perform_aggregation_test
        IMPORTING
          initial_numbers        TYPE sorted_initial
        RETURNING
          VALUE(aggregated_data) TYPE aggregated_data.

  PROTECTED SECTION.
  PRIVATE SECTION.
    METHODS:
      fill_initial_data
        RETURNING
          VALUE(initial_data) TYPE initial_numbers.
ENDCLASS.



CLASS zcl_challenge_week_03 IMPLEMENTATION.

  METHOD fill_initial_data.
    initial_data = VALUE #( ( group = 'A' number = 10 )
                            ( group = 'B' number = 5 )
                            ( group = 'A' number = 6 )
                            ( group = 'C' number = 22 )
                            ( group = 'A' number = 13 )
                            ( group = 'C' number = 500 ) ).

  ENDMETHOD.

  METHOD perform_aggregation.
    DATA(lv_groupA) = 'A'.
*    DATA(groupa) = VALUE aggregated_data_type(
*      FOR GROUPS lv_groupA OF <groupA> IN initial_numbers
*        GROUP BY ( group = <groupA> ) ( <groupA> ) ).
*    ).

*    LOOP AT initial_numbers ASSIGNING FIELD-SYMBOL(<initial_numbers>)
*      GROUP BY <initial_numbers>-group ASSIGNING FIELD-SYMBOL(<group>).
*
*    ENDLOOP.

*    LOOP AT initial_numbers ASSIGNING FIELD-SYMBOL(<initial_number>)
*      GROUP BY ( key = <initial_number>-group count = GROUP SIZE ) ASCENDING
*      ASSIGNING FIELD-SYMBOL(<group_key>).
*
*      APPEND INITIAL LINE TO aggregated_data ASSIGNING FIELD-SYMBOL(<aggregated_items>).
*      <aggregated_items>-group = <group_key>-key.
*      <aggregated_items>-count = <group_key>-count.
*      <aggregated_items>-min = 999999.
*      LOOP AT GROUP <group_key> ASSIGNING FIELD-SYMBOL(<group_item>).
*        <aggregated_items>-sum = <aggregated_items>-sum + <group_item>-number.
*        <aggregated_items>-min = nmin( val1 = <aggregated_items>-min
*                                       val2 = <group_item>-number ).
*        <aggregated_items>-max = nmax( val1 = <aggregated_items>-max
*                                      val2 = <group_item>-number ).
*
*      ENDLOOP.
*      <aggregated_items>-average = <aggregated_items>-sum / <aggregated_items>-count.
*
*    ENDLOOP.
*
*    CLEAR: aggregated_data.
*
*    aggregated_data = VALUE #( LET lt_copy = initial_numbers IN
*                               FOR GROUPS <groups> OF ls_idx IN lt_copy
*                               GROUP BY ( group = ls_idx
*                                          count = GROUP SIZE ) ASCENDING
*                               FOR step IN GROUP <groups> ( CORRESPONDING #( <groups> ) ) ).

*    DATA(lt_test_data) = VALUE aggregated_data_type( FOR GROUPS <groups> OF ls_idx IN lt_copy
*                               GROUP BY ( group = ls_idx
*                                          count = GROUP SIZE ) ASCENDING
*                               FOR step IN GROUP <groups> ( CORRESPONDING #( <groups> ) ) ).


    LOOP AT initial_numbers REFERENCE INTO DATA(initial_number)
      GROUP BY ( key = initial_number->group count = GROUP SIZE ) ASCENDING
      REFERENCE INTO DATA(group_key).

      APPEND INITIAL LINE TO aggregated_data REFERENCE INTO DATA(aggregated_items).
      aggregated_items->group = group_key->key.
      aggregated_items->count = group_key->count.
      aggregated_items->min = 999999.
      LOOP AT GROUP group_key REFERENCE INTO DATA(group_item).
        aggregated_items->sum = aggregated_items->sum + group_item->number.
        aggregated_items->min = nmin( val1 = aggregated_items->min
                                       val2 = group_item->number ).
        aggregated_items->max = nmax( val1 = aggregated_items->max
                                      val2 = group_item->number ).

      ENDLOOP.
      aggregated_items->average = aggregated_items->sum / aggregated_items->count.

    ENDLOOP.
  ENDMETHOD.

  METHOD if_oo_adt_classrun~main.
    DATA(initial_data) = fill_initial_data( ).
    DATA(aggregated_data) = perform_aggregation( initial_data ).

    out->write( initial_data ).
    out->write( aggregated_data ).

*    INSERT z001_zhoward_001 FROM TABLE initial_data.
    fill_database_table( CORRESPONDING #( initial_data ) ).

    DATA(data_from_db) = get_data_from_dbtable( ).
    out->write( data_from_db ).

    DATA(aggregation_test) = perform_aggregation_test( CORRESPONDING sorted_initial( data_from_db ) ).

    DATA(aggregate_from_db) = get_updated_data_from_dbtable( ).

    out->write( aggregate_from_db ).
  ENDMETHOD.


  METHOD fill_database_table.

    DATA: insert_data_to_db TYPE STANDARD TABLE OF z001_zhoward_001.
    DATA: wa_insert_data TYPE z001_zhoward_001.
    DATA(client) = sy-mandt.

    DATA: number TYPE char4 VALUE '0001'.
    LOOP AT initial_data REFERENCE INTO DATA(data).
      wa_insert_data-hashid = | { number ALPHA = IN }{ data->group }{ data->number } |.
      wa_insert_data-client = sy-mandt.
      wa_insert_data-grouptest = data->group.
      wa_insert_data-numbertest = data->number.

      number = number + 1.
      APPEND wa_insert_data TO insert_data_to_db.
    ENDLOOP.

*    insert_data_to_db = CORRESPONDING #( initial_data
*                             MAPPING grouptest = group
*                                     numbertest = number ).
    "refresh table
    DELETE FROM z001_zhoward_001.
    INSERT z001_zhoward_001 FROM TABLE @insert_data_to_db.
  ENDMETHOD.

  METHOD get_data_from_dbtable.
    SELECT FROM z001_zhoward_001
      FIELDS
        client,
        grouptest,
        hashid,
        numbertest
      ORDER BY hashid
      INTO TABLE @DATA(data_from_dbtable).

    initial_data = CORRESPONDING #( data_from_dbtable
                                    MAPPING group = grouptest
                                            number = numbertest ).
  ENDMETHOD.

  METHOD get_updated_data_from_dbtable.
    SELECT FROM z001_zhoward_001
      FIELDS
        grouptest as group,
        count(*) AS count,
        SUM( numbertest ) as sum,
        min( numbertest ) as min,
        max( numbertest ) as max,
        avg( numbertest ) as average
        GROUP BY grouptest
      INTO TABLE @updated_data.
  ENDMETHOD.

  METHOD perform_aggregation_test.

    LOOP AT initial_numbers REFERENCE INTO DATA(initial_data)
      GROUP BY ( group = initial_data->group count = GROUP SIZE )
      REFERENCE INTO DATA(group_data).

      APPEND INITIAL LINE TO aggregated_data REFERENCE INTO DATA(aggregated_items).
      aggregated_items->group = group_data->group.
      aggregated_items->count = group_data->count.
      aggregated_items->min = 99999. "set to max to work properly


      "get sum, min and max
      aggregated_items->sum = REDUCE #( INIT init = 0
                                        FOR <initial> IN
                                        FILTER #( initial_numbers
                                        WHERE group = group_data->group )
                                        NEXT init = init + <initial>-number ).

*      aggregated_items->min =
    ENDLOOP.

    IF aggregated_data IS NOT INITIAL.
    ENDIF.
  ENDMETHOD.

ENDCLASS.
