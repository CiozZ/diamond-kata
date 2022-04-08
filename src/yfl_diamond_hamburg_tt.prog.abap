*&---------------------------------------------------------------------*
*& Report yfl_diamond_hamburg_tt
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT yfl_diamond_hamburg_tt.

CLASS diamond_kata DEFINITION FINAL.

  PUBLIC SECTION.

    TYPES: BEGIN OF ty_whitespaces,
             indentation TYPE i,
             spacing     TYPE i,
           END OF ty_whitespaces.

    TYPES tt_whitespaces TYPE STANDARD TABLE OF ty_whitespaces WITH EMPTY KEY.

    METHODS print
      IMPORTING seed          TYPE c
      RETURNING VALUE(result) TYPE string.

    METHODS determine_layers
      IMPORTING seed          TYPE c
      RETURNING VALUE(result) TYPE string.

    METHODS calculate_layer_whitespaces
      IMPORTING
        iv_number_of_layers TYPE i
      RETURNING
        VALUE(rt_result)    TYPE tt_whitespaces.
  PRIVATE SECTION.

ENDCLASS.

CLASS diamond_kata IMPLEMENTATION.
  METHOD print.
    result = |A|.
  ENDMETHOD.

  METHOD determine_layers.
    FIND to_upper( seed ) IN sy-abcde MATCH OFFSET DATA(offset).
    result = substring( val = sy-abcde off = 0 len = offset + 1 ).
  ENDMETHOD.

  METHOD calculate_layer_whitespaces.
    rt_result = VALUE #( FOR i = 0 THEN i + 1 UNTIL i < iv_number_of_layers ( indentation = iv_number_of_layers - i - 1
                                                                              spacing = 2 * i - 1 ) ).
  ENDMETHOD.

ENDCLASS.

CLASS ltc_diamond_kata DEFINITION FINAL FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.

  PRIVATE SECTION.
    METHODS diamond_a               FOR TESTING.
    METHODS diamond_c               FOR TESTING.
    METHODS determine_layers_for_a  FOR TESTING.
    METHODS determine_layers_for_b  FOR TESTING.
    METHODS determine_layers_for_f  FOR TESTING.
    METHODS calc_layer_whitespace_3 FOR TESTING.

ENDCLASS.


CLASS ltc_diamond_kata IMPLEMENTATION.

  METHOD diamond_a.
    DATA(result) = NEW diamond_kata( )->print( |A| ).
    cl_abap_unit_assert=>assert_equals( act = result exp = |A| ).
  ENDMETHOD.

  METHOD diamond_c.
    DATA(result) = NEW diamond_kata( )->print( |C| ).
    cl_abap_unit_assert=>assert_equals(
        act = result
        exp = |  A  \n| &&
              | B B \n| &&
              |C   C\n| &&
              | B B \n| &&
              |  A  | ).
  ENDMETHOD.

  METHOD determine_layers_for_a.
    DATA(result) = NEW diamond_kata( )->determine_layers( |a| ).
    cl_abap_unit_assert=>assert_equals( act = result exp = |A| ).
  ENDMETHOD.

  METHOD determine_layers_for_b.
    DATA(result) = NEW diamond_kata( )->determine_layers( |b| ).
    cl_abap_unit_assert=>assert_equals( act = result exp = |AB| ).
  ENDMETHOD.

  METHOD determine_layers_for_f.
    DATA(result) = NEW diamond_kata( )->determine_layers( |F| ).
    cl_abap_unit_assert=>assert_equals( act = result exp = |ABCDEF| ).
  ENDMETHOD.

  METHOD calc_layer_whitespace_3.
    cl_abap_unit_assert=>assert_equals( exp = VALUE diamond_kata=>tt_whitespaces( ( indentation = 2 spacing = -1 )
                                                                                  ( indentation = 1 spacing = 1 )
                                                                                  ( indentation = 0 spacing = 3 ) )
                                        act = NEW diamond_kata( )->calculate_layer_whitespaces( 3 ) ).
  ENDMETHOD.



ENDCLASS.
