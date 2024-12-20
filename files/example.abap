
CLASS lcl_material DEFINITION CREATE PUBLIC.

    PUBLIC SECTION.
      METHODS constructor
        IMPORTING im_material TYPE matnr
                  im_desc     TYPE makt-maktx.
  
      METHODS change
        RETURNING VALUE(result) TYPE bapiret2.
  
    PRIVATE SECTION.
  
      TYPES:
        BEGIN OF ty_data,
          material  TYPE bapimathead-material,
          matl_desc TYPE bapi_makt-matl_desc,
        END OF ty_data.
  
      DATA:
        gs_data        TYPE ty_data,
        gs_header      TYPE bapimathead,
        gt_description TYPE tt_bapi_makt.
  
      METHODS fill.
  
      METHODS bapi
        RETURNING VALUE(result) TYPE bapiret2.
  
  ENDCLASS.
  
  CLASS lcl_material IMPLEMENTATION.
  
    METHOD constructor.
  
      me->gs_data = VALUE #( material  = im_material
                             matl_desc = im_desc ).
    ENDMETHOD.
  
  
    METHOD change.
  
      IF    me->gs_header               IS INITIAL
         OR lines( me->gt_description )  = 0.
        RETURN.
      ENDIF.
  
      me->fill( ).
  
      result = me->bapi( ).
  
  
    ENDMETHOD.
  
  
    METHOD fill.
  
      me->gs_header      = VALUE #( material = |{ me->gs_header-material ALPHA = OUT }| ).
      me->gt_description = VALUE #( ( langu     = sy-langu
                                      matl_desc = me->gs_data-matl_desc ) ).
  
    ENDMETHOD.
  
    METHOD bapi.
  
      CALL FUNCTION 'BAPI_MATERIAL_SAVEDATA'
        EXPORTING headdata            = me->gs_header
        IMPORTING return              = result
        TABLES    materialdescription = me->gt_description.
  
    ENDMETHOD.
  
  ENDCLASS.