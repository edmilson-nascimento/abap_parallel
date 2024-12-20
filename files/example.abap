
CLASS lcl_material DEFINITION CREATE PUBLIC.

    PUBLIC SECTION.
      METHODS constructor
        IMPORTING im_material TYPE matnr
                  im_desc     TYPE makt-maktx.
  
      METHODS change
        RETURNING VALUE(result) TYPE bapiret2_t .
  
    PROTECTED SECTION.
    PRIVATE SECTION.
    
    types:
      BEGIN OF ty_data,
      material type c,
      description type c,
      END OF ty_data .
    
    data: gs_header type c
          gt_description type c .
  METHODS fill .
  METHODS bapi
        RETURNING VALUE(result) TYPE bapiret2_t .
  
  
  ENDCLASS.
  
  CLASS lcl_material IMPLEMENTATION.
  
    METHOD constructor .
    
      me->ls_data = value #( mat )
    
    ENDMETHOD.
  
  
    METHOD change .
    ENDMETHOD.
  
  ENDCLASS.
  
  BREAK-POINT.