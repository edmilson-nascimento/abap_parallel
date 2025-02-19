report parallel .


CLASS main DEFINITION.

    PUBLIC SECTION.
  
      TYPES:
        BEGIN OF shared_record,
          process_mode TYPE c LENGTH 1,
        END OF shared_record,
  
        single_record TYPE scarr-carrid,
  
        BEGIN OF task_result,
          carrid TYPE scarr-carrid,
          count  TYPE i,
        END OF task_result.
  
      CLASS-METHODS process.
  
  ENDCLASS.
  
  CLASS single_task DEFINITION
    INHERITING FROM cl_abap_parallel.
  
    PUBLIC SECTION.
  
      METHODS: do REDEFINITION.
  
  ENDCLASS.
  
  *This is the only statement outside of the classes
  main=>process( ).
  
  CLASS main IMPLEMENTATION.
  
    METHOD process.
  
      DATA: tasks_input        TYPE cl_abap_parallel=>t_in_tab,
            task_input_single  TYPE xstring,
            single_record      TYPE single_record,
            tasks_input_shared TYPE xstring,
            shared_record      TYPE shared_record,
            task_result        TYPE task_result.
  
      shared_record-process_mode = 1.
  
  *   Since the process_mode value is shared across all tasks, store
  *   it in the shared variable (tasks_input_shared) instead of
  *   repeating it for every task input record (tasks_input)
  
  *   To be imported by SINGLE_TASK->DO
      EXPORT buffer_task_shared = shared_record
        TO DATA BUFFER tasks_input_shared.
  
  *   Get data to be processed.  EXPORT each record and collect them all into
  *   table tasks_input.
      SELECT
        carrid
      FROM scarr
      INTO @single_record.
  
  *     To be imported by SINGLE_TASK->DO
        EXPORT buffer_task = single_record TO DATA BUFFER task_input_single.
        INSERT task_input_single INTO TABLE tasks_input.
  
      ENDSELECT.
  
  *   Create the instance while configuring the resource usage
      DATA(parallel) = NEW single_task(
  *                          p_num_tasks     = 8
  *                          p_timeout       = 200
  *                          p_percentage    = 50
  *                          p_num_processes = 20
  *                          p_local_server  =
                           ).
  
      DATA(debug) = ' '.
  
  *   Perform the tasks in parallel
      parallel->run(
        EXPORTING
          p_in_tab  = tasks_input
          p_in_all  = tasks_input_shared
          p_debug   = debug
        IMPORTING
          p_out_tab = DATA(tasks_output)
      ).
  
      LOOP AT tasks_output ASSIGNING FIELD-SYMBOL(<task_output_single>).
  
  *     Something went wrong, like a timeout, if the message has a value
        IF <task_output_single>-message IS NOT INITIAL.
          WRITE: / 'Task Error:', <task_output_single>-message.
        ENDIF.
  
        IF <task_output_single>-result IS NOT INITIAL.
  *       Exported from SINGLE_TASK->DO
          IMPORT buffer_result = task_result
            FROM DATA BUFFER <task_output_single>-result.
  
          WRITE: / task_result-carrid, task_result-count.
        ENDIF.
  
      ENDLOOP.
  
    ENDMETHOD.
  
  ENDCLASS.
  
  CLASS single_task IMPLEMENTATION.
  
    METHOD do.
  
  *   I referenced the MAIN local class instead of using the data dictionary
  *   for the ease for copy/paste
      DATA: shared_record TYPE main=>shared_record,
            single_record TYPE main=>single_record,
            task_result   TYPE main=>task_result.
  
  *   Exported by MAIN->PROCCESS
      IMPORT buffer_task_shared = shared_record FROM DATA BUFFER p_in_all.
  
  *   Exported by MAIN->PROCCESS
      IMPORT buffer_task = single_record FROM DATA BUFFER p_in.
  
  *   Other shared values I like to include are a simulation/update flag
  *   and a level for the logging detail
      CASE shared_record-process_mode.
        WHEN '1'.
  
  *       Incredibly simplistic example which doesn't warrant parallel processing
          SELECT
            carrid,
            COUNT( * )
          FROM sflight
          WHERE
            carrid = @single_record
          GROUP BY
            carrid
          INTO ( @task_result-carrid, @task_result-count ).
  
          ENDSELECT.
  
        WHEN '2'.
      ENDCASE.
  
  *   To be imported by MAIN->PROCESS
      EXPORT buffer_result = task_result TO DATA BUFFER p_out.
  
    ENDMETHOD.
  
  ENDCLASS.
  