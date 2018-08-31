      *****************************************************************
      *                                                               *
      *       PROGRAMA DE MIGUEL ANTONIO CHAMORRO MARTINEZ            *
      *                                                               *
      *          ESTE PROGRAMA CARGGA UN ARCHIVO Y REALIZAMOS         *
      *           OPERACIONES SOBRE DICHO FICHERO                     *
      *****************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID.    COBOL003.
       AUTHOR MIGUEL-CHAMORRO.
      *
      *****************************************************************
      *                  ENVIROMENT DIVISION                          *
      *****************************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
      *
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT FICHERO
           ASSIGN TO
      *    "/home/forma2/cobol/ficheros/fichero.empleado"
      *     "/home/forma2/cobol/ficheros/fichero.empleado.correcto"
      *    "/home/forma2/cobol/ficheros/fichero.empleado.cruce"
      *    "/home/forma2/cobol/ficheros/fichero.empleado.cruce.uno"
           "/home/forma2/cobol/ficheros/fichero.empleado.cruce.vacio"
      *    "/home/forma2/cobol/ficheros/fichero.empleado.cruce.raro"
             FILE STATUS IS WS-FILE-STATUS.
      *
      *****************************************************************
      *                    DATA DIVISION                              *
      *****************************************************************
       DATA DIVISION.
       FILE SECTION.
       FD FICHERO.
       01  RG-EMPLE                              PIC X(61).
      *
      *****************************************************************
      *               WORKING STORAGE SECTION                         *
      *****************************************************************
       WORKING-STORAGE SECTION.
      *
      *****************************************************************
      *               VARIABLES  FICHERO ENTRADA                      *
      *****************************************************************
       COPY COPYEMPLE.
      *
      ***************************************************************** 
      *               VARIABLES  FICHERO SALIDA                       *
      *****************************************************************
      *
      *****************************************************************
      *               SWITCHES                                        * 
      *****************************************************************
       01  SW-SWITCHES.
           05  SW-FIN-FICHERO                    PIC 9.
                88 FIN-FICHERO                   VALUE "1".
                88 NO-FIN-FICHERO                VALUE "0".
           05  SW-ERRORES                        PIC 9 VALUE 0.
                88 SI-ERROR                      VALUE "1".
                88 NO-ERROR                      VALUE "0".
      *
      *****************************************************************
      *               CONSTANTES Y LITERALES                          *
      *****************************************************************
       01  LT-LITERALES.
           05  LT-FICHERO           PIC X(16) VALUE "FICHERO.EMPLEADO".
           05  LT-OPEN                           PIC X(4) VALUE "OPEN".
           05  LT-READ                           PIC X(4) VALUE "READ".
           05  LT-CLOSE                          PIC X(4) VALUE "CLOSE".
      *
      *****************************************************************
      *               VARIABLES AUXILIARES                            *
      *****************************************************************
       01  WS-VARIABLES.
           05  WS-FILE-STATUS                       PIC XX.
           05  WS-FICHERO-ERROR                     PIC X(16).
           05  WS-PARRAFO-ERROR                     PIC 9(4).
           05  WS-OPERACION-ERROR                   PIC X(4).    
      *
      *****************************************************************
      *               CONTADORES                                      *
      *****************************************************************
       01  WS-CONTADORES.
           05 WC-CONTADOR                        PIC 9(2).
           05 WC-CONTADOR1                       PIC 9(2).
           05 WC-CONTADOR2                       PIC 9(2).
           05 WC-CONTADOR3                       PIC 9(2).
           05 WC-CONTADOR4                       PIC 9(2).
      *
      *****************************************************************
      *               PROCEDURE  DIVISION.                            *
      *****************************************************************
       PROCEDURE DIVISION.
      *
           PERFORM 1000-INICIO
              THRU 1000-INICIO-EXIT.
      *
           PERFORM 3000-PROCESO
              THRU 3000-PROCESO-EXIT
              UNTIL FIN-FICHERO.
      *
           PERFORM 8000-FIN
              THRU 8000-FIN-EXIT.
      *
      *****************************************************************
      *               INICIO                                          *
      *****************************************************************
       1000-INICIO.
      *INICIALIZAMOS LAS VARIABLES NECESARIAS
           INITIALIZE WS-CONTADORES.
      *
           OPEN INPUT FICHERO.
           MOVE LT-FICHERO TO WS-FICHERO-ERROR.
      *
      *SI SE PRODUCE UN ERROR AL ABIR EL FICHERO NO SIGUE EJECUTANDOSE
           IF WS-FILE-STATUS NOT = 00
                MOVE 1000 TO WS-PARRAFO-ERROR
                MOVE LT-OPEN TO WS-OPERACION-ERROR
                PERFORM 9100-ERRORES
                THRU 9100-ERRORES-EXIT
           END-IF.
      *                
           READ FICHERO RECORD INTO WS-REG-EMPLEADO
              AT END SET FIN-FICHERO TO TRUE.
      *        
      *SI SE PRODUCE UN ERROR AL LEEREL PRIMER REGISTRO
      *NO SIGUE EJECUTANDOSE        
           IF WS-FILE-STATUS = 00
               CONTINUE
           ELSE
                MOVE 1000 TO WS-PARRAFO-ERROR
                MOVE LT-READ TO WS-OPERACION-ERROR
                PERFORM 9100-ERRORES
                THRU 9100-ERRORES-EXIT
           END-IF.
      *
       1000-INICIO-EXIT.
       EXIT.
      *
      *****************************************************************
      *             PROCESO                                           *
      *****************************************************************
       3000-PROCESO.
           DISPLAY "COD.EMPLE: " WS-EMPLE-CODIGO "  NOMBRE: "
                WS-EMPLE-NOMBRE.
      *
      *SI SE PRODUCE UN ERROR LLAMA AL PARRAFO DE ERRORES
           IF WS-FILE-STATUS = 00
                 CONTINUE
           ELSE
                MOVE 3000 TO WS-PARRAFO-ERROR
                MOVE LT-READ TO WS-OPERACION-ERROR
                PERFORM 9100-ERRORES
                THRU 9100-ERRORES-EXIT
           END-IF.
      *
           EVALUATE TRUE
               WHEN WS-EMPLE-DEPT(1:1) = "A"
                   ADD 1 TO WC-CONTADOR1
               WHEN WS-EMPLE-DEPT(1:1) = "B"
                   ADD 1 TO WC-CONTADOR2
               WHEN WS-EMPLE-DEPT(1:1) = "D"
                   ADD 1 TO WC-CONTADOR3
               WHEN OTHER
                   ADD 1 TO WC-CONTADOR4
           END-EVALUATE.
      *
           READ FICHERO RECORD INTO WS-REG-EMPLEADO
              AT END SET FIN-FICHERO TO TRUE.
      *
           ADD 1 TO WC-CONTADOR.
      *
       3000-PROCESO-EXIT.
       EXIT.
      *
      *****************************************************************
      *             FIN                                               *
      *****************************************************************
       8000-FIN.
           IF NO-ERROR
                 DISPLAY "******************************************"
                 DISPLAY "**FILAS LEIDAS:      " WC-CONTADOR
                 DISPLAY "******************************************"
                 DISPLAY "**EMPIEZAN POR A:    " WC-CONTADOR1
                 DISPLAY "******************************************"
                 DISPLAY "**EMPIEZAN POR B:    " WC-CONTADOR2
                 DISPLAY "******************************************"
                 DISPLAY "**EMPIEZAN POR C:    " WC-CONTADOR3
                 DISPLAY "******************************************"
                 DISPLAY "**EMPIEZAN POR OTRO: " WC-CONTADOR4
                 DISPLAY "******************************************"
                 
           END-IF.
      *
           CLOSE FICHERO.
           DISPLAY "FICHERO CERRADO".
           DISPLAY "FILE.STATUS: " WS-FILE-STATUS.
           STOP RUN.
      *
       8000-FIN-EXIT.
       EXIT.
      *****************************************************************
      *                COMPROBACION DE ERRORES                        *
      *****************************************************************
       9100-ERRORES.
           SET SI-ERROR TO TRUE.
      *     
           DISPLAY "************************************".
           DISPLAY "          E R R O R       ".
           DISPLAY "************************************".
           DISPLAY "************************************".
      *
      *EVALUAMOS EL TIPO DE ERROR Y LO MOSTRAMOS POR PANTALLA 
           EVALUATE WS-FILE-STATUS
                WHEN 04
                      DISPLAY "SE HA PRODUCIDO UN DESBORDAMIENTO"
                WHEN 10
                     DISPLAY "EL FICHERO ESTA VACIO"
                WHEN 35
                     DISPLAY "NO SE ENCUENTRA EL FICHERO"
                WHEN OTHER
                     DISPLAY "SE HA PRODUCIDO UN ERROR"
           END-EVALUATE.
      * 
           DISPLAY "*********************************".
           DISPLAY "*********************************".
           DISPLAY "ANALISIS DE ERROR: ".
           DISPLAY "*********************************".
           DISPLAY "****FILE STATUS: " WS-FILE-STATUS.
           DISPLAY "****FICHERO:     " WS-FICHERO-ERROR.
           DISPLAY "****PARRAFO:     " WS-PARRAFO-ERROR.
           DISPLAY "****LINEA:       " WC-CONTADOR.
           DISPLAY "****OPERACION:   " WS-OPERACION-ERROR.
           DISPLAY "*********************************".
      *
           PERFORM 8000-FIN
              THRU 8000-FIN-EXIT.
      *
       9100-ERRORES-EXIT.
       EXIT.
