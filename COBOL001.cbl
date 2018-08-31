*****************************************************************
      *                                                               *
      * PROGRAMA DE XXXXXXXXXXXXXXXX                                  *
      *                                                               *
      * ESTE PROGRAMA GENERA XXXXXXXXXXXXXXXXX                        *
      *****************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID.    XXXXXX.

      *****************************************************************
      * ENVIROMENT DIVISION                                           *
      *****************************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT FICHERO
           ASSIGN TO
      *    "/home/forma2/cobol/ficheros/fichero.empleado"
      *    "/home/forma2/cobol/ficheros/fichero.empleado.correcto"
      *    "/home/forma2/cobol/ficheros/fichero.empleado.cruce"
      *    "/home/forma2/cobol/ficheros/fichero.empleado.cruce.uno"
      *    "/home/forma2/cobol/ficheros/fichero.empleado.cruce.vacio"
      *    "/home/forma2/cobol/ficheros/fichero.empleado.cruce.raro"
             FILE STATUS IS WS-FILE-STATUS.
      *****************************************************************
      * DATA DIVISION                                                 *
      *****************************************************************
       DATA DIVISION.

       FILE SECTION.
       FD FICHERO.
       01  RG-EMPLE                              PIC X(61).
      *****************************************************************
      * WORKING STORAGE SECTION                                       *
      *****************************************************************
       WORKING-STORAGE SECTION.

      ***************************************************************** *
      **              VARIABLES  FICHERO ENTRADA                      * *
      ***************************************************************** *
       01  WS-REG-EMPLEADO.
           05  WS-EMPLE-CODIGO                   PIC X(6).
           05  WS-EMPLE-NOMBRE                   PIC X(12).
           05  WS-EMPLE-INICIAL                  PIC X(1).
           05  WS-EMPLE-APELLIDO                 PIC X(15).
           05  WS-EMPLE-DEPT                     PIC X(3).
           05  WS-EMPLE-SALARIO                  PIC 9(9)V99.
           05  WS-EMPLE-COMISION                 PIC 9(9)V99.
           05  WS-EMPLE-VACIO                    PIC XX.
      *
       01  WS-CONTADORES.
           05 WC-CONTADOR                        PIC 9(2).
           05 WC-CONTADOR1                       PIC 9(2).
           05 WC-CONTADOR2                       PIC 9(2).
           05 WC-CONTADOR3                       PIC 9(2).
           05 WC-CONTADOR4                       PIC 9(2).

      ***************************************************************** *
      **              VARIABLES  FICHERO SALIDA                       * *
      ***************************************************************** *

      ***************************************************************** *
      **              SWITCHES                                        * *
      ***************************************************************** *
       01  SW-SWITCHES.
           05  SW-FIN-FICHERO                    PIC X.
                88 FIN-FICHERO                   VALUE "Y".
                88 NO-FIN-FICHERO                VALUE "N".
      ***************************************************************** *
      **              CONSTANTES                                      * *
      ***************************************************************** *

      ***************************************************************** *
      **              VARIABLES AUXILIARES                            * *
      ***************************************************************** *
       01  WS-VARIABLES.
           05  WS-FILE-STATUS                       PIC XX.
           05  SW-ERROR                             PIC 9 VALUE "0".
      ***************************************************************** *
      **              PROCEDURE  DIVISION.                            * *
      ***************************************************************** *
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
      ***************************************************************** *
      **              INICIO                                          * *
      ***************************************************************** *
       1000-INICIO.
           INITIALIZE WS-CONTADORES.
      *
           OPEN INPUT FICHERO.
      *
           READ FICHERO RECORD INTO WS-REG-EMPLEADO
              AT END SET FIN-FICHERO TO TRUE.
      *        
           IF WS-FILE-STATUS = 00
               CONTINUE
           ELSE
                PERFORM 9100-ERRORES
                THRU 9100-ERRORES-EXIT
           END-IF.
      *
       1000-INICIO-EXIT.
       EXIT.
      *
      *****************************************************
      * PROCESO                                           *
      *****************************************************
       3000-PROCESO.
           DISPLAY "COD.EMPLE: " WS-EMPLE-CODIGO "  NOMBRE: "
                WS-EMPLE-NOMBRE.
      *
           IF WS-FILE-STATUS = 00
                 CONTINUE
           ELSE
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
           ADD 1 TO WC-CONTADOR.
      *
       3000-PROCESO-EXIT.
       EXIT.
      *
      *****************************************************
      * FIN                                               *
      *****************************************************
       8000-FIN.
           IF SW-ERROR = "0"
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
           STOP RUN.
       8000-FIN-EXIT.
       EXIT.
      *
       9100-ERRORES.
           MOVE 1 TO SW-ERROR.
      *     
           DISPLAY "************************************"
           DISPLAY "************************************"
           EVALUATE WS-FILE-STATUS
                WHEN 10
                     DISPLAY "EL FICHERO ESTA VACIO"
                 WHEN OTHER
                     DISPLAY "SE HA PRODUCIDO UN ERROR"
           END-EVALUATE.
      * 
           DISPLAY "*********************************"
           DISPLAY "**FILE STATUS: " WS-FILE-STATUS
           DISPLAY "*********************************"
      *
           PERFORM 8000-FIN
              THRU 8000-FIN-EXIT.
      *
       9100-ERRORES-EXIT.
       EXIT.
