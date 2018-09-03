      *****************************************************************
      *                                                               *
      *       PROGRAMA DE MIGUEL ANTONIO CHAMORRO MARTINEZ            *
      *                                                               *
      *          ESTE PROGRAMA CARGGA UN ARCHIVO Y REALIZAMOS         *
      *          OPERACIONES SOBRE DICHO FICHERO SACANDO DOS          *
      *          FICHEROS DE SALIDA                                   *
      *****************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID.    COBOL300.
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
           "/home/forma2/cobol/ficheros/fichero.empleado"
             FILE STATUS IS WS-FILE-STATUS.
      *
           SELECT SALIDA1
           ASSIGN TO "/home/forma2/cobol/ficheros/salida1"
             FILE STATUS IS WS-FILE-STATUS1.
      *
           SELECT SALIDA2
           ASSIGN TO "/home/forma2/cobol/ficheros/salida2"
             FILE STATUS IS WS-FILE-STATUS2.
      *
      *****************************************************************
      *                    DATA DIVISION                              *
      *****************************************************************
       DATA DIVISION.
       FILE SECTION.
       FD FICHERO.
       01  REG-EMPLE                              PIC X(61).
      *
       FD SALIDA1.
       01  REG-SALIDA1                            PIC X(59).
      *
       FD SALIDA2.
       01  REG-SALIDA2                            PIC X(55).
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
       COPY COPYSALIDA1.
       COPY COPYSALIDA2.
      *
      *****************************************************************
      *               SWITCHES                                        *
      *****************************************************************
       01  SW-SWITCHES.
           05  SW-FIN-FICHERO                    PIC X.
                88 FIN-FICHERO                   VALUE "1".
                88 NO-FIN-FICHERO                VALUE "0".
           05  SW-ERRORES                        PIC X VALUE 0.
                88 SI-ERROR                      VALUE "1".
                88 NO-ERROR                      VALUE "0".
      *
      *****************************************************************
      *               CONSTANTES Y LITERALES                          *
      *****************************************************************
       01  LT-LITERALES.
           05  LT-FICHERO1          PIC X(16) VALUE "FICHERO.EMPLEADO".
           05  LT-FICHERO2          PIC X(16) VALUE "SALIDA1".
           05  LT-FICHERO3          PIC X(16) VALUE "SALIDA2".
           05  LT-OPEN              PIC X(4) VALUE "OPEN".
           05  LT-READ              PIC X(4) VALUE "READ".
           05  LT-CLOSE             PIC X(5) VALUE "CLOSE".
           05  LT-WRITE             PIC X(5) VALUE "WRITE".
      *
      *****************************************************************
      *               VARIABLES AUXILIARES                            *
      *****************************************************************
       01  WS-VARIABLES.
           05  WS-FILE-STATUS                       PIC XX.
           05  WS-FILE-STATUS1                      PIC XX.
           05  WS-FILE-STATUS2                      PIC XX.
           05  WS-FICHERO-ERROR                     PIC X(16).
           05  WS-PARRAFO-ERROR                     PIC 9(4).
           05  WS-OPERACION-ERROR                   PIC X(5).
           05  WS-FILE-STATUS-ERROR                 PIC XX.
           05  WS-DATE                              PIC 9(8).
      *
      *****************************************************************
      *               CONTADORES                                      *
      *****************************************************************
       01  WS-CONTADORES.
           05 WC-CONTADOR                        PIC 9(2).
           05 WC-CONTADOR-EXIT1                  PIC 9(2).
           05 WC-CONTADOR-EXIT2                  PIC 9(2).
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
      *OBTENEMOS LA FECHA DEL SISTEMA
           ACCEPT WS-DATE FROM DATE YYYYMMDD.
      *
      *ESTABLECEMOS LOS DATOS DE INICIO PARA ERRORES
           MOVE 1000 TO WS-PARRAFO-ERROR
           MOVE LT-OPEN TO WS-OPERACION-ERROR
      *
      *ABRO EL FICHERO 1 Y COMPRUEBO SI HAY ERRORES
           OPEN INPUT FICHERO.
           IF WS-FILE-STATUS NOT = 00
                MOVE LT-FICHERO1 TO WS-FICHERO-ERROR
                MOVE WS-FILE-STATUS TO WS-FILE-STATUS-ERROR
                PERFORM 9100-ERRORES
                THRU 9100-ERRORES-EXIT
           END-IF.
      *
      *ABRO LA SALIDA 1 Y COMPRUEBO SI HAY ERRORES
           OPEN OUTPUT SALIDA1.
           IF WS-FILE-STATUS1 NOT = 00
                MOVE LT-FICHERO2 TO WS-FICHERO-ERROR
                MOVE WS-FILE-STATUS1 TO WS-FILE-STATUS-ERROR
                PERFORM 9100-ERRORES
                THRU 9100-ERRORES-EXIT
           END-IF.
      *
      *ABRO LA SALIDA 2 Y COMPRUEBO SI HAY ERRORES
           OPEN OUTPUT SALIDA2.
           IF WS-FILE-STATUS2 NOT = 00
                MOVE LT-FICHERO3 TO WS-FICHERO-ERROR
                MOVE WS-FILE-STATUS2 TO WS-FILE-STATUS-ERROR
                PERFORM 9100-ERRORES
                THRU 9100-ERRORES-EXIT
           END-IF.
      *
      *LEO EL PRIMER REGISTRO DEL FICHERO 1 Y COMPRUEBO ERRORES SI NO
      *ERRORES, ENVIO A BUCLE
           READ FICHERO RECORD INTO WS-REG-EMPLEADO
              AT END SET FIN-FICHERO TO TRUE.
           IF WS-FILE-STATUS = 00
               CONTINUE
           ELSE
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
      *LLAMAMOS AL PERFORM QUE EVALUA EL REGISTRO LEIDO
           PERFORM 3100-EVALUACION
              THRU 3100-EVALUACION-EXIT.
      *
      *LEE LA SIGUIENTE LINEA DEL FICHERO PRINCIPAL
           READ FICHERO RECORD INTO WS-REG-EMPLEADO
              AT END SET FIN-FICHERO TO TRUE.
      *
      *SI SE PRODUCE UN ERROR EN LA LECTURA ENVIA A PERFORM ERRORES
           IF WS-FILE-STATUS = 00 or WS-FILE-STATUS=10
                 CONTINUE
           ELSE
                MOVE 3000 TO WS-PARRAFO-ERROR
                MOVE LT-READ TO WS-OPERACION-ERROR
                MOVE WS-FILE-STATUS TO WS-FILE-STATUS-ERROR
                PERFORM 9100-ERRORES
                THRU 9100-ERRORES-EXIT
           END-IF.
      *AÑADE UNA LINEA AL CONTADOR PRINCIPAL
           ADD 1 TO WC-CONTADOR.
      *
       3000-PROCESO-EXIT.
       EXIT.
      *
      *****************************************************************
      *            EVALUACION DE REGISTROS                            *
      *****************************************************************
*      3100-EVALUACION.
      *HACE UNA EVALUACION DE LOS DATOS Y MANDA A DOS SALIDAS DISTINTAS
      *EN CADA ENVIO, COMPRUEBA ERRORES
           EVALUATE TRUE
                 WHEN WS-EMPLE-APELLIDO(1:1) < "M"
                      MOVE WS-EMPLE-NOMBRE   TO WS-NOMBRE-SALIDA1
                      MOVE WS-EMPLE-INICIAL  TO WS-INICIAL-SALIDA1
                      MOVE WS-EMPLE-APELLIDO TO WS-APELLIDO-SALIDA1
                      MOVE WS-EMPLE-SALARIO  TO WS-SALARIO-SALIDA1
                      MOVE WS-EMPLE-COMISION TO WS-COMISION-SALIDA1
                      MOVE WS-EMPLE-DEPT     TO WS-DEPT-SALIDA1
                      MOVE WS-DATE           TO WS-FECHA-SALIDA1
                      WRITE REG-SALIDA1 FROM WS-REG-SALIDA1
                         AFTER ADVANCING 1 LINES
                      IF WS-FILE-STATUS1 = 00
                           CONTINUE
                      ELSE
                           MOVE LT-FICHERO2 TO WS-FICHERO-ERROR
                           MOVE WS-FILE-STATUS1 TO WS-FILE-STATUS-ERROR
                           MOVE 3000 TO WS-PARRAFO-ERROR
                           MOVE LT-WRITE TO WS-OPERACION-ERROR
                           PERFORM 9100-ERRORES
                           THRU 9100-ERRORES-EXIT
                      END-IF
                      ADD 1 TO WC-CONTADOR-EXIT1
                 WHEN OTHER
                      MOVE WS-EMPLE-CODIGO   TO WS-CODIGO-SALIDA2
                      MOVE WS-EMPLE-NOMBRE   TO WS-NOMBRE-SALIDA2
                      MOVE WS-EMPLE-APELLIDO TO WS-APELLIDO-SALIDA2
                      MOVE WS-EMPLE-DEPT     TO WS-DEPT-SALIDA2
                      MOVE WS-EMPLE-SALARIO  TO WS-SALARIO-SALIDA2
                      MOVE WS-DATE           TO WS-FECHA-SALIDA2
                      WRITE REG-SALIDA2 FROM WS-REG-SALIDA2
                           AFTER ADVANCING 1 LINES
                      IF WS-FILE-STATUS2 = 00
                           CONTINUE
                      ELSE
                           MOVE LT-FICHERO3 TO WS-FICHERO-ERROR
                           MOVE WS-FILE-STATUS2 TO WS-FILE-STATUS-ERROR
                           MOVE 3000 TO WS-PARRAFO-ERROR
                           MOVE LT-WRITE TO WS-OPERACION-ERROR
                           PERFORM 9100-ERRORES
                           THRU 9100-ERRORES-EXIT
                      END-IF
                      ADD 1 TO WC-CONTADOR-EXIT2
           END-EVALUATE.
       3100-EVALUACION-EXIT.
       EXIT.
      *
      *****************************************************************
      *             FIN                                               *
      *****************************************************************
       8000-FIN.
      *SI NO SE PRODUCEN ERRORES DURANTE EL PROGRAMA MOSTRAMOS
      *LAS ESTADISTICAS DEL MISMO
           IF NO-ERROR
                 DISPLAY "******************************************"
                 DISPLAY "      ESTADISTICAS DE PROGRAMA"
                 DISPLAY "******************************************"
                 DISPLAY "REGISTROS POR FICHERO:"
                 DISPLAY "**LEIDOS FICHERO ENTRADA: " WC-CONTADOR
                 DISPLAY "**ESCRITOS FICHERO 1:     " WC-CONTADOR-EXIT1
                 DISPLAY "**ESCRITOS FICHERO 1:     " WC-CONTADOR-EXIT2
                 DISPLAY "******************************************"
           END-IF.
      *
           CLOSE FICHERO.
           CLOSE SALIDA1.
           CLOSE SALIDA2.
           DISPLAY "FICHEROS CERRADOS".
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
           EVALUATE WS-FILE-STATUS-ERROR
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
      *MOSTRAMOS UN ANALISIS DEL ERROR PRODUCIDO
           DISPLAY "*********************************".
           DISPLAY "*********************************".
           DISPLAY "ANALISIS DE ERROR: ".
           DISPLAY "*********************************".
           DISPLAY "****FILE STATUS: " WS-FILE-STATUS-ERROR.
           DISPLAY "****FICHERO:     " WS-FICHERO-ERROR.
           DISPLAY "****PARRAFO:     " WS-PARRAFO-ERROR.
           DISPLAY "****LINEA:       " WC-CONTADOR.
           DISPLAY "****OPERACION:   " WS-OPERACION-ERROR.
           DISPLAY "*********************************".
      *
      *LLAMAMOS AL PERFORM QUE CIERRA EL PROGRAMA
           PERFORM 8000-FIN
              THRU 8000-FIN-EXIT.
      *
       9100-ERRORES-EXIT.
       EXIT.
