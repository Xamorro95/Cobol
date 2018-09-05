      *****************************************************************
      *           PROGRAMA DE MIGUEL ANTONIO CHAMORRO MARTINEZ        *
      *                                                               *
      *  ESTE PROGRAMA CARGGA UN ARCHIVO Y REALIZAMOS OPERACIONES     *
      *  SOBRE DICHO FICHERO LLAMANDO A UNA RUTINA                    *
      *                                                               *
      *****************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID.    COBOL900.
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
      *****************************************************************
      *                    DATA DIVISION                              *
      *****************************************************************
       DATA DIVISION.
       FILE SECTION.
       FD FICHERO.
       01  REG-EMPLE                              PIC X(61).
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
      *               VARIABLES  DE COMUNICACION                      *
      *****************************************************************
       COPY RUTEMPL1.
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
      *               CONSTANTES Y LITERALES                           *
      *****************************************************************
       01  LT-LITERALES.
           05  LT-FICHERO1          PIC X(16) VALUE "FICHERO.EMPLEADO".
           05  LT-OPEN              PIC X(4) VALUE "OPEN".
           05  LT-READ              PIC X(4) VALUE "READ".
           05  LT-CLOSE             PIC X(5) VALUE "CLOSE".
           05  LT-WRITE             PIC X(5) VALUE "WRITE".
           05  LT-RUTINA            PIC X(15) VALUE "RUTEMPLE".
      *
      *****************************************************************
      *               VARIABLES AUXILIARES                            *
      *****************************************************************
       01  WS-VARIABLES.
           05  WS-FILE-STATUS                       PIC XX.
           05  WS-FICHERO-ERROR                     PIC X(16).
           05  WS-PARRAFO-ERROR                     PIC X(20).
           05  WS-OPERACION-ERROR                   PIC X(10).
      *
      *****************************************************************
      *               CONTADORES                                      *
      *****************************************************************
       01  WS-CONTADORES.
           05 WC-CONTADOR                        PIC 9(2).
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

      *ABRO EL FICHERO Y COMPRUEBO SI HAY ERRORES
           OPEN INPUT FICHERO.
           IF WS-FILE-STATUS NOT = 00
                MOVE '1000' TO WS-PARRAFO-ERROR
                MOVE LT-OPEN TO WS-OPERACION-ERROR
                MOVE LT-FICHERO1 TO WS-FICHERO-ERROR
                PERFORM 9100-ERRORES
                THRU 9100-ERRORES-EXIT
           END-IF.
      *
      *LLAMAMOS AL PERFON DE LECTURA DEL FICHERO
           PERFORM  9200-LEER-FICHERO
             THRU  9200-LEER-FICHERO-EXIT.
       1000-INICIO-EXIT.
       EXIT.
      *

      *****************************************************************
      *             PROCESO                                           *
      *****************************************************************
       3000-PROCESO.

      *LEE REGISTROS DEL FICHERO EN PERFORM LEER
           PERFORM  9200-LEER-FICHERO
             THRU  9200-LEER-FICHERO-EXIT.

      *LLAMAMOS AL PERFORM DE RUTINA
           PERFORM 9300-RUTINA
             THRU 9300-RUTINA-EXIT.
      *
      *SI DEVUELVE TODO CORRECTO AÑADE UNA LINEA AL CONTADOR PRINCIPAL
           ADD 1 TO WC-CONTADOR
           END-ADD.
      *
       3000-PROCESO-EXIT.
       EXIT.
      *****************************************************************
      *             FIN                                               *
      *****************************************************************
       8000-FIN.
      *SI NO SE PRODUCEN ERRORES DURANTE EL PROGRAMA MOSTRAMOS
      *LAS ESTADISTICAS DEL MISMO
           IF NO-ERROR
                 DISPLAY "******************************************"
                 END-DISPLAY
                 DISPLAY "      ESTADISTICAS DE PROGRAMA"
                 END-DISPLAY
                 DISPLAY "******************************************"
                 END-DISPLAY
                 DISPLAY "REGISTROS POR FICHERO:"
                 END-DISPLAY
                 DISPLAY "**LEIDOS FICHERO ENTRADA: " WC-CONTADOR
                 END-DISPLAY
                 DISPLAY "******************************************"
                 END-DISPLAY
           END-IF.
      *
           CLOSE FICHERO.
           DISPLAY "FICHERO CERRADO"
           END-DISPLAY.
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
      *MOSTRAMOS UN ANALISIS DEL ERROR PRODUCIDO
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
      *LLAMAMOS AL PERFORM QUE CIERRA EL PROGRAMA
           PERFORM 8000-FIN
              THRU 8000-FIN-EXIT.
      *
       9100-ERRORES-EXIT.
       EXIT.
      *
      *****************************************************************
      *               LEE EL FICHERO                                  *
      *****************************************************************
       9200-LEER-FICHERO.
      *LEO UN REGISTRO DEL FICHERO  Y COMPRUEBA ERRORES
           READ FICHERO RECORD INTO WS-REG-EMPLEADO
              AT END SET FIN-FICHERO TO TRUE.
      *
      *CONTROL DE ERRORES DE LECTURA
           IF WS-FILE-STATUS = 00
               CONTINUE
           ELSE
               IF WS-FILE-STATUS = 10 AND WC-CONTADOR > 0
                  CONTINUE
               ELSE
                  MOVE "9200-LEER" TO WS-PARRAFO-ERROR
                  MOVE LT-READ TO WS-OPERACION-ERROR
                  PERFORM 9100-ERRORES
                   THRU 9100-ERRORES-EXIT
           END-IF.
      *
      *LLAMAMOS AL PERFORM DE RUTINA
           PERFORM 9300-RUTINA
             THRU 9300-RUTINA-EXIT.
      *
       9200-LEER-FICHERO-EXIT.
       EXIT.
      *
      *****************************************************************
      *                LLAMADA A RUTINA                               *
      *****************************************************************
       9300-RUTINA.
      *INICIALIZAMOS EL CONTENIDO DE LAS VARIABLES COMPARTIDAS
           INITIALIZE CP100-RUTEMPL1.
      *
      *MOVEMOS EL CONTENIDO DE LAS VARIABLES NECESARIAS A LAS VARIABLES
      *COMPARTIDAS
           MOVE WS-EMPLE-CODIGO TO CP100I-EMPNO.
      *
      *LLAMAMOS A LA RUTINA
           CALL LT-RUTINA USING CP100-RUTEMPL1.
      *
      *MOSTRAMOS LA SALIDA DE LA RUTINA
           DISPLAY "EMPLEADO: " WS-EMPLE-CODIGO.
           DISPLAY "GENERO:   " CP100O-GENRE.
           DISPLAY "**********************".
      *
       9300-RUTINA-EXIT.
       EXIT.
