      *****************************************************************
      *  PROGRAMA DE MIGUEL ANTONIO CHAMORRO MARTINEZ                 *
      *                                                               *
      *  ESTE PROGRAMA CARGA DOS ARCHIVOS Y CRUZAMOS SU CONTENIDO     *
      *  ALMACENAMOS EN TRES ARCHIVOS SEGUN COINCIDEN O NO            *
      *                                                               *
      *****************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID.    COBOL500.
      *
      *****************************************************************
      *                  ENVIROMENT DIVISION                          *
      *****************************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
      *
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT FICHEROA
           ASSIGN TO
           "/home/forma2/cobol/ficheros/fichero.empleado"
             FILE STATUS IS WS-FILE-STATUS.
      *
           SELECT FICHEROB
           ASSIGN TO
           "/home/forma2/cobol/ficheros/fichero.empleado.cruce"
             FILE STATUS IS WS-FILE-STATUS.
      *
           SELECT SALIDA1
           ASSIGN TO "/home/forma2/cobol/ficheros/fichero.salida-AMBOS"
             FILE STATUS IS WS-FILE-STATUS.
      *
           SELECT SALIDA2
           ASSIGN TO "/home/forma2/cobol/ficheros/fichero.salidaA"
             FILE STATUS IS WS-FILE-STATUS.
      *
           SELECT SALIDA3
           ASSIGN TO "/home/forma2/cobol/ficheros/fichero.salidaB"
             FILE STATUS IS WS-FILE-STATUS.
      *
      *****************************************************************
      *                    DATA DIVISION                              *
      *****************************************************************
       DATA DIVISION.
       FILE SECTION.
       FD FICHEROA.
       01  REG-EMPLE                              PIC X(61).
      *
       FD FICHEROB.
       01  REG-EMPLE-CRUCE                        PIC X(61).
      *
       FD SALIDA1.
       01  REG-SALIDA1                            PIC X(122).
      *
       FD SALIDA2.
       01  REG-SALIDA2                            PIC X(61).
      *
       FD SALIDA3.
       01  REG-SALIDA3                            PIC X(61).
      *
      *****************************************************************
      *               WORKING STORAGE SECTION                         *
      *****************************************************************
       WORKING-STORAGE SECTION.
      *
      *****************************************************************
      *               VARIABLES  FICHERO ENTRADA                      *
      *****************************************************************
      *FICHERO ENTRADA 1
       01  WS-REG-EMPLEADO.
           05  WS-EMPLE-CODIGO                 PIC X(6).
           05  WS-EMPLE-NOMBRE                 PIC X(12).
           05  WS-EMPLE-INICIAL                PIC X(1).
           05  WS-EMPLE-APELLIDO               PIC X(15).
           05  WS-EMPLE-DEPT                   PIC X(3).
           05  WS-EMPLE-SALARIO                PIC 9(9)V99.
           05  WS-EMPLE-COMISION               PIC 9(9)V99.
           05  WS-EMPLE-VACIO                  PIC XX.
      *
      *FICHERO ENTRADA 2
       01  WS-REG-CRUCE-EMPLEADO.
           05  WS-EMPLE-CRUCE-CODIGO                 PIC X(6).
           05  WS-EMPLE-CRUCE-NOMBRE                 PIC X(12).
           05  WS-EMPLE-CRUCE-INICIAL                PIC X(1).
           05  WS-EMPLE-CRUCE-APELLIDO               PIC X(15).
           05  WS-EMPLE-CRUCE-DEPT                   PIC X(3).
           05  WS-EMPLE-CRUCE-SALARIO                PIC 9(9)V99.
           05  WS-EMPLE-CRUCE-COMISION               PIC 9(9)V99.
           05  WS-EMPLE-CRUCE-VACIO                  PIC XX.
      *
      *****************************************************************
      *               VARIABLES  FICHERO SALIDA                       *
      *****************************************************************
      *FICHERO DE SALIDA AMBOS
       01  WS-REG-AMBOS.
           05  WS-SAL-AMBOS-1-CODIGO                 PIC X(6).
           05  WS-SAL-AMBOS-1-NOMBRE                 PIC X(12).
           05  WS-SAL-AMBOS-1-INICIAL                PIC X(1).
           05  WS-SAL-AMBOS-1-APELLIDO               PIC X(15).
           05  WS-SAL-AMBOS-1-DEPT                   PIC X(3).
           05  WS-SAL-AMBOS-1-SALARIO                PIC 9(9)V99.
           05  WS-SAL-AMBOS-1-COMISION               PIC 9(9)V99.
           05  WS-SAL-AMBOS-1-VACIO                  PIC XX.
           05  WS-SAL-AMBOS-2-CODIGO                 PIC X(6).
           05  WS-SAL-AMBOS-2-NOMBRE                 PIC X(12).
           05  WS-SAL-AMBOS-2-INICIAL                PIC X(1).
           05  WS-SAL-AMBOS-2-APELLIDO               PIC X(15).
           05  WS-SAL-AMBOS-2-DEPT                   PIC X(3).
           05  WS-SAL-AMBOS-2-SALARIO                PIC 9(9)V99.
           05  WS-SAL-AMBOS-2-COMISION               PIC 9(9)V99.
           05  WS-SAL-AMBOS-2-VACIO                  PIC XX.
      *
      *FICHERO DE SALIDA A
       01  WS-REG-SALIDA1.
            05  WS-SAL1-CODIGO                 PIC X(6).
            05  WS-SAL1-NOMBRE                 PIC X(12).
            05  WS-SAL1-INICIAL                PIC X(1).
            05  WS-SAL1-APELLIDO               PIC X(15).
            05  WS-SAL1-DEPT                   PIC X(3).
            05  WS-SAL1-SALARIO                PIC 9(9)V99.
            05  WS-SAL1-COMISION               PIC 9(9)V99.
            05  WS-SAL1-VACIO                  PIC XX.
      *
      *FICHERO DE SALIDA B
       01  WS-REG-SALIDA2.
            05  WS-SAL2-CODIGO                 PIC X(6).
            05  WS-SAL2-NOMBRE                 PIC X(12).
            05  WS-SAL2-INICIAL                PIC X(1).
            05  WS-SAL2-APELLIDO               PIC X(15).
            05  WS-SAL2-DEPT                   PIC X(3).
            05  WS-SAL2-SALARIO                PIC 9(9)V99.
            05  WS-SAL2-COMISION               PIC 9(9)V99.
            05  WS-SAL2-VACIO                  PIC XX.
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
           05  LT-FICHERO1    PIC X(20) VALUE "FICHERO.EMPLEADO".
           05  LT-FICHERO2    PIC X(25) VALUE "FICHERO.EMPLEADO.CRUCE".
           05  LT-SALIDA1     PIC X(16) VALUE "SALIDA-AMBOS".
           05  LT-SALIDA2     PIC X(16) VALUE "SALIDA-A".
           05  LT-SALIDA3     PIC X(16) VALUE "SALIDA-B".
           05  LT-OPEN        PIC X(4) VALUE "OPEN".
           05  LT-READ        PIC X(4) VALUE "READ".
           05  LT-CLOSE       PIC X(5) VALUE "CLOSE".
           05  LT-WRITE       PIC X(5) VALUE "WRITE".
      *
      *****************************************************************
      *               VARIABLES AUXILIARES                            *
      *****************************************************************
       01  WS-VARIABLES.
           05  WS-FILE-STATUS                    PIC XX.
           05  WS-FICHERO-ERROR                  PIC X(25).
           05  WS-PARRAFO-ERROR                  PIC 9(4).
           05  WS-OPERACION-ERROR                PIC X(5).
      *
      *****************************************************************
      *               CONTADORES                                      *
      *****************************************************************
       01  WS-CONTADORES.
           05 WC-CONTADOR                      PIC 9(2).
           05 WC-CONTADOR-A                      PIC 9(2).
           05 WC-CONTADOR-B                      PIC 9(2).
           05 WC-CONTADOR-S1                     PIC 9(2).
           05 WC-CONTADOR-S2                     PIC 9(2).
           05 WC-CONTADOR-S3                     PIC 9(2).
      *
      *****************************************************************
      *               PROCEDURE  DIVISION.                            *
      *****************************************************************
       PROCEDURE DIVISION.
      *
           PERFORM 1000-INICIO
              THRU 1000-INICIO-EXIT.
      *
           PERFORM 1100-APERTURA-FICHEROS
              THRU 1100-APERTURA-FICHEROS-EXIT.
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
           INITIALIZE WS-CONTADORES
                      WS-REG-EMPLEADO
                      WS-REG-CRUCE-EMPLEADO
                      WS-REG-AMBOS
                      WS-REG-SALIDA1
                      WS-REG-SALIDA2
                      WS-VARIABLES.
      *
       1000-INICIO-EXIT.
       EXIT.
      *
      *****************************************************************
      *             APERTURA DE FICHEROS CON CONTROL DE ERROR         *
      *****************************************************************
       1100-APERTURA-FICHEROS.
      *ESTABLECEMOS LOS DATOS DE PARRAFO Y OPERACION PARA ERRORES
           MOVE 1100 TO WS-PARRAFO-ERROR
           MOVE LT-OPEN TO WS-OPERACION-ERROR
      *
      *ABRO EL FICHERO A Y COMPRUEBO SI HAY ERRORES
           OPEN INPUT FICHEROA.
           IF WS-FILE-STATUS NOT = 00
                MOVE LT-FICHERO1 TO WS-FICHERO-ERROR
                PERFORM 9100-ERRORES
                   THRU 9100-ERRORES-EXIT
           END-IF.
      *
      *ABRO EL FICHERO B Y COMPRUEBO SI HAY ERRORES
           OPEN INPUT FICHEROB.
           IF WS-FILE-STATUS NOT = 00
                MOVE LT-FICHERO2 TO WS-FICHERO-ERROR
                PERFORM 9100-ERRORES
                   THRU 9100-ERRORES-EXIT
           END-IF.
      *
      *ABRO LA SALIDA 1 Y COMPRUEBO SI HAY ERRORES
           OPEN OUTPUT SALIDA1.
           IF WS-FILE-STATUS NOT = 00
                MOVE LT-SALIDA1 TO WS-FICHERO-ERROR
                PERFORM 9100-ERRORES
                   THRU 9100-ERRORES-EXIT
           END-IF.
      *
      *ABRO LA SALIDA 2 Y COMPRUEBO SI HAY ERRORES
           OPEN OUTPUT SALIDA2.
           IF WS-FILE-STATUS NOT = 00
                MOVE LT-SALIDA2 TO WS-FICHERO-ERROR
                PERFORM 9100-ERRORES
                   THRU 9100-ERRORES-EXIT
           END-IF.
      *
      *ABRO LA SALIDA 3 Y COMPRUEBO SI HAY ERRORES
           OPEN OUTPUT SALIDA3.
           IF WS-FILE-STATUS NOT = 00
                MOVE LT-SALIDA3 TO WS-FICHERO-ERROR
                PERFORM 9100-ERRORES
                   THRU 9100-ERRORES-EXIT
           END-IF.
      *
      *SI NO SE PRODUCEN ERRORES LEEMOS EL PRIMER REGISTRO DE A
           PERFORM 9200-LEER-FICHEROA
              THRU 9200-LEER-FICHEROA-EXIT.
      *
      *SI NO SE PRODUCEN ERRORES LEEMOS EL PRIMER REGISTRO DE B
           PERFORM 9300-LEER-FICHEROB
              THRU 9300-LEER-FICHEROB-EXIT.
      *
       1100-APERTURA-FICHEROS-EXIT.
       EXIT.
      *
      *****************************************************************
      *             PROCESO                                           *
      *****************************************************************
       3000-PROCESO.
           IF WS-EMPLE-CODIGO = WS-EMPLE-CRUCE-CODIGO
      *       ESCRITURA EN LA SALIDA DE AMBOS
              PERFORM 3100-ESCRITURA-SALIDA1
                 THRU 3100-ESCRITURA-SALIDA1-EXIT
      *       LEEMOS UNA LINEA FICHERO A
              PERFORM 9200-LEER-FICHEROA
                 THRU 9200-LEER-FICHEROA-EXIT
      *       LEEMOS UNA LINEA FICHERO B
              PERFORM 9300-LEER-FICHEROB
                 THRU 9300-LEER-FICHEROB-EXIT
           ELSE
              IF WS-EMPLE-CODIGO < WS-EMPLE-CRUCE-CODIGO
      *          ESCRITURA EN LA SALIDA 2
                 PERFORM 3200-ESCRITURA-SALIDA2
                    THRU 3200-ESCRITURA-SALIDA2-EXIT
      *          LEEMOS UNA LINEA FICHERO A
                 PERFORM 9200-LEER-FICHEROA
                    THRU 9200-LEER-FICHEROA-EXIT
              ELSE
      *          ESCRITURA EN LA SALIDA 3
                 PERFORM 3300-ESCRITURA-SALIDA3
                    THRU 3300-ESCRITURA-SALIDA3-EXIT
      *          LEEMOS UNA LINEA FICHERO B
                 PERFORM 9300-LEER-FICHEROB
                    THRU 9300-LEER-FICHEROB-EXIT
              END-IF
           END-IF.

       3000-PROCESO-EXIT.
       EXIT.
      *
      *****************************************************************
      *             ESCRITURA SALIDA AMBOS                            *
      *****************************************************************
       3100-ESCRITURA-SALIDA1.
      *MOVEMOS LAS VARIABLES DEL FICHERO A A SALIDA 1
           MOVE WS-EMPLE-CODIGO         TO WS-SAL-AMBOS-1-CODIGO.
           MOVE WS-EMPLE-NOMBRE         TO WS-SAL-AMBOS-1-NOMBRE.
           MOVE WS-EMPLE-INICIAL        TO WS-SAL-AMBOS-1-INICIAL.
           MOVE WS-EMPLE-APELLIDO       TO WS-SAL-AMBOS-1-APELLIDO.
           MOVE WS-EMPLE-DEPT           TO WS-SAL-AMBOS-1-DEPT.
           MOVE WS-EMPLE-SALARIO        TO WS-SAL-AMBOS-1-SALARIO.
           MOVE WS-EMPLE-COMISION       TO WS-SAL-AMBOS-1-COMISION.
      *
      *MOVEMOS LAS VARIABLES DEL FICHERO B A SALIDA 1
           MOVE WS-EMPLE-CRUCE-CODIGO   TO WS-SAL-AMBOS-2-CODIGO.
           MOVE WS-EMPLE-CRUCE-NOMBRE   TO WS-SAL-AMBOS-2-NOMBRE.
           MOVE WS-EMPLE-CRUCE-INICIAL  TO WS-SAL-AMBOS-2-INICIAL.
           MOVE WS-EMPLE-CRUCE-APELLIDO TO WS-SAL-AMBOS-2-APELLIDO.
           MOVE WS-EMPLE-CRUCE-DEPT     TO WS-SAL-AMBOS-2-DEPT.
           MOVE WS-EMPLE-CRUCE-SALARIO  TO WS-SAL-AMBOS-2-SALARIO.
           MOVE WS-EMPLE-CRUCE-COMISION TO WS-SAL-AMBOS-2-COMISION.
      *
      *ESCRIBIMOS LA SALIDA EN EL FICHERO DE AMBOS Y COMPROBAMOS ERROR
           WRITE REG-SALIDA1 FROM WS-REG-AMBOS
             AFTER ADVANCING 1 LINES
           IF WS-FILE-STATUS = 00
              CONTINUE
           ELSE
              MOVE LT-SALIDA1 TO WS-FICHERO-ERROR
              MOVE 3100 TO WS-PARRAFO-ERROR
              MOVE LT-WRITE TO WS-OPERACION-ERROR
              PERFORM 9100-ERRORES
                 THRU 9100-ERRORES-EXIT
           END-IF.
      *
      *AUMENTAMOS EN 1 EL CONTADOR DE REGISTROS ESCRITOS SALIDA AMBOS
           ADD 1 TO WC-CONTADOR-S1.
      *
       3100-ESCRITURA-SALIDA1-EXIT.
       EXIT.
      *
      *****************************************************************
      *             ESCRITURA SALIDA 2 DEL FICHERO A                  *
      *****************************************************************
       3200-ESCRITURA-SALIDA2.
      *MOVEMOS LAS VARIABLES DEL FICHERO A A SALIDA 2
           MOVE WS-REG-EMPLEADO TO WS-REG-SALIDA1.
      *
      *ESCRIBIMOS LA SALIDA EN EL FICHERO DE AMBOS Y COMPROBAMOS ERROR
           WRITE REG-SALIDA2 FROM WS-REG-SALIDA1.
           IF WS-FILE-STATUS = 00
              CONTINUE
           ELSE
              MOVE LT-SALIDA2 TO WS-FICHERO-ERROR
              MOVE 3100 TO WS-PARRAFO-ERROR
              MOVE LT-WRITE TO WS-OPERACION-ERROR
              PERFORM 9100-ERRORES
                 THRU 9100-ERRORES-EXIT
           END-IF.
      *
      *AUMENTAMOS EN 1 EL CONTADOR DE REGISTROS ESCRITOS SALIDA 2
           ADD 1 TO WC-CONTADOR-S2.
      *
       3200-ESCRITURA-SALIDA2-EXIT.
       EXIT.
      *
      *****************************************************************
      *             ESCRITURA SALIDA 3 DEL FICHERO 3                  *
      *****************************************************************
       3300-ESCRITURA-SALIDA3.
      *MOVEMOS LAS VARIABLES DEL FICHERO B A SALIDA 3
           MOVE WS-REG-CRUCE-EMPLEADO TO WS-REG-SALIDA2.
      *
      *ESCRIBIMOS LA SALIDA EN EL FICHERO DE AMBOS Y COMPROBAMOS ERROR
           WRITE REG-SALIDA3 FROM WS-REG-SALIDA2.
           IF WS-FILE-STATUS = 00
              CONTINUE
           ELSE
              MOVE LT-SALIDA3 TO WS-FICHERO-ERROR
              MOVE 3200 TO WS-PARRAFO-ERROR
              MOVE LT-WRITE TO WS-OPERACION-ERROR
              PERFORM 9100-ERRORES
                 THRU 9100-ERRORES-EXIT
           END-IF.
      *
      *AUMENTAMOS EN 1 EL CONTADOR DE REGISTROS ESCRITOS SALIDA 3
           ADD 1 TO WC-CONTADOR-S3.
      *
       3300-ESCRITURA-SALIDA3-EXIT.
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
                 DISPLAY "**LEIDOS FICHERO ENTRADA A: " WC-CONTADOR-A
                 DISPLAY "**LEIDOS FICHERO ENTRADA B: " WC-CONTADOR-B
                 DISPLAY "**ESCRITOS FICHERO 1:"   WC-CONTADOR-S1"(X2)"
                 DISPLAY "**ESCRITOS FICHERO 2:     "   WC-CONTADOR-S2
                 DISPLAY "**ESCRITOS FICHERO 3:     "   WC-CONTADOR-S3
                 DISPLAY "******************************************"
           END-IF.
      *
           CLOSE FICHEROA.
           CLOSE FICHEROB.
           CLOSE SALIDA1.
           CLOSE SALIDA2.
           CLOSE SALIDA3.
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
      *                LEECTURA DEL FICHERO A                         *
      *****************************************************************
       9200-LEER-FICHEROA.
      *LEE UN REGISTRO DEL FICHERO A
           READ FICHEROA RECORD INTO WS-REG-EMPLEADO
              AT END MOVE HIGH-VALUE TO WS-REG-EMPLEADO.
      *
      *SI SE PRODUCE UN ERROR EN LA LECTURA ENVIA A PERFORM ERRORES
           IF WS-FILE-STATUS = 00
              CONTINUE
           ELSE
              IF WS-FILE-STATUS = 10 AND WC-CONTADOR-A > 0
                 CONTINUE
              ELSE
                 MOVE 9200 TO WS-PARRAFO-ERROR
                 MOVE LT-READ TO WS-OPERACION-ERROR
                 PERFORM 9100-ERRORES
                    THRU 9100-ERRORES-EXIT
              END-IF
           END-IF.
      *
      *COMPROBAMOS SI HA LLEGADO AL FINAL DEL FICHERO
           IF WS-REG-CRUCE-EMPLEADO = HIGH-VALUE AND
              WS-REG-EMPLEADO = HIGH-VALUE
              SET FIN-FICHERO TO TRUE
           ELSE
      *AUMENTAMOS EN 1 EL CONTADOR DE REG LEIDOS DE FICHERO A SI NO FIN
              IF WS-REG-EMPLEADO = HIGH-VALUE
                 CONTINUE
              ELSE
                 ADD 1 TO WC-CONTADOR-A
              END-IF
           END-IF.
       9200-LEER-FICHEROA-EXIT.
       EXIT.
      *
      *****************************************************************
      *                LEECTURA DEL FICHERO B                         *
      *****************************************************************
       9300-LEER-FICHEROB.
      *LEE UN REGISTRO DEL FICHERO A
           READ FICHEROB RECORD INTO WS-REG-CRUCE-EMPLEADO
              AT END MOVE HIGH-VALUE TO WS-REG-CRUCE-EMPLEADO.
      *
      *SI SE PRODUCE UN ERROR EN LA LECTURA ENVIA A PERFORM ERRORES
           IF WS-FILE-STATUS = 00
              CONTINUE
           ELSE
              IF WS-FILE-STATUS = 10 AND WC-CONTADOR-A > 0
                 CONTINUE
              ELSE
                 MOVE 9300 TO WS-PARRAFO-ERROR
                 MOVE LT-READ TO WS-OPERACION-ERROR
                 PERFORM 9100-ERRORES
                    THRU 9100-ERRORES-EXIT
              END-IF
           END-IF.
      *
      *COMPROBAMOS SI HA LLEGADO AL FINAL DEL FICHERO
           IF WS-REG-CRUCE-EMPLEADO = HIGH-VALUE AND
              WS-REG-EMPLEADO = HIGH-VALUE
              SET FIN-FICHERO TO TRUE
           ELSE
      *AUMENTAMOS EN 1 EL CONTADOR DE REG LEIDOS DE FICHERO B SI NO FIN
              IF WS-REG-CRUCE-EMPLEADO = HIGH-VALUE
                 CONTINUE
              ELSE
                 ADD 1 TO WC-CONTADOR-B
              END-IF
           END-IF.
       9300-LEER-FICHEROB-EXIT.
       EXIT.
