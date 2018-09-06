      *****************************************************************
      *                                                               *
      * PROGRAMA DE XXXXXXXXXXXXXXXX                                  *
      *                                                               *
      * ESTE PROGRAMA GENERA XXXXXXXXXXXXXXXXX                        *
      *****************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID.    COBOL350.
      *                                                               *
      *****************************************************************
      * ENVIROMENT DIVISION                                           *
      *****************************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
      *
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
      *
      *****************************************************************
      * DATA DIVISION                                                 *
      *****************************************************************
       DATA DIVISION.
      *                                                               *
       FILE SECTION.
      *                                                               *
      *****************************************************************
      * WORKING STORAGE SECTION                                       *
      *****************************************************************
       WORKING-STORAGE SECTION.
      *
      ******************************************************************
      **              VARIABLES DE TABLAS                              *
      ******************************************************************
       01  WT-TABLA-DEPART.
           COPY COPY_COD_DEPART.
           05 WT-REG-TABLA REDEFINES WS-DATOS-TABLA
              OCCURS 24 TIMES
              ASCENDING KEY IS WT-CODIGO-DEPART
              INDEXED BY I.
                10 WT-CODIGO-DEPART              PIC X(3).
                10 WT-NOMBRE-DEPART              PIC X(22).
       01  WS-VARIABLES.
           05 WS-SYSIN                           PIC X(3).
      *
      ***************************************************************** *
      **              PROCEDURE  DIVISION.                            * *
      ***************************************************************** *
       PROCEDURE DIVISION.
      *
           PERFORM 1000-INICIO
              THRU 1000-INICIO-EXIT.
      *
           PERFORM 3000-PROCESO
              THRU 3000-PROCESO-EXIT.
      *
           PERFORM 8000-FIN
              THRU 8000-FIN-EXIT.
      *
      ***************************************************************** *
      **              INICIO                                          * *
      ***************************************************************** *
       1000-INICIO.
      *
       1000-INICIO-EXIT.
       EXIT.
      *
      *****************************************************
      * PROCESO                                           *
      *****************************************************
       3000-PROCESO.
           DISPLAY "**************** FASE 1 ************************"
      *
      *FASE 1: MUEVO 7 A INDICE Y MOSTRAMOS CODIGO
           SET I TO 7.
           DISPLAY "CODIGO DE DEPARTAMENTO 7: " WT-CODIGO-DEPART(I).
      *FASE 1: MOVEMOS 17 A INDICE Y MOSTRAMOS NOMBRE
           SET I TO 17.
           DISPLAY "NOMBRE DE DEPARTAMENTO 17: " WT-NOMBRE-DEPART(I).
      *
           DISPLAY "**************** FASE 2 ************************"
      *
      *FASE 2: SOLICITAMOS UN CODIGO Y LO BUSCAMOS MEDIANTE SEARCH
           SET I TO 1.
           DISPLAY "INTRODUCE UN CODIGO DE DEPARTAMENTO: "
           ACCEPT WS-SYSIN FROM SYSIN.
           SEARCH WT-REG-TABLA
              AT END
                 DISPLAY "NO ENCONTRADO EL DEPARTAMENTO " WS-SYSIN
              WHEN WT-CODIGO-DEPART(I) = WS-SYSIN

                 DISPLAY "NOMBRE DE DEPARTAMENTO: " WT-NOMBRE-DEPART(I)
           END-SEARCH.
      *
           DISPLAY "**************** FASE 3 ************************"
      *
      *FASE 3: SOLICITAMOS UN CODIGO Y LO BUSCAMOS MEDIANTE SEARCH ALL
           DISPLAY "INTRODUCE UN CODIGO DE DEPARTAMENTO: "
           ACCEPT WS-SYSIN FROM SYSIN.
           SEARCH ALL WT-REG-TABLA
              AT END
                 DISPLAY "NO ENCONTRADO EL DEPARTAMENTO " WS-SYSIN
              WHEN WT-CODIGO-DEPART(I) = WS-SYSIN
                 DISPLAY "NOMBRE DE DEPARTAMENTO: " WT-NOMBRE-DEPART(I)
           END-SEARCH.
      *
       3000-PROCESO-EXIT.
       EXIT.
      *
      *****************************************************
      * FIN                                               *
      *****************************************************
       8000-FIN.
           STOP RUN.
       8000-FIN-EXIT.
       EXIT.
