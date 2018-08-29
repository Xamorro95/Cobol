      ***************************************************************** 
      *                                                               * 
      * PROGRAMA DE XXXXXXXXXXXXXXXX                                  * 
      *                                                               * 
      * ESTE PROGRAMA GENERA XXXXXXXXXXXXXXXXX                        * 
      ***************************************************************** 
      *
       IDENTIFICATION DIVISION.                                         
       PROGRAM-ID.    XXXXXX.
      *
      ***************************************************************** 
      * ENVIROMENT DIVISION                                           * 
      ***************************************************************** 
       ENVIRONMENT DIVISION.                                            
      *
       CONFIGURATION SECTION.                                           
      *
       INPUT-OUTPUT SECTION.                                            
      *
       FILE-CONTROL.                                                                                                                               
      *
      ***************************************************************** 
      * DATA DIVISION                                                 * 
      ***************************************************************** 
      *
       DATA DIVISION.                                                                                                                           
       FILE SECTION.                                                    
      *                                                                         
      ***************************************************************** 

      * WORKING STORAGE SECTION                                       * 

      ***************************************************************** 

       WORKING-STORAGE SECTION.                                         

                                                                                  

      ***************************************************************** * 

      **              VARIABLES  FICHERO ENTRADA                      * * 

      ***************************************************************** * 

                                                                         

      ***************************************************************** * 

      **              VARIABLES  FICHERO SALIDA                       * * 

      ***************************************************************** * 

                                                                                  

      ***************************************************************** * 

      **              SWITCHES                                        * * 

      ***************************************************************** * 

                                                                         

      ***************************************************************** * 

      **              CONSTANTES                                      * * 

      ***************************************************************** * 

                                                                         

      ***************************************************************** * 

      **              VARIABLES AUXILIARES                            * * 

      ***************************************************************** * 

                                                                         

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

                                                                                  

       1000-INICIO-EXIT.                                                

       EXIT.                                                        

                                                                         

      *****************************************************             

      * PROCESO                                           *             

      *****************************************************             

       3000-PROCESO.                                                    

                                                                                  

       3000-PROCESO-EXIT.                                               

       EXIT.                                                        

                                                                         

      *****************************************************             

      * FIN                                               *             

      *****************************************************             

       8000-FIN.                                                        

                                                                         

                                                                                  

           STOP RUN.                                                            

                                                                                  

       8000-FIN-EXIT.                                                   

       EXIT.                                                        

                                                                         
