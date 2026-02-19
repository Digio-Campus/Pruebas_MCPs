      *================================================================*
      * PROGRAMA: BANCO-INGRESOS
      * DESCRIPCION: Simula el ingreso de dinero en una cuenta bancaria.
      *              Permite registrar N ingresos y muestra la suma total.
      * AUTOR: Proyecto MCPs
      * FECHA: 2026-02-18
      *================================================================*
      *
      *-------- BLOQUE 1: IDENTIFICATION DIVISION -------------------*
       IDENTIFICATION DIVISION.
       PROGRAM-ID. BANCO-INGRESOS.
       AUTHOR. PROYECTO-MCPS.
      *
      *-------- BLOQUE 2: ENVIRONMENT DIVISION ----------------------*
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       REPOSITORY.
      *
      *-------- BLOQUE 3: DATA DIVISION -----------------------------*
       DATA DIVISION.
       WORKING-STORAGE SECTION.
      *
      *--- Variables de cuenta ---
       01 WS-NUMERO-CUENTA     PIC X(20).
       01 WS-TITULAR            PIC X(40).
      *
      *--- Variables de ingresos ---
       01 WS-NUM-INGRESOS       PIC 9(3) VALUE 0.
       01 WS-CONTADOR           PIC 9(3) VALUE 0.
       01 WS-IMPORTE-INGRESO    PIC 9(8)V99 VALUE 0.
       01 WS-SUMA-TOTAL         PIC 9(10)V99 VALUE 0.
      *
      *--- Variables de control ---
       01 WS-CONTINUAR          PIC X VALUE 'S'.
       01 WS-FECHA-ACTUAL       PIC X(10).
      *
      *--- Tabla de ingresos (max 100) ---
       01 WS-TABLA-INGRESOS.
          05 WS-INGRESO-ENTRY OCCURS 100 TIMES.
             10 WS-ING-IMPORTE  PIC 9(8)V99.
             10 WS-ING-CONCEPTO PIC X(30).
      *
      *-------- BLOQUE 4: PROCEDURE DIVISION ------------------------*
       PROCEDURE DIVISION.
      *
      *--- Parrafo principal ---
       0000-PRINCIPAL.
           PERFORM 1000-INICIALIZAR
           PERFORM 2000-PEDIR-DATOS-CUENTA
           PERFORM 3000-REGISTRAR-INGRESOS
           PERFORM 4000-CALCULAR-TOTAL
           PERFORM 5000-MOSTRAR-RESUMEN
           PERFORM 9000-FINALIZAR
           STOP RUN.
      *
      *--- Inicializar variables ---
       1000-INICIALIZAR.
           INITIALIZE WS-NUMERO-CUENTA
           INITIALIZE WS-TITULAR
           MOVE 0 TO WS-NUM-INGRESOS
           MOVE 0 TO WS-SUMA-TOTAL
           MOVE 0 TO WS-CONTADOR
           ACCEPT WS-FECHA-ACTUAL FROM DATE YYYYMMDD
           DISPLAY "=========================================="
           DISPLAY "   SISTEMA DE INGRESOS BANCARIOS"
           DISPLAY "   Fecha: " WS-FECHA-ACTUAL
           DISPLAY "==========================================".
      *
      *--- Pedir datos de la cuenta ---
       2000-PEDIR-DATOS-CUENTA.
           DISPLAY " "
           DISPLAY "Introduzca el numero de cuenta: "
           ACCEPT WS-NUMERO-CUENTA
           DISPLAY "Introduzca el nombre del titular: "
           ACCEPT WS-TITULAR.
      *
      *--- Bucle para registrar ingresos ---
       3000-REGISTRAR-INGRESOS.
           MOVE 'S' TO WS-CONTINUAR
           PERFORM UNTIL WS-CONTINUAR = 'N'
              ADD 1 TO WS-CONTADOR
              DISPLAY " "
              DISPLAY "--- Ingreso #" WS-CONTADOR " ---"
              DISPLAY "Importe del ingreso: "
              ACCEPT WS-IMPORTE-INGRESO
              DISPLAY "Concepto del ingreso: "
              ACCEPT WS-ING-CONCEPTO(WS-CONTADOR)
              MOVE WS-IMPORTE-INGRESO
                 TO WS-ING-IMPORTE(WS-CONTADOR)
              ADD 1 TO WS-NUM-INGRESOS
              DISPLAY "Desea registrar otro ingreso? (S/N): "
              ACCEPT WS-CONTINUAR
           END-PERFORM.
      *
      *--- Calcular suma total ---
       4000-CALCULAR-TOTAL.
           MOVE 0 TO WS-SUMA-TOTAL
           PERFORM VARYING WS-CONTADOR FROM 1 BY 1
              UNTIL WS-CONTADOR > WS-NUM-INGRESOS
              ADD WS-ING-IMPORTE(WS-CONTADOR)
                 TO WS-SUMA-TOTAL
           END-PERFORM.
      *
      *--- Mostrar resumen de ingresos ---
       5000-MOSTRAR-RESUMEN.
           DISPLAY " "
           DISPLAY "=========================================="
           DISPLAY "   RESUMEN DE INGRESOS"
           DISPLAY "=========================================="
           DISPLAY "Cuenta:  " WS-NUMERO-CUENTA
           DISPLAY "Titular: " WS-TITULAR
           DISPLAY "------------------------------------------"
           PERFORM VARYING WS-CONTADOR FROM 1 BY 1
              UNTIL WS-CONTADOR > WS-NUM-INGRESOS
              DISPLAY "  Ingreso #" WS-CONTADOR
                 ": " WS-ING-IMPORTE(WS-CONTADOR)
                 " - " WS-ING-CONCEPTO(WS-CONTADOR)
           END-PERFORM
           DISPLAY "------------------------------------------"
           DISPLAY "Numero de ingresos: " WS-NUM-INGRESOS
           DISPLAY "SUMA TOTAL:         " WS-SUMA-TOTAL
           DISPLAY "==========================================".
      *
      *--- Finalizar programa ---
       9000-FINALIZAR.
           DISPLAY " "
           DISPLAY "Operacion finalizada correctamente."
           DISPLAY "Gracias por usar el sistema bancario.".
