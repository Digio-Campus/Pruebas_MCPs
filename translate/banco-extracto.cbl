      *================================================================*
      * PROGRAMA: BANCO-EXTRACTO
      * DESCRIPCION: Genera un extracto de movimientos bancarios.
      *              Muestra ingresos, gastos y saldo final.
      * AUTOR: Proyecto MCPs
      * FECHA: 2026-02-18
      *================================================================*
      *
      *-------- BLOQUE 1: IDENTIFICATION DIVISION -------------------*
       IDENTIFICATION DIVISION.
       PROGRAM-ID. BANCO-EXTRACTO.
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
      *--- Datos de cuenta ---
       01 WS-NUMERO-CUENTA       PIC X(20).
       01 WS-TITULAR              PIC X(40).
       01 WS-SALDO-INICIAL        PIC S9(10)V99 VALUE 5000.00.
       01 WS-SALDO-FINAL          PIC S9(10)V99 VALUE 0.
      *
      *--- Totales ---
       01 WS-TOTAL-INGRESOS       PIC 9(10)V99 VALUE 0.
       01 WS-TOTAL-GASTOS         PIC 9(10)V99 VALUE 0.
       01 WS-NUM-MOVIMIENTOS      PIC 9(3) VALUE 0.
      *
      *--- Tabla de movimientos (max 50) ---
       01 WS-TABLA-MOVIMIENTOS.
          05 WS-MOVIMIENTO OCCURS 50 TIMES.
             10 WS-MOV-FECHA       PIC X(10).
             10 WS-MOV-CONCEPTO    PIC X(30).
             10 WS-MOV-TIPO        PIC X(1).
             10 WS-MOV-IMPORTE     PIC 9(8)V99.
      *
      *--- Variables de control ---
       01 WS-IDX                   PIC 9(3) VALUE 0.
       01 WS-FECHA-ACTUAL          PIC X(10).
      *
      *-------- BLOQUE 4: PROCEDURE DIVISION ------------------------*
       PROCEDURE DIVISION.
      *
       0000-PRINCIPAL.
           PERFORM 1000-INICIALIZAR
           PERFORM 2000-CARGAR-MOVIMIENTOS-SIMULADOS
           PERFORM 3000-CALCULAR-TOTALES
           PERFORM 4000-MOSTRAR-EXTRACTO
           PERFORM 9000-FINALIZAR
           STOP RUN.
      *
       1000-INICIALIZAR.
           ACCEPT WS-FECHA-ACTUAL FROM DATE YYYYMMDD
           MOVE "ES1234567890123456" TO WS-NUMERO-CUENTA
           MOVE "GARCIA LOPEZ, MARIA" TO WS-TITULAR
           DISPLAY "=========================================="
           DISPLAY "   EXTRACTO DE MOVIMIENTOS"
           DISPLAY "   Fecha: " WS-FECHA-ACTUAL
           DISPLAY "==========================================".
      *
       2000-CARGAR-MOVIMIENTOS-SIMULADOS.
           MOVE 8 TO WS-NUM-MOVIMIENTOS
      *
           MOVE "2026-02-01" TO WS-MOV-FECHA(1)
           MOVE "NOMINA FEBRERO"  TO WS-MOV-CONCEPTO(1)
           MOVE "I" TO WS-MOV-TIPO(1)
           MOVE 2500.00 TO WS-MOV-IMPORTE(1)
      *
           MOVE "2026-02-03" TO WS-MOV-FECHA(2)
           MOVE "ALQUILER VIVIENDA" TO WS-MOV-CONCEPTO(2)
           MOVE "G" TO WS-MOV-TIPO(2)
           MOVE 850.00 TO WS-MOV-IMPORTE(2)
      *
           MOVE "2026-02-05" TO WS-MOV-FECHA(3)
           MOVE "SUPERMERCADO"    TO WS-MOV-CONCEPTO(3)
           MOVE "G" TO WS-MOV-TIPO(3)
           MOVE 125.50 TO WS-MOV-IMPORTE(3)
      *
           MOVE "2026-02-07" TO WS-MOV-FECHA(4)
           MOVE "TRANSFERENCIA RECIBIDA" TO WS-MOV-CONCEPTO(4)
           MOVE "I" TO WS-MOV-TIPO(4)
           MOVE 300.00 TO WS-MOV-IMPORTE(4)
      *
           MOVE "2026-02-10" TO WS-MOV-FECHA(5)
           MOVE "SEGURO COCHE"   TO WS-MOV-CONCEPTO(5)
           MOVE "G" TO WS-MOV-TIPO(5)
           MOVE 75.00 TO WS-MOV-IMPORTE(5)
      *
           MOVE "2026-02-12" TO WS-MOV-FECHA(6)
           MOVE "LUZ ELECTRICA"  TO WS-MOV-CONCEPTO(6)
           MOVE "G" TO WS-MOV-TIPO(6)
           MOVE 95.30 TO WS-MOV-IMPORTE(6)
      *
           MOVE "2026-02-15" TO WS-MOV-FECHA(7)
           MOVE "INGRESO EFECTIVO" TO WS-MOV-CONCEPTO(7)
           MOVE "I" TO WS-MOV-TIPO(7)
           MOVE 500.00 TO WS-MOV-IMPORTE(7)
      *
           MOVE "2026-02-18" TO WS-MOV-FECHA(8)
           MOVE "GASOLINERA"     TO WS-MOV-CONCEPTO(8)
           MOVE "G" TO WS-MOV-TIPO(8)
           MOVE 60.00 TO WS-MOV-IMPORTE(8).
      *
       3000-CALCULAR-TOTALES.
           MOVE 0 TO WS-TOTAL-INGRESOS
           MOVE 0 TO WS-TOTAL-GASTOS
           PERFORM VARYING WS-IDX FROM 1 BY 1
              UNTIL WS-IDX > WS-NUM-MOVIMIENTOS
              IF WS-MOV-TIPO(WS-IDX) = "I"
                 ADD WS-MOV-IMPORTE(WS-IDX)
                    TO WS-TOTAL-INGRESOS
              ELSE
                 ADD WS-MOV-IMPORTE(WS-IDX)
                    TO WS-TOTAL-GASTOS
              END-IF
           END-PERFORM
           COMPUTE WS-SALDO-FINAL =
              WS-SALDO-INICIAL + WS-TOTAL-INGRESOS
              - WS-TOTAL-GASTOS.
      *
       4000-MOSTRAR-EXTRACTO.
           DISPLAY " "
           DISPLAY "=========================================="
           DISPLAY "  EXTRACTO - " WS-NUMERO-CUENTA
           DISPLAY "  Titular: " WS-TITULAR
           DISPLAY "=========================================="
           DISPLAY "Saldo inicial: " WS-SALDO-INICIAL
           DISPLAY "------------------------------------------"
           DISPLAY "FECHA       CONCEPTO"
              "                TIPO  IMPORTE"
           DISPLAY "------------------------------------------"
           PERFORM VARYING WS-IDX FROM 1 BY 1
              UNTIL WS-IDX > WS-NUM-MOVIMIENTOS
              DISPLAY WS-MOV-FECHA(WS-IDX) " "
                 WS-MOV-CONCEPTO(WS-IDX) " "
                 WS-MOV-TIPO(WS-IDX) "  "
                 WS-MOV-IMPORTE(WS-IDX)
           END-PERFORM
           DISPLAY "------------------------------------------"
           DISPLAY "Total ingresos: " WS-TOTAL-INGRESOS
           DISPLAY "Total gastos:   " WS-TOTAL-GASTOS
           DISPLAY "SALDO FINAL:    " WS-SALDO-FINAL
           DISPLAY "==========================================".
      *
       9000-FINALIZAR.
           DISPLAY " "
           DISPLAY "Extracto generado correctamente.".
