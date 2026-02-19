      *================================================================*
      * PROGRAMA: BANCO-CONSULTA-SALDO
      * DESCRIPCION: Consulta el saldo de una cuenta bancaria.
      *              Muestra saldo disponible, retenido y total.
      * AUTOR: Proyecto MCPs
      * FECHA: 2026-02-18
      *================================================================*
      *
      *-------- BLOQUE 1: IDENTIFICATION DIVISION -------------------*
       IDENTIFICATION DIVISION.
       PROGRAM-ID. BANCO-CONSULTA-SALDO.
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
       01 WS-NUMERO-CUENTA      PIC X(20).
       01 WS-TITULAR             PIC X(40).
       01 WS-TIPO-CUENTA         PIC X(15).
      *
      *--- Variables de saldo ---
       01 WS-SALDO-DISPONIBLE    PIC S9(10)V99 VALUE 0.
       01 WS-SALDO-RETENIDO      PIC 9(10)V99 VALUE 0.
       01 WS-SALDO-TOTAL         PIC S9(10)V99 VALUE 0.
      *
      *--- Variables de control ---
       01 WS-FECHA-ACTUAL        PIC X(10).
       01 WS-HORA-ACTUAL         PIC X(8).
       01 WS-CUENTA-ENCONTRADA   PIC X VALUE 'N'.
      *
      *--- Simulacion de base de datos (3 cuentas) ---
       01 WS-DB-CUENTAS.
          05 WS-DB-CUENTA OCCURS 3 TIMES.
             10 WS-DB-NUM-CUENTA  PIC X(20).
             10 WS-DB-TITULAR     PIC X(40).
             10 WS-DB-TIPO        PIC X(15).
             10 WS-DB-SALDO-DISP  PIC S9(10)V99.
             10 WS-DB-SALDO-RET   PIC 9(10)V99.
      *
       01 WS-IDX                  PIC 9(2) VALUE 0.
      *
      *-------- BLOQUE 4: PROCEDURE DIVISION ------------------------*
       PROCEDURE DIVISION.
      *
       0000-PRINCIPAL.
           PERFORM 1000-INICIALIZAR
           PERFORM 2000-CARGAR-DATOS-SIMULADOS
           PERFORM 3000-PEDIR-CUENTA
           PERFORM 4000-BUSCAR-CUENTA
           IF WS-CUENTA-ENCONTRADA = 'S'
              PERFORM 5000-MOSTRAR-SALDO
           ELSE
              PERFORM 6000-CUENTA-NO-ENCONTRADA
           END-IF
           PERFORM 9000-FINALIZAR
           STOP RUN.
      *
       1000-INICIALIZAR.
           ACCEPT WS-FECHA-ACTUAL FROM DATE YYYYMMDD
           ACCEPT WS-HORA-ACTUAL FROM TIME
           DISPLAY "=========================================="
           DISPLAY "   CONSULTA DE SALDO BANCARIO"
           DISPLAY "   Fecha: " WS-FECHA-ACTUAL
              " Hora: " WS-HORA-ACTUAL
           DISPLAY "==========================================".
      *
       2000-CARGAR-DATOS-SIMULADOS.
           MOVE "ES1234567890123456" TO WS-DB-NUM-CUENTA(1)
           MOVE "GARCIA LOPEZ, MARIA" TO WS-DB-TITULAR(1)
           MOVE "CORRIENTE" TO WS-DB-TIPO(1)
           MOVE 15250.75 TO WS-DB-SALDO-DISP(1)
           MOVE 500.00 TO WS-DB-SALDO-RET(1)
      *
           MOVE "ES9876543210987654" TO WS-DB-NUM-CUENTA(2)
           MOVE "MARTINEZ RUIZ, PEDRO" TO WS-DB-TITULAR(2)
           MOVE "AHORRO" TO WS-DB-TIPO(2)
           MOVE 42000.00 TO WS-DB-SALDO-DISP(2)
           MOVE 0.00 TO WS-DB-SALDO-RET(2)
      *
           MOVE "ES5555666677778888" TO WS-DB-NUM-CUENTA(3)
           MOVE "FERNANDEZ DIAZ, ANA" TO WS-DB-TITULAR(3)
           MOVE "NOMINA" TO WS-DB-TIPO(3)
           MOVE 3200.50 TO WS-DB-SALDO-DISP(3)
           MOVE 150.00 TO WS-DB-SALDO-RET(3).
      *
       3000-PEDIR-CUENTA.
           DISPLAY " "
           DISPLAY "Introduzca el numero de cuenta: "
           ACCEPT WS-NUMERO-CUENTA.
      *
       4000-BUSCAR-CUENTA.
           MOVE 'N' TO WS-CUENTA-ENCONTRADA
           PERFORM VARYING WS-IDX FROM 1 BY 1
              UNTIL WS-IDX > 3
              IF WS-DB-NUM-CUENTA(WS-IDX) = WS-NUMERO-CUENTA
                 MOVE WS-DB-TITULAR(WS-IDX) TO WS-TITULAR
                 MOVE WS-DB-TIPO(WS-IDX) TO WS-TIPO-CUENTA
                 MOVE WS-DB-SALDO-DISP(WS-IDX)
                    TO WS-SALDO-DISPONIBLE
                 MOVE WS-DB-SALDO-RET(WS-IDX)
                    TO WS-SALDO-RETENIDO
                 COMPUTE WS-SALDO-TOTAL =
                    WS-SALDO-DISPONIBLE + WS-SALDO-RETENIDO
                 MOVE 'S' TO WS-CUENTA-ENCONTRADA
              END-IF
           END-PERFORM.
      *
       5000-MOSTRAR-SALDO.
           DISPLAY " "
           DISPLAY "=========================================="
           DISPLAY "   INFORMACION DE CUENTA"
           DISPLAY "=========================================="
           DISPLAY "Cuenta:     " WS-NUMERO-CUENTA
           DISPLAY "Titular:    " WS-TITULAR
           DISPLAY "Tipo:       " WS-TIPO-CUENTA
           DISPLAY "------------------------------------------"
           DISPLAY "Saldo disponible: " WS-SALDO-DISPONIBLE
           DISPLAY "Saldo retenido:   " WS-SALDO-RETENIDO
           DISPLAY "SALDO TOTAL:      " WS-SALDO-TOTAL
           DISPLAY "==========================================".
      *
       6000-CUENTA-NO-ENCONTRADA.
           DISPLAY " "
           DISPLAY "ERROR: Cuenta no encontrada."
           DISPLAY "Verifique el numero de cuenta e intente "
              "de nuevo.".
      *
       9000-FINALIZAR.
           DISPLAY " "
           DISPLAY "Consulta finalizada correctamente.".
