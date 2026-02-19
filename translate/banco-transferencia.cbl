      *================================================================*
      * PROGRAMA: BANCO-TRANSFERENCIA
      * DESCRIPCION: Simula una transferencia bancaria entre dos cuentas.
      *              Valida saldo suficiente y registra el movimiento.
      * AUTOR: Proyecto MCPs
      * FECHA: 2026-02-18
      *================================================================*
      *
      *-------- BLOQUE 1: IDENTIFICATION DIVISION -------------------*
       IDENTIFICATION DIVISION.
       PROGRAM-ID. BANCO-TRANSFERENCIA.
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
      *--- Datos de la transferencia ---
       01 WS-CUENTA-ORIGEN       PIC X(20).
       01 WS-CUENTA-DESTINO      PIC X(20).
       01 WS-IMPORTE-TRANSFER    PIC 9(8)V99 VALUE 0.
       01 WS-CONCEPTO            PIC X(50).
      *
      *--- Saldos ---
       01 WS-SALDO-ORIGEN        PIC S9(10)V99 VALUE 25000.00.
       01 WS-SALDO-DESTINO       PIC S9(10)V99 VALUE 10000.00.
       01 WS-SALDO-ORIG-DESPUES  PIC S9(10)V99 VALUE 0.
       01 WS-SALDO-DEST-DESPUES  PIC S9(10)V99 VALUE 0.
      *
      *--- Variables de control ---
       01 WS-FECHA-ACTUAL        PIC X(10).
       01 WS-HORA-ACTUAL         PIC X(8).
       01 WS-TRANSFER-VALIDA     PIC X VALUE 'N'.
       01 WS-CONFIRMAR           PIC X VALUE 'N'.
       01 WS-COMISION            PIC 9(5)V99 VALUE 0.
       01 WS-IMPORTE-TOTAL       PIC 9(8)V99 VALUE 0.
      *
      *-------- BLOQUE 4: PROCEDURE DIVISION ------------------------*
       PROCEDURE DIVISION.
      *
       0000-PRINCIPAL.
           PERFORM 1000-INICIALIZAR
           PERFORM 2000-PEDIR-DATOS-TRANSFERENCIA
           PERFORM 3000-VALIDAR-TRANSFERENCIA
           IF WS-TRANSFER-VALIDA = 'S'
              PERFORM 4000-CONFIRMAR-OPERACION
              IF WS-CONFIRMAR = 'S'
                 PERFORM 5000-EJECUTAR-TRANSFERENCIA
                 PERFORM 6000-MOSTRAR-JUSTIFICANTE
              ELSE
                 DISPLAY "Transferencia cancelada por el usuario"
              END-IF
           ELSE
              PERFORM 7000-MOSTRAR-ERROR
           END-IF
           PERFORM 9000-FINALIZAR
           STOP RUN.
      *
       1000-INICIALIZAR.
           ACCEPT WS-FECHA-ACTUAL FROM DATE YYYYMMDD
           ACCEPT WS-HORA-ACTUAL FROM TIME
           DISPLAY "=========================================="
           DISPLAY "   TRANSFERENCIA BANCARIA"
           DISPLAY "   Fecha: " WS-FECHA-ACTUAL
              " Hora: " WS-HORA-ACTUAL
           DISPLAY "==========================================".
      *
       2000-PEDIR-DATOS-TRANSFERENCIA.
           DISPLAY " "
           DISPLAY "Cuenta origen: "
           ACCEPT WS-CUENTA-ORIGEN
           DISPLAY "Cuenta destino: "
           ACCEPT WS-CUENTA-DESTINO
           DISPLAY "Importe a transferir: "
           ACCEPT WS-IMPORTE-TRANSFER
           DISPLAY "Concepto: "
           ACCEPT WS-CONCEPTO.
      *
       3000-VALIDAR-TRANSFERENCIA.
           MOVE 'N' TO WS-TRANSFER-VALIDA
      *    Calcular comision (0.5% si > 3000)
           IF WS-IMPORTE-TRANSFER > 3000
              COMPUTE WS-COMISION =
                 WS-IMPORTE-TRANSFER * 0.005
           ELSE
              MOVE 0 TO WS-COMISION
           END-IF
           COMPUTE WS-IMPORTE-TOTAL =
              WS-IMPORTE-TRANSFER + WS-COMISION
      *    Validar saldo suficiente
           IF WS-SALDO-ORIGEN >= WS-IMPORTE-TOTAL
              MOVE 'S' TO WS-TRANSFER-VALIDA
           END-IF
      *    Validar que no sea la misma cuenta
           IF WS-CUENTA-ORIGEN = WS-CUENTA-DESTINO
              MOVE 'N' TO WS-TRANSFER-VALIDA
           END-IF
      *    Validar importe positivo
           IF WS-IMPORTE-TRANSFER <= 0
              MOVE 'N' TO WS-TRANSFER-VALIDA
           END-IF.
      *
       4000-CONFIRMAR-OPERACION.
           DISPLAY " "
           DISPLAY "------------------------------------------"
           DISPLAY "  RESUMEN DE TRANSFERENCIA"
           DISPLAY "------------------------------------------"
           DISPLAY "Origen:    " WS-CUENTA-ORIGEN
           DISPLAY "Destino:   " WS-CUENTA-DESTINO
           DISPLAY "Importe:   " WS-IMPORTE-TRANSFER
           DISPLAY "Comision:  " WS-COMISION
           DISPLAY "TOTAL:     " WS-IMPORTE-TOTAL
           DISPLAY "------------------------------------------"
           DISPLAY "Confirmar transferencia? (S/N): "
           ACCEPT WS-CONFIRMAR.
      *
       5000-EJECUTAR-TRANSFERENCIA.
           COMPUTE WS-SALDO-ORIG-DESPUES =
              WS-SALDO-ORIGEN - WS-IMPORTE-TOTAL
           COMPUTE WS-SALDO-DEST-DESPUES =
              WS-SALDO-DESTINO + WS-IMPORTE-TRANSFER.
      *
       6000-MOSTRAR-JUSTIFICANTE.
           DISPLAY " "
           DISPLAY "=========================================="
           DISPLAY "   JUSTIFICANTE DE TRANSFERENCIA"
           DISPLAY "=========================================="
           DISPLAY "Fecha:     " WS-FECHA-ACTUAL
           DISPLAY "Hora:      " WS-HORA-ACTUAL
           DISPLAY "Origen:    " WS-CUENTA-ORIGEN
           DISPLAY "Destino:   " WS-CUENTA-DESTINO
           DISPLAY "Importe:   " WS-IMPORTE-TRANSFER
           DISPLAY "Comision:  " WS-COMISION
           DISPLAY "Concepto:  " WS-CONCEPTO
           DISPLAY "------------------------------------------"
           DISPLAY "Nuevo saldo origen:  " WS-SALDO-ORIG-DESPUES
           DISPLAY "Nuevo saldo destino: " WS-SALDO-DEST-DESPUES
           DISPLAY "=========================================="
           DISPLAY "TRANSFERENCIA REALIZADA CON EXITO".
      *
       7000-MOSTRAR-ERROR.
           DISPLAY " "
           DISPLAY "ERROR: Transferencia no valida."
           DISPLAY "Posibles causas:"
           DISPLAY "  - Saldo insuficiente"
           DISPLAY "  - Cuenta origen = cuenta destino"
           DISPLAY "  - Importe no valido".
      *
       9000-FINALIZAR.
           DISPLAY " "
           DISPLAY "Operacion finalizada.".
