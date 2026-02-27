package com.banco.consultasaldo.dto;

import java.math.BigDecimal;
import java.time.LocalDateTime;

/**
 * TRADUCCIÓN COBOL → SPRING BOOT
 * BLOQUE 2: DATA DIVISION → DTO de salida
 * BLOQUE 3: PROCEDURE DIVISION → DISPLAY → ResponseEntity<ConsultaSaldoResponse>
 * -----------------------------------------
 * COBOL:
 *   5000-MOSTRAR-SALDO.
 *       DISPLAY "Cuenta:     " WS-NUMERO-CUENTA        → numeroCuenta
 *       DISPLAY "Titular:    " WS-TITULAR               → titular
 *       DISPLAY "Tipo:       " WS-TIPO-CUENTA           → tipoCuenta
 *       DISPLAY "Saldo disponible: " WS-SALDO-DISPONIBLE → saldoDisponible (BigDecimal)
 *       DISPLAY "Saldo retenido:   " WS-SALDO-RETENIDO   → saldoRetenido   (BigDecimal)
 *       DISPLAY "SALDO TOTAL:      " WS-SALDO-TOTAL      → saldoTotal      (BigDecimal)
 *
 * Regla memoria: COMPUTE WS-SALDO-TOTAL = WS-SALDO-DISPONIBLE + WS-SALDO-RETENIDO
 *               → saldoTotal = saldoDisponible.add(saldoRetenido)
 */
public class ConsultaSaldoResponse {

    /** COBOL: WS-NUMERO-CUENTA PIC X(20) */
    private String numeroCuenta;

    /** COBOL: WS-TITULAR PIC X(40) */
    private String titular;

    /** COBOL: WS-TIPO-CUENTA PIC X(15) */
    private String tipoCuenta;

    /** COBOL: WS-SALDO-DISPONIBLE PIC S9(10)V99 — BigDecimal (NUNCA double/float) */
    private BigDecimal saldoDisponible;

    /** COBOL: WS-SALDO-RETENIDO PIC 9(10)V99 — BigDecimal */
    private BigDecimal saldoRetenido;

    /**
     * COBOL: COMPUTE WS-SALDO-TOTAL = WS-SALDO-DISPONIBLE + WS-SALDO-RETENIDO
     * Java:  saldoTotal = saldoDisponible.add(saldoRetenido)  ← .add() para BigDecimal
     */
    private BigDecimal saldoTotal;

    /** COBOL: ACCEPT WS-FECHA-ACTUAL FROM DATE YYYYMMDD + WS-HORA-ACTUAL FROM TIME */
    private LocalDateTime fechaConsulta;

    public ConsultaSaldoResponse() {}

    public ConsultaSaldoResponse(String numeroCuenta, String titular, String tipoCuenta,
                                  BigDecimal saldoDisponible, BigDecimal saldoRetenido,
                                  LocalDateTime fechaConsulta) {
        this.numeroCuenta   = numeroCuenta;
        this.titular        = titular;
        this.tipoCuenta     = tipoCuenta;
        this.saldoDisponible = saldoDisponible;
        this.saldoRetenido  = saldoRetenido;
        // COMPUTE WS-SALDO-TOTAL = WS-SALDO-DISPONIBLE + WS-SALDO-RETENIDO
        this.saldoTotal     = saldoDisponible.add(saldoRetenido);
        this.fechaConsulta  = fechaConsulta;
    }

    public String getNumeroCuenta() { return numeroCuenta; }
    public void setNumeroCuenta(String numeroCuenta) { this.numeroCuenta = numeroCuenta; }

    public String getTitular() { return titular; }
    public void setTitular(String titular) { this.titular = titular; }

    public String getTipoCuenta() { return tipoCuenta; }
    public void setTipoCuenta(String tipoCuenta) { this.tipoCuenta = tipoCuenta; }

    public BigDecimal getSaldoDisponible() { return saldoDisponible; }
    public void setSaldoDisponible(BigDecimal saldoDisponible) { this.saldoDisponible = saldoDisponible; }

    public BigDecimal getSaldoRetenido() { return saldoRetenido; }
    public void setSaldoRetenido(BigDecimal saldoRetenido) { this.saldoRetenido = saldoRetenido; }

    public BigDecimal getSaldoTotal() { return saldoTotal; }
    public void setSaldoTotal(BigDecimal saldoTotal) { this.saldoTotal = saldoTotal; }

    public LocalDateTime getFechaConsulta() { return fechaConsulta; }
    public void setFechaConsulta(LocalDateTime fechaConsulta) { this.fechaConsulta = fechaConsulta; }
}
