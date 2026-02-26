package com.banco.consultasaldo.model;

import jakarta.persistence.*;
import java.math.BigDecimal;

/**
 * TRADUCCIÓN COBOL → SPRING BOOT
 * BLOQUE 2: DATA DIVISION → @Entity
 * -----------------------------------------
 * COBOL:
 *   01 WS-DB-CUENTAS.
 *      05 WS-DB-CUENTA OCCURS 3 TIMES.      → List<CuentaBancaria> (regla: OCCURS n TIMES → List<T>)
 *         10 WS-DB-NUM-CUENTA  PIC X(20).   → String @Size(max=20)
 *         10 WS-DB-TITULAR     PIC X(40).   → String @Size(max=40)
 *         10 WS-DB-TIPO        PIC X(15).   → String @Size(max=15)
 *         10 WS-DB-SALDO-DISP  PIC S9(10)V99. → BigDecimal (regla: PIC S9(n)V99 → BigDecimal)
 *         10 WS-DB-SALDO-RET   PIC 9(10)V99.  → BigDecimal (regla: PIC 9(n)V99 → BigDecimal)
 *
 * Patrón bancario (regla memoria):
 *   Cuenta bancaria (WS-NUMERO-CUENTA, WS-TITULAR) → @Entity CuentaBancaria con @Id
 *   Operaciones monetarias siempre con BigDecimal, NUNCA double/float
 */
@Entity
@Table(name = "cuentas")
public class CuentaBancaria {

    @Id
    @GeneratedValue(strategy = GenerationType.IDENTITY)
    private Long id;

    /** COBOL: 10 WS-DB-NUM-CUENTA PIC X(20) */
    @Column(name = "numero_cuenta", nullable = false, unique = true, length = 20)
    private String numeroCuenta;

    /** COBOL: 10 WS-DB-TITULAR PIC X(40) */
    @Column(name = "titular", nullable = false, length = 40)
    private String titular;

    /** COBOL: 10 WS-DB-TIPO PIC X(15) */
    @Column(name = "tipo", length = 15)
    private String tipoCuenta;

    /** COBOL: 10 WS-DB-SALDO-DISP PIC S9(10)V99 — BigDecimal (NUNCA double/float) */
    @Column(name = "saldo_disponible", precision = 12, scale = 2)
    private BigDecimal saldoDisponible;

    /** COBOL: 10 WS-DB-SALDO-RET PIC 9(10)V99 — BigDecimal (NUNCA double/float) */
    @Column(name = "saldo_retenido", precision = 12, scale = 2)
    private BigDecimal saldoRetenido;

    public CuentaBancaria() {}

    public CuentaBancaria(String numeroCuenta, String titular, String tipoCuenta,
                          BigDecimal saldoDisponible, BigDecimal saldoRetenido) {
        this.numeroCuenta = numeroCuenta;
        this.titular = titular;
        this.tipoCuenta = tipoCuenta;
        this.saldoDisponible = saldoDisponible;
        this.saldoRetenido = saldoRetenido;
    }

    // ---- Getters y Setters ----

    public Long getId() { return id; }
    public void setId(Long id) { this.id = id; }

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
}
