package com.banco.model;

import jakarta.persistence.*;
import java.math.BigDecimal;

/**
 * Entidad CuentaBancaria.
 * Traducida desde las variables COBOL nivel 01:
 *   WS-NUMERO-CUENTA PIC X(20) → String numeroCuenta
 *   WS-TITULAR PIC X(40) → String titular
 *   WS-TIPO-CUENTA PIC X(15) → String tipoCuenta
 *   WS-SALDO-DISPONIBLE PIC S9(10)V99 → BigDecimal saldoDisponible
 *   WS-SALDO-RETENIDO PIC 9(10)V99 → BigDecimal saldoRetenido
 */
@Entity
@Table(name = "cuentas")
public class CuentaBancaria {

    @Id
    @Column(length = 20)
    private String numeroCuenta;

    @Column(length = 40, nullable = false)
    private String titular;

    @Column(length = 15)
    private String tipoCuenta;

    @Column(precision = 12, scale = 2)
    private BigDecimal saldoDisponible = BigDecimal.ZERO;

    @Column(precision = 12, scale = 2)
    private BigDecimal saldoRetenido = BigDecimal.ZERO;

    public CuentaBancaria() {}

    public CuentaBancaria(String numeroCuenta, String titular, String tipoCuenta,
                          BigDecimal saldoDisponible, BigDecimal saldoRetenido) {
        this.numeroCuenta = numeroCuenta;
        this.titular = titular;
        this.tipoCuenta = tipoCuenta;
        this.saldoDisponible = saldoDisponible;
        this.saldoRetenido = saldoRetenido;
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

    public BigDecimal getSaldoTotal() {
        return saldoDisponible.add(saldoRetenido);
    }
}
