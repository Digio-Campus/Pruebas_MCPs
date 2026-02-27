package com.banco.dto;

import java.math.BigDecimal;

/**
 * DTO de respuesta para consulta de saldo.
 * Traducido desde DISPLAY del párrafo 5000-MOSTRAR-SALDO.
 */
public class SaldoResponse {

    private String numeroCuenta;
    private String titular;
    private String tipoCuenta;
    private BigDecimal saldoDisponible;
    private BigDecimal saldoRetenido;
    private BigDecimal saldoTotal;

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
}
