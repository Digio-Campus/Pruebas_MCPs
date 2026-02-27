package com.banco.model;

import jakarta.persistence.Column;
import jakarta.persistence.Entity;
import jakarta.persistence.Table;
import jakarta.persistence.Id;
import jakarta.persistence.Transient;
import java.math.BigDecimal;

@Entity
@Table(name = "cuentas")
public class CuentaBancaria {

  @Id
  @Column(length = 20, nullable = false)
  private String numeroCuenta;

  @Column(length = 40, nullable = false)
  private String titular;

  @Column(length = 15, nullable = false)
  private String tipoCuenta;

  @Column(precision = 12, scale = 2, nullable = false)
  private BigDecimal saldoDisponible = BigDecimal.ZERO;

  @Column(precision = 12, scale = 2, nullable = false)
  private BigDecimal saldoRetenido = BigDecimal.ZERO;

  protected CuentaBancaria() {}

  public CuentaBancaria(
      String numeroCuenta,
      String titular,
      String tipoCuenta,
      BigDecimal saldoDisponible,
      BigDecimal saldoRetenido) {
    this.numeroCuenta = numeroCuenta;
    this.titular = titular;
    this.tipoCuenta = tipoCuenta;
    this.saldoDisponible = saldoDisponible;
    this.saldoRetenido = saldoRetenido;
  }

  public String getNumeroCuenta() {
    return numeroCuenta;
  }

  public void setNumeroCuenta(String numeroCuenta) {
    this.numeroCuenta = numeroCuenta;
  }

  public String getTitular() {
    return titular;
  }

  public void setTitular(String titular) {
    this.titular = titular;
  }

  public String getTipoCuenta() {
    return tipoCuenta;
  }

  public void setTipoCuenta(String tipoCuenta) {
    this.tipoCuenta = tipoCuenta;
  }

  public BigDecimal getSaldoDisponible() {
    return saldoDisponible;
  }

  public void setSaldoDisponible(BigDecimal saldoDisponible) {
    this.saldoDisponible = saldoDisponible;
  }

  public BigDecimal getSaldoRetenido() {
    return saldoRetenido;
  }

  public void setSaldoRetenido(BigDecimal saldoRetenido) {
    this.saldoRetenido = saldoRetenido;
  }

  @Transient
  public BigDecimal getSaldoTotal() {
    return saldoDisponible.add(saldoRetenido);
  }
}
