package com.banco.model;

import jakarta.persistence.Column;
import jakarta.persistence.Entity;
import jakarta.persistence.EnumType;
import jakarta.persistence.Enumerated;
import jakarta.persistence.FetchType;
import jakarta.persistence.GeneratedValue;
import jakarta.persistence.GenerationType;
import jakarta.persistence.Id;
import jakarta.persistence.JoinColumn;
import jakarta.persistence.ManyToOne;
import jakarta.persistence.Table;
import java.math.BigDecimal;
import java.time.LocalDateTime;

@Entity
@Table(name = "movimientos")
public class Movimiento {

  @Id
  @GeneratedValue(strategy = GenerationType.IDENTITY)
  private Long id;

  @Column(nullable = false)
  private LocalDateTime fecha;

  @Column(length = 50, nullable = false)
  private String concepto;

  @Enumerated(EnumType.STRING)
  @Column(length = 1, nullable = false)
  private TipoMovimiento tipo;

  @Column(precision = 12, scale = 2, nullable = false)
  private BigDecimal importe;

  @ManyToOne(fetch = FetchType.LAZY, optional = false)
  @JoinColumn(name = "cuenta_numero", nullable = false)
  private CuentaBancaria cuenta;

  protected Movimiento() {}

  public Movimiento(
      CuentaBancaria cuenta,
      LocalDateTime fecha,
      String concepto,
      TipoMovimiento tipo,
      BigDecimal importe) {
    this.cuenta = cuenta;
    this.fecha = fecha;
    this.concepto = concepto;
    this.tipo = tipo;
    this.importe = importe;
  }

  public Long getId() {
    return id;
  }

  public LocalDateTime getFecha() {
    return fecha;
  }

  public String getConcepto() {
    return concepto;
  }

  public TipoMovimiento getTipo() {
    return tipo;
  }

  public BigDecimal getImporte() {
    return importe;
  }

  public CuentaBancaria getCuenta() {
    return cuenta;
  }
}
