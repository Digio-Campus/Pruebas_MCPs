package com.banco.dto;

import jakarta.validation.constraints.NotBlank;
import jakarta.validation.constraints.Positive;
import jakarta.validation.constraints.Size;
import java.math.BigDecimal;

/**
 * DTO de petición para registrar un ingreso.
 * Traducido desde los ACCEPT del programa BANCO-INGRESOS:
 *   ACCEPT WS-IMPORTE-INGRESO → importe
 *   ACCEPT WS-ING-CONCEPTO → concepto
 */
public class IngresoRequest {

    @NotBlank
    @Size(max = 20)
    private String numeroCuenta;

    @Positive
    private BigDecimal importe;

    @Size(max = 30)
    private String concepto;

    public String getNumeroCuenta() { return numeroCuenta; }
    public void setNumeroCuenta(String numeroCuenta) { this.numeroCuenta = numeroCuenta; }
    public BigDecimal getImporte() { return importe; }
    public void setImporte(BigDecimal importe) { this.importe = importe; }
    public String getConcepto() { return concepto; }
    public void setConcepto(String concepto) { this.concepto = concepto; }
}
