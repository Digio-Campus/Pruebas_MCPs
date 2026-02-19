package com.banco.dto;

import java.math.BigDecimal;
import java.util.List;

/**
 * DTO de respuesta para el resumen de ingresos.
 * Traducido desde los DISPLAY del párrafo 5000-MOSTRAR-RESUMEN.
 */
public class IngresoResponse {

    private String numeroCuenta;
    private String titular;
    private List<IngresoDetalle> ingresos;
    private int numIngresos;
    private BigDecimal sumaTotal;

    public static class IngresoDetalle {
        private BigDecimal importe;
        private String concepto;

        public IngresoDetalle(BigDecimal importe, String concepto) {
            this.importe = importe;
            this.concepto = concepto;
        }

        public BigDecimal getImporte() { return importe; }
        public String getConcepto() { return concepto; }
    }

    public String getNumeroCuenta() { return numeroCuenta; }
    public void setNumeroCuenta(String numeroCuenta) { this.numeroCuenta = numeroCuenta; }
    public String getTitular() { return titular; }
    public void setTitular(String titular) { this.titular = titular; }
    public List<IngresoDetalle> getIngresos() { return ingresos; }
    public void setIngresos(List<IngresoDetalle> ingresos) { this.ingresos = ingresos; }
    public int getNumIngresos() { return numIngresos; }
    public void setNumIngresos(int numIngresos) { this.numIngresos = numIngresos; }
    public BigDecimal getSumaTotal() { return sumaTotal; }
    public void setSumaTotal(BigDecimal sumaTotal) { this.sumaTotal = sumaTotal; }
}
