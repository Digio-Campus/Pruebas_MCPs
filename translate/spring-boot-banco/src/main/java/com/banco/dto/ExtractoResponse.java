package com.banco.dto;

import java.math.BigDecimal;
import java.util.List;

/**
 * DTO de respuesta para extracto de movimientos.
 * Traducido desde DISPLAY del párrafo 4000-MOSTRAR-EXTRACTO.
 */
public class ExtractoResponse {

    private String numeroCuenta;
    private String titular;
    private BigDecimal saldoInicial;
    private List<MovimientoDetalle> movimientos;
    private BigDecimal totalIngresos;
    private BigDecimal totalGastos;
    private BigDecimal saldoFinal;

    public static class MovimientoDetalle {
        private String fecha;
        private String concepto;
        private String tipo;
        private BigDecimal importe;

        public MovimientoDetalle(String fecha, String concepto, String tipo, BigDecimal importe) {
            this.fecha = fecha;
            this.concepto = concepto;
            this.tipo = tipo;
            this.importe = importe;
        }

        public String getFecha() { return fecha; }
        public String getConcepto() { return concepto; }
        public String getTipo() { return tipo; }
        public BigDecimal getImporte() { return importe; }
    }

    public String getNumeroCuenta() { return numeroCuenta; }
    public void setNumeroCuenta(String numeroCuenta) { this.numeroCuenta = numeroCuenta; }
    public String getTitular() { return titular; }
    public void setTitular(String titular) { this.titular = titular; }
    public BigDecimal getSaldoInicial() { return saldoInicial; }
    public void setSaldoInicial(BigDecimal saldoInicial) { this.saldoInicial = saldoInicial; }
    public List<MovimientoDetalle> getMovimientos() { return movimientos; }
    public void setMovimientos(List<MovimientoDetalle> movimientos) { this.movimientos = movimientos; }
    public BigDecimal getTotalIngresos() { return totalIngresos; }
    public void setTotalIngresos(BigDecimal totalIngresos) { this.totalIngresos = totalIngresos; }
    public BigDecimal getTotalGastos() { return totalGastos; }
    public void setTotalGastos(BigDecimal totalGastos) { this.totalGastos = totalGastos; }
    public BigDecimal getSaldoFinal() { return saldoFinal; }
    public void setSaldoFinal(BigDecimal saldoFinal) { this.saldoFinal = saldoFinal; }
}
