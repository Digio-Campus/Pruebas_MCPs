package com.banco.dto;

import java.math.BigDecimal;

/**
 * DTO de respuesta (justificante) de transferencia.
 * Traducido desde DISPLAY del párrafo 6000-MOSTRAR-JUSTIFICANTE.
 */
public class TransferenciaResponse {

    private String fecha;
    private String hora;
    private String cuentaOrigen;
    private String cuentaDestino;
    private BigDecimal importe;
    private BigDecimal comision;
    private BigDecimal importeTotal;
    private String concepto;
    private BigDecimal nuevoSaldoOrigen;
    private BigDecimal nuevoSaldoDestino;

    public String getFecha() { return fecha; }
    public void setFecha(String fecha) { this.fecha = fecha; }
    public String getHora() { return hora; }
    public void setHora(String hora) { this.hora = hora; }
    public String getCuentaOrigen() { return cuentaOrigen; }
    public void setCuentaOrigen(String cuentaOrigen) { this.cuentaOrigen = cuentaOrigen; }
    public String getCuentaDestino() { return cuentaDestino; }
    public void setCuentaDestino(String cuentaDestino) { this.cuentaDestino = cuentaDestino; }
    public BigDecimal getImporte() { return importe; }
    public void setImporte(BigDecimal importe) { this.importe = importe; }
    public BigDecimal getComision() { return comision; }
    public void setComision(BigDecimal comision) { this.comision = comision; }
    public BigDecimal getImporteTotal() { return importeTotal; }
    public void setImporteTotal(BigDecimal importeTotal) { this.importeTotal = importeTotal; }
    public String getConcepto() { return concepto; }
    public void setConcepto(String concepto) { this.concepto = concepto; }
    public BigDecimal getNuevoSaldoOrigen() { return nuevoSaldoOrigen; }
    public void setNuevoSaldoOrigen(BigDecimal nuevoSaldoOrigen) { this.nuevoSaldoOrigen = nuevoSaldoOrigen; }
    public BigDecimal getNuevoSaldoDestino() { return nuevoSaldoDestino; }
    public void setNuevoSaldoDestino(BigDecimal nuevoSaldoDestino) { this.nuevoSaldoDestino = nuevoSaldoDestino; }
}
