package com.banco.dto;

import jakarta.validation.constraints.NotBlank;
import jakarta.validation.constraints.Positive;
import jakarta.validation.constraints.Size;
import java.math.BigDecimal;

/**
 * DTO de petición para transferencia.
 * Traducido desde los ACCEPT del párrafo 2000-PEDIR-DATOS-TRANSFERENCIA.
 */
public class TransferenciaRequest {

    @NotBlank
    @Size(max = 20)
    private String cuentaOrigen;

    @NotBlank
    @Size(max = 20)
    private String cuentaDestino;

    @Positive
    private BigDecimal importe;

    @Size(max = 50)
    private String concepto;

    public String getCuentaOrigen() { return cuentaOrigen; }
    public void setCuentaOrigen(String cuentaOrigen) { this.cuentaOrigen = cuentaOrigen; }
    public String getCuentaDestino() { return cuentaDestino; }
    public void setCuentaDestino(String cuentaDestino) { this.cuentaDestino = cuentaDestino; }
    public BigDecimal getImporte() { return importe; }
    public void setImporte(BigDecimal importe) { this.importe = importe; }
    public String getConcepto() { return concepto; }
    public void setConcepto(String concepto) { this.concepto = concepto; }
}
