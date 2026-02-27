package com.banco.memory.dto;

import java.math.BigDecimal;
import java.util.List;

public class IngresosBatchRequestDTO {

    private String numeroCuenta;

    // Traducción de OCCURS 100 TIMES en COBOL a una List en DTO
    private List<IngresoItem> ingresos;

    public static class IngresoItem {
        private BigDecimal importe;
        private String concepto;

        public BigDecimal getImporte() {
            return importe;
        }

        public void setImporte(BigDecimal importe) {
            this.importe = importe;
        }

        public String getConcepto() {
            return concepto;
        }

        public void setConcepto(String concepto) {
            this.concepto = concepto;
        }
    }

    public String getNumeroCuenta() {
        return numeroCuenta;
    }

    public void setNumeroCuenta(String numeroCuenta) {
        this.numeroCuenta = numeroCuenta;
    }

    public List<IngresoItem> getIngresos() {
        return ingresos;
    }

    public void setIngresos(List<IngresoItem> ingresos) {
        this.ingresos = ingresos;
    }
}
