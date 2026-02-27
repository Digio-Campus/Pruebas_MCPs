package com.banco.baseline.service;

import org.springframework.stereotype.Service;
import java.util.ArrayList;
import java.util.List;
import java.math.BigDecimal;
import java.time.LocalDate;

@Service
public class BancoIngresosService {

    // Simulación de variables de Working Storage
    private String numeroCuenta;
    private String titular;
    private int numIngresos;
    private int contador;
    private BigDecimal importeIngreso;
    private BigDecimal sumaTotal;
    private String continuar;
    private String fechaActual;

    // Tabla de ingresos
    public static class IngresoEntry {
        public BigDecimal importe;
        public String concepto;
    }
    private List<IngresoEntry> tablaIngresos = new ArrayList<>();

    public void inicializar(String cuenta, String nombreTitular) {
        this.numeroCuenta = cuenta;
        this.titular = nombreTitular;
        this.numIngresos = 0;
        this.sumaTotal = BigDecimal.ZERO;
        this.contador = 0;
        this.fechaActual = LocalDate.now().toString();
    }

    public void registrarIngreso(BigDecimal importe, String concepto) {
        this.contador++;
        this.importeIngreso = importe;
        
        IngresoEntry entry = new IngresoEntry();
        entry.importe = importe;
        entry.concepto = concepto;
        tablaIngresos.add(entry);
        
        this.numIngresos++;
    }

    public BigDecimal calcularTotal() {
        this.sumaTotal = BigDecimal.ZERO;
        for (int i = 0; i < this.numIngresos; i++) {
            this.sumaTotal = this.sumaTotal.add(tablaIngresos.get(i).importe);
        }
        return this.sumaTotal;
    }

    // Retorna string a modo de resumen (simulando DISPLAY)
    public String mostrarResumen() {
        StringBuilder resumen = new StringBuilder();
        resumen.append("==========================================\n");
        resumen.append("   RESUMEN DE INGRESOS\n");
        resumen.append("==========================================\n");
        resumen.append("Cuenta:  ").append(this.numeroCuenta).append("\n");
        resumen.append("Titular: ").append(this.titular).append("\n");
        resumen.append("------------------------------------------\n");
        
        for (int i = 0; i < this.numIngresos; i++) {
            resumen.append("  Ingreso #").append(i+1).append(": ")
                   .append(tablaIngresos.get(i).importe).append(" - ")
                   .append(tablaIngresos.get(i).concepto).append("\n");
        }
        
        resumen.append("------------------------------------------\n");
        resumen.append("Numero de ingresos: ").append(this.numIngresos).append("\n");
        resumen.append("SUMA TOTAL:         ").append(calcularTotal()).append("\n");
        resumen.append("==========================================\n");
        return resumen.toString();
    }
}
