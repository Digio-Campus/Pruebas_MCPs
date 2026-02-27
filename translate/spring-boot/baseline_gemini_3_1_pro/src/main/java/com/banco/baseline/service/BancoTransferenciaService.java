package com.banco.baseline.service;

import org.springframework.stereotype.Service;
import java.math.BigDecimal;
import java.time.LocalDate;
import java.time.LocalTime;

@Service
public class BancoTransferenciaService {

    private String cuentaOrigen;
    private String cuentaDestino;
    private BigDecimal importeTransfer = BigDecimal.ZERO;
    private String concepto;

    // Saldos fijos simulados como en COBOL
    private BigDecimal saldoOrigen = new BigDecimal("25000.00");
    private BigDecimal saldoDestino = new BigDecimal("10000.00");
    private BigDecimal saldoOrigDespues = BigDecimal.ZERO;
    private BigDecimal saldoDestDespues = BigDecimal.ZERO;

    private String fechaActual;
    private String horaActual;
    private boolean transferValida = false;
    private BigDecimal comision = BigDecimal.ZERO;
    private BigDecimal importeTotal = BigDecimal.ZERO;

    public void inicializar() {
        this.fechaActual = LocalDate.now().toString();
        this.horaActual = LocalTime.now().toString();
    }

    public void pedirDatosTransferencia(String origen, String destino, BigDecimal importe, String cto) {
        this.cuentaOrigen = origen;
        this.cuentaDestino = destino;
        this.importeTransfer = importe;
        this.concepto = cto;
    }

    public boolean validarTransferencia() {
        this.transferValida = false;
        
        if (this.importeTransfer.compareTo(new BigDecimal("3000")) > 0) {
            this.comision = this.importeTransfer.multiply(new BigDecimal("0.005"));
        } else {
            this.comision = BigDecimal.ZERO;
        }
        
        this.importeTotal = this.importeTransfer.add(this.comision);

        if (this.saldoOrigen.compareTo(this.importeTotal) >= 0) {
            this.transferValida = true;
        }

        if (this.cuentaOrigen.equals(this.cuentaDestino)) {
            this.transferValida = false;
        }

        if (this.importeTransfer.compareTo(BigDecimal.ZERO) <= 0) {
            this.transferValida = false;
        }

        return this.transferValida;
    }

    public void ejecutarTransferencia() {
        this.saldoOrigDespues = this.saldoOrigen.subtract(this.importeTotal);
        this.saldoDestDespues = this.saldoDestino.add(this.importeTransfer);
        
        // Actualizar saldos
        this.saldoOrigen = this.saldoOrigDespues;
        this.saldoDestino = this.saldoDestDespues;
    }

    public String mostrarJustificante() {
        StringBuilder sb = new StringBuilder();
        sb.append("==========================================\n");
        sb.append("   JUSTIFICANTE DE TRANSFERENCIA\n");
        sb.append("==========================================\n");
        sb.append("Fecha:     ").append(this.fechaActual).append("\n");
        sb.append("Hora:      ").append(this.horaActual).append("\n");
        sb.append("Origen:    ").append(this.cuentaOrigen).append("\n");
        sb.append("Destino:   ").append(this.cuentaDestino).append("\n");
        sb.append("Importe:   ").append(this.importeTransfer).append("\n");
        sb.append("Comision:  ").append(this.comision).append("\n");
        sb.append("Concepto:  ").append(this.concepto).append("\n");
        sb.append("------------------------------------------\n");
        sb.append("Nuevo saldo origen:  ").append(this.saldoOrigDespues).append("\n");
        sb.append("Nuevo saldo destino: ").append(this.saldoDestDespues).append("\n");
        sb.append("==========================================\n");
        sb.append("TRANSFERENCIA REALIZADA CON EXITO\n");
        return sb.toString();
    }
}
