package com.banco.service;

import com.banco.dto.TransferenciaRequest;
import com.banco.dto.TransferenciaResponse;
import com.banco.exception.CuentaNoEncontradaException;
import com.banco.exception.SaldoInsuficienteException;
import com.banco.exception.TransferenciaNoValidaException;
import com.banco.model.CuentaBancaria;
import com.banco.model.Movimiento;
import com.banco.repository.CuentaRepository;
import com.banco.repository.MovimientoRepository;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.math.BigDecimal;
import java.time.LocalDate;
import java.time.LocalTime;
import java.time.format.DateTimeFormatter;

/**
 * Traducción de BANCO-TRANSFERENCIA (banco-transferencia.cbl).
 * @Transactional obligatorio para atomicidad (patrón bancario).
 * Comisiones con @Value para porcentajes configurables.
 */
@Service
public class TransferenciaService {

    private static final Logger log = LoggerFactory.getLogger(TransferenciaService.class);

    @Value("${banco.comision.umbral:3000}")
    private BigDecimal umbralComision;

    @Value("${banco.comision.porcentaje:0.005}")
    private BigDecimal porcentajeComision;

    private final CuentaRepository cuentaRepository;
    private final MovimientoRepository movimientoRepository;

    public TransferenciaService(CuentaRepository cuentaRepository,
                                MovimientoRepository movimientoRepository) {
        this.cuentaRepository = cuentaRepository;
        this.movimientoRepository = movimientoRepository;
    }

    /**
     * Párrafo 0000-PRINCIPAL: flujo completo de transferencia.
     */
    @Transactional
    public TransferenciaResponse ejecutarTransferencia(TransferenciaRequest request) {
        log.info("========== TRANSFERENCIA BANCARIA ==========");

        // 3000-VALIDAR-TRANSFERENCIA
        validarTransferencia(request);

        CuentaBancaria origen = cuentaRepository.findById(request.getCuentaOrigen())
                .orElseThrow(() -> new CuentaNoEncontradaException(request.getCuentaOrigen()));
        CuentaBancaria destino = cuentaRepository.findById(request.getCuentaDestino())
                .orElseThrow(() -> new CuentaNoEncontradaException(request.getCuentaDestino()));

        // Calcular comisión (IF WS-IMPORTE-TRANSFER > 3000 → COMPUTE comision)
        BigDecimal comision = BigDecimal.ZERO;
        if (request.getImporte().compareTo(umbralComision) > 0) {
            comision = request.getImporte().multiply(porcentajeComision);
        }
        BigDecimal importeTotal = request.getImporte().add(comision);

        // Validar saldo suficiente (IF WS-SALDO-ORIGEN >= WS-IMPORTE-TOTAL)
        if (origen.getSaldoDisponible().compareTo(importeTotal) < 0) {
            throw new SaldoInsuficienteException(
                    "Saldo insuficiente. Disponible: " + origen.getSaldoDisponible()
                    + ", Requerido: " + importeTotal);
        }

        // 5000-EJECUTAR-TRANSFERENCIA
        // COMPUTE WS-SALDO-ORIG-DESPUES = WS-SALDO-ORIGEN - WS-IMPORTE-TOTAL
        origen.setSaldoDisponible(origen.getSaldoDisponible().subtract(importeTotal));
        // COMPUTE WS-SALDO-DEST-DESPUES = WS-SALDO-DESTINO + WS-IMPORTE-TRANSFER
        destino.setSaldoDisponible(destino.getSaldoDisponible().add(request.getImporte()));

        cuentaRepository.save(origen);
        cuentaRepository.save(destino);

        // Registrar movimientos
        String fechaHoy = LocalDate.now().toString();
        movimientoRepository.save(new Movimiento(fechaHoy,
                "TRANSF. A " + request.getCuentaDestino(), "G",
                importeTotal, origen));
        movimientoRepository.save(new Movimiento(fechaHoy,
                "TRANSF. DE " + request.getCuentaOrigen(), "I",
                request.getImporte(), destino));

        // 6000-MOSTRAR-JUSTIFICANTE → construir response
        TransferenciaResponse response = new TransferenciaResponse();
        response.setFecha(fechaHoy);
        response.setHora(LocalTime.now().format(DateTimeFormatter.ofPattern("HH:mm:ss")));
        response.setCuentaOrigen(request.getCuentaOrigen());
        response.setCuentaDestino(request.getCuentaDestino());
        response.setImporte(request.getImporte());
        response.setComision(comision);
        response.setImporteTotal(importeTotal);
        response.setConcepto(request.getConcepto());
        response.setNuevoSaldoOrigen(origen.getSaldoDisponible());
        response.setNuevoSaldoDestino(destino.getSaldoDisponible());

        log.info("TRANSFERENCIA REALIZADA CON EXITO");
        return response;
    }

    /**
     * Párrafo 3000-VALIDAR-TRANSFERENCIA.
     */
    private void validarTransferencia(TransferenciaRequest request) {
        if (request.getCuentaOrigen().equals(request.getCuentaDestino())) {
            throw new TransferenciaNoValidaException("Cuenta origen = cuenta destino");
        }
        if (request.getImporte().compareTo(BigDecimal.ZERO) <= 0) {
            throw new TransferenciaNoValidaException("Importe no valido");
        }
    }
}
