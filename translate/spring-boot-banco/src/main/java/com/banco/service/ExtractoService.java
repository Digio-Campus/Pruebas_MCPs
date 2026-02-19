package com.banco.service;

import com.banco.dto.ExtractoResponse;
import com.banco.exception.CuentaNoEncontradaException;
import com.banco.model.CuentaBancaria;
import com.banco.model.Movimiento;
import com.banco.repository.CuentaRepository;
import com.banco.repository.MovimientoRepository;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.stereotype.Service;

import java.math.BigDecimal;
import java.util.List;
import java.util.stream.Collectors;

/**
 * Traducción de BANCO-EXTRACTO (banco-extracto.cbl).
 * 3000-CALCULAR-TOTALES (PERFORM VARYING + IF tipo + ADD) → streams
 * 4000-MOSTRAR-EXTRACTO → construir response
 */
@Service
public class ExtractoService {

    private static final Logger log = LoggerFactory.getLogger(ExtractoService.class);

    private final CuentaRepository cuentaRepository;
    private final MovimientoRepository movimientoRepository;

    public ExtractoService(CuentaRepository cuentaRepository,
                           MovimientoRepository movimientoRepository) {
        this.cuentaRepository = cuentaRepository;
        this.movimientoRepository = movimientoRepository;
    }

    /**
     * Párrafo 0000-PRINCIPAL: generar extracto completo.
     */
    public ExtractoResponse generarExtracto(String numeroCuenta, BigDecimal saldoInicial) {
        log.info("========== EXTRACTO DE MOVIMIENTOS ==========");

        CuentaBancaria cuenta = cuentaRepository.findById(numeroCuenta)
                .orElseThrow(() -> new CuentaNoEncontradaException(numeroCuenta));

        List<Movimiento> movimientos = movimientoRepository
                .findByCuentaOrderByFechaAsc(cuenta);

        // 3000-CALCULAR-TOTALES
        // PERFORM VARYING + IF WS-MOV-TIPO = "I" → streams con filter
        BigDecimal totalIngresos = movimientos.stream()
                .filter(m -> "I".equals(m.getTipo()))
                .map(Movimiento::getImporte)
                .reduce(BigDecimal.ZERO, BigDecimal::add);

        BigDecimal totalGastos = movimientos.stream()
                .filter(m -> "G".equals(m.getTipo()))
                .map(Movimiento::getImporte)
                .reduce(BigDecimal.ZERO, BigDecimal::add);

        // COMPUTE WS-SALDO-FINAL = WS-SALDO-INICIAL + INGRESOS - GASTOS
        BigDecimal saldoFinal = saldoInicial.add(totalIngresos).subtract(totalGastos);

        // 4000-MOSTRAR-EXTRACTO → construir response
        List<ExtractoResponse.MovimientoDetalle> detalles = movimientos.stream()
                .map(m -> new ExtractoResponse.MovimientoDetalle(
                        m.getFecha(), m.getConcepto(), m.getTipo(), m.getImporte()))
                .collect(Collectors.toList());

        ExtractoResponse response = new ExtractoResponse();
        response.setNumeroCuenta(cuenta.getNumeroCuenta());
        response.setTitular(cuenta.getTitular());
        response.setSaldoInicial(saldoInicial);
        response.setMovimientos(detalles);
        response.setTotalIngresos(totalIngresos);
        response.setTotalGastos(totalGastos);
        response.setSaldoFinal(saldoFinal);

        log.info("Extracto generado: {} movimientos, saldo final: {}",
                detalles.size(), saldoFinal);
        return response;
    }
}
