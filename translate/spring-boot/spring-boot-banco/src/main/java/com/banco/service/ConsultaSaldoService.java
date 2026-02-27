package com.banco.service;

import com.banco.dto.SaldoResponse;
import com.banco.exception.CuentaNoEncontradaException;
import com.banco.model.CuentaBancaria;
import com.banco.repository.CuentaRepository;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.stereotype.Service;

/**
 * Traducción de BANCO-CONSULTA-SALDO (banco-consulta-saldo.cbl).
 * 4000-BUSCAR-CUENTA (PERFORM VARYING + IF) → findById de JPA.
 * COMPUTE WS-SALDO-TOTAL = WS-SALDO-DISPONIBLE + WS-SALDO-RETENIDO
 *   → saldoDisponible.add(saldoRetenido)
 */
@Service
public class ConsultaSaldoService {

    private static final Logger log = LoggerFactory.getLogger(ConsultaSaldoService.class);

    private final CuentaRepository cuentaRepository;

    public ConsultaSaldoService(CuentaRepository cuentaRepository) {
        this.cuentaRepository = cuentaRepository;
    }

    /**
     * Párrafo 0000-PRINCIPAL: consulta de saldo.
     */
    public SaldoResponse consultarSaldo(String numeroCuenta) {
        log.info("========== CONSULTA DE SALDO BANCARIO ==========");

        // 4000-BUSCAR-CUENTA → JPA findById
        CuentaBancaria cuenta = cuentaRepository.findById(numeroCuenta)
                .orElseThrow(() -> new CuentaNoEncontradaException(numeroCuenta));

        // 5000-MOSTRAR-SALDO → construir response
        SaldoResponse response = new SaldoResponse();
        response.setNumeroCuenta(cuenta.getNumeroCuenta());
        response.setTitular(cuenta.getTitular());
        response.setTipoCuenta(cuenta.getTipoCuenta());
        response.setSaldoDisponible(cuenta.getSaldoDisponible());
        response.setSaldoRetenido(cuenta.getSaldoRetenido());
        // COMPUTE WS-SALDO-TOTAL = WS-SALDO-DISPONIBLE + WS-SALDO-RETENIDO
        response.setSaldoTotal(cuenta.getSaldoTotal());

        log.info("Cuenta: {} | Saldo total: {}", numeroCuenta, response.getSaldoTotal());
        return response;
    }
}
