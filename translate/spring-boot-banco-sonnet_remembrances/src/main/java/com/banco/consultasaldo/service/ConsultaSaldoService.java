package com.banco.consultasaldo.service;

import com.banco.consultasaldo.dto.ConsultaSaldoRequest;
import com.banco.consultasaldo.dto.ConsultaSaldoResponse;
import com.banco.consultasaldo.exception.CuentaNoEncontradaException;
import com.banco.consultasaldo.model.CuentaBancaria;
import com.banco.consultasaldo.repository.CuentaBancariaRepository;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.stereotype.Service;

import java.time.LocalDateTime;

/**
 * TRADUCCIÓN COBOL → SPRING BOOT
 * BLOQUE 3: PROCEDURE DIVISION → @Service
 * -----------------------------------------
 * Cada párrafo COBOL se convierte en un método privado del @Service.
 *
 * COBOL paragraph → Java method:
 *   0000-PRINCIPAL          → consultarSaldo() (método público principal)
 *   1000-INICIALIZAR        → inicializar()
 *   4000-BUSCAR-CUENTA      → buscarCuenta()
 *   5000-MOSTRAR-SALDO      → retorna ConsultaSaldoResponse (DISPLAY → ResponseEntity)
 *   6000-CUENTA-NO-ENCONTRADA → lanza CuentaNoEncontradaException
 *
 * Regla memoria: PERFORM párrafo → llamada a método privado del service
 */
@Service
public class ConsultaSaldoService {

    private static final Logger logger = LoggerFactory.getLogger(ConsultaSaldoService.class);
    // COBOL: DISPLAY "..." → Logger.info()

    private final CuentaBancariaRepository cuentaRepository;

    public ConsultaSaldoService(CuentaBancariaRepository cuentaRepository) {
        this.cuentaRepository = cuentaRepository;
    }

    /**
     * COBOL: 0000-PRINCIPAL  (párrafo principal)
     * -----------------------------------------
     *   PERFORM 1000-INICIALIZAR
     *   PERFORM 3000-PEDIR-CUENTA         ← recibido como @RequestBody
     *   PERFORM 4000-BUSCAR-CUENTA
     *   IF WS-CUENTA-ENCONTRADA = 'S'
     *      PERFORM 5000-MOSTRAR-SALDO
     *   ELSE
     *      PERFORM 6000-CUENTA-NO-ENCONTRADA
     *   END-IF
     *   STOP RUN.                          ← return
     */
    public ConsultaSaldoResponse consultarSaldo(ConsultaSaldoRequest request) {
        // PERFORM 1000-INICIALIZAR
        LocalDateTime fechaConsulta = inicializar();

        // PERFORM 4000-BUSCAR-CUENTA
        // IF WS-CUENTA-ENCONTRADA = 'S' ... ELSE PERFORM 6000-CUENTA-NO-ENCONTRADA
        CuentaBancaria cuenta = cuentaRepository
                .findByNumeroCuenta(request.getNumeroCuenta())
                .orElseThrow(() -> new CuentaNoEncontradaException(request.getNumeroCuenta()));

        // PERFORM 5000-MOSTRAR-SALDO → return ConsultaSaldoResponse
        return construirRespuesta(cuenta, fechaConsulta);
    }

    /**
     * COBOL: 1000-INICIALIZAR.
     *   ACCEPT WS-FECHA-ACTUAL FROM DATE YYYYMMDD
     *   ACCEPT WS-HORA-ACTUAL FROM TIME
     *   DISPLAY "======================================="
     *   DISPLAY "   CONSULTA DE SALDO BANCARIO"
     *   DISPLAY "   Fecha: " WS-FECHA-ACTUAL " Hora: " WS-HORA-ACTUAL
     *   DISPLAY "======================================="
     *
     * Regla memoria: DISPLAY → Logger.info()
     *                ACCEPT ... FROM DATE → LocalDateTime.now()
     */
    private LocalDateTime inicializar() {
        LocalDateTime ahora = LocalDateTime.now();
        logger.info("==========================================");
        logger.info("   CONSULTA DE SALDO BANCARIO");
        logger.info("   Fecha/Hora: {}", ahora);
        logger.info("==========================================");
        return ahora;
    }

    /**
     * COBOL: 5000-MOSTRAR-SALDO.
     *   DISPLAY "Cuenta:     " WS-NUMERO-CUENTA
     *   DISPLAY "Titular:    " WS-TITULAR
     *   DISPLAY "Tipo:       " WS-TIPO-CUENTA
     *   DISPLAY "Saldo disponible: " WS-SALDO-DISPONIBLE
     *   DISPLAY "Saldo retenido:   " WS-SALDO-RETENIDO
     *   DISPLAY "SALDO TOTAL:      " WS-SALDO-TOTAL
     *
     * Regla memoria:
     *   COMPUTE WS-SALDO-TOTAL = WS-SALDO-DISPONIBLE + WS-SALDO-RETENIDO
     *   → saldoTotal = saldoDisponible.add(saldoRetenido)  ← .add() BigDecimal
     */
    private ConsultaSaldoResponse construirRespuesta(CuentaBancaria cuenta,
                                                      LocalDateTime fechaConsulta) {
        logger.info("Consulta exitosa - Cuenta: {} Titular: {}",
                cuenta.getNumeroCuenta(), cuenta.getTitular());

        return new ConsultaSaldoResponse(
                cuenta.getNumeroCuenta(),
                cuenta.getTitular(),
                cuenta.getTipoCuenta(),
                cuenta.getSaldoDisponible(),
                cuenta.getSaldoRetenido(),
                fechaConsulta
                // COMPUTE WS-SALDO-TOTAL: se calcula en el constructor del DTO
        );
    }
}
