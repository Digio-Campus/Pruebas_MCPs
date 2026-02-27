package com.banco.service;

import com.banco.dto.IngresoRequest;
import com.banco.dto.IngresoResponse;
import com.banco.exception.CuentaNoEncontradaException;
import com.banco.model.CuentaBancaria;
import com.banco.model.Movimiento;
import com.banco.repository.CuentaRepository;
import com.banco.repository.MovimientoRepository;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.math.BigDecimal;
import java.time.LocalDate;
import java.util.ArrayList;
import java.util.List;

/**
 * Traducción de BANCO-INGRESOS (banco-ingresos.cbl).
 * Cada PERFORM párrafo → método privado.
 * DISPLAY → log.info()
 * ACCEPT → parámetros del método
 * ADD X TO Y → y = y.add(x)
 */
@Service
public class IngresoService {

    private static final Logger log = LoggerFactory.getLogger(IngresoService.class);

    private final CuentaRepository cuentaRepository;
    private final MovimientoRepository movimientoRepository;

    public IngresoService(CuentaRepository cuentaRepository,
                          MovimientoRepository movimientoRepository) {
        this.cuentaRepository = cuentaRepository;
        this.movimientoRepository = movimientoRepository;
    }

    /**
     * Párrafo 0000-PRINCIPAL: flujo completo de registro de ingresos.
     */
    @Transactional
    public IngresoResponse registrarIngresos(String numeroCuenta,
                                              List<IngresoRequest> ingresos) {
        log.info("========== SISTEMA DE INGRESOS BANCARIOS ==========");

        // 2000-PEDIR-DATOS-CUENTA → recibido como parámetro
        CuentaBancaria cuenta = cuentaRepository.findById(numeroCuenta)
                .orElseThrow(() -> new CuentaNoEncontradaException(numeroCuenta));

        // 3000-REGISTRAR-INGRESOS (PERFORM UNTIL → for loop)
        List<IngresoResponse.IngresoDetalle> detalles = new ArrayList<>();
        for (IngresoRequest ingreso : ingresos) {
            log.info("Ingreso #{}: {} - {}", detalles.size() + 1,
                    ingreso.getImporte(), ingreso.getConcepto());

            Movimiento mov = new Movimiento(
                    LocalDate.now().toString(),
                    ingreso.getConcepto(),
                    "I",
                    ingreso.getImporte(),
                    cuenta
            );
            movimientoRepository.save(mov);
            detalles.add(new IngresoResponse.IngresoDetalle(
                    ingreso.getImporte(), ingreso.getConcepto()));
        }

        // 4000-CALCULAR-TOTAL (PERFORM VARYING + ADD → stream + reduce)
        BigDecimal sumaTotal = detalles.stream()
                .map(IngresoResponse.IngresoDetalle::getImporte)
                .reduce(BigDecimal.ZERO, BigDecimal::add);

        // Actualizar saldo de cuenta
        cuenta.setSaldoDisponible(cuenta.getSaldoDisponible().add(sumaTotal));
        cuentaRepository.save(cuenta);

        // 5000-MOSTRAR-RESUMEN → construir response
        IngresoResponse response = new IngresoResponse();
        response.setNumeroCuenta(cuenta.getNumeroCuenta());
        response.setTitular(cuenta.getTitular());
        response.setIngresos(detalles);
        response.setNumIngresos(detalles.size());
        response.setSumaTotal(sumaTotal);

        log.info("SUMA TOTAL: {}", sumaTotal);
        return response;
    }
}
