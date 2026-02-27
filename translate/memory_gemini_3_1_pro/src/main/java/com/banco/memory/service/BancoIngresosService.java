package com.banco.memory.service;

import com.banco.memory.entity.CuentaEntity;
import com.banco.memory.repository.CuentaRepository;
import com.banco.memory.dto.IngresosBatchRequestDTO;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import java.math.BigDecimal;
import jakarta.persistence.EntityNotFoundException;

@Service
public class BancoIngresosService {

    private final CuentaRepository cuentaRepository;

    public BancoIngresosService(CuentaRepository cuentaRepository) {
        this.cuentaRepository = cuentaRepository;
    }

    @Transactional
    public BigDecimal registrarIngresos(IngresosBatchRequestDTO request) {
        CuentaEntity cuenta = cuentaRepository.findById(request.getNumeroCuenta())
                .orElseThrow(() -> new EntityNotFoundException("Cuenta no encontrada"));

        BigDecimal sumaTotal = BigDecimal.ZERO;

        // Traducción de PERFORM VARYING y bucles estructurados de COBOL
        if (request.getIngresos() != null) {
            for (IngresosBatchRequestDTO.IngresoItem item : request.getIngresos()) {
                if (item.getImporte().compareTo(BigDecimal.ZERO) <= 0) {
                    throw new IllegalArgumentException("Importe inválido en la lista de ingresos.");
                }
                sumaTotal = sumaTotal.add(item.getImporte());
            }
        }

        cuenta.setSaldo(cuenta.getSaldo().add(sumaTotal));
        cuentaRepository.save(cuenta); // Equivalente a REWRITE

        return sumaTotal;
    }
}
