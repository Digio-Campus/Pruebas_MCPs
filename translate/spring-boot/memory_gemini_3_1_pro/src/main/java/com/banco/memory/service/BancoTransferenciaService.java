package com.banco.memory.service;

import com.banco.memory.entity.CuentaEntity;
import com.banco.memory.repository.CuentaRepository;
import com.banco.memory.dto.TransferenciaRequestDTO;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import java.math.BigDecimal;
import jakarta.persistence.EntityNotFoundException;

@Service
public class BancoTransferenciaService {

    private final CuentaRepository cuentaRepository;

    public BancoTransferenciaService(CuentaRepository cuentaRepository) {
        this.cuentaRepository = cuentaRepository;
    }

    @Transactional
    public void ejecutarTransferencia(TransferenciaRequestDTO request) {
        // Validacion de misma cuenta (remplaza a IF WS-CUENTA-ORIGEN =
        // WS-CUENTA-DESTINO)
        if (request.getCuentaOrigen().equals(request.getCuentaDestino())) {
            throw new IllegalArgumentException("La cuenta origen y destino no pueden ser la misma.");
        }

        // Validar importe positivo (remplaza a IF WS-IMPORTE-TRANSFER <= 0)
        if (request.getImporte().compareTo(BigDecimal.ZERO) <= 0) {
            throw new IllegalArgumentException("El importe a transferir debe ser mayor a 0.");
        }

        // Recuperar cuentas (remplaza a lectura de ficheros y FILE STATUS 23)
        CuentaEntity origen = cuentaRepository.findById(request.getCuentaOrigen())
                .orElseThrow(() -> new EntityNotFoundException("Cuenta origen no encontrada"));
        CuentaEntity destino = cuentaRepository.findById(request.getCuentaDestino())
                .orElseThrow(() -> new EntityNotFoundException("Cuenta destino no encontrada"));

        // Calcular comisiones
        BigDecimal comision = BigDecimal.ZERO;
        if (request.getImporte().compareTo(new BigDecimal("3000")) > 0) {
            comision = request.getImporte().multiply(new BigDecimal("0.005"));
        }
        BigDecimal importeTotal = request.getImporte().add(comision);

        // Validar saldo (remplaza a IF WS-SALDO-ORIGEN >= WS-IMPORTE-TOTAL)
        if (origen.getSaldo().compareTo(importeTotal) < 0) {
            throw new IllegalStateException("Saldo insuficiente en la cuenta de origen.");
        }

        // Ejecutar operaciones (remplaza COMPUTE de WS-SALDO)
        origen.setSaldo(origen.getSaldo().subtract(importeTotal));
        destino.setSaldo(destino.getSaldo().add(request.getImporte()));

        // Guardar cambios (remplaza a REWRITE)
        cuentaRepository.save(origen);
        cuentaRepository.save(destino);
    }
}
