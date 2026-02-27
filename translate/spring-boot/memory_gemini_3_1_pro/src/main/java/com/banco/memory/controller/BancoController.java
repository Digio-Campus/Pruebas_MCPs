package com.banco.memory.controller;

import com.banco.memory.service.BancoTransferenciaService;
import com.banco.memory.service.BancoIngresosService;
import com.banco.memory.dto.TransferenciaRequestDTO;
import com.banco.memory.dto.IngresosBatchRequestDTO;
import org.springframework.web.bind.annotation.*;
import org.springframework.http.ResponseEntity;
import java.math.BigDecimal;

@RestController
@RequestMapping("/api/memory")
public class BancoController {

    private final BancoTransferenciaService transferenciaService;
    private final BancoIngresosService ingresosService;

    // Inyección de dependencias (Mapeo de COPY y CALL program USING params)
    public BancoController(BancoTransferenciaService tService, BancoIngresosService iService) {
        this.transferenciaService = tService;
        this.ingresosService = iService;
    }

    @PostMapping("/transferencia")
    public ResponseEntity<String> realizarTransferencia(@RequestBody TransferenciaRequestDTO request) {
        transferenciaService.ejecutarTransferencia(request);
        return ResponseEntity.ok("Transferencia por importe de " + request.getImporte() + " realizada con éxito.");
    }

    @PostMapping("/ingreso")
    public ResponseEntity<String> realizarIngresos(@RequestBody IngresosBatchRequestDTO request) {
        BigDecimal totalAñadido = ingresosService.registrarIngresos(request);
        return ResponseEntity.ok("Ingresos registrados correctamente. Suma total: " + totalAñadido);
    }
}
