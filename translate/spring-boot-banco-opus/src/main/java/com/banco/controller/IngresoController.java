package com.banco.controller;

import com.banco.dto.IngresoRequest;
import com.banco.dto.IngresoResponse;
import com.banco.service.IngresoService;
import jakarta.validation.Valid;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;

import java.util.List;

/**
 * Traducción de BANCO-INGRESOS (banco-ingresos.cbl).
 * ACCEPT → @RequestBody / @PathVariable
 * DISPLAY resumen → ResponseEntity<IngresoResponse>
 */
@RestController
@RequestMapping("/api/ingresos")
public class IngresoController {

    private final IngresoService ingresoService;

    public IngresoController(IngresoService ingresoService) {
        this.ingresoService = ingresoService;
    }

    @PostMapping("/{numeroCuenta}")
    public ResponseEntity<IngresoResponse> registrarIngresos(
            @PathVariable String numeroCuenta,
            @RequestBody @Valid List<IngresoRequest> ingresos) {
        IngresoResponse response = ingresoService.registrarIngresos(numeroCuenta, ingresos);
        return ResponseEntity.ok(response);
    }
}