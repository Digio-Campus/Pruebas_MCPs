package com.banco.controller;

import com.banco.dto.ExtractoResponse;
import com.banco.service.ExtractoService;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;

import java.math.BigDecimal;

/**
 * Traducción de BANCO-EXTRACTO (banco-extracto.cbl).
 * ACCEPT cuenta → @PathVariable
 * DISPLAY extracto → ResponseEntity<ExtractoResponse>
 */
@RestController
@RequestMapping("/api/extracto")
public class ExtractoController {

    private final ExtractoService extractoService;

    public ExtractoController(ExtractoService extractoService) {
        this.extractoService = extractoService;
    }

    @GetMapping("/{numeroCuenta}")
    public ResponseEntity<ExtractoResponse> generarExtracto(
            @PathVariable String numeroCuenta,
            @RequestParam(defaultValue = "0") BigDecimal saldoInicial) {
        ExtractoResponse response = extractoService.generarExtracto(numeroCuenta, saldoInicial);
        return ResponseEntity.ok(response);
    }
}
