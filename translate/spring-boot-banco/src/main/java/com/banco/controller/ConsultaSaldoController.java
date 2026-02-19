package com.banco.controller;

import com.banco.dto.SaldoResponse;
import com.banco.service.ConsultaSaldoService;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;

/**
 * Traducción de BANCO-CONSULTA-SALDO (banco-consulta-saldo.cbl).
 * ACCEPT WS-NUMERO-CUENTA → @PathVariable
 * DISPLAY saldo → ResponseEntity<SaldoResponse>
 */
@RestController
@RequestMapping("/api/saldo")
public class ConsultaSaldoController {

    private final ConsultaSaldoService consultaSaldoService;

    public ConsultaSaldoController(ConsultaSaldoService consultaSaldoService) {
        this.consultaSaldoService = consultaSaldoService;
    }

    @GetMapping("/{numeroCuenta}")
    public ResponseEntity<SaldoResponse> consultarSaldo(
            @PathVariable String numeroCuenta) {
        SaldoResponse response = consultaSaldoService.consultarSaldo(numeroCuenta);
        return ResponseEntity.ok(response);
    }
}
