package com.banco.controller;

import com.banco.dto.TransferenciaRequest;
import com.banco.dto.TransferenciaResponse;
import com.banco.service.TransferenciaService;
import jakarta.validation.Valid;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;

/**
 * Traducción de BANCO-TRANSFERENCIA (banco-transferencia.cbl).
 * ACCEPT datos → @RequestBody TransferenciaRequest
 * DISPLAY justificante → ResponseEntity<TransferenciaResponse>
 */
@RestController
@RequestMapping("/api/transferencias")
public class TransferenciaController {

    private final TransferenciaService transferenciaService;

    public TransferenciaController(TransferenciaService transferenciaService) {
        this.transferenciaService = transferenciaService;
    }

    @PostMapping
    public ResponseEntity<TransferenciaResponse> realizarTransferencia(
            @RequestBody @Valid TransferenciaRequest request) {
        TransferenciaResponse response = transferenciaService.ejecutarTransferencia(request);
        return ResponseEntity.ok(response);
    }
}
