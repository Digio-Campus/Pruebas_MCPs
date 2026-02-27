package com.banco.consultasaldo.controller;

import com.banco.consultasaldo.dto.ConsultaSaldoRequest;
import com.banco.consultasaldo.dto.ConsultaSaldoResponse;
import com.banco.consultasaldo.service.ConsultaSaldoService;
import jakarta.validation.Valid;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;

/**
 * TRADUCCIÓN COBOL → SPRING BOOT
 * BLOQUE 3: PROCEDURE DIVISION → @RestController
 * -----------------------------------------
 * COBOL:
 *   3000-PEDIR-CUENTA.
 *       DISPLAY "Introduzca el numero de cuenta: "
 *       ACCEPT WS-NUMERO-CUENTA.      → @RequestBody ConsultaSaldoRequest (POST)
 *                                       o @RequestParam numeroCuenta (GET)
 *
 * Regla memoria:
 *   ACCEPT → @RequestBody o @RequestParam en el Controller
 *   DISPLAY → ResponseEntity con datos para el frontend
 *   STOP RUN → return del método
 *
 * Ofrece dos endpoints:
 *   GET  /api/saldo/{numeroCuenta}   — consulta rápida por path variable
 *   POST /api/saldo/consultar        — consulta con body (para validaciones JSR-380)
 */
@RestController
@RequestMapping("/api/saldo")
public class ConsultaSaldoController {

    private final ConsultaSaldoService consultaSaldoService;

    public ConsultaSaldoController(ConsultaSaldoService consultaSaldoService) {
        this.consultaSaldoService = consultaSaldoService;
    }

    /**
     * COBOL:
     *   ACCEPT WS-NUMERO-CUENTA   → @RequestParam / @PathVariable
     *   PERFORM 0000-PRINCIPAL    → consultaSaldoService.consultarSaldo()
     *   DISPLAY resultado         → ResponseEntity.ok(response)
     *   STOP RUN                  → return
     */
    @GetMapping("/{numeroCuenta}")
    public ResponseEntity<ConsultaSaldoResponse> consultarSaldoGet(
            @PathVariable String numeroCuenta) {

        ConsultaSaldoRequest request = new ConsultaSaldoRequest(numeroCuenta);
        ConsultaSaldoResponse response = consultaSaldoService.consultarSaldo(request);
        // STOP RUN → return del método
        return ResponseEntity.ok(response);
    }

    /**
     * COBOL:
     *   ACCEPT WS-NUMERO-CUENTA   → @Valid @RequestBody ConsultaSaldoRequest
     *   PERFORM 0000-PRINCIPAL    → consultaSaldoService.consultarSaldo(request)
     *   DISPLAY resultado         → ResponseEntity.ok(response)
     *   STOP RUN                  → return
     *
     * Bean Validation (regla memoria):
     *   IF WS-NUMERO-CUENTA = SPACES → @NotBlank (validado automáticamente con @Valid)
     */
    @PostMapping("/consultar")
    public ResponseEntity<ConsultaSaldoResponse> consultarSaldoPost(
            @Valid @RequestBody ConsultaSaldoRequest request) {

        ConsultaSaldoResponse response = consultaSaldoService.consultarSaldo(request);
        // STOP RUN → return del método
        return ResponseEntity.ok(response);
    }
}
