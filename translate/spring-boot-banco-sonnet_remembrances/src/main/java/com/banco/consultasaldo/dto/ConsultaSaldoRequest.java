package com.banco.consultasaldo.dto;

import jakarta.validation.constraints.NotBlank;
import jakarta.validation.constraints.Pattern;
import jakarta.validation.constraints.Size;

/**
 * TRADUCCIÓN COBOL → SPRING BOOT
 * BLOQUE 2: DATA DIVISION → DTO de entrada
 * BLOQUE 3: PROCEDURE DIVISION → ACCEPT → @RequestBody
 * -----------------------------------------
 * COBOL:
 *   3000-PEDIR-CUENTA.
 *       ACCEPT WS-NUMERO-CUENTA.          → @RequestBody ConsultaSaldoRequest
 *
 *   01 WS-NUMERO-CUENTA PIC X(20).       → String @Size(max=20)
 *
 * Validaciones (regla memoria):
 *   IF campo = SPACES → @NotBlank
 *   Validación de IBAN → @Pattern con regex
 */
public class ConsultaSaldoRequest {

    /** COBOL: 01 WS-NUMERO-CUENTA PIC X(20) + IF WS-NUMERO-CUENTA = SPACES → @NotBlank */
    @NotBlank(message = "El número de cuenta no puede estar vacío")
    @Size(max = 20, message = "El número de cuenta no puede superar 20 caracteres")
    @Pattern(regexp = "^ES\\d{18}$|^\\w{1,20}$",
             message = "Formato de cuenta inválido")
    private String numeroCuenta;

    public ConsultaSaldoRequest() {}

    public ConsultaSaldoRequest(String numeroCuenta) {
        this.numeroCuenta = numeroCuenta;
    }

    public String getNumeroCuenta() { return numeroCuenta; }
    public void setNumeroCuenta(String numeroCuenta) { this.numeroCuenta = numeroCuenta; }
}
