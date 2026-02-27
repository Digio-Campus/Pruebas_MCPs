package com.banco.consultasaldo.exception;

import org.springframework.http.HttpStatus;
import org.springframework.web.bind.annotation.ResponseStatus;

/**
 * TRADUCCIÓN COBOL → SPRING BOOT
 * Patrones bancarios (regla memoria):
 *   Validaciones de saldo → @Service con lógica de negocio + excepciones custom
 * -----------------------------------------
 * COBOL:
 *   6000-CUENTA-NO-ENCONTRADA.
 *       DISPLAY "ERROR: Cuenta no encontrada."
 *       DISPLAY "Verifique el numero de cuenta e intente de nuevo."
 *
 * En lugar de DISPLAY del error, se lanza excepción HTTP 404.
 */
@ResponseStatus(HttpStatus.NOT_FOUND)
public class CuentaNoEncontradaException extends RuntimeException {

    public CuentaNoEncontradaException(String numeroCuenta) {
        super("Cuenta no encontrada: " + numeroCuenta +
              ". Verifique el número de cuenta e intente de nuevo.");
    }
}
