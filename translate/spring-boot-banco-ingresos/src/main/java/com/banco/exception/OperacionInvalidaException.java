package com.banco.exception;

/**
 * Excepción lanzada cuando se intenta realizar una operación inválida.
 * Patrón de validación del dominio bancario.
 */
public class OperacionInvalidaException extends RuntimeException {
    
    public OperacionInvalidaException(String mensaje) {
        super(mensaje);
    }
    
    public OperacionInvalidaException(String mensaje, Throwable causa) {
        super(mensaje, causa);
    }
}
