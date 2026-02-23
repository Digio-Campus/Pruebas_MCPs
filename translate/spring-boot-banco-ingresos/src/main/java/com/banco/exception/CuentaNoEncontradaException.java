package com.banco.exception;

/**
 * Excepción lanzada cuando no se encuentra una cuenta bancaria.
 * Patrón de validación del dominio bancario.
 */
public class CuentaNoEncontradaException extends RuntimeException {
    
    public CuentaNoEncontradaException(String mensaje) {
        super(mensaje);
    }
    
    public CuentaNoEncontradaException(String mensaje, Throwable causa) {
        super(mensaje, causa);
    }
}
