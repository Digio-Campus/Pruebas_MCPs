package com.banco.exception;

public class TransferenciaNoValidaException extends RuntimeException {
    public TransferenciaNoValidaException(String mensaje) {
        super(mensaje);
    }
}
