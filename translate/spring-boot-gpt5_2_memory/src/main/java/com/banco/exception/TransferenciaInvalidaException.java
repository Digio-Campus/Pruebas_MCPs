package com.banco.exception;

public class TransferenciaInvalidaException extends RuntimeException {
  public TransferenciaInvalidaException(String motivo) {
    super("Transferencia no valida: " + motivo);
  }
}
