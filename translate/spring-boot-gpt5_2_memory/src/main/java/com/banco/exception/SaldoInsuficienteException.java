package com.banco.exception;

import java.math.BigDecimal;

public class SaldoInsuficienteException extends RuntimeException {
  public SaldoInsuficienteException(String numeroCuenta, BigDecimal requerido, BigDecimal disponible) {
    super(
        "Saldo insuficiente en "
            + numeroCuenta
            + ": requerido="
            + requerido
            + ", disponible="
            + disponible);
  }
}
