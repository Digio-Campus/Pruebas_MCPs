package com.banco.dto;

import java.math.BigDecimal;

public record SaldoResponse(
    String numeroCuenta,
    String titular,
    String tipoCuenta,
    BigDecimal saldoDisponible,
    BigDecimal saldoRetenido,
    BigDecimal saldoTotal) {}
