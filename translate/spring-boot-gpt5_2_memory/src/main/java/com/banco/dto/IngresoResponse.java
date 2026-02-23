package com.banco.dto;

import java.math.BigDecimal;

public record IngresoResponse(
    String numeroCuenta,
    BigDecimal importe,
    String concepto,
    BigDecimal saldoDisponibleDespues) {}
