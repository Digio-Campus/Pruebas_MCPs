package com.banco.dto;

import java.math.BigDecimal;
import java.util.List;

public record ExtractoResponse(
    String numeroCuenta,
    String titular,
    BigDecimal saldoInicial,
    BigDecimal totalIngresos,
    BigDecimal totalGastos,
    BigDecimal saldoFinal,
    List<MovimientoDto> movimientos) {}
