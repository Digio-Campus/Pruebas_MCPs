package com.banco.dto;

import java.math.BigDecimal;

public record TransferenciaResponse(
    String cuentaOrigen,
    String cuentaDestino,
    BigDecimal importe,
    BigDecimal comision,
    BigDecimal importeTotal,
    BigDecimal saldoOrigenDespues,
    BigDecimal saldoDestinoDespues) {}
