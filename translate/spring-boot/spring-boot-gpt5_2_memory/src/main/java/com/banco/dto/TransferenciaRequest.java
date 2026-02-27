package com.banco.dto;

import jakarta.validation.constraints.NotBlank;
import jakarta.validation.constraints.NotNull;
import jakarta.validation.constraints.Positive;
import jakarta.validation.constraints.Size;
import java.math.BigDecimal;

public record TransferenciaRequest(
    @NotBlank @Size(max = 20) String cuentaOrigen,
    @NotBlank @Size(max = 20) String cuentaDestino,
    @NotNull @Positive BigDecimal importe,
    @NotBlank @Size(max = 50) String concepto) {}
