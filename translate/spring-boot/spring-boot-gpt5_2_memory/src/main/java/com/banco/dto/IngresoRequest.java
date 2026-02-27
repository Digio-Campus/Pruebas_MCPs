package com.banco.dto;

import jakarta.validation.constraints.NotBlank;
import jakarta.validation.constraints.NotNull;
import jakarta.validation.constraints.Positive;
import jakarta.validation.constraints.Size;
import java.math.BigDecimal;

public record IngresoRequest(
    @NotNull @Positive BigDecimal importe,
    @NotBlank @Size(max = 30) String concepto) {}
