package com.banco.dto;

import java.math.BigDecimal;
import java.time.LocalDateTime;

public record MovimientoDto(LocalDateTime fecha, String concepto, String tipo, BigDecimal importe) {}
