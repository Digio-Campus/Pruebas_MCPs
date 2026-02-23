package com.banco.dto;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.math.BigDecimal;
import java.time.LocalDateTime;

/**
 * DTO para responder con los detalles de un ingreso registrado.
 * Mapeo de PROCEDURE DIVISION - párrafo 5000-MOSTRAR-RESUMEN (DISPLAY statements)
 */
@Data
@NoArgsConstructor
@AllArgsConstructor
@Builder
public class IngresoResponseDTO {
    
    private Long id;
    
    private BigDecimal importe;
    
    private String concepto;
    
    private LocalDateTime fechaIngreso;
    
    private String descripcion;
}
