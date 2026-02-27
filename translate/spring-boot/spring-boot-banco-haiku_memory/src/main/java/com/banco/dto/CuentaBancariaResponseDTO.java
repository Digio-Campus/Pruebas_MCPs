package com.banco.dto;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.math.BigDecimal;
import java.util.List;

/**
 * DTO para responder con los detalles de una cuenta bancaria y sus ingresos.
 * Mapeo de PROCEDURE DIVISION - párrafo 5000-MOSTRAR-RESUMEN (DISPLAY completo de resumen)
 */
@Data
@NoArgsConstructor
@AllArgsConstructor
@Builder
public class CuentaBancariaResponseDTO {
    
    private Long id;
    
    private String numeroCuenta;
    
    private String titular;
    
    private BigDecimal saldo;
    
    private Integer numeroIngresos;
    
    private BigDecimal sumaTotalIngresos;
    
    private List<IngresoResponseDTO> ingresos;
}
