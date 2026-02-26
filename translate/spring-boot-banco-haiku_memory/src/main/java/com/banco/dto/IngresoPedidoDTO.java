package com.banco.dto;

import jakarta.validation.constraints.NotBlank;
import jakarta.validation.constraints.Positive;
import jakarta.validation.constraints.Size;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.math.BigDecimal;

/**
 * DTO para solicitar un nuevo ingreso que será registrado.
 * Mapeo de PROCEDURE DIVISION - párrafo 3000-REGISTRAR-INGRESOS (ACCEPT statements)
 */
@Data
@NoArgsConstructor
@AllArgsConstructor
@Builder
public class IngresoPedidoDTO {
    
    @Positive(message = "El importe debe ser positivo")
    private BigDecimal importe;
    
    @NotBlank(message = "El concepto no puede estar vakío")
    @Size(max = 30, message = "El concepto debe tener máximo 30 caracteres")
    private String concepto;
    
    private String descripcion;
}
