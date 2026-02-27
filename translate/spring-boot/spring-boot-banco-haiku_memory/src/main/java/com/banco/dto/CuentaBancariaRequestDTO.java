package com.banco.dto;

import jakarta.validation.constraints.NotBlank;
import jakarta.validation.constraints.Size;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

/**
 * DTO para crear o actualizar una cuenta bancaria.
 * Mapeo de PROCEDURE DIVISION - párrafo 2000-PEDIR-DATOS-CUENTA (ACCEPT statements)
 */
@Data
@NoArgsConstructor
@AllArgsConstructor
@Builder
public class CuentaBancariaRequestDTO {
    
    @NotBlank(message = "El número de cuenta no puede estar vacío")
    @Size(max = 20, message = "El número de cuenta debe tener máximo 20 caracteres")
    private String numeroCuenta;
    
    @NotBlank(message = "El nombre del titular no puede estar vacío")
    @Size(max = 40, message = "El nombre del titular debe tener máximo 40 caracteres")
    private String titular;
}
