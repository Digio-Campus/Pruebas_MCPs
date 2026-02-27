package com.banco.model;

import jakarta.persistence.*;
import jakarta.validation.constraints.NotBlank;
import jakarta.validation.constraints.Positive;
import jakarta.validation.constraints.Size;
import lombok.*;

import java.math.BigDecimal;
import java.time.LocalDateTime;

/**
 * Entidad que representa un ingreso en una cuenta bancaria.
 * Traducida del BLOQUE-DATA-DIVISION estructura WS-INGRESO-ENTRY
 * (nivel 05/10 dentro de WS-TABLA-INGRESOS)
 */
@Entity
@Table(name = "ingresos")
@Data
@NoArgsConstructor
@AllArgsConstructor
@Builder
public class Ingreso {
    
    @Id
    @GeneratedValue(strategy = GenerationType.IDENTITY)
    private Long id;
    
    @ManyToOne(fetch = FetchType.LAZY, optional = false)
    @JoinColumn(name = "cuenta_bancaria_id", nullable = false)
    private CuentaBancaria cuentaBancaria;
    
    @Column(name = "importe", precision = 10, scale = 2, nullable = false)
    @Positive(message = "El importe del ingreso debe ser positivo")
    private BigDecimal importe;
    
    @Column(name = "concepto", nullable = false, length = 30)
    @NotBlank(message = "El concepto del ingreso no puede estar vacío")
    @Size(max = 30, message = "El concepto debe tener máximo 30 caracteres")
    private String concepto;
    
    @Column(name = "fecha_ingreso")
    @Builder.Default
    private LocalDateTime fechaIngreso = LocalDateTime.now();
    
    @Column(name = "descripcion", length = 255)
    private String descripcion;
}
