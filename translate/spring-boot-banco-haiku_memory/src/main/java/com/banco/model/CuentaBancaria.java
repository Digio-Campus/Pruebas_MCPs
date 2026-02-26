package com.banco.model;

import jakarta.persistence.*;
import jakarta.validation.constraints.NotBlank;
import jakarta.validation.constraints.Size;
import lombok.*;

import java.math.BigDecimal;
import java.util.ArrayList;
import java.util.List;

/**
 * Entidad que representa una cuenta bancaria.
 * Traducida del BLOQUE-DATA-DIVISION de BANCO-INGRESOS.cbl
 */
@Entity
@Table(name = "cuentas_bancarias")
@Data
@NoArgsConstructor
@AllArgsConstructor
@Builder
public class CuentaBancaria {
    
    @Id
    @GeneratedValue(strategy = GenerationType.IDENTITY)
    private Long id;
    
    @Column(name = "numero_cuenta", unique = true, nullable = false, length = 20)
    @NotBlank(message = "El número de cuenta no puede estar vacío")
    @Size(max = 20, message = "El número de cuenta debe tener máximo 20 caracteres")
    private String numeroCuenta;
    
    @Column(name = "titular", nullable = false, length = 40)
    @NotBlank(message = "El nombre del titular no puede estar vacío")
    @Size(max = 40, message = "El nombre del titular debe tener máximo 40 caracteres")
    private String titular;
    
    @Column(name = "saldo", precision = 12, scale = 2, nullable = false)
    @Builder.Default
    private BigDecimal saldo = BigDecimal.ZERO;
    
    @OneToMany(mappedBy = "cuentaBancaria", cascade = CascadeType.ALL, orphanRemoval = true)
    @Builder.Default
    private List<Ingreso> ingresos = new ArrayList<>();
    
    @Column(name = "fecha_creacion")
    private String fechaCreacion;
    
    /**
     * Método para añadir un ingreso a la cuenta
     * Mapeo de PROCEDURE DIVISION - párrafo 3000-REGISTRAR-INGRESOS
     */
    public void agregarIngreso(Ingreso ingreso) {
        ingreso.setCuentaBancaria(this);
        this.ingresos.add(ingreso);
        this.saldo = this.saldo.add(ingreso.getImporte());
    }
    
    /**
     * Método para calcular la suma total de ingresos
     * Mapeo de PROCEDURE DIVISION - párrafo 4000-CALCULAR-TOTAL
     */
    public BigDecimal calcularTotalIngresos() {
        return this.ingresos.stream()
                .map(Ingreso::getImporte)
                .reduce(BigDecimal.ZERO, BigDecimal::add);
    }
    
    /**
     * Método para obtener el número de ingresos registrados
     */
    public int obtenerNumeroIngresos() {
        return this.ingresos.size();
    }
}
