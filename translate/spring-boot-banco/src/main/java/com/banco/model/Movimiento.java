package com.banco.model;

import jakarta.persistence.*;
import java.math.BigDecimal;

/**
 * Entidad Movimiento.
 * Traducida desde la tabla COBOL OCCURS:
 *   WS-MOVIMIENTO OCCURS 50 TIMES → List<Movimiento>
 *   WS-MOV-FECHA PIC X(10) → String fecha
 *   WS-MOV-CONCEPTO PIC X(30) → String concepto
 *   WS-MOV-TIPO PIC X(1) → String tipo (I=ingreso, G=gasto)
 *   WS-MOV-IMPORTE PIC 9(8)V99 → BigDecimal importe
 */
@Entity
@Table(name = "movimientos")
public class Movimiento {

    @Id
    @GeneratedValue(strategy = GenerationType.IDENTITY)
    private Long id;

    @Column(length = 10)
    private String fecha;

    @Column(length = 30)
    private String concepto;

    @Column(length = 1)
    private String tipo;

    @Column(precision = 10, scale = 2)
    private BigDecimal importe;

    @ManyToOne
    @JoinColumn(name = "numero_cuenta")
    private CuentaBancaria cuenta;

    public Movimiento() {}

    public Movimiento(String fecha, String concepto, String tipo,
                      BigDecimal importe, CuentaBancaria cuenta) {
        this.fecha = fecha;
        this.concepto = concepto;
        this.tipo = tipo;
        this.importe = importe;
        this.cuenta = cuenta;
    }

    public Long getId() { return id; }
    public void setId(Long id) { this.id = id; }
    public String getFecha() { return fecha; }
    public void setFecha(String fecha) { this.fecha = fecha; }
    public String getConcepto() { return concepto; }
    public void setConcepto(String concepto) { this.concepto = concepto; }
    public String getTipo() { return tipo; }
    public void setTipo(String tipo) { this.tipo = tipo; }
    public BigDecimal getImporte() { return importe; }
    public void setImporte(BigDecimal importe) { this.importe = importe; }
    public CuentaBancaria getCuenta() { return cuenta; }
    public void setCuenta(CuentaBancaria cuenta) { this.cuenta = cuenta; }
}
