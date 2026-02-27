package com.banco.memory.entity;

import jakarta.persistence.*;
import java.math.BigDecimal;

@Entity
@Table(name = "CUENTAS")
public class CuentaEntity {

    @Id
    @Column(name = "NUMERO_CUENTA", length = 20)
    private String numeroCuenta;

    @Column(name = "TITULAR", length = 40)
    private String titular;

    @Column(name = "SALDO", precision = 12, scale = 2)
    private BigDecimal saldo;

    // Getters and Setters
    public String getNumeroCuenta() {
        return numeroCuenta;
    }

    public void setNumeroCuenta(String numeroCuenta) {
        this.numeroCuenta = numeroCuenta;
    }

    public String getTitular() {
        return titular;
    }

    public void setTitular(String titular) {
        this.titular = titular;
    }

    public BigDecimal getSaldo() {
        return saldo;
    }

    public void setSaldo(BigDecimal saldo) {
        this.saldo = saldo;
    }
}
