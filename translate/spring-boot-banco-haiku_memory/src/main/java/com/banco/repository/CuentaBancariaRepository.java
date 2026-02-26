package com.banco.repository;

import com.banco.model.CuentaBancaria;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.stereotype.Repository;

import java.util.Optional;

/**
 * Repositorio para gestionar operaciones de persistencia de CuentaBancaria.
 * Mapeo de BLOQUE-ENVIRONMENT-DIVISION
 */
@Repository
public interface CuentaBancariaRepository extends JpaRepository<CuentaBancaria, Long> {
    
    /**
     * Busca una cuenta bancaria por su número de cuenta
     */
    Optional<CuentaBancaria> findByNumeroCuenta(String numeroCuenta);
}
