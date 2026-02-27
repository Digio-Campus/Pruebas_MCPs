package com.banco.consultasaldo.repository;

import com.banco.consultasaldo.model.CuentaBancaria;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.stereotype.Repository;

import java.util.Optional;

/**
 * TRADUCCIÓN COBOL → SPRING BOOT
 * BLOQUE 2: DATA DIVISION, FILE SECTION → @Repository Spring Data JPA
 * -----------------------------------------
 * COBOL:
 *   4000-BUSCAR-CUENTA.
 *       PERFORM VARYING WS-IDX FROM 1 BY 1
 *          UNTIL WS-IDX > 3
 *          IF WS-DB-NUM-CUENTA(WS-IDX) = WS-NUMERO-CUENTA
 *             MOVE ... TO WS-TITULAR ...
 *             MOVE 'S' TO WS-CUENTA-ENCONTRADA
 *          END-IF
 *       END-PERFORM.
 *
 * El bucle PERFORM VARYING que busca en la tabla en memoria
 * se reemplaza por una query JPA derivada del nombre del método.
 *
 * Regla memoria: FILE SECTION → @Entity + @Repository con Spring Data JPA
 */
@Repository
public interface CuentaBancariaRepository extends JpaRepository<CuentaBancaria, Long> {

    /**
     * COBOL: IF WS-DB-NUM-CUENTA(WS-IDX) = WS-NUMERO-CUENTA
     *        MOVE 'S' TO WS-CUENTA-ENCONTRADA
     * Java:  findByNumeroCuenta → Optional.empty() si no encontrada
     */
    Optional<CuentaBancaria> findByNumeroCuenta(String numeroCuenta);
}
