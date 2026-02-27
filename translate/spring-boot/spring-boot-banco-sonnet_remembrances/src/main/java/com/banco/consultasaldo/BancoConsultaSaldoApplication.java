package com.banco.consultasaldo;

import org.springframework.boot.SpringApplication;
import org.springframework.boot.autoconfigure.SpringBootApplication;

/**
 * TRADUCCIÓN COBOL → SPRING BOOT
 * BLOQUE 1: IDENTIFICATION DIVISION
 * -----------------------------------------
 * COBOL:  PROGRAM-ID. BANCO-CONSULTA-SALDO.
 * JAVA:   @SpringBootApplication BancoConsultaSaldoApplication
 *
 * Patrón de nombrado (regla memoria):
 *   kebab-case COBOL → PascalCase Java
 *   BANCO-CONSULTA-SALDO → BancoConsultaSaldo
 *
 * @author Proyecto-MCPs  (COBOL: AUTHOR. PROYECTO-MCPS)
 */
@SpringBootApplication
public class BancoConsultaSaldoApplication {

    public static void main(String[] args) {
        SpringApplication.run(BancoConsultaSaldoApplication.class, args);
    }
}
