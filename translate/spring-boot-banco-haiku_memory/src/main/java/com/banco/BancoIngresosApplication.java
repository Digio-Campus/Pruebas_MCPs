package com.banco;

import org.springframework.boot.SpringApplication;
import org.springframework.boot.autoconfigure.SpringBootApplication;

/**
 * Clase principal de la aplicación Spring Boot.
 * Traducción de IDENTIFICATION DIVISION → PROGRAM-ID. BANCO-INGRESOS
 * 
 * Esta aplicación implementa un sistema bancario para registrar ingresos,
 * basado en el programa COBOL: BANCO-INGRESOS
 */
@SpringBootApplication
public class BancoIngresosApplication {
    
    public static void main(String[] args) {
        SpringApplication.run(BancoIngresosApplication.class, args);
    }
}
