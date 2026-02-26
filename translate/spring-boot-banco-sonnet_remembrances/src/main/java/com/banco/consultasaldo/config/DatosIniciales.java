package com.banco.consultasaldo.config;

import com.banco.consultasaldo.model.CuentaBancaria;
import com.banco.consultasaldo.repository.CuentaBancariaRepository;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.boot.CommandLineRunner;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;

import java.math.BigDecimal;

/**
 * TRADUCCIÓN COBOL → SPRING BOOT
 * BLOQUE 4: ENVIRONMENT DIVISION → @Configuration
 * BLOQUE 3: 2000-CARGAR-DATOS-SIMULADOS → CommandLineRunner (carga inicial)
 * -----------------------------------------
 * COBOL:
 *   2000-CARGAR-DATOS-SIMULADOS.
 *       MOVE "ES1234567890123456" TO WS-DB-NUM-CUENTA(1)
 *       MOVE "GARCIA LOPEZ, MARIA" TO WS-DB-TITULAR(1)
 *       MOVE "CORRIENTE" TO WS-DB-TIPO(1)
 *       MOVE 15250.75 TO WS-DB-SALDO-DISP(1)       ← BigDecimal (NUNCA double)
 *       MOVE 500.00 TO WS-DB-SALDO-RET(1)
 *       ... (3 cuentas en total)
 *
 * Regla memoria:
 *   ENVIRONMENT DIVISION → @Configuration class en Spring Boot
 *   Datos hardcoded COBOL → datos de inicialización en CommandLineRunner
 *   Operaciones monetarias siempre con BigDecimal, NUNCA double/float
 */
@Configuration
public class DatosIniciales {

    private static final Logger logger = LoggerFactory.getLogger(DatosIniciales.class);

    @Bean
    CommandLineRunner cargarDatosSimulados(CuentaBancariaRepository repo) {
        return args -> {
            // COBOL: MOVE "ES1234567890123456" TO WS-DB-NUM-CUENTA(1) ...
            repo.save(new CuentaBancaria(
                    "ES1234567890123456",
                    "GARCIA LOPEZ, MARIA",
                    "CORRIENTE",
                    new BigDecimal("15250.75"),  // PIC S9(10)V99: BigDecimal, NUNCA double
                    new BigDecimal("500.00")
            ));
            // COBOL: MOVE "ES9876543210987654" TO WS-DB-NUM-CUENTA(2) ...
            repo.save(new CuentaBancaria(
                    "ES9876543210987654",
                    "MARTINEZ RUIZ, PEDRO",
                    "AHORRO",
                    new BigDecimal("42000.00"),
                    new BigDecimal("0.00")
            ));
            // COBOL: MOVE "ES5555666677778888" TO WS-DB-NUM-CUENTA(3) ...
            repo.save(new CuentaBancaria(
                    "ES5555666677778888",
                    "FERNANDEZ DIAZ, ANA",
                    "NOMINA",
                    new BigDecimal("3200.50"),
                    new BigDecimal("150.00")
            ));
            logger.info("2000-CARGAR-DATOS-SIMULADOS: {} cuentas cargadas en BD", repo.count());
        };
    }
}
