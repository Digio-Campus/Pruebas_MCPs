package com.banco.config;

import com.banco.model.CuentaBancaria;
import com.banco.model.Movimiento;
import com.banco.model.TipoMovimiento;
import com.banco.repository.CuentaBancariaRepository;
import com.banco.repository.MovimientoRepository;
import java.math.BigDecimal;
import java.time.LocalDate;
import java.time.LocalDateTime;
import org.springframework.boot.CommandLineRunner;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;

@Configuration
public class DataInitializer {

  @Bean
  CommandLineRunner init(CuentaBancariaRepository cuentas, MovimientoRepository movimientos) {
    return args -> {
      if (cuentas.count() > 0) {
        return;
      }

      // Traducción de los datos simulados del COBOL BANCO-CONSULTA-SALDO.
      CuentaBancaria c1 =
          new CuentaBancaria(
              "ES1234567890123456",
              "GARCIA LOPEZ, MARIA",
              "CORRIENTE",
              new BigDecimal("15250.75"),
              new BigDecimal("500.00"));
      CuentaBancaria c2 =
          new CuentaBancaria(
              "ES9876543210987654",
              "MARTINEZ RUIZ, PEDRO",
              "AHORRO",
              new BigDecimal("42000.00"),
              new BigDecimal("0.00"));
      CuentaBancaria c3 =
          new CuentaBancaria(
              "ES5555666677778888",
              "FERNANDEZ DIAZ, ANA",
              "NOMINA",
              new BigDecimal("3200.50"),
              new BigDecimal("150.00"));

      cuentas.save(c1);
      cuentas.save(c2);
      cuentas.save(c3);

      // Traducción de movimientos simulados del COBOL BANCO-EXTRACTO (asociados a c1).
      saveMovimiento(movimientos, c1, "2026-02-01", "NOMINA FEBRERO", TipoMovimiento.I, "2500.00");
      saveMovimiento(movimientos, c1, "2026-02-03", "ALQUILER VIVIENDA", TipoMovimiento.G, "850.00");
      saveMovimiento(movimientos, c1, "2026-02-05", "SUPERMERCADO", TipoMovimiento.G, "125.50");
      saveMovimiento(
          movimientos, c1, "2026-02-07", "TRANSFERENCIA RECIBIDA", TipoMovimiento.I, "300.00");
      saveMovimiento(movimientos, c1, "2026-02-10", "SEGURO COCHE", TipoMovimiento.G, "75.00");
      saveMovimiento(movimientos, c1, "2026-02-12", "LUZ ELECTRICA", TipoMovimiento.G, "95.30");
      saveMovimiento(movimientos, c1, "2026-02-15", "INGRESO EFECTIVO", TipoMovimiento.I, "500.00");
      saveMovimiento(movimientos, c1, "2026-02-18", "GASOLINERA", TipoMovimiento.G, "60.00");
    };
  }

  private static void saveMovimiento(
      MovimientoRepository repo,
      CuentaBancaria cuenta,
      String fechaIso,
      String concepto,
      TipoMovimiento tipo,
      String importe) {
    LocalDateTime fecha = LocalDate.parse(fechaIso).atStartOfDay();
    repo.save(new Movimiento(cuenta, fecha, concepto, tipo, new BigDecimal(importe)));
  }
}
