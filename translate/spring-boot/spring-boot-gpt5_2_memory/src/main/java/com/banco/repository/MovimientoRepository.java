package com.banco.repository;

import com.banco.model.Movimiento;
import java.util.List;
import org.springframework.data.jpa.repository.JpaRepository;

public interface MovimientoRepository extends JpaRepository<Movimiento, Long> {
  List<Movimiento> findTop50ByCuentaNumeroCuentaOrderByFechaDesc(String numeroCuenta);
}
