package com.banco.repository;

import com.banco.model.Movimiento;
import com.banco.model.CuentaBancaria;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.stereotype.Repository;

import java.util.List;

@Repository
public interface MovimientoRepository extends JpaRepository<Movimiento, Long> {
    List<Movimiento> findByCuentaOrderByFechaAsc(CuentaBancaria cuenta);
}
