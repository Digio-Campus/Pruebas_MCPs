package com.banco.service;

import com.banco.dto.ExtractoResponse;
import com.banco.dto.IngresoRequest;
import com.banco.dto.IngresoResponse;
import com.banco.dto.MovimientoDto;
import com.banco.dto.SaldoResponse;
import com.banco.dto.TransferenciaRequest;
import com.banco.dto.TransferenciaResponse;
import com.banco.exception.CuentaNoEncontradaException;
import com.banco.exception.SaldoInsuficienteException;
import com.banco.exception.TransferenciaInvalidaException;
import com.banco.model.CuentaBancaria;
import com.banco.model.Movimiento;
import com.banco.model.TipoMovimiento;
import com.banco.repository.CuentaBancariaRepository;
import com.banco.repository.MovimientoRepository;
import java.math.BigDecimal;
import java.math.RoundingMode;
import java.time.LocalDateTime;
import java.util.Comparator;
import java.util.List;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

@Service
public class BancoService {

  private final CuentaBancariaRepository cuentaRepo;
  private final MovimientoRepository movRepo;

  public BancoService(CuentaBancariaRepository cuentaRepo, MovimientoRepository movRepo) {
    this.cuentaRepo = cuentaRepo;
    this.movRepo = movRepo;
  }

  public SaldoResponse consultarSaldo(String numeroCuenta) {
    CuentaBancaria c =
        cuentaRepo
            .findById(numeroCuenta)
            .orElseThrow(() -> new CuentaNoEncontradaException(numeroCuenta));

    return new SaldoResponse(
        c.getNumeroCuenta(),
        c.getTitular(),
        c.getTipoCuenta(),
        c.getSaldoDisponible(),
        c.getSaldoRetenido(),
        c.getSaldoTotal());
  }

  @Transactional
  public IngresoResponse ingresar(String numeroCuenta, IngresoRequest req) {
    CuentaBancaria c =
        cuentaRepo
            .findById(numeroCuenta)
            .orElseThrow(() -> new CuentaNoEncontradaException(numeroCuenta));

    BigDecimal nuevoSaldo = c.getSaldoDisponible().add(req.importe());
    c.setSaldoDisponible(nuevoSaldo);

    movRepo.save(new Movimiento(c, LocalDateTime.now(), req.concepto(), TipoMovimiento.I, req.importe()));

    return new IngresoResponse(c.getNumeroCuenta(), req.importe(), req.concepto(), c.getSaldoDisponible());
  }

  @Transactional
  public TransferenciaResponse transferir(TransferenciaRequest req) {
    if (req.cuentaOrigen().equals(req.cuentaDestino())) {
      throw new TransferenciaInvalidaException("Cuenta origen = cuenta destino");
    }

    CuentaBancaria origen =
        cuentaRepo
            .findById(req.cuentaOrigen())
            .orElseThrow(() -> new CuentaNoEncontradaException(req.cuentaOrigen()));
    CuentaBancaria destino =
        cuentaRepo
            .findById(req.cuentaDestino())
            .orElseThrow(() -> new CuentaNoEncontradaException(req.cuentaDestino()));

    BigDecimal comision = BigDecimal.ZERO;
    // COBOL: 0.5% si importe > 3000
    if (req.importe().compareTo(new BigDecimal("3000")) > 0) {
      comision = req.importe().multiply(new BigDecimal("0.005")).setScale(2, RoundingMode.HALF_UP);
    }
    BigDecimal total = req.importe().add(comision);

    if (origen.getSaldoDisponible().compareTo(total) < 0) {
      throw new SaldoInsuficienteException(origen.getNumeroCuenta(), total, origen.getSaldoDisponible());
    }

    origen.setSaldoDisponible(origen.getSaldoDisponible().subtract(total));
    destino.setSaldoDisponible(destino.getSaldoDisponible().add(req.importe()));

    String conceptoOrigen = "TRANSFERENCIA A " + destino.getNumeroCuenta() + ": " + req.concepto();
    String conceptoDestino = "TRANSFERENCIA DE " + origen.getNumeroCuenta() + ": " + req.concepto();

    movRepo.save(new Movimiento(origen, LocalDateTime.now(), conceptoOrigen, TipoMovimiento.G, total));
    movRepo.save(new Movimiento(destino, LocalDateTime.now(), conceptoDestino, TipoMovimiento.I, req.importe()));

    return new TransferenciaResponse(
        origen.getNumeroCuenta(),
        destino.getNumeroCuenta(),
        req.importe(),
        comision,
        total,
        origen.getSaldoDisponible(),
        destino.getSaldoDisponible());
  }

  public ExtractoResponse extracto(String numeroCuenta, int limit) {
    CuentaBancaria c =
        cuentaRepo
            .findById(numeroCuenta)
            .orElseThrow(() -> new CuentaNoEncontradaException(numeroCuenta));

    List<Movimiento> movimientos = movRepo.findTop50ByCuentaNumeroCuentaOrderByFechaDesc(numeroCuenta);
    movimientos =
        movimientos.stream()
            .sorted(Comparator.comparing(Movimiento::getFecha))
            .toList();

    if (limit > 0 && movimientos.size() > limit) {
      movimientos = movimientos.subList(Math.max(0, movimientos.size() - limit), movimientos.size());
    }

    BigDecimal totalIngresos = BigDecimal.ZERO;
    BigDecimal totalGastos = BigDecimal.ZERO;
    for (Movimiento m : movimientos) {
      if (m.getTipo() == TipoMovimiento.I) {
        totalIngresos = totalIngresos.add(m.getImporte());
      } else {
        totalGastos = totalGastos.add(m.getImporte());
      }
    }

    BigDecimal saldoFinal = c.getSaldoDisponible();
    BigDecimal saldoInicial = saldoFinal.subtract(totalIngresos).add(totalGastos);

    List<MovimientoDto> dtos =
        movimientos.stream()
            .map(m -> new MovimientoDto(m.getFecha(), m.getConcepto(), m.getTipo().name(), m.getImporte()))
            .toList();

    return new ExtractoResponse(
        c.getNumeroCuenta(),
        c.getTitular(),
        saldoInicial,
        totalIngresos,
        totalGastos,
        saldoFinal,
        dtos);
  }
}
