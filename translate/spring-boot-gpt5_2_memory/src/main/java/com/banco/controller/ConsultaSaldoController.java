package com.banco.controller;

import com.banco.dto.SaldoResponse;
import com.banco.service.BancoService;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

/** Traducción del COBOL: BANCO-CONSULTA-SALDO. */
@RestController
@RequestMapping("/api/saldo")
public class ConsultaSaldoController {

  private final BancoService bancoService;

  public ConsultaSaldoController(BancoService bancoService) {
    this.bancoService = bancoService;
  }

  @GetMapping("/{numeroCuenta}")
  public SaldoResponse consultar(@PathVariable String numeroCuenta) {
    return bancoService.consultarSaldo(numeroCuenta);
  }
}
