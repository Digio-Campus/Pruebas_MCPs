package com.banco.controller;

import com.banco.dto.IngresoRequest;
import com.banco.dto.IngresoResponse;
import com.banco.service.BancoService;
import jakarta.validation.Valid;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

/** Traducción del COBOL: BANCO-INGRESOS. */
@RestController
@RequestMapping("/api/ingresos")
public class IngresosController {

  private final BancoService bancoService;

  public IngresosController(BancoService bancoService) {
    this.bancoService = bancoService;
  }

  @PostMapping("/{numeroCuenta}")
  public IngresoResponse ingresar(
      @PathVariable String numeroCuenta, @Valid @RequestBody IngresoRequest req) {
    return bancoService.ingresar(numeroCuenta, req);
  }
}
