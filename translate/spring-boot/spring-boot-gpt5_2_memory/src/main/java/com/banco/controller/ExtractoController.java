package com.banco.controller;

import com.banco.dto.ExtractoResponse;
import com.banco.service.BancoService;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.RestController;

/** Traducción del COBOL: BANCO-EXTRACTO. */
@RestController
@RequestMapping("/api/extracto")
public class ExtractoController {

  private final BancoService bancoService;

  public ExtractoController(BancoService bancoService) {
    this.bancoService = bancoService;
  }

  @GetMapping("/{numeroCuenta}")
  public ExtractoResponse extracto(
      @PathVariable String numeroCuenta, @RequestParam(defaultValue = "50") int limit) {
    return bancoService.extracto(numeroCuenta, limit);
  }
}
