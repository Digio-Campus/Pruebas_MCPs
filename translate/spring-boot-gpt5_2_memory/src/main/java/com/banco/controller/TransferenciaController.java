package com.banco.controller;

import com.banco.dto.TransferenciaRequest;
import com.banco.dto.TransferenciaResponse;
import com.banco.service.BancoService;
import jakarta.validation.Valid;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

/** Traducción del COBOL: BANCO-TRANSFERENCIA. */
@RestController
@RequestMapping("/api/transferencias")
public class TransferenciaController {

  private final BancoService bancoService;

  public TransferenciaController(BancoService bancoService) {
    this.bancoService = bancoService;
  }

  @PostMapping
  public TransferenciaResponse transferir(@Valid @RequestBody TransferenciaRequest req) {
    return bancoService.transferir(req);
  }
}
