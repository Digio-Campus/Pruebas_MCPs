package com.banco.baseline.controller;

import com.banco.baseline.service.BancoTransferenciaService;
import com.banco.baseline.service.BancoIngresosService;
import org.springframework.web.bind.annotation.*;
import java.math.BigDecimal;

@RestController
@RequestMapping("/api/baseline")
public class BancoController {

    private final BancoTransferenciaService transferenciaService;
    private final BancoIngresosService ingresosService;

    public BancoController(BancoTransferenciaService tService, BancoIngresosService iService) {
        this.transferenciaService = tService;
        this.ingresosService = iService;
    }

    @PostMapping("/transferencia")
    public String realizarTransferencia(@RequestParam String origen,
            @RequestParam String destino,
            @RequestParam BigDecimal importe,
            @RequestParam String concepto) {
        transferenciaService.inicializar();
        transferenciaService.pedirDatosTransferencia(origen, destino, importe, concepto);

        if (transferenciaService.validarTransferencia()) {
            transferenciaService.ejecutarTransferencia();
            return transferenciaService.mostrarJustificante();
        } else {
            return "ERROR: Transferencia no valida. Posibles causas: Saldo insuficiente, misma cuenta, importe no valido.";
        }
    }

    @PostMapping("/ingreso")
    public String realizarIngreso(@RequestParam String cuenta,
            @RequestParam String titular,
            @RequestParam BigDecimal importe,
            @RequestParam String concepto) {
        ingresosService.inicializar(cuenta, titular);
        ingresosService.registrarIngreso(importe, concepto);
        return ingresosService.mostrarResumen();
    }
}
