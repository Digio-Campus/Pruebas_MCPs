package com.banco.controller;

import com.banco.dto.CuentaBancariaRequestDTO;
import com.banco.dto.CuentaBancariaResponseDTO;
import com.banco.dto.IngresoPedidoDTO;
import com.banco.service.IngresosService;
import jakarta.validation.Valid;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;

import java.util.List;

/**
 * Controlador REST para gestionar operaciones de ingresos bancarios.
 * Traducción del PROGRAMA: BANCO-INGRESOS
 * 
 * Mapeos:
 * - Comentarios de IDENTIFICATION DIVISION → Documentación de la clase
 * - Párrafos COBOL → Métodos del endpoint
 * - DISPLAY → Logger.info() en el servicio
 * - ACCEPT → @RequestBody, @RequestParam, @PathVariable
 * 
 * Referencia memory: BLOQUE-PROCEDURE-DIVISION (implementa)
 */
@Slf4j
@RestController
@RequestMapping("/api/ingresos")
public class IngresosController {
    
    @Autowired
    private IngresosService ingresosService;
    
    /**
     * Endpoint principal para procesar ingresos en una cuenta
     * POST /api/ingresos/procesar
     * 
     * Mapeo de párrafos COBOL:
     * - 0000-PRINCIPAL: orquestación del flujo
     * - 1000-INICIALIZAR: inicialización
     * - 2000-PEDIR-DATOS-CUENTA: datos de entrada (cuerpo JSON)
     * - 3000-REGISTRAR-INGRESOS: registro de ingresos (lista de ingresos)
     * - 4000-CALCULAR-TOTAL: cálculo automático
     * - 5000-MOSTRAR-RESUMEN: respuesta con resumen
     * - 9000-FINALIZAR: fin del proceso
     */
    @PostMapping("/procesar")
    public ResponseEntity<CuentaBancariaResponseDTO> procesarIngresos(
            @RequestParam String numeroCuenta,
            @RequestParam String titular,
            @Valid @RequestBody List<IngresoPedidoDTO> ingresos) {
        
        log.info("Iniciando proceso de ingresos para cuenta: {}", numeroCuenta);
        
        CuentaBancariaResponseDTO resultado = ingresosService.procesarIngresos(
                numeroCuenta,
                titular,
                ingresos);
        
        return ResponseEntity.status(HttpStatus.CREATED).body(resultado);
    }
    
    /**
     * Obtiene los detalles completos de una cuenta bancaria incluido resumen de ingresos
     * GET /api/ingresos/cuentas/{id}
     */
    @GetMapping("/cuentas/{id}")
    public ResponseEntity<CuentaBancariaResponseDTO> obtenerCuenta(@PathVariable Long id) {
        log.info("Obteniendo cuenta con ID: {}", id);
        
        CuentaBancariaResponseDTO cuenta = ingresosService.obtenerCuenta(id);
        
        return ResponseEntity.ok(cuenta);
    }
    
    /**
     * Obtiene una cuenta bancaria por su número de cuenta
     * GET /api/ingresos/cuentas/numero/{numeroCuenta}
     */
    @GetMapping("/cuentas/numero/{numeroCuenta}")
    public ResponseEntity<CuentaBancariaResponseDTO> obtenerCuentaPorNumero(
            @PathVariable String numeroCuenta) {
        
        log.info("Obteniendo cuenta con número: {}", numeroCuenta);
        
        CuentaBancariaResponseDTO cuenta = ingresosService.obtenerCuentaPorNumero(numeroCuenta);
        
        return ResponseEntity.ok(cuenta);
    }
    
    /**
     * Agrega un nuevo ingreso a una cuenta existente
     * POST /api/ingresos/cuentas/{cuentaId}/agregar-ingreso
     */
    @PostMapping("/cuentas/{cuentaId}/agregar-ingreso")
    public ResponseEntity<CuentaBancariaResponseDTO> agregarIngreso(
            @PathVariable Long cuentaId,
            @Valid @RequestBody IngresoPedidoDTO ingresoPedido) {
        
        log.info("Agregando ingreso a cuenta con ID: {}", cuentaId);
        
        CuentaBancariaResponseDTO cuenta = ingresosService.agregarIngreso(cuentaId, ingresoPedido);
        
        return ResponseEntity.ok(cuenta);
    }
}
