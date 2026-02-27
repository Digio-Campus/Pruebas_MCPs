package com.banco.service;

import com.banco.dto.CuentaBancariaRequestDTO;
import com.banco.dto.CuentaBancariaResponseDTO;
import com.banco.dto.IngresoPedidoDTO;
import com.banco.dto.IngresoResponseDTO;
import com.banco.exception.CuentaNoEncontradaException;
import com.banco.exception.OperacionInvalidaException;
import com.banco.model.CuentaBancaria;
import com.banco.model.Ingreso;
import com.banco.repository.CuentaBancariaRepository;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.math.BigDecimal;
import java.time.LocalDateTime;
import java.time.format.DateTimeFormatter;
import java.util.List;
import java.util.Optional;
import java.util.stream.Collectors;

/**
 * Servicio que implementa la lógica de negocio para ingresos bancarios.
 * Traducción de PROGRAMA: BANCO-INGRESOS y PROCEDURE DIVISION.
 * 
 * Mapeos:
 * - PROGRAM-ID BANCO-INGRESOS → clase BancoIngresosService
 * - Párrafos PERFORM → métodos del servicio
 * - DISPLAY → Logger.info()
 * - ACCEPT → parámetros de entrada
 * - STOP RUN → return/fin del método
 * 
 * Referencia memory: BLOQUE-PROCEDURE-DIVISION
 */
@Slf4j
@Service
@Transactional
public class IngresosService {
    
    @Autowired
    private CuentaBancariaRepository cuentaBancariaRepository;
    
    /**
     * Párrafo 0000-PRINCIPAL en COBOL
     * Proceso principal: inicializar, pedir datos, registrar ingresos, calcular y mostrar resumen
     */
    public CuentaBancariaResponseDTO procesarIngresos(
            String numeroCuenta,
            String titular,
            List<IngresoPedidoDTO> ingresosDTO) {
        
        // 1000-INICIALIZAR
        inicializar(numeroCuenta, titular);
        
        // 2000-PEDIR-DATOS-CUENTA y creación/búsqueda de la cuenta
        CuentaBancaria cuenta = obtenerOCrearCuenta(numeroCuenta, titular);
        
        // 3000-REGISTRAR-INGRESOS
        registrarIngresos(cuenta, ingresosDTO);
        
        // 4000-CALCULAR-TOTAL
        BigDecimal totalIngresos = calcularTotal(cuenta);
        
        // 5000-MOSTRAR-RESUMEN
        mostrarResumen(cuenta);
        
        // 9000-FINALIZAR
        finalizar();
        
        return construirResponse(cuenta);
    }
    
    /**
     * Párrafo 1000-INICIALIZAR
     * Inicializa las variables del programa
     */
    private void inicializar(String numeroCuenta, String titular) {
        log.info("==========================================");
        log.info("   SISTEMA DE INGRESOS BANCARIOS");
        DateTimeFormatter formatter = DateTimeFormatter.ofPattern("yyyy-MM-dd HH:mm:ss");
        log.info("   Fecha: {}", LocalDateTime.now().format(formatter));
        log.info("==========================================");
    }
    
    /**
     * Párrafo 2000-PEDIR-DATOS-CUENTA
     * Obtiene una cuenta existente o crea una nueva
     */
    private CuentaBancaria obtenerOCrearCuenta(String numeroCuenta, String titular) {
        Optional<CuentaBancaria> cuentaExistente = cuentaBancariaRepository.findByNumeroCuenta(numeroCuenta);
        
        if (cuentaExistente.isPresent()) {
            log.info("Cuenta existente encontrada: {}", numeroCuenta);
            return cuentaExistente.get();
        }
        
        // Crear nueva cuenta
        CuentaBancaria nuevaCuenta = CuentaBancaria.builder()
                .numeroCuenta(numeroCuenta)
                .titular(titular)
                .saldo(BigDecimal.ZERO)
                .fechaCreacion(LocalDateTime.now().toString())
                .build();
        
        log.info("Creando nueva cuenta: {} - Titular: {}", numeroCuenta, titular);
        return cuentaBancariaRepository.save(nuevaCuenta);
    }
    
    /**
     * Párrafo 3000-REGISTRAR-INGRESOS
     * PERFORM UNTIL WS-CONTINUAR = 'N'
     * Registra los ingresos proporcionados
     */
    private void registrarIngresos(CuentaBancaria cuenta, List<IngresoPedidoDTO> ingresosDTO) {
        int contador = 1;
        
        for (IngresoPedidoDTO pedido : ingresosDTO) {
            log.info("--- Ingreso #{} ---", contador);
            log.info("Importe del ingreso: {}", pedido.getImporte());
            log.info("Concepto del ingreso: {}", pedido.getConcepto());
            
            // Validación: importe positivo
            if (pedido.getImporte() == null || pedido.getImporte().compareTo(BigDecimal.ZERO) <= 0) {
                throw new OperacionInvalidaException("El importe debe ser un valor positivo");
            }
            
            // Crear ingreso y agregarlo a la cuenta
            Ingreso ingreso = Ingreso.builder()
                    .importe(pedido.getImporte())
                    .concepto(pedido.getConcepto())
                    .descripcion(pedido.getDescripcion())
                    .fechaIngreso(LocalDateTime.now())
                    .build();
            
            cuenta.agregarIngreso(ingreso);
            contador++;
        }
        
        // Guardar la cuenta con sus ingresos
        cuentaBancariaRepository.save(cuenta);
    }
    
    /**
     * Párrafo 4000-CALCULAR-TOTAL
     * PERFORM VARYING WS-CONTADOR FROM 1 BY 1
     * UNTIL WS-CONTADOR > WS-NUM-INGRESOS
     * ADD WS-ING-IMPORTE(WS-CONTADOR) TO WS-SUMA-TOTAL
     * 
     * Calcula la suma total de ingresos usando Stream en lugar de PERFORM VARYING
     */
    private BigDecimal calcularTotal(CuentaBancaria cuenta) {
        BigDecimal total = cuenta.calcularTotalIngresos();
        log.info("Total de ingresos calculado: {}", total);
        return total;
    }
    
    /**
     * Párrafo 5000-MOSTRAR-RESUMEN
     * DISPLAY de todo el resumen de la cuenta e ingresos
     */
    private void mostrarResumen(CuentaBancaria cuenta) {
        log.info(" ");
        log.info("==========================================");
        log.info("   RESUMEN DE INGRESOS");
        log.info("==========================================");
        log.info("Cuenta:  {}", cuenta.getNumeroCuenta());
        log.info("Titular: {}", cuenta.getTitular());
        log.info("------------------------------------------");
        
        int contador = 1;
        for (Ingreso ingreso : cuenta.getIngresos()) {
            log.info("  Ingreso #{}: {} - {}", 
                    contador, 
                    ingreso.getImporte(), 
                    ingreso.getConcepto());
            contador++;
        }
        
        log.info("------------------------------------------");
        log.info("Numero de ingresos: {}", cuenta.obtenerNumeroIngresos());
        log.info("SUMA TOTAL:         {}", cuenta.calcularTotalIngresos());
        log.info("==========================================");
    }
    
    /**
     * Párrafo 9000-FINALIZAR
     * STOP RUN
     */
    private void finalizar() {
        log.info(" ");
        log.info("Operacion finalizada correctamente.");
        log.info("Gracias por usar el sistema bancario.");
    }
    
    /**
     * Obtiene una cuenta bancaria por su ID
     */
    public CuentaBancariaResponseDTO obtenerCuenta(Long id) {
        CuentaBancaria cuenta = cuentaBancariaRepository.findById(id)
                .orElseThrow(() -> new CuentaNoEncontradaException("Cuenta no encontrada con ID: " + id));
        
        return construirResponse(cuenta);
    }
    
    /**
     * Obtiene una cuenta bancaria por su número de cuenta
     */
    public CuentaBancariaResponseDTO obtenerCuentaPorNumero(String numeroCuenta) {
        CuentaBancaria cuenta = cuentaBancariaRepository.findByNumeroCuenta(numeroCuenta)
                .orElseThrow(() -> new CuentaNoEncontradaException("Cuenta no encontrada: " + numeroCuenta));
        
        return construirResponse(cuenta);
    }
    
    /**
     * Agrega un ingreso a una cuenta existente
     */
    public CuentaBancariaResponseDTO agregarIngreso(Long cuentaId, IngresoPedidoDTO ingresoPedido) {
        CuentaBancaria cuenta = cuentaBancariaRepository.findById(cuentaId)
                .orElseThrow(() -> new CuentaNoEncontradaException("Cuenta no encontrada con ID: " + cuentaId));
        
        // Validación
        if (ingresoPedido.getImporte().compareTo(BigDecimal.ZERO) <= 0) {
            throw new OperacionInvalidaException("El importe debe ser positivo");
        }
        
        Ingreso ingreso = Ingreso.builder()
                .importe(ingresoPedido.getImporte())
                .concepto(ingresoPedido.getConcepto())
                .descripcion(ingresoPedido.getDescripcion())
                .fechaIngreso(LocalDateTime.now())
                .build();
        
        cuenta.agregarIngreso(ingreso);
        cuentaBancariaRepository.save(cuenta);
        
        log.info("Ingreso agregado a la cuenta {}: {}", cuenta.getNumeroCuenta(), ingresoPedido.getImporte());
        
        return construirResponse(cuenta);
    }
    
    /**
     * Construye el DTO de respuesta a partir de una entidad CuentaBancaria
     */
    private CuentaBancariaResponseDTO construirResponse(CuentaBancaria cuenta) {
        List<IngresoResponseDTO> ingresosDTOs = cuenta.getIngresos().stream()
                .map(ingreso -> IngresoResponseDTO.builder()
                        .id(ingreso.getId())
                        .importe(ingreso.getImporte())
                        .concepto(ingreso.getConcepto())
                        .fechaIngreso(ingreso.getFechaIngreso())
                        .descripcion(ingreso.getDescripcion())
                        .build())
                .collect(Collectors.toList());
        
        return CuentaBancariaResponseDTO.builder()
                .id(cuenta.getId())
                .numeroCuenta(cuenta.getNumeroCuenta())
                .titular(cuenta.getTitular())
                .saldo(cuenta.getSaldo())
                .numeroIngresos(cuenta.obtenerNumeroIngresos())
                .sumaTotalIngresos(cuenta.calcularTotalIngresos())
                .ingresos(ingresosDTOs)
                .build();
    }
}
