package com.banco.exception;

import lombok.extern.slf4j.Slf4j;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.ExceptionHandler;
import org.springframework.web.bind.annotation.RestControllerAdvice;
import org.springframework.web.context.request.WebRequest;

import java.time.LocalDateTime;
import java.util.LinkedHashMap;
import java.util.Map;

/**
 * Manejador global de excepciones para la aplicación.
 * Proporciona respuestas consistentes para errores del dominio bancario.
 */
@Slf4j
@RestControllerAdvice
public class GlobalExceptionHandler {
    
    /**
     * Maneja excepciones cuando una cuenta no es encontrada
     */
    @ExceptionHandler(CuentaNoEncontradaException.class)
    public ResponseEntity<Map<String, Object>> manejarCuentaNoEncontrada(
            CuentaNoEncontradaException ex,
            WebRequest request) {
        
        log.error("Cuenta no encontrada: {}", ex.getMessage());
        
        Map<String, Object> cuerpo = new LinkedHashMap<>();
        cuerpo.put("timestamp", LocalDateTime.now());
        cuerpo.put("estado", HttpStatus.NOT_FOUND.value());
        cuerpo.put("error", "Cuenta no encontrada");
        cuerpo.put("mensaje", ex.getMessage());
        
        return new ResponseEntity<>(cuerpo, HttpStatus.NOT_FOUND);
    }
    
    /**
     * Maneja excepciones cuando se intenta realizar una operación inválida
     */
    @ExceptionHandler(OperacionInvalidaException.class)
    public ResponseEntity<Map<String, Object>> manejarOperacionInvalida(
            OperacionInvalidaException ex,
            WebRequest request) {
        
        log.error("Operación inválida: {}", ex.getMessage());
        
        Map<String, Object> cuerpo = new LinkedHashMap<>();
        cuerpo.put("timestamp", LocalDateTime.now());
        cuerpo.put("estado", HttpStatus.BAD_REQUEST.value());
        cuerpo.put("error", "Operación inválida");
        cuerpo.put("mensaje", ex.getMessage());
        
        return new ResponseEntity<>(cuerpo, HttpStatus.BAD_REQUEST);
    }
    
    /**
     * Maneja excepciones genéricas
     */
    @ExceptionHandler(Exception.class)
    public ResponseEntity<Map<String, Object>> manejarExcepcionGlobal(
            Exception ex,
            WebRequest request) {
        
        log.error("Error interno del servidor", ex);
        
        Map<String, Object> cuerpo = new LinkedHashMap<>();
        cuerpo.put("timestamp", LocalDateTime.now());
        cuerpo.put("estado", HttpStatus.INTERNAL_SERVER_ERROR.value());
        cuerpo.put("error", "Error interno del servidor");
        cuerpo.put("mensaje", ex.getMessage());
        
        return new ResponseEntity<>(cuerpo, HttpStatus.INTERNAL_SERVER_ERROR);
    }
}
