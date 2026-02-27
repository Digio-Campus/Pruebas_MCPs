-- Datos iniciales simulados (equivalente a 2000-CARGAR-DATOS-SIMULADOS del COBOL)
INSERT INTO cuentas (numero_cuenta, titular, tipo_cuenta, saldo_disponible, saldo_retenido) VALUES
('ES1234567890123456', 'GARCIA LOPEZ, MARIA', 'CORRIENTE', 15250.75, 500.00);
INSERT INTO cuentas (numero_cuenta, titular, tipo_cuenta, saldo_disponible, saldo_retenido) VALUES
('ES9876543210987654', 'MARTINEZ RUIZ, PEDRO', 'AHORRO', 42000.00, 0.00);
INSERT INTO cuentas (numero_cuenta, titular, tipo_cuenta, saldo_disponible, saldo_retenido) VALUES
('ES5555666677778888', 'FERNANDEZ DIAZ, ANA', 'NOMINA', 3200.50, 150.00);

-- Movimientos simulados (equivalente a 2000-CARGAR-MOVIMIENTOS-SIMULADOS)
INSERT INTO movimientos (fecha, concepto, tipo, importe, numero_cuenta) VALUES
('2026-02-01', 'NOMINA FEBRERO', 'I', 2500.00, 'ES1234567890123456');
INSERT INTO movimientos (fecha, concepto, tipo, importe, numero_cuenta) VALUES
('2026-02-03', 'ALQUILER VIVIENDA', 'G', 850.00, 'ES1234567890123456');
INSERT INTO movimientos (fecha, concepto, tipo, importe, numero_cuenta) VALUES
('2026-02-05', 'SUPERMERCADO', 'G', 125.50, 'ES1234567890123456');
INSERT INTO movimientos (fecha, concepto, tipo, importe, numero_cuenta) VALUES
('2026-02-07', 'TRANSFERENCIA RECIBIDA', 'I', 300.00, 'ES1234567890123456');
INSERT INTO movimientos (fecha, concepto, tipo, importe, numero_cuenta) VALUES
('2026-02-10', 'SEGURO COCHE', 'G', 75.00, 'ES1234567890123456');
INSERT INTO movimientos (fecha, concepto, tipo, importe, numero_cuenta) VALUES
('2026-02-12', 'LUZ ELECTRICA', 'G', 95.30, 'ES1234567890123456');
INSERT INTO movimientos (fecha, concepto, tipo, importe, numero_cuenta) VALUES
('2026-02-15', 'INGRESO EFECTIVO', 'I', 500.00, 'ES1234567890123456');
INSERT INTO movimientos (fecha, concepto, tipo, importe, numero_cuenta) VALUES
('2026-02-18', 'GASOLINERA', 'G', 60.00, 'ES1234567890123456');
