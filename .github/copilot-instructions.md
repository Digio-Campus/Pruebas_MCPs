# Copilot Instructions for MCP Testing Project

## Project Overview
This repository documents testing and integration of Model Context Protocol (MCP) servers with GitHub Copilot. Focus areas are browser automation (Chrome DevTools MCP) and knowledge persistence (Memory MCP).

# Ticketmaster – contar eventos por ciudad y rango de fechas

## Herramientas
- `chrome-devtools`
- `memory`

---

## Objetivo
Contar cuántos eventos hay en `www.ticketmaster.es` para **una ciudad cualquiera** dentro de **un rango de fechas cualquiera**, para ello tienes que ver si hay información al respecto en memory MCP si no hay utilizar chrome-devtools.

Variables de entrada:
- `ciudad`
- `fecha_inicio`
- `fecha_fin`

---

## Procedimiento (chrome-devtools)

1. Abrir `https://www.ticketmaster.es`.
2. Aceptar o cerrar el banner de cookies si aparece.
3. En el buscador principal (texto “BUSCAR” / “Buscar”), escribir `{ciudad}` y lanzar la búsqueda.
4. Esperar a que carguen los resultados.
5. Abrir el filtro **FECHAS** (si no es visible, abrir primero “Filtros”).
6. Establecer:
   - Fecha inicio: `{fecha_inicio}`
   - Fecha fin: `{fecha_fin}`
7. Pulsar **Aplicar**.
8. Esperar a que el listado se actualice.
9. Contar los eventos:
   - Usar un contador visible si existe.
   - Si no, contar las tarjetas de eventos del listado.

---

## Salida
Devolver solo: `Número de eventos: X`

## Memoria (memory MCP)

Guardar este procedimiento como: ticketmaster_contar_eventos_por_ciudad_y_fechas

Contenido mínimo a guardar:
- Variables: ciudad, fecha_inicio, fecha_fin
- Pasos: búsqueda, filtro por fechas, aplicar, esperar carga, conteo
- Nota: gestionar cookies si aparecen

Este procedimiento debe reutilizarse cada vez que se pida contar eventos en Ticketmaster por ciudad y fechas.