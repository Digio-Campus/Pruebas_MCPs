# Copilot Instructions for MCP Testing Project

## Project Overview
This repository documents testing and integration of Model Context Protocol (MCP) servers with GitHub Copilot. Focus areas are browser automation (Chrome DevTools MCP) and knowledge persistence (Memory MCP).

## Architecture & Key Components

### MCP Server Configuration
- **Location:** [.vscode/mcp.json](.vscode/mcp.json)
- **Pattern:** Servers are configured as NPX-based tools with environment variables for persistence
- **Active Servers:**
  - `chrome-devtools`: Browser automation via `chrome-devtools-mcp`
  - `memory`: Knowledge graph persistence via `@modelcontextprotocol/server-memory`

### Memory Persistence Setup
- Environment variable: `MEMORY_PERSIST=true`
- File location: `~/memory.json`
- **Use case:** Store extracted data, navigation states, and structured knowledge across sessions

## Testing Methodology

### Chrome DevTools MCP Tests
See [PRUEBAS.md](PRUEBAS.md) for complete test documentation. Test pattern:
1. **Open browser & navigate** to target website
2. **Extract structured data** (text, links, JSON)
3. **Parse & analyze** content (headings, network requests, console errors)
4. **Return findings** to user with relevant metadata

### Test Categories
- **Web Scraping:** Extract links, headings, and news items from university websites
- **Network Analysis:** Capture HTTP/2 requests, headers, SSL/TLS configuration
- **Console Inspection:** Identify JS errors, warnings, and module loading issues
- **Data Extraction:** Convert HTML/DOM content to structured formats (JSON, lists)

## Project-Specific Patterns

### Data Extraction Pattern
When extracting web content:
- Always use the Memory MCP to store results with descriptive keys
- Include source URLs and timestamps
- Convert HTML/DOM to structured JSON when possible
- Offer user options for file saving (e.g., `.txt`, `.json`)

### Error Handling
- Wrap optional module loads in try-catch blocks (see Liferay module pattern in test results)
- Distinguish between critical errors (HTTP status) and expected warnings (missing optional modules)
- Always report HTTP/2 status and SSL/TLS certificate validity

### Navigation Workflow
1. **Fetch page** via browser automation
2. **Wait for interactive elements** before querying
3. **Extract DOM structure** as JSON when schema matters
4. **Cache results** in Memory MCP with semantic keys (e.g., `CH-20000` for specific data)

## Critical Developer Information

### Running Tests
Tests are manually executed through GitHub Copilot prompts. Each test is documented with:
- Task description and expected outcome
- Screenshots of execution (stored in [imagenes/](imagenes/) directory)
- Full transcript of AI agent interactions
- Extracted data results

### Common Test Targets
- University of Murcia (um.es): Faculty lists, news, institutional structure
- YouTube videos: Content summarization from transcripts and metadata
- Network inspection: Protocol analysis, certificate validation

## Integration Points

### External Dependencies
- **Chrome browser:** Required for DevTools MCP testing
- **NPX packages:** Ensure `chrome-devtools-mcp` and `@modelcontextprotocol/server-memory` are available
- **Liferay CMS:** Target website uses Liferay Community Edition (informs parsing strategy)

### VS Code Configuration
The MCP servers are registered in `.vscode/mcp.json`. When updating:
- Maintain exact `command` and `args` structure
- Preserve environment variable format for persistence
- Test configuration by running a simple test prompt

## Key Files Reference
- [.vscode/mcp.json](.vscode/mcp.json) — Server configuration
- [PRUEBAS.md](PRUEBAS.md) — Test documentation with results
- [imagenes/](imagenes/) — Screenshots and visual evidence of tests

## Notes for AI Agents
- Always check if a Memory MCP entry exists before fetching new data
- Offer to save extracted data in multiple formats for user convenience
- Document network requests with full headers and response codes
- When parsing complex websites, break tasks into sub-steps (open → wait → extract → format)
