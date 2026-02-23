package com.banco.controller;

import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.RestController;

/** Traducción del COBOL: tra1.cbl (HelloWorld). */
@RestController
public class HelloWorldController {

  @GetMapping("/api/hello")
  public String hello() {
    return "Hello World!";
  }
}
