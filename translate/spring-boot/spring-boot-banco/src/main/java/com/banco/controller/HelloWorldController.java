package com.banco.controller;

import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.RestController;

/**
 * Traducción de tra1.cbl (HelloWorld).
 * PROGRAM-ID. HelloWorld → HelloWorldController
 * DISPLAY "Hello World!" → return "Hello World!"
 */
@RestController
public class HelloWorldController {

    @GetMapping("/hello")
    public String hello() {
        return "Hello World!";
    }
}
