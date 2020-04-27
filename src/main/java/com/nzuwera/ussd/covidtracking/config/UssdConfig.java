package com.nzuwera.ussd.covidtracking.config;


import org.springframework.context.annotation.Configuration;
import org.springframework.web.servlet.config.annotation.CorsRegistry;
import org.springframework.web.servlet.config.annotation.WebMvcConfigurer;

@Configuration(value = "UssdConfig")
public class UssdConfig implements WebMvcConfigurer {


    /**
     * Overriding the CORS configuration to exposed required header for ussd to work
     *
     * @param registry CorsRegistry
     */
    @Override
    public void addCorsMappings(CorsRegistry registry) {
        registry.addMapping("/ussd/**")
                .allowedOrigins("*")
                .allowedMethods("GET", "HEAD")
                .allowedHeaders("X-Auth-Token", "Content-Type")
                .exposedHeaders("Freeflow")
                .allowCredentials(false)
                .maxAge(4800);
    }
}
