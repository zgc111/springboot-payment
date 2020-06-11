package com.suncd.epm.cm;

import org.mybatis.spring.annotation.MapperScan;
import org.springframework.boot.SpringApplication;
import org.springframework.boot.autoconfigure.SpringBootApplication;
import org.springframework.context.annotation.ComponentScan;

/**
 * Spring Boot 应用启动类
 * <p>
 *
 * @author bysocket
 * @date 16/4/26
 */
@SpringBootApplication
@MapperScan("com.suncd.epm.cm.dao")
@ComponentScan("com.suncd")
public class Application {

    public static void main(String[] args) {
        SpringApplication.run(Application.class, args);
    }
}
