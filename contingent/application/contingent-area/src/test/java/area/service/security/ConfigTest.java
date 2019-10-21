/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package area.service.security;

import java.io.IOException;
import java.io.InputStreamReader;
import java.io.Reader;
import java.lang.reflect.Method;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.Map;
import moscow.ptnl.contingent.area.ws.v1.AreaServiceImpl;
import moscow.ptnl.contingent.domain.security.annotation.EMIASSecured;
import moscow.ptnl.contingent.domain.security.setting.AuthMethod;
import moscow.ptnl.contingent.domain.security.setting.AuthService;
import moscow.ptnl.contingent.service.security.SecuritySettingService;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.junit.jupiter.MockitoExtension;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.test.context.ContextConfiguration;
import org.springframework.test.context.junit.jupiter.SpringExtension;
import static org.junit.jupiter.api.Assertions.*;
import org.junit.jupiter.api.Disabled;

/**
 *
 * @author m.kachalov
 */
@ExtendWith(SpringExtension.class)
@ExtendWith(MockitoExtension.class)
@ContextConfiguration(classes= {area.service.security.MockConfiguration.class})
public class ConfigTest {
    
    @Autowired
    private SecuritySettingService settingService;
    
    private String CONFIG;
    private final Class<?>[] SERVICES = new Class<?>[]{
        AreaServiceImpl.class
    };
    
    @BeforeEach
    public void init() {
        CONFIG = getFileContent("security/config.json");
    }
    
    //@Test
    public void checkMethods() {
        Map<String, AuthService> configSetting = settingService.unmarshall(CONFIG); //сгенерен по файлу
        Map<String, AuthService> configGenerated = createConfig(SERVICES); //сгенерен по реальным методам
        
        //проверим наличие сервиса и методов в классе
        for (Map.Entry<String, AuthService> serviceEntry : configSetting.entrySet()) {
            assertTrue(configGenerated.containsKey(serviceEntry.getKey()), "Отсутсвует сервис с именем [" + serviceEntry.getKey() + "]");
            
            AuthService generatedService = configGenerated.get(serviceEntry.getKey());
            for (Map.Entry<String, AuthMethod> methodEntry : serviceEntry.getValue().getAuthMethods().entrySet()) {
                assertTrue(generatedService.getAuthMethods().containsKey(methodEntry.getKey()), "Сервис [" + serviceEntry.getKey() + "] не содержит метода [" + methodEntry.getKey() + "] или он не аннотирован");
            }
        }
        
        //проверим наличие сервиса и методов в конфиге
        for (Map.Entry<String, AuthService> serviceEntry : configGenerated.entrySet()) {
            assertTrue(configSetting.containsKey(serviceEntry.getKey()), "В конфигурации отсутсвует настройка для сервиса с именем [" + serviceEntry.getKey() + "]");
            
            AuthService settingAreaService = configSetting.get(serviceEntry.getKey());
            for (Map.Entry<String, AuthMethod> methodEntry : serviceEntry.getValue().getAuthMethods().entrySet()) {
                assertTrue(settingAreaService.getAuthMethods().containsKey(methodEntry.getKey()), "Конфигурация сервиса [" + serviceEntry.getKey() + "] не содержит настроек для аннотированного метода [" + methodEntry.getKey() + "]");
            }
        }
    }
    
    //@Test метод для генерации конфига (без прав) по списку методов класса web-сервиса
    public void generateConfig() {        
        try {
            String config = settingService.marshall(createConfig(SERVICES));
            System.out.println("Длина конфига: " + config.length());
            System.out.println("Конфиг: ");
            System.out.println(config);
            System.out.println();
        } catch (Exception e) {
            e.printStackTrace();
        }
    }
    
    private Map<String, AuthService> createConfig(Class<?> ... serviceClasses) {
        Map<String, AuthService> config = new HashMap<>();
        
        for(Class<?> serviceClass : serviceClasses) {
            AuthService conf = new AuthService();
            if (serviceClass.isAnnotationPresent(Service.class)) {
                Service serviceAnn = serviceClass.getAnnotation(Service.class);
                if (serviceAnn.value() == null || serviceAnn.value().isEmpty()) {
                    throw new RuntimeException("У сервиса не определено имя");
                }
                conf.setEnabled(true);
                config.put(serviceAnn.value(), conf);
                for (Method method : serviceClass.getDeclaredMethods()) {
                    EMIASSecured securedAnn = method.getAnnotation(EMIASSecured.class);
                    if (securedAnn != null) {
                        AuthMethod met = new AuthMethod();
                        met.setEnabled(true);
                        met.setPermissions(new ArrayList<>());
                        conf.getAuthMethods().put(method.getName(), met);
                    }
                }
            }
        }
        
        return config;
    }
    
    private String getFileContent(String pathToFile) {
        StringBuilder out = new StringBuilder();
        try (Reader in = new InputStreamReader(ConfigTest.class.getClassLoader().getResourceAsStream(pathToFile), "UTF-8")) {
            char[] buffer = new char[512];
            for (int i = 0; (i = in.read(buffer)) > 0;) {
                out.append(buffer, 0, i);
            }
        } catch (IOException ex) {
            System.err.println(ex);
        }
        return out.toString();
    }
    
}
