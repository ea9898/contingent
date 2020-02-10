/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package moscow.ptnl.contingent.service.security;

import com.fasterxml.jackson.core.type.TypeReference;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.fasterxml.jackson.module.jaxb.JaxbAnnotationModule;
import java.util.HashMap;
import java.util.Map;
import java.util.Optional;
import javax.annotation.PostConstruct;
import moscow.ptnl.contingent.domain.security.setting.AuthService;
import moscow.ptnl.contingent.infrastructure.service.setting.SettingService;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.scheduling.annotation.Scheduled;
import org.springframework.stereotype.Service;

/**
 * Сериализует/десереиализует настройки безопасности в JSON-строку.
 * Формат строки описан в: https://wiki.emias.mos.ru/pages/viewpage.action?pageId=77437934
 * 
 * @author mkachalov
 */
@Service
public class SecuritySettingService {
    
    private static final Logger LOG = LoggerFactory.getLogger(SecuritySettingService.class);
    
    private ObjectMapper mapper;
    
    private Map<String, AuthService> settings;
    
    @Autowired
    private SettingService settingService;

    public SecuritySettingService() {
        this.mapper = new ObjectMapper();
        this.mapper.registerModule(new JaxbAnnotationModule());
    }
    
    @PostConstruct
    @Scheduled(fixedRate = 60000 * 5, initialDelay = 60000)
    protected void init() {
        LOG.info("Чтение настроек безопасности");
        this.settings = unmarshall(settingService.getSettingProperty(SettingService.SERVICES_SECURITY_SETTINGS));
    }
    
    /**
     * Настройки ограничений доступа для сервиса.
     * 
     * @param serviceName
     * @return 
     */
    public Optional<AuthService> getSecuritySettings(String serviceName) {
        if (settings != null && settings.containsKey(serviceName)) {
            return Optional.of(settings.get(serviceName));
        }
        return Optional.empty();
    }
    
    public String marshall(Map<String, AuthService> object) throws RuntimeException {
        if (object == null) {
            return null;
        }
        try {
            return mapper.writeValueAsString(object);
        } catch (Exception e) {
            LOG.error("Ошибка сериализации настроек безопасности", e);
            throw new RuntimeException(e);
        }
    }
    
    public Map<String, AuthService> unmarshall(String jsonString) throws RuntimeException {
        if (jsonString == null) {
            return null;
        }
        TypeReference<HashMap<String, AuthService>> typeRef = new TypeReference<HashMap<String, AuthService>>() {};
        try {
            return mapper.readValue(jsonString.trim(), typeRef);
        } catch (Exception e) {
            LOG.error("Ошибка десериализации настроек безопасности", e);
            throw new RuntimeException(e);
        }
    }
    
}
