/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package moscow.ptnl.contingent.area.service.interceptor;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.Set;
import javax.validation.constraints.NotNull;
import moscow.ptnl.contingent.domain.security.setting.AuthMethod;
import moscow.ptnl.contingent.domain.security.setting.AuthService;

import moscow.ptnl.contingent.service.security.SecuritySettingService;
import moscow.ptnl.ws.security.UserContextHolder;
import org.aspectj.lang.ProceedingJoinPoint;
import org.aspectj.lang.annotation.Around;
import org.aspectj.lang.annotation.Aspect;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;
import org.springframework.stereotype.Service;
//import ru.mos.emias.contingent2.area.Fault;
//import ru.mos.emias.errors.domain.ErrorMessageType;
//import ru.mos.emias.errors.domain.Message;
//import ru.mos.emias.errors.domain.OtherSecurityException;
//import ru.mos.emias.errors.domain.UnauthorizedException;

//TODO Поправить создаваемые исключительные ситуации

/**
 * Проверяет наличие прав у пользователя на доступ к методу аннотированному
 * как @see EMIASSecured с учетом настроек из AD_CONFIG.
 * 
 * @author mkachalov
 */
@Aspect
@Component
public class SecurityInterceptor {
    
    private final static Logger LOG = LoggerFactory.getLogger(SecurityInterceptor.class);
    
    @Autowired
    private SecuritySettingService securitySettings;
    
    @Around(
        value = "execution(public * *(..)) && @annotation(annotation)",
        argNames="annotation"
    )
    public Object secured(ProceedingJoinPoint joinPoint, moscow.ptnl.contingent.domain.security.annotation.EMIASSecured annotation) throws Throwable {

        String serviceName = getServiceName(joinPoint.getTarget().getClass());
        String methodName = joinPoint.getSignature().getName();
        
        LOG.info("Защищенный метод: {} в сервисе: {}", methodName, serviceName);
        
        Optional<AuthService> setting = securitySettings.getSecuritySettings(serviceName);
        if (setting.isPresent()) {
            AuthService serviceSetting = setting.get();
            if (!serviceSetting.isEnabled()) {
                throw new RuntimeException("Доступ к методам сервиса запрещен");
            }
            
            ; //
            
            Map<String, AuthMethod> methodSetting = serviceSetting.getAuthMethods();
            if (!methodSetting.containsKey(methodName)) {
                LOG.warn("В настройках ограничений доступа для сервиса: {}, метод: {} не перечислен", serviceName, methodName);
            } else if (!methodSetting.get(methodName).isEnabled()) {
                throw new RuntimeException("Доступ к методу запрещен");
            } else if (!hasAcessRights(methodSetting.get(methodName).getPermissions(), UserContextHolder.getPrincipal().getAccessRights())) { //FIXME нужно передать права текущего пользователя
                throw new RuntimeException("Недостаточно прав для выполнения метода");
            }
            
        }
        
        return joinPoint.proceed();
    }
    
    /*
    private Fault getSecurityFault(@NotNull List<Long> requiredRights) {
        UnauthorizedException ex = new UnauthorizedException(requiredRights.stream().mapToLong(l -> l).toArray());
        return new Fault(ex.getMessage() != null ? ex.getMessage() : ex.toString(), ex);
    }
    */
    
    
    /*
    private Fault getSecurityFault() {
    	OtherSecurityException ex = new OtherSecurityException(new Message(messagesAdapter.createErrorReason(ErrorCode.INCORRECT_HEADER.getCode(), ErrorMessageType.ERROR)));
        return new Fault(ex.getMessage() != null ? ex.getMessage() : ex.toString(), ex);
    } 
    */
    
    private String getServiceName(Class<?> clazz) {
        Service serviceAnnotation = clazz.getAnnotation(Service.class);
        if (serviceAnnotation == null) {
            throw new RuntimeException("У сервиса не определено имя");
        }
        return serviceAnnotation.value();
    }
    
    private boolean hasAcessRights(List<Long> methodRights, Set<Long> userRights) {
        return methodRights.stream().anyMatch(userRights::contains);
    }
    
    
}
