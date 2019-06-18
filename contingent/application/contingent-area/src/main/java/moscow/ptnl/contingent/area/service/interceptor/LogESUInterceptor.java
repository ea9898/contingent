package moscow.ptnl.contingent.area.service.interceptor;

import moscow.ptnl.contingent.domain.esu.event.annotation.LogESU;
import java.lang.reflect.Method;
import moscow.ptnl.contingent.area.entity.area.Area;
import moscow.ptnl.contingent.area.repository.area.AreaCRUDRepository;
import moscow.ptnl.contingent.area.service.AreaServiceHelper;
import moscow.ptnl.contingent.area.service.EsuHelperService;
import moscow.ptnl.contingent.domain.esu.event.AreaInfoEvent;
import org.aspectj.lang.ProceedingJoinPoint;
import org.aspectj.lang.annotation.Around;
import org.aspectj.lang.annotation.Aspect;
import org.aspectj.lang.reflect.MethodSignature;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

/**
 *
 * @author mkachalov
 */
@Aspect
@Component
public class LogESUInterceptor {
    
    private final static Logger LOG = LoggerFactory.getLogger(LogESUInterceptor.class);
    
    @Autowired
    private AreaServiceHelper areaHelper;
    
    @Autowired
    private EsuHelperService esuHelperService;
    
    @Autowired
    private AreaCRUDRepository areaCRUDRepository;
    
    @Around("@annotation(LogESU)")
    public Object logESU(ProceedingJoinPoint joinPoint) throws Throwable {
        
        MethodSignature signature = (MethodSignature) joinPoint.getSignature();
        Method method = signature.getMethod();
                
        String methodName = method.getName();
        LogESU annotation = method.getAnnotation(LogESU.class);
        LOG.info("INVOCE : "+ methodName + " FOR EVENT : " + annotation.value().getName());

        final Object proceed = joinPoint.proceed();
        
        if (annotation.value().isAssignableFrom(AreaInfoEvent.class)) {
            //Area area = areaCRUDRepository.findById(areaId);
            //if (areaHelper.isAreaPrimary(area)) {
            //    esuHelperService.sendAreaInfoEvent(area, methodName);
            //}
        }
        
        return proceed;
    }
    
}
