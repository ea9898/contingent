package moscow.ptnl.contingent.area.service.interceptor;

import java.lang.reflect.Method;
import java.util.Optional;
import java.util.concurrent.TimeUnit;
import moscow.ptnl.metrics.bind.ServiceMetrics;
import org.aspectj.lang.ProceedingJoinPoint;
import org.aspectj.lang.annotation.Around;
import org.aspectj.lang.annotation.Aspect;
import org.aspectj.lang.reflect.MethodSignature;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;



/**
 * Регистрирует метод аннотированному как @see Metrics для сбора статистики.
 * 
 * @author m.kachalov
 */
@Aspect
@Component
public class MetricsInterceptor {
    
    private final static Logger LOG = LoggerFactory.getLogger(MetricsInterceptor.class);
    
    //
    //private ApplicationContext appContext;
    @Autowired
    private ServiceMetrics metrics;
    
    @Around(
        value = "execution(public * *(..)) && @annotation(annotation)",
        argNames="annotation"
    )
    public Object meter(ProceedingJoinPoint joinPoint, moscow.ptnl.metrics.Metrics annotation) throws Throwable {
        
        Method method = getMethod(joinPoint);
        LOG.info("CALL METHOD: {}", method.getName());
        
        Optional<ServiceMetrics.MethodMetrics> metrica = metrics.getMetrics(method);
        if (metrica.isPresent()) {
            LOG.info("FINDED METRICA");
            metrica.get().getRequests().increment();
        }
        
        long method_start_time = System.nanoTime();        
        try {
            Object result = joinPoint.proceed();
            return result;
        } catch (Throwable e) {
            //считаем ошибки
            if (metrica.isPresent()) {
                metrica.get().getErrors().increment();
            }
            throw e;
        } finally {
            if (metrica.isPresent()) {
                long method_execution_time = System.nanoTime() - method_start_time;
                metrica.get().getTimer().record(method_execution_time, TimeUnit.NANOSECONDS);
            }
        }
    }
    
    private Method getMethod(ProceedingJoinPoint joinPoint) {
        MethodSignature signature = (MethodSignature) joinPoint.getSignature();
        Method method = signature.getMethod();
        if (method.getDeclaringClass().isInterface()) {
            try {
                method = joinPoint.getTarget().getClass().getDeclaredMethod(
                        joinPoint.getSignature().getName(),
                        method.getParameterTypes()
                );
            } catch (Exception e) {
                LOG.error("Ошибка получения метода", e);
            }
        }
        return method;
    }
    
}
