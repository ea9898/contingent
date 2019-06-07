package moscow.ptnl.contingent.area.service;

import org.aspectj.lang.JoinPoint;
import org.aspectj.lang.ProceedingJoinPoint;
import org.aspectj.lang.annotation.AfterReturning;
import org.aspectj.lang.annotation.Around;
import org.aspectj.lang.annotation.Aspect;
import org.aspectj.lang.annotation.Before;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.stereotype.Component;

@Aspect
@Component
public class SendAreaInfoToESU {

    private static final Logger LOG = LoggerFactory.getLogger(SendAreaInfoToESU.class);

    @Around("execution(* *(..))")
    public void sendToESU(ProceedingJoinPoint joinPoint) {
        LOG.error("Executing loggingAdvice on getAreaById()");
    }
}
