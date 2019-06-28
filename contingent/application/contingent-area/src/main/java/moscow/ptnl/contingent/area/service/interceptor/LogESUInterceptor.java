package moscow.ptnl.contingent.area.service.interceptor;

import moscow.ptnl.contingent.domain.esu.event.annotation.LogESU;
import java.lang.reflect.Method;
import java.lang.reflect.Parameter;
import java.util.ArrayList;
import java.util.Collection;
import java.util.List;
import java.util.Optional;

import moscow.ptnl.contingent.area.entity.area.Area;
import moscow.ptnl.contingent.area.service.AreaServiceHelper;
import moscow.ptnl.contingent.area.service.EsuHelperService;
import moscow.ptnl.contingent.domain.esu.event.AreaInfoEvent;
import moscow.ptnl.contingent.repository.area.AreaCRUDRepository;
import org.aspectj.lang.ProceedingJoinPoint;
import org.aspectj.lang.annotation.Around;
import org.aspectj.lang.annotation.Aspect;
import org.aspectj.lang.reflect.MethodSignature;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

/**
 * Перехватчик методов аннотированных как {@link LogESU}.
 * После того как метод завершит работу, производит отправку события в канал.
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
    
    @Around(
        value = "execution(public * *(..)) && @annotation(annotation)",
        argNames="annotation"
    )
    public Object logESU(ProceedingJoinPoint joinPoint, moscow.ptnl.contingent.domain.esu.event.annotation.LogESU annotation) throws Throwable {
        
        MethodSignature signature = (MethodSignature) joinPoint.getSignature();
        Method method = signature.getMethod();
        Object[] args = joinPoint.getArgs();
            
        method.setAccessible(true);
        String methodName = method.getName();
        
        final Object result = joinPoint.proceed();
        
        if (annotation.type().isAssignableFrom(AreaInfoEvent.class)) {
            List<Long> areaIds = getAreaId(annotation, method, args, result);

            if (areaIds.isEmpty()) {
                throw new IllegalArgumentException("идентификатор сущности null");
            }
            for (Long areaId : areaIds) {
                Optional<Area> area = areaCRUDRepository.findById(areaId);

                if (!area.isPresent()) {
                    throw new IllegalArgumentException("сущность с идентификатором " + areaId + " не найдена");
                }
                Area areaObject = area.get();

                if (areaHelper.isAreaPrimary(areaObject)) {
                    esuHelperService.sendAreaInfoEvent(areaObject, methodName);
                }
            }
        } else {
            throw new RuntimeException("не поддерживаемый тип события");
        }
        
        return result;
    }
    
    private List<Long> getAreaId(LogESU annotation, Method method, Object[] args, Object result) {
        Object value;

        if (annotation.useResult()) {
            if (result == null) {
                throw new IllegalArgumentException("в методе " + method.getName() + " результат не может быть null");
            }
            value = result;
        } else {
            if (annotation.parameters().length == 0) {
                throw new IllegalArgumentException("в методе " + method.getName() + " в списке параметров должно быть название параметра содержащего идентификатор сущности");
            }
            String parameterName = annotation.parameters()[0];
            Optional<Object> parameterValue = getParameterByName(method, args, parameterName);
            if (!parameterValue.isPresent()) {
                throw new IllegalArgumentException("в методе " + method.getName() + " не найден параметр " + parameterName);
            }
            value = parameterValue.get();

            if (value == null) {
                throw new IllegalArgumentException("в методе " + method.getName() + " параметр " + parameterName + " не может быть null");
            }
        }
        List<Long> areaIds = mapObjectToAreaIds(value);

        if (areaIds.isEmpty()) {
            throw new IllegalArgumentException("в методе " + method.getName() + " невозможно получить идентификатор сущности из результата выполнения метода");
        }
        return areaIds;
    }

    private List<Long> mapObjectToAreaIds(Object obj) {
        List<Long> result = new ArrayList<>();

        if (obj instanceof Long) {
            result.add((Long) obj);
        }
        else if (obj instanceof Area) {
            result.add(((Area) obj).getId());
        }
        else if (obj instanceof Collection) {
            ((Collection<?>) obj).forEach(o -> result.addAll(mapObjectToAreaIds(o)));
        }
        return result;
    }
    
    /**
     * Для определения имени аргумента при компиляции maven-compiler-plugin 
     * должен иметь настройку &lt;parameters&gt;true&lt;/parameters&gt;.
     * 
     * @param method
     * @param args
     * @param parameterName
     * @return 
     */
    private Optional<Object> getParameterByName(Method method, Object[] args, String parameterName) {
        for (int i = 0; i < method.getParameterCount(); i++) {            
            Parameter parameter = method.getParameters()[i];  
            LOG.debug("PARAMETER NAME: " + parameter.getName());
            if (parameter.getName().equals(parameterName)) {
                return Optional.of(args[i]);
            }
        }
        return Optional.empty();
    }
    
}
